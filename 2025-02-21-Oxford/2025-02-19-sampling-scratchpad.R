library(tidyverse)
set.seed(1983)

# Here eligible is defined as having a child aged 0-9 from 2021 Census data
leeds <- read_csv("~/ECLIPS-slides/data/household_age_data.csv") |> 
  select(lsoa = Lower.layer.Super.Output.Areas,
         n_households = total_number_of_households,
         n_eligible = eligible,
         frac_eligible = share_of_eligible) |> 
  mutate(frac_eligible = frac_eligible / 100)


# Sanity check
leeds |> 
  mutate(out = n_eligible / n_households) |> 
  pull(out) |> 
  all.equal(leeds$frac_eligible)

# Make a ggplot histogram of frac_eligible, 
leeds |> 
  ggplot(aes(x = frac_eligible)) +
  geom_histogram(bins = 30) +
  labs(title = "Estimated fraction of households eligible for ECLIPS, by LSOA", 
       subtitle = "Source: 2021 UK Census",
       x = "Fraction of eligible households",
       y = "Count") 

n_households <- leeds |> 
  pull(n_households) |> 
  sum()

n_lsoas <- nrow(leeds)

n_addresses <- 373000

# Is this population change? Are these vacant addresses? Have M look for data
# on number of vacant properties (we know this is available in US Census)
# (Vacant dwellings) We may want to take this into account in our sampling design
c(`Addresses (2024 Council Tax)` = n_addresses, 
  `Households (2021 Census)` = n_households) 

n_letters <- 30000 # suppose our initial sample is 20% of the 150,000 letters 


# How many letters to send to each LSOA? Calculate two sets of sampling weights:
leeds <- leeds |> 
         # Weight by number of households
  mutate(w_households = n_households / sum(n_households),
         # Weight by number of eligible households
         w_eligible_households = n_eligible / sum(n_eligible))

# sanity check: ensure weights sum to one
leeds |> 
  select(w_households, w_eligible_households) |> 
  summarise_all(sum)

# make a ggplot scatter plot of the weights for each LSOA, including 45-degree line
leeds |> 
  ggplot(aes(x = w_households, y = w_eligible_households)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Sampling weights for each LSOA",
       subtitle = "Weights based on number of households and number of eligible households",
       x = "Stratify by number of households",
       y = "Stratify by number of eligible households")


# can't send fractional letters, but want to send exactly n_letters, so send out
# floor(weight * n_letters) letters to each LSOA and then *randomly* allocate 
# the remaining letters with probability proportional to the fractional part 
# of the weight. Each LSOA gets *at most* one additional letter
allocate_exactly_n <- function(weights, n) {
 
  exact <- weights * n # exact, non-integer allocation
  int_part <- floor(exact) # guaranteed allocation
  frac_part <- exact - int_part # probabilistic allocation
  
  # How many are distributed randomly? 
  remaining <- n - sum(int_part)
  
  # Use fractional parts as probabilities for distributing remaining letters
  # Sample indices with probability proportional to fractional parts
  additional <- numeric(length(weights))
  if (remaining > 0) {
    chosen <- sample(1:length(weights), size = remaining, prob = frac_part, 
                     replace = FALSE)
    additional[chosen] <- 1
  }
  int_part + additional # final allocation
}

leeds <- leeds |> 
  mutate(n_letters_households = allocate_exactly_n(w_households, n_letters),
         n_letters_eligible = allocate_exactly_n(w_eligible_households, n_letters))

# Sanity check: the letters sent under either weighting scheme should sum
# to n_letters
leeds |> 
  select(n_letters_households, n_letters_eligible) |> 
  summarise_all(sum)

# Plot the number of letters sent to each LSOA under each weighting scheme as
# above with the weights
leeds |> 
  ggplot(aes(x = n_letters_households, y = n_letters_eligible)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Number of letters sent to each LSOA",
       subtitle = "Letters allocated based on number of households and number of eligible households",
       x = "Stratify by number of households",
       y = "Stratify by number of eligible households")

letters_households <- leeds |>  
  pull(n_letters_households)

letters_eligible <- leeds |>  
  pull(n_letters_eligible)

# Use weights from 2021 census to project the number of households and eligible
# households in each LSOA in 2024 based on the number of addresses, using the
# same tie-breaking rule as above
n_households <- leeds |> 
  pull(w_households) |> 
  allocate_exactly_n(n_addresses)

n_eligible_households <- leeds |> 
  pull(w_eligible_households) |>
  allocate_exactly_n(n_addresses)

# Simulation study to compare sampling methods
 
# Function to calculate parameters of beta distribution from desired mean
# and standard deviation relying on:
#   mean = alpha / (alpha + beta)  
#   var = sd^2 = alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1))
beta_params <- function(mean, sd) {
  var <- sd^2
  alpha <- mean * (mean * (1 - mean) / var - 1)
  beta <- alpha * (1 - mean) / mean
  list(alpha = alpha, beta = beta)
}

# Test with a mean of 0.1 and a sd of 0.02
beta_params(0.1, 0.02)

# Check beta_params() by simulated from a beta distribution and computing the
# mean and variance
params <- beta_params(0.1, 0.02)
simulated <- rbeta(10000, params$alpha, params$beta)
mean(simulated)
sd(simulated)

rm(simulated, params)


# Function that uses beta_params() to plot the density of a beta distribution
# with specified mean and standard deviation
plot_beta <- function(mean, sd, upper = 1, n_points = 1000) {
  params <- beta_params(mean, sd)
  x <- seq(0, upper, length.out = n_points)
  y <- dbeta(x, params$alpha, params$beta)
  tibble(x = x, y = y) |> 
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    labs(title = "Beta distribution with specified mean and standard deviation",
         # create subtitle from parameters passed to function
         subtitle = glue::glue("Mean = {mean}, SD = {sd}"),
         x = "x",
         y = "Density")
}

# Plot some beta distributions to simulate different average response rates
# Take 10% as a baseline response rate but consider lower ones as well
plot_beta(mean = 0.1, sd = 0.01, upper = 0.15)
plot_beta(mean = 0.05, sd = 0.01, upper = 0.15)
plot_beta(mean = 0.01, sd = 0.01, upper = 0.15)


# Simulate response rates for each LSOA under each weighting scheme
p_eligible <- leeds |> 
  pull(frac_eligible)


simulate_design <- function(beta_mean, beta_sd, nreps = 1000) {
  params <- beta_params(beta_mean, beta_sd)
  sim_response_rates <- rbeta(n_lsoas, params$alpha, params$beta)
  
  baseline1 <- rbinom(n_lsoas, size = letters_households, prob = p_eligible)
  baseline2 <- rbinom(n_lsoas, size = baseline1, prob = sim_response_rates)
  baseline <- sum(baseline2)
  
  eligible1 <- rbinom(n_lsoas, size = letters_eligible, prob = p_eligible)
  eligible2 <- rbinom(n_lsoas, size = eligible1, prob = sim_response_rates)
  eligible <- sum(eligible2)
  
  tibble(baseline = baseline, eligible = eligible)
}


# Use purrr to repeatedly call simulate_design() with the same parameters, a 
# total of nreps times, and store the results in a tibble
nreps <- 1000
sim_high_response <- map(1:nreps, ~ simulate_design(0.1, 0.01)) |> 
  bind_rows()
 
sim_mid_response <- map(1:nreps, ~ simulate_design(0.05, 0.01)) |> 
  bind_rows() 

sim_low_response <- map(1:nreps, ~ simulate_design(0.01, 0.01)) |> 
  bind_rows()

# Make a ggplot histogram of the number of responses under each weighting scheme
# use transparency to show the histograms "baseline" and "eligible" on the same 
# plot
sim_high_response |> 
  pivot_longer(cols = everything(), names_to = "scheme") |> 
  ggplot(aes(x = value, fill = scheme)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(title = "Simulated number of responses under each sampling scheme",
       subtitle = "Beta response rates for eligible: mean = 10%, SD = 10%",
       x = "Number of responses",
       y = "Count") +
  theme(legend.position = "top")

# Same for mid response
sim_mid_response |> 
  pivot_longer(cols = everything(), names_to = "scheme") |> 
  ggplot(aes(x = value, fill = scheme)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(title = "Simulated number of responses under each sampling scheme",
       subtitle = "Beta response rates for eligible: mean = 5%, SD = 1%",
       x = "Number of responses",
       y = "Count") +
  theme(legend.position = "top")

# And finally, same for low response
sim_low_response |> 
  pivot_longer(cols = everything(), names_to = "scheme") |> 
  ggplot(aes(x = value, fill = scheme)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(title = "Simulate number of responses under each sampling scheme",
       subtitle = "Beta response rates for eligible: mean = 1%, SD = 1%",
       x = "Number of responses",
       y = "Count") +
  theme(legend.position = "top")
