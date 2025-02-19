library(tidyverse)

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

# Simulation study to compare sampling methods
n_households <- leeds |> 
  pull(n_households) |> 
  sum()

n_addresses <- 373000

c(`Addresses (2024 Council Tax)` = n_addresses, 
  `Households (2021 Census)` = n_households) 

n_letters <- 150000
 
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
set.seed(123)
params <- beta_params(0.1, 0.02)
simulated <- rbeta(10000, params$alpha, params$beta)
mean(simulated)
sd(simulated)


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


 
  