# This simulation shows that an adaptive sampling scheme is more efficient than
# a simple random sample when the population is highly skewed. Conditioning on
# the realized totals yields valid inference. 

# The standard error of a difference of proportions is given by
# sqrt(p * (1 - p) / n_p + q * (1 - q) / n_q)
# For a fixed overall sampling budget N, if we know p and q, our optimal choice
# of n_p versus n_q = (N - n_p), is chosen to minimize the standard error. The 
# solution sets:
#
# n_p = N * V_p / (V_p + V_q)
# n_q = N * V_q / (V_p + V_q)
#
# Where we define: V_p = p * (1 - p) and V_q = q * (1 - q)
#
# A feasible approximation to this approach uses a pilot sample to estimate p
# and q and then uses these estimates to allocate the remaining budget. 

set.seed(1983)
library(tidyverse)

sim_adaptive <- function(p, q, n_prelim = 500, N_final = n_prelim * 8) {
  # Estimate p and q from the pilot sample
  p_pilot <- rbinom(1, n_prelim, p) / n_prelim
  q_pilot <- rbinom(1, n_prelim, q) / n_prelim
  
  # Allocate the remaining budget optimally
  V_p <- p_pilot * (1 - p_pilot)
  V_q <- q_pilot * (1 - q_pilot)
  n_p_final <- round(N_final * V_p / (V_p + V_q))
  n_q_final <- N_final - n_p_final
  
  # Simulate the final sample
  if(n_p_final > 0) p_final <- rbinom(1, n_p_final, p) / n_p_final
  if(n_q_final > 0) q_final <- rbinom(1, n_q_final, q) / n_q_final
  
  # Compute overall estimates from both stages
  n_p_total <- n_prelim + n_p_final
  n_q_total <- n_prelim + n_q_final
  
  p_total <- (n_p_final * p_final + n_prelim * p_pilot) / (n_p_final + n_prelim)
  q_total <- (n_q_final * q_final + n_prelim * q_pilot) / (n_q_final + n_prelim)
  
  # Compute Wilson confidence intervals for the difference of proportions 
  out <- prop.test(c(n_p_total * p_total, n_q_total * q_total),
            c(n_p_total, n_q_total), correct = FALSE)$conf.int
  attributes(out) <- NULL
  out
}


# Repeat simulation many times and store results in a tibble using purrr
nreps <- 1e5
p <- 0.05
q <- 0.1

sim_results <- map(1:nreps, ~possibly(sim_adaptive, otherwise = NULL)(p, q)) |> 
  compact() |> 
  do.call(rbind, args = _) |> 
  as_tibble()
names(sim_results) <- c('lower', 'upper')

sim_results <- sim_results |> 
  mutate(width = upper - lower, 
         covers_truth = (lower < p - q) & (upper > p - q)) 

sim_results |> 
  summarize(avg_width = mean(width), coverage = mean(covers_truth))

# Slight under-coverage, perhaps because of the situations where prop.test fails?
# Or maybe just because the Wilson test isn't exact.
# Need to figure out why it fails and how to handle these situations.

# Compare average coverage and width of "plain vanilla" non-adaptive approach
sim_non_adaptive <- function(p, q, N = 4000) {
  n_p <- round(N / 2)
  n_q <- N - n_p
  
  p_final <- rbinom(1, n_p, p) / n_p
  q_final <- rbinom(1, n_q, q) / n_q
  
  out <- prop.test(c(n_p * p_final, n_q * q_final),
            c(n_p, n_q), correct = FALSE)$conf.int
  attributes(out) <- NULL
  out
}

sim_results_nonadaptive <- map(1:nreps, ~possibly(sim_non_adaptive, otherwise = NULL)(p, q))  
 
sim_results_nonadaptive <- sim_results_nonadaptive |> 
  compact() |> 
  do.call(rbind, args = _) |> 
  as_tibble()

names(sim_results_nonadaptive) <- c('lower', 'upper')
sim_results_nonadaptive <- sim_results_nonadaptive |>
  mutate(width = upper - lower, 
         covers_truth = (lower < p - q) & (upper > p - q))

sim_results_nonadaptive |>
  summarize(avg_width = mean(width), coverage = mean(covers_truth))

sim_results |> 
  summarize(avg_width = mean(width), coverage = mean(covers_truth))

# Modest improvement in confidence interval width here: 12%
# Is the adaptive design worthwhile in practice?