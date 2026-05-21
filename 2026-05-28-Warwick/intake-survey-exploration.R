library(tidyverse)
library(modelsummary)

library(readr)
raw <- read_csv('~/ECLIPS-PAP/data/processed/combined-pseudonymized.csv')


surv <- raw |> 
  filter(usable) |> 
  select(prior_lead_knowledge, 
         worried_exposure,
         home,
         income,
         gov_benefits,
         highest_education,
         ethnicity,
         born, 
         language,
         read_write_english,
         round) 

#datasummary_skim(surv)

# Lead
table(surv$prior_lead_knowledge)
table(surv$worried_exposure)

surv |> 
  select(prior_lead_knowledge) |> 
  ggplot(aes(x = prior_lead_knowledge)) +
  geom_bar()

# Demographics
table(surv$home) 
table(surv$income) 
table(surv$gov_benefits) 
table(surv$highest_education) 
table(surv$ethnicity)
table(surv$born)
table(surv$language)



# A bunch of little histograms; make ordinal things into factors!
# Slide: lead knowledge / worried
# Various demographics: income / race and maybe plot against Leeds as a whole?
# Compare against income distribution of Leeds (but should re-weight based on our sampling weights)
# Ditto for race / ethnicity

#library(ggplot2)
#library(dplyr)
#
#df <- tibble(
#  response = c("High", "Low", "Medium", "High", "Low", "Medium", "Medium", "High")
#)
#
## the known ordering, low -> high
#lvl <- c("Low", "Medium", "High")
#
#df <- df %>%
#  mutate(response = factor(response, levels = lvl, ordered = TRUE))
#
#ggplot(df, aes(x = response)) +
#  geom_bar()


# Focus on getting the "simple" comparisons / results in for all comparisons and all outcome measures; if there is time can add in the Bayesian stuff by try to present it graphically: regression plots with error bars / maps etc.

# Try to do the "clicked on but didn't complete" survey stuff
rm(surv)

raw |> 
  select(treatment, treatment_parsed) 

# No mismatches; not sure why some are missing for treatment...
with(raw, table(treatment == treatment_parsed))
   
# Later we need to remove the ones who started *before it was mailed!* that was us but it shouldn't make a big difference

# Interesting: it looks like generalized risk language (which is present in both G and P) makes a difference for *clicks* but not completed responses
raw |> 
  select(lsoa, treatment = treatment_parsed,
         house_type = house_type_parsed) |> 
  filter(house_type == 'NonOld') |> 
  count(treatment) |> 
  mutate(prop = n / sum(n))

raw |> 
  filter(usable) |> 
  select(lsoa, treatment = treatment_parsed,
         house_type = house_type_parsed) |> 
  filter(house_type == 'NonOld') |> 
  count(treatment) |> 
  mutate(prop = n / sum(n))

raw |> 
  select(lsoa, treatment = treatment_parsed,
         house_type = house_type_parsed) |> 
  filter(house_type == 'Old') |> 
  count(treatment) |> 
  mutate(prop = n / sum(n))

raw |> 
  filter(usable) |> 
  select(lsoa, treatment = treatment_parsed,
         house_type = house_type_parsed) |> 
  filter(house_type == 'Old') |> 
  count(treatment) |> 
  mutate(prop = n / sum(n))
