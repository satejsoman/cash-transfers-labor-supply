library(tidyverse)
library(haven)
library(lme4)

# read in original data
data <- read_dta("dataverse_files/pooled_hh.dta")


# set up dummy variables and controls for regressions
(data 
  # keep only eligible households for DiD
  %>% filter(tau > -5) 
  
  # indicators household head age, language, and hours worked
  %>% mutate(
    age           = if_else(hhmale == 0, age_m,     age_p),
    spanish       = if_else(hhmale == 0, spanish_m, spanish_p),
    w_hoursw_head = rowSums(select(., w_hoursw_p, w_hoursw_m), na.rm = TRUE),
    work_head     = if_else(w_hoursw_head > 0, 1, NA_real_),
    work_head     = if_else(is.na(work_m) & is.na(work_p), NA_real_, work_head))
) -> data

model <- lmer(
  nwork ~ tau_1 + tau_2 + tau_3 + tau_4 + tau_5 + tau_7 + tau_8 + tau_9 + tau_10 + (1 | departamento) + (1 | municipio), 
  data = data, weights = factor)

summary(model)
