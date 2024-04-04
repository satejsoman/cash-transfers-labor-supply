library(tidyverse)
library(haven)
library(lme4)

# read in original data
data <- read_dta("dataverse_files/pooled_hh.dta")

# helper variables for legibility for stata users
i. <- as.factor
c. <- as.factor

# helper variables for formulae
taus <- "tau_1 + tau_2 + tau_3 + tau_4 + tau_5 + tau_7 + tau_8 + tau_9 + tau_10" # beta_6 defined to be 0, so only estimate tau != 6
covs <- "age + spanish + schooling + hhmale + nchild5 + urban + c.(age_sac)*i.(year) + c.(np)*i.(year) + i.(departamento)*i.(year)" # 'a*b' equivalent to 'a + b + a:b'


prepare <- function(data, tau_threshold = -5) { 
  # set up dummy variables and controls for regressions
  return (data 
   # keep only eligible households for DiD
   %>% filter(tau > -5) 
   
   # indicators household head age, language, and hours worked
   %>% mutate(
     age           = if_else(hhmale == 0, age_m,     age_p),
     spanish       = if_else(hhmale == 0, spanish_m, spanish_p),
     w_hoursw_head = rowSums(select(., w_hoursw_p, w_hoursw_m), na.rm = TRUE),
     work_head     = if_else(w_hoursw_head > 0, 1, NA_real_),
     work_head     = if_else(is.na(work_m) & is.na(work_p), NA_real_, work_head))
  )
}

estimate <- function(outcome, data){
  model <- lmer(
    nwork ~ tau_1 + tau_2 + tau_3 + tau_4 + tau_5 + tau_7 + tau_8 + tau_9 + tau_10 + (1 | departamento) + (1 | municipio), 
    weights = factor, data = data)
  
  return (summary(model))
}

extract_coefs <- function(model) { 

}

plot_coefs <- function(coefs) {
  
}

run_estimates <- function(data, tau_threshold, min_min_schooling, max_min_schooling){
  data <- prepare(data, tau_threshold)
  for (outcome in c()) {
    model <- estimate(outcome, data)
    coefs <- extract_coefs(model)
    plot_coefs(coefs)
  }
}

