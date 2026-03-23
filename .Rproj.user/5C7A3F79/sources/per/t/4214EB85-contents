# Analyses 
# General syntax ----
## Clear environment
# rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(lavaan)
library(here)
library(lavaangui)

# Load data ----
data <- readRDS(
  here('output/data_transformed.rds')
)

# Analysis ----
## Specify model parts ----
### specify regression part
mod_splitted_regression_full <- '
# regressions
wordreading_score_post ~ wordreading_score_pre + practice_cii_words
practice_cii_words ~ practice_cii_time
practice_cii_time ~ practice_length + practice_frequency + practice_duration
'

### specify covariance and intercept part
mod_splitted_covariance_intercept_structure <- ' 
# variances and covariances 
wordreading_score_pre ~~ wordreading_score_pre
wordreading_score_post ~~ wordreading_score_post
practice_cii_words ~~ practice_cii_words
practice_cii_time ~~ practice_cii_time
practice_length ~~ practice_length
practice_frequency ~~ practice_frequency
practice_duration ~~ practice_duration

# Intercepts
wordreading_score_pre ~ 1
wordreading_score_post ~ 1
practice_cii_words ~ 1
practice_cii_time ~ 1
practice_length ~ 1
practice_frequency ~ 1
practice_duration ~ 1
'

### combine into one specification
mod_splitted_full_v0 <- c(
  mod_splitted_regression_full,
  mod_splitted_covariance_intercept_structure
)

## Fit full mediation model ----
fit_splitted_full_v0 <- sem(
  mod_splitted_full_v0, 
  data = data, 
  missing = "FIML",
  group = "grade",
  cluster = "class_ID"
)
# plot_lavaan(fit_splitted_full_v0, where = 'browser')

### Inspect full mediation model ----
fitmeasures(fit_splitted_full_v0, c('df', 'chisq', 'pvalue', 'CFI', 'rmsea')) # no satisfactory fit based on cfi and rmsea

## Modify model following modification indices ----
### Inspect modification indices ----
modificationindices(fit_splitted_full_v0,
                    sort. = TRUE) |> 
  filter(
    mi > 3
  ) |> 
  slice_head(n = 10)# first theoretically possible modification is covariance between freq and duration

### Specify modification 1 (covariance between frequency and duration) ----
#### Specify modification
mod_splitted_modification_freq_and_dur <- '
practice_frequency ~~ practice_duration
'
#### Combine into one specification
mod_splitted_full_v1 <- c(
  mod_splitted_full_v0,
  mod_splitted_modification_freq_and_dur
)

### Fit modification 1 ----
fit_splitted_full_v1 <- sem(
  mod_splitted_full_v1, 
  data = data, 
  missing = "FIML",
  group = "grade",
  cluster = "class_ID"
)
# plot_lavaan(fit_splitted_full_v1,
#             where = "browser")

### Inspect modified model ----
anova(fit_splitted_full_v0,
      fit_splitted_full_v1) # significant increase in model fit
fitmeasures(fit_splitted_full_v1, c('df', 'CFI', 'rmsea')) # no satisfactory fit based on cfi and rmsea

### Inspect modification indices ----
modificationindices(fit_splitted_full_v1,
                    sort. = TRUE) |> 
  filter(
    mi > 3
  ) |> 
  slice_head(n = 10)# first theoretically possible modification is effect of pre on words

### Specify modification 2 (covariance between frequency and duration) ----
#### Specify modification
mod_splitted_modification_pre_on_words <- '
wordreading_score_pre ~ practice_cii_words
'
#### Combine into one specification
mod_splitted_full_v2 <- c(
  mod_splitted_full_v1,
  mod_splitted_modification_pre_on_words
)

### Fit modification 1 ----
fit_splitted_full_v2 <- sem(
  mod_splitted_full_v2, 
  data = data, 
  missing = "FIML",
  group = "grade",
  cluster = "class_ID"
)
plot_lavaan(fit_splitted_full_v2,
            where = "browser")

### Inspect modified model ----
anova(fit_splitted_full_v0,
      fit_splitted_full_v1) # significant increase in model fit
fitmeasures(fit_splitted_full_v1, c('df', 'CFI', 'rmsea')) # no satisfactory fit based on cfi and rmsea
