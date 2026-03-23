# Analyses performed for SSSR abstract

# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(lavaan)
library(here)

# Load data ----
data <- readRDS(
  here('output/data_transformed.rds')
)

# Descriptive analysis ----
## Summary ----
summary(data)

## Correlation table
data |> 
  dplyr::select(
    where(is.numeric)
  ) |> 
  cor(
    use = "pairwise.complete.obs"
  ) #|> View()

data |> 
  dplyr::select(
    contains('practice'),
    contains('wordreading')
  ) |> 
  cor(
    use = "pairwise.complete.obs"
  ) #|> View()

data |> 
  group_by(grade) |> 
  summarise(
    mean(wordreading_score_pre, na.rm = T)
  )

# SSSR -----
# Mediation analysis ----
## Full mediation analysis (summed time) ----
### Specify full mediation model ----
mod_full <- '
# regressions
wordreading_score_post ~ wordreading_score_pre + practice_cii_words
practice_cii_words ~ practice_cii_time
practice_cii_time ~ wordreading_score_pre

# variances and covariances 
wordreading_score_pre ~~ wordreading_score_pre
wordreading_score_post ~~ wordreading_score_post
practice_cii_words ~~ practice_cii_words
practice_cii_time ~~ practice_cii_time

# Intercepts
wordreading_score_pre ~ 1
wordreading_score_post ~ 1
practice_cii_words ~ 1
practice_cii_time ~ 1
'

### Fit full mediation model ----
fit_full <- sem(
  mod_full, 
  data = data, 
  missing = "FIML",
  # group = "grade",
  cluster = "class_ID",
  # group.equal = c('regressions')
)

### Inspect full mediation model ----
summary(fit_full, standardized = TRUE, fit.measures = T)
fitmeasures(fit_full, c('CFI', 'rmsea'))
modificationindices(fit_full,
                    sort. = TRUE)

## Partial mediation
### Specify model additions ----
#### Specify direct effect of premeasurement on words read ----
mod_add_partial_words_pre <-'
# Direct regression effects of premeasurement on words
practice_cii_words ~ wordreading_score_pre
'

#### Specify direct effect of time on post measurement ----
mod_add_partial_post_time <-'
# Direct regression effects of time on post
wordreading_score_post ~ practice_cii_time
'

### Testing singular additions of partial mediation ----
#### Partial mediation 1: words ~ pre ----
##### Fit model ----
fit_partial_words_pre <- sem(
  c(mod_full, mod_add_partial_words_pre),
  data = data, 
  missing = "FIML",
  # group = "grade",
  cluster = "class_ID",
  # group.equal = c('regressions')
)

##### Inspect partial mediation model ----
anova(fit_full, fit_partial_words_pre)
summary(fit_partial_words_pre, standardized = TRUE, fit.measures = T)
fitmeasures(fit_partial_words_pre, c('CFI', 'rmsea'))

#### Partial mediation 2: post ~ time ----
##### Fitting model ----
fit_partial_post_time <- sem(
  c(mod_full, mod_add_partial_post_time),
  data = data, 
  missing = "FIML",
  # group = "grade",
  cluster = "class_ID",
  # group.equal = c('regressions')
)

##### Inspect partial mediation model ----
anova(fit_full, fit_partial_post_time)
summary(fit_partial_post_time, standardized = TRUE, fit.measures = T)
fitmeasures(fit_partial_post_time, c('CFI', 'rmsea'))

### Adding direct effect of time on post after adding effect of pre on words ----
#### Partial mediation 2: post ~ time ----
#### Fitting model ----
fit_partial_all <- sem(
  c(mod_full, mod_add_partial_words_pre, mod_add_partial_post_time),
  data = data, 
  missing = "FIML",
  # group = "grade",
  cluster = "class_ID",
  # group.equal = c('regressions')
)

##### Inspect partial mediation model ----
anova(fit_partial_all, fit_partial_words_pre)
anova(fit_partial_all, fit_partial_post_time)

summary(fit_partial_all, standardized = TRUE, fit.measures = T)
fitmeasures(fit_partial_all, c('CFI', 'rmsea'))

## inspecting all tests ----
### Comparing full mediation model with model that includes effect of time on post
anova(fit_full,
      fit_partial_post_time)

anova(fit_partial_post_time,
      fit_partial_all)

anova(fit_full,
      fit_partial_words_pre)

anova(fit_partial_words_pre,
      fit_partial_all)

summary(fit_partial_words_pre, std = T)

summary(data)
data |> 
  group_by(grade) |> 
  summarise(n())

