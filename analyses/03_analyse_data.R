# Analyses 
# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(lavaan)
library(here)
library(lavaangui)
library(shiny)

# Load data ----
data <- readRDS(
  here('output/data_transformed.rds')
)

# Mediation analysis ----
## Full mediation analysis (summed time) ----
### Specify full mediation model ----
#### specify regression part
mod_regression_full <- '
# regressions
wordreading_score_post ~ wordreading_score_pre + practice_cii_words
practice_cii_words ~ practice_cii_time
'

#### specify covariance and intercept part
mod_covariance_intercept_structure <- ' 
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

#### combine into one specification
mod_full_v0 <- c(
  mod_regression_full,
  mod_covariance_intercept_structure
)

### Fit full mediation model ----
fit_full_v0 <- sem(
  mod_full_v0,
  data = data, 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID"
)
# plot_lavaan(fit_full_v0,
#             where = "browser")

### Inspect full mediation model ----
fitmeasures(fit_full_v0, c('df', 'CFI', 'rmsea')) # no satisfactory fit based on cfi and rmsea

## Modify model following modification indices ----
### Inspect modification indices ----
modificationindices(fit_full_v0,
                    sort. = TRUE) |> 
  filter(
    mi > 3
  ) # first theoretically possible modification is effect of pre on time grade 3

### Specify modification 1 (pre on time) ----
#### Specify modification
mod_modification_pre_on_time_grade3 <- '
practice_cii_time ~ c(0, NA, 0)*wordreading_score_pre
'
#### Combine into one specification
mod_full_v1 <- c(
  mod_regression_full,
  mod_covariance_intercept_structure,
  mod_modification_pre_on_time_grade3
)

### Fit modification 1 ----
fit_full_v1 <- sem(
  mod_full_v1, 
  data = data, 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID"
)
# plot_lavaan(fit_full_v1,
            # where = "browser")

### Inspect modified model ----
anova(fit_full_v0,
      fit_full_v1) # significant increase in model fit
fitmeasures(fit_full_v1, c('df', 'CFI', 'rmsea')) # no satisfactory fit based on rmsea

### Inspect modification indices ----
modificationindices(fit_full_v1,
                    sort. = TRUE) |> 
  filter(
    mi > 3,
    op == '~'
  ) # first theoretically possible modification is effect of pre on time grade 2

### Specify modification 2 (pre on time) ----
#### Specify modification
mod_modification_pre_on_time_grade23 <- '
practice_cii_time ~ c(NA, NA, 0)*wordreading_score_pre
'
#### Combine into one specification
mod_full_v2 <- c(
  mod_regression_full,
  mod_covariance_intercept_structure,
  mod_modification_pre_on_time_grade23
)

### Fit modification 2 ----
fit_full_v2 <- sem(
  mod_full_v2,
  data = data, 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID"
)
# plot_lavaan(fit_full_v2,
#             where = "browser")

### Inspect modified model ----
anova(fit_full_v1,
      fit_full_v2) # significant improvement of model fit
fitmeasures(fit_full_v2, c('df', 'CFI', 'rmsea')) # no satisfactory fit based on RMSEA

### Inspect modification indices ----
modificationindices(fit_full_v2,
                    sort. = TRUE) |> 
  filter(
    mi > 3,
    op == '~'
  ) # first theoretically possible modification is effect of pre on words grade 2

### Specify modification 3 (pre on words) ----
#### Specify modification
mod_modification_pre_on_words_grade2 <- '
practice_cii_words ~ c(0, NA, NA)*wordreading_score_pre
'
#### Combine into one specification
mod_full_v3 <- c(
    mod_regression_full,
    mod_covariance_intercept_structure,
    mod_modification_pre_on_time_grade23,
    mod_modification_pre_on_words_grade2
  )

### Fit modification 2 ----
fit_full_v3 <- sem(
  mod_full_v3,
  data = data, 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID"
)
# plot_lavaan(fit_full_v2,
#             where = "browser")

### Inspect modified model ----
anova(fit_full_v2,
      fit_full_v3) # no significant improvement of model fit
fitmeasures(fit_full_v2, c('df', 'CFI', 'rmsea')) # no satisfactory fit based on RMSEA

resid(fit_full_v2, type = "cor") # biggest misfit is in the correlation between pre and words in grade 3
# This was the addition made by modification 3. However, this did not lead to a significant improvement in model fit.
# Therefore, final model is fit_full_v2

## Partial mediation ----
### Specify model ----
#### Specify direct effect of time on post measurement
mod_modification_time_on_post <-'
# Direct regression effect of time on post
wordreading_score_post ~ practice_cii_time
'

#### Combine into complete model
mod_partial_V0 <- c(
  mod_full_v2,
  mod_modification_time_on_post
)

### Fit partial model ----
fit_partial_V0 <- sem(
  mod_partial_V0,
  data = data, 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID",
)
# plot_lavaan(fit_partial_V0, where = "browser")

### Inspect partial model ----
anova(
  fit_full_v2,
  fit_partial_V0
) # significant improvement of model fit
fitmeasures(fit_partial_V0, c('df', 'chisq', 'pvalue', 'cfi', 'rmsea')) # no satisfactory fit of rmsea

# Thus partial mediation of the effect of total practice time on the posttest through the number of words read
# But also a direct effect

### Inspect final model ----
# Inspecting significant parameter estimates when correcting for multiple testing 
parameterestimates(fit_partial_V0, standardized = T) |> 

  mutate(
    pvalue_cor = pvalue * 3, # bonferoni adjustment for three groups of testing
    sig_cor = pvalue_cor < .05
  ) |>
  filter(
    op == '~',
    # sig_cor,
  ) |> 
  dplyr::select(
    lhs:rhs, group, est, se, pvalue, std.all, pvalue_cor, sig_cor
  ) |> 
  arrange(
    lhs, rhs, group
  )

plot_lavaan(fit_partial_V0, where = "browser")
