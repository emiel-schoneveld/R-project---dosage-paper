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
wordreading_score_post ~ c(NA, NA, NA)*wordreading_score_pre + c(post_words_grade2, post_words_grade3, post_words_grade4)*practice_cii_words
practice_cii_words ~ c(words_time_grade2, words_time_grade3, words_time_grade4)*practice_cii_time
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
practice_cii_words ~ c(NA, 0, 0)*wordreading_score_pre
'

#### Combine into one specification
mod_full_v3 <- c(
    mod_regression_full,
    mod_covariance_intercept_structure,
    mod_modification_pre_on_time_grade23,
    mod_modification_pre_on_words_grade2
  )

### Fit modification 3 ----
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
      fit_full_v3) # Satorra-Bentler 2000 correction provides an impossible negative scaling factor.
lavTestLRT(fit_full_v2, 
           fit_full_v3, 
           method = "satorra.bentler.2010") # More robust comparison shows significant improvement of model fit.
fitmeasures(fit_full_v3, c('df', 'CFI', 'rmsea')) # no satisfactory fit based on RMSEA

### Inspect modification indices ----
modificationindices(fit_full_v3,
                    sort. = TRUE) |> 
  filter(
    mi > 3,
    op == '~'
  ) # first theoretically possible modification is effect of time on post, however this is the mediation analysis
# Second possibility is pre on words for grade 3

### Specify modification 4 (pre on words) ----
#### Specify modification
mod_modification_pre_on_words_grade23 <- '
practice_cii_words ~ c(NA, NA, 0)*wordreading_score_pre
'

#### Combine into one specification
mod_full_v4 <- c(
  mod_regression_full,
  mod_covariance_intercept_structure,
  mod_modification_pre_on_time_grade23,
  mod_modification_pre_on_words_grade23
)

### Fit modification 4 ----
fit_full_v4 <- sem(
  mod_full_v4,
  data = data, 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID"
)
# plot_lavaan(fit_full_v4,
            # where = "browser")

### Inspect modified model ----
anova(fit_full_v3,
      fit_full_v4) # No significant improvement of model fit
fitmeasures(fit_full_v4, c('df', 'CFI', 'rmsea')) # no satisfactory fit based on RMSEA
# Thus model 3 is the final model

## Partial mediation ----
### Specify model ----
#### Specify direct effect of time on post measurement
mod_modification_time_on_post <-'
# Direct regression effect of time on post
wordreading_score_post ~ c(post_time_grade2, post_time_grade3, post_time_grade4)*practice_cii_time
'

#### Specify total effect of time on post measurement
mod_modification_totaleffect_time_on_post <- '
# total effect of time on post
total_time_grade2 := post_time_grade2 + (post_words_grade2 * words_time_grade2)
total_time_grade3 := post_time_grade3 + (post_words_grade3 * words_time_grade3)
total_time_grade4 := post_time_grade4 + (post_words_grade4 * words_time_grade4)
'

#### Combine into complete model
mod_partial_v0 <- c(
  mod_full_v3,
  mod_modification_time_on_post,
  mod_modification_totaleffect_time_on_post
)

### Fit partial model ----
fit_partial_v0 <- sem(
  mod_partial_v0,
  data = data, 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID",
)
# plot_lavaan(fit_partial_V0, where = "browser")

### Inspect partial model ----
anova(
  fit_full_v3,
  fit_partial_v0
) # significant improvement of model fit
fitmeasures(fit_partial_v0, c('df', 'chisq', 'pvalue', 'cfi', 'rmsea')) # no satisfactory fit of rmsea, but way better
resid(fit_partial_v0, type = "cor")

# Thus partial mediation model is the final model

### Inspect final model ----
# Inspecting significant parameter estimates when correcting for multiple testing 
parameterestimates(
  fit_partial_v0, 
  standardized = T) |> 
  mutate(
    pvalue_cor = pvalue * 3, # bonferoni adjustment for three groups of testing
    sig_cor = pvalue_cor < .05
  ) |>
  filter(
    op %in% c('~', ':=')
    # sig_cor,
  ) |> 
  dplyr::select(
    lhs:rhs, group, est, se, pvalue, std.all, pvalue_cor, sig_cor
  ) |> 
  arrange(
    # sig_cor,
    lhs, rhs, group
  )
# Positive effect of time on words and of words on post meaning: students who practised more time, 
# read more words and improved their word reading more

# negative direct effect of time on post meaning: controlling for how many words a student has read and their reading fluency at pre, 
# students who practised for more total time scored lower at posttest. However, only significant for grade 2 after adjusting for 
# multiple testing.
# The total effect of time was only significant for Grade 2. It was also robust against multiple testing adjustment. 
# In this grade, more time practising led to more progress.

plot_lavaan(fit_partial_v0, where = "browser")



# Saturated model ----
## Specify saturated path model
mod_regression_saturated <- '
# regressions
wordreading_score_post ~ wordreading_score_pre + c(post_words_grade2, post_words_grade3, post_words_grade4)*practice_cii_words + c(post_time_grade2, post_time_grade3, post_time_grade4)*practice_cii_time
practice_cii_words ~ c(words_time_grade2, words_time_grade3, words_time_grade4)*practice_cii_time + c(words_pre_grade2, words_pre_grade3, words_pre_grade4)*wordreading_score_pre
practice_cii_time ~ c(time_pre_grade2, time_pre_grade3, time_pre_grade4)*wordreading_score_pre

# constraints
words_pre_grade4 == 0
time_pre_grade4 == 0
# time_pre_grade2 == 0
'

mod_saturated <- c(
  mod_regression_saturated,
  mod_covariance_intercept_structure,
  mod_modification_totaleffect_time_on_post
)

## Fit saturated path model
fit_saturated <- sem(
  mod_saturated,
  data = data, 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID",
)
# plot_lavaan(fit_saturated, where = "browser")

anova(
  fit_old,
  fit_saturated
) # significant improvement of model fit
fit_old <- fit_saturated
fitmeasures(fit_saturated, c('df', 'chisq', 'pvalue', 'cfi', 'rmsea')) # perfect fit because saturated model
# resid(fit_saturated, type = "cor") # no residual correlations because saturated model

parameterestimates(fit_saturated) |> 
  filter(
    op == '~',
  ) |> 
  arrange(
    desc(pvalue)
  ) |> 
  slice_head(n = 4)

summary(fit_saturated)
parameterestimates(fit_saturated) |> 
  filter(
    op == '~' | op == ':='
  ) |> 
  arrange(
    lhs, rhs, group,
  ) |> 
  dplyr::select(
    # lhs, rhs, 
    label, est, se, pvalue
  ) |> 
  mutate(
    p_cor = pvalue*3,
    sig_cor = p_cor < .05
  )
