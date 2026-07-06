# Mediation Analysis
# By Emiel Schoneveld
# General syntax ----
## Clear environment
# rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(here)
library(ggplot2)
library(lavaan)
library(lavaanPlot)
library(broom)
library(effectsize)
library(patchwork)

## Load functions and themes
source(
  here::here('analyses/00_functions_themes_analysis.R')
)

# Load data ----
data <- readRDS(
  here::here('output/data_cleaned.rds')
)

# Mediation analysis ----
## Specify model ----
### Full mediation model
mod_fullmediation <- '
# regression
## Predicting reading
fluency_post ~ fluency_pre + c(post_words_2, post_words_3, post_words_4)*words_exposures

## Predicting total words
words_exposures ~ c(words_time_2, words_time_3, words_time_4)*time

## Covariances
fluency_pre ~~ time + words_exposures

# variances
fluency_pre ~~ fluency_pre
fluency_post ~~ fluency_post
words_exposures ~~ words_exposures
time ~~ time
'

### Partial mediation addition
mod_addition_partialmediation <- '
fluency_post ~ c(post_time_2, post_time_3, post_time_4)*time
'

### Total effects addition
mod_addition_totaltimeeffect_partialmediation <- '
post_time_total_2 := (post_words_2*words_time_2) + post_time_2
post_time_total_3 := (post_words_3*words_time_3) + post_time_3
post_time_total_4 := (post_words_4*words_time_4) + post_time_4
'
mod_addition_totaltimeeffect_fullmediation <- '
post_time_total_2 := post_words_2*words_time_2
post_time_total_3 := post_words_3*words_time_3
post_time_total_4 := post_words_4*words_time_4
'

## Fit models ----
### DMT ----
fit_partialmediation_DMT <- sem(
  c(
    mod_fullmediation,
    mod_addition_partialmediation,
    mod_addition_totaltimeeffect_partialmediation
    ),
  data = data |> 
    mutate(
      words_exposures = words_exposures / 1000,
      fluency_pre = fluency_DMT_pre,
      fluency_post = fluency_DMT_post,
    )
  , 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID",
)

fit_fullmediation_DMT <- sem(
  c(
    mod_fullmediation,
    # mod_addition_partialmediation,
    mod_addition_totaltimeeffect_fullmediation
  ),
  data = data |> 
    mutate(
      words_exposures = words_exposures / 1000,
      fluency_pre = fluency_DMT_pre,
      fluency_post = fluency_DMT_post,
    )
  , 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID",
)

fitMeasures(fit_fullmediation_DMT)[c('cfi', 'rmsea')]

anova_mediation_DMT <- anova(
  fit_partialmediation_DMT,
  fit_fullmediation_DMT
)

# p value after correction
(anova_mediation_DMT$`Pr(>Chisq)`[2]*3)

estimates_mediation_DMT <- parameterEstimates(
  fit_fullmediation_DMT,
  standardized = T
  ) |> 
  dplyr::select(
    lhs, op, rhs, group, label, est, se, pvalue, ci.lower, ci.upper, std.all
  ) |> 
  filter(
    op %in% c('~', ':=')
  ) |> 
  arrange(
    lhs, rhs, group
  ) |> 
  mutate(
    outcome = 'DMT',
    .before = 1
  )

### discrete ----
fit_partialmediation_discrete <- sem(
  c(
    mod_fullmediation,
    mod_addition_partialmediation,
    mod_addition_totaltimeeffect_partialmediation
  ),
  data = data |> 
    mutate(
      words_exposures = words_exposures / 1000,
      fluency_pre = fluency_discrete_pre,
      fluency_post = fluency_discrete_post,
    )
  , 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID",
)

fit_fullmediation_discrete <- sem(
  c(
    mod_fullmediation,
    # mod_addition_partialmediation,
    mod_addition_totaltimeeffect_fullmediation
  ),
  data = data |> 
    mutate(
      words_exposures = words_exposures / 1000,
      fluency_pre = fluency_discrete_pre,
      fluency_post = fluency_discrete_post,
    )
  , 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID",
)

anova_mediation_discrete <- anova(
  fit_partialmediation_discrete,
  fit_fullmediation_discrete
)

# p value after correction
(anova_mediation_discrete$`Pr(>Chisq)`[2]*3)

estimates_mediation_discrete <- parameterEstimates(
  fit_fullmediation_discrete,
  standardized = T
) |> 
  dplyr::select(
    lhs, op, rhs, group, label, est, se, pvalue, ci.lower, ci.upper, std.all
  ) |> 
  filter(
    op %in% c('~', ':=')
  ) |> 
  arrange(
    lhs, rhs, group
  ) |> 
  mutate(
    outcome = 'discrete',
    .before = 1
  )
 
### serial ----
fit_partialmediation_serial <- sem(
  c(
    mod_fullmediation,
    mod_addition_partialmediation,
    mod_addition_totaltimeeffect_partialmediation
  ),
  data = data |> 
    mutate(
      words_exposures = words_exposures / 1000,
      fluency_pre = fluency_serial_pre,
      fluency_post = fluency_serial_post,
    )
  , 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID",
)

fit_fullmediation_serial <- sem(
  c(
    mod_fullmediation,
    # mod_addition_partialmediation,
    mod_addition_totaltimeeffect_fullmediation
  ),
  data = data |> 
    mutate(
      words_exposures = words_exposures / 1000,
      fluency_pre = fluency_serial_pre,
      fluency_post = fluency_serial_post,
    )
  , 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID",
)

anova_mediation_serial <- anova(
  fit_partialmediation_serial,
  fit_fullmediation_serial
)

# p value after correction
(anova_mediation_serial$`Pr(>Chisq)`[2]*3)

estimates_mediation_serial <- parameterEstimates(
  fit_fullmediation_serial,
  standardized = T
) |> 
  dplyr::select(
    lhs, op, rhs, group, label, est, se, pvalue, ci.lower, ci.upper, std.all
  ) |> 
  filter(
    op %in% c('~', ':=')
  ) |> 
  arrange(
    lhs, rhs, group
  ) |> 
  mutate(
    outcome = 'serial',
    .before = 1
  )

### LED ----
# fit_partialmediation_LED <- sem(
#   c(
#     mod_fullmediation,
#     mod_addition_partialmediation,
#     mod_addition_totaltimeeffect_partialmediation
#   ),
#   data = data |> 
#     mutate(
#       words_exposures = words_exposures / 1000,
#       fluency_pre = fluency_LED_pre,
#       fluency_post = fluency_LED_post,
#     )
#   , 
#   missing = "FIML",
#   group = "grade",
#   group.label = c('grade_2', 'grade_3', 'grade_4'),
#   cluster = "class_ID",
# )
# 
# fit_fullmediation_LED <- sem(
#   c(
#     mod_fullmediation,
#     # mod_addition_partialmediation,
#     mod_addition_totaltimeeffect_fullmediation
#   ),
#   data = data |> 
#     mutate(
#       words_exposures = words_exposures / 1000,
#       fluency_pre = fluency_LED_pre,
#       fluency_post = fluency_LED_post,
#     )
#   , 
#   missing = "FIML",
#   group = "grade",
#   group.label = c('grade_2', 'grade_3', 'grade_4'),
#   cluster = "class_ID",
# )
# 
# anova_mediation_LED <- anova(
#   fit_partialmediation_LED,
#   fit_fullmediation_LED
# )
# 
# # p value after correction
# (anova_mediation_LED$`Pr(>Chisq)`[2]*3)
# 
# estimates_mediation_LED <- parameterEstimates(
#   fit_fullmediation_LED,
#   standardized = T
# ) |> 
#   dplyr::select(
#     lhs, op, rhs, group, label, est, se, pvalue, ci.lower, ci.upper, std.all
#   ) |> 
#   filter(
#     op %in% c('~', ':=')
#   ) |> 
#   arrange(
#     lhs, rhs, group
#   ) |> 
#   mutate(
#     outcome = 'LED',
#     .before = 1
#   )

### pseudo ----
# fit_partialmediation_pseudo <- sem(
#   c(
#     mod_fullmediation,
#     mod_addition_partialmediation,
#     mod_addition_totaltimeeffect_partialmediation
#   ),
#   data = data |> 
#     mutate(
#       words_exposures = words_exposures / 1000,
#       fluency_pre = fluency_pseudo_pre,
#       fluency_post = fluency_pseudo_post,
#     )
#   , 
#   missing = "FIML",
#   group = "grade",
#   group.label = c('grade_2', 'grade_3', 'grade_4'),
#   cluster = "class_ID",
# )
# 
# fit_fullmediation_pseudo <- sem(
#   c(
#     mod_fullmediation,
#     # mod_addition_partialmediation,
#     mod_addition_totaltimeeffect_fullmediation
#   ),
#   data = data |> 
#     mutate(
#       words_exposures = words_exposures / 1000,
#       fluency_pre = fluency_pseudo_pre,
#       fluency_post = fluency_pseudo_post,
#     )
#   , 
#   missing = "FIML",
#   group = "grade",
#   group.label = c('grade_2', 'grade_3', 'grade_4'),
#   cluster = "class_ID",
# )
# 
# anova_mediation_pseudo <- anova(
#   fit_partialmediation_pseudo,
#   fit_fullmediation_pseudo
# )
# 
# # p value after correction
# (anova_mediation_pseudo$`Pr(>Chisq)`[2]*3)
# 
# estimates_mediation_pseudo <- parameterEstimates(
#   fit_partialmediation_pseudo,
#   standardized = T
# ) |> 
#   dplyr::select(
#     lhs, op, rhs, group, label, est, se, pvalue, ci.lower, ci.upper, std.all
#   ) |> 
#   filter(
#     op %in% c('~', ':=')
#   ) |> 
#   arrange(
#     lhs, rhs, group
#   ) |> 
#   mutate(
#     outcome = 'pseudo',
#     .before = 1
#   )

## Inspect results ----
### Bind results of mediation models (and adding missing values explicitely for plotting purposes) ----
estimates_mediation <- bind_rows(
  estimates_mediation_DMT,
  estimates_mediation_discrete,
  estimates_mediation_serial,
  # estimates_mediation_LED,
  # estimates_mediation_pseudo,
) |> as_tibble() |> 
  mutate(
    significant = case_when(
    pvalue < .05/9 ~ 'Significant',
    pvalue >= .05/9 ~ 'Not significant'
  ),
    group = case_when(
      group == 1 ~ 'grade_2',
      group == 2 ~ 'grade_3',
      group == 3 ~ 'grade_4',
    ),
  outcome = case_when(
    outcome == 'DMT' ~ 'serial_normed',
    outcome == 'serial' ~ 'serial_exp',
    outcome == 'discrete' ~ 'discrete_exp'
  ),
  outcome = factor(outcome, levels = c('serial_normed', 'serial_exp', 'discrete_exp'))
  ) |> 
  rename(
    grade = group
  )

### Extract missing combos
# missing_combos <- estimates_mediation |> 
#   filter(
#     str_detect(op, '~'),
#     str_detect(lhs, 'post'),
#     str_detect(rhs, 'time'),
#   ) |> 
#   add_row(
#     outcome = c(
#       'serial_exp',
#       'discrete_exp',
#       'serial_exp',
#       'LED',
#       'pseudo'
#       ),
#     lhs = c('fluency_post', 'fluency_post', 'fluency_post'),
#     rhs = c('time', 'time', 'time'),
#     op = c('~', '~', '~')
#   ) |> 
#   expand(
#     outcome, op, lhs, rhs, grade
#   ) |> 
#   drop_na() |> 
#   filter(
#     !(outcome %in% c('pseudo'))
#   )
# 
# ### Add missing combos
# estimates_mediation <- estimates_mediation |> 
#   bind_rows(
#     missing_combos
#   )

### Bind anova comparisons ----
estimates_mediation_comparison <- bind_rows(
  c('outcome' = 'serial_normed', unlist(anova_mediation_DMT)),
  c("outcome" = "discrete_exp", unlist(anova_mediation_discrete)),
  c("outcome" = "serial_exp", unlist(anova_mediation_serial)),
  # c("outcome" = "LED", unlist(anova_mediation_LED)),
  # c("outcome" = "pseudo", unlist(anova_mediation_pseudo)),
) |> 
  rename(
    'chisq_diff' = `Chisq diff2`,
    'pvalue' = `Pr(>Chisq)2`,
    'df_diff' = Df2
  ) |> 
  dplyr::select(
    outcome, df_diff, chisq_diff, pvalue
  ) |> 
  mutate(
    significant = case_when(
      pvalue < .05/3 ~ 'Significant',
      pvalue >= .05/3 ~ 'Not significant'
      ),
    grade = '',
    outcome = factor(outcome, levels = c('serial_normed', 'serial_exp', 'discrete_exp'))
    )

### Plot results ----
p_mediation_postpre <- estimates_mediation |> 
  filter(
    str_detect(op, '~'),
    str_detect(lhs, 'post'),
    str_detect(rhs, 'pre'),
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = significant,
      label = std.all |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text(color = 'white', size = 8) +
  ggtitle('Pre -> post') +
  common_theme_labs +
  labs(x = NULL) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('Significant' = sig_color, 'Not significant' = insig_color))

p_mediation_postwords <- estimates_mediation |> 
  filter(
    str_detect(op, '~'),
    str_detect(lhs, 'post'),
    str_detect(rhs, 'words'),
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = significant,
      label = std.all |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text(color = 'white', size = 8) +
  ggtitle('Words -> post') +
  common_theme_labs +
  labs(x = NULL) +
  scale_fill_manual(values = c('Significant' = sig_color, 'Not significant' = insig_color))

p_mediation_wordstime <- estimates_mediation |> 
  filter(
    str_detect(op, '~'),
    str_detect(lhs, 'words'),
    str_detect(rhs, 'time'),
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = significant,
      label = std.all |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text(color = 'white', size = 8) +
  ggtitle('Time -> words') +
  common_theme_labs +
  labs(x = NULL) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('Significant' = sig_color, 'Not significant' = insig_color))

p_mediation_posttime <- estimates_mediation |> 
  filter(
    str_detect(op, '~'),
    str_detect(lhs, 'post'),
    str_detect(rhs, 'time'),
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = significant,
      label = std.all |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text(color = 'white', size = 8) +
  ggtitle('Time -> post') +
  common_theme_labs +
  labs(x = NULL) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('Significant' = sig_color, 'Not significant' = insig_color))

p_mediation_posttimetotal <- estimates_mediation |> 
  filter(
    str_detect(op, ':='),
    str_detect(label, 'post_time_total'),
  ) |> 
  mutate(
    grade = str_c(
      'grade_',
      str_sub(label, -1, -1)
    )
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = significant,
      label = std.all |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text(color = 'white', size = 8) +
  ggtitle('Time -> post (total effect)') +
  common_theme_labs +
  labs(x = NULL) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('Significant' = sig_color, 'Not significant' = insig_color))

p_comparison <- estimates_mediation_comparison |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = significant
      )
  ) +
  geom_tile(color = "black") +
  ggtitle('Full vs partial mediation') +
  common_theme_labs +
  theme(
    axis.text.x = element_blank()
  ) +
  labs(x = NULL) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c('Significant' = sig_color, 'Not significant' = insig_color))

p_mediation <- (p_mediation_postpre +
  p_mediation_wordstime +
  p_mediation_postwords +
  p_mediation_posttime +
  p_mediation_posttimetotal +
  p_comparison +
  plot_layout(
    axis = 'collect',
    guides = 'collect'
    )) &
  scale_fill_manual(values = c('Significant' = sig_color, 'Not significant' = insig_color))

# Plot all results ----
p_mediation_formatted <- p_mediation +
  plot_annotation(
  title = 'Mediation analysis',
)

