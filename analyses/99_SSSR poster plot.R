# SSSR poster plot
# source(
#   here::here('analyses/09_analysis_interaction.R')
# )

# Plot parameters ----
## Set colors ----
color_mean = '#1B1918'
color_plus_1SD = "#257835"
color_min_1SD = '#bc0031'
color_background = "#D7D6D4"

## set sizes ----
title_fontsize = 22
axes_fontsize = 20
line_size = 3
key_size = 2
plot_height = 19
plot_width = 26


## Set theme ----
common_theme_SSSR_plots <- theme_bw() +
  theme(
    # Titles
    plot.title = element_text(hjust = 0.5, size = title_fontsize),
    axis.title = element_text(size = title_fontsize),
    
    # Axis
    axis.text = element_text(size = axes_fontsize),
    axis.text.x = element_text(angle = 45, hjust = 1),
    
    # Background
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = color_background, color = NA),
    
    # Gridlines
    panel.grid.major = element_line(color = color_background),
    panel.grid.minor = element_blank(),
    
    # Legend
    legend.background = element_rect(fill = color_background, color = NA),
    legend.text = element_text(size = axes_fontsize),
    legend.title = element_text(size = title_fontsize),
    legend.key.size = unit(key_size, "lines"),
    
    # Strip
    strip.text = element_text(size = title_fontsize),
    strip.background = element_rect(
      fill = color_background,
      color = NA
    ),
  ) 

# Plot main accuracy effect ----
## Build a tibble with slopes and intercepts ----
data_plotting_postaccuracy <- estimates_interaction_standardized |> 
  filter(
    str_detect(term, 'Intercept') |
      str_detect(term, 'accuracy_anytry') |
      str_detect(term, ':'),
  ) |> 
  dplyr::select(
    outcome, grade, term, std_estimate, significant
  ) |> 
  rename(
    estimate = std_estimate,
    Significance = significant,
    Grade = grade
  ) |> 
  mutate(
    term = case_when(
      term == '(Intercept)' ~ 'intercept',
      term == 'accuracy_anytry' ~ 'slope',
      term == 'words_exposures:accuracy_anytry' ~ 'interaction',
    ),
    Significance = factor(Significance, levels = c('Significant', 'Not significant')),
    Grade = case_when(
      Grade == 'grade_2' ~ 'Grade 2',
      Grade == 'grade_3' ~ 'Grade 3',
      Grade == 'grade_4' ~ 'Grade 4',
    ),
    Grade = factor(Grade, levels = c('Grade 2', 'Grade 3', 'Grade 4'))
  ) |> 
  pivot_wider(
    names_from = term,
    values_from = c(estimate, Significance)
  ) |> 
  rename(
    estimate_slope_mean = estimate_slope
  )

## Format the data ----
data_plotting_postaccuracy_long <- data_plotting_postaccuracy |> 
  mutate(
    estimate_slope_min1SD = estimate_slope_mean - estimate_interaction,
    estimate_slope_plus1SD = estimate_slope_mean + estimate_interaction,
  ) |> 
  pivot_longer(
    contains('estimate_slope_'),
    names_to = 'Words read',
    names_prefix = 'estimate_slope_',
    values_to = 'estimate_slope',
  ) |> 
  mutate(
    `Words read` = case_when(
      `Words read` == 'mean' ~ 'Mean',
      `Words read` == 'min1SD' ~ '-1 SD',
      `Words read` == 'plus1SD' ~ '+1 SD',
    ),
    `Words read` = factor(`Words read`, levels = c('-1 SD', 'Mean', '+1 SD'))
  )

## Basic plot ----
p_moderation_postaccuracy_basic <- 
  ggplot() +
  geom_abline(
    data = data_plotting_postaccuracy_long |>
      filter(
        str_detect(`Words read`, 'SD')
      ),
    aes(
      intercept = estimate_intercept,
      slope = estimate_slope,
      linetype = Significance_interaction,
      color = `Words read`
    ),
    linewidth = line_size
  ) +
  geom_abline(
    data = data_plotting_postaccuracy_long |> 
      filter(
        `Words read` == 'Mean'
      ),
    aes(
      intercept = estimate_intercept,
      slope = estimate_slope,
      linetype = Significance_slope,
      color = `Words read`
    ),
    linewidth = line_size
  ) +
  expand_limits(x = c(-2, 2), y = c(-1, 1)) +
  facet_grid(Grade ~ outcome)

## Plot visuals ----
p_moderation_postaccuracy <- p_moderation_postaccuracy_basic +
  scale_color_manual(
    values = c('Mean' = color_mean,
               '+1 SD' = color_plus_1SD,
               '-1 SD' = color_min_1SD
    ),
    breaks = c('-1 SD', 'Mean', '+1 SD'),
  ) +
  scale_linetype_manual(
    values = c('Significant' = 'solid', 'Not significant' = '11'),
  ) +
  scale_x_continuous(
    name = "Accuracy",
    breaks = seq(-2, 2, 1),
    labels = c("-2 SD", "-1 SD", "Mean", "+1 SD", "+2 SD")  # or custom labels
  ) +
  scale_y_continuous(
    name = "Fluency post",
    breaks = seq(-1, 1, 1),
    labels = c("-1 SD", "Mean", "+1 SD")  # or custom labels
  ) +
  guides(linetype = guide_legend(title = "Significance")) +
  common_theme_SSSR_plots

## Display plot ----
p_moderation_postaccuracy

## Save plot ----
ggsave(
  here::here('output/interaction_plot_SSSR_postaccuracy.png'),
  dpi = 600,
  height = plot_height,
  width = plot_width,
  units = 'cm'
)

# Plot interaction ----
## Build a tibble with slopes and intercepts ----
data_plotting_postword <- estimates_interaction_standardized |> 
  filter(
    str_detect(term, 'Intercept') |
    str_detect(term, 'words_exposures') |
    str_detect(term, ':'),
  ) |> 
  dplyr::select(
    outcome, grade, term, std_estimate, significant
  ) |> 
  rename(
    estimate = std_estimate,
    Significance = significant,
    Grade = grade
  ) |> 
  mutate(
    term = case_when(
      term == '(Intercept)' ~ 'intercept',
      term == 'words_exposures' ~ 'slope',
      term == 'words_exposures:accuracy_anytry' ~ 'interaction',
    ),
    Significance = factor(Significance, levels = c('Significant', 'Not significant')),
    Grade = case_when(
      Grade == 'grade_2' ~ 'Grade 2',
      Grade == 'grade_3' ~ 'Grade 3',
      Grade == 'grade_4' ~ 'Grade 4',
    ),
    Grade = factor(Grade, levels = c('Grade 2', 'Grade 3', 'Grade 4'))
  ) |> 
  pivot_wider(
    names_from = term,
    values_from = c(estimate, Significance)
  ) |> 
  rename(
    estimate_slope_mean = estimate_slope
  )

## Format the data ----
data_plotting_postword_long <- data_plotting_postword |> 
  mutate(
    estimate_slope_min1SD = estimate_slope_mean - estimate_interaction,
    estimate_slope_plus1SD = estimate_slope_mean + estimate_interaction,
  ) |> 
  pivot_longer(
    contains('estimate_slope_'),
    names_to = 'Accuracy',
    names_prefix = 'estimate_slope_',
    values_to = 'estimate_slope',
  ) |> 
  mutate(
    Accuracy = case_when(
      Accuracy == 'mean' ~ 'Mean',
      Accuracy == 'min1SD' ~ '-1 SD',
      Accuracy == 'plus1SD' ~ '+1 SD',
    ),
    Accuracy = factor(Accuracy, levels = c('-1 SD', 'Mean', '+1 SD'))
  )

## Basic plot ----
p_moderation_postword_basic <- 
  ggplot() +
    geom_abline(
      data = data_plotting_postword_long |>
        filter(
          str_detect(Accuracy, 'SD')
        ),
      aes(
        intercept = estimate_intercept,
        slope = estimate_slope,
        linetype = Significance_interaction,
        color = Accuracy
      ),
      linewidth = line_size
    ) +
  geom_abline(
    data = data_plotting_postword_long |> 
      filter(
        Accuracy == 'Mean'
      ),
    aes(
      intercept = estimate_intercept,
      slope = estimate_slope,
      linetype = Significance_slope,
      color = Accuracy
    ),
    linewidth = line_size
  ) +
  expand_limits(x = c(-2, 2), y = c(-1, 1)) +
  facet_grid(Grade ~ outcome)

## Plot visuals ----
p_moderation_postword <- p_moderation_postword_basic +
  scale_color_manual(
    values = c('Mean' = color_mean,
               '+1 SD' = color_plus_1SD,
               '-1 SD' = color_min_1SD
    ),
    breaks = c('-1 SD', 'Mean', '+1 SD'),
  ) +
  scale_linetype_manual(
    values = c('Significant' = 'solid', 'Not significant' = '11'),
  ) +
  scale_x_continuous(
    name = "Words read",
    breaks = seq(-2, 2, 1),
    labels = c("-2 SD", "-1 SD", "Mean", "+1 SD", "+2 SD")
  ) +
  scale_y_continuous(
    name = "Fluency post",
    breaks = seq(-1, 1, 1),
    labels = c("-1 SD", "Mean", "+1 SD")
  ) +
  guides(linetype = guide_legend(title = "Significance")) +
  common_theme_SSSR_plots
  
## Display plot ----
p_moderation_postword

## Save plot ----
ggsave(
  here::here('output/interaction_plot_SSSR_postword.png'),
  dpi = 600,
  height = plot_height,
  width = plot_width,
  units = 'cm'
)

