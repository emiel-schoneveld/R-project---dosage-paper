# SSSR poster plot
source(
  here::here('analyses/09_analysis_interaction.R')
)

# Set colors ----
color_mean = '#1B1918'
color_plus_1SD = "#257835"
color_min_1SD = '#bc0031'
color_background = "#D7D6D4"

# set size
title_fontsize = 26
axes_fontsize = 20
line_size = 4
key_size = 2

# Build a small data frame of slopes/intercepts per grade ----
## Grade 2 ----
abline_data_grade2 <- tibble(
  Grade = factor(c('Grade 2', 'Grade 2', 'Grade 2'), levels = c('Grade 2', 'Grade 3', 'Grade 4')),
  Accuracy = factor(c('Mean', '+1 SD', '-1 SD'), levels = c('Mean', '+1 SD', '-1 SD')),
  intercept = rep(
    estimates_interaction |> 
      filter(outcome == 'discrete_exp', grade == 'grade_2', str_detect(term, 'Intercept')) |> 
      pull(std_estimate),
    3
  ),
  slope = c(
    estimates_interaction |> 
      filter(outcome == 'discrete_exp', grade == 'grade_2', term == 'words_exposures') |> 
      pull(std_estimate),
    (estimates_interaction |> filter(outcome == 'discrete_exp', grade == 'grade_2', term == 'words_exposures') |> pull(std_estimate)) +
      (estimates_interaction |> filter(outcome == 'discrete_exp', grade == 'grade_2', str_detect(term, ':')) |> pull(std_estimate)),
    (estimates_interaction |> filter(outcome == 'discrete_exp', grade == 'grade_2', term == 'words_exposures') |> pull(std_estimate)) -
      (estimates_interaction |> filter(outcome == 'discrete_exp', grade == 'grade_2', str_detect(term, ':')) |> pull(std_estimate))
  ),
  Significance = factor(c('Significant', 'Significant', 'Significant'), levels = c('Significant', 'Not significant'))
)

## Grade 3 ----
abline_data_grade3 <- tibble(
  Grade = factor(c('Grade 3', 'Grade 3', 'Grade 3'), levels = c('Grade 2', 'Grade 3', 'Grade 4')),
  Accuracy = factor(c('Mean', '+1 SD', '-1 SD'), levels = c('Mean', '+1 SD', '-1 SD')),
  intercept = rep(
    estimates_interaction |> 
      filter(outcome == 'discrete_exp', grade == 'grade_3', str_detect(term, 'Intercept')) |> 
      pull(std_estimate),
    3
  ),
  slope = c(
    estimates_interaction |> 
      filter(outcome == 'discrete_exp', grade == 'grade_3', term == 'words_exposures') |> 
      pull(std_estimate),
    (estimates_interaction |> filter(outcome == 'discrete_exp', grade == 'grade_3', term == 'words_exposures') |> pull(std_estimate)) +
      (estimates_interaction |> filter(outcome == 'discrete_exp', grade == 'grade_3', str_detect(term, ':')) |> pull(std_estimate)),
    (estimates_interaction |> filter(outcome == 'discrete_exp', grade == 'grade_3', term == 'words_exposures') |> pull(std_estimate)) -
      (estimates_interaction |> filter(outcome == 'discrete_exp', grade == 'grade_3', str_detect(term, ':')) |> pull(std_estimate))
  ),
  Significance = factor(c('Significant', 'Not significant', 'Not significant'), levels = c('Significant', 'Not significant'))
)
## Grade 4 ----
abline_data_grade4 <- tibble(
  Grade = factor(c('Grade 4', 'Grade 4', 'Grade 4'), levels = c('Grade 2', 'Grade 3', 'Grade 4')),
  Accuracy = factor(c('Mean', '+1 SD', '-1 SD'), levels = c('Mean', '+1 SD', '-1 SD')),
  intercept = rep(
    estimates_interaction |> 
      filter(outcome == 'discrete_exp', grade == 'grade_4', str_detect(term, 'Intercept')) |> 
      pull(std_estimate),
    3
  ),
  slope = c(
    estimates_interaction |> 
      filter(outcome == 'discrete_exp', grade == 'grade_4', term == 'words_exposures') |> 
      pull(std_estimate),
    (estimates_interaction |> filter(outcome == 'discrete_exp', grade == 'grade_4', term == 'words_exposures') |> pull(std_estimate)) +
      (estimates_interaction |> filter(outcome == 'discrete_exp', grade == 'grade_4', str_detect(term, ':')) |> pull(std_estimate)),
    (estimates_interaction |> filter(outcome == 'discrete_exp', grade == 'grade_4', term == 'words_exposures') |> pull(std_estimate)) -
      (estimates_interaction |> filter(outcome == 'discrete_exp', grade == 'grade_4', str_detect(term, ':')) |> pull(std_estimate))
  ),
  Significance = factor(c('Significant', 'Significant', 'Significant'), levels = c('Significant', 'Not significant'))
)

## Combine grades
abline_data <- bind_rows(
  abline_data_grade2,
  abline_data_grade3,
  abline_data_grade4
)

# Plot probed interactions ----
p_moderation <- data |> 
  group_by(grade) |> 
  mutate(
    fluency_discrete_post = scale(fluency_discrete_post),
    words_exposures = scale(words_exposures)
  ) |> 
  ggplot(aes(x = words_exposures, y = fluency_discrete_post)) +
  geom_point(alpha = 0.0) +
  geom_abline(
    data = abline_data,
    aes(intercept = intercept, slope = slope, color = Accuracy, linetype = Significance),
    linewidth = line_size
  ) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2)) +
  scale_x_continuous(name = 'Words read', breaks = -2:2,
                     labels = c('-2 SD', '-1 SD', 'Mean', '+1 SD', '+2 SD')) +
  scale_y_continuous(name = 'Discrete post', breaks = -2:2,
                     labels = c('-2 SD', '-1 SD', 'Mean', '+1 SD', '+2 SD')) +
  facet_wrap(~Grade) +
  scale_color_manual(
    values = c('Mean' = color_mean,
               '+1 SD' = color_plus_1SD,
               '-1 SD' = color_min_1SD
    )
  ) +
  scale_linetype_manual(
    values = c('Significant' = 'solid', 'Not significant' = '11'),
  ) +
  theme_bw() +
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
  
p_moderation
ggsave(
  here::here('output/interaction_plot_SSSR.png'),
  dpi = 600,
  height = 16,
  width = 26,
  units = 'cm'
)

