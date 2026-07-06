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
title_fontsize = 32
axes_fontsize = 28
line_size = 4


# Build a small data frame of slopes/intercepts per grade ----
## Grade 2 ----
abline_data_grade2 <- tibble(
  Accuracy = c('Mean', '+1 SD', '-1 SD'),
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
  )
) |> 
  mutate(
    Accuracy = factor(Accuracy, levels = c('+1 SD', 'Mean', '-1 SD')))

## Grade 4 ----
abline_data_grade4 <- tibble(
  Accuracy = c('Mean', '+1 SD', '-1 SD'),
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
  )
) |> 
  mutate(
    Accuracy = factor(Accuracy, levels = c('+1 SD', 'Mean', '-1 SD')))

# Plot probed interactions ----
## Grade 2 ----
p_prob_grade2 <- data |> 
  group_by(grade) |> 
  mutate(
    fluency_discrete_post = scale(fluency_discrete_post),
    words_exposures = scale(words_exposures)
  ) |> 
  filter(grade == 'grade_2') |> 
  ggplot(aes(x = words_exposures, y = fluency_discrete_post)) +
  geom_point(alpha = 0.0) +
  geom_abline(
    data = abline_data_grade2,
    aes(intercept = intercept, slope = slope, color = Accuracy),
    linewidth = line_size
  ) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2)) +
  labs(title = 'Grade 2') +
  scale_x_continuous(name = 'Words read', breaks = -2:2,
                     labels = c('-2 SD', '-1 SD', 'Mean', '+1 SD', '+2 SD')) +
  scale_y_continuous(name = 'Discrete post', breaks = -2:2,
                     labels = c('-2 SD', '-1 SD', 'Mean', '+1 SD', '+2 SD'))

## Grade 4 ----
p_prob_grade4 <- data |> 
  group_by(grade) |> 
  mutate(
    fluency_discrete_post = scale(fluency_discrete_post),
    words_exposures = scale(words_exposures)
  ) |> 
  filter(grade == 'grade_4') |> 
  ggplot(aes(x = words_exposures, y = fluency_discrete_post)) +
  geom_point(alpha = 0.0) +
  geom_abline(
    data = abline_data_grade4,
    aes(intercept = intercept, slope = slope, color = Accuracy),
    linewidth = line_size
  ) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2)) +
  labs(title = 'Grade 4') +
  scale_x_continuous(name = 'Words read', breaks = -2:2,
                     labels = c('-2 SD', '-1 SD', 'Mean', '+1 SD', '+2 SD')) +
  scale_y_continuous(name = 'Discrete post', breaks = -2:2,
                     labels = c('-2 SD', '-1 SD', 'Mean', '+1 SD', '+2 SD'))

# p_prob_grade4
p_prob <- (p_prob_grade2 + 
             p_prob_grade4 + 
             plot_layout(
               axes = 'collect',
               guides = 'collect'
             )) & 
  theme_bw() & 
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
  ) &
  scale_color_manual(
    name = "Accuracy",
    values = c('Mean' = color_mean,
      '+1 SD' = color_plus_1SD,
      '-1 SD' = color_min_1SD
      )
  )

p_prob
ggsave(
  here::here('output/interaction_plot_SSSR.png'),
  dpi = 600,
  height = 16,
  width = 26,
  units = 'cm'
)

