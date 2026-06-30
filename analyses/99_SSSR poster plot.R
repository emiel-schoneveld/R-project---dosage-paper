# SSSR poster plot
# source(
#   here::here('analyses/09_analysis_interaction.R')
# )


color_data <- tibble(
  Effect = c('Mean accuracy', '+1 SD accuracy', '-1 SD accuracy') |> as_factor(),
  Color = c(color_mean, color_plus_1SD, color_min_1SD)
)


# Set colors ----
color_mean = '#1B1918'
color_plus_1SD = '#e98300'
color_min_1SD = '#bc0031'
color_background = "#D7D6D4"




# Build a small data frame of slopes/intercepts per grade ----
## Grade 2 ----
abline_data_grade2 <- tibble(
  Effect = c('Mean accuracy', '+1 SD accuracy', '-1 SD accuracy'),
  intercept = rep(
    estimates_interaction |> 
      filter(outcome == 'discrete', grade == 'grade_2', str_detect(term, 'Intercept')) |> 
      pull(std_estimate),
    3
  ),
  slope = c(
    estimates_interaction |> 
      filter(outcome == 'discrete', grade == 'grade_2', term == 'words_exposures') |> 
      pull(std_estimate),
    (estimates_interaction |> filter(outcome == 'discrete', grade == 'grade_2', term == 'words_exposures') |> pull(std_estimate)) +
      (estimates_interaction |> filter(outcome == 'discrete', grade == 'grade_2', str_detect(term, ':')) |> pull(std_estimate)),
    (estimates_interaction |> filter(outcome == 'discrete', grade == 'grade_2', term == 'words_exposures') |> pull(std_estimate)) -
      (estimates_interaction |> filter(outcome == 'discrete', grade == 'grade_2', str_detect(term, ':')) |> pull(std_estimate))
  )
)

## Grade 4 ----
abline_data_grade4 <- tibble(
  Effect = c('Mean accuracy', '+1 SD accuracy', '-1 SD accuracy'),
  intercept = rep(
    estimates_interaction |> 
      filter(outcome == 'discrete', grade == 'grade_4', str_detect(term, 'Intercept')) |> 
      pull(std_estimate),
    3
  ),
  slope = c(
    estimates_interaction |> 
      filter(outcome == 'discrete', grade == 'grade_4', term == 'words_exposures') |> 
      pull(std_estimate),
    (estimates_interaction |> filter(outcome == 'discrete', grade == 'grade_4', term == 'words_exposures') |> pull(std_estimate)) +
      (estimates_interaction |> filter(outcome == 'discrete', grade == 'grade_4', str_detect(term, ':')) |> pull(std_estimate)),
    (estimates_interaction |> filter(outcome == 'discrete', grade == 'grade_4', term == 'words_exposures') |> pull(std_estimate)) -
      (estimates_interaction |> filter(outcome == 'discrete', grade == 'grade_4', str_detect(term, ':')) |> pull(std_estimate))
  )
)

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
    aes(intercept = intercept, slope = slope, color = Effect)
  ) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2)) +
  labs(title = 'Grade 2') +
  scale_x_continuous(name = 'Words read', breaks = -2:2,
                     labels = c('-2 SD', '-1 SD', 'Mean', '+1 SD', '+2 SD')) +
  scale_y_continuous(name = 'Discrete fluency post', breaks = -2:2,
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
    aes(intercept = intercept, slope = slope, color = Effect)
  ) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2)) +
  labs(title = 'Grade 4') +
  scale_x_continuous(name = 'Words read', breaks = -2:2,
                     labels = c('-2 SD', '-1 SD', 'Mean', '+1 SD', '+2 SD')) +
  scale_y_continuous(name = 'Discrete fluency post', breaks = -2:2,
                     labels = c('-2 SD', '-1 SD', 'Mean', '+1 SD', '+2 SD'))

# p_prob_grade4
p_prob <- (p_prob_grade2 + 
             p_prob_grade4 + 
             plot_layout(
               axis = 'collect',
               guides = 'collect'
             )) & 
  theme_bw() & 
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = color_background, color = NA),
    panel.grid.major = element_line(color = color_background),
    panel.grid.minor = element_blank()
  ) &
  scale_color_manual(
    name = "Interaction",
    values = c('Mean accuracy' = color_data$Color[1],
      '+1 SD accuracy' = color_data$Color[2],
      '-1 SD accuracy' = color_data$Color[3]
      )
  )

p_prob
ggsave(
  here::here('output/interaction_plot_SSSR.png'),
  dpi = 600,
  width = 11.2,
  height = 6.6,
  units = 'cm'
)

