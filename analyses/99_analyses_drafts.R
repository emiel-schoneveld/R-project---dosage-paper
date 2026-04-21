# analyses draft

data_mod <- data |> 
  mutate(
    wordreading_score_pre = wordreading_score_pre - mean(wordreading_score_pre, na.rm = T),
    wordreading_score_post = wordreading_score_post - mean(wordreading_score_post, na.rm = T),
    practice_cii_words_total = practice_cii_words_total - mean(practice_cii_words_total, na.rm = T),
    practice_accuracy = practice_accuracy - mean(practice_accuracy, na.rm = T),
  )
  
mod_selfteaching <- '
wordreading_score_post ~ wordreading_score_pre + practice_cii_words_total + practice_accuracy #+ practice_cii_words_total:practice_accuracy
practice_cii_words_total ~ wordreading_score_pre
practice_accuracy ~ wordreading_score_pre

practice_cii_words_total ~~ practice_accuracy

wordreading_score_pre ~~ wordreading_score_pre
wordreading_score_post ~~ wordreading_score_post
practice_cii_words_total ~~ practice_cii_words_total
practice_accuracy ~~ practice_accuracy

wordreading_score_pre ~ 1
wordreading_score_post ~ 1
practice_cii_words_total ~ 1
practice_accuracy ~ 1
'

### Fit full mediation model ----
fit_selfteaching <- sem(
  mod_selfteaching,
  data = data, 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID"
)

fitmeasures(fit_selfteaching, c('df', 'CFI', 'rmsea')) # no satisfactory fit based on cfi and rmsea
summary(fit_selfteaching)
varTable(fit_selfteaching)
# plot_lavaan(fit_selfteaching,
#             where = "browser")

### Inspect full mediation model ----
resid(fit_selfteaching, type = "cor")
summary(fit_selfteaching)