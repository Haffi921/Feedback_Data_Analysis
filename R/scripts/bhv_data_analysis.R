{
  requiredPackages <- c("devtools", "tidyverse")
  install.packages(setdiff(requiredPackages, rownames(installed.packages())))
  
  if (!"AutoWD" %in% installed.packages()) {
    devtools::install_github("Haffi921/AutoWD")
  }
  
  rm(requiredPackages)
}

AutoWD::autowd(1)
library(tidyverse)
library(rstatix)

# ----------------
# Data prep
# ----------------
df <- read_csv(
    "data/feedback_full_data.csv",
    col_types = c(
      gender = "f", # TODO: Delete when more participants come in
      participant = "f",
      feedback = "f",
      congruency = "f",
      congruencyNmin1 = "f"
      )
  ) %>%
  filter(trial_block == "trial", made == TRUE, correct == TRUE) %>%
  group_by(participant, block) %>%
  slice(2:n()) %>%
  ungroup() %>%
  mutate(congruencyNmin1 = fct_drop(congruencyNmin1))
# ----------------

# ----------------
# Sanity check
# TODO: add to data_valication script but with 800
#nrow(df) %% 768 == 0
# ----------------

# ----------------
# Outliers
# ----------------
zCalc <- function(x) {
  return(abs((x - mean(x, na.rm=T)) / sd(x, na.rm = T)))
}

outlier_detection <- function(..., initial = NULL, z = 3.5) {
  columns <- list(...)
  outliers <- if_else(is.null(initial), rep(FALSE, length(columns[1])), initial)
  for(column in columns) {
    outliers <- outliers | zCalc(column) > z
  }
  return(replace(outliers, is.na(outliers), FALSE))
}

df %<>%
  group_by(feedback, congruency, congruencyNmin1) %>%
  mutate(is.outlier = is_outlier(rt)) %>%
  ungroup()

outliers <- df %>% filter(is.outlier == T)

df <- df %>%
  filter(is.outlier == F)
# ----------------

# ----------------
# Summary
# ----------------
df %>%
  group_by(feedback, congruency, congruencyNmin1) %>%
  get_summary_stats(rt, type = "mean_sd")
# ----------------

# ----------------
# Visualization
# ----------------
df %>%
  mutate(group = paste(feedback, congruency, congruencyNmin1)) %>%
  ggplot(aes(x = group, y = rt)) +
  geom_boxplot()
# ----------------

# ----------------
# Assumptions
# ----------------

# Normality
df %>%
  group_by(feedback, congruency, congruencyNmin1) %>%
  shapiro_test(rt)

df %>%
  mutate(group = as_factor(paste(feedback, congruency, congruencyNmin1))) %>%
  ggplot(aes(sample=rt)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(feedback ~ congruency + congruencyNmin1, labeller = "label_both")
# ----------------

# ----------------
# Anova
# ----------------
df_for_aov <- df %>%
  group_by(participant, feedback, congruency, congruencyNmin1) %>%
  summarise(rt = mean(rt)) %>%
  ungroup()

res.aov <- df_for_aov %>%
  anova_test(dv = rt, wid = participant, within = c(feedback, congruency, congruencyNmin1))

get_anova_table(res.aov) %>% adjust_pvalue(method = "bonferroni")

df_for_aov %>%
  pairwise_t_test(rt ~ feedback, paired = T, p.adjust.method = "bonferroni")

df_for_aov %>%
  pairwise_t_test(rt ~ congruency, paired = T, p.adjust.method = "bonferroni")

df_for_aov %>%
  pairwise_t_test(rt ~ congruencyNmin1, paired = T, p.adjust.method = "bonferroni")


df_for_aov %>%
  group_by(feedback) %>%
  pairwise_t_test(rt ~ congruency, paired = T, p.adjust.method = "bonferroni")

df_for_aov %>%
  group_by(feedback) %>%
  pairwise_t_test(rt ~ congruencyNmin1, paired = T, p.adjust.method = "bonferroni")


df_for_aov %>%
  group_by(congruency) %>%
  pairwise_t_test(rt ~ feedback, paired = T, p.adjust.method = "bonferroni")

df_for_aov %>%
  group_by(congruency) %>%
  pairwise_t_test(rt ~ congruencyNmin1, paired = T, p.adjust.method = "bonferroni")


df_for_aov %>%
  group_by(congruencyNmin1) %>%
  pairwise_t_test(rt ~ feedback, paired = T, p.adjust.method = "bonferroni")

df_for_aov %>%
  group_by(congruencyNmin1) %>%
  pairwise_t_test(rt ~ congruency, paired = T, p.adjust.method = "bonferroni")


df_for_aov %>%
  group_by(feedback, congruency) %>%
  pairwise_t_test(rt ~ congruencyNmin1, paired = T, p.adjust.method = "bonferroni")

df_for_aov %>%
  group_by(feedback, congruencyNmin1) %>%
  pairwise_t_test(rt ~ congruency, paired = T, p.adjust.method = "bonferroni")

df_for_aov %>%
  group_by(congruency, congruencyNmin1) %>%
  pairwise_t_test(rt ~ feedback, paired = T, p.adjust.method = "bonferroni")
# ----------------

# ----------------
# Plotting
# ----------------
make_gratton_plot <- function(df, title) {
  df %>%
    ggplot(aes(
      congruencyNmin1, rt,
      group=congruency, color=congruency
    )) +
    geom_ribbon(aes(
      ymin = CI_min, ymax = CI_max
    ), fill = "gray70", alpha = .3, linetype = "dashed") +
    geom_line() +
    geom_point() +
    ggtitle(title$feedback) +
    ylim(400, 550)
}

df_for_aov %>%
  group_by(feedback, congruency, congruencyNmin1) %>%
  summarise(se = sd(rt) / n(), rt = mean(rt), CI_min = rt - 1.96*se, CI_max = rt + 1.96*se) %>%
  ungroup() %>%
  group_by(feedback) %>%
  group_map(make_gratton_plot)
# -------------------------------------- #
