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

df <- read_csv(
  "data/feedback_emg_data.csv",
  col_types = c(gender = "f") # TODO: Delete when more participants come in
) %>%
  # Filter out practice
  filter(trial_block == "trial") %>%
  
  # Filter out first trial (?)
  group_by(participant, block) %>%
  slice(2:n()) %>%
  ungroup() %>%
  
  # Filter out incorrect and no response
  filter(made == TRUE & correct == TRUE) %>%
  
  # Extract bin information
  pivot_longer(cols = c(starts_with("Corr_"), starts_with("Zygo_")), names_to = c("Muscle", "Bin"), names_sep = "_") %>%
  pivot_wider(names_from = Muscle) %>%
  mutate(Bin = as.numeric(Bin)) %>%
  
  # Create difference score
  group_by(participant, block, trial) %>%
  mutate(Corr_Diff = Corr - lag(Corr), Zygo_Diff = Zygo - lag(Zygo)) %>%
  ungroup() %>%
  
  # Remove unwanted columns
  select(-c(trial_block, made, correct))

glimpse(df)


# Function for Z-Calculation
ZCalc <- function(column) {
  avg <- mean(column, na.rm = TRUE)
  std <- sd(column, na.rm = TRUE)
  return(abs((column - avg) / std))
}

# Function for outlier detection
outlier <- function(..., initial = NULL, z = 3.5) {
  columns <- list(...)
  if(is.null(initial)) {
    outliers <- rep(FALSE, length(columns[1]))
  }
  else {
    outliers <- initial
  }
  for(column in columns) {
    outliers <- outliers | (ZCalc(column) > z)
  }
  return(replace(outliers, is.na(outliers), FALSE))
}

# Run outlier detection
df <- df %>%
  
  # Detect outliers within each individual trial
  group_by(participant, block, trial) %>%
  mutate(
    Corr_Outlier = outlier(Corr, Corr_Diff),
    Zygo_Outlier = outlier(Zygo, Zygo_Diff),
  ) %>%
  ungroup() %>%
  
  # Detect outliers within each individual bin, across trials
  group_by(participant, Bin) %>%
  mutate(
    Corr_Outlier = outlier(Corr, Corr_Diff, initial = Corr_Outlier),
    Zygo_Outlier = outlier(Zygo, Zygo_Diff, initial = Zygo_Outlier),
  ) %>%
  ungroup() %>%
  
  # Mark each trial as outlier
  group_by(participant, block, trial) %>%
  mutate(
    Corr_Outlier = any(Corr_Outlier),
    Zygo_Outlier = any(Zygo_Outlier),
  ) %>%
  ungroup()

# Collect outliers for inspection
outliers <- df %>%
  filter(Corr_Outlier | Zygo_Outlier)

# Create specific dataframes for each
Corr_df <- df %>%
  filter(!Corr_Outlier) %>%
  select(-c(Zygo, ends_with("Outlier"), ends_with("Diff")))

Zygo_df <- df %>%
  filter(!Zygo_Outlier) %>%
  select(-c(Corr, ends_with("Outlier"), ends_with("Diff")))


Corr_df_summary <- Corr_df %>%
  group_by(participant, feedback, congruency, Bin) %>%
  summarise(Corr = mean(Corr)) %>%
  ungroup() %>%
  group_by(feedback, congruency, Bin) %>%
  summarise(mean = mean(Corr), se = sd(Corr) / n(), CI = 1.96 * se) %>%
  ungroup()

Zygo_df_summary <- Zygo_df %>%
  group_by(feedback, congruency, Bin) %>%
  summarise(mean = mean(Zygo), se = sd(Zygo) / n(), CI = 1.96 * se) %>%
  ungroup()

Corr_df %>%
  mutate(group = paste(feedback, congruency, congruencyNmin1)) %>%
  ggplot(aes(x = group, y = Corr)) +
  geom_boxplot()


Corr_df %>%
  group_by(feedback, congruency, congruencyNmin1) %>%
  shapiro_test(Corr)

Corr_df %>%
  mutate(group = as_factor(paste(feedback, congruency, congruencyNmin1))) %>%
  ggplot(aes(sample=Corr)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(feedback ~ congruency + congruencyNmin1, labeller = "label_both")

Corr_df_for_aov <- df %>%
  group_by(participant, feedback, congruency, congruencyNmin1) %>%
  summarise(Corr = mean(Corr)) %>%
  ungroup()

res.aov <- Corr_df_for_aov %>%
  anova_test(dv = Corr, wid = participant, within = c(feedback, congruency, congruencyNmin1))

get_anova_table(res.aov) %>% adjust_pvalue(method = "bonferroni")

Corr_df_summary %>%
  ggplot(aes(Bin, mean, group=congruency, color=congruency)) +
  facet_grid(rows = vars(feedback)) +
  scale_x_continuous("Bins", seq(0, 10, 1)) +
  scale_y_continuous("Corrugator activation", breaks=seq(-0.0005, 0.00160, 0.00015)) +
  geom_ribbon(aes(ymin=mean - CI, ymax = mean + CI), fill="gray70", alpha=.3, linetype="dashed") +
  geom_line() +
  theme(panel.grid.minor = element_blank())
