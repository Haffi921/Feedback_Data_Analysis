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

df <- read_csv(
  "feedback_emg_data.csv",
  col_types = c(gender = "f") # TODO: Delete when more participants come in
) %>%
  # Filter out practice
  filter(trial_block == "trial") %>%
  group_by(participant, block) %>%
  slice(2:n()) %>%
  ungroup() %>%
  filter(made == TRUE & correct == TRUE) %>%
  pivot_longer(cols = c(starts_with("Corr_"), starts_with("Zygo_")), names_to = c("Muscle", "Bin"), names_sep = "_") %>%
  pivot_wider(names_from = Muscle) %>%
  mutate(Corr_Diff = Corr - lag(Corr), Zygo_Diff = Zygo - lag(Zygo)) %>%
  mutate(Corr_Outlier = FALSE, Zygo_Outlier = FALSE) %>%
  select(-c(trial_block, made, correct))

glimpse(df)

outlier <- function(column, outliers) {
  avg = mean(column, na.rm = TRUE)
  std = sd(column, na.rm = TRUE)
  for(i in 1:length(column)) {
    if(!is.na(column[i]) & abs((column[i] - avg) / std) > 3.5) {
      outliers[i] = TRUE
    }
  }
  return(outliers)
}

df <- df %>%
  group_by(participant, block, trial) %>%
  mutate(
    Corr_Outlier = outlier(Corr, Corr_Outlier),
    Zygo_Outlier = outlier(Zygo, Zygo_Outlier),
    Corr_Outlier = outlier(Corr_Diff, Corr_Outlier),
    Zygo_Outlier = outlier(Zygo_Diff, Zygo_Outlier),
  ) %>%
  ungroup() %>%
  group_by(participant, block, Bin) %>%
  mutate(
    Corr_Outlier = outlier(Corr, Corr_Outlier),
    Zygo_Outlier = outlier(Zygo, Zygo_Outlier),
    Corr_Outlier = outlier(Corr_Diff, Corr_Outlier),
    Zygo_Outlier = outlier(Zygo_Diff, Zygo_Outlier),
  ) %>%
  ungroup()

outliers <- df %>%
  filter(Corr_Outlier == TRUE | Zygo_Outlier == TRUE)

l <- aggregate(Corr ~ congruencyNmin1 + congruency + feedback, df, mean)
l$fcon <- paste(l$feedback, l$congruency, sep="_")

l
model <- aov(Corr ~ feedback * congruency * congruencyNmin1, df)

summary(model)
