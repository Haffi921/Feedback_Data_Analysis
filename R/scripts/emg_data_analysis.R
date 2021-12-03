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
library(broom)

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
  outliers <- if_else(is.null(initial), rep(FALSE, length(columns[1])), initial)
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
    Corr_Outlier = outlier(Corr, Corr_Diff),
    Zygo_Outlier = outlier(Zygo, Zygo_Diff),
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
  select(-c(Zygo, ends_with("Outlier")))

tidy(Corr_df)

Zygo_df <- df %>%
  filter(!Zygo_Outlier) %>%
  select(-c(Corr, ends_with("Outlier")))

l <- aggregate(Corr ~ Bin + congruencyNmin1 + congruency + feedback, Corr_df, mean)
l

model <- aov(Corr ~ feedback * congruency * congruencyNmin1 * Bin, Corr_df)

summary(model)

l <- aggregate(Zygo ~ Bin + feedback, Zygo_df, mean)
l

model <- aov(Zygo ~ feedback * congruency * congruencyNmin1 * Bin, Zygo_df)

summary(model)
