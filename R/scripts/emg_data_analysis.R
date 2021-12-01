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
  group_by(participant, Segment) %>%
  mutate(Corr_Diff = Corr - lag(Corr), Zygo_Diff = Zygo - lag(Zygo)) %>%
  ungroup() %>%
  
  # Create columns for Z calcuations
  mutate(
    Corr_Trial_Z = NA, Corr_Diff_Trial_Z =  NA, Zygo_Trial_Z = NA, Zygo_Diff_Trial_Z =  NA,
    Corr_Bin_Z = NA, Corr_Diff_Bin_Z =  NA, Zygo_Bin_Z = NA, Zygo_Diff_Bin_Z =  NA, 
  ) %>%
  
  # Create outlier column
  mutate(Corr_Outlier = FALSE, Zygo_Outlier = FALSE) %>%
  
  # Remove unwanted columns
  select(-c(trial_block, made, correct))

glimpse(df)


# Function for Z-Calculation
ZCalc <- function(column, zcol) {
  avg = mean(column, na.rm = TRUE)
  std = sd(column, na.rm = TRUE)
  zcol <- abs((column - avg) / std)
  return(zcol)
}

# Function for outlier detection
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

# Run outlier detection
df <- df %>%
  
  # Detect outliers within each individual trial
  group_by(participant, block, trial) %>%
  mutate(
    Corr_Trial_Z = ZCalc(Corr, Corr_Trial_Z),
    Corr_Diff_Trial_Z = ZCalc(Corr_Diff, Corr_Diff_Trial_Z),
    Zygo_Trial_Z = ZCalc(Zygo, Zygo_Trial_Z),
    Zygo_Diff_Trial_Z = ZCalc(Zygo_Diff, Zygo_Diff_Trial_Z),
  ) %>%
  ungroup() %>%
  
  # Detect outliers within each individual bin, across trials
  group_by(participant, Bin) %>%
  mutate(
    Corr_Bin_Z = ZCalc(Corr, Corr_Bin_Z),
    Corr_Diff_Bin_Z = ZCalc(Corr_Diff, Corr_Diff_Bin_Z),
    Zygo_Bin_Z = ZCalc(Zygo, Zygo_Bin_Z),
    Zygo_Diff_Bin_Z = ZCalc(Zygo_Diff, Zygo_Diff_Bin_Z),
  ) %>%
  ungroup() %>%
  
  # Detect outliers within each individual trial
  group_by(participant, block, trial) %>%
  mutate(
    Corr_Outlier = outlier(Corr, Corr_Outlier),
    Zygo_Outlier = outlier(Zygo, Zygo_Outlier),
    Corr_Outlier = outlier(Corr_Diff, Corr_Outlier),
    Zygo_Outlier = outlier(Zygo_Diff, Zygo_Outlier),
  ) %>%
  ungroup() %>%
  
  # Detect outliers within each individual bin, across trials
  group_by(participant, Bin) %>%
  mutate(
    Corr_Outlier = outlier(Corr, Corr_Outlier),
    Zygo_Outlier = outlier(Zygo, Zygo_Outlier),
    Corr_Outlier = outlier(Corr_Diff, Corr_Outlier),
    Zygo_Outlier = outlier(Zygo_Diff, Zygo_Outlier),
  ) %>%
  ungroup()

# Collect outliers for inspection
outliers <- df %>%
  filter(Corr_Outlier | Zygo_Outlier)

# Remove Z calc columns
df <- df %>%
  select(-c(
    Corr_Trial_Z, Corr_Diff_Trial_Z, Zygo_Trial_Z, Zygo_Diff_Trial_Z,
    Corr_Bin_Z, Corr_Diff_Bin_Z, Zygo_Bin_Z, Zygo_Diff_Bin_Z, 
  ))

# Create specific dataframes for each
Corr_df <- df %>%
  filter(!Corr_Outlier) %>%
  select(-c(Zygo, ends_with("Outlier")))

Zygo_df <- df %>%
  filter(!Zygo_Outlier) %>%
  select(-c(Corr, ends_with("Outlier")))

l <- aggregate(Corr ~ congruencyNmin1 + congruency + feedback, df, mean)
l

model <- aov(Corr ~ feedback * congruency * congruencyNmin1 * Bin, Corr_df)

summary(model)

model <- aov(Zygo ~ feedback * congruency * congruencyNmin1 * Bin, Zygo_df)

summary(model)
