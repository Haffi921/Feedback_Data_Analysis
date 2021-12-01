{
  requiredPackages <- setdiff(c("devtools", "tidyverse"), rownames(installed.packages()))
  if(length(requiredPackages)) {
    install.packages(setdiff(requiredPackages, rownames(installed.packages())))
  }
  
  if (!"AutoWD" %in% installed.packages()) {
    devtools::install_github("Haffi921/AutoWD")
  }
  
  rm(requiredPackages)
}

AutoWD::autowd(1)
library(tidyverse)


# Get a list of all the files in 'raw_data' folder
files <- list.files(path = "data/raw/bhv_data", pattern = "[1-9]{4}.csv$", full.names = T)


# We read in all csv files we found using map_dfr and read_csv
bhv <- map_dfr(
    files, read_csv,
    
    # With cols_only we cherry-pick the columns we want
    # ... and specify the type
    col_types = cols_only(
      experiment_name = "c",
      participant = "i",
      session = "i",
      age = "i",
      gender = "f",
      psychopy_version = "c",
      date = "c",
      trial_block = "c",
      block = "i",
      trial = "i",
      hand = "f",
      congruency = "f",
      feedback_block = "f",
      correct_key = "c",
      Trial.response.correct = "l",
      Trial.response.made = "l",
      Trial.response.key = "c",
      Trial.response.rt = "d",
      Trial.target.time_started_flip = "d",
      post_response_keypresses = "c"
    ),
    na = c("None", "[]")
  ) %>%
  
  # Drop all unnecessary rows
  drop_na(trial) %>%
  
  # Factor recoding
  mutate(
    hand = recode(hand, "0" = "left", "1" = "right"),
    congruency = recode(congruency, "0" = "congruent", "1" = "incongruent"),
    feedback = recode(feedback_block, "True" = "Feedback", "False" = "No feedback"),
  ) %>%
  
  # Reorganization
  rename(
    correct = Trial.response.correct,
    made = Trial.response.made,
    response = Trial.response.key,
    rt = Trial.response.rt,
    target_display = Trial.target.time_started_flip,
    extra_keys = post_response_keypresses
  ) %>%
  
  # Calculate RT in ms and congruency N-1
  mutate(
    rt = (rt - target_display) * 1000,
    congruencyNmin1 = lag(congruency)
  ) %>%
  
  # Final selection
  select(
    # Session data
    experiment_name:date,
    # Progress data
    trial_block:trial,
    # Trial data
    hand, congruencyNmin1, congruency, correct_key, feedback,
    # Response data
    made, response, correct, rt, extra_keys
  )

# Uncomment to look at data structure
glimpse(bhv)

bhv %>%
  write_csv("data/feedback_bhv_data.csv")

emg <- read_csv("data/raw/emg_data/Feedback_EMG.csv") %>%
  rename(participant = Count) %>%
  select(-c("File")) %>%
  mutate(
    Channel = recode(Channel, "EMG_Corr_RMS" = "Corr", "EMG_Zygo_RMS" = "Zygo")
  ) %>%
  filter(Bin > 4) %>%
  mutate(Bin = Bin - 4) %>%
  pivot_wider(names_from = c(Channel, Bin), values_from = Value) %>%
  mutate(block = ceiling((Segment - 24) / 97)) %>% relocate(block, .before = Segment) %>%
  group_by(participant, block) %>%
  mutate(trial = row_number(participant)) %>% relocate(trial, .after = block) %>%
  mutate(trial_block = ifelse(block == 0, "practice", "trial"), block = ifelse(block == 0, 1, block)) %>%
  ungroup()

emg <- bhv %>%
  select(
    # Session data
    experiment_name:gender,
    
    # Trial data
    trial_block:congruency, feedback,
    
    # Response data
    made, correct
  ) %>%
  inner_join(emg)

emg %>%
  write_csv("data/feedback_emg_data.csv")

bhv %>%
  full_join(emg) %>%
  write_csv("data/feedback_full_data.csv")

rm(list = ls())
