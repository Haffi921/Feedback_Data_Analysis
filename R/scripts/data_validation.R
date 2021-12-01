{
  requiredPackages <- c("devtools", "tidyverse", "glue")
  install.packages(setdiff(requiredPackages, rownames(installed.packages())))
  
  if (!"AutoWD" %in% installed.packages()) {
    devtools::install_github("Haffi921/AutoWD")
  }
  
  rm(requiredPackages)
}

AutoWD::autowd(1)
library(tidyverse)

validate <- function(df) {
  df <- read_csv(df, show_col_types = F) %>%
    select(-c(starts_with("Screen.")), -last_col()) %>%
    drop_na(starts_with("Trial.")) %>%
    filter(trial_block != "practice") %>%
    group_by(block) %>%
    slice(2:n()) %>%
    ungroup()

  
  # Hands alternating
  hand <- df %>%
    select(block, hand) %>%
    group_by(block) %>%
    mutate(lag = lag(hand)) %>%
    drop_na()
  hands_alternate <- all(base::xor(hand$hand, hand$lag))

  
  # Congruency equality
  congruency <- df %>%
    count(congruency) %>%
    mutate(conper = n / nrow(df))
  congruency_equal <- all(congruency$conper == 0.5)
  
  
  # Correct feedback groups
  feedback_groups <- df %>%
    mutate(half = ifelse(block <= max(block) / 2, "First", "Last")) %>%
    select(participant, feedback_block, half) %>%
    mutate(
      correct = ifelse(half == "First", feedback_block == strtoi(participant) %% 2, feedback_block == (strtoi(df$participant) + 1.0) %% 2)
    )
  
  feedback_groups_correct <- all(feedback_groups$correct)
  
  
  # Trial timing check
  trial_time <- df %>%
    mutate(
      trial_time = Feedback.feedback.time_stopped_global_flip - (Trial.distractor.time_started_global_flip - Trial.distractor.time_started_flip),
      frames_over = (trial_time - 2.883) * 60
    ) %>%
    select(frames_over)
  no_exessive_trial_frames <- all(trial_time$frames_over < 3.0)
  
  
  # All components finished
  all_components_finished <- all(
    (df %>%
      select(ends_with(".status"))) == -1
  )
  
  print(
    glue::glue(
      "Hands alternate: {hands_alternate}
      Congruency equal: {congruency_equal}
      Feedback groups correct: {feedback_groups_correct}
      No excessive trial frames: {no_exessive_trial_frames}
      All components finish: {all_components_finished}
      
      ")
  )
}

files <- list.files(path = "data/raw/bhv_data", pattern = "[1-9]{4}.csv$", full.names = T)

for (file in files) {
  print(glue::glue("{file}:"))
  suppressMessages(validate(file))
}

rm(list = ls())
