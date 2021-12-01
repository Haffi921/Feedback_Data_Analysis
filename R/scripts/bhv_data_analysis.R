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
    "feedback_full_data.csv",
    col_types = c(gender = "f") # TODO: Delete when more participants come in
  ) %>%
  filter(trial_block == "trial") %>%
  group_by(participant, block) %>%
  slice(2:n()) %>%
  ungroup()

# ----------------
# Sanity check
nrow(df) %% 768 == 0 # TODO: add to data_valication script but with 800
# ----------------

xtabs( ~ feedback + congruency, df)

l <- aggregate(rt ~ congruencyNmin1 + congruency + feedback, df, mean)
l$fcon <- paste(l$feedback, l$congruency, sep="_")

model <- aov(rt ~ feedback * congruency * congruencyNmin1, df)

summary(model)

l %>%
  ggplot(aes(congruencyNmin1, rt, group=fcon, color=fcon)) +
  geom_line() +
  geom_point()

