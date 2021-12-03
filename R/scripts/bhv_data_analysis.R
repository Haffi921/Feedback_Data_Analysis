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
  ungroup()

# ----------------
# Sanity check
# TODO: add to data_valication script but with 800
#nrow(df) %% 768 == 0
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
  
df %>%
  group_by(feedback, congruency, congruencyNmin1) %>%
  shapiro_test(rt)

df %>%
  mutate(group = paste(feedback, congruency, congruencyNmin1)) %>%
  ggplot(aes(x = group, y = rt)) +
  geom_boxplot()

select_group <- function(data, groups) {
  data %>% nest %>% ungroup %>% slice(groups) %>% unnest(data)
}

gdf <- df %>%
  group_by(participant, feedback, congruency, congruencyNmin1) %>%
  summarise(rt = mean(rt)) %>%
  ungroup()

num_g <- df %>%
  summarise(num_g = length(unique(feedback))) %>%
  pull(num_g)

num_p <- df %>%
  summarise(num_p = length(unique(participant))) %>%
  pull(num_p)

sst <- df %>%
  summarise(sst = var(rt) * (n() - 1)) %>%
  pull(sst)

dfsst <- nrow(df) - 1


ssw <- df %>%
  group_by(participant) %>%
  summarise(ssr = var(rt) * (n() - 1)) %>%
  ungroup() %>%
  summarise(ssr = sum(ssr)) %>% pull(ssr)

dfssw <- num_p * num_g


t_mean <- mean(df$rt)

ssm <- df %>%
  group_by(feedback) %>%
  summarise(ssm = (mean(rt) - t_mean)^2 * n()) %>%
  ungroup() %>%
  summarise(ssm = sum(ssm)) %>% pull(ssm)

dfssm <- num_g - 1


ssr <- ssw - ssm

dfssr <- dfssw - dfssm


msm <- ssm / dfssm

msr <- ssr / dfssr


f <- msm / msr

# -------------------------------------- # 
# res.aov <- df %>%
#   anova_test(dv = rt, wid = participant, within = c(feedback, congruency, congruencyNmin1))
# get_anova_table(res.aov)
# 
# xtabs( ~ feedback + congruency, df)
# 
# l <- aggregate(rt ~ congruencyNmin1 + congruency + feedback + participant, df, mean)
# l$fcon <- paste(l$feedback, l$congruency, sep="_")
# 
# model <- aov(rt ~ feedback, gdf)
# 
# summary(model)
# 
# l %>%
#   ggplot(aes(congruencyNmin1, rt, group=fcon, color=fcon)) +
#   geom_line() +
#   geom_point()
# -------------------------------------- #
