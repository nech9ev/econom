# install.packages(c("readxl", "tidyverse", "dplyr", "ggplot2", "ggpubr", "car", "multcomp", "readxl", "broom", "gridExtra", "rstatix", "gtExtras"))

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(car)
library(multcomp)
library(readxl)
library(broom)
library(gridExtra)
library(rstatix)
library(readxl)
library(knitr)

# Загрузка данных
task_data <- read_excel("data/wage_gap.xlsx")

# Добавление HGT_parent
task_data <- task_data %>%
  mutate(
    HGT_parent =
      ifelse(
        test = is.na(HGT_mother) | is.na(HGT_father),
        yes = coalesce(HGT_mother, HGT_father),
        no = (HGT_mother + HGT_father) / 2.0
      )
  )

# Добавление moved
task_data <- task_data %>%
  group_by(n) %>%
  mutate(
    moved = case_when(
      all(SMSA_central == 1) ~ 1,  # Всегда жил в городе
      all(SMSA_central == 0) ~ 2,  # Всегда жил вне города
      first(SMSA_central) == 0 ~ 3,  # Переехал в город
      first(SMSA_central) == 1 ~ 4,  # Уехал из города
    )
  ) %>%
  ungroup()

##todo remove
str(task_data)

# Расчет средних значений для каждой группы
variables <- c("fam_size", "education", "HGT_parent", "self_conf", "size_of_firm", "risk")
means <- task_data %>%
  group_by(moved) %>%
  summarize(
    mean_fam_size = mean(fam_size, na.rm = TRUE),
    mean_education = mean(education, na.rm = TRUE),
    mean_HGT_parent = mean(HGT_parent, na.rm = TRUE),
    mean_self_conf = mean(self_conf, na.rm = TRUE),
    mean_size_of_firm = mean(size_of_firm, na.rm = TRUE),
    mean_risk = mean(risk, na.rm = TRUE),
  )
kable(means, caption = "Средние значения для каждой группы")

pairwise_test_results <- task_data %>%
  gather(variable, value, all_of(variables)) %>%
  group_by(variable) %>%
  pairwise_wilcox_test(value ~ moved, p.adjust.method = "bonferroni")

create_table <- function(df, var) {
  df %>%
    filter(variable == var) %>%
    select(group1, group2, p.adj)
}

for (var in variables) {
  table_for_var <- as.data.frame(create_table(pairwise_test_results, var))
  cat("\n\n")
  print(sprintf("p-value значения между группами moved для %s", var))
  print(table_for_var)
}
