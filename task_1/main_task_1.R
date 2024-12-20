# install.packages(c("readxl", "tidyverse", "dplyr", "ggplot2", "multcomp", "readxl", "broom", "gridExtra", "rstatix", "readxl", "knitr"))
# install.packages("tableone", repos = "https://cloud.r-project.org/")
# install.packages("MatchIt", repos = "https://cloud.r-project.org/")
# install.packages("glmnet", repos = "https://cloud.r-project.org/")
# install.packages("rddensity", repos = "https://cloud.r-project.org/")
# install.packages("WeightIt", repos = "https://cloud.r-project.org/")
# install.packages("grf", repos = "https://cloud.r-project.org/")
# install.packages("caTools", repos = "https://cloud.r-project.org/")

library(tidyverse)
library(dplyr)
library(tableone)
library(ggplot2)
library(multcomp)
library(readxl)
library(broom)
library(gridExtra)
library(rstatix)
library(readxl)
library(knitr)
library("MatchIt")
library(glmnet)
library(tidyverse)
library(rddensity)
library('WeightIt')
library('tableone')
library(boot)
library(grf)
library(hdm)
library(caTools)

# Загрузка данных
task_data <- read_excel("../data/wage_gap.xlsx", na = c(".", "", " "))
set.seed(239)
# Добавление HGT_parent
task_data <- task_data %>%
  mutate(
    HGT_parent = coalesce(HGT_mother, HGT_father)
  )

task_data <- task_data %>%
  filter(
    !(HGT_parent %in% boxplot(HGT_parent)$out)
      &
      !(education %in% boxplot(education)$out)
      &
      !(size_of_firm %in% boxplot(size_of_firm)$out)
      &
      !(AFQT2 %in% boxplot(AFQT2)$out)
      &
      !(years %in% boxplot(years)$out)
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
  ) %>% ungroup()

# Расчет средних значений для каждой группы
means <- task_data %>%
  group_by(moved) %>%
  summarize(
    m_fam_size = mean(fam_size, na.rm = TRUE),
    m_education = mean(education, na.rm = TRUE),
    m_HGT_parent = mean(HGT_parent, na.rm = TRUE),
    m_self_conf = mean(self_conf, na.rm = TRUE),
    m_size_of_firm = mean(size_of_firm, na.rm = TRUE),
    m_risk = mean(risk, na.rm = TRUE),
  )
kable(means, caption = "Средние значения для каждой группы")

variables <- c("fam_size", "education", "HGT_parent", "self_conf", "size_of_firm", "risk")
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
  data <- as.data.frame(create_table(pairwise_test_results, var))
  cat("\n")
  print(sprintf("p-value между группами для %s", var))
  print(data)
}

# # end of 1 task

#Добавление treatment
#переехал и сейчас живет в городе
all_convariants <- c("children", "fam_size", "married", "region", "years", "class_of_work", "education", "HGT_parent", "risk", "size_of_firm", "self_conf", "white", "woman")
task_data <- task_data %>%
  mutate(
    treatment = ifelse((moved == 3 & SMSA_central == 1), 1, 0)
  )
task_data <- task_data %>%
  filter((moved == 2 | moved == 3) & years > 21) %>%
  select(-n, -moved, -hours, -av_central_SMSA, -SMSA_central, -not_central_SMSA, -SMSA_not, -urban, -wage, -HGT_father, -HGT_mother, -sample_id_79, -union, -black)
task_data <- na.omit(task_data)
task_data <- task_data %>%
  filter(
    !(HGT_parent %in% boxplot(HGT_parent)$out)
      &
      !(education %in% boxplot(education)$out)
      &
      !(size_of_firm %in% boxplot(size_of_firm)$out)
      &
      !(AFQT2 %in% boxplot(AFQT2)$out)
      &
      !(years %in% boxplot(years)$out)
  )

model <- lm(cpi_w ~ treatment, data = task_data)
model_summary <- summary(model)
model_summary
table1 <- CreateTableOne(vars = all_convariants, strata = "treatment", data = task_data, test = TRUE)
table1

# Осмысленный вывод почему смещена
# Из таблицы видно что баланс ковариатов не соблюдается, следовательно оценка смещена так как
# между группами есть статистически значимое различие
# end of 2 task

m.out <- matchit(
  formula = treatment ~ children +
    fam_size +
    married +
    region +
    years +
    class_of_work +
    education +
    HGT_parent +
    risk +
    size_of_firm +
    self_conf +
    white +
    woman +
    AFQT2,
  data = task_data,
  nearest = "optimal",
  distance = "mahalanobis",
  estimand = 'ATT'
)
summary(m.out)
matched_data <- match.data(m.out)
model <- lm(cpi_w ~ treatment, data = matched_data)
summary(model)


# task 7
effect_fun <- function(data, indices) {
  boot_sample <- data[indices,]
  boot_model <- lm(cpi_w ~ treatment, data = boot_sample)
  return(coef(boot_model)["treatment"]) # бутстрапируемая статистика -- оценка ATE
}

# Бутстрап
boot_results <- boot(data = matched_data, statistic = effect_fun, R = 1000)

# 95% доверительный интервал
boot.ci(boot_results, conf = 0.95, type = "perc") #перцентильный метод построения доверительного интервала
# end of 3 task

table1 <- CreateTableOne(vars= all_convariants, strata = "treatment", data=matched_data, test=TRUE)
table1
# Вывод что при матчинге лучше соблюдается баланс ковариатов и оценка ATE лучше
# Баланс ковариатов улучшился при матчинге

# end of 4 task

# Модель пропенси-скора
propscore <- weightit(
  formula = treatment ~ children + fam_size + married + region,
  data = task_data,
  estimand = 'ATT',
  method = 'ps',
)
head(task_data, 100000)
summary(propscore)
head(propscore$weights, 10)
result <- lm(cpi_w ~ treatment, data = task_data, weights = propscore$weights)
summary(result)

#end of 5 task
X <- model.matrix(data = task_data, cpi_w ~
  treatment +
    region +
    education +
    HGT_parent +
    risk +
    size_of_firm +
    self_conf +
    white +
    woman +
    AFQT2)
head(X, 3)
DR <- rlassoEffects(X, task_data$cpi_w, 2, data = task_data)
# тритмент стоит на 2м месте в матрице X
print_coef(DR) # ATE
# confint(DR)
summary(DR)


# DR$selection.matrix #кого выкинули или оставили
# DR$coef.mat #вывести все коэффициенты
# Y <- task_data$cpi_w
# head(Y, 5)
# X <- model.matrix(data = task_data, cpi_w ~ 0 +
#   treatment +
#   region +
#   education +
#   HGT_parent +
#   risk +
#   size_of_firm +
#   self_conf +
#   white +
#   woman)
# head(X, 5)
#
# cv <- cv.glmnet(X, Y, alpha = 1)
# coef(cv, s = "lambda.1se")

#end of 6 task

split <- sample.split(rownames(task_data), SplitRatio = .8)

data_train_sample <- task_data %>% subset(split == TRUE)
data_test_sample <- task_data %>% subset(split == FALSE)
print("Sizez")
print(nrow(data_test_sample))
print(nrow(data_train_sample))

data_train <- list(
  df = data_train_sample,
  Y = data_train_sample$cpi_w,
  W = data_train_sample$treatment,
  X = data_train_sample %>% select(-cpi_w, -treatment)
)

data_test <- list(
  df = data_test_sample,
  Y = data_test_sample$cpi_w,
  W = data_test_sample$treatment,
  X = data_test_sample %>% select(-cpi_w, -treatment)
)

# B <- 100000 # количество деревьев - такой же результат
B <- 10000
tau.forest <- causal_forest(data_train$X, data_train$Y, data_train$W, num.trees = B)
tau.hat <- predict(tau.forest, data_test$X, estimate.variance = TRUE)

# Построим 95%-й доверительный интервал для оценок
# head(tau.hat$predictions, 2) # вектор оценок HTE
# head(tau.hat$variance.estimates, 2) # вектор дисперсий оценок HTE

sigma.hat <- sqrt(tau.hat$variance.estimates)
CI <- list(
  L = tau.hat$predictions - qnorm(0.95) * sigma.hat, # нижняя граница
  U = tau.hat$predictions + qnorm(0.95) * sigma.hat # верхняя граница
)

D <- data.frame( # объединим все в табличку
  predicted_effect = tau.hat$predictions,
  CI_lower_bound = CI$L,
  CI_upper_bound = CI$U
)
head(D, 3)

var_imp <- variable_importance(tau.forest)
var_imp[is.na(var_imp)] <- 0
names <- colnames(data_test$X)
head(var_imp, 20)
head(names, 20)
var_imp_df <- data.frame(importance = var_imp, variable = names) %>% arrange(desc(importance))
var_imp_df

ggplot(var_imp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Переменная", y = "Важность")

hist(tau.hat$predictions, main = "Гистограмма распределения эффектов от фактора переезда")

plot_data <- data.frame(
  predicted_effect = tau.hat$predictions,
  years = data_test$X$years,
  self_conf = data_test$X$self_conf,
  t = data_test$X$t,
  size_of_firm = data_test$X$size_of_firm,
  education = data_test$X$education,
  AFQT2 = data_test$X$AFQT2,
  self_conf = data_test$X$self_conf
)

ggplot(
  data = plot_data,
  mapping = aes(x = size_of_firm, y = predicted_effect)
) + geom_point()

ggplot(
  data = plot_data,
  mapping = aes(x = education, y = predicted_effect)
) + geom_point()

ggplot(
  data = plot_data,
  mapping = aes(x = years, y = predicted_effect)
) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(
  data = plot_data,
  mapping = aes(x = t, y = predicted_effect)
) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(
  data = plot_data,
  mapping = aes(x = AFQT2, y = predicted_effect)
) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(
  data = plot_data,
  mapping = aes(x = self_conf, y = predicted_effect)
) +
  geom_point() +
  geom_smooth(method = "loess")

# ggplot(plot_data, aes(x = HGT_parent, y = predicted_effect)) +
#   geom_point() +
#   labs(x = "HGT_parent", y = "Предсказанный эффект") +
#   ggtitle("Зависимость предсказанного эффекта от HGT_parent")
# ggplot(data = plot_data) + geom_bar(aes(x = HGT_parent, y = predicted_effect), fill = "blue") +
#   ggtitle("HGT_parent -> predicted_effect") +
#   xlab("HGT_parent") +
#   ylab("predicted_effect")
# ggplot(plot_data, aes(x = HGT_parent, y = predicted_effect)) +
#   geom_point() +
#   labs(x = "Рост родителя (HGT_parent)",
#        y = "Предсказанный эффект",
#        title = "Зависимость предсказанного эффекта от роста родителя")

# ggplot(data = data_test$X, aes(x = data_test$X$HGT_parent, y = tau.hat$predictions)) +
#   geom_point() +
#   geom_smooth(method = "loess") +
#   labs(x = "HGT_parent", y = "Predictions")

# key_vars <- var_imp_df$variable[1:5]
# head(key_vars)
# for (var in key_vars) {
#   ggplot(data = data_testt, aes(x = .data[[var]], y = tau.hat$predictions)) +
#     geom_point() +
#     geom_smooth(method = "loess") +
#     labs(x = var, y = "HTE")
# }

# plot(variable_importance(tau.forest))

# head(average_treatment_effect(tau.forest, target.sample = "treated"))
#
# # head(variable_importance(tau.forest)) # какой вклад (доля объясненной дисперсии) вносит каждая переменная в гетерогенность эффекта
#
# tau.forest %>% # таблица с вкладом каждой переменной
#   variable_importance() %>%
#   as.data.frame() %>%
#   mutate(variable = colnames(tau.forest$X.orig)) %>%
#   arrange(desc(V1))
#
#
# mean(as.numeric(CI$U<0)) # для 44% тестовой выборки эффект значимый отрицательный (B=10000)
# mean(as.numeric(CI$L>0)) # для 2,5% тестовой выборки эффект значимый положительный
#
# plot(tau.hat$predictions) # эффект для каждого наблюдения
# summary(tau.hat$predictions)
plot(tau.hat$predictions ~ data_test$df$region, type = "S") #в зависимости от региона
#
# ggplot(data_test$df) + geom_boxplot(aes(factor(white), tau.hat$predictions))