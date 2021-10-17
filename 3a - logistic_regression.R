library(tidyverse)
library(MASS)
library(pROC)
library(ROCit)


# Data -----
data <- read_csv("data-post-processed.csv")
data$DESEMPENHO <- factor(data$DESEMPENHO)
data$DESEMPENHO <- relevel(data$DESEMPENHO, ref = "Baixo")

set.seed(1987)
index_data <-  sort(sample(nrow(data), nrow(data)*.7))
train <- data[index_data,]
test <- data[-index_data,]

# Model -----
model <- glm(DESEMPENHO ~ ., data = train, family = binomial) %>%
  stepAIC(trace = FALSE)

summary(model)
summary(model)$coef

# Odds ratio ----
exp(0.132354) # IDADE
exp(-0.835080368) # Turno noturno
exp(-0.708106464) # Trabalha sim

# AUCs
test_prob <- predict(model, newdata = test, type = "response")
roc(test$DESEMPENHO ~ test_prob, plot = TRUE, print.auc = TRUE, legacy.axes = TRUE)
