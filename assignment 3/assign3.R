library(tidyverse)
library(modelsummary)
library(marginaleffects)

setwd("C:/Users/jorge/Documents/aqmssii/assignment 3")

raw <- read_csv("anes_timeseries_2020.csv")

data <- raw %>% 
  transmute(
    voted = ifelse(V202109x < 0, NA, V202109x),
    age= ifelse(V201507x < 0, NA, V201507x),
    female = case_when(
      V201600 == 2 ~ 1, 
      V201600 == 1 ~ 0,
      TRUE ~ NA_real_),
    education = case_when(
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14,
      V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_),
    income = ifelse(V201617x < 0, NA, V201617x),
    party_id = ifelse(V201231x < 0, NA, V201231x)
  )

data <- data %>% filter(!is.na(income))
# NEVER USE NA_OMIT !


data %>% 
  filter(!is.na(voted)) %>% 
  summarize(
    mean = mean(voted))


# Voter turnout was 86.2 %


# Voter Turnout X Education

data %>% 
  filter(!is.na(education), !is.na(voted)) %>% 
  group_by(education) %>% 
  summarize(
    turnout = mean(voted))



# Linear probability model

lpm = lm(voted ~ age + education + income + female, data = data)
modelsummary(lpm)


logit = glm(voted ~ age + education + income + female, family = binomial, data = data)
modelsummary(logit)

nd = data.frame(
  age = c(25, 50),
  education = c(10, 10),
  income = rep(20, 2),
  female = rep(1, 2)
)

predictions <- predict(logit, newdata = nd)
exp(predictions) / (1 + exp(predictions))
# or:
predictions <- predict(logit, newdata = nd, type = "response") #adding the type argument with "response"
predictions

avg_slopes(logit)


p1 = plot_predictions(logit, condition = "education")

logit2 = glm(voted ~ age + education*female + income, family = binomial, data = data)

predicha <- predict(logit2, newdata = nd, type = "response")
p2 = plot_predictions(logit2, condition = c("education", "female"))
p2
