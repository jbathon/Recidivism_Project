rm(list = ls())
library(tidyverse)
library(lubridate)
library(olsrr)
library(stringr)
library(ggfortify)
library(patchwork)

recid <- read.csv("datasets/Project3Sample4000.csv")

createTraining <- function(data, seed = 123, trainPercent = 0.8) {
  set.seed(seed)
  n <- nrow(data)
  
  numTrain <- floor(trainPercent * n)
  trainingRows <- sample(1:n, size = numTrain, replace = FALSE)
  
  trainingData <- data[trainingRows, ]
  testingData <- data[-trainingRows, ]
  
  return(list(training = trainingData, testing = testingData))
}

# Data Cleaning
recid2 <- recid %>%
  rename(
    dayBefScreenArrest = days_b_screening_arrest,
    jailIn = c_jail_in,
    jailOut = c_jail_out,
    daysFromCompas = c_days_from_compas,
    chargeDegree = c_charge_degree,
    chargeDesc = c_charge_desc,
    riskRecidDecileScore = RiskRecidDecileScore,
    riskRecidScoreLevel = RiskRecidScoreLevel,
    riskRecidScreeningDate = RiskRecidScreeningDate,
    riskViolenceDecileScore = RiskViolenceDecileScore,
    riskViolenceScoreLevel = RiskViolenceScoreLevel
  ) %>%
  mutate(
    dob = as_date(dmy(dob)),
    ageCat = factor(as.factor(ageCat), levels = c("Less than 25", "25 - 45", "Greater than 45")),
    race = factor(as.factor(race), levels = c("white", "black", "hispanic", "other")),
    jailIn = as.Date(dmy_hm(jailIn, tz = "EST")),
    jailOut = as.Date(dmy_hm(jailOut, tz = "EST")),
    chargeDegree = as.factor(gsub("[()]", "", chargeDegree)),
    riskRecidScoreLevel = as.factor(riskRecidScoreLevel),
    riskRecidScreeningDate = as_date(dmy(riskRecidScreeningDate)),
    recidCat = fct_recode(as.factor(isRecid), Yes = "1", No = "0")
  ) %>%
  select(-name, -dob) %>%
  filter(!is.na(isRecid) & (!is.na(jailIn) | !is.na(jailOut)))

## Data Engineering

recid3 <- recid2 %>%
  mutate(
    daysInJail = as.numeric(difftime(jailOut, jailIn, unit = "days") + 1),
    logPriorsCount = log10(priorsCount + 0.1),
    juvCount = juvFelonyCount + juvMisdemeanerCount + juvOtherCount,
    logDaysInJail = log10(daysInJail),
    chargeDescCount = str_count(chargeDesc),
    logChargeDescCount = log10(chargeDescCount + 0.1),
    logJuvCount = log10(juvCount + 0.1)
  )

recid4 <- recid3 %>%
  select(
    -race
  )

jailTimeTestingTraining <-  createTraining(recid4, seed=123)

jailTimeTraining <- jailTimeTestingTraining$training
jailTimeTesting <- jailTimeTestingTraining$testing


# Visuals

jjIntervals <- function(data, model) {
  confidence <- as.data.frame(predict.lm(model, newdata = data, interval = "confidence")) %>%
    rename(confLwr = lwr, confUpr = upr)
  
  prediction <- as.data.frame(predict.lm(model, newdata = data, interval = "prediction")) %>%
    rename(predictLwr = lwr, predictUpr = upr) %>%
    select(predictLwr, predictUpr)
  
  intervalData <- cbind(data, confidence, prediction)
  
  return(intervalData)
}

jjplotPoint <- function(data, x, y, color, model) {
  data <- jjIntervals(data, model)
  plot <- ggplot(data = data, aes(x = {{ x }}, y = {{ y }}, color = {{ color }})) +
    geom_jitter() +
    geom_ribbon(aes(ymin = confLwr, ymax = confUpr), fill = "yellow", alpha = 0.4) +
    geom_line(aes(y = fit), color = "#3366FF", size = 0.75) +
    geom_line(aes(y = confLwr), linetype = "dashed", size = 0.75) +
    geom_line(aes(y = confUpr), linetype = "dashed", size = 0.75) +
    geom_line(aes(y = predictLwr), linetype = "dashed", color = "red", size = 0.75) +
    geom_line(aes(y = predictUpr), linetype = "dashed", color = "red", size = 0.75)
  return(plot)
}

jjplotLogPoint <- function(data, x, y, color, model) {
  data <- jjIntervals(data, model)
  plot <- ggplot(data = data, aes(x = {{ x }}, y = {{ y }}, color = {{ color }})) +
    geom_jitter() +
    geom_ribbon(aes(ymin = 10^confLwr, ymax = 10^confUpr), fill = "yellow", alpha = 0.4) +
    geom_line(aes(y = 10^fit), color = "#3366FF", size = 0.75) +
    geom_line(aes(y = 10^confLwr), linetype = "dashed", size = 0.75) +
    geom_line(aes(y = 10^confUpr), linetype = "dashed", size = 0.75) +
    geom_line(aes(y = 10^predictLwr), linetype = "dashed", color = "red", size = 0.75) +
    geom_line(aes(y = 10^predictUpr), linetype = "dashed", color = "red", size = 0.75)
  return(plot)
}

jjplotDensity <- function(data, x, fill, color) {
  plot <- ggplot(data, aes(x = {{ x }})) +
    geom_density(aes(fill = {{ fill }}), alpha = 0.4) +
    geom_rug(aes(color = {{ color }}), y = 0) +
    theme(legend.position = "none")
  return(plot)
}

## Days in Jail vs Priors Count

proirsModel <- lm(logDaysInJail ~ logPriorsCount, jailTimeTraining)

jjplotLogPoint(data = jailTimeTraining,x = logPriorsCount,y=daysInJail,color=recidCat,model=proirsModel) +
  theme(legend.position = "none") +
  labs(
    title="Days in Jail vs log10(Priors Count)",
    x="log10(Priors Count)",
    y="Days in Jail"
  )

## Days in Jail vs Age

ageModel <- lm(logDaysInJail ~ age, jailTimeTraining)

jjplotLogPoint(data = jailTimeTraining,x = age,y=daysInJail,color=recidCat,model=ageModel) +
  theme(legend.position = "none")

## Days in Jail vs Juvenile Priors Count

juvModel <- lm(logDaysInJail ~ juvCount, jailTimeTraining)

jjplotLogPoint(data = jailTimeTraining,x = priorsCount,y=daysInJail,color=recidCat,model=juvModel) +
  theme(legend.position = "none")

## Days in Jail vs Charge Description Word Count
chargeDescModel <- lm(logDaysInJail ~ chargeDescCount, jailTimeTraining)

jjplotLogPoint(data = jailTimeTraining,x = chargeDescCount,y=daysInJail,color=recidCat,model=chargeDescModel) +
  theme(legend.position = "none")

## Days in Jail vs Charge Degree

jjplotDensity(data = jailTimeTraining, x=logDaysInJail,fill=recidCat) +
  facet_wrap(~chargeDegree) +
  labs(
    title = "Charge Degree",
    x="Log10(Day In Jail)"
  )

## Days in Jail vs Sex

jjplotDensity(data = jailTimeTraining, x=logDaysInJail,fill=recidCat) +
  facet_wrap(~sex) +
  labs(
    title = "Sex",
    x="Log10(Days In Jail)"
  )

# Best Subset

jailTimeEverythingModel <- lm(logDaysInJail ~ sex + age + chargeDegree + priorsCount + juvCount + logPriorsCount + logJuvCount + chargeDescCount + logChargeDescCount + riskRecidDecileScore + riskViolenceDecileScore, data=jailTimeTraining)
ols_step_best_subset(jailTimeEverythingModel)

RMSE <- function(predict, obs) {
  RMSE <- sqrt(mean((predict - obs)^2, na.rm = TRUE))
  return(RMSE)
}

bestJailTimeModel <-  lm(logDaysInJail ~ sex + chargeDegree + logPriorsCount, data=jailTimeTraining)

jailTimeTrainingPredict <- 10^predict.lm(bestJailTimeModel,newdata = jailTimeTraining)

cor(jailTimeTrainingPredict,jailTimeTraining$daysInJail)

RMSE(jailTimeTrainingPredict,jailTimeTraining$daysInJail)

jailTimeTestingPredict <- 10^predict.lm(bestJailTimeModel,newdata = jailTimeTesting)

RMSE(jailTimeTestingPredict,jailTimeTesting$daysInJail)

autoplot(jailTimeEverythingModel, 1:2)


