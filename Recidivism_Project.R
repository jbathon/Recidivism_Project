rm(list = ls())
library(tidyverse)
library(lubridate)

recid <- read.csv("datasets/Project3Sample4000.csv")

# Functions

createTraining <- function(data, seed = 123, trainPercent = 0.8) {
  set.seed(seed)
  n <- nrow(data)
  
  numTrain <- floor(trainPercent * n)
  trainingRows <- sample(1:n, size = numTrain, replace = FALSE)
  
  trainingData <- data[trainingRows, ]
  testingData <- data[-trainingRows, ]
  
  return(list(training = trainingData, testing = testingData))
}

logicRegression <- function(data, model) {
  
  numTrain <- nrow(data)
  
  trainingPredict <- predict.glm(model, newdata=data, type="response")
  
  trainingWithPredictions <- cbind(data,trainingPredict) %>% 
    mutate(prediction = ifelse(trainingPredict < 0.5, "Not Heart Disease", "Heart Disease"))
  
  trainingMatrix <- table(trainingWithPredictions$isRecid,trainingWithPredictions$prediction)
  
  error <- (trainingMatrix[1,2] + trainingMatrix[2,1])/numTrain
  
  pHat <- (trainingMatrix[2,1]+trainingMatrix[2,2])/numTrain
  
  standardError <- sqrt(pHat*(1-pHat)/numTrain)
  
  pValue <- pnorm(error,pHat,standardError)
  
  return(list(prediction = trainingWithPredictions, matrix = trainingMatrix, error = error, pValue = pValue))
} 

# Task 1

## Data Cleaning
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
    ageCat = as.factor(ageCat),
    race = as.factor(race),
    jailIn = as.Date(dmy_hm(jailIn, tz = "EST")),
    jailOut = as.Date(dmy_hm(jailOut, tz = "EST")),
    chargeDegree = as.factor(gsub("[()]","",chargeDegree)),
    riskRecidScoreLevel = as.factor(riskRecidScoreLevel),
    riskRecidScreeningDate = as_date(dmy(riskRecidScreeningDate))
    )

## Data Engineering

strptime(recid2$jailIn, format = "%Y %m %d")

recid3 <- recid2 %>% 
  mutate(
    daysInJail = as.numeric(difftime(jailOut,jailIn,unit="days")+1),
    logDaysInJail = log10(daysInJail),
    juvTotalCount = juvFelonyCount + juvMisdemeanerCount + juvOtherCount,
  ) %>% 
  select(
    -name,
    -dob,
    -race
  )

## Testing Training Split

testingTraining <- createTraining(recid3)

recidTraining <- testingTraining$training

recidTesting <-  testingTraining$testing

# Task 2

model1 <- glm(isRecid ~ sex + age + juvTotalCount + priorsCount + logDaysInJail,data=recidTraining, family = binomial)

summary(model1)

numTrain <- nrow(recidTraining)

recidPredict <- predict.glm(model1, newdata=recidTraining, type="response")

trainingWithPredictions <- cbind(recidTraining,recidPredict) %>% 
  mutate(prediction = ifelse(recidPredict < 0.43, "No No Reaffend", "Reaffend Uh oh"))

trainingMatrix <- table(trainingWithPredictions$isRecid,trainingWithPredictions$prediction)

(error <- (trainingMatrix[1,2] + trainingMatrix[2,1])/numTrain)

(pHat <- (trainingMatrix[2,1]+trainingMatrix[2,2])/numTrain)

(standardError <- sqrt(pHat*(1-pHat)/numTrain))

(pValue <- pnorm(error,pHat,standardError))

model2 <- glm(isRecid ~ sex + age + priorsCount + daysInJail,data=recidTraining, family = binomial)

summary(model2)

numTrain <- nrow(recidTraining)

recidPredict <- predict.glm(model2, newdata=recidTraining, type="response")

trainingWithPredictions <- cbind(recidTraining,recidPredict) %>% 
  mutate(prediction = ifelse(recidPredict < 0.431, "No No Reaffend", "Reaffend Uh oh"))

trainingMatrix <- table(trainingWithPredictions$isRecid,trainingWithPredictions$prediction)

(error <- (trainingMatrix[1,2] + trainingMatrix[2,1])/numTrain)

(pHat <- (trainingMatrix[2,1]+trainingMatrix[2,2])/numTrain)

(standardError <- sqrt(pHat*(1-pHat)/numTrain))

(pValue <- pnorm(error,pHat,standardError))

testingPredict <- predict.glm(model2, newdata=recidTesting, type="response")

testingWithPredictions <- cbind(recidTesting,testingPredict) %>% 
  mutate(prediction = ifelse(testingPredict < 0.431, "No No Reaffend", "Reaffend Uh oh"))

testingMatrix <- table(testingWithPredictions$isRecid,testingWithPredictions$prediction)

(testingError <- (error <- (testingMatrix[1,2] + testingMatrix[2,1])/numTrain))

recidMysteryBox <- read.csv("datasets/Project3Mystery100.csv")



