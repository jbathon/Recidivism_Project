rm(list = ls())
library(tidyverse)
library(lubridate)
library(olsrr)
library(patchwork)

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
    ageCat = factor(as.factor(ageCat),levels=c("Less than 25","25 - 45","Greater than 45")),
    race = factor(as.factor(race),levels=c("white","black","hispanic","other")),
    jailIn = as.Date(dmy_hm(jailIn, tz = "EST")),
    jailOut = as.Date(dmy_hm(jailOut, tz = "EST")),
    chargeDegree = as.factor(gsub("[()]","",chargeDegree)),
    riskRecidScoreLevel = as.factor(riskRecidScoreLevel),
    riskRecidScreeningDate = as_date(dmy(riskRecidScreeningDate)),
    recidCat = fct_recode(as.factor(isRecid),Yes = "1", No = "0")
    ) %>% 
  select(-name,-dob) %>% 
  filter(!is.na(isRecid) & (!is.na(jailIn) | !is.na(jailOut)))

## Data Engineering

recid3 <- recid2 %>% 
  mutate(
    daysInJail = as.numeric(difftime(jailOut,jailIn,unit="days")+1),
    logDaysInJail = log10(daysInJail),
    logPriorsCount = log10(priorsCount+0.1),
    juvCount = juvFelonyCount + juvMisdemeanerCount + juvOtherCount,
    logJuvCount = log10(juvCount+0.1)
  )

recid4 <- recid3 %>% 
  select (
    -race
  )

## Testing Training Split

testingTraining <- createTraining(recid4, seed=8675309)

recidTraining <- testingTraining$training

recidTesting <-  testingTraining$testing

## Visualize Data

### Functions

jjIntervals <-  function(data, model) {
  
  confidence <- as.data.frame(predict.lm(model, newdata = data, interval = "confidence")) %>% 
    rename(confLwr = lwr, confUpr = upr)
  
  prediction <- as.data.frame(predict.lm(model, newdata = data, interval = "prediction")) %>%
    rename(predictLwr = lwr, predictUpr = upr) %>%
    select(predictLwr, predictUpr)
  
  intervalData <- cbind(data,confidence,prediction)
  
  return(intervalData)
}

jjplotDensity <- function(data,x,fill,color) {
  plot <- ggplot(data, aes(x={{x}})) +
    geom_density(aes(fill={{fill}}), alpha=0.4)+
    geom_rug(aes(color={{color}}), y=0) +
    theme(legend.position = "none")
  return(plot)
}

jjplotBoxplot <- function(data,x,y,fill) {
  plot <- ggplot(data=data, aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
    geom_boxplot() +
    coord_flip() +
    theme(legend.position = "none")
  return(plot)
}

jjplotPoint <- function(data,x,y,color, model) {
  data <- jjIntervals(data,model)
  plot <- ggplot(data=data, aes(x = {{x}}, y = {{y}}, color = {{color}})) +
    geom_jitter() +
    geom_ribbon(aes(ymin = confLwr, ymax = confUpr), fill = "yellow", alpha = 0.4) +
    geom_line(aes(y = fit), color = "#3366FF", size = 0.75) +
    geom_line(aes(y = confLwr), linetype = "dashed", size = 0.75) +
    geom_line(aes(y = confUpr), linetype = "dashed", size = 0.75) +
    geom_line(aes(y = predictLwr), linetype = "dashed", color = "red", size = 0.75) +
    geom_line(aes(y = predictUpr), linetype = "dashed", color = "red", size = 0.75)
  return(plot)
}

### DaysInJail

p1 <- recidTraining %>% 
  jjplotDensity(x = daysInJail, fill = as.factor(isRecid), color = as.factor(isRecid)) +
  labs(
    title="Days in Jail",
    x = "Days in Jail",
  )

p2 <- recidTraining %>% 
  jjplotDensity(x = logDaysInJail, fill = as.factor(isRecid), color = as.factor(isRecid)) +
  labs(
    title="log10(Days in Jail",
    x = "log10(Days in Jail)",
  )

p3 <- recidTraining %>% 
  jjplotBoxplot(x = isRecid, y=daysInJail, fill=as.factor(isRecid)) +
  labs(
    title="Days in Jail",
    x = "Days in Jail",
    y = "Recidivated"
  )

p4 <- recidTraining %>% 
  jjplotBoxplot(x = isRecid, y=logDaysInJail, fill=fct_recode(as.factor(isRecid),Yes = "1", No = "0")) +
  
  labs(
    title="log10(Days in Jail)",
    x = "log10(Days in Jail)",
    y = "Recidivated",
    fill = "Recidivated"
  )

p1 + p2 + p3 + p4 + 
  plot_annotation(
    title = "Days in Jail and log10(Days in Jail)",
  ) + plot_layout(guides = 'collect')

### Priors Count

p5 <- recidTraining %>% 
  jjplotDensity(x = priorsCount, fill = as.factor(isRecid), color = as.factor(isRecid)) +
  labs(
    title="Priors Counts",
    x = "Priors Counts"
  )
p6 <- recidTraining %>% 
  jjplotDensity(x = logPriorsCount, fill = as.factor(isRecid), color = as.factor(isRecid)) +
  labs(
    title="log10(Priors Counts) + 0.1",
    x = "log10(Priors Counts) + 0.1"
  )
p7 <- recidTraining %>% 
  jjplotBoxplot(x = isRecid, y=priorsCount, fill=as.factor(isRecid)) +
  labs(
    title="Priors Counts",
    x = "Priors Counts",
    y = "Recidivated",
    fill = "Recidivated"
  )
p8 <- recidTraining %>% 
  jjplotBoxplot(x = isRecid, y=logPriorsCount, fill=fct_recode(as.factor(isRecid),Yes = "1", No = "0")) +
  theme(legend.position = "right") +
  labs(
    title="log10(Priors Counts) + 0.1",
    x = "log10(Priors Counts) + 0.1",
    y = "Recidivated",
    fill = "Recidivated"
  )
p5 + p6 + p7 + p8 + plot_annotation(title = "Priors Counts") + plot_layout(guides = 'collect')

### Juvenile Priors Count

p9 <- recidTraining %>% 
  jjplotDensity(x = juvCount, fill = as.factor(isRecid), color = as.factor(isRecid)) +
  labs(
    title="Juvenile Priors Counts",
    x = "Juvenile Priors Counts"
  )
p10 <- recidTraining %>% 
  jjplotDensity(x = logJuvCount, fill = as.factor(isRecid), color = as.factor(isRecid)) +
  labs(
    title="log10(Juvenile Priors Counts) + 0.1",
    x = "log10(Juvenile Priors Counts) + 0.1"
  )
p11 <- recidTraining %>% 
  jjplotBoxplot(x = isRecid, y=juvCount, fill=as.factor(isRecid)) +
  labs(
    title="Juvenile Priors Counts",
    x = "Juvenile Priors Counts",
    y = "Recidivated",
    fill = "Recidivated"
  )
p12 <- recidTraining %>% 
  jjplotBoxplot(x = isRecid, y=logJuvCount, fill=fct_recode(as.factor(isRecid),Yes = "1", No = "0")) +
  theme(legend.position = "right") +
  labs(
    title="log10(Juvenile Priors Counts) + 0.1",
    x = "log10(Juvenile Priors Counts) + 0.1",
    y = "Recidivated",
    fill = "Recidivated"
  )
p9 + p10 + p11 + p12 + plot_annotation(title = "Juvenile Priors Counts") + plot_layout(guides = 'collect')

### Age

p13 <- recidTraining %>% 
  jjplotDensity(x = age, fill = as.factor(isRecid), color = as.factor(isRecid)) +
  labs(
    title="Age",
    x = "Juvenile Priors Counts"
  )

p14 <- recidTraining %>% 
  jjplotBoxplot(x = isRecid, y=age, fill=fct_recode(as.factor(isRecid),Yes = "1", No = "0")) +
  theme(legend.position = "right") +
  labs(
    title="Age",
    x = "Age",
    y = "Recidivated",
    fill = "Recidivated"
  )

p13 / p14

### Sex

ggplot(data=recidTraining,aes(x=sex, fill=fct_recode(as.factor(isRecid),Yes = "1", No = "0"))) +
  geom_bar(position = "dodge") +
  labs(
    title="Sex",
    x = "Sex",
    fill = "Recidivated"
  )

### ChargeDegree

ggplot(data=recidTraining,aes(x=chargeDegree, fill=fct_recode(as.factor(isRecid),Yes = "1", No = "0"))) +
  geom_bar(position = "dodge") +
  labs(
    title="Charge Degree",
    x = "Charge Degree",
    fill = "Recidivated"
  )

### Colinearity Check

p15 <- ggplot(recidTraining, aes(x = logDaysInJail, y = logPriorsCount, color = fct_recode(as.factor(isRecid),Yes = "1", No = "0"))) +
  geom_point() +
  labs(
    title="log10(Days In Jail) vs log10(Priors Count)",
    x = "log10(Days In Jail)",
    y = "log10(Priors Count)",
    color = "Recidivated"
  )

p16 <- ggplot(recidTraining, aes(x = logDaysInJail, y = age, color = fct_recode(as.factor(isRecid),Yes = "1", No = "0"))) +
  geom_point() +
  labs(
    title="log10(Days In Jail) vs Age",
    x = "log10(Days In Jail)",
    y = "Age",
    color = "Recidivated"
  )

p17 <- ggplot(recidTraining, aes(x = logPriorsCount, y = age, color = fct_recode(as.factor(isRecid),Yes = "1", No = "0"))) +
  geom_point() +
  labs(
    title="log10(Priors Count) vs Age",
    x = "log10(Priors Count)",
    y = "Age",
    color = "Recidivated"
  )

p15 / (p16 + p17) + plot_annotation(title = "Colinearity Check") + plot_layout(guides = 'collect')

# Task 2

getPredict <- function(data, model) {
  
  recidPredict <- predict.glm(model, newdata=data, type="response")
  dataWithPredictions <- cbind(data,recidPredict)
  
  return(dataWithPredictions)
}

checkModel <- function(data, matrix) {
  n <- nrow(data)
  error <- (matrix[1,2] + matrix[2,1])/n
  
  pHat <- (matrix[2,1]+matrix[2,2])/n
  
  standardError <- sqrt(pHat*(1-pHat)/n)
  
  pValue <- pnorm(error,pHat,standardError)
  
  return(list("error" = error,"pValue" = pValue))
}

## Everything Model

everythingModel <- glm(isRecid ~  age + priorsCount + daysInJail + logPriorsCount + logDaysInJail,data=recidTraining, family = binomial)

everythingPredictTrain <- getPredict(recidTraining, everythingModel) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
everythingMatrixTrain <- table(everythingPredictTrain$isRecid,everythingPredictTrain$prediction)

everythingPredictTest <- getPredict(recidTesting, everythingModel) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
everythingMatrixTest <- table(everythingPredictTest$isRecid,everythingPredictTest$prediction)

(checkModel(recidTraining,everythingMatrixTrain))

(checkModel(recidTesting,everythingMatrixTest))

summary(everythingModel)

## Basic Model

baseModel <- glm(isRecid ~ sex + age + priorsCount + daysInJail,data=recidTraining, family = binomial)

basePredictTrain <- getPredict(recidTraining, baseModel) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
baseMatrixTrain <- table(basePredictTrain$isRecid,basePredictTrain$prediction)

basePredictTest <- getPredict(recidTesting, baseModel) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
baseMatrixTest <- table(basePredictTest$isRecid,basePredictTest$prediction)

(checkModel(recidTraining,baseMatrixTrain))

(checkModel(recidTesting,baseMatrixTest))

## Log Model

logModel <- glm(isRecid ~ sex + age + logPriorsCount + logDaysInJail, data=recidTraining, family = binomial)

logPredictTrain <- getPredict(recidTraining, logModel) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
logMatrixTrain <- table(logPredictTrain$isRecid,logPredictTrain$prediction)

logPredictTest <- getPredict(recidTesting, logModel) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
logMatrixTest <- table(logPredictTest$isRecid,logPredictTest$prediction)

(checkModel(recidTraining,logMatrixTrain))

(checkModel(recidTesting,logMatrixTest))

## Recid Model

recidModel <- glm(isRecid ~ sex + age + logPriorsCount + daysInJail, data=recidTraining, family = binomial)

recidPredictTrain <- getPredict(recidTraining, recidModel) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
recidMatrixTrain <- table(recidPredictTrain$isRecid,recidPredictTrain$prediction)

recidPredictTest <- getPredict(recidTesting, recidModel) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
recidMatrixTest <- table(recidPredictTest$isRecid,recidPredictTest$prediction)

(checkModel(recidTraining,recidMatrixTrain))

(checkModel(recidTesting,recidMatrixTest))

recidMysteryBox <- read.csv("datasets/Project3Mystery100.csv")

## Recid 2 Model

recid2Model <- glm(isRecid ~ age + logPriorsCount + daysInJail, data=recidTraining, family = binomial)

recid2PredictTrain <- getPredict(recidTraining, recid2Model) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
recid2MatrixTrain <- table(recid2PredictTrain$isRecid,recid2PredictTrain$prediction)

recid2PredictTest <- getPredict(recidTesting, recid2Model) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
recid2MatrixTest <- table(recid2PredictTest$isRecid,recid2PredictTest$prediction)

(checkModel(recidTraining,recid2MatrixTrain))

(checkModel(recidTesting,recid2MatrixTest))

## Recid 3 Model

recid4Model <- glm(isRecid ~ age + logPriorsCount, data=recidTraining, family = binomial)

recid3PredictTrain <- getPredict(recidTraining, recid3Model) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
recid3MatrixTrain <- table(recid3PredictTrain$isRecid,recid3PredictTrain$prediction)

recid3PredictTest <- getPredict(recidTesting, recid3Model) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
recid3MatrixTest <- table(recid3PredictTest$isRecid,recid3PredictTest$prediction)

(checkModel(recidTraining,recid3MatrixTrain))

(checkModel(recidTesting,recid3MatrixTest))

## Recid 4 Model

recid4Model <- glm(isRecid ~ age + sex + logPriorsCount, data=recidTraining, family = binomial)

recid4PredictTrain <- getPredict(recidTraining, recid4Model) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
recid4MatrixTrain <- table(recid4PredictTrain$isRecid,recid4PredictTrain$prediction)

recid4PredictTest <- getPredict(recidTesting, recid4Model) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
recid4MatrixTest <- table(recid4PredictTest$isRecid,recid4PredictTest$prediction)

(checkModel(recidTraining,recid4MatrixTrain))

(checkModel(recidTesting,recid4MatrixTest))

recidMysteryBox <- read.csv("datasets/Project3Mystery100.csv")

## Recid 5 Model

recid5Model <- glm(isRecid ~ age + priorsCount, data=recidTraining, family = binomial)

recid5PredictTrain <- getPredict(recidTraining, recid5Model) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
recid5MatrixTrain <- table(recid5PredictTrain$isRecid,recid5PredictTrain$prediction)

recid5PredictTest <- getPredict(recidTesting, recid5Model) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
recid5MatrixTest <- table(recid5PredictTest$isRecid,recid5PredictTest$prediction)

(checkModel(recidTraining,recid5MatrixTrain))

(checkModel(recidTesting,recid5MatrixTest))

## Recid 6 Model

recid6Model <- glm(isRecid ~ age + logPriorsCount + logDaysInJail, data=recidTraining, family = binomial)

recid6PredictTrain <- getPredict(recidTraining, recid6Model) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
recid6MatrixTrain <- table(recid6PredictTrain$isRecid,recid6PredictTrain$prediction)

recid6PredictTest <- getPredict(recidTesting, recid6Model) %>% 
  mutate(prediction = ifelse(recidPredict < 0.5, "Did Not Reaffend", "Reaffended"))
recid6MatrixTest <- table(recid6PredictTest$isRecid,recid6PredictTest$prediction)

(checkModel(recidTraining,recid6MatrixTrain))

(checkModel(recidTesting,recid6MatrixTest))

## Final Model

finalModel <-  recid6Model

# Recid Score Model & Visuals

testingTraining2 <- createTraining(recid3, seed="859")

recidTraining2 <- testingTraining2$training

recidTesting2 <-  testingTraining2$testing

## Visualization

## Days in Jail vs riskRecidDecileScore


p18 <- ggplot(data = recidTraining2, aes(x = daysInJail, y = riskRecidDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="Days in Jail",
    y="Risk Recidivate Score",
    fill = "Recidivated?"
  ) + 
  coord_flip()
  

p19 <- ggplot(data = recidTraining2, aes(x = daysInJail, y = riskRecidDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="log10(Days in Jail)",
    y="Risk Recidivate Score",
    fill = "Recidivated?"
  ) + 
  coord_flip()

p18 / p19 + plot_annotation(title = "Days in Jail") + plot_layout(guides = 'collect')

## Juvenile Count vs riskRecidDecileScore

p20 <- ggplot(data = recidTraining2, aes(x = juvCount, y = riskRecidDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="Juvenile Count",
    y="Risk Recidivate Score",
    fill = "Recidivated?"
  ) + 
  coord_flip()


p21 <- ggplot(data = recidTraining2, aes(x = logJuvCount, y = riskRecidDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="log10(Juvenile Count + 0.01)",
    y="Risk Recidivate Score",
    fill = "Recidivated?"
  ) + 
  coord_flip()

p20 / p21 + plot_annotation(title = "Juvenile Count") + plot_layout(guides = 'collect')

## Priors Count vs riskRecidDecileScore

p22 <- ggplot(data = recidTraining2, aes(x = priorsCount, y = riskRecidDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="Priors Count",
    y="Risk Recidivate Score",
    fill = "Recidivated?"
  ) + 
  coord_flip()


p23 <- ggplot(data = recidTraining2, aes(x = logPriorsCount, y = riskRecidDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="log10(Priors Count + 0.01)",
    y="Risk Recidivate Score",
    fill = "Recidivated?"
  ) + 
  coord_flip()

p22 / p23 + plot_annotation(title = "Priors Count") + plot_layout(guides = 'collect')

## Age vs riskRecidDecileScore

ggplot(data = recidTraining2, aes(x = age, y = riskRecidDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="Age",
    y="Risk Recidivate Score",
    fill = "Recidivated?",
    title = "Age"
  ) + 
  coord_flip()

## Age Cat vs riskRecidDecileScore
ggplot(data=recidTraining2, aes(riskRecidDecileScore, fill = recidCat)) +
  geom_density(alpha=.4) +
  facet_wrap(~ ageCat)

## Race vs riskRecidDecileScore

ggplot(data=recidTraining2, aes(riskRecidDecileScore, fill = recidCat)) +
  geom_density(alpha=.4) +
  facet_wrap(~ race)

## Charge Degree vs riskRecidDecileScore

ggplot(data=recidTraining2, aes(riskRecidDecileScore, fill = recidCat)) +
  geom_density(alpha=.4) +
  facet_wrap(~ chargeDegree)

## Best Subset

scoreSubsetModel <- lm(riskRecidDecileScore ~ logPriorsCount + priorsCount + age + chargeDegree + logDaysInJail + daysInJail + sex + race, data = recidTesting2)

(olsSubset <- ols_step_best_subset(scoreSubsetModel))

scoreFinalModel <- lm(riskRecidDecileScore ~ priorsCount + age + chargeDegree + logDaysInJail + race, data=recidTraining2)

RMSE <- function(predict, obs) {
  RMSE <- sqrt(mean((predict - obs)^2, na.rm = TRUE))
  return(RMSE)
}

scoreTestingPredicts <- predict.lm(scoreFinalModel, newdata=recidTesting2)

RMSE(scoreTestingPredicts,recidTesting2$riskRecidDecileScore)

## recidViolenceScoreDecileScore

## Days in Jail vs riskViolenceDecileScore


p24 <- ggplot(data = recidTraining2, aes(x = daysInJail, y = riskViolenceDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="Days in Jail",
    y="Risk Violence Score",
    fill = "Recidivated?"
  ) + 
  coord_flip()


p25<- ggplot(data = recidTraining2, aes(x = daysInJail, y = riskViolenceDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="log10(Days in Jail)",
    y="Risk Violence Score",
    fill = "Recidivated?"
  ) + 
  coord_flip()

p24 / p25 + plot_annotation(title = "Days in Jail") + plot_layout(guides = 'collect')

## Juvenile Count vs riskViolenceDecileScore

p26 <- ggplot(data = recidTraining2, aes(x = juvCount, y = riskViolenceDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="Juvenile Count",
    y="Risk Violence Score",
    fill = "Recidivated?"
  ) + 
  coord_flip()


p27 <- ggplot(data = recidTraining2, aes(x = logJuvCount, y = riskViolenceDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="log10(Juvenile Count + 0.01)",
    y="Risk Violence Score",
    fill = "Recidivated?"
  ) + 
  coord_flip()

p26 / p27 + plot_annotation(title = "Juvenile Count") + plot_layout(guides = 'collect')

## Priors Count vs riskViolenceDecileScore

p28 <- ggplot(data = recidTraining2, aes(x = priorsCount, y = riskViolenceDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="Priors Count",
    y="Risk Violence Score",
    fill = "Recidivated?"
  ) + 
  coord_flip()


p29 <- ggplot(data = recidTraining2, aes(x = logPriorsCount, y = riskViolenceDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="log10(Priors Count + 0.01)",
    y="Risk Violence Score",
    fill = "Recidivated?"
  ) + 
  coord_flip()

p28 / p29 + plot_annotation(title = "Priors Count") + plot_layout(guides = 'collect')

## Age vs riskViolenceDecileScore

ggplot(data = recidTraining2, aes(x = age, y = riskViolenceDecileScore, fill= recidCat)) +
  geom_boxplot() +
  labs (
    x="Age",
    y="Risk Violence Score",
    fill = "Recidivated?",
    title = "Age"
  ) + 
  coord_flip() recidTraining2, x = age, y = riskViolenceDecileScore, model = ageScoreModel, color = recidCat)

## Age Cat vs riskViolenceDecileScore
ggplot(data=recidTraining2, aes(riskViolenceDecileScore, fill = recidCat)) +
  geom_density(alpha=.4) +
  facet_wrap(~ ageCat)

## Race vs riskViolenceDecileScore

ggplot(data=recidTraining2, aes(riskViolenceDecileScore, fill = recidCat)) +
  geom_density(alpha=.4) +
  facet_wrap(~ race)

## Charge Degree vs riskViolenceDecileScore

ggplot(data=recidTraining2, aes(riskViolenceDecileScore, fill = recidCat)) +
  geom_density(alpha=.4) +
  facet_wrap(~ chargeDegree)

## Best Subset

violenceSubsetModel <- lm(riskViolenceDecileScore ~ logPriorsCount + priorsCount + age + chargeDegree + logDaysInJail + daysInJail + sex + race, data = recidTesting2)

(olsSubset <- ols_step_best_subset(violenceSubsetModel))

violenceFinalModel <- lm(riskViolenceDecileScore ~ priorsCount + age + logDaysInJail + race, data=recidTraining2)


violenceTestingPredicts <- predict.lm(scoreFinalModel, newdata=recidTesting2)

RMSE(scoreTestingPredicts,recidTesting2$riskViolenceDecileScore)


# Calculating Mystery Data

recidMystery <- read.csv("datasets/Project3Mystery100.csv")

recidMystery2 <- recidMystery %>% 
  rename(
    dayBefScreenArrest = days_b_screening_arrest,
    jailIn = c_jail_in,
    jailOut = c_jail_out,
    daysFromCompas = c_days_from_compas,
    chargeDegree = c_charge_degree,
    chargeDesc = c_charge_desc,
    riskRecidScreeningDate = RiskRecidScreeningDate,
  ) %>% 
  mutate(
    dob = as_date(dmy(dob)),
    ageCat = factor(as.factor(ageCat),levels=c("Less than 25","25 - 45","Greater than 45")),
    race = factor(as.factor(race),levels=c("white","black","hispanic","other")),
    jailIn = as.Date(dmy_hm(jailIn, tz = "EST")),
    jailOut = as.Date(dmy_hm(jailOut, tz = "EST")),
    chargeDegree = as.factor(gsub("[()]","",chargeDegree)),
    riskRecidScreeningDate = as_date(dmy(riskRecidScreeningDate)),
  ) %>% 
  select(-dob) %>% 
  filter((!is.na(jailIn) | !is.na(jailOut)))

recidMystery3 <- recidMystery2 %>% 
  mutate(
    daysInJail = as.numeric(difftime(jailOut,jailIn,unit="days")+1),
    logDaysInJail = log10(daysInJail),
    logPriorsCount = log10(priorsCount+0.1),
    juvCount = juvFelonyCount + juvMisdemeanerCount + juvOtherCount,
    logJuvCount = log10(juvCount+0.1)
  )

willRecidivate <- ifelse(predict.glm(finalModel, newdata=recidMystery3, type="response") > .5, 1, 0)
  

predictedRecidScore <- predict.lm(scoreFinalModel, newdata=recidMystery3)

predictedViolenceScore <-  predict.lm(violenceFinalModel, newdata=recidMystery3)

predictedDataframe <- cbind(recidMystery,willRecidivate,predictedRecidScore,predictedViolenceScore) %>% 
  select(personID,willRecidivate,predictedRecidScore,predictedViolenceScore)

write.csv(predictedDataframe,"datasets/PredictedMystery.csv")

