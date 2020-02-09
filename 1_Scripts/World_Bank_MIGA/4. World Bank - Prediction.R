####################################################################

## World Bank MIGA
## This  file:
##  - Reads in cleaned training and testing data sets
##  - Performs predictive analysis of in - sample (devoid of lockbox)
##
## Output:
##  - Regression outputs
##  - Scatter plot of analysis
## Author: Jake Schneider
## User: Jake Schneider
## Date Created:  06/23/2019
## Last Modified: 08/06/2019

####################################################################

## Install packages

#install.packages("plyr")
#install.packages("dplyr")
#install.packages("tidyverse", type = "binary")
#install.packages("stringr")
#install.packages("readxl")
#install.packages("data.table")
#install.packages("hablar")
#install.packages("naniar")
#install.packages("leaps")
#install.packages("plm")
#install.packages("glmnet")
#install.packages("DMwR2")
#install.packages("lmtest")
#install.packages("stargazer")
#install.packages("AER")
#install.packages("Hmisc")
#install.packages("caret")
#install.packages("devtools")
#install.packages("randomForest")
#install.packages("class")
#install.packages("Metrics")
#install.packages("devtools")
#devtools::install_github('araastat/reprtree')
#install.packages("kableExtra")
#install.packages("ggthemes")
#install.packages("sf")
#install.packages("raster")
#install.packages("spData")
#install.packages("spDataLarge")
#install.packages("sf")      # for static and interactive maps
#install.packages("leaflet") # for interactive maps
#install.packages("mapview") # for interactive maps
#install.packages("shiny")   # for web applications
#install.packages("rgeos", type = "source")   
#install.packages("rgdal", type="source")
#install.packages("magrittr")

## Load relevant libraries

library(plyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(readxl)
library(data.table)
library(reshape2)
library(hablar)
library(naniar)
library(leaps)
library(plm)
library(glmnet)
library(DMwR2)
library(lmtest)
library(stargazer)
library(AER)
library(Hmisc)
library(caret)
library(devtools)
library(randomForest)
library(class)
library(Metrics)
library(devtools)
library(reprtree)
library(kableExtra)
library(ggthemes)
library(sf)
library(raster)
library(spData)
#library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(shiny)   # for web applications
library(magrittr)

## Setting a working directory 

getwd()

setwd("/Users/jschneids13/Desktop/World Bank - MIGA STT/Projects/Investment Analysis/MIGA-Private-/Full Research - By Country")

## Read in Dataset

load("R Data Sets/FDI Inputs/FDI_Input_Panel.Rdata")
load("R Data Sets/FDI Inputs/FDI_Aggregate_Country.RData")

## Create Training, Testing and Lockbox

# Create Prediction dataset for 1969 - 2016 averages

fdi_input_panel$Year_Numeric <- as.numeric(fdi_input_panel$Year)
prediction_df <- aggregate(fdi_input_panel
                           [,c(8:100, 124:125)], 
                           list(fdi_input_panel$Country, 
                                fdi_input_panel$Year_Numeric < 49),
                           mean, 
                           na.rm = TRUE)
prediction_df <- prediction_df[prediction_df$Group.2==TRUE,]
prediction_df$Group.2 <- NULL
prediction_df[is.na(prediction_df)] <- NA

# Create Lockbox dataset for values for 2017

lockbox <- fdi_input_panel[fdi_input_panel$Year==2017,]

## Drop Missing Values for FDI Net Inflows
# Prediction: 197 countries
# Lockbox: 185

# Prediction DF

complete_rows_prediction_df <- sum(
  complete.cases(
    prediction_df$Foreign.direct.investment..net.inflows....of.GDP.))
missing_rows_prediction_df <- sum(
  !complete.cases(
    prediction_df$Foreign.direct.investment..net.inflows....of.GDP.))

prediction_df <- prediction_df[
  !is.na(
    prediction_df$Foreign.direct.investment..net.inflows....of.GDP.),]

# Lockbox

complete_rows_lockbox <- sum(
  complete.cases(
    lockbox$Foreign.direct.investment..net.inflows....of.GDP.))
missing_rows_lockbox <- sum(
  !complete.cases(
    lockbox$Foreign.direct.investment..net.inflows....of.GDP.))

lockbox <- lockbox[
  !is.na(
    lockbox$Foreign.direct.investment..net.inflows....of.GDP.),]

## Create Testing and Training Data Set
# From prediction_df
# Creates: test_data and train_data

set.seed(13)
test_obs              <- round(0.2 * nrow(prediction_df))
train_obs             <- nrow(prediction_df) - test_obs
test_train_vec        <- c(rep("test", test_obs),
                           rep("train", train_obs))
test_train_vec        <- sample(test_train_vec, 
                                nrow(prediction_df), 
                                replace = FALSE)
test_data             <- prediction_df[which
                                       (test_train_vec == 
                                                 "test"),]
train_data            <- prediction_df[which
                                       (test_train_vec == 
                                                 "train"),]

## Root Mean Square Error (RSME) Formula
# https://stackoverflow.com/questions/26237688/rmse-root-mean-square-deviation-calculation-in-r

RMSE <- function(predicted, true) { 
  sqrt(mean((predicted - true)^2, na.rm=TRUE)) 
  }

## Adjusted Root Mean Square Error (ARSME) Formula

ARMSE <- function(root_error, values) {
  root_error / values
}

## Run Algorithms to find best forecasting model
# Models to run: (* indicates first priority)
# 1a. Linear regression (using model from Stata) *
# 1b. Linear regression (using model from regsubsets) *
# 1c. Linear regression (using model from inference) *
# 2. Principal Components Regression (PCR)
# 3. Partial Least Squares (PLS) 
# 4. Smoothing Splines
# 5. Regression Trees
# 6. Bagging
# 7. Random Forest *
# 8. Boosting
# 9. Neural Networs (!!)

# 1a. Linear regression (using model from Stata) *:

regfit_full_all <- 
  lm(Foreign.direct.investment..net.inflows....of.GDP. 
     ~ sp 
     + mdy 
     + fitch
     + polity2
     + cpia_composite
     + wgi_composite
     + GDP.per.capita..constant.2010.US..
     + GDP.growth..annual..., 
     data = train_data)

summary(regfit_full_all) # collinear: r^2 1.0
nobs(regfit_full_all) # 8 

yhat_regfit_full_all <- predict(regfit_full_all, newdata=test_data)
# This model does not work for lack of data and linear dependencies

# 1b. Linear regression (using model from regsubsets) *

lm_regfit6 <- lm(
  Foreign.direct.investment..net.inflows....of.GDP. ~
    Central.government.debt..total....of.GDP. +
    sp +
    GDP.per.capita..constant.2010.US.. +
    csdr_avg +
    polity2 +
    Population.in.largest.city,
  data = train_data)

summary(lm_regfit6) # r^2 46.08, adjusted r^2 22.97
nobs(lm_regfit6) # 21 
# This model is not good: too few observations to be useful

yhat_lm_regfit6 <- predict(lm_regfit6, newdata=test_data)

df_lm_regfit6 <- data.frame(order_id = seq(nrow(test_data)),
      country = test_data$Group.1,
      true = 
        as.numeric(
          test_data$Foreign.direct.investment..net.inflows....of.GDP.),
      predicted = as.numeric(yhat_lm_regfit6))

lm_regfit6_RMSE <- RMSE(
  yhat_lm_regfit6, 
  test_data$Foreign.direct.investment..net.inflows....of.GDP.) 
#RMSE = 1.42

lm_regfit6_MV <- sum(is.na(yhat_lm_regfit6))
#Missing Values = 33 (out of 39)
#Present Values = 6

lm_regfit6_ARMSE <- ARMSE(lm_regfit6_RMSE, 6)
#ARMSE = 0.24

# 1c. Linear regression (using model from inference) *

regfit_optimal2_lm <- lm(
  Foreign.direct.investment..net.inflows....of.GDP.
  ~ Central.government.debt..total....of.GDP.
  + csdr_avg
  + GDP.per.capita..constant.2010.US..
  + GDP.growth..annual...
  + Exports.of.Commodities.to.the.World.....of.GDP.
  + Current.account.balance....of.GDP.
  + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
  + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate,
  data = train_data) 

summary(regfit_optimal2_lm) #r^2 18.35% and adjstr^2 0.016%
nobs(regfit_optimal2_lm) #48
# Still too few observation

regfit_optimal2_lm.1 <- lm(
  Foreign.direct.investment..net.inflows....of.GDP.
  ~ csdr_avg
  + GDP.per.capita..constant.2010.US..
  + GDP.growth..annual...
  + Exports.of.Commodities.to.the.World.....of.GDP.
  + Current.account.balance....of.GDP.
  + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
  + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate,
  data = train_data) 

summary(regfit_optimal2_lm.1) #r^2 20.02% and adjstr^2 12.45%
nobs(regfit_optimal2_lm.1) #82

regfit_optimal2_lm.2 <- lm(
  Foreign.direct.investment..net.inflows....of.GDP.
  ~ GDP.per.capita..constant.2010.US..
  + GDP.growth..annual...
  + Exports.of.Commodities.to.the.World.....of.GDP.
  + Current.account.balance....of.GDP.
  + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
  + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate,
  data = train_data) 

summary(regfit_optimal2_lm.2) #r^2 15.9% and adjstr^2 10.54%
nobs(regfit_optimal2_lm.2) #101

yhat_regfit_optimal2_lm.2 <- predict(regfit_optimal2_lm.2, 
                                     newdata = test_data)

regfit_optimal2_lm.2_RMSE <- RMSE(
  yhat_regfit_optimal2_lm.2, 
  test_data$Foreign.direct.investment..net.inflows....of.GDP.) 
#RMSE = 9.397997

regfit_optimal2_lm.2_MV <- sum(is.na(yhat_regfit_optimal2_lm.2))
#Missing Values = 16 (out of 39)
#Present Values = 23 

regfit_optimal2_lm.2_ARMSE <- ARMSE(regfit_optimal2_lm.2_RMSE, 23)
# ARMSE = 0.4086086

regfit_optimal2_lm.3 <- lm(
  Foreign.direct.investment..net.inflows....of.GDP.
  ~ GDP.per.capita..constant.2010.US..
  + GDP.growth..annual...
  + Exports.of.Commodities.to.the.World.....of.GDP.
  + Current.account.balance....of.GDP.
  + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
  + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate
  + Population.in.largest.city,
  data = train_data) 

summary(regfit_optimal2_lm.3) #r^2 35.13% and adjstr^2 28.56%
nobs(regfit_optimal2_lm.3) #88

regfit_optimal2_lm.4 <- lm(
  Foreign.direct.investment..net.inflows....of.GDP.
  ~ GDP.per.capita..constant.2010.US..
  + GDP.growth..annual...
  + Exports.of.Commodities.to.the.World.....of.GDP.
  + Current.account.balance....of.GDP.
  + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
  + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate
  + Population.in.largest.city
  + Population..total,
  data = train_data) 

summary(regfit_optimal2_lm.4) #r^2 35.13% and adjstr^2 28.56%
nobs(regfit_optimal2_lm.3) #88
# Exact same

regfit_optimal2_lm.5 <- lm(
  Foreign.direct.investment..net.inflows....of.GDP.
  ~ GDP.per.capita..constant.2010.US..
  + GDP.growth..annual...
  + Exports.of.Commodities.to.the.World.....of.GDP.
  + Current.account.balance....of.GDP.
  + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
  + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate
  + Population.in.largest.city
  + Population.density..people.per.sq..km.of.land.area.,
  data = train_data) 

summary(regfit_optimal2_lm.5) #r^2 35.8% and adjstr^2 29.3%
nobs(regfit_optimal2_lm.5) #88
# Best lm prediction model

yhat_regfit_optimal2_lm.5 <- predict(regfit_optimal2_lm.5, 
                                     newdata=test_data)

df_regfit_optimal2_lm.5 <- data.frame(order_id = seq(nrow(test_data)),
                            country = test_data$Group.1,
                            true = 
                              as.numeric(
                                test_data$Foreign.direct.investment..net.inflows....of.GDP.),
                            predicted = as.numeric(
                              yhat_regfit_optimal2_lm.5))

regfit_optimal2_lm.5_RMSE <- RMSE(
  yhat_regfit_optimal2_lm.5, 
  test_data$Foreign.direct.investment..net.inflows....of.GDP.) 
#RMSE = 2.852889

regfit_optimal2_lm.5_MV <- sum(is.na(yhat_regfit_optimal2_lm.5))
#Missing Values = 21 (out of 39)
#Present Values = 18 

regfit_optimal2_lm.5_ARMSE <- ARMSE(regfit_optimal2_lm.5_RMSE, 18)
# ARMSE = 0.16

# Factor engineering

train_data$gdp.per.capta2 <- 
  (train_data$GDP.per.capita..constant.2010.US.. ^ 2)
train_data$gdp.per.capta3 <- 
  (train_data$GDP.per.capita..constant.2010.US.. ^ 3)

train_data$gdp.growth2 <- 
  (train_data$GDP.growth..annual... ^ 2)
train_data$gdp.growth3 <- 
  (train_data$GDP.growth..annual... ^ 3)

regfit_optimal2_lm.6 <- lm(
  Foreign.direct.investment..net.inflows....of.GDP.
  ~ GDP.per.capita..constant.2010.US..
  + gdp.per.capta2
  + gdp.per.capta3
  + GDP.growth..annual...
  + gdp.growth2
  + gdp.growth3
  + Exports.of.Commodities.to.the.World.....of.GDP.
  + Current.account.balance....of.GDP.
  + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
  + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate
  + Population.in.largest.city
  + Population.density..people.per.sq..km.of.land.area.,
  data = train_data) 

summary(regfit_optimal2_lm.6) #r^2 36.81% and adjstr^2 26.7%
nobs(regfit_optimal2_lm.6) #88
# Slight improves r^2 but much worse r^2

# Create Data Frame to Collect the Models

values <- data.frame("Model" = c("Linear 1", 
                                 "Linear 2",
                                 "Linear 3"),
                     "RMSE" = c(regfit_optimal2_lm.2_RMSE,
                                lm_regfit6_RMSE, 
                                regfit_optimal2_lm.5_RMSE),
                     "Number of Observations" = c(23, 6, 18),
                     "ARMSE" = c(regfit_optimal2_lm.2_ARMSE,
                                 lm_regfit6_ARMSE,
                                 regfit_optimal2_lm.5_ARMSE))

# 7. Random Forest *

# Random Forest 1

rf1 <- randomForest(Foreign.direct.investment..net.inflows....of.GDP. ~
                      . - Group.1, 
                    ntree = 100,
                    na.action = na.omit,
                    data = train_data)

rf1$predicted
#only produces one response

# Random Forest 2

rf2 <- randomForest(Foreign.direct.investment..net.inflows....of.GDP.
                    ~ Central.government.debt..total....of.GDP.
                    + sp 
                    + mdy 
                    + fitch
                    + csdr_avg
                    + polity2
                    + cpia_composite
                    + wgi_composite
                    + GDP.per.capita..constant.2010.US..
                    + GDP.growth..annual...
                    + Reserves.and.related.items..BoP..current.US..
                    + Current.account.balance....of.GDP.
                    + Exports.of.Commodities.to.the.World.....of.GDP.
                    + Population.density..people.per.sq..km.of.land.area.
                    + Population.growth..annual...
                    + Population.in.largest.city
                    + Population..total,
                    ntree = 100,
                    na.action = na.omit,
                    data = train_data)

rf2$predicted
#only produces four responses

# Random Forest 3

rf3 <- randomForest(Foreign.direct.investment..net.inflows....of.GDP.
                    ~ GDP.per.capita..constant.2010.US..
                    + GDP.growth..annual...
                    + Exports.of.Commodities.to.the.World.....of.GDP.
                    + Current.account.balance....of.GDP.
                    + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
                    + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate,
                    ntree = 100,
                    na.action = na.omit,
                    data = train_data)

rf3$predicted

yhat_rf3 <- predict(rf3, newdata=test_data)

jpeg("Plots/Prediction/Maximization/Random Forest 3.jpg", 
     width = 500, height = 500)
plot(rf3)
dev.off()

jpeg(
  "Plots/Prediction/Tree Diagrams/Random Forest 3 Tree Diagram.jpg", 
  width = 1500, height = 1500)
reprtree:::plot.getTree(rf3)
dev.off()

print(rf3)
# % Var explained: -4.1

df_rf3 <- data.frame(order_id = seq(nrow(test_data)),
                     country = test_data$Group.1,
                     true = 
                       as.numeric(
                         test_data$Foreign.direct.investment..net.inflows....of.GDP.),
                     predicted = as.numeric(yhat_rf3))

rf3_RMSE <- RMSE(
  yhat_rf3, 
  test_data$Foreign.direct.investment..net.inflows....of.GDP.) 
#RMSE = 9.093509

rf3_MV <- sum(is.na(yhat_rf3))
#Missing Values = 16 (out of 39)
#Present Values = 23

rf3_ARMSE <- ARMSE(rf3_RMSE, 23)
#ARMSE =  0.3953699

# Random Forest 4

rf4 <- randomForest(Foreign.direct.investment..net.inflows....of.GDP.
                    ~ GDP.per.capita..constant.2010.US..
                    + GDP.growth..annual...
                    + Exports.of.Commodities.to.the.World.....of.GDP.
                    + Current.account.balance....of.GDP.
                    + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
                    + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate,
                    ntree = 500,
                    na.action = na.omit,
                    data = train_data)

rf4$predicted

yhat_rf4 <- predict(rf4, newdata=test_data)

jpeg("Plots/Prediction/Maximization/Random Forest 4.jpg", 
     width = 500, height = 500)
plot(rf4)
dev.off()

jpeg(
  "Plots/Prediction/Tree Diagrams/Random Forest 4 Tree Diagram.jpg", 
     width = 1500, height = 1500)
reprtree:::plot.getTree(rf4)
dev.off()

print(rf4)
# % Var explained: 6.29
# Mean of squared residuals: 21.41168
# randomforest4 is best

df_rf4 <- data.frame(order_id = seq(nrow(test_data)),
                            country = test_data$Group.1,
                            true = 
                              as.numeric(
                                test_data$Foreign.direct.investment..net.inflows....of.GDP.),
                            predicted = as.numeric(yhat_rf4))

rf4_RMSE <- RMSE(
  yhat_rf4, 
  test_data$Foreign.direct.investment..net.inflows....of.GDP.) 
#RMSE = 9.062386

rf4_MV <- sum(is.na(yhat_rf4))
#Missing Values = 16 (out of 39)
#Present Values = 23

rf4_ARMSE <- ARMSE(rf4_RMSE, 23)
#ARMSE =  0.3940168

# Random Forest 4.1

rf4.1 <- randomForest(Foreign.direct.investment..net.inflows....of.GDP.
                    ~ GDP.per.capita..constant.2010.US..
                    + GDP.growth..annual...
                    + Exports.of.Commodities.to.the.World.....of.GDP.
                    + Current.account.balance....of.GDP.
                    + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
                    + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate,
                    ntree = 500,
                    mtry = sqrt(6),
                    na.action = na.omit,
                    data = train_data)

rf4.1$predicted

yhat_rf4.1 <- predict(rf4.1, newdata=test_data)

jpeg("Plots/Prediction/Maximization/Random Forest 4.1.jpg", 
     width = 500, height = 500)
plot(rf4.1)
dev.off()

jpeg(
  "Plots/Prediction/Tree Diagrams/Random Forest 4.1 Tree Diagram.jpg", 
  width = 1500, height = 1500)
reprtree:::plot.getTree(rf4.1)
dev.off()

print(rf4.1)
# % Var explained: 3.40
# Mean of squared residuals: 22.05247
# This model is worse

df_rf4.1 <- data.frame(order_id = seq(nrow(test_data)),
                     country = test_data$Group.1,
                     true = 
                       as.numeric(
                         test_data$Foreign.direct.investment..net.inflows....of.GDP.),
                     predicted = as.numeric(yhat_rf4.1))

rf4.1_RMSE <- RMSE(
  yhat_rf4.1, 
  test_data$Foreign.direct.investment..net.inflows....of.GDP.) 
#RMSE = 9.052339

rf4.1_MV <- sum(is.na(yhat_rf4.1))
#Missing Values = 16 (out of 39)
#Present Values = 23

rf4.1_ARMSE <- ARMSE(rf4.1_RMSE, 23)
#ARMSE =  0.39358

# Random Forest 5

rf5 <- randomForest(Foreign.direct.investment..net.inflows....of.GDP.
                    ~ GDP.per.capita..constant.2010.US..
                    + GDP.growth..annual...
                    + Exports.of.Commodities.to.the.World.....of.GDP.
                    + Current.account.balance....of.GDP.
                    + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
                    + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate,
                    ntree = 1000,
                    na.action = na.omit,
                    data = train_data)

rf5$predicted

yhat_rf5 <- predict(rf5, newdata=test_data)

jpeg("Plots/Prediction/Maximization/Random Forest 5.jpg", 
     width = 500, height = 500)
plot(rf5)
dev.off()

print(rf5)
# % Var explained: 6.15
#Mean of squared residuals: 21.44358

## Add rf3 and rf4 to Values table

rf_values <- data.frame("Model" = c("Random Forest 1", 
                                    "Random Forest 2"),
                        "RMSE" = c(rf3_RMSE, 
                                   rf4_RMSE),
                        "Number of Observations" = c(23, 23),
                        "ARMSE" = c(rf3_ARMSE,
                                    rf4_ARMSE))

values <- rbind(values, rf_values)


# Random Forest 6

rf6 <- randomForest(Foreign.direct.investment..net.inflows....of.GDP.
                      ~ GDP.per.capita..constant.2010.US..
                      + GDP.growth..annual...
                      + Exports.of.Commodities.to.the.World.....of.GDP.
                      + Current.account.balance....of.GDP.
                      + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
                      + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate
                      + Population.density..people.per.sq..km.of.land.area.
                      + Population.in.the.largest.city....of.urban.population.
                      + Population..total,
                      ntree = 500,
                      mtry = sqrt(8),
                      na.action = na.omit,
                      data = train_data)

rf6$predicted

yhat_rf6 <- predict(rf6, newdata=test_data)

jpeg("Plots/Prediction/Maximization/Random Forest 6.jpg", 
     width = 500, height = 500)
plot(rf6)
dev.off()

jpeg(
  "Plots/Prediction/Tree Diagrams/Random Forest 6 Tree Diagram.jpg", 
  width = 1500, height = 1500)
reprtree:::plot.getTree(rf6)
dev.off()

print(rf6)
# % Var explained: 18.47
# Mean of squared residuals: 7.070419
# This model is best by far

df_rf6 <- data.frame(order_id = seq(nrow(test_data)),
                       country = test_data$Group.1,
                       true = 
                         as.numeric(
                           test_data$Foreign.direct.investment..net.inflows....of.GDP.),
                       predicted = as.numeric(yhat_rf6))

rf6_RMSE <- RMSE(
  yhat_rf6, 
  test_data$Foreign.direct.investment..net.inflows....of.GDP.) 
#RMSE = 2.424073

rf6_MV <- sum(is.na(yhat_rf6))
#Missing Values = 21 (out of 39)
#Present Values = 18

rf6_ARMSE <- ARMSE(rf6_RMSE, 18)
#ARMSE =  0.1346707

rf_values2 <- data.frame("Model" = c("Random Forest 3"),
                        "RMSE" = c(rf6_RMSE),
                        "Number of Observations" = c(18),
                        "ARMSE" = c(rf6_ARMSE))

values <- rbind(values, rf_values2)
# I think 13 pp is acceptable: model selection complete

## Output Values Table

kable(values, 
      format = "html",
      caption = "Prediction Model Selection Table",
      col.names = c("Model", "RMSE",
                    "Number of Observations",
                    "Adjusted RMSE"),
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped"), 
                position = "center",
                font_size = 12,
                full_width = FALSE) %>%
#  column_spec(4:6, width = "1.5cm") %>%
  save_kable(
    file = "Plots/Prediction/Tables/Model Selection.html",
    self_contained = T)


# Random Forest 7

rf7 <- randomForest(Foreign.direct.investment..net.inflows....of.GDP.
                    ~ GDP.per.capita..constant.2010.US..
                    + GDP.growth..annual...
                    + Exports.of.Commodities.to.the.World.....of.GDP.
                    + Current.account.balance....of.GDP.
                    + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
                    + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate
                    + Population.density..people.per.sq..km.of.land.area.
                    + Population.in.the.largest.city....of.urban.population.
                    + Population..total
                    + csdr_avg,
                    ntree = 500,
                    mtry = sqrt(9),
                    na.action = na.omit,
                    data = train_data)

rf7$predicted

yhat_rf7 <- predict(rf7, newdata=test_data)

jpeg("Plots/Prediction/Maximization/Random Forest 7.jpg", 
     width = 500, height = 500)
plot(rf7)
dev.off()

jpeg(
  "Plots/Prediction/Tree Diagrams/Random Forest 7 Tree Diagram.jpg", 
  width = 1500, height = 1500)
reprtree:::plot.getTree(rf7)
dev.off()

print(rf7)
# % Var explained: 16.13
# Mean of squared residuals: 7.8981
# This model is best by far

df_rf7 <- data.frame(order_id = seq(nrow(test_data)),
                     country = test_data$Group.1,
                     true = 
                       as.numeric(
                         test_data$Foreign.direct.investment..net.inflows....of.GDP.),
                     predicted = as.numeric(yhat_rf7))

rf7_RMSE <- RMSE(
  yhat_rf7, 
  test_data$Foreign.direct.investment..net.inflows....of.GDP.) 
#RMSE = 2.71689

rf7_MV <- sum(is.na(yhat_rf7))
#Missing Values = 24 (out of 39)
#Present Values = 15

rf7_ARMSE <- ARMSE(rf7_RMSE, 15)
#ARMSE =  0.181126

## Create Variable Importance Graphic

# Random Forest 2

jpeg(
  "Plots/Prediction/Variable Importance/Variable Importance RF4.jpg",
     width = 500, height = 500)
varImpPlot(rf4,
           type = 2,
           main = "Variable Importance: Random Forest 2",
           labels = rev(c("Exports of Commodities (% of GDP)",
                      "Port Infrastructure Quality (Scale 1-7)",
                      "Exchange Rate Price Level (PPP)",
                      "GDP Growth Rate (annual %)",
                      "Current Account Balance (% of GDP)",
                      "GDP per capita (2010 US$)")
                      )
           )
dev.off()

# Random Forest 3

jpeg(
  "Plots/Prediction/Variable Importance/Variable Importance RF6.jpg",
  width = 500, height = 500)
varImpPlot(rf6,
           type = 2,
           main = "Variable Importance: Random Forest 3",
           labels = rev(c("Exports of Commodities (% of GDP)",
                      "Population, total",
                      "Exchange Rate Price Level (PPP)",
                      "Current Account Balance (% of GDP)",
                      "Port Infrastructure Quality (Scale 1-7)",
                      "Population Density (per sq km)",
                      "Population in Largest City",
                      "GDP Growth Rate (annual %)",
                      "GDP per capita (2010 US$)")
                      )
)
dev.off()

# Predict Values for Lockbox 2017 data

lockbox_rf4 <- predict(rf4, newdata=lockbox)
lockbox_rf4_df <- data.frame(order_id = seq(nrow(lockbox)),
                       country = lockbox$Country,
                       true = 
                         as.numeric(
                           lockbox$Foreign.direct.investment..net.inflows....of.GDP.),
                       predicted = as.numeric(lockbox_rf4))

lockbox_rf4_MV <- sum(is.na(lockbox_rf4))
# Missing: 90
# Present: 95
# Will use this model because of more data availability

lockbox_rf6 <- predict(rf6, newdata=lockbox)
lockbox_rf6_df <- data.frame(order_id = seq(nrow(lockbox)),
                             country = lockbox$Country,
                             true = 
                               as.numeric(
                                 lockbox$Foreign.direct.investment..net.inflows....of.GDP.),
                             predicted = as.numeric(lockbox_rf4))

lockbox_rf6_MV <- sum(is.na(lockbox_rf6))
# Missing: 101
# Present: 84

# Add Predicted Values for RF4 and RF6 to Lockbox Dataframe

lockbox$Predicted.RF4 <- lockbox_rf4
lockbox$Predicted.RF6 <- lockbox_rf6
lockbox$Deviation.RF4 <- lockbox$Predicted.RF4 - 
  lockbox$Foreign.direct.investment..net.inflows....of.GDP.
lockbox$Deviation.RF6 <- lockbox$Predicted.RF6 - 
  lockbox$Foreign.direct.investment..net.inflows....of.GDP.

## Identify the Deviation of the Top 10, Bottom 10 and Closest in Value

# Top 10

top10_df <- 
  lockbox[, c(1, 3:4, 13, 180, 182)] %>%
  arrange(desc(Deviation.RF4)) %>%
  slice(1:10)

kable(top10_df, 
      format = "html",
      caption = "Top 10 Countries for Under-Investment 
      in FDI Capital Flows, 2017",
      col.names = c("Country", "Region", "Income Group",
                    "Actual FDI",
                    "Predicted FDI",
                    "Difference"),
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped"), 
                position = "center",
                font_size = 12,
                full_width = FALSE) %>%
  column_spec(4:6, width = "1.5cm") %>%
  save_kable(
    file = "Plots/Prediction/Tables/Top 10.html",
    self_contained = T)

# Top 10 & Low-Income

top10_lowincome_df <- 
  lockbox[
    lockbox$Income.Group == "Low income", 
    c(1, 3:4, 13, 180, 182) ] %>%
  arrange(desc(Deviation.RF4)) %>%
  slice(1:10)

#top10_lowincome_df_test <- 
#  subset(lockbox, Income.Group == "Low income",
#         select = c(Country, 
#                    Region,
#                    Income.Group,
#                    Foreign.direct.investment..net.inflows....of.GDP.,
#                    Predicted.RF4,
#                    Deviation.RF4)) %>%
#         arrange(desc(Deviation.RF4)) %>%
#         slice(1:10)

kable(top10_lowincome_df, 
      format = "html",
      caption = "Top 10 Low Income Countries for Under-Investment 
      in FDI Capital Flows, 2017",
      col.names = c("Country", "Region", "Income Group",
                    "Actual FDI",
                    "Predicted FDI",
                    "Difference"),
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped"), 
                position = "center",
                font_size = 12,
                full_width = FALSE) %>%
  column_spec(4:6, width = "1.5cm") %>%
  save_kable(
    file = "Plots/Prediction/Tables/Top 10: Low Income.html",
    self_contained = T)

# Top 10 & Lower Middle Income

top10_lowermiddleincome_df <- 
  lockbox[
    lockbox$Income.Group == "Lower middle income", 
    c(1, 3:4, 13, 180, 182) ] %>%
  arrange(desc(Deviation.RF4)) %>%
  slice(1:10)

kable(top10_lowermiddleincome_df, 
      format = "html",
      caption = "Top 10 Lower Middle Income Countries for Under-Investment in FDI Capital Flows, 2017",
      col.names = c("Country", "Region", "Income Group",
                    "Actual FDI",
                    "Predicted FDI",
                    "Difference"),
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped"), 
                position = "center",
                font_size = 12,
                full_width = FALSE) %>%
  column_spec(4:6, width = "1.5cm") %>%
  save_kable(
    file = "Plots/Prediction/Tables/Top 10: Lower Middle Income.html",
    self_contained = T)


# Top 10 & Low Income & Lower Middle Income

top10_bothbottomincome_df <- 
  lockbox[
    c(lockbox$Income.Group == "Low income" |
      lockbox$Income.Group == "Lower middle income"), 
    c(1, 3:4, 13, 180, 182) ] %>%
  arrange(desc(Deviation.RF4)) %>%
  slice(1:10)

top10_bothbottomincome_df$MIGA.Percentage <-
  c(0.04, 0.65, 1.16, "No Projects" , 1.26, 5.22, 
    2.60, "No Projects", "No Projects", "No Projects")

top10_bothbottomincome_df$MIGA.Rank <-
  c(80, 38, 28, "Unranked", 25, 5, 
    13, "Unranked", "Unranked", "Unranked")

kable(top10_bothbottomincome_df, 
      format = "html",
      caption = "Top 10 Low & Lower Middle Income Countries 
      for Under-Investment in FDI Capital Flows, 2017",
      col.names = c("Country", "Region", "Income Group",
                    "Actual FDI",
                    "Predicted FDI",
                    "Difference",
                    "MIGA Exposure (%)",
                    "MIGA Rank"),
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped"), 
                position = "center",
                font_size = 12,
                full_width = FALSE) %>%
  column_spec(4:8, width = "1.5cm") %>%
  footnote(general = "MIGA Exposure and Rank are based on the 10 year average of MIGA PRI offerings between 2008-2017.") %>%
  save_kable(
    file = "Plots/Prediction/Tables/Top 10: Low & Lower Middle Income.html",
    self_contained = T)

# Bottom 10

bottom10_df <- 
  lockbox[, c(1, 3:4, 13, 180, 182)] %>%
  na.omit() %>%
  arrange(Deviation.RF4) %>%
  slice(1:10)

kable(bottom10_df, 
      format = "html",
      caption = "Bottom 10 Countries for Under-Investment in 
      FDI Capital Flows, 2017",
      col.names = c("Country", "Region", "Income Group",
                    "Actual FDI",
                    "Predicted FDI",
                    "Difference"),
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped"), 
                position = "center",
                font_size = 12,
                full_width = FALSE) %>%
  column_spec(4:6, width = "1.5cm") %>%
  save_kable(
    file = "Plots/Prediction/Tables/Bottom 10.html",
    self_contained = T)


# Bottom 10 & Low Income & Lower Middle Income

bottom10_bothbottomincome_df <- 
  lockbox[
    c(lockbox$Income.Group == "Low income" |
        lockbox$Income.Group == "Lower middle income"), 
    c(1, 3:4, 13, 180, 182) ] %>%
  na.omit() %>%
  arrange(Deviation.RF4) %>%
  slice(1:10)

kable(bottom10_bothbottomincome_df, 
      format = "html",
      caption = "Bottom 10 Low & Lower Middle Income Countries for
      Under-Investment in FDI Capital Flows, 2017",
      col.names = c("Country", "Region", "Income Group",
                    "Actual FDI",
                    "Predicted FDI",
                    "Difference"),
      digits = 2) %>%
  kable_styling(bootstrap_options = c("striped"), 
                position = "center",
                font_size = 12,
                full_width = FALSE) %>%
  column_spec(4:6, width = "1.5cm") %>%
  save_kable(
    file = "Plots/Prediction/Tables/Bottom 10: Low & Lower Middle Income.html",
    self_contained = T)

## Create scatter plots

# All Countries (95)

# reg_rf4 for all countries
reg_rf4 <- lm(Predicted.RF4 ~ 
                Foreign.direct.investment..net.inflows....of.GDP.,
              data = lockbox)

summary(reg_rf4)
nobs(reg_rf4)

# reg_rf4 for low & lower middle income
reg_rf4.1 <- lm(Predicted.RF4 ~ 
                Foreign.direct.investment..net.inflows....of.GDP.,
              data = subset(lockbox, 
                            Income.Group %in% 
                              c("Low income",
                                "Lower middle income")))

summary(reg_rf4.1)
nobs(reg_rf4.1)

# scatter plot: predicted

jpeg(
  "Plots/Prediction/Deviation Scatter Plots/Scatter Plot - Predicted - All.jpg", 
  width = 500, height = 500)
scatter_predicted_all <- 
  ggplot(lockbox,
       aes(x = Foreign.direct.investment..net.inflows....of.GDP.,
           y = Predicted.RF4,
           label = Country)) +
  geom_point(shape = 20) +
  geom_text(aes(label = ifelse(Deviation.RF4>4 & Deviation.RF4<30, 
                               as.character(Country), " "), 
                hjust = 1.0, 
                vjust = -0.2)) +
  geom_text(aes(label = ifelse(Deviation.RF4>30, 
                               as.character(Country), " "), 
                hjust = 0, 
                vjust = -0.2)) +
#  geom_smooth(method = lm,
#              se = FALSE) +
  geom_abline(slope = 1, 
              size = 1.5,
              color = "blue", 
              linetype = "dashed") +
  scale_y_continuous(
    "FDI, Predicted 2017", limits = c(-10,20)) +
  scale_x_continuous("FDI, Actual 2017") +
  ggtitle(
    "FDI: Over- and Under-Invested Countries for the World, 2017") +
  theme_tufte() +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5))
scatter_predicted_all +
  annotate("text", x = -11.5, y = 16, 
         label = "Under-Invested",
         color = "darkgreen",
         size = 5) +
  annotate("text", x = 35, y = 16, 
           label = "Over-Invested",
           color = "darkred",
           size = 5)
dev.off()

# scatter plot: predicted & low and lower middle income

jpeg(
  "Plots/Prediction/Deviation Scatter Plots/Scatter Plot - Predicted - Low & Lower Middle Income.jpg", 
  width = 500, height = 500)
scatter_predicted_bottomincome <- 
  ggplot(subset(lockbox, Income.Group %in% c("Low income",
                                           "Lower middle income")),
       aes(x = Foreign.direct.investment..net.inflows....of.GDP.,
           y = Predicted.RF4,
           label = Country)) +
  geom_point(shape = 20) +
  geom_text(aes(label = ifelse(
    Deviation.RF4>1.05 & Deviation.RF4<1.1, 
    as.character(Country), " "), 
    hjust = 1.1, 
    vjust = 2.2)) +
  geom_text(aes(label = ifelse(
    Deviation.RF4>1.1 & Deviation.RF4<2.1, 
    as.character(Country), " "), 
                hjust = 1.1, 
                vjust = 0.6)) +
  geom_text(aes(label = ifelse(
    Deviation.RF4>2.1, 
    as.character(Country), " "), 
    hjust = 1.1, 
    vjust = -0.2)) +
  #  geom_smooth(method = lm,
  #              se = FALSE) +
  geom_abline(slope = 1, 
              size = 1.5,
              color = "blue", 
              linetype = "dashed") +
  scale_y_continuous(
    "FDI, Predicted 2017", limits = c(0,10)) +
  scale_x_continuous(
    "FDI, Actual 2017", limits = c(-5,15)) +
  ggtitle(
    "FDI: Over- and Under-Invested Countries for \nLow & Lower-Middle Income Countries, 2017") +
  theme_tufte() +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5))
scatter_predicted_bottomincome +
  annotate("text", x = 1, y = 7, 
           label = "Under-Invested",
           color = "darkgreen",
           size = 5) +
  annotate("text", x = 11, y = 7, 
           label = "Over-Invested",
           color = "darkred",
           size = 5)
dev.off()


# scatter plot: deviation

jpeg(
  "Plots/Prediction/Deviation Scatter Plots/Scatter Plot - Deviation - All.jpg", 
     width = 500, height = 500)
ggplot(lockbox,
       aes(x = Foreign.direct.investment..net.inflows....of.GDP.,
           y = Deviation.RF4)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm,
              se = FALSE) +
  scale_y_continuous(
    "FDI, Deviation Between Predicted and Actual 2017") +
  scale_x_continuous("FDI, Actual 2017") +
  ggtitle(
    "FDI: Over- and Under-Invested Countries for the World, 2017") +
  theme_tufte() +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5))
dev.off()

# scatter plot: deviation by income group

jpeg(
  "Plots/Prediction/Deviation Scatter Plots/Scatter Plot - Deviation - Low & Lower Middle Income.jpg", 
  width = 500, height = 500)
ggplot(subset(lockbox, Income.Group %in% c("Low income",
                                           "Lower middle income")),
       aes(x = Foreign.direct.investment..net.inflows....of.GDP.,
           y = Deviation.RF4,
           label = Country)) +
  geom_point(shape = 1) +
  geom_text(aes(label = ifelse(Deviation.RF4>1, as.character(Country), " "), hjust = 0, vjust = 0)) +
  geom_smooth(method = lm,
              se = FALSE) +
  scale_y_continuous(
    "FDI, Deviation Between Predicted and Actual 2017") +
  scale_x_continuous("FDI, Actual 2017") +
  ggtitle(
    "FDI: Over- and Under-Invested Countries for Low & Lower Middle Income Countries, 2017") +
  theme_tufte() +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5))
dev.off()

## Make a Map of Predicted Deviations of FDI
# https://geocompr.robinlovelace.net/adv-map.html

data("World")
setnames(World, c("name", "iso_a3"), c("Country", "Code"))

lockbox$Code[!(lockbox$Code %in% World$Code)]

lockbox <- lockbox[order(lockbox$Code),]
World <- World[order(World$Code),]

lockbox_min <- lockbox[lockbox$Code %in% World$Code,] #159

World$Code[!(World$Code %in% lockbox_min$Code)] #177 levels
missing_country_df <- data.frame("Code" = c("ATA",
                                            "ATF",
                                            "CUB",
                                            "ERI",
                                            "ESH",
                                            "FLK",
                                            "GRL",
                                            "LBY",
                                            "NCL",
                                            "PRI",
                                            "PRK",
                                            "SOL",
                                            "SSD",
                                            "SYR",
                                            "TWN",
                                            "UNK",
                                            "VEN",
                                            "XTX"),
                                 stringsAsFactors = F)
lockbox_min <- rbind.fill(lockbox_min,missing_country_df)
lockbox_min <- lockbox_min[order(lockbox_min$Code),]

World$Deviation.RF4 <- lockbox_min$Deviation.RF4

## Create Map w/Better Title Boundary
#https://www.jla-data.net/eng/adjusting-bounding-box-of-a-tmap-map/

bbox_new <- st_bbox(World)

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.5 * xrange) # xmin - left
# bbox_new[3] <- bbox_new[3] + (0.5 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.5 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top

tmap_style("natural")
fdi_map <- 
  tm_shape(World, bbox = bbox_new) +
  tm_polygons("Deviation.RF4",
              title = "Measure of FDI \nUnder-Investment",
              breaks = c(-60, -20, -5, 0, 5, 20, 60)) +
  tm_layout(bg.color = "skyblue", 
            inner.margins = c(0, .02, .02, .02),
            title = "FDI Under-Investment for the World, 2017",
            legend.position = c("left", "bottom"),
            legend.title.size = 1.5,
            legend.text.size = 1.2,
            title.size = 3,
            frame = T) 
tmap_save(fdi_map, filename = "Plots/Prediction/Map/FDI Map.jpg")
# Deviation available for 159 out of 217 countries

