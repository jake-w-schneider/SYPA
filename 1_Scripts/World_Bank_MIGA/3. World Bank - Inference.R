####################################################################

## World Bank MIGA
## This  file:
##  - Reads in cleaned training and testing data sets
##  - Performs inferential analysis of in-sample (devoid of lockbox)
##
## Output:
##  - Regression outputs
##  - Scatter plot of analysis
## Author: Jake Schneider
## User: Jake Schneider
##Date Created:  06/24/2019
## Last Modified: 07/11/2019

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

## Setting a working directory 

getwd()

setwd("/Users/jschneids13/Desktop/World Bank - MIGA STT/Projects/Investment Analysis/MIGA-Private-/Full Research - By Country")

## Read in Datasets

load("R Data Sets/FDI Inputs/FDI_Input_Panel.Rdata")
load("R Data Sets/FDI Inputs/FDI_Complete_Cases.Rdata")

## Time Series of Aggregate World Levels

# Graph of Aggegrate FDI

## Variable Selection
## Analysis Based Upon ISLR 6.5 - 6.7 Lab 1 - Lab 3

# Set Name of Variables To Examine Using Subsets

attach(fdi_input_panel)
colnames(fdi_input_panel)

# Identifying A Working Panel of Data
# Source on Subsetting LM Model: https://stackoverflow.com/questions/11991692/using-rs-lm-on-a-dataframe-with-a-list-of-predictors

#regfit_full <- lm(fdi_input_panel[,11] ~ ., data = subset(fdi_input_panel, select = c(13 ,8 ,10, 17, 23, 24, 37, 41, 59, 74, 79, 80, 81, 82, 83, 84, 85, 86, 87))) # full regression with no cases

regfit_full <- lm(fdi_input_panel[,13] ~ ., data = subset(fdi_input_panel, select = c(10, 23, 24, 37, 59, 74, 82, 83, 87))) # full regression based upon Missing Data Visualization - Key Variables
coefficients(regfit_full)
summary(regfit_full)

# regfit_full1 <- plm(fdi_input_panel[,13] ~ fdi_input_panel[,10] 
#                     + fdi_input_panel[,23]
#                     + fdi_input_panel[,24]
#                     + fdi_input_panel[,37]
#                     + fdi_input_panel[,59]
#                     + fdi_input_panel[,74]
#                     + fdi_input_panel[,82]
#                     + fdi_input_panel[,83]
#                     + fdi_input_panel[,87],
#                     data = fdi_input_panel) # full regression based # upon Missing Data Visualization - Key Variables
# coefficients(regfit_full1)
# summary(regfit_full1)

# regfit_full1.1 <- plm(fdi_input_panel[,13] ~ fdi_input_panel[,10] 
#                     + fdi_input_panel[,23]
#                     + fdi_input_panel[,24]
#                     + fdi_input_panel[,37]
#                     + fdi_input_panel[,59]
#                     + fdi_input_panel[,74]
#                     + fdi_input_panel[,82]
#                     + fdi_input_panel[,83]
#                     + fdi_input_panel[,87],
#                     data = fdi_input_panel, index = c("Country", # "Year"), model = "within")  # full regression based upon Missing Data # Visualization - Key Variables
# coefficients(regfit_full1.1)
# summary(regfit_full1.1)

# regfit_full1.2 <- plm(fdi_input_panel[,13] ~ fdi_input_panel[,10] 
#                       + fdi_input_panel[,23]
#                       + fdi_input_panel[,24]
#                       + fdi_input_panel[,37]
#                       + fdi_input_panel[,59]
#                       + fdi_input_panel[,74]
#                       + fdi_input_panel[,82]
#                       + fdi_input_panel[,83]
#                       + fdi_input_panel[,87],
#                       data = fdi_input_panel, index = c("Country", # "Year"), model = "within", effect = "twoways")  # full regression # based upon Missing Data Visualization - Key Variables
# coefficients(regfit_full1.2)
# summary(regfit_full1.2) # not very good result: will try to recreate # original results to see what is going on

# Recreate original model from Stata

regfit_full_all <- 
    lm(Foreign.direct.investment..net.inflows....of.GDP. 
       ~ sp 
       + mdy 
       + fitch
       + polity2
       + cpia_composite
       + wgi_composite
       + fdi_input_panel$GDP.per.capita..constant.2010.US..
       + fdi_input_panel$GDP.growth..annual..., 
       data = fdi_input_panel)

summary(regfit_full_all) #r^2 64.31
nobs(regfit_full_all) #38

# regfit_full_all_fe <- plm(fdi_input_panel$Foreign.direct.investment# ..net.inflows....of.GDP. ~ fdi_input_panel$sp 
#                           + fdi_input_panel$mdy 
#                           + fdi_input_panel$fitch
#                           + fdi_input_panel$polity2
#                           + fdi_input_panel$cpia_composite
#                           + fdi_input_panel$wgi_composite
#                           + fdi_input_panel$GDP.per.capita..constant# .2010.US..
#                           + fdi_input_panel$GDP.growth..annual...,
#                           data = fdi_input_panel,
#                           index = c("Country", "Year"),
#                           model = "within")
# 
# summary(regfit_full_all_fe)
# nobs(regfit_full_all_fe)
# 
# regfit_full_all_re <- plm(fdi_input_panel$Foreign.direct.investment# ..net.inflows....of.GDP. ~ fdi_input_panel$sp 
#                           + fdi_input_panel$mdy 
#                           + fdi_input_panel$fitch
#                           + fdi_input_panel$polity2
#                           + fdi_input_panel$cpia_composite
#                           + fdi_input_panel$wgi_composite
#                           + fdi_input_panel$GDP.per.capita..constant# .2010.US..
#                           + fdi_input_panel$GDP.growth..annual...,
#                           data = fdi_input_panel,
#                           index = c("Country", "Year"),
#                           model = "random")
# 
# summary(regfit_full_all_re)
# nobs(regfit_full_all_re)

# regfit_full_all1 <- lm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, total # (% of GDP)`
#                       + fdi_input_panel$polity2
#                       + fdi_input_panel$csdr_avg
#                       + fdi_input_panel$wgi_composite
#                       + fdi_input_panel$`GDP per capita (constant 2010 # US$)`
#                       + fdi_input_panel$`GDP growth (annual %)`, data # = fdi_input_panel)
# 
# summary(regfit_full_all1)
# nobs(regfit_full_all1)
# 
# regfit_full_all2 <- lm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, total # (% of GDP)`
#                       + fdi_input_panel$sp 
#                       + fdi_input_panel$mdy 
#                       + fdi_input_panel$fitch
#                       + fdi_input_panel$csdr_avg
#                       + fdi_input_panel$polity2
#                       + fdi_input_panel$cpia_composite
#                       + fdi_input_panel$wgi_composite
#                       + fdi_input_panel$`GDP per capita (constant 2010 # US$)`
#                       + fdi_input_panel$`GDP growth (annual %)`, data # = fdi_input_panel)
# 
# summary(regfit_full_all2)
# nobs(regfit_full_all2)
# 
# 
# regfit_full_all3 <- lm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, total # (% of GDP)`
#                        + fdi_input_panel$sp 
#                        + fdi_input_panel$mdy 
#                        + fdi_input_panel$fitch
#                        + fdi_input_panel$csdr_avg
#                        + fdi_input_panel$polity2
#                        + fdi_input_panel$cpia_composite
#                        + fdi_input_panel$wgi_composite
#                        + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                        + fdi_input_panel$`GDP growth (annual %)`
#                        + fdi_input_panel$`Quality of port infrastructu# re, WEF (1=extremely underdeveloped to 7=well developed and efficient # by international standards)`, data = fdi_input_panel)
# 
# summary(regfit_full_all3)
# nobs(regfit_full_all3)
# 
# regfit_full_all4 <- lm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, total # (% of GDP)`
#                        + fdi_input_panel$sp 
#                        + fdi_input_panel$mdy 
#                        + fdi_input_panel$fitch
#                        + fdi_input_panel$csdr_avg
#                        + fdi_input_panel$polity2
#                        + fdi_input_panel$cpia_composite
#                        + fdi_input_panel$wgi_composite
#                        + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                        + fdi_input_panel$`GDP growth (annual %)`
#                        + fdi_input_panel$`Exports of Commodities to # the World, US$`, data = fdi_input_panel)
# 
# summary(regfit_full_all4)
# nobs(regfit_full_all4)
# 
# regfit_full_all5 <- lm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, total # (% of GDP)`
#                        + fdi_input_panel$sp 
#                        + fdi_input_panel$mdy 
#                        + fdi_input_panel$fitch
#                        + fdi_input_panel$csdr_avg
#                        + fdi_input_panel$polity2
#                        + fdi_input_panel$cpia_composite
#                        + fdi_input_panel$wgi_composite
#                        + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                        + fdi_input_panel$`GDP growth (annual %)`
#                        + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`, data = fdi_input_panel)
# 
# summary(regfit_full_all5)
# nobs(regfit_full_all5)
# 
# regfit_full_2017 <- lm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ fdi_input_panel$sp 
#                       + fdi_input_panel$mdy 
#                       + fdi_input_panel$fitch
#                       + fdi_input_panel$polity2
#                       + fdi_input_panel$cpia_composite
#                       + fdi_input_panel$wgi_composite
#                       + fdi_input_panel$`GDP per capita (constant 2010 # US$)`
#                       + fdi_input_panel$`GDP growth (annual %)`, data # = fdi_input_panel, subset = (fdi_input_panel$Year == 2017)) 
# 
# summary(regfit_full_2017) #no observations


# Subset Selection Methods: Best Subset Selection

regfit_full_subsets <- 
    regsubsets(fdi_input_panel$Foreign.direct.investment..net.inflows....of.GDP.
               ~ fdi_input_panel$Central.government.debt..total....of.GDP.
                       + fdi_input_panel$sp 
                       + fdi_input_panel$mdy 
                       + fdi_input_panel$fitch
                       + fdi_input_panel$csdr_avg
                       + fdi_input_panel$polity2
                       + fdi_input_panel$cpia_composite
                       + fdi_input_panel$wgi_composite
                       + fdi_input_panel$GDP.per.capita..constant.2010.US..
                       + fdi_input_panel$GDP.growth..annual...
                       + fdi_input_panel$Exports.of.Commodities.to.the.World.....of.GDP.
               + fdi_input_panel$Population.density..people.per.sq..km.of.land.area.
               + fdi_input_panel$Population.growth..annual...
               + fdi_input_panel$Population.in.largest.city
               + fdi_input_panel$Population..total,
                       data = fdi_input_panel,
               nvmax = 15)

summary(regfit_full_subsets)

regfit_summary <- summary(regfit_full_subsets)
names(regfit_summary)

regfit_summary$rsq
regfit_summary$adjr2

which.max(regfit_summary$adjr2)

coef(regfit_full_subsets, 6)

attach(fdi_input_panel)

lm_regfit6 <- lm(
    Foreign.direct.investment..net.inflows....of.GDP. ~
        Central.government.debt..total....of.GDP. +
        sp +
        GDP.per.capita..constant.2010.US.. +
        csdr_avg +
        polity2 +
        Population.in.largest.city,
    data = fdi_input_panel)

summary(lm_regfit6)
nobs(lm_regfit6)

fe_regfit6 <- plm(
    Foreign.direct.investment..net.inflows....of.GDP. ~
        Central.government.debt..total....of.GDP. +
        sp +
        GDP.per.capita..constant.2010.US.. +
        csdr_avg +
        polity2 +
        Population.in.largest.city,
    data = fdi_input_panel,
    model = "within",
    effect = "twoways",
    index = c("Country", "Year"))

summary(fe_regfit6)
nobs(fe_regfit6)

# re_regfit6 <- plm(
#     `Foreign direct investment, net inflows (% of GDP)` ~
#         `Central government debt, total (% of GDP)` +
#         sp +
#         `GDP per capita (constant 2010 US$)` +
#         csdr_avg +
#         polity2 +
#         `Population in largest city`,
#     data = fdi_input_panel,
#     model = "random",
#     effect = "twoways",
#     index = c("Country", "Year"))
# 
# summary(re_regfit6)
# nobs(re_regfit6)

# lm_subset_nv10 <- lm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~
#                        fdi_input_panel$`Central government debt, total # (% of GDP)`
#                      + fdi_input_panel$cpia_composite
#                      + fdi_input_panel$`Quality of port infrastructure# , WEF (1=extremely underdeveloped to 7=well developed and efficient by # international standards)`, data = fdi_input_panel)
# 
# summary(lm_subset_nv10)
# nobs(lm_subset_nv10)

# Plot Best Subset Selection

par(mfrow = c(2,2))

jpeg("Plots/Best Subset Selection/Best Subset Selection -- RSS.jpg", width = 500, height = 500)
plot(regfit_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
dev.off()

jpeg("Plots/Best Subset Selection/Best Subset Selection -- RSQ.jpg", width = 500, height = 500)
plot(regfit_summary$rsq, xlab = "Number of Variables", ylab = "RSQ", type = "l")
dev.off()

jpeg("Plots/Best Subset Selection/Best Subset Selection -- Adjusted RSQ", width = 500, height = 500)
plot(regfit_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSQ", type = "l")
dev.off()

which.max(regfit_summary$adjr2)

jpeg("Plots/Best Subset Selection/Best Subset Selection -- Adjusted RSQ Max", width = 500, height = 500)
plot(regfit_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSQ", type = "l")
points(6, regfit_summary$adjr2[6], col = "red", cex=2, pch=20)
dev.off()

jpeg("Plots/Best Subset Selection/Best Subset Selection -- Cp", width = 500, height = 500)
plot(regfit_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
dev.off()

which.min(regfit_summary$cp)

jpeg("Plots/Best Subset Selection/Best Subset Selection -- BIC", width = 500, height = 500)
plot(regfit_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
dev.off()

which.min(regfit_summary$bic)

jpeg("Plots/Best Subset Selection/Best Subset Selection -- BIC Min", width = 500, height = 500)
plot(regfit_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, regfit_summary$bic[6], col = "red", cex=2, pch=20)
dev.off()

jpeg("Plots/Maximing Subset/Reg Subsets -- R2", width = 500, height = 500)
plot(regfit_full_subsets, scale = "r2")
dev.off()

jpeg("Plots/Maximing Subset/Reg Subsets -- AdjR2", width = 500, height = 500)
plot(regfit_full_subsets, scale = "adjr2")
dev.off()

jpeg("Plots/Maximing Subset/Reg Subsets -- Cp", width = 500, height = 500)
plot(regfit_full_subsets, scale = "Cp")
dev.off()

jpeg("Plots/Maximing Subset/Reg Subsets -- BIC", width = 500, height = 500)
plot(regfit_full_subsets, scale = "bic")
dev.off()

# Base Regressions

# regfit_optimal_r2_1 <- lm(fdi_input_panel$`Foreign direct investment, # net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                        + fdi_input_panel$sp 
#                        + fdi_input_panel$mdy 
#                        + fdi_input_panel$fitch
#                        + fdi_input_panel$csdr_avg
#                        + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                        + fdi_input_panel$`GDP growth (annual %)` 
#                        + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`,
#                        data = fdi_input_panel) 
# 
# summary(regfit_optimal_r2_1) #r^2 31.76% and adjstr^2 30.3%
# nobs(regfit_optimal_r2_1) #383
# 
# regfit_optimal_r2_1.1 <- lm(fdi_input_panel$`Foreign direct investment# , net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                         + fdi_input_panel$csdr_avg
#                         + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                         + fdi_input_panel$`GDP growth (annual %)` 
#                         + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`,
#                         data = fdi_input_panel) 
# 
# summary(regfit_optimal_r2_1.1) #r^2 25.55% and adjstr^2 24.83%
# nobs(regfit_optimal_r2_1.1) # 523
# 
# regfit_optimal_r2_1.2 <- lm(fdi_input_panel$`Foreign direct investment# , net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                           + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                           + fdi_input_panel$`GDP growth (annual %)` 
#                           + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`,
#                           data = fdi_input_panel) 
# 
# summary(regfit_optimal_r2_1.2) #r^2 17.18% and adjstr^2 16.75%
# nobs(regfit_optimal_r2_1.2) # 785
# 
# regfit_optimal_r2_2 <- lm(fdi_input_panel$`Foreign direct investment, # net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                           + fdi_input_panel$sp 
#                           + fdi_input_panel$csdr_avg
#                           + fdi_input_panel$polity2
#                           + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                           + fdi_input_panel$`GDP growth (annual %)` 
#                           + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`,
#                           data = fdi_input_panel) 
# 
# summary(regfit_optimal_r2_2) #r^2 17.08% and adjstr^2 13.7%
# nobs(regfit_optimal_r2_2) #180
# 
# regfit_optimal_r2_4 <- lm(fdi_input_panel$`Foreign direct investment, # net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                           + fdi_input_panel$sp 
#                           + fdi_input_panel$wgi_composite
#                           + fdi_input_panel$polity2
#                           + fdi_input_panel$`GDP per capita (constant # 2010 US$)`,
#                           data = fdi_input_panel) 
# 
# summary(regfit_optimal_r2_4) #r^2 9.88% and adjstr^2 7.32%
# nobs(regfit_optimal_r2_4) #182

# regfit_optimal_r2_5 <- lm(fdi_input_panel$Foreign.direct.investment# ..net.inflows....of.GDP. ~  fdi_input_panel$Central.government.debt# ..total....of.GDP.
#              + fdi_input_panel$sp 
#              + fdi_input_panel$mdy 
#              + fdi_input_panel$fitch
#              + fdi_input_panel$csdr_avg
#              + fdi_input_panel$GDP.per.capita..constant.2010.US..
#              + fdi_input_panel$GDP.growth..annual...
#              + fdi_input_panel$Exports.of.Commodities.to.the.World# .....of.GDP.
#              + fdi_input_panel$Current.account.balance....of.GDP.
#              +fdi_input_panel$Reserves.and.related.items..BoP..current# .US..,
#              data = fdi_input_panel) 
# 
# summary(regfit_optimal_r2_5) #r^2 35.48% and adjstr^2 33.73%
# nobs(regfit_optimal_r2_5) #380
# This is the best model so far
# Note: This came from extra
# Note2: This model does not work with random effects for some reason

## Fixed Effects

# regfit_optimal_r2_1.0.1 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                              + fdi_input_panel$sp 
#                              + fdi_input_panel$mdy 
#                              + fdi_input_panel$fitch
#                              + fdi_input_panel$csdr_avg
#                              + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                              + fdi_input_panel$`GDP growth (annual %)` # 
#                              + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`,
#                              data = fdi_input_panel, model = "within") # 
# 
# summary(regfit_optimal_r2_1.0.1) #r^2 17.01% and adjstr^2 5.92%
# nobs(regfit_optimal_r2_1.0.1) # 383
# 
# regfit_optimal_r2_1.0.2 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                              + fdi_input_panel$sp 
#                              + fdi_input_panel$mdy 
#                              + fdi_input_panel$fitch
#                              + fdi_input_panel$csdr_avg
#                              + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                              + fdi_input_panel$`GDP growth (annual %)` # 
#                              + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`,
#                              data = fdi_input_panel, 
#                              model = "within", 
#                              effect = "twoways") 
# 
# summary(regfit_optimal_r2_1.0.2) #r^2 16.60% and adjstr^2 -5.04%
# nobs(regfit_optimal_r2_1.0.2) # 383

# regfit_optimal_r2_1.0.3 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$sp 
#                                + fdi_input_panel$mdy 
#                                + fdi_input_panel$fitch
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`,
#                                data = fdi_input_panel, 
#                                model = "random", 
#                                effect = "twoways") 
# 
# summary(regfit_optimal_r2_1.0.3) #r^2 30.2%, adjstr^2 28.7%, almost # all variables statistically significant at the highest level (except # sp)
# nobs(regfit_optimal_r2_1.0.3) # 383
# # This is the best model
# 
# regfit_optimal_r2_1.0.3.1 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$mdy 
#                                + fdi_input_panel$fitch
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`,
#                                data = fdi_input_panel, 
#                                model = "random", 
#                                effect = "twoways") 
# 
# summary(regfit_optimal_r2_1.0.3.1) #r^2 29.31%, adjstr^2 28.01%, # almost all variables statistically significant at the highest level # (except sp)
# nobs(regfit_optimal_r2_1.0.3.1) # 389

# regfit_optimal_r2_1.0.3.2 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$sp 
#                                + fdi_input_panel$mdy 
#                                + fdi_input_panel$fitch
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditie# s to the World, (% of GDP)`,
#                                data = fdi_input_panel, 
#                                subset = (fdi_input_panel$Year > 1995),
#                                model = "random", 
#                                effect = "twoways") 
# 
# summary(regfit_optimal_r2_1.0.3.2) #r^2 30.2%, adjstr^2 28.7%, almost # all variables statistically significant at the highest level (except sp# )
# nobs(regfit_optimal_r2_1.0.3.2) # 383
# # No observations

# regfit_optimal_r2_1.0.3.3 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$sp 
#                                + fdi_input_panel$mdy 
#                                + fdi_input_panel$fitch
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`Reserves and related # items (BoP, current US$)`
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditie# s to the World, (% of GDP)`,
#                                data = fdi_input_panel, 
#                                model = "random", 
#                                effect = "twoways") 
# 
# summary(regfit_optimal_r2_1.0.3.3)
# nobs(regfit_optimal_r2_1.0.3.3) 
# # Not invertible

# regfit_optimal_r2_1.0.3.4 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$sp 
#                                + fdi_input_panel$mdy 
#                                + fdi_input_panel$fitch
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditie# s to the World, (% of GDP)`
#                                + factor(fdi_input_panel$Year),
#                                data = fdi_input_panel, 
#                                model = "random") 
# 
# summary(regfit_optimal_r2_1.0.3.4)
# nobs(regfit_optimal_r2_1.0.3.4) 
# # Computationally singular

# regfit_optimal_r2_1.1.1 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`,
#                                data = fdi_input_panel, 
#                                model = "within") 
# 
# summary(regfit_optimal_r2_1.1.1) #r^2 12.4% and adjstr^2 2.7%
# nobs(regfit_optimal_r2_1.1.1) # 523
# 
# regfit_optimal_r2_1.1.2 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`,
#                                data = fdi_input_panel, 
#                                model = "within", 
#                                effect = "twoways") 
# 
# summary(regfit_optimal_r2_1.1.2) #r^2 10.29% and adjstr^2 -4.05%
# nobs(regfit_optimal_r2_1.1.2) # 523
# 
# regfit_optimal_r2_1.1.3 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`,
#                                data = fdi_input_panel, 
#                                model = "random", 
#                                effect = "twoways") 
# 
# summary(regfit_optimal_r2_1.1.3) #r^2 23.82% and adjstr^2 23.08% #all # variables significant at the highest level
# nobs(regfit_optimal_r2_1.1.3) # 523
# 
# regfit_optimal_r2_2.1 <- plm(fdi_input_panel$`Foreign direct investmen# t, net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                           + fdi_input_panel$sp 
#                           + fdi_input_panel$csdr_avg
#                           + fdi_input_panel$polity2
#                           + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                           + fdi_input_panel$`GDP growth (annual %)` 
#                           + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`,
#                           data = fdi_input_panel, model = "within") 
# 
# summary(regfit_optimal_r2_2.1) #r^2 6.5% and adjstr^2 -8.7%
# nobs(regfit_optimal_r2_2.1) #180
# 
# regfit_optimal_r2_2.2 <- plm(fdi_input_panel$`Foreign direct investmen# t, net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                              + fdi_input_panel$sp 
#                              + fdi_input_panel$csdr_avg
#                              + fdi_input_panel$polity2
#                              + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                              + fdi_input_panel$`GDP growth (annual %)` # 
#                              + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`,
#                              data = fdi_input_panel, 
#                              model = "within",
#                              effect = "twoways") 
# 
# summary(regfit_optimal_r2_2.2) #r^2 16.3% and adjstr^2 -25%
# nobs(regfit_optimal_r2_2.2) #180
# 
# regfit_optimal_r2_2.3 <- plm(fdi_input_panel$`Foreign direct investmen# t, net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                              + fdi_input_panel$sp 
#                              + fdi_input_panel$csdr_avg
#                              + fdi_input_panel$polity2
#                              + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                              + fdi_input_panel$`GDP growth (annual %)` # 
#                              + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`,
#                              data = fdi_input_panel, 
#                              model = "random",
#                              effect = "twoways") 
# 
# summary(regfit_optimal_r2_2.3) #r^2 16.93% and adjstr^2 13.5%
# nobs(regfit_optimal_r2_2.3) #180
# 
# regfit_optimal_r2_3.3 <- plm(fdi_input_panel$`Foreign direct investmen# t, net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                              + fdi_input_panel$sp 
#                              + fdi_input_panel$csdr_avg
#                              + fdi_input_panel$polity2
#                              + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                              + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`,
#                              data = fdi_input_panel, 
#                              model = "random",
#                              effect = "twoways") 
# 
# summary(regfit_optimal_r2_3.3) #r^2 16.99%, adjstr^2 14.11%, all # statistically significant at the highest levels
# nobs(regfit_optimal_r2_3.3) #180
# 
# regfit_optimal_r2_4.3 <- plm(fdi_input_panel$`Foreign direct investmen# t, net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                           + fdi_input_panel$sp 
#                           + fdi_input_panel$wgi_composite
#                           + fdi_input_panel$polity2
#                           + fdi_input_panel$`GDP per capita (constant # 2010 US$)`,
#                           data = fdi_input_panel,
#                           model = "random",
#                           effect = "twoways") 
# 
# summary(regfit_optimal_r2_4.3) #r^2 9.85% and adjstr^2 7.28%
# nobs(regfit_optimal_r2_4.3) #182
# 
# ## Testing Which Governance Indicators are Most Correlated with FDI
# 
# 
# 
# ## Validation Set Apprach
# 
# # Set Seed and Split Data
# 
# set.seed(13)
# train <- sample(c(TRUE, FALSE), nrow(fdi_input_panel), rep = TRUE)
# test <- (!train)
# 
# # Apply Regsubsets to Training
# 
# regfit_full_train <- regsubsets(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ 
#                       fdi_input_panel$`Central government debt, total # (% of GDP)`
#                     + fdi_input_panel$sp 
#                     + fdi_input_panel$mdy 
#                     + fdi_input_panel$fitch
#                     + fdi_input_panel$csdr_avg
#                     + fdi_input_panel$polity2
#                     + fdi_input_panel$cpia_composite
#                     + fdi_input_panel$wgi_composite
#                     + fdi_input_panel$`GDP per capita (constant 2010 # US$)`
#                     + fdi_input_panel$`GDP growth (annual %)`
#                     + fdi_input_panel$`Exports of Commodities to the # World, (% of GDP)`, 
#                     data = fdi_input_panel[train,])  
# 
# # Calculate Validation Set Error Using Test Data
# 
# test_matrix <- model.matrix(fdi_input_panel$`Foreign direct investment# , net inflows (% of GDP)` ~ 
#                 fdi_input_panel$`Central government debt, total (% of # GDP)`
#               + fdi_input_panel$sp 
#               + fdi_input_panel$mdy 
#               + fdi_input_panel$fitch
#               + fdi_input_panel$csdr_avg
#               + fdi_input_panel$polity2
#               + fdi_input_panel$cpia_composite
#               + fdi_input_panel$wgi_composite
#               + fdi_input_panel$`GDP per capita (constant 2010 US$)`
#               + fdi_input_panel$`GDP growth (annual %)`
#               + fdi_input_panel$`Exports of Commodities to the World, # (% of GDP)`, data = fdi_input_panel[test,])
# 
# # Create Validation Matrix
# 
# validation_errors = rep(NA, 8)
# 
# for (i in 1:8) {
#   coeficients = coef(regfit_full_train, id = i)
#   pred = test_matrix[,names(coeficients)]%*%coeficients
#   validation_errors[i]=mean((fdi_input_panel$`Foreign direct investmen# t, net inflows (% of GDP)`[c(641, 642, 4437:4445)]-pred)^2)
# }
# # Something strange here: worked the first time (with throwing an # error), sometimes throws a strange error with options 0, 1 or 2
# 
# # Find the Lowest Validation Error
# 
# validation_errors
# which.min(validation_errors)
# 
# coef(regfit_full_train, 7)
# summary(regfit_full_train, 7)
# 
# # Create Predict Function for Regsubsets
# 
# # predict.regsubsets <- function(object, newdata, id, ...){
# #   form=as.formula(object$call[[2]])
# #   mat=matrix(form, newdata)
# #   coeficients = coef(object, id=id)
# #   xvars=names(coeficients)
# #   mat[,xvars]%*%coeficients
# # }
# 
# # Try Function
# 
# # regfit_full_function <- predict.regsubsets(fdi_input_panel$`Foreign # # # direct investment, net inflows (% of GDP)` ~ 
# #             fdi_input_panel$`Central government debt, total (% of # GDP)`
# #           + fdi_input_panel$sp 
# #           + fdi_input_panel$mdy 
# #           + fdi_input_panel$fitch
# #           + fdi_input_panel$csdr_avg
# #           + fdi_input_panel$polity2
# #           + fdi_input_panel$cpia_composite
# #           + fdi_input_panel$wgi_composite
# #           + fdi_input_panel$`GDP per capita (constant 2010 US$)`
# #           + fdi_input_panel$`GDP growth (annual %)`
# #           + fdi_input_panel$`Exports of Commodities to the World, (% # of # GDP)`,fdi_input_panel[test,])
# # # Cannot get to work
# 
# ## Cross Validation
# 
# # k <- 10
# # set.seed(13)
# # folds <- sample(1:k, nrow(fdi_input_panel), replace = TRUE)
# # cv_errors <- matrix(NA, k, 8, dimnames = list(NULL, paste(1:8))) 
# # 
# # for (j in 1:k) {
# #   best_fit <- regsubsets(fdi_input_panel$`Foreign direct investment, # # net inflows (% of GDP)` ~ 
# #             fdi_input_panel$`Central government debt, total (% of # GDP)`
# #           + fdi_input_panel$sp 
# #           + fdi_input_panel$mdy 
# #           + fdi_input_panel$fitch
# #           + fdi_input_panel$csdr_avg
# #           + fdi_input_panel$polity2
# #           + fdi_input_panel$cpia_composite
# #           + fdi_input_panel$wgi_composite
# #           + fdi_input_panel$`GDP per capita (constant 2010 US$)`
# #           + fdi_input_panel$`GDP growth (annual %)`
# #           + fdi_input_panel$`Exports of Commodities to the World, (% # of GDP)`, data = fdi_input_panel[folds!=j,])
# #   for (i in 1:8) {
# #     pred <- predict.regsubsets(best_fit, fdi_input_panel[folds==j,], # id =i)
# #     cv_errors[j,i]=mean((fdi_input_panel$`Foreign direct investment, # # net inflows (% of GDP)`[folds==j]-pred)^2)
# #   }
# # }
# # Did not work
# 
# ## Ridge Regression and Lasso Regression
# 
# # Ridge Ression
# 
# # Intro to Ridge (Without Training and Testing)
# 
# x <- model.matrix(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ 
#                    fdi_input_panel$`Central government debt, total (% # of GDP)`
#                  + fdi_input_panel$sp 
#                  + fdi_input_panel$mdy 
#                  + fdi_input_panel$fitch
#                  + fdi_input_panel$csdr_avg
#                  + fdi_input_panel$`GDP per capita (constant 2010 US$# )`
#                  + fdi_input_panel$`GDP growth (annual %)` 
#                  + fdi_input_panel$`Exports of Commodities to the # World, (% of GDP)`, fdi_input_panel[,-1])
# 
# y <- fdi_complete_cases$`Foreign direct investment, net inflows (% of # GDP)`
# 
# grid <- 10^seq(10, -2, length = 100)
# ridge_mod <- glmnet(x, y, alpha = 0, lambda = grid)
# 
# dim(coef(ridge_mod))
# 
# ridge_mod$lambda[50]
# sqrt(sum(coef(ridge_mod)[-1,50]^2))
# 
# ridge_mod$lambda[60]
# sqrt(sum(coef(ridge_mod)[-1,60]^2))
# 
# # Split Training and Testing
# 
# set.seed(13)
# train_ridge <- sample(1:nrow(x), nrow(x)/2)
# test_ridge <- -(train_ridge)
# y_test <- y[test_ridge]
# 
# # Run Ridge Regression Using Lambda = 4
# 
# ridge_mod_train <- glmnet(x[train_ridge,], y[train_ridge], alpha = 0, # lambda = grid, thresh = 1e-12)
# 
# ridge_predict_4 <- predict(ridge_mod_train, s=4, newx = x[test_ridge,]# )
# 
# mean((ridge_predict_4 - y_test)^2) #MSE of 55.57
# 
# # Compare to Fit with Just an Intercept
# 
# mean((mean(y[train_ridge]) - y_test)^2) #MSE of 72.18
# 
# # Compare to Normal Regression (Ridge with Lambda = 0)
# 
# ridge_predict_0 <- predict(ridge_mod_train, s=0, newx = x[test_ridge,]# )
# mean((ridge_predict_0 - y_test)^2) #MSE of 52.12
# 
# ridge_predict_lm <- lm(y~x, subset = train_ridge)
# ridge_predict_lm$coefficients
# # mean((ridge_predict_lm$fitted.values[y_test, ] - y_test)^2) 
# 
# # predict(ridge_mod_train, s=0, type = "coefficients")[1:20,]
# ridge_mod_0 <- glmnet(x[train_ridge,], y[train_ridge], alpha = 0, # lambda = 0, thresh = 1e-12)
# ridge_mod_0$beta
# 
# # These are basically the same
# # Take away: Ridge Regressin Does not help to minimize MSE
# 
# # Lasso Regression
# 
# ## PLS and PCR Regression
# 
# ## Random Forest Variable Selection
# 
# ## Generalized Method of Moments
# ## Read about GMM models to see if appropriate
# 
# ## Optimal Model
# 
# regfit_optimal <- plm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, total # (% of GDP)`
#                           + fdi_input_panel$csdr_avg
#                           + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                           + fdi_input_panel$`GDP growth (annual %)` 
#                           + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`
#                           + fdi_input_panel$`Current account balance # (% of GDP)`
#                           + fdi_input_panel$`Quality of port infrastru# cture, WEF (1=extremely underdeveloped to 7=well developed and # efficient by international standards)`,
# 
#                           data = fdi_input_panel, 
#                           model = "random",
#                           effect = "twoway") 
# 
# summary(regfit_optimal) #r^2 31.15 and adjstr^2 29.534%
# nobs(regfit_optimal) #307
# # Note: experimented with numerous versions of this model, still # believe this is the best

regfit_optimal2 <- plm(
    Foreign.direct.investment..net.inflows....of.GDP.
    ~ Central.government.debt..total....of.GDP.
    + csdr_avg
    + GDP.per.capita..constant.2010.US..
    + GDP.growth..annual...
    + Exports.of.Commodities.to.the.World.....of.GDP.
    + Current.account.balance....of.GDP.
    + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
    + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate,
                      data = fdi_input_panel, 
                      model = "random",
                      effect = "twoway",
                      index = c("Country", "Year")) 

summary(regfit_optimal2) #r^2 31.39% and adjstr^2 29.54%
nobs(regfit_optimal2) #307
# This regression is even better: price level is also highly statistically related

regfit_optimal2_fe <- plm(
    Foreign.direct.investment..net.inflows....of.GDP.
    ~ Central.government.debt..total....of.GDP.
    + csdr_avg
    + GDP.per.capita..constant.2010.US..
    + GDP.growth..annual...
    + Exports.of.Commodities.to.the.World.....of.GDP.
    + Current.account.balance....of.GDP.
    + Quality.of.port.infrastructure..WEF..1.extremely.underdeveloped.to.7.well.developed.and.efficient.by.international.standards.
    + Price.level.ratio.of.PPP.conversion.factor..GDP..to.market.exchange.rate,
    data = fdi_input_panel, 
    model = "within",
    effect = "twoway",
    index = c("Country", "Year")) 

summary(regfit_optimal2_fe) #r^2 17.21% and adjstr^2 -0.001%
nobs(regfit_optimal2_fe) #307

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
    data = fdi_input_panel)  

summary(regfit_optimal2_lm) #r^2 32.1% and adjstr^2 30.27%
nobs(regfit_optimal2_lm) #307

# Test Regfit Optimal2 with Population: regfit_optimal3

regfit_optimal3_lm <- lm(
    `Foreign direct investment, net inflows (% of GDP)`
    ~ `Central government debt, total (% of GDP)`
    + csdr_avg
    + `GDP per capita (constant 2010 US$)`
    + `GDP growth (annual %)`
    + `Exports of Commodities to the World, (% of GDP)`
    + `Current account balance (% of GDP)`
    + `Quality of port infrastructure, WEF (1=extremely underdeveloped to 7=well developed and efficient by international standards)`
    + `Price level ratio of PPP conversion factor (GDP) to market exchange rate`
    + `Population in largest city`,
    data = fdi_input_panel) 

summary(regfit_optimal3_lm) #r^2 37.55% and adjstr^2 35.58%
nobs(regfit_optimal3_lm) #296
# This regression is better but does not deal with OVB

regfit_optimal3_fe <- plm(
    `Foreign direct investment, net inflows (% of GDP)`
    ~ `Central government debt, total (% of GDP)`
    + csdr_avg
    + `GDP per capita (constant 2010 US$)`
    + `GDP growth (annual %)`
    + `Exports of Commodities to the World, (% of GDP)`
    + `Current account balance (% of GDP)`
    + `Quality of port infrastructure, WEF (1=extremely underdeveloped to 7=well developed and efficient by international standards)`
    + `Price level ratio of PPP conversion factor (GDP) to market exchange rate`
    + `Population in largest city`,
    model = "within",
    effect = "twoways",
    index = c("Country", "Year"),
    data = fdi_input_panel) 

summary(regfit_optimal3_fe) #r^2 22.166% and adjstr^2 0.04726%
nobs(regfit_optimal3_fe) #296
#Terrible

# regfit_optimal3 <- plm(
#     `Foreign direct investment, net inflows (% of GDP)`
#     ~ `Central government debt, total (% of GDP)`
#     + csdr_avg
#     + `GDP per capita (constant 2010 US$)`
#     + `GDP growth (annual %)`
#     + `Exports of Commodities to the World, (% of GDP)`
#     + `Current account balance (% of GDP)`
#     + `Quality of port infrastructure, WEF (1=extremely underdeveloped # to 7=well developed and efficient by international standards)`
#     + `Price level ratio of PPP conversion factor (GDP) to market # exchange rate`
#     + `Population in largest city`,
#     model = "random",
#     effect = "twoways",
#     index = c("Country", "Year"),
#     data = fdi_input_panel) 
# 
# summary(regfit_optimal3) #r^2 37.55% and adjstr^2 35.58%
# nobs(regfit_optimal3) #296
# #Does not run, cannot find a combination with population that is invertible


# regfit_optimal2_pooling <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                        + fdi_input_panel$csdr_avg
#                        + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                        + fdi_input_panel$`GDP growth (annual %)` 
#                        + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`
#                        + fdi_input_panel$`Current account balance (% # of GDP)`
#                        + fdi_input_panel$`Quality of port infrastructu# re, WEF (1=extremely underdeveloped to 7=well developed and efficient # by international standards)`
#                        + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate`,
#                        data = fdi_input_panel, 
#                        model = "pooling",
#                        effect = "twoways") 
# 
# summary(regfit_optimal2_pooling) #r^2 32.1% and adjstr^2 30.27%
# nobs(regfit_optimal2_pooling) #307
# 
# 
# regfit_optimal3 <- plm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, total # (% of GDP)`
#                       + fdi_input_panel$csdr_avg
#                       + fdi_input_panel$`GDP per capita (constant 2010 # US$)`
#                       + fdi_input_panel$`GDP growth (annual %)` 
#                       + fdi_input_panel$`Exports of Commodities to the # World, (% of GDP)`
#                       + fdi_input_panel$`Current account balance (% of # GDP)`
#                       + fdi_input_panel$`Quality of port infrastructur# e, WEF (1=extremely underdeveloped to 7=well developed and efficient # by international standards)`  
#                       + fdi_input_panel$sitc_eci,
#                       data = fdi_input_panel, 
#                       model = "random",
#                       effect = "twoway") 
# 
# summary(regfit_optimal3) #r^2 30.07 and adjstr^2 28.12%
# nobs(regfit_optimal3) #296
# # This is also a good model: not quite as good as the other but can # use both price level and atlas of economic complexity because of data # paucity

# regfit_optimal2_ssa <- plm(fdi_input_panel$Foreign.direct.investment# ..net.inflows....of.GDP. ~ fdi_input_panel$Central.government.debt# ..total....of.GDP.
#                        + fdi_input_panel$csdr_avg
#                        + fdi_input_panel$GDP.per.capita..constant.2010# .US..
#                        + fdi_input_panel$Exports.of.Commodities.to.the# .World..US.
#                        + fdi_input_panel$Current.account.balance....of# .GDP.
#                        + fdi_input_panel$Price.level.ratio.of.PPP# .conversion.factor..GDP..to.market.exchange.rate
#                        + (fdi_input_panel$GDP.growth..annual...*fdi_in# put_panel$`Sub-Saharan Africa`) 
#                        ,
#                        data = fdi_input_panel, 
#                        model = "random",
#                        effect = "twoway",
#                        index = c("Country", "Year")) 
# 
# summary(regfit_optimal2_ssa) #r^2 31.39% and adjstr^2 29.54%
# nobs(regfit_optimal2_ssa) #307

# regfit_optimal2_ida <- plm(fdi_input_panel$`Foreign direct investment, # net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                            + fdi_input_panel$csdr_avg
#                            + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                            + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`
#                            + fdi_input_panel$`Current account balance # (% of GDP)`
#                            + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate`
#                            + (fdi_input_panel$`GDP growth (annual %)`# *fdi_input_panel$IDA) 
#                            ,
#                            data = fdi_input_panel, 
#                            model = "random",
#                            effect = "twoway") 
# 
# summary(regfit_optimal2_ida) #r^2 16.62% and adjstr^2 14.3%
# nobs(regfit_optimal2_ida) #334
# 
# regfit_optimal2_ibrd <- plm(fdi_input_panel$`Foreign direct investment# , net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                            + fdi_input_panel$csdr_avg
#                            + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                            + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`
#                            + fdi_input_panel$`Current account balance # (% of GDP)`
#                            + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate`
#                            + (fdi_input_panel$`GDP growth (annual %)`# *fdi_input_panel$IBRD) 
#                            ,
#                            data = fdi_input_panel, 
#                            model = "random",
#                            effect = "twoway") 
# 
# summary(regfit_optimal2_ibrd) #r^2 17.45% and adjstr^2 15.16%
# nobs(regfit_optimal2_ibrd) #334
# 
# regfit_optimal2_noida <- plm(fdi_input_panel$`Foreign direct investmen# t, net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                             + fdi_input_panel$csdr_avg
#                             + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                             + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`
#                             + fdi_input_panel$`Current account balance # (% of GDP)`
#                             + fdi_input_panel$`Price level ratio of # PPP conversion factor (GDP) to market exchange rate`
#                             + (fdi_input_panel$`GDP growth (annual %)`# *fdi_input_panel$`Not IDA or IBRD`) 
#                             ,
#                             data = fdi_input_panel, 
#                             model = "random",
#                             effect = "twoway") 
# 
# summary(regfit_optimal2_noida) #r^2 17.45% and adjstr^2 15.16%
# nobs(regfit_optimal2_noida) #334

# regfit_optimal2_incomegroup <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                            + fdi_input_panel$csdr_avg
#                            + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                            + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`
#                            + fdi_input_panel$`Current account balance # (% of GDP)`
#                            + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate`
#                            + (fdi_input_panel$`GDP growth (annual %)`# *fdi_input_panel$`Income Group`) 
#                            ,
#                            data = fdi_input_panel, 
#                            model = "random",
#                            effect = "twoway") 
# 
# summary(regfit_optimal2_incomegroup) #r^2 31.39% and adjstr^2 29.54%
# nobs(regfit_optimal2_incomegroup) #307

# regfit_optimal2_lending <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                            + fdi_input_panel$csdr_avg
#                            + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                            + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`
#                            + fdi_input_panel$`Current account balance # (% of GDP)`
#                            + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate`
#                            + (fdi_input_panel$`GDP growth (annual %)`# *fdi_input_panel$`Lending Category`) 
#                            ,
#                            data = fdi_input_panel, 
#                            model = "random",
#                            effect = "twoway") 

# summary(regfit_optimal2_lending) #r^2 17.27% and adjstr^2 14.44%
# nobs(regfit_optimal2_lending) #334
# 
# regfit_optimal2.1 <- plm(fdi_input_panel$`Foreign direct investment, # net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                        + fdi_input_panel$csdr_avg
#                        + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                        + fdi_input_panel$`GDP growth (annual %)` 
#                        + fdi_input_panel$gdp_growth2
#                        + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`
#                        + fdi_input_panel$`Current account balance (% # of GDP)`
#                        + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate`,
#                        data = fdi_input_panel, 
#                        model = "random",
#                        effect = "twoway") 
# 
# summary(regfit_optimal2.1) #r^2 35.86% and adjstr^2 34.84%
# nobs(regfit_optimal2.1) #515
# # This regression is even better: price level is also highly statistic# ally related
# 
# regfit_optimal2.1.1 <- plm(fdi_input_panel$`Foreign direct investment, # net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                          + fdi_input_panel$debt2
#                          + fdi_input_panel$csdr_avg
#                          + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                          + fdi_input_panel$`GDP growth (annual %)` 
#                          + fdi_input_panel$gdp_growth2
#                          + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`
#                          + fdi_input_panel$`Current account balance (% # of GDP)`
#                          + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate`,
#                          data = fdi_input_panel, 
#                          model = "random",
#                          effect = "twoway") 
# 
# summary(regfit_optimal2.1.1) #r^2 36.17% and adjstr^2 35.03%
# nobs(regfit_optimal2.1.1) #515
# # This regression improves on the fit
# 
# regfit_optimal2.1.2 <- plm(fdi_input_panel$`Foreign direct investment, # net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                            + fdi_input_panel$debt2
#                            + fdi_input_panel$csdr_avg
#                            + fdi_input_panel$csdr_avg2
#                            + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                            + fdi_input_panel$`GDP growth (annual %)` 
#                            + fdi_input_panel$gdp_growth2
#                            + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`
#                            + fdi_input_panel$`Current account balance # (% of GDP)`
#                            + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate`,
#                            data = fdi_input_panel, 
#                            model = "random",
#                            effect = "twoway") 
# 
# summary(regfit_optimal2.1.2) #r^2 36.22% and adjstr^2 34.96%
# nobs(regfit_optimal2.1.2) #307
# # This regression is even better: price level is also highly statistic# ally related

# regfit_optimal2.2 <- plm(fdi_input_panel$`Foreign direct investment, # net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                          + fdi_input_panel$csdr_avg
#                          + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                          + fdi_input_panel$gdp_per_capita2
#                          + fdi_input_panel$`GDP growth (annual %)` 
#                          + fdi_input_panel$`Current account balance (% # of GDP)`
#                         ,
#                          data = fdi_input_panel, 
#                          model = "random",
#                          effect = "twoway") 
# 
# summary(regfit_optimal2.2) #r^2 35.86% and adjstr^2 34.84%
# nobs(regfit_optimal2.2) #307
# Does not work

# Lags

# regfit_optimal2_lag1 <- plm(fdi_input_panel$`Foreign direct investment# , net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                        + fdi_input_panel$csdr_avg
#                        + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                        + fdi_input_panel$`GDP growth (annual %)` 
#                        + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`
#                        + fdi_input_panel$`Current account balance (% # of GDP)`
#                        + fdi_input_panel$`Quality of port infrastructu# re, WEF (1=extremely underdeveloped to 7=well developed and efficient # by international standards)`
#                        + fdi_input_panel$fdi_1,
#                        data = fdi_input_panel, 
#                        model = "random",
#                        effect = "twoway") 
# 
# summary(regfit_optimal2_lag1) #r^2 35.82% and adjstr^2 34.10%
# nobs(regfit_optimal2_lag1) #307
# 
# regfit_optimal2_lag2 <- plm(fdi_input_panel$`Foreign direct investment# , net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                             + fdi_input_panel$csdr_avg
#                             + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                             + fdi_input_panel$`GDP growth (annual %)` 
#                             + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`
#                             + fdi_input_panel$`Current account balance # (% of GDP)`
#                             + fdi_input_panel$fdi_1
#                             + fdi_input_panel$fdi_2,
#                             data = fdi_input_panel, 
#                             model = "random",
#                             effect = "twoway") 
# 
# summary(regfit_optimal2_lag2) #r^2 38.51% and adjstr^2 37.49%
# nobs(regfit_optimal2_lag2) #489
# 
# regfit_optimal2_lag3 <- plm(fdi_input_panel$`Foreign direct investment# , net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                             + fdi_input_panel$csdr_avg
#                             + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                             + fdi_input_panel$`GDP growth (annual %)` 
#                             + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`
#                             + fdi_input_panel$`Current account balance # (% of GDP)`
#                             + fdi_input_panel$fdi_1
#                             + fdi_input_panel$fdi_2
#                             + fdi_input_panel$fdi_3,
#                             data = fdi_input_panel, 
#                             model = "random",
#                             effect = "twoway") 
# 
# summary(regfit_optimal2_lag3) #r^2 43.33% and adjstr^2 42.19%; lose # significance on variables for lagged fdi
# nobs(regfit_optimal2_lag3) #459
# 
# regfit_optimal2_lag2.1 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                             + fdi_input_panel$csdr_avg
#                             + fdi_input_panel$gdpcap_1
#                             + fdi_input_panel$`GDP growth (annual %)` 
#                             + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`
#                             + fdi_input_panel$`Current account balance # (% of GDP)`
#                             + fdi_input_panel$fdi_1
#                             + fdi_input_panel$fdi_2,
#                             data = fdi_input_panel, 
#                             model = "random",
#                             effect = "twoway") 
# 
# summary(regfit_optimal2_lag2.1) #r^2 38.49% and adjstr^2 37.47%
# nobs(regfit_optimal2_lag2.1) #489
# 
# regfit_optimal2_lag2.2 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                               + fdi_input_panel$csdr_avg
#                               + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                               + fdi_input_panel$`GDP growth (annual %# )` 
#                               + fdi_input_panel$gdpgrowth_1
#                               + fdi_input_panel$`Exports of Commoditie# s to the World, (% of GDP)`
#                               + fdi_input_panel$`Current account # balance (% of GDP)`
#                               + fdi_input_panel$fdi_1
#                               + fdi_input_panel$fdi_2,
#                               data = fdi_input_panel, 
#                               model = "random",
#                               effect = "twoway") 
# 
# summary(regfit_optimal2_lag2.2) #r^2 38.91% and adjstr^2 37.77%
# nobs(regfit_optimal2_lag2.2) #489
# 
# regfit_optimal2_lag2.3 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                               + fdi_input_panel$csdr_avg
#                               + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                               + fdi_input_panel$`GDP growth (annual %# )` 
#                               + fdi_input_panel$gdpgrowth_1
#                               + fdi_input_panel$gdpgrowth_2
#                               + fdi_input_panel$`Exports of Commoditie# s to the World, (% of GDP)`
#                               + fdi_input_panel$`Current account # balance (% of GDP)`
#                               + fdi_input_panel$fdi_1
#                               + fdi_input_panel$fdi_2,
#                               data = fdi_input_panel, 
#                               model = "random",
#                               effect = "twoway") 
# 
# summary(regfit_optimal2_lag2.3) #r^2 39.25% and adjstr^2 37.97%
# nobs(regfit_optimal2_lag2.3) #489
# 
# regfit_optimal2_lag2.4 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                               + fdi_input_panel$csdr_avg
#                               + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                               + fdi_input_panel$`GDP growth (annual %# )` 
#                               + fdi_input_panel$gdpgrowth_1
#                               + fdi_input_panel$gdpgrowth_2
#                               + fdi_input_panel$gdpgrowth_3
#                               + fdi_input_panel$`Exports of Commoditie# s to the World, (% of GDP)`
#                               + fdi_input_panel$`Current account # balance (% of GDP)`
#                               + fdi_input_panel$fdi_1
#                               + fdi_input_panel$fdi_2,
#                               data = fdi_input_panel, 
#                               model = "random",
#                               effect = "twoway") 
# 
# summary(regfit_optimal2_lag2.4) #r^2 44.30% and adjstr^2 42.93%
# nobs(regfit_optimal2_lag2.4) #459
# 
# regfit_optimal2_lag2.5 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                               + fdi_input_panel$csdr_avg
#                               + fdi_input_panel$csdr_1
#                               + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                               + fdi_input_panel$`GDP growth (annual %# )` 
#                               + fdi_input_panel$gdpgrowth_1
#                               + fdi_input_panel$gdpgrowth_2
#                               + fdi_input_panel$gdpgrowth_3
#                               + fdi_input_panel$`Exports of Commoditie# s to the World, (% of GDP)`
#                               + fdi_input_panel$`Current account # balance (% of GDP)`
#                               + fdi_input_panel$fdi_1
#                               + fdi_input_panel$fdi_2,
#                               data = fdi_input_panel, 
#                               model = "random",
#                               effect = "twoway") 
# 
# summary(regfit_optimal2_lag2.5) #r^2 44.48% and adjstr^2 42.98%; # losing significance on gdpgrowth_1; lost significance on central gov # debt
# nobs(regfit_optimal2_lag2.5) #459
# 
# regfit_optimal2_lag2.6 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                               + fdi_input_panel$csdr_avg
#                               + fdi_input_panel$csdr_1
#                               + fdi_input_panel$csdr_2
#                               + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                               + fdi_input_panel$`GDP growth (annual %# )` 
#                               + fdi_input_panel$gdpgrowth_1
#                               + fdi_input_panel$gdpgrowth_2
#                               + fdi_input_panel$gdpgrowth_3
#                               + fdi_input_panel$`Exports of Commoditie# s to the World, (% of GDP)`
#                               + fdi_input_panel$`Current account # balance (% of GDP)`
#                               + fdi_input_panel$fdi_1
#                               + fdi_input_panel$fdi_2,
#                               data = fdi_input_panel, 
#                               model = "random",
#                               effect = "twoway") 
# 
# summary(regfit_optimal2_lag2.6) #r^2 44.69% and adjstr^2 43.07%; # losing significance on gdpgrowth_1; lost significance on central gov # debt
# nobs(regfit_optimal2_lag2.6) #459
# 
# regfit_optimal2_lag2.7 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                               + fdi_input_panel$csdr_avg
#                               + fdi_input_panel$csdr_1
#                               + fdi_input_panel$csdr_2
#                               + fdi_input_panel$csdr_3
#                               + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                               + fdi_input_panel$`GDP growth (annual %# )` 
#                               + fdi_input_panel$gdpgrowth_1
#                               + fdi_input_panel$gdpgrowth_2
#                               + fdi_input_panel$gdpgrowth_3
#                               + fdi_input_panel$`Exports of Commoditie# s to the World, (% of GDP)`
#                               + fdi_input_panel$`Current account # balance (% of GDP)`
#                               + fdi_input_panel$fdi_1
#                               + fdi_input_panel$fdi_2,
#                               data = fdi_input_panel, 
#                               model = "random",
#                               effect = "twoway") 
# 
# summary(regfit_optimal2_lag2.7) #r^2 44.69% and adjstr^2 42.94%; # adjusted r^2 peaked
# nobs(regfit_optimal2_lag2.7) #459
# 
# regfit_optimal2_lag2.8 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                               + fdi_input_panel$cgd_1
#                               + fdi_input_panel$csdr_avg
#                               + fdi_input_panel$csdr_1
#                               + fdi_input_panel$csdr_2
#                               + fdi_input_panel$csdr_3
#                               + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                               + fdi_input_panel$`GDP growth (annual %# )` 
#                               + fdi_input_panel$gdpgrowth_1
#                               + fdi_input_panel$gdpgrowth_2
#                               + fdi_input_panel$gdpgrowth_3
#                               + fdi_input_panel$`Exports of Commoditie# s to the World, (% of GDP)`
#                               + fdi_input_panel$`Current account # balance (% of GDP)`
#                               + fdi_input_panel$fdi_1
#                               + fdi_input_panel$fdi_2,
#                               data = fdi_input_panel, 
#                               model = "random",
#                               effect = "twoway") 
# 
# summary(regfit_optimal2_lag2.8) #r^2 45.01% and adjstr^2 43.07%; # better significance on previously insignificant variables
# nobs(regfit_optimal2_lag2.8) #427
# 
# regfit_optimal2_lag2.9 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                               + fdi_input_panel$cgd_1
#                               + fdi_input_panel$cgd_2
#                               + fdi_input_panel$csdr_avg
#                               + fdi_input_panel$csdr_1
#                               + fdi_input_panel$csdr_2
#                               + fdi_input_panel$csdr_3
#                               + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                               + fdi_input_panel$`GDP growth (annual %# )` 
#                               + fdi_input_panel$gdpgrowth_1
#                               + fdi_input_panel$gdpgrowth_2
#                               + fdi_input_panel$gdpgrowth_3
#                               + fdi_input_panel$`Exports of Commoditie# s to the World, (% of GDP)`
#                               + fdi_input_panel$`Current account # balance (% of GDP)`
#                               + fdi_input_panel$fdi_1
#                               + fdi_input_panel$fdi_2,
#                               data = fdi_input_panel, 
#                               model = "random",
#                               effect = "twoway")
# 
# summary(regfit_optimal2_lag2.9) #r^2 47.63% and adjstr^2 45.49%; # better significance on previously insignificant variables
# nobs(regfit_optimal2_lag2.9) #397
# # Best model so far

# regfit_optimal2_lag2.10 <- plm(fdi_input_panel$`Foreign direct # nvestment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # overnment debt, total (% of GDP)`
#                              + fdi_input_panel$cgd_1
#                              + fdi_input_panel$cgd_2
#                              + fdi_input_panel$cgd_3
#                              + fdi_input_panel$csdr_avg
#                              + fdi_input_panel$csdr_1
#                              + fdi_input_panel$csdr_2
#                              + fdi_input_panel$csdr_3
#                              + fdi_input_panel$`GDP per capita # constant 2010 US$)`
#                              + fdi_input_panel$`GDP growth (annual %# ` 
#                              + fdi_input_panel$gdpgrowth_1
#                              + fdi_input_panel$gdpgrowth_2
#                              + fdi_input_panel$gdpgrowth_3
#                              + fdi_input_panel$`Exports of Commoditie#  to the World, (% of GDP)`
#                              + fdi_input_panel$`Current account # alance (% of GDP)`
#                              + fdi_input_panel$fdi_1
#                              + fdi_input_panel$fdi_2,
#                              data = fdi_input_panel, 
#                              model = "random",
#                              effect = "twoway") 
# 
# summary(regfit_optimal2_lag2.10) #r^2 47.63% and adjstr^2 45.43%; # better significance on previously insignificant variables
# nobs(regfit_optimal2_lag2.10) #397
# Does not run

regfit_optimal2_lag2.11 <- plm(
    `Foreign direct investment, net inflows (% of GDP)`
    ~ `Central government debt, total (% of GDP)`
    + cgd_1
    + cgd_2
    + csdr_avg
    + csdr_1
    + csdr_2
    + csdr_3
    + `GDP per capita (constant 2010 US$)`
    + `GDP growth (annual %)`
    + gdpgrowth_1
    + gdpgrowth_2
    + gdpgrowth_3
    + `Exports of Commodities to the World, (% of GDP)`
    + `Current account balance (% of GDP)`
    + ca_1
    + fdi_1
    + fdi_2,
    data = fdi_input_panel, 
    model = "random",
    effect = "twoway",
    index = c("Country", "Year")) 

summary(regfit_optimal2_lag2.11) #r^2 48.04% and adjstr^2 45.71%; better significance on previously insignificant variables
nobs(regfit_optimal2_lag2.11) #397
# Best model so far
# 
# regfit_optimal2_lag2.11.1 <- plm(
#     `Foreign direct investment, net inflows (% of GDP)`
#     ~ `Central government debt, total (% of GDP)`
#     + cgd_1
#     + cgd_2
#     + csdr_avg
#     + csdr_1
#     + csdr_2
#     + csdr_3
#     + `GDP per capita (constant 2010 US$)`
#     + `GDP growth (annual %)`
#     + gdpgrowth_1
#     + gdpgrowth_2
#     + gdpgrowth_3
#     + `Exports of Commodities to the World, (% of GDP)`
#     + `Current account balance (% of GDP)`
#     + ca_1
#     + fdi_1
#     + fdi_2
#     + `Population in largest city`,
#     data = fdi_input_panel, 
#     model = "random",
#     effect = "twoway",
#     index = c("Country", "Year")) 
# 
# summary(regfit_optimal2_lag2.11.1) #r^2 48.04% and adjstr^2 45.71%; # better significance on previously insignificant variables
# nobs(regfit_optimal2_lag2.11.1) #397
# Could not get this run

# regfit_optimal2_lag2.11.3 <- plm(fdi_input_panel$d1.l.fdi ~ 
#                                  + fdi_input_panel$d2.l.fdi
#                                  + fdi_input_panel$d3.l.fdi,
#                                  data = fdi_input_panel, 
#                                  model = "within",
#                                  effect = "twoway") 
# 
# summary(regfit_optimal2_lag2.11.3) #r^2 29.69% and adjstr^2 10.55%
# nobs(regfit_optimal2_lag2.11.3) #361

# regfit_optimal2_lag2.12 <- plm(fdi_input_panel$Foreign.direct.investme# nt..net.inflows....of.GDP. ~ fdi_input_panel$Central.government.debt# ..total....of.GDP.
#                                + fdi_input_panel$cgd_1
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$csdr_1
#                                + fdi_input_panel$csdr_2
#                                + fdi_input_panel$csdr_3
#                                + fdi_input_panel$GDP.per.capita# ..constant.2010.US..
#                                + fdi_input_panel$GDP.growth..annual...
#                                + fdi_input_panel$gdpgrowth_1
#                                + fdi_input_panel$gdpgrowth_2
#                                + fdi_input_panel$gdpgrowth_3
#                                + fdi_input_panel$Exports.of.Commoditie# s.to.the.World..US.
#                                + fdi_input_panel$Current.account# .balance....of.GDP.
#                                + fdi_input_panel$ca_1
#                                + fdi_input_panel$ca_2
#                                + fdi_input_panel$fdi_1
#                                + fdi_input_panel$fdi_2,
#                                data = fdi_input_panel, 
#                                model = "random",
#                                effect = "twoway",
#                                index = c("Country", "Year")) 
# 
# summary(regfit_optimal2_lag2.12) #r^2 45.99% and adjstr^2 43.75%; # peaked
# nobs(regfit_optimal2_lag2.12) #427

# regfit_optimal2_lag2.13 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$cgd_1
#                                + fdi_input_panel$cgd_2
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$csdr_1
#                                + fdi_input_panel$csdr_2
#                                + fdi_input_panel$csdr_3
#                                + fdi_input_panel$polity2
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$gdpgrowth_1
#                                + fdi_input_panel$gdpgrowth_2
#                                + fdi_input_panel$gdpgrowth_3
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                + fdi_input_panel$`Current account # balance (% of GDP)`
#                                + fdi_input_panel$ca_1
#                                + fdi_input_panel$fdi_1
#                                + fdi_input_panel$fdi_2,
#                                data = fdi_input_panel, 
#                                model = "random",
#                                effect = "twoway") 
# 
# summary(regfit_optimal2_lag2.13) #r^2 48.04% and adjstr^2 45.71%; # better significance on previously insignificant variables
# nobs(regfit_optimal2_lag2.13) #397
# # Can't get to run with governance: wgi or polity2


# Test for Heteroskedasticity (Correllated Errors) to know if need to cluster standard errors for Regfit_Optimal
#https://rpubs.com/cyobero/187387

fdi_data <- data.frame(matrix(0, ncol = 1, nrow = 397))
fdi_data$residual <- regfit_optimal2_lag2.11$residuals
fdi_data$predict <- predict(regfit_optimal2_lag2.11)

ggplot(data = fdi_data, aes(y = fdi_data$residual, x =fdi_data$predict )) + geom_point(col = "red") + geom_abline(slope = 0)
#It appears there might be a pattern to the standard errors...

jpeg("Plots/Heteroskedasticity/Heteroskedasticity Chart -- Regfit_Optimal.jpg", width = 1500, height = 1500)
ggplot(data = fdi_data, aes(y = fdi_data$residual, x =fdi_data$predict )) + geom_point(col = "red") + geom_abline(slope = 0)
dev.off()

# Run Breusch-Pagan Test

bptest(regfit_optimal2_lag2.11)
# p-value of 5.319e-06
# Very clearly NO heteroskedasticity!!!

# Create Histogram of Residuals

jpeg("Plots/Heteroskedasticity/Histogram of Residuals.jpg", width = 1500, height = 1500)
hist(regfit_optimal2_lag2.11$residuals)
dev.off()
# Seems ok, centered on zero

# Cluster Standard Errors to See If Coefficients are Still Significant

coeftest(regfit_optimal, vcov=vcovHC(regfit_optimal, type = "HC0", cluster = "group"))
# This is not necessary -- no heteroskedasiticy

## Fixed or Random Effects? Hausman Test

# Run Standard (LM) and Fixed Effects (FE) Models

regfit_optimal2_lag2.11_lm <- lm(
    `Foreign direct investment, net inflows (% of GDP)`
    ~ `Central government debt, total (% of GDP)`
    + cgd_1
    + cgd_2
    + csdr_avg
    + csdr_1
    + csdr_2
    + csdr_3
    + `GDP per capita (constant 2010 US$)`
    + `GDP growth (annual %)`
    + gdpgrowth_1
    + gdpgrowth_2
    + gdpgrowth_3
    + `Exports of Commodities to the World, (% of GDP)`
    + `Current account balance (% of GDP)`
    + ca_1
    + fdi_1
    + fdi_2,
    data = fdi_input_panel) 


summary(regfit_optimal2_lag2.11_lm) #r^2 48.05% and adjstr^2 45.72%
nobs(regfit_optimal2_lag2.11_lm) #397

regfit_optimal2_lag2.11_fe <- plm(
    `Foreign direct investment, net inflows (% of GDP)`
    ~ `Central government debt, total (% of GDP)`
    + cgd_1
    + cgd_2
    + csdr_avg
    + csdr_1
    + csdr_2
    + csdr_3
    + `GDP per capita (constant 2010 US$)`
    + `GDP growth (annual %)`
    + gdpgrowth_1
    + gdpgrowth_2
    + gdpgrowth_3
    + `Exports of Commodities to the World, (% of GDP)`
    + `Current account balance (% of GDP)`
    + ca_1
    + fdi_1
    + fdi_2,
    data = fdi_input_panel, 
    model = "within",
    effect = "twoway",
    index = c("Country", "Year")) 

summary(regfit_optimal2_lag2.11_fe) #r^2 12.13% and adjstr^2 -0.09%
nobs(regfit_optimal2_lag2.11_fe) #397

# Graph Fixed Effects and OLS

# yhat_lm <- as.data.frame(regfit_optimal2_lag2.11_lm$fitted)
# yhat_fe <- regfit_optimal2_lag2.11_fe$fitted
# 
# y_lm <- regfit_optimal2_lag2.11_lm$model[,"fdi_input_panel$`Foreign # direct investment, net inflows (% of GDP)`"]
# 
# y_lm <- as.data.frame(regfit_optimal2_lag2.11_lm$model[1])
# attributes(y_lm)
# y_lm$row.names
# attr(y_lm, "row.names") <- NULL
# attributes(y_lm)
# 
# 
# 
# # y_lm[,0] <- NULL
# # row.names(y_lm) <- NULL
# # y_lm$Country
# 
# 
# plot(yhat_lm, y_lm2, pch = 19, xlab = "Fitted OLS FDI", ylab = # "Predicted OLS FDI")
# abline(regfit_optimal2_lag2.11_lm)

## Fixed Effects or Random Effects
#https://www.princeton.edu/~otorres/Panel101R.pdf

# Hausman Test

phtest(regfit_optimal2_fe, regfit_optimal2)
# This test says use FE
# P-Value 0.0003255

# Breusch-Pagan Langrange Multiplier for Random Effects

plmtest(regfit_optimal2, type = c("bp"))
# Rejected the null,  Random Effects is Appropriate!!
# P-Value 0.002753

plmtest(regfit_optimal3, type = c("bp"))
# Rejected the null,  Random Effects is Appropriate!!
# P-Value 0.002753

# These tests show conflicting results: Hausman test says use FE, LM test says use Random Effects. Not sure what to do in this

## Testing for Regional Effects

# Contrasts for Regional

# contrasts(fdi_input_panel$Region)
# contrasts(fdi_input_panel$Country)
# contrasts(fdi_input_panel$`Lending Category`)
# contrasts(fdi_input_panel$`Income Group`)
# contrasts(fdi_input_panel$Other)
# 
# 
# # Regressions for Regional 
# 
# regfit_optimal_regional <- lm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                       + fdi_input_panel$csdr_avg
#                       + fdi_input_panel$`GDP per capita (constant 2010 # US$)`
#                       + fdi_input_panel$`GDP growth (annual %)` 
#                       + fdi_input_panel$`Exports of Commodities to the # World, (% of GDP)`
#                       + fdi_input_panel$`Current account balance (% of # GDP)`
#                       + fdi_input_panel$Region,
#                        data = fdi_input_panel) 
#  
#  summary(regfit_optimal_regional) #r^2 34.32 and adjstr^2 32.88%
#  nobs(regfit_optimal_regional) #516
#  
# regfit_optimal_regional2 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                + fdi_input_panel$`Current account # balance (% of GDP)`
#                                + fdi_input_panel$Region,
#                                data = fdi_input_panel,
#                                model = "random",
#                                effect = "twoways") 
#  
#  summary(regfit_optimal_regional2) #r^2 33.5% and adjstr^2 32.1%
#  nobs(regfit_optimal_regional2) #516
#  
# regfit_optimal_regional3.1 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ #fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                  + fdi_input_panel$csdr_avg
#                                  + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                  + fdi_input_panel$`GDP growth (annual # %)` 
#                                  + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                  + fdi_input_panel$`Current account # balance (% of GDP)`
#                                  + (fdi_input_panel$Region*fdi_input_p# anel$`Central government debt, total (% of GDP)`),
#                                  data = fdi_input_panel,
#                                  model = "random",
#                                  effect = "twoways") 
#  
#  summary(regfit_optimal_regional3.1) #r^2 33.5% and adjstr^2 32.1%
#  nobs(regfit_optimal_regional3.1) #516
#  
# regfit_optimal_regional3.2 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                  #+ fdi_input_panel$csdr_avg
#                                  + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                  + fdi_input_panel$`GDP growth (annual # %)` 
#                                  + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                  + fdi_input_panel$`Current account # balance (% of GDP)`
#                                  + (fdi_input_panel$Region*fdi_input_p# anel$csdr_avg),
#                                  data = fdi_input_panel,
#                                 model = "random",
#                                 effect = "twoways") 
# 
#  summary(regfit_optimal_regional3.2) #r^2 33.5% and adjstr^2 32.1%
#  nobs(regfit_optimal_regional3.2) #516
#  
# regfit_optimal_regional3.3 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                    + fdi_input_panel$csdr_avg
#                                    #+ fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                    + fdi_input_panel$`GDP growth # (annual %)` 
#                                    + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                    + fdi_input_panel$`Current account # balance (% of GDP)`
#                                    + (fdi_input_panel$Region*fdi_input# _panel$`GDP per capita (constant 2010 US$)`),
#                                    data = fdi_input_panel,
#                                    model = "random",
#                                    effect = "twoways") 
#  
# summary(regfit_optimal_regional3.3) #r^2 33.5% and adjstr^2 32.1%
# nobs(regfit_optimal_regional3.3) #516
#  
# regfit_optimal_regional3.4 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                    + fdi_input_panel$csdr_avg
#                                    + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                    #+ fdi_input_panel$`GDP growth # (annual %)` 
#                                    + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                    + fdi_input_panel$`Current account # balance (% of GDP)`
#                                   + (fdi_input_panel$Region*fdi_input_# panel$`GDP growth (annual %)`)
#                                  ,
#                                    data = fdi_input_panel,
#                                    model = "random",
#                                    effect = "twoways") 
#  
# summary(regfit_optimal_regional3.4) #r^2 33.5% and adjstr^2 32.1%
# nobs(regfit_optimal_regional3.4) #516 
# 
# # regfit_optimal_regional3.5 <- plm(fdi_input_panel$`Foreign direct # # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # # government debt, total (% of GDP)`
# #                                   + fdi_input_panel$csdr_avg
# #                                   + fdi_input_panel$`GDP per capita # # (constant 2010 US$)`
# #                                   + fdi_input_panel$`GDP growth # # (annual %)` 
# #                                   #+ fdi_input_panel$`Exports of # # Commodities to the World, (% of GDP)`
# #                                   + fdi_input_panel$`Current account # # balance (% of GDP)`
# #                                   + (fdi_input_panel$Region*fdi_inpu# t_# panel$`Exports of Commodities to the World, (% of GDP)`)
# #                                   ,
# #                                   data = fdi_input_panel,
# #                                   model = "random",
# #                                   effect = "twoways") 
# # 
# # summary(regfit_optimal_regional3.5) #r^2 33.5% and adjstr^2 32.1%
# # nobs(regfit_optimal_regional3.5) #516 
# ## Does not work: singular
# 
# regfit_optimal_regional3.6 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                   + fdi_input_panel$csdr_avg
#                                   + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                   + fdi_input_panel$`GDP growth # (annual %)` 
#                                   + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                   #+ fdi_input_panel$`Current account # balance (% of GDP)`
#                                   + (fdi_input_panel$Region*fdi_input_# panel$`Current account balance (% of GDP)`)
#                                   ,
#                                   data = fdi_input_panel,
#                                   model = "random",
#                                   effect = "twoways") 
# 
# summary(regfit_optimal_regional3.6) #r^2 33.5% and adjstr^2 32.1%
# nobs(regfit_optimal_regional3.6) #516 

## Building Up Optimal Model

# regfit_opt1 <- lm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, total # (% of GDP)`
#                       + fdi_input_panel$csdr_avg
#                       + fdi_input_panel$`GDP per capita (constant 2010 # US$)`
#                       + fdi_input_panel$`GDP growth (annual %)` 
#                       ,
#                       data = fdi_input_panel) 
# 
# summary(regfit_opt1) #r^2 6.5 and adjstr^2 5.9%
# nobs(regfit_opt1) #690
# 
# regfit_opt2 <- lm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, total # (% of GDP)`
#                   + fdi_input_panel$csdr_avg
#                   + fdi_input_panel$`GDP per capita (constant 2010 US$# )`
#                   + fdi_input_panel$`GDP growth (annual %)` 
#                   + fdi_input_panel$`Exports of Commodities to the # World, (% of GDP)`
#                   ,
#                   data = fdi_input_panel) 
# 
# summary(regfit_opt2) #r^2 25.55 and adjstr^2 24.83%
# nobs(regfit_opt2) #523
# 
# regfit_opt3 <- lm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, total # (% of GDP)`
#                   + fdi_input_panel$csdr_avg
#                   + fdi_input_panel$`GDP per capita (constant 2010 US$# )`
#                   + fdi_input_panel$`GDP growth (annual %)` 
#                   + fdi_input_panel$`Exports of Commodities to the # World, (% of GDP)`
#                   + fdi_input_panel$`Current account balance (% of GDP# )`
#                     ,
#                   data = fdi_input_panel) 
# 
# summary(regfit_opt3) #r^2 28.86 and adjstr^2 28.02%
# nobs(regfit_opt3) #516
# 
# regfit_opt4 <- lm(fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, total # (% of GDP)`
#                   + fdi_input_panel$csdr_avg
#                   + fdi_input_panel$`GDP per capita (constant 2010 US$# )`
#                   + fdi_input_panel$`GDP growth (annual %)` 
#                   + fdi_input_panel$`Exports of Commodities to the # World, (% of GDP)`
#                   + fdi_input_panel$`Current account balance (% of GDP# )`
#                   + fdi_input_panel$`Quality of port infrastructure, # WEF (1=extremely underdeveloped to 7=well developed and efficient by # international standards)`
#                     ,
#                   data = fdi_input_panel) 
# 
# summary(regfit_opt4) #r^2 31.75 and adjstr^2 30.16%
# nobs(regfit_opt4) #307
# 
# ## Regression in FCS / IDA
# 
# # Subset Data
# 
# regfit_optimal2_ida <- lm(fdi_input_panel$`Foreign direct investment, # net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                        + fdi_input_panel$csdr_avg
#                        + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                        + fdi_input_panel$`GDP growth (annual %)` 
#                        + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`
#                        + fdi_input_panel$`Current account balance (% # of GDP)`
#                        + fdi_input_panel$`Quality of port infrastructu# re, WEF (1=extremely underdeveloped to 7=well developed and efficient # by international standards)`
#                        + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate`,
#                        data = subset(fdi_input_panel, fdi_input_panel$# `Lending Category` == "IDA")) 
# 
# summary(regfit_optimal2_ida) #r^2 31.39% and adjstr^2 29.54%
# nobs(regfit_optimal2_ida) #307
# # PLM does not work
# 
# # Interaction Terms
# 
# # regfit_optimal_incomegroup1 <- regfit_optimal_regional3.1 <- plm# # (fdi_input_panel$`Foreign direct investment, net inflows (% of GDP)` ~ # # #fdi_input_panel$`Central government debt, total (% of GDP)`
# #                             + fdi_input_panel$csdr_avg
# #                             + fdi_input_panel$`GDP per capita # # (constant 2010 US$)`
# #                             + fdi_input_panel$`GDP growth (annual %# )` 
# #                             + fdi_input_panel$`Exports of Commoditie# s # to the World, (% of GDP)`
# #                             + fdi_input_panel$`Current account # balance # (% of GDP)`
# #                             + (fdi_input_panel$`Income Group`# *fdi_inpu# t_panel$`Central government debt, total (% of GDP)`),
# #                             data = fdi_input_panel,
# #                             model = "random",
# #                             effect = "twoways") 
# #                                   
# # summary(regfit_optimal_incomegroup1) #r^2 33.5% and adjstr^2 32.1%
# # nobs(regfit_optimal_incomegroup1) #516
#                                   
# # regfit_optimal_incomegroup2 <- regfit_optimal_regional3.1 <- plm# # (fdi_input_panel$`Foreign direct investment, net inflows (% of GDP)` ~ # # fdi_input_panel$`Central government debt, total (% of GDP)`
# #                                                                    + # # #fdi_input_panel$csdr_avg
# #                                                                  + # # fdi_input_panel$`GDP per capita (constant 2010 US$)`
# #                                                                  + # # fdi_input_panel$`GDP growth (annual %)` 
# #                                                                  + # # fdi_input_panel$`Exports of Commodities to the World, (% of GDP)`
# #                                                                  + # # fdi_input_panel$`Current account balance (% of GDP)`
# #                                                                  + # # (fdi_input_panel$`Income Group`*fdi_input_panel$csdr_avg),
# #                                                                  # data # = fdi_input_panel,
# #                                                                  # model # = "random",
# #                                                                  # # effect = "twoways") 
# # 
# # summary(regfit_optimal_incomegroup2) #r^2 33.5% and adjstr^2 32.1%
# # nobs(regfit_optimal_incomegroup2) #516   
# # 
# # regfit_optimal_incomegroup3 <- regfit_optimal_regional3.1 <- plm# # (fdi_input_panel$`Foreign direct investment, net inflows (% of GDP)` ~ # # fdi_input_panel$`Central government debt, total (% of GDP)`
# #                                                                  + # # fdi_input_panel$csdr_avg
# #                                                                    + # # #fdi_input_panel$`GDP per capita (constant 2010 US$)`
# #                                                                  + # # fdi_input_panel$`GDP growth (annual %)` 
# #                                                                  + # # fdi_input_panel$`Exports of Commodities to the World, (% of GDP)`
# #                                                                  + # # fdi_input_panel$`Current account balance (% of GDP)`
# #                                                                  + # # (fdi_input_panel$`Income Group`*fdi_input_panel$`GDP per capita # # (constant 2010 US$)`),
# #                                                                  # data # = fdi_input_panel,
# #                                                                  # model # = "random",
# #                                                                  # # effect = "twoways") 
# 
# # Lending Category
# 
# regfit_optimal_lending1 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ #fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                                                  + # fdi_input_panel$csdr_avg
#                                                                    + # fdi_input_panel$`GDP per capita (constant 2010 US$)`
#                                                                  + # fdi_input_panel$`GDP growth (annual %)` 
#                                                                  + # fdi_input_panel$`Exports of Commodities to the World, (% of GDP)`
#                                                                  + # fdi_input_panel$`Current account balance (% of GDP)`
#                                  + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                +fdi_input_panel$`Price level ratio of # PPP conversion factor (GDP) to market exchange rate` +
#                               (fdi_input_panel$`Lending Category`# *fdi_input_panel$`Central government debt, total (% of GDP)`),
#                                                                  data # = fdi_input_panel,
#                                                                  model # = "random",
#                                                                  # effect = "twoways") 
# 
# summary(regfit_optimal_lending1) #r^2 16.59% and adjstr^2 13.74%
# nobs(regfit_optimal_lending1) #334
# 
# regfit_optimal_lending2 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                # + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                + fdi_input_panel$`Current account # balance (% of GDP)`
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                +fdi_input_panel$`Price level ratio of # PPP conversion factor (GDP) to market exchange rate`
#                                + (fdi_input_panel$`Lending Category`# *fdi_input_panel$csdr_avg),
#                                data = fdi_input_panel,
#                                model = "random",
#                                effect = "twoways") 
# 
# summary(regfit_optimal_lending2) #r^2 17.72% and adjstr^2 14.91%
# nobs(regfit_optimal_lending2) #334
# 
# regfit_optimal_lending3 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$csdr_avg
#                                #+ fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                + fdi_input_panel$`Current account # balance (% of GDP)`
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                +fdi_input_panel$`Price level ratio of # PPP conversion factor (GDP) to market exchange rate`
#                                + (fdi_input_panel$`Lending Category`# *fdi_input_panel$`GDP per capita (constant 2010 US$)`),
#                                data = fdi_input_panel,
#                                model = "random",
#                                effect = "twoways") 
# 
# summary(regfit_optimal_lending3) #r^2 17.39% and adjstr^2 14.56%
# nobs(regfit_optimal_lending3) #334
# 
# regfit_optimal_lending4 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                #+ fdi_input_panel$`GDP growth (annual # %)` 
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                + fdi_input_panel$`Current account # balance (% of GDP)`
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                +fdi_input_panel$`Price level ratio of # PPP conversion factor (GDP) to market exchange rate`
#                                + (fdi_input_panel$`Lending Category`# *fdi_input_panel$`GDP growth (annual %)`),
#                                data = fdi_input_panel,
#                                model = "random",
#                                effect = "twoways") 
# 
# summary(regfit_optimal_lending4) #r^2 17.39% and adjstr^2 14.56%
# nobs(regfit_optimal_lending4) #334
# 
# regfit_optimal_lending5 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                #+ fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                + fdi_input_panel$`Current account # balance (% of GDP)`
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                +fdi_input_panel$`Price level ratio of # PPP conversion factor (GDP) to market exchange rate`
#                                + (fdi_input_panel$`Lending Category` * # fdi_input_panel$`Exports of Commodities to the World, (% of GDP)`),
#                                data = fdi_input_panel,
#                                model = "random",
#                                effect = "twoways") 
# 
# summary(regfit_optimal_lending5) #r^2 17.07% and adjstr^2 14.23%
# nobs(regfit_optimal_lending5) #334
# 
# regfit_optimal_lending6 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                #+ fdi_input_panel$`Current account # balance (% of GDP)`
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                +fdi_input_panel$`Price level ratio of # PPP conversion factor (GDP) to market exchange rate`
#                                + (fdi_input_panel$`Lending Category` * # fdi_input_panel$`Current account balance (% of GDP)`),
#                                data = fdi_input_panel,
#                                model = "random",
#                                effect = "twoways") 
# 
# summary(regfit_optimal_lending6) #r^2 17.63% and adjstr^2 14.82%
# nobs(regfit_optimal_lending6) #334
# 
# regfit_optimal_lending7 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                + fdi_input_panel$`Current account # balance (% of GDP)`
#                                #+ fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                +fdi_input_panel$`Price level ratio of # PPP conversion factor (GDP) to market exchange rate`
#                                + (fdi_input_panel$`Lending Category` * # fdi_input_panel$`Exports of Commodities to the World, (% of GDP)`),
#                                data = fdi_input_panel,
#                                model = "random",
#                                effect = "twoways") 
# 
# summary(regfit_optimal_lending7) #r^2 17.07% and adjstr^2 14.24%
# nobs(regfit_optimal_lending7) #334
# 
# regfit_optimal_lending8 <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                + fdi_input_panel$csdr_avg
#                                + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                + fdi_input_panel$`GDP growth (annual %# )` 
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                + fdi_input_panel$`Current account # balance (% of GDP)`
#                                + fdi_input_panel$`Exports of Commoditi# es to the World, (% of GDP)`
#                                #+fdi_input_panel$`Price level ratio of # PPP conversion factor (GDP) to market exchange rate`
#                                + (fdi_input_panel$`Lending Category` * # fdi_input_panel$`Price level ratio of PPP conversion factor (GDP) to # market exchange rate`),
#                                data = fdi_input_panel,
#                                model = "random",
#                                effect = "twoways") 
# 
# summary(regfit_optimal_lending8) #r^2 16.66% and adjstr^2 13.81%
# nobs(regfit_optimal_lending8) #334

# ## Test for Endogeneity: IV Regression
# # Potentially two endogenous variables: gdp growth and gdp per capita
# # Strategy 1 to address gdp per capita: test with geography data
# # https://bookdown.org/ccolonescu/RPoE4/random-regressors.html
# 
# # 1. GDP per Capita
# 
# # Relevance: Regressions between GDP per capita and Variables
# 
# lm_relevance_gdpcap1 <- lm(fdi_input_panel$`GDP per capita (constant # 2010 US$)` ~ fdi_input_panel$areakm2)
# 
# summary(lm_relevance_gdpcap1) #f-statistic 71.52 and highest significa# nce
# nobs(lm_relevance_gdpcap1) #5975
# 
# lm_relevance_gdpcap2 <- lm(fdi_input_panel$`GDP per capita (constant # 2010 US$)` ~ fdi_input_panel$cen_lat)
# 
# summary(lm_relevance_gdpcap2) #f-statistic 1564, highest significance, # and large practical magnitude; R^2 20%
# nobs(lm_relevance_gdpcap2) #5975
# 
# lm_relevance_gdpcap3 <- lm(fdi_input_panel$`GDP per capita (constant # 2010 US$)` ~ fdi_input_panel$cen_lon)
# 
# summary(lm_relevance_gdpcap3) #f-statistic 0.5761 --> fails
# nobs(lm_relevance_gdpcap3) #5975
# # This is the worst relationship I've ever seen
# 
# lm_relevance_gdpcap4 <- lm(fdi_input_panel$`GDP per capita (constant # 2010 US$)` ~ fdi_input_panel$pdenpavg)
# 
# summary(lm_relevance_gdpcap4) #f-statistic 83.61 -
# nobs(lm_relevance_gdpcap4) #5975
# 
# # Test Exogeneity
# 
# lm_exogeneity_gdpcap1 <- lm(fdi_input_panel$`Foreign direct investment# , net inflows (% of GDP)` ~ fdi_input_panel$areakm2)
# 
# summary(lm_exogeneity_gdpcap1) #definitely correlated, highest # significance
# nobs(lm_exogeneity_gdpcap1) #5382
# 
# lm_exogeneity_gdpcap2 <- lm(fdi_input_panel$`Foreign direct investment# , net inflows (% of GDP)` ~ fdi_input_panel$cen_lat)
# 
# summary(lm_exogeneity_gdpcap2) #definitely correlated, highest # significance
# nobs(lm_exogeneity_gdpcap2) #5382
# 
# lm_exogeneity_gdpcap4 <- lm(fdi_input_panel$`Foreign direct investment# , net inflows (% of GDP)` ~ fdi_input_panel$pdenpavg)
# 
# summary(lm_exogeneity_gdpcap4) #definitely correlated, highest # significance
# nobs(lm_exogeneity_gdpcap4) #5382
# 
# # Instrumental Variable Using LM
# 
# regfit_optimal2
# regfit_optimal_lm
# 
# regfit_gdpcap_IV <- ivreg(fdi_input_panel$`Foreign direct investment, # net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                          + fdi_input_panel$csdr_avg
#                          + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                           + fdi_input_panel$`GDP growth (annual %)` 
#                           + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`
#                           + fdi_input_panel$`Current account balance # (% of GDP)`
#                           + fdi_input_panel$`Quality of port infrastru# cture, WEF (1=extremely underdeveloped to 7=well developed and # efficient by international standards)`
#                           + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate` | 
#                             fdi_input_panel$`Central government debt, # total (% of GDP)`
#                           + fdi_input_panel$csdr_avg
#                           + fdi_input_panel$cen_lat
#                           + fdi_input_panel$areakm2
#                           + fdi_input_panel$`GDP growth (annual %)`
#                           + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`
#                           + fdi_input_panel$`Current account balance # (% of GDP)`
#                           + fdi_input_panel$`Quality of port infrastru# cture, WEF (1=extremely underdeveloped to 7=well developed and # efficient by international standards)`
#                           + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate`,
#                           data = fdi_input_panel)
# 
# summary(regfit_gdpcap_IV, diagnostics = TRUE)
# # Couple of problems here:
# # 1. Wu-Hausman test:GDP per capita is NOT endogenous (does not clear # critical boundary i.e. p-value is 0.2)
# # 2. Sargon Test: Instruents are NOT valid (uncorrelated with the # error term)
# 
# # Instrumental Variable Using PLM with IVs
# 
# # regfit_optimal2_IV_PLM <- plm(fdi_input_panel$`Foreign direct # # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # # government debt, total (% of GDP)`
# #                           + fdi_input_panel$csdr_avg
# #                           + fdi_input_panel$`GDP per capita # (constant # 2010 US$)`
# #                           + fdi_input_panel$`GDP growth (annual %)` 
# #                           + fdi_input_panel$`Exports of Commodities # to # the World, (% of GDP)`
# #                           + fdi_input_panel$`Current account balance # # (% of GDP)`
# #                           + fdi_input_panel$`Quality of port # infrastru# cture, WEF (1=extremely underdeveloped to 7=well developed # and # efficient by international standards)`
# #                           + fdi_input_panel$`Price level ratio of # PPP # conversion factor (GDP) to market exchange rate` | 
# #                             fdi_input_panel$`Central government debt# , # total (% of GDP)`
# #                           + fdi_input_panel$csdr_avg
# #                           + fdi_input_panel$cen_lat
# #                           + fdi_input_panel$areakm2
# #                           + fdi_input_panel$`GDP growth (annual %)`
# #                           + fdi_input_panel$`Exports of Commodities # to # the World, (% of GDP)`
# #                           + fdi_input_panel$`Current account balance # # (% of GDP)`
# #                           + fdi_input_panel$`Quality of port # infrastru# cture, WEF (1=extremely underdeveloped to 7=well developed # and # efficient by international standards)`
# #                           + fdi_input_panel$`Price level ratio of # PPP # conversion factor (GDP) to market exchange rate`,
# #                           data = fdi_input_panel, 
# #                           model = "random")
# # 
# # summary(regfit_optimal2_IV_PLM)
# 
# 
# # Instrumental Variable with Hausman-Taylor Estimator
# 
# regfit_optimal2_HT <- plm(fdi_input_panel$`Foreign direct investment, # net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                              + fdi_input_panel$csdr_avg
#                              + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                              + fdi_input_panel$`GDP growth (annual %)` # 
#                              + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`
#                              + fdi_input_panel$`Current account # balance (% of GDP)`
#                              + fdi_input_panel$`Quality of port # infrastructure, WEF (1=extremely underdeveloped to 7=well developed # and efficient by international standards)`
#                              + fdi_input_panel$`Price level ratio of # PPP conversion factor (GDP) to market exchange rate` | 
#                           + fdi_input_panel$csdr_avg
#                           + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`
#                           + fdi_input_panel$`Current account balance # (% of GDP)`
#                           + fdi_input_panel$`Quality of port infrastru# cture, WEF (1=extremely underdeveloped to 7=well developed and # efficient by international standards)`
#                           + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate`,
#                              data = fdi_input_panel, 
#                              model = "ht",
#                              effect = "twoway")
# 
# summary(regfit_optimal2_HT, diagnostics = TRUE)
# 
# # 2. GDP growth
# 
# # LM IV Test 
# 
# regfit_gdpgrowth_IV <- ivreg(fdi_input_panel$`Foreign direct investmen# t, net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                           + fdi_input_panel$csdr_avg
#                           + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                           + fdi_input_panel$`GDP growth (annual %)` 
#                           + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`
#                           + fdi_input_panel$`Current account balance # (% of GDP)`
#                           + fdi_input_panel$`Quality of port infrastru# cture, WEF (1=extremely underdeveloped to 7=well developed and # efficient by international standards)`
#                           + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate` | 
#                             fdi_input_panel$`Central government debt, # total (% of GDP)`
#                           + fdi_input_panel$csdr_avg
#                           + fdi_input_panel$cen_lat
#                           + fdi_input_panel$areakm2
#                           + fdi_input_panel$`GDP per capita (constant # 2010 US$)`
#                           + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`
#                           + fdi_input_panel$`Current account balance # (% of GDP)`
#                           + fdi_input_panel$`Quality of port infrastru# cture, WEF (1=extremely underdeveloped to 7=well developed and # efficient by international standards)`
#                           + fdi_input_panel$`Price level ratio of PPP # conversion factor (GDP) to market exchange rate`,
#                           data = fdi_input_panel)
# 
# summary(regfit_gdpgrowth_IV, diagnostics = TRUE)
# 
# # GDP growth IS endogenous
# # Sargan passes showing that these instruments are valide
# # DOES NOT pass weak instruments: need a new instrument for growth
# 
# # Relevance
# 
# lm_relevance_gdpgrowth1 <- lm(fdi_input_panel$`GDP growth (annual %)` # ~ fdi_input_panel$pdenpavg)
# 
# summary(lm_relevance_gdpgrowth1) #No
# 
# lm_relevance_gdpgrowth2 <- lm(fdi_input_panel$`GDP growth (annual %)` # ~ fdi_input_panel$tropicar)
# 
# summary(lm_relevance_gdpgrowth2) #No, f stat of 5.3
# 
# lm_relevance_gdpgrowth3 <- lm(fdi_input_panel$`GDP growth (annual %)` # ~ fdi_input_panel$troppop)
# 
# summary(lm_relevance_gdpgrowth3) #No, f stat of 3.5
# 
# lm_relevance_gdpgrowth4 <- lm(fdi_input_panel$`GDP growth (annual %)` # ~ fdi_input_panel$troppop + fdi_input_panel$tropicar)
# 
# summary(lm_relevance_gdpgrowth4) #No, f stat of 5.1
# 
# lm_relevance_gdpgrowth5 <- lm(fdi_input_panel$`GDP growth (annual %)` # ~ fdi_input_panel$pop95)
# 
# summary(lm_relevance_gdpgrowth5) #Yes, f stat of 30
# 
# lm_relevance_gdpgrowth6 <- lm(fdi_input_panel$`GDP growth (annual %)` # ~ fdi_input_panel$pop100cr)
# 
# summary(lm_relevance_gdpgrowth6) #Yes, f stat of 12
# 
# lm_relevance_gdpgrowth7 <- lm(fdi_input_panel$`GDP growth (annual %)` # ~ fdi_input_panel$pop100km)
# 
# summary(lm_relevance_gdpgrowth7) #No
# 
# # Exogeneity
# 
# lm_exogeneity_gdpgrowth5 <- lm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$pop95)
# 
# summary(lm_exogeneity_gdpgrowth5)
# 
# lm_exogeneity_gdpgrowth6 <- lm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$pop100cr)
# 
# summary(lm_exogeneity_gdpgrowth6) #No, f stat of 7
# 
# # LM IV Test for Pop95
# 
# regfit_gdpgrowth_IV_Pop95 <- ivreg(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                              + fdi_input_panel$csdr_avg
#                              + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                              + fdi_input_panel$`GDP growth (annual %)` # 
#                              + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`
#                              + fdi_input_panel$`Current account # balance (% of GDP)`
#                              + fdi_input_panel$`Quality of port # infrastructure, WEF (1=extremely underdeveloped to 7=well developed # and efficient by international standards)`
#                              + fdi_input_panel$`Price level ratio of # PPP conversion factor (GDP) to market exchange rate` | 
#                                fdi_input_panel$`Central government # debt, total (% of GDP)`
#                              + fdi_input_panel$csdr_avg
#                              + fdi_input_panel$pop95
#                              + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                              + fdi_input_panel$`Exports of Commodities # to the World, (% of GDP)`
#                              + fdi_input_panel$`Current account # balance (% of GDP)`
#                              + fdi_input_panel$`Quality of port # infrastructure, WEF (1=extremely underdeveloped to 7=well developed # and efficient by international standards)`
#                              + fdi_input_panel$`Price level ratio of # PPP conversion factor (GDP) to market exchange rate`,
#                              data = fdi_input_panel)
# 
# summary(regfit_gdpgrowth_IV_Pop95, diagnostics = TRUE)
# 
# # Strong instrument
# # Not endogenous
# 
# # LM IV Test for PopCR100
# 
# regfit_gdpgrowth_IV_PopCR100 <- ivreg(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                    + fdi_input_panel$csdr_avg
#                                    + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                    + fdi_input_panel$`GDP growth # (annual %)` 
#                                    + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                    + fdi_input_panel$`Current account # balance (% of GDP)`
#                                    + fdi_input_panel$`Quality of port # infrastructure, WEF (1=extremely underdeveloped to 7=well developed # and efficient by international standards)`
#                                    + fdi_input_panel$`Price level # ratio of PPP conversion factor (GDP) to market exchange rate` | 
#                                      fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                    + fdi_input_panel$csdr_avg
#                                    + fdi_input_panel$pop100cr
#                                    + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                    + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                    + fdi_input_panel$`Current account # balance (% of GDP)`
#                                    + fdi_input_panel$`Quality of port # infrastructure, WEF (1=extremely underdeveloped to 7=well developed # and efficient by international standards)`
#                                    + fdi_input_panel$`Price level # ratio of PPP conversion factor (GDP) to market exchange rate`,
#                                    data = fdi_input_panel)
# 
# summary(regfit_gdpgrowth_IV_PopCR100, diagnostics = TRUE)
# 
# # GDP growth not correlated with the error
# 
# # LM IV Test for PopCR100 and Pop95
# 
# regfit_gdpgrowth_IV_Both <- ivreg(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                       + fdi_input_panel$csdr_avg
#                                       + fdi_input_panel$`GDP per # capita (constant 2010 US$)`
#                                       + fdi_input_panel$`GDP growth # (annual %)` 
#                                       + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                       + fdi_input_panel$`Current # account balance (% of GDP)`
#                                       + fdi_input_panel$`Quality of # port infrastructure, WEF (1=extremely underdeveloped to 7=well # developed and efficient by international standards)`
#                                       + fdi_input_panel$`Price level # ratio of PPP conversion factor (GDP) to market exchange rate` | 
#                                         fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                       + fdi_input_panel$csdr_avg
#                                       + fdi_input_panel$pop100cr
#                                       + fdi_input_panel$pop95
#                                       + fdi_input_panel$`GDP per # capita (constant 2010 US$)`
#                                       + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                       + fdi_input_panel$`Current # account balance (% of GDP)`
#                                       + fdi_input_panel$`Quality of # port infrastructure, WEF (1=extremely underdeveloped to 7=well # developed and efficient by international standards)`
#                                       + fdi_input_panel$`Price level # ratio of PPP conversion factor (GDP) to market exchange rate`,
#                                       data = fdi_input_panel)
# 
# summary(regfit_gdpgrowth_IV_Both, diagnostics = TRUE)

## Ouput Results

# stargazer(regfit_opt1, regfit_opt2, regfit_opt3, regfit_opt4, regfit_optimal_lm, regfit_optimal_fe, regfit_optimal2, regfit_optimal2_ssa, regfit_optimal2.1.2, regfit_optimal3, type = "html", title = "Results", align = TRUE, out="Outputs/FDI Main Regression Results.html")

# stargazer(regfit_optimal2, regfit_optimal2.1,regfit_optimal2.1.1, regfit_optimal2.1.2, regfit_optimal3, type = "html", title = "Results", align = TRUE, out="Outputs/FDI Main Regression Results 2.html")


stargazer(regfit_optimal2_lag2.11, regfit_optimal2_lag2.11_lm,regfit_optimal2_lag2.11_fe, type = "html", title = "Results", align = TRUE, out="Outputs/FDI Main Regression Results 3 DO NOT USE.html")

stargazer(regfit_optimal2, regfit_optimal2_lm,regfit_optimal2_fe, 
          type = "html", 
          title = "Results", 
          align = TRUE, 
          covariate.labels = c("Central Gov. Debt (% of GDP)",
                               "Average Sovereign Credit Ratings",
                               "GDP per Capita (2010 US$)",
                               "GDP Growth (Annual %)",
                               "Exports of Commodities (% of GDP)",
                               "Current Account Balance (% of GDP)",
                               "Port Infrastracture Rating",
                               "Price Level Exchange Rates (PPP)"),
          dep.var.labels = "FDI, net inflows (% of GDP)",
          model.names = FALSE,
          column.labels = c("Random Effects", "OLS", "Fixed Effects"),
          star.char = c( " ", ".", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001, 0.0001),
          notes = c("0 '***' 0.001 '**' '0.01' * '0.05' . 0.1 ' ' 1"),
          notes.label = "R Significance Levels:",
          notes.append = FALSE,
          df = FALSE,
          out="Tables/Final Regression 3 Table/FDI Main Regression Results 3 FINAL.html")

# stargazer(regfit_optimal_regional3.1, regfit_optimal_regional3.2, regfit_optimal_regional3.3, regfit_optimal_regional3.4, regfit_optimal_regional3.6, type = "html", title = "Results", align = TRUE, out="Outputs/FDI Geographical Regression Results.html")

# stargazer(regfit_optimal2, regfit_optimal_lending1, regfit_optimal_lending2, regfit_optimal_lending3, regfit_optimal_lending4, regfit_optimal_lending5, regfit_optimal_lending6, regfit_optimal_lending7, regfit_optimal_lending8, type = "html", title = "Results", align = TRUE, out="Outputs/FDI Lending Category Regression Results.html")

# stargazer(regfit_optimal2_ida, regfit_optimal2_ibrd, regfit_optimal2_noida, type = "html", title = "Results", align = TRUE, out="Outputs/IDA, IBRD or Other.html")

# ## Extra
# 
# regfit_optimal_r2_1_Extra2 <- lm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                 + fdi_input_panel$sp 
#                                 + fdi_input_panel$mdy 
#                                 + fdi_input_panel$fitch
#                                 + fdi_input_panel$csdr_avg
#                                 + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                 + fdi_input_panel$`GDP growth (annual # %)` 
#                                 + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                 + fdi_input_panel$`Current account # balance (% of GDP)`
#                                 + fdi_input_panel$`Reserves and # related items (BoP, current US$)`
#                                 + fdi_input_panel$`Quality of port # infrastructure, WEF (1=extremely underdeveloped to 7=well developed # and efficient by international standards)`,
#                                 data = fdi_input_panel) 
# 
# summary(regfit_optimal_r2_1_Extra2) #r^2 35.51% and adjstr^2 32.82%
# nobs(regfit_optimal_r2_1_Extra2) #276
# 
# regfit_optimal_r2_1_Extra2 <- lm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                  + fdi_input_panel$sp 
#                                  + fdi_input_panel$mdy 
#                                  + fdi_input_panel$fitch
#                                  + fdi_input_panel$csdr_avg
#                                  + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                  + fdi_input_panel$`GDP growth (annual # %)` 
#                                  + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                  + fdi_input_panel$`Current account # balance (% of GDP)`
#                                  + fdi_input_panel$`Reserves and # related items (BoP, current US$)`
#                                  ,
#                                  data = fdi_input_panel) 
# 
# summary(regfit_optimal_r2_1_Extra2) #r^2 35.48% and adjstr^2 33.73%
# nobs(regfit_optimal_r2_1_Extra2) #276
# 
# regfit_optimal_r2_1_Extra_FE <- plm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                 + fdi_input_panel$sp 
#                                 + fdi_input_panel$mdy 
#                                 + fdi_input_panel$fitch
#                                 + fdi_input_panel$csdr_avg
#                                 + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                 + fdi_input_panel$`GDP growth (annual # %)` 
#                                 + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                 + fdi_input_panel$`Current account # balance (% of GDP)`
#                                 +fdi_input_panel$`Reserves and related # items (BoP, current US$)`,
#                                 data = fdi_input_panel, 
#                                 model = "within") 
# 
# summary(regfit_optimal_r2_1_Extra_FE) #r^2 17.05% and adjstr^2 5.31%
# nobs(regfit_optimal_r2_1_Extra_FE) #380
# 
# regfit_optimal_r2_1_Extra_FE_Twoways <- plm(fdi_input_panel$`Foreign # direct investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                                     + fdi_input_panel$sp 
#                                     + fdi_input_panel$mdy 
#                                     + fdi_input_panel$fitch
#                                     + fdi_input_panel$csdr_avg
#                                     + fdi_input_panel$`GDP per capita # (constant 2010 US$)`
#                                     + fdi_input_panel$`GDP growth # (annual %)` 
#                                     + fdi_input_panel$`Exports of # Commodities to the World, (% of GDP)`
#                                     + fdi_input_panel$`Current account # balance (% of GDP)`
#                                     +fdi_input_panel$`Reserves and # related items (BoP, current US$)`,
#                                     data = fdi_input_panel, 
#                                     model = "within", effect = # "twoways") 
# 
# summary(regfit_optimal_r2_1_Extra_FE_Twoways) #r^2 16.35% and adjstr^2 # -0.01%
# nobs(regfit_optimal_r2_1_Extra_FE_Twoways) #380
# 
# # regfit_optimal_r2_1_Extra_RE_Twoways <- plm(fdi_input_panel$`Foreign # # direct investment, net inflows (% of GDP)` ~ fdi_input_panel$`Centra# l # government debt, total (% of GDP)`
# #                                             + fdi_input_panel$sp 
# #                                             + fdi_input_panel$mdy 
# #                                             + fdi_input_panel$fitch
# #                                             + fdi_input_panel$csdr_a# vg
# #                                             + fdi_input_panel$`GDP # per # capita (constant 2010 US$)`
# #                                             + fdi_input_panel$`GDP # # growth (annual %)` 
# #                                             + fdi_input_panel$`Expor# ts # of Commodities to the World, (% of GDP)`
# #                                             + fdi_input_panel$`Curre# nt # account balance (% of GDP)`
# #                                             +fdi_input_panel$`Reserv# es # and related items (BoP, current US$)`,
# #                                             data = fdi_input_panel, 
# #                                             model = "random") 
# # 
# # summary(regfit_optimal_r2_1_Extra_RE_Twoways) #r^2 16.35% and # adjstr^2 # -0.01%
# # nobs(regfit_optimal_r2_1_Extra_RE_Twoways) #380
# 
# # regfit_optimal_r2_1_Extra_RE_Twoways <- plm(fdi_input_panel$`Foreign #  # direct investment, net inflows (% of GDP)` ~ fdi_input_panel$`Centr# al  # government debt, total (% of GDP)`
# #                  + fdi_input_panel$sp 
# #                  + fdi_input_panel$mdy 
# #                  + fdi_input_panel$fitch
# #                  + fdi_input_panel$csdr_avg
# #                  + fdi_input_panel$`GDP per capita (constant 2010 # US$)`
# #                  + fdi_input_panel$`GDP # growth (annual %)` 
# #                  + fdi_input_panel$`Exports of Commodities to the # World# , (% of GDP)`
# #                  + fdi_input_panel$`Currentaccount balance (% of GDP# )`
# #                  ,
# #                  data = fdi_input_panel, 
# #                  model = "random",
# #                  effect = "twoways")
# # 
# # summary(regfit_optimal_r2_1_Extra_RE_Twoways) #r^2 16.35% and # adjstr^2 # # -0.01%
# # nobs(regfit_optimal_r2_1_Extra_RE_Twoways) #380# 
# 
# regfit_best_subsets <- regsubsets(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~ fdi_input_panel$`Central # government debt, total (% of GDP)`
#                + fdi_input_panel$sp 
#                + fdi_input_panel$mdy 
#                + fdi_input_panel$fitch
#                + fdi_input_panel$csdr_avg
#                + fdi_input_panel$`GDP per capita (constant 2010 US$)`
#                + fdi_input_panel$`GDP growth (annual %)` 
#                + fdi_input_panel$`Exports of Commodities to the World, # (% of GDP)`
#                + fdi_input_panel$`Current account balance (% of GDP)`
#                + fdi_input_panel$`Reserves and related items (BoP, # current US$)`,
#                                 data = fdi_input_panel,
#                nvmax = 9)
# 
# summary(regfit_best_subsets)
# 
# regfit_best_summary <- summary(regfit_best_subsets)
# names(regfit_best_summary)
# 
# regfit_best_summary$adjr2
# which.max(regfit_best_summary$adjr2) #7 has the highest r^2
# 
# coef(regfit_best_subsets, 7)
# 
# regfit_optimal_r2_1_Extra2 <- lm(fdi_input_panel$`Foreign direct # investment, net inflows (% of GDP)` ~  
#                   fdi_input_panel$fitch
#                 + fdi_input_panel$csdr_avg
#                 + fdi_input_panel$`GDP per capita (constant 2010 US$)`
#                 + fdi_input_panel$`GDP growth (annual %)` 
#                 + fdi_input_panel$`Exports of Commodities to the World# , (% of GDP)`
#                 + fdi_input_panel$`Current account balance (% of GDP)`
#                 + fdi_input_panel$`Reserves and related items (BoP, # current US$)`,
#                                   data = fdi_input_panel)
# 
# summary(regfit_optimal_r2_1_Extra2)
# 
# regfit_optimal_Extra3 <- plm(fdi_input_panel$`Foreign direct investmen# t, net inflows (% of GDP)` ~ fdi_input_panel$`Central government debt, # total (% of GDP)`
#                        + fdi_input_panel$csdr_avg
#                        + fdi_input_panel$l.gdp_per_capita
#                        + fdi_input_panel$`GDP growth (annual %)` 
#                        + fdi_input_panel$`Exports of Commodities to # the World, (% of GDP)`
#                        + fdi_input_panel$`Current account balance (% # of GDP)`
#                        + fdi_input_panel$l.infrastracture_quality
#                        + fdi_input_panel$l.price_level_PPP,
#                        data = fdi_input_panel, 
#                        model = "random",
#                        effect = "twoway") 
# 
# summary(regfit_optimal_Extra3) #r^2 31.39% and adjstr^2 29.54%
# nobs(regfit_optimal_Extra3) #307
# # This regression is even better: price level is also highly statistic# ally related