####################################################################

## World Bank MIGA
## This  file:
##  - Reads in cleaned training and testing data sets
##  - Performs inferential analysis of governance indicators
##
## Output:
##  - Regression outputs
##  - Scatter plot of analysis
## Author: Jake Schneider
## User: Jake Schneider
##Date Created:  07/03/2019
## Last Modified: 07/03/2019

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

## Setting a working directory 

getwd()

setwd("/Users/jschneids13/Desktop/World Bank - MIGA STT/Projects/Investment Analysis/MIGA-Private-/Full Research - By Country")

## Read in Datasets

load("R Data Sets/FDI Inputs/FDI_Input_Panel.Rdata")
load("R Data Sets/FDI Inputs/FDI_Complete_Cases.Rdata")

## Subset Selection Methods: Best Subset Selection

# CPIA on FDI

grep("cpia", names(fdi_input_panel), value = TRUE) #21 variables with "cpia"; but only 20 unique bc created a composite

regfit_cpia_fdi_subsets <- regsubsets(fdi_input_panel$`Foreign direct investment, net inflows (% of GDP)` ~ 
                         fdi_input_panel$cpia_budgetary_and_financial_management
                       + fdi_input_panel$cpia_building_human_resources
                       + fdi_input_panel$cpia_business_reg_environment
                       + fdi_input_panel$cpia_debt_policy_rating
                       + fdi_input_panel$cpia_economic_management
                       + fdi_input_panel$cpia_efficiency_of_revenue
                       + fdi_input_panel$cpia_environmental_sustainability_rating
                       + fdi_input_panel$cpia_equity_of_public_resources
                       + fdi_input_panel$cpia_financial_sector_rating
                       + fdi_input_panel$cpia_fiscal_policy_rating
                       + fdi_input_panel$cpia_gender_equality_rating
                       + fdi_input_panel$cpia_macroeconomic_management_rating
                       + fdi_input_panel$`cpia_policies_for_social inclusion`
                       + fdi_input_panel$cpia_structural_policies
                       + fdi_input_panel$cpia_trade_rating
                       + fdi_input_panel$cpia_transparency_accountability_corruption
                       , data = fdi_input_panel
                       , nvmax = 20)

summary(regfit_cpia_fdi_subsets)

regfit_cpia_fdi_summary <- summary(regfit_cpia_fdi_subsets)
names(regfit_cpia_fdi_summary)

regfit_cpia_fdi_summary$adjr2

par(mfrow = c(2,2))

jpeg("Plots/Governance/CPIA/CPIA FDI Best Subset Selection -- RSS.jpg", width = 1500, height = 1500)
plot(regfit_cpia_fdi_subsets$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
dev.off()

# jpeg("Plots/Governance/CPIA/Best Subset Selection -- RSQ.jpg", width = # 1500, height = 1500)
# plot(regfit_cpia_fdi_subsets$rsq, xlab = "Number of Variables", ylab = # "RSQ", type = "l")
# dev.off()
# 
# jpeg("Plots/Governance/CPIA/Best Subset Selection -- Adjusted RSQ", # width = 1500, height = 1500)
# plot(regfit_cpia_fdi_subsets$adjr2, xlab = "Number of Variables", ylab # = "Adjusted RSQ", type = "l")
# dev.off()