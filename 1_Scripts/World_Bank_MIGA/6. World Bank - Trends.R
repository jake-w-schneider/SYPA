####################################################################

## World Bank MIGA
## This  file:
##  - Reads in cleaned training and testing data sets
##  - Performs analysis of trends
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
#install.packages("lmtest")
#install.packages("gplots")
#install.packages("tseries")
#install.packages("car")
#install.packages("ggthemes")
#install.packages("plotly")
#install.packages("stargazer")
#install.packages("kableExtra")
#install.packages("urca")
#install.packages("strucchange")
#install.packages("forecast")
#install.packages("FitAR")
#install.packages("fUnitRoots")
#install.packages("rowr")
#install.packages("Hmisc")
#install.packages("ggfan")

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
library(gplots)
library(tseries)
library(car)
library(ggthemes)
library(scales)
library(reshape2)
library(plotly)
library(stargazer)
library(knitr)
library(kableExtra)
library(urca)
library(MASS)
library(strucchange)
library(forecast)
library(FitAR)
library(fUnitRoots)
library(rowr)
library(Hmisc)
library(ggfan)

## Setting a working directory 

getwd()

setwd("/Users/jschneids13/Desktop/World Bank - MIGA STT/Projects/Investment Analysis/MIGA-Private-/Full Research - By Country")

## Read in Datasets

load("R Data Sets/FDI Inputs/FDI_Input_Panel.Rdata")
load("R Data Sets/FDI Inputs/FDI_Complete_Cases.Rdata")
load("R Data Sets/FDI Inputs/FDI_Aggregate_Country.Rdata")
load("R Data Sets/FDI Inputs/FDI_Aggregate_Region.Rdata")
load("R Data Sets/FDI Inputs/FDI_Aggregate_Lending_Category.Rdata")
load("R Data Sets/FDI Inputs/FDI_Aggregate_Income_Group.Rdata")
load("R Data Sets/FDI Inputs/FDI_Aggregate.Rdata")

## Time Series Trends

#Build Function to Return Element Text Object

rotatedAxisElementText = function(angle,position='x'){
  angle     = angle[1]; 
  position  = position[1]
  positions = list(x=0,y=90,top=180,right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (angle - positions[[ position ]])*pi/180
  hjust = 0.5*(1 - sin(rads))
  vjust = 0.5*(1 + cos(rads))
  element_text(angle=angle,vjust=vjust,hjust=hjust)
}

# FDI By Region

fdi_aggregate_region$Year <- as.Date(fdi_aggregate_region$Group.2, "%Y", format = "%Y")

fdi_aggregate_region$Year2 <- as.POSIXct(fdi_aggregate_region$Group.2, format = "%Y")

class(fdi_aggregate_region$Year2)

fdi_aggregate_region$Year3 <- year(as.Date(fdi_aggregate_region$Year, format = "%Y/%M/%D"))

class(fdi_aggregate_region$Year3)

jpeg("Plots/Trend Analysis/Trends -- FDI (Perc. of GDP) by Region.jpg", width = 500, height = 500)
ggplot(fdi_aggregate_region, 
       aes(x = Year2, 
           y = Foreign.direct.investment..net..BoP..current.US..,
           group = Group.1, 
           colour = Group.1)) + 
  geom_line(size=2) +
  facet_wrap( ~  Group.1, ncol=3) +
  scale_y_continuous("FDI, Net Inflows (% of GDP)") +
  scale_x_discrete("Year") +
  scale_x_datetime(breaks = date_breaks("10 years"), 
                   labels = date_format("%Y")) +
  ggtitle("FDI Inflows by Region, 1970 - 2017") +
  theme_tufte() +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5),
        axis.text.x = rotatedAxisElementText(90, "x"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.spacing.x = unit(0.3, "cm")
        )
dev.off()

# FDI By Region 2

# jpeg("Plots/Trend Analysis/Trends -- FDI (Perc. of GDP) by Region, WAvg.jpg", width = 500, height = 500)
# ggplot(fdi_aggregate_region, aes(x = Year2, y = fdi_aggregate_region$`GDP (current US$) (sum)`, group = Group.1, colour = Group.1)) + 
#  geom_line(size=2) +
#  facet_wrap( ~  Group.1, ncol=3) +
#  scale_y_continuous("FDI, Net Inflows (% of GDP)") +
#  scale_x_discrete("Year") +
#  scale_x_datetime(breaks = date_breaks("10 years"), 
#                   labels = date_format("%Y")) +
#  ggtitle("FDI Inflows by Region, 1969 - 2018") +
#  theme_tufte() +
#  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5),
#        axis.text.x = rotatedAxisElementText(90, "x"),
#        axis.title.x = element_blank(),
#        legend.title = element_blank(),
#        legend.position = "bottom",
#        legend.spacing.x = unit(0.3, "cm")
#  )
#dev.off()

# FDI By Region 3

jpeg("Plots/Trend Analysis/Trends -- FDI (Perc. of GDP) by Region, WAvg.jpg", width = 500, height = 500)
ggplot(fdi_aggregate_region, 
       aes(x = Year2, 
           y = `Weighted Average FDI, Net Inflows (%), By Region`,
           group = Group.1, 
           colour = Group.1)) + 
  geom_line(size=2) +
  facet_wrap( ~  Group.1, ncol=3) +
  scale_y_continuous("Weighted Average FDI, Net Inflows (% of GDP)") +
  scale_x_discrete("Year") +
  scale_x_datetime(breaks = date_breaks("10 years"), 
                   labels = date_format("%Y")) +
  ggtitle("Weighted Average FDI Inflows by Region, 1970 - 2017") +
  theme_tufte() +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5),
        axis.text.x = rotatedAxisElementText(90, "x"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.spacing.x = unit(0.3, "cm")
  )
dev.off()

# FDI By Lending Category

fdi_aggregate_lending_category$Year <- as.POSIXct(fdi_aggregate_lending_category$Group.2, format = "%Y")

jpeg("Plots/Trend Analysis/Trends -- FDI (Perc. of GDP) by Lending Category.jpg", width = 500, height = 500)
ggplot(fdi_aggregate_lending_category, 
       aes(x = Year, 
           y = Foreign.direct.investment..net.inflows....of.GDP., group = Group.1, colour = Group.1)) + 
  geom_line(size=2) +
  facet_wrap( ~  Group.1, ncol=1) +
  scale_y_continuous("FDI, Net Inflows (% of GDP)") +
  scale_x_discrete("Year") +
  scale_x_datetime(breaks = date_breaks("3 years"), 
                   labels = date_format("%Y")) +
  ggtitle("FDI Inflows by Lending Category, 1970 - 2017") +
  theme(axis.text.x = rotatedAxisElementText(90, "x")) +
  theme_tufte() +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5),
        axis.text.x = rotatedAxisElementText(90, "x"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1.0, "cm"),
        legend.spacing.x = unit(0.3, "cm"))
dev.off()

# FDI By Lending Category 2

# jpeg("Plots/Trend Analysis/Trends -- FDI (Perc. of GDP) by Lending # Category, WAvg.jpg", width = 500, height = 500)
# ggplot(fdi_aggregate_lending_category, aes(x = fdi_aggregate_lending_c# ategory$Year, y = fdi_aggregate_lending_category$`Weighted Average FDI# , Net Inflows (%), By Lending Category`, group = Group.1, colour = # Group.1)) + 
#   geom_line(size=2) +
#   facet_wrap( ~  Group.1, ncol=1) +
#   scale_y_continuous("Weighted Average FDI, Net Inflows (% of GDP)") +
#   scale_x_discrete("Year") +
#   scale_x_datetime(breaks = date_breaks("3 years"), 
#                    labels = date_format("%Y")) +
#   ggtitle("Weighted Average FDI Inflows by Lending Category, 1969 - # 2018") +
#   theme(axis.text.x = rotatedAxisElementText(90, "x")) +
#   theme_tufte() +
#   theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5# ),
#         axis.text.x = rotatedAxisElementText(90, "x"),
#         axis.title.x = element_blank(),
#         legend.title = element_blank(),
#         legend.position = "bottom",
#         legend.key.size = unit(1.0, "cm"),
#         legend.spacing.x = unit(0.3, "cm"))
# dev.off()

# FDI By Income Group

fdi_aggregate_income_group$Year <- as.POSIXct(fdi_aggregate_income_group$Group.2, format = "%Y")

jpeg("Plots/Trend Analysis/Trends -- FDI (Perc. of GDP) by Income Group.jpg", width = 500, height = 500)
ggplot(fdi_aggregate_income_group, 
       aes(x = Year, 
           y = Foreign.direct.investment..net.inflows....of.GDP., 
           group = Group.1, colour = Group.1)) + 
  geom_line(size=2) +
  facet_wrap( ~  Group.1, ncol=2) +
  scale_y_continuous("FDI, Net Inflows (% of GDP)") +
  scale_x_discrete("Year") +
  scale_x_datetime(breaks = date_breaks("5 years"), 
                   labels = date_format("%Y")) +
  ggtitle("FDI Inflows by Income Group, 1970 - 2017") +
  theme(axis.text.x = rotatedAxisElementText(90, "x")) +
  theme_tufte() +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5),
        axis.text.x = rotatedAxisElementText(90, "x"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1.0, "cm"),
        legend.spacing.x = unit(0.5, "cm")
        )
dev.off()

# FDI For the World on the Level

#Enter World Bank Statistic for 2018
fdi_aggregate[50,5] <- 	2.1878147

fdi_aggregate$Year <- as.POSIXct(fdi_aggregate$Group.1, format = "%Y")

jpeg("Plots/Trend Analysis/Trends -- FDI (Perc. of GDP) for World.jpg", width = 500, height = 500)
ggplot(data = fdi_aggregate, 
       aes(x = Year, 
           y = Foreign.direct.investment..net.inflows....of.GDP.)) +
  geom_line(size = 2) +
  scale_y_continuous("FDI, Net Inflows (% of GDP)") +
  scale_x_discrete("Year") +
  scale_x_datetime(breaks = date_breaks("3 years"), 
                   labels = date_format("%Y")) +
  ggtitle("FDI Inflows for the World, 1970 - 2018") +
  theme(axis.text.x = rotatedAxisElementText(90, "x")) +
  theme_tufte() +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5),
        axis.text.x = rotatedAxisElementText(90, "x"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1.0, "cm"),
        legend.spacing.x = unit(0.5, "cm")
  )
dev.off()

# FDI For the World at Logs

jpeg("Plots/Trend Analysis/Trends -- FDI (Perc. of GDP) for World (Log).jpg", width = 500, height = 500)
fdi_logs_plot <- ggplot(
  data = fdi_aggregate, 
  aes(x = Year, 
  y = log(`Foreign direct investment, net inflows (% of GDP)`))) +
  geom_line(size = 2) +
  scale_y_continuous("Log FDI, Net Inflows (% of GDP)") +
  scale_x_discrete("Year") +
  scale_x_datetime(breaks = date_breaks("3 years"), 
                   labels = date_format("%Y")) +
  ggtitle("Log FDI Inflows for the World, 1970 - 2017") +
  theme(axis.text.x = rotatedAxisElementText(90, "x")) +
  theme_tufte() +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5),
        axis.text.x = rotatedAxisElementText(90, "x"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1.0, "cm"),
        legend.spacing.x = unit(0.5, "cm")
        )
dev.off()

# FDI For the World at the First Difference

jpeg("Plots/Trend Analysis/Trends -- FDI (Perc. of GDP) for World (FD).jpg", width = 500, height = 500)
fdi_fd_plot <- 
  ggplot(data = fdi_aggregate, 
         aes(x = Year, 
             y = d1.fdi)) +
  geom_line(size = 2) +
  scale_y_continuous("First-Difference FDI, Net Inflows (% of GDP)") +
  scale_x_discrete("Year") +
  scale_x_datetime(breaks = date_breaks("3 years"), 
                   labels = date_format("%Y")) +
  ggtitle("First-Difference FDI Inflows for the World, 1970 - 2017") +
  theme(axis.text.x = rotatedAxisElementText(90, "x")) +
  theme_tufte() +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5),
        axis.text.x = rotatedAxisElementText(90, "x"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1.0, "cm"),
        legend.spacing.x = unit(0.5, "cm")
  )
fdi_fd_plot + geom_hline(yintercept = 0, 
                         linetype = "dashed",
                         color = "red",
                         size = 2)
dev.off()

# FDI For the World at the Growth Rates

jpeg("Plots/Trend Analysis/Trends -- FDI (Perc. of GDP) for World (Growth Rates).jpg", width = 500, height = 500)
fdi_growth_rates_plot <- 
  ggplot(data = fdi_aggregate, aes(x = fdi_aggregate$Year, y = fdi_aggregate$d.l.fdi_1)) +
  geom_line(size = 2) +
  scale_y_continuous("Growth Rates of FDI, Net Inflows (% of GDP)") +
  scale_x_discrete("Year") +
  scale_x_datetime(breaks = date_breaks("3 years"), 
                   labels = date_format("%Y")) +
  ggtitle("Growth Rates of FDI Inflows for the World, 1970 - 2017") +
  theme(axis.text.x = rotatedAxisElementText(90, "x")) +
  theme_tufte() +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5),
        axis.text.x = rotatedAxisElementText(90, "x"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1.0, "cm"),
        legend.spacing.x = unit(0.5, "cm")
        )
fdi_growth_rates_plot + geom_hline(yintercept = 0, 
                                   linetype = "dashed",
                                   color = "red",
                                   size = 2)
dev.off()

## Create Time Series Analysis

# Autoregressions on the level

# lm_AR1 <- lm(fdi_aggregate$`Foreign direct investment, net inflows (% # of GDP)` ~ fdi_aggregate$fdi_1, data = fdi_aggregate)
# 
# summary(lm_AR1) #R^2 78.33%, Adjusted R^2 77.85%
# nobs(lm_AR1) #47
# 
# lm_AR2 <- lm(fdi_aggregate$`Foreign direct investment, net inflows (% # of GDP)` ~ fdi_aggregate$fdi_1 + fdi_aggregate$fdi_2, data = fdi_aggre# gate)
# 
# summary(lm_AR2) #R^2 78.61%, Adjusted R^2 77.62%; first lag is stat # sig at the highest level, second lag is not 
# nobs(lm_AR2) #46
# 
# lm_AR3 <- lm(fdi_aggregate$`Foreign direct investment, net inflows (% # of GDP)` ~ fdi_aggregate$fdi_1 + fdi_aggregate$fdi_2 + fdi_aggregate$f# di_3, data = fdi_aggregate)
# 
# summary(lm_AR3) #R^2 78.54%, Adjusted R^2 76.97%; first lag is stat # sig at the highest level, second lag is not 
# nobs(lm_AR3) #45
# 
# lm_AR4 <- lm(fdi_aggregate$`Foreign direct investment, net inflows (% # of GDP)` ~ fdi_aggregate$fdi_1 + fdi_aggregate$fdi_2 + fdi_aggregate$f# di_3 + fdi_aggregate$fdi_4, data = fdi_aggregate)
# 
# summary(lm_AR4) #R^2 80.61%, Adjusted R^2 78.62%; first lag is stat # sig at the highest level, second lag and third are not, fourth is at # lowest level
# nobs(lm_AR4) #44
# 
# lm_AR5 <- lm(fdi_aggregate$`Foreign direct investment, net inflows (% # of GDP)` ~ fdi_aggregate$fdi_1 + fdi_aggregate$fdi_2 + fdi_aggregate$f# di_3 + fdi_aggregate$fdi_4 + fdi_aggregate$fdi_5, data = fdi_aggregate# )
# 
# summary(lm_AR5) #R^2 80.8%, Adjusted R^2 78.2%; first lag is stat sig # at the highest level, no others are
# nobs(lm_AR5) #43
# 
# lm_AR6 <- lm(fdi_aggregate$`Foreign direct investment, net inflows (% # of GDP)` ~ fdi_aggregate$fdi_1 + fdi_aggregate$fdi_2 + fdi_aggregate$f# di_3 + fdi_aggregate$fdi_4 + fdi_aggregate$fdi_5 + fdi_aggregate$fdi_6# , data = fdi_aggregate)
# 
# summary(lm_AR6) #R^2 84.21%, Adjusted R^2 81.5%; first lag is stat sig # at the highest level, no others are
# nobs(lm_AR6) #42
# 
# lm_AR7 <- lm(fdi_aggregate$`Foreign direct investment, net inflows (% # of GDP)` ~ fdi_aggregate$fdi_1 + fdi_aggregate$fdi_2 + fdi_aggregate$f# di_3 + fdi_aggregate$fdi_4 + fdi_aggregate$fdi_5 + fdi_aggregate$fdi_6 # + fdi_aggregate$fdi_7, data = fdi_aggregate)
# 
# summary(lm_AR7) #R^2 84.85%, Adjusted R^2 81.64%; first lag is stat # sig at the highest level, no others are
# nobs(lm_AR7) #41
# 
# lm_AR8 <- lm(fdi_aggregate$`Foreign direct investment, net inflows (% # of GDP)` ~ fdi_aggregate$fdi_1 + fdi_aggregate$fdi_2 + fdi_aggregate$f# di_3 + fdi_aggregate$fdi_4 + fdi_aggregate$fdi_5 + fdi_aggregate$fdi_6 # + fdi_aggregate$fdi_7 + fdi_aggregate$fdi_8, data = fdi_aggregate)
# 
# summary(lm_AR8) #R^2 84.5%, Adjusted R^2 80.5%; first lag is stat sig # at the highest level, no others are
# nobs(lm_AR8) #40
# 
# lm_AR9 <- lm(fdi_aggregate$`Foreign direct investment, net inflows (% # of GDP)` ~ fdi_aggregate$fdi_1 + fdi_aggregate$fdi_2 + fdi_aggregate$f# di_3 + fdi_aggregate$fdi_4 + fdi_aggregate$fdi_5 + fdi_aggregate$fdi_6 # + fdi_aggregate$fdi_7 + fdi_aggregate$fdi_8 + fdi_aggregate$fdi_9, # data = fdi_aggregate)
# 
# summary(lm_AR9) #R^2 84.31%, Adjusted R^2 79.44%; first lag is stat # sig at the highest level, no others are; f-stat 17.31
# nobs(lm_AR9) #39
# 
# lm_AR10 <- lm(fdi_aggregate$`Foreign direct investment, net inflows (% # of GDP)` ~ fdi_aggregate$fdi_1 + fdi_aggregate$fdi_2 + fdi_aggregate$f# di_3 + fdi_aggregate$fdi_4 + fdi_aggregate$fdi_5 + fdi_aggregate$fdi_6 # + fdi_aggregate$fdi_7 + fdi_aggregate$fdi_8 + fdi_aggregate$fdi_9 + # fdi_aggregate$fdi_10, data = fdi_aggregate)
# 
# summary(lm_AR10) #R^2 88.5%, Adjusted R^2 84.24%; first lag is stat # sig at the highest level, no others are; f-stat 20.77
# nobs(lm_AR10) #38

# Autoregressions of the first difference

lm_fd_AR1 <- lm(fdi_aggregate$d1.fdi ~ fdi_aggregate$d2.fdi, data = fdi_aggregate)

summary(lm_fd_AR1) #R^2 0.1%, Adjusted R^2 -2.1%; first lag is not stat sig; f-stat 0.05653
nobs(lm_fd_AR1) #46

lm_fd_AR2 <- lm(fdi_aggregate$d1.fdi ~ fdi_aggregate$d2.fdi + fdi_aggregate$d3.fdi, data = fdi_aggregate)

summary(lm_fd_AR2) #R^2 1.5%, Adjusted R^2 -3.2%; first lag is not stat sig; f-stat 0.05653
nobs(lm_fd_AR2) #45

lm_fd_AR3 <- lm(fdi_aggregate$d1.fdi ~ fdi_aggregate$d2.fdi + fdi_aggregate$d3.fdi + fdi_aggregate$d4.fdi, data = fdi_aggregate)

summary(lm_fd_AR3) #R^2 13.83%, Adjusted R^2 7.37%; first through second lag are not stat sig, d4 is at 5%; f-stat 2.141
nobs(lm_fd_AR3) #44

lm_fd_AR4 <- lm(fdi_aggregate$d1.fdi ~ fdi_aggregate$d2.fdi + fdi_aggregate$d3.fdi + fdi_aggregate$d4.fdi + fdi_aggregate$d5.fdi, data = fdi_aggregate)

summary(lm_fd_AR4) #R^2 15.63%, Adjusted R^2 6.75%; first through second lag are not stat sig, d4 is at 5%; f-stat 1.761
nobs(lm_fd_AR4) #43

lm_fd_AR5 <- lm(fdi_aggregate$d1.fdi ~ fdi_aggregate$d2.fdi + fdi_aggregate$d3.fdi + fdi_aggregate$d4.fdi + fdi_aggregate$d5.fdi + fdi_aggregate$d6.fdi, data = fdi_aggregate)

summary(lm_fd_AR5) #R^2 22.07%, Adjusted R^2 11.25%; first through second lag are not stat sig, d4 is at 5%; f-stat 2.039
nobs(lm_fd_AR5) #42

lm_fd_AR6 <- lm(fdi_aggregate$d1.fdi ~ fdi_aggregate$d2.fdi + fdi_aggregate$d3.fdi + fdi_aggregate$d4.fdi + fdi_aggregate$d5.fdi + fdi_aggregate$d6.fdi + fdi_aggregate$d7.fdi, data = fdi_aggregate)

summary(lm_fd_AR6) #R^2 24.47%, Adjusted R^2 11.15%; first through second lag are not stat sig, d4 is at 5%; f-stat 1.836
nobs(lm_fd_AR6) #41

lm_fd_AR7 <- lm(fdi_aggregate$d1.fdi ~ fdi_aggregate$d2.fdi + fdi_aggregate$d3.fdi + fdi_aggregate$d4.fdi + fdi_aggregate$d5.fdi + fdi_aggregate$d6.fdi + fdi_aggregate$d7.fdi + fdi_aggregate$d8.fdi, data = fdi_aggregate)

summary(lm_fd_AR7) #R^2 25.68%, Adjusted R^2 9.41%; first through second lag are not stat sig, d4 is at 5%; f-stat 1.579
nobs(lm_fd_AR7) #40

lm_fd_AR8 <- lm(fdi_aggregate$d1.fdi ~ fdi_aggregate$d2.fdi + fdi_aggregate$d3.fdi + fdi_aggregate$d4.fdi + fdi_aggregate$d5.fdi + fdi_aggregate$d6.fdi + fdi_aggregate$d7.fdi + fdi_aggregate$d8.fdi + fdi_aggregate$d9.fdi, data = fdi_aggregate)

summary(lm_fd_AR8) #R^2 24.08%, Adjusted R^2 3.83%; first through second lag are not stat sig, d4 is at 5%; f-stat 1.189
nobs(lm_fd_AR8) #39

lm_fd_AR9 <- lm(fdi_aggregate$d1.fdi ~ fdi_aggregate$d2.fdi + fdi_aggregate$d3.fdi + fdi_aggregate$d4.fdi + fdi_aggregate$d5.fdi + fdi_aggregate$d6.fdi + fdi_aggregate$d7.fdi + fdi_aggregate$d8.fdi + fdi_aggregate$d9.fdi + fdi_aggregate$d10.fdi, data = fdi_aggregate)

summary(lm_fd_AR9) #R^2 37.91%, Adjusted R^2 17.95%; first through second lag are not stat sig, d4 is at 5%; f-stat 1.899
nobs(lm_fd_AR9) #38

# Autoregressions of the growth rates

lm_gr_AR1 <- lm(fdi_aggregate$d.l.fdi_1 ~ fdi_aggregate$d.l.fdi_2, data = fdi_aggregate)

summary(lm_gr_AR1) #R^2 1.8%, Adjusted R^2 -0.4%; first lag is not stat sig; f-stat 0.8199
nobs(lm_gr_AR1) #46


lm_gr_AR1.1 <- lm(fdi_aggregate$d.l.fdi_1 ~ fdi_aggregate$d.growth.fdi_1, data = fdi_aggregate)

summary(lm_gr_AR1.1) #R^2 57.12%, Adjusted R^2 56.16%; f-stat 58.62
nobs(lm_gr_AR1.1) #46

lm_gr_AR2 <- lm(fdi_aggregate$d.l.fdi_1 ~ fdi_aggregate$d.l.fdi_2 + fdi_aggregate$d.l.fdi_3, data = fdi_aggregate)

summary(lm_gr_AR2) #R^2 1.5%, Adjusted R^2 -3.2%; first lag is not stat sig; f-stat 0.05653
nobs(lm_gr_AR2) #45

lm_gr_AR2.1 <- lm(fdi_aggregate$d.l.fdi_1 ~ fdi_aggregate$d.growth.fdi_1 + fdi_aggregate$d.growth.fdi_2, data = fdi_aggregate)

summary(lm_gr_AR2.1) #R^2 72.76%, Adjusted R^2 71.46%; f-stat 56.09
nobs(lm_gr_AR2.1) #45

lm_gr_AR3.1 <- lm(fdi_aggregate$d.l.fdi_1 ~ fdi_aggregate$d.growth.fdi_1 + fdi_aggregate$d.growth.fdi_2 + fdi_aggregate$d.growth.fdi_3, data = fdi_aggregate)

summary(lm_gr_AR3.1) #R^2 80.97%, Adjusted R^2 79.54%; f-stat 56.71
nobs(lm_gr_AR3.1) #44

lm_gr_AR4.1 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
                    fdi_aggregate$d.growth.fdi_1
                  + fdi_aggregate$d.growth.fdi_2 
                  + fdi_aggregate$d.growth.fdi_3
                  + fdi_aggregate$d.growth.fdi_4, 
                  data = fdi_aggregate)

summary(lm_gr_AR4.1) #R^2 87.17%, Adjusted R^2 85.82%; f-stat 64.52
nobs(lm_gr_AR4.1) #43

lm_gr_AR5.1 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
                    fdi_aggregate$d.growth.fdi_1
                  + fdi_aggregate$d.growth.fdi_2 
                  + fdi_aggregate$d.growth.fdi_3
                  + fdi_aggregate$d.growth.fdi_4
                  + fdi_aggregate$d.growth.fdi_5, 
                  data = fdi_aggregate)

summary(lm_gr_AR5.1) #R^2 89.83%, Adjusted R^2 88.41%; f-stat 64.52
nobs(lm_gr_AR5.1) #42

## Test For Correct Number of Lags: Forward Selection BIC / AIC

# On the level model

# aic_table <- add1(lm(fdi_aggregate$`Foreign direct investment, net # inflows (% of GDP)`~1), 
#      fdi_aggregate$`Foreign direct investment, net inflows (% of GDP)` # ~
#        fdi_aggregate$fdi_1 +
#        fdi_aggregate$fdi_2 +
#        fdi_aggregate$fdi_3 + 
#        fdi_aggregate$fdi_4 +
#        fdi_aggregate$fdi_5 + 
#        fdi_aggregate$fdi_6 +
#        fdi_aggregate$fdi_7 + 
#        fdi_aggregate$fdi_8 + 
#        fdi_aggregate$fdi_9 +
#        fdi_aggregate$fdi_10, 
#      test = "F") 
# 
# which.min(aic_table$AIC) # Result: use 1 lag
# 
# aic_table <- add_column(aic_table, Lags = c(0:10), .before = 1)
# 
# fdi.n <- nrow(fdi_aggregate)
# 
# bic_table <- add1(lm(fdi_aggregate$`Foreign direct investment, net # inflows (% of GDP)`~1), 
#      fdi_aggregate$`Foreign direct investment, net inflows (% of GDP)` # ~
#        fdi_aggregate$fdi_1 +
#        fdi_aggregate$fdi_2 +
#        fdi_aggregate$fdi_3 + 
#        fdi_aggregate$fdi_4 +
#        fdi_aggregate$fdi_5 + 
#        fdi_aggregate$fdi_6 +
#        fdi_aggregate$fdi_7 + 
#        fdi_aggregate$fdi_8 + 
#        fdi_aggregate$fdi_9 +
#        fdi_aggregate$fdi_10, 
#      k = log(fdi.n))
# 
# which.min(bic_table$AIC) # Result: use 1 lag
# 
# bic_table <- add_column(bic_table, Lags = c(0:10), .before = 1)
# setnames(bic_table, c("AIC"), c("BIC"))

# First Difference Models

aic_table_fd <- add1(lm(fdi_aggregate$d1.fdi ~1), 
                  fdi_aggregate$d1.fdi ~
                    fdi_aggregate$d2.fdi +
                    fdi_aggregate$d3.fdi +
                    fdi_aggregate$d4.fdi + 
                    fdi_aggregate$d5.fdi +
                    fdi_aggregate$d6.fdi + 
                    fdi_aggregate$d7.fdi +
                    fdi_aggregate$d8.fdi + 
                    fdi_aggregate$d9.fdi + 
                    fdi_aggregate$d10.fdi,
                  test = "F") 

which.min(aic_table_fd$AIC) # Result: use d4, 3 lags

aic_table_fd <- add_column(aic_table_fd, Lags = c(0:9), .before = 1)

bic_table_fd <- add1(lm(fdi_aggregate$d1.fdi ~1), 
                         fdi_aggregate$d1.fdi ~
                           fdi_aggregate$d2.fdi +
                           fdi_aggregate$d3.fdi +
                           fdi_aggregate$d4.fdi + 
                           fdi_aggregate$d5.fdi +
                           fdi_aggregate$d6.fdi + 
                           fdi_aggregate$d7.fdi +
                           fdi_aggregate$d8.fdi + 
                           fdi_aggregate$d9.fdi + 
                           fdi_aggregate$d10.fdi,
                  k = log(fdi.n))

which.min(bic_table_fd$AIC) # Result: use d4, 3 lags

bic_table_fd <- add_column(bic_table_fd, Lags = c(0:9), .before = 1)
setnames(bic_table_fd, c("AIC"), c("BIC"))

# Growth Rates Models

aic_table_gr <- add1(lm(fdi_aggregate$d.l.fdi_1 ~1), 
                     fdi_aggregate$d.growth.fdi_1 ~
                       fdi_aggregate$d.growth.fdi_1 +
                       fdi_aggregate$d.growth.fdi_2 +
                       fdi_aggregate$d.growth.fdi_3 + 
                       fdi_aggregate$d.growth.fdi_4 +
                       fdi_aggregate$d.growth.fdi_5,
                     test = "F") 

which.min(aic_table_gr$AIC) # Result: use 1 lag on growth

aic_table_gr <- add_column(aic_table_gr, Lags = c(0:5), .before = 1)

bic_table_gr <- add1(lm(fdi_aggregate$d.l.fdi_1 ~1), 
                     fdi_aggregate$d.growth.fdi_1 ~
                       fdi_aggregate$d.growth.fdi_1 +
                       fdi_aggregate$d.growth.fdi_2 +
                       fdi_aggregate$d.growth.fdi_3 + 
                       fdi_aggregate$d.growth.fdi_4 +
                       fdi_aggregate$d.growth.fdi_5,
                     k = log(fdi.n))

which.min(bic_table_gr$AIC) # Result: use 1 lag on growth

bic_table_gr <- add_column(bic_table_gr, Lags = c(0:5), .before = 1)
setnames(bic_table_gr, c("AIC"), c("BIC"))

## Report Tables

# On the level AIC and BIC

#aic_table %>%
#  kable() %>%
#  kable_styling(bootstrap_options = c("striped"), full_width = F) %>%
#  add_header_above(c(
#    "AIC Table for FDI Net Inflows with Up to 10 Lags"
#    = 8)) %>%
#  save_kable(file = "Tables/AIC and BIC/AIC Table for FDI Lags.html",
#             self_contained = T)
#
#bic_table %>%
#  kable() %>%
#  kable_styling(bootstrap_options = c("striped"), full_width = F) %>%
#  add_header_above(c(
#    "BIC Table for FDI Net Inflows with Up to 10 Lags"
#    = 6)) %>%
#  save_kable(file = "Tables/AIC and BIC/BIC Table for FDI Lags.html",
#             self_contained = T)

# FD Models

aic_table_fd %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"), full_width = F) %>%
  add_header_above(c(
    "AIC Table for First Difference FDI Net Inflows with Up to 9 Lags"
    = 8)) %>%
  save_kable(file = "Tables/AIC and BIC/AIC Table for FD FDI Lags.html",
             self_contained = T)

bic_table_fd %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"), full_width = F) %>%
  add_header_above(c(
    "BIC Table for First Difference FDI Net Inflows with Up to 9 Lags"
    = 6)) %>%
  save_kable(file = "Tables/AIC and BIC/BIC Table for FD FDI Lags.html",
             self_contained = T)

# GR Models

aic_table_gr %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"), full_width = F) %>%
  add_header_above(c(
    "AIC Table for Growth Rates of FDI Net Inflows with Up to 5 Lags"
    = 8)) %>%
  save_kable(file = "Tables/AIC and BIC/AIC Table for GR FDI Lags.html",
             self_contained = T)

bic_table_gr %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"), full_width = F) %>%
  add_header_above(c(
    "BIC Table for Growth Rates of FDI Net Inflows with Up to 9 Lags"
    = 6)) %>%
  save_kable(file = "Tables/AIC and BIC/BIC Table for GR FDI Lags.html",
             self_contained = T)

## Test for Stationarity 

# On the level for AR1 Model

# Informal Methods: Chart Analysis Combined with Coefficient Significantly Different from 1

# Chart Analysis

fdi_logs_plot #appears to have a trend

# AR(1) Based Upon Best Analysis from BIC and AIC

# summary(lm_AR1) #co-efficient is 0.89 which is quite close to unit root

# Formal Tests: Dickey-Fuller Test

# fdi_aggregate_subset <- fdi_aggregate[c(2:49), ]
# 
# adf1 = ur.df(fdi_aggregate_subset$`Foreign direct investment, net # inflows (% of GDP)`, type = "none", lags = 0)

# summary(adf1) #model is non-stationary and unit root

# First Difference for AR3 (d4) Model

# Informal Methods: Chart Analysis Combined with Coefficient Significantly Different from 1

# Chart Analysis

fdi_fd_plot #appears to be stationary

# AR(3) Based Upon Best Analysis from BIC and AIC

summary(lm_fd_AR3) #co-efficient on d4 (the statistically significant variable) is negative and close to 0 (-0.35)

lm_fd_AR3.1 <- lm(fdi_aggregate$d1.fdi ~ fdi_aggregate$d4.fdi, fdi_aggregate)

summary(lm_fd_AR3.1) # R^2 12.85, Adjusted R^2 10.77

# Formal Tests: Dickey-Fuller Test

fdi_aggregate_subset2 <- fdi_aggregate[c(2:45), ]

adf2 = ur.df(fdi_aggregate_subset2$d1.fdi, type = "none", lags = 3)
summary(adf2) #model is very stationary

adf2.1 = ur.df(fdi_aggregate_subset2$d1.fdi, type = "none", lags = 2)
summary(adf2.1) #model is even better stationarity; test-stat of -5.75

adf2.1.1 = ur.df(fdi_aggregate_subset2$d1.fdi, type = "none", selectlags = "BIC") #selects 1 lag
summary(adf2.1.1) 

adf2.1.2 = ur.df(fdi_aggregate_subset2$d1.fdi, type = "none", selectlags = "AIC") #selects 1 lag
summary(adf2.1.2) 

adf.test(fdi_aggregate_subset2$d1.fdi, k=2)

lm_fd_AR2.1 <- lm(fdi_aggregate$d1.fdi ~ fdi_aggregate$dd1.fdi + fdi_aggregate$dd2.fdi, data = fdi_aggregate)

summary(lm_fd_AR2.1) # R^2 of 67.95, Adjusted R^2 of 66.43
nobs(lm_fd_AR2.1) #45

lm_fd_AR2.1 <- lm(fdi_aggregate$d1.fdi ~ fdi_aggregate$dd1.fdi + fdi_aggregate$dd2.fdi, data = fdi_aggregate)

summary(lm_fd_AR2.1) # R^2 of 67.95, Adjusted R^2 of 66.43
nobs(lm_fd_AR2.1) #45

# # Growth Rates for FDI
# 
# Formal Tests: Dickey-Fuller Test
 
fdi_aggregate_subset3 <- fdi_aggregate[c(2:47), ]

adf3 = ur.df(fdi_aggregate_subset3$d.l.fdi_1, type = "none", lags = 1)
summary(adf3) #model is very stationary, test-statistics of -4.7
# 
# summary(lm_gr_AR1.1) #very good baseline model
# 
# lm_gr_AR1.1.1 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                       fdi_aggregate$d.l.fdi_2 +
#                       fdi_aggregate$d.growth.fdi_1, 
#                     data = fdi_aggregate)
# 
# summary(lm_gr_AR1.1.1) #perfect collinearity
# nobs(lm_gr_AR1.1.1)
# 
# ## Improve lm_gr_AR1.1: add explanatory variables
# 
# # Test Which Variables To Include
# 
# # regsubsets(fdi_aggregate$d.l.fdi_1 ~ ., data = fdi_aggregate, really# .big = T)
# 
# which.min(bic_table_all$AIC) # Result: use 0 lags on gdpcap on the # level
# 
# # CSDR Lags
# 
# bic_table_csdr_gr <- add1(lm(fdi_aggregate$d.l.fdi_1 ~1),
#                           fdi_aggregate$d.l.fdi_1 ~
#                        fdi_aggregate$d.l.csdr_1 +
#                        fdi_aggregate$d.l.csdr_2 +
#                        fdi_aggregate$d.l.csdr_3 +
#                        fdi_aggregate$d.l.csdr_4 + 
#                        fdi_aggregate$d.l.csdr_5,
#                      k = log(fdi.n))
# 
# which.min(bic_table_csdr_gr$AIC) # Result: use 2 lags on csdr
# 
# 
# lm_gr_ADL1 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                       fdi_aggregate$d.growth.fdi_1 
#                     + fdi_aggregate$d.l.csdr_1
#                     + fdi_aggregate$d.l.csdr_2, 
#                     data = fdi_aggregate)
# 
# summary(lm_gr_ADL1) # R^2 67.74 and Adjusted R^2 65.38
# nobs(lm_gr_ADL1) #45
# 
# bic_table_csdr_gr1 <- add1(lm(fdi_aggregate$d.l.fdi_1 ~1),
#                           fdi_aggregate$d.l.fdi_1 ~
#                             fdi_aggregate$csdr_avg +
#                             fdi_aggregate$csdr_1 +
#                             fdi_aggregate$csdr_2 +
#                             fdi_aggregate$csdr_3 +
#                             fdi_aggregate$csdr_4 + 
#                             fdi_aggregate$csdr_5,
#                           k = log(fdi.n))
# 
# which.min(bic_table_csdr_gr1$AIC) # Result: use 0 lags on csdr on the # level
# 
# lm_gr_ADL1.1 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                       fdi_aggregate$d.growth.fdi_1 
#                     + fdi_aggregate$csdr_1
#                     + fdi_aggregate$csdr_2, 
#                     data = fdi_aggregate)
# 
# summary(lm_gr_ADL1.1) # R^2 64.46 and Adjusted R^2 61.93
# nobs(lm_gr_ADL1.1) #46
# 
# # GDP per capita
# 
# bic_table_gdpcap1 <- add1(lm(fdi_aggregate$d.l.fdi_1 ~1),
#                           fdi_aggregate$d.l.fdi_1 ~
#                             fdi_aggregate$d.l.gdpcap_1 +
#                             fdi_aggregate$d.l.gdpcap_2 +
#                             fdi_aggregate$d.l.gdpcap_3 +
#                             fdi_aggregate$d.l.gdpcap_4 + 
#                             fdi_aggregate$d.l.gdpcap_5,
#                           k = log(fdi.n))
# 
# which.min(bic_table_gdpcap1$AIC) # Result: don't use growth rate of # GDP cap
# 
# bic_table_gdpcap2 <- add1(lm(fdi_aggregate$d.l.fdi_1 ~1),
#                            fdi_aggregate$d.l.fdi_1 ~
#                              fdi_aggregate$`GDP per capita (constant # 2010 US$)` +
#                              fdi_aggregate$gdpcap_1 +
#                              fdi_aggregate$gdpcap_2 +
#                              fdi_aggregate$gdpcap_3 +
#                              fdi_aggregate$gdpcap_4 + 
#                              fdi_aggregate$gdpcap_5 +
#                              fdi_aggregate$gdpcap_6,
#                            k = log(fdi.n))
# 
# which.min(bic_table_gdpcap2$AIC) # Result: use 0 lags on gdpcap on the # level
# 
# # GDP growth
# 
# bic_table_gdpgrowth <- add1(lm(fdi_aggregate$d.l.fdi_1 ~1),
#                           fdi_aggregate$d.l.fdi_1 ~
#                             fdi_aggregate$`GDP growth (annual %)` +
#                             fdi_aggregate$gdpgrowth_1 +
#                             fdi_aggregate$gdpgrowth_2 +
#                             fdi_aggregate$gdpgrowth_3 +
#                             fdi_aggregate$gdpgrowth_4 + 
#                             fdi_aggregate$gdpgrowth_5 +
#                             fdi_aggregate$gdpgrowth_6,
#                           k = log(fdi.n))
# 
# which.min(bic_table_gdpgrowth$AIC) # Result: use 5 lags on gdpgrowth # on the level
# 
# lm_gr_ADL1.2 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                         fdi_aggregate$d.growth.fdi_1 
#                       + fdi_aggregate$csdr_1
#                       + fdi_aggregate$csdr_2
#                       + fdi_aggregate$`GDP growth (annual %)`
#                       + fdi_aggregate$gdpgrowth_1
#                       + fdi_aggregate$gdpgrowth_2
#                       + fdi_aggregate$gdpgrowth_3
#                       + fdi_aggregate$gdpgrowth_4
#                       + fdi_aggregate$gdpgrowth_5, 
#                       data = fdi_aggregate)
# 
# summary(lm_gr_ADL1.2) # R^2 78.24 and Adjusted R^2 72.3
# nobs(lm_gr_ADL1.2) #43
# 
# # Exports of Commodities
# 
# bic_table_exports <- add1(lm(fdi_aggregate$d.l.fdi_1 ~1),
#                             fdi_aggregate$d.l.fdi_1 ~
#                               fdi_aggregate$d.l.exports_1 +
#                               fdi_aggregate$d.l.exports_2 +
#                               fdi_aggregate$d.l.exports_3 +
#                               fdi_aggregate$d.l.exports_4 +
#                               fdi_aggregate$d.l.exports_5, 
#                             k = log(fdi.n))
# 
# which.min(bic_table_exports$AIC) # Result: use 0 lags on exports on # growth
# 
# bic_table_exports1 <- add1(lm(fdi_aggregate$d.l.fdi_1 ~1),
#                             fdi_aggregate$d.l.fdi_1 ~
#                               fdi_aggregate$`Exports of Commodities to # the World, (% of GDP)` +
#                               fdi_aggregate$exports_1 +
#                               fdi_aggregate$exports_2 +
#                               fdi_aggregate$exports_3 +
#                               fdi_aggregate$exports_4 + 
#                               fdi_aggregate$exports_5 +
#                               fdi_aggregate$exports_6,
#                             k = log(fdi.n))
# 
# which.min(bic_table_exports1$AIC) # Result: use 5 lags on exports on # the level
# 
# lm_gr_ADL1.3 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                      fdi_aggregate$d.growth.fdi_1 
#                    + fdi_aggregate$csdr_1
#                    + fdi_aggregate$csdr_2
#                    + fdi_aggregate$`GDP growth (annual %)`
#                    + fdi_aggregate$gdpgrowth_1
#                    + fdi_aggregate$gdpgrowth_2
#                    + fdi_aggregate$gdpgrowth_3
#                    + fdi_aggregate$gdpgrowth_4
#                    + fdi_aggregate$gdpgrowth_5
#                    + fdi_aggregate$`Exports of Commodities to the # World, (% of GDP)`
#                    + fdi_aggregate$exports_1
#                    + fdi_aggregate$exports_2
#                    + fdi_aggregate$exports_3
#                    + fdi_aggregate$exports_4
#                    + fdi_aggregate$exports_5, 
#                    data = fdi_aggregate)
# 
# summary(lm_gr_ADL1.3) # R^2 99.33 and Adjusted R^2 89.31; no variables # statistically significant; f-stat not stat sig
# nobs(lm_gr_ADL1.3) #17
# 
# # Current Account Balance
# 
# bic_table_ca <- add1(lm(fdi_aggregate$d.l.fdi_1 ~1),
#                            fdi_aggregate$d.l.fdi_1 ~
#                              fdi_aggregate$`Current account balance (% # of GDP)` +
#                              fdi_aggregate$ca_1 +
#                              fdi_aggregate$ca_2 +
#                              fdi_aggregate$ca_3 +
#                              fdi_aggregate$ca_4 + 
#                              fdi_aggregate$ca_5 +
#                              fdi_aggregate$ca_6,
#                            k = log(fdi.n))
# 
# which.min(bic_table_ca$AIC) # Result: use 0 lags for ca on the level
# 
# 
# # Port Infrastracture
# 
# bic_table_port <- add1(lm(fdi_aggregate$d.l.fdi_1 ~1),
#                      fdi_aggregate$d.l.fdi_1 ~
#                        fdi_aggregate$`Quality of port infrastructure, # WEF (1=extremely underdeveloped to 7=well developed and efficient by # international standards)` +
#                        fdi_aggregate$port_1 +
#                        fdi_aggregate$port_2 +
#                        fdi_aggregate$port_3 +
#                        fdi_aggregate$port_4 + 
#                        fdi_aggregate$port_5 +
#                        fdi_aggregate$port_6,
#                      k = log(fdi.n))
# 
# which.min(bic_table_port$AIC) # Result: use 1 lags for port on the # level
# 
# lm_gr_ADL1.4 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                      fdi_aggregate$d.growth.fdi_1 
#                    + fdi_aggregate$csdr_1
#                    + fdi_aggregate$csdr_2
#                    + fdi_aggregate$`GDP growth (annual %)`
#                    + fdi_aggregate$gdpgrowth_1
#                    + fdi_aggregate$gdpgrowth_2
#                    + fdi_aggregate$gdpgrowth_3
#                    + fdi_aggregate$gdpgrowth_4
#                    + fdi_aggregate$gdpgrowth_5
#                    + fdi_aggregate$`Exports of Commodities to the # World, (% of GDP)`
#                    + fdi_aggregate$exports_1
#                    + fdi_aggregate$exports_2
#                    + fdi_aggregate$exports_3
#                    + fdi_aggregate$exports_4
#                    + fdi_aggregate$exports_5
#                    + fdi_aggregate$`Quality of port infrastructure, # WEF (1=extremely underdeveloped to 7=well developed and efficient by # international standards)`
#                    + fdi_aggregate$port_1, 
#                    data = fdi_aggregate)
# 
# summary(lm_gr_ADL1.4) # Multicollinear
# nobs(lm_gr_ADL1.4) #6
# 
# lm_gr_ADL1.3.1 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                      fdi_aggregate$d.growth.fdi_1 
#                    + fdi_aggregate$csdr_1
#                    + fdi_aggregate$csdr_2
#                    + fdi_aggregate$`GDP growth (annual %)`
#                    + fdi_aggregate$gdpgrowth_1
#                    + fdi_aggregate$gdpgrowth_2
#                    + fdi_aggregate$gdpgrowth_3
#                    + fdi_aggregate$gdpgrowth_4
#                    + fdi_aggregate$gdpgrowth_5
#                    + fdi_aggregate$`Quality of port infrastructure, # WEF (1=extremely underdeveloped to 7=well developed and efficient by # international standards)`
#                    + fdi_aggregate$port_1, 
#                    data = fdi_aggregate)
# 
# summary(lm_gr_ADL1.3.1) # Multicollinear
# nobs(lm_gr_ADL1.3.1) #6
# 
# # Don't use ports
# # Don't want to use price level or sitc because of data availability
# 
# # Polity2
# 
# bic_table_polity2 <- add1(lm(fdi_aggregate$d.l.fdi_1 ~1),
#                        fdi_aggregate$d.l.fdi_1 ~
#                          fdi_aggregate$polity2 +
#                          fdi_aggregate$pol2_1 +
#                          fdi_aggregate$pol2_2 +
#                          fdi_aggregate$pol2_3 +
#                          fdi_aggregate$pol2_4 + 
#                          fdi_aggregate$pol2_5 +
#                          fdi_aggregate$pol2_6,
#                        k = log(fdi.n))
# 
# which.min(bic_table_polity2$AIC) # Result: use 1 lags for port on the # level
# 
# # Maximizing the bias variance trade-off (in-sample), best model is: 
# summary(lm_gr_ADL1.3)
# 
# ## Dickey-Fuller Test for lm_gr_ADL1.3
# 
## Chow TesT: Test for Stationary Break in 2009
## https://www.r-bloggers.com/endogenously-detecting-structural-breaks# -in-a-time-series-implementation-in-r/

# Store FDI data as time series object
# 
# fdi <- ts(fdi_aggregate$Foreign.direct.investment..net.inflows....of# .GDP., start = c(1969, 1), end = c(2018, 2), frequency = 1)
# 
# # Store Breakpoints
# 
# bp.fdi <- breakpoints(fdi~1)
# summary(bp.fdi)
# 
# # Plot 
# jpeg("Plots/Trend Analysis/Structural Break.jpg", width = 500, height # = 500)
# plot(bp.fdi$breakpoints)
# par(new=T)
# plot(fdi)
# lines(bp.fdi)
# dev.off()
# 
# # Confidence Intervals
# ci.fdi <- confint(bp.fdi)
# ci.fdi
# lines(ci.fdi)
# 
# ## Chow Test: Attempt2
# ## https://stats.stackexchange.com/questions/93529/dummies-instead-of# # -the-chow-test
# 
# # Try first for lm_gr_ADL1.2
# 
# Year <- fdi_aggregate$Year
# 
# sctest(lm_gr_ADL1.2, type = "Chow", Year = 2009) # says no, p-value of # 0.28
# 
# # Now try for lm_gr_ADL1.3
# 
# sctest(lm_gr_ADL1.3, type = "Chow", Year = 2009) # says hell no, p# -value of 0.86
# 
# # I think these models are fit too closely to these trends
# 
# # Try for lm_gr_AR1.1
# 
# sctest(lm_gr_AR1.1, type = "Chow", Year = 2009) # says  no, p-value of # 0.43
# 
# # Try for 2000
# 
# sctest(lm_gr_ADL1.2, data = fdi_aggregate, type = "Chow", Year4 = 35) # # says  no, p-value of 0.43
# 
# # Try for 2009 for adf2.1
# 
# #sctest(adf2.1, data = fdi_aggregate, type= "Chow", Year4 = 41)
# #Does not work
# 
# # Try Dummy Variable
# 
# fdi_aggregate$GFC <- rep(1, 50)
# fdi_aggregate$Year4 <- as.numeric(fdi_aggregate$Group.1)
# fdi_aggregate$GFC[fdi_aggregate$Year4<41]=0
# 
# lm_gr_AR1.1.3 <- lm(fdi_aggregate$d.l.fdi_1 ~ fdi_aggregate$d.growth# .fdi_1 * GFC, data = fdi_aggregate)
# 
# summary(lm_gr_AR1.1.3) # as expected, does not work
# 
# lm_gr_AR1.1.3.1 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                     fdi_aggregate$d.growth.fdi_1 
#                   + fdi_aggregate$d.growth.fdi_2 * GFC, 
#                     data = fdi_aggregate)
# 
# summary(lm_gr_AR1.1.3.1) # as expected, does not work
# 
# lm_gr_AR1.1.3.2 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                         fdi_aggregate$d.growth.fdi_1 
#                       + fdi_aggregate$d.growth.fdi_2
#                       + fdi_aggregate$d.growth.fdi_3 * GFC, 
#                       data = fdi_aggregate)
# 
# summary(lm_gr_AR1.1.3.2) # as expected, does not work
# 
# lm_gr_AR1.1.3.3 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                         fdi_aggregate$d.growth.fdi_1 
#                       + fdi_aggregate$d.growth.fdi_2 
#                       + fdi_aggregate$gdpgrowth_1 * GFC, 
#                       data = fdi_aggregate)
# 
# summary(lm_gr_AR1.1.3.3) # as expected, does not work
# 
# lm_gr_AR1.1.3.4 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                         fdi_aggregate$d.growth.fdi_1 
#                       + fdi_aggregate$d.growth.fdi_2 
#                       + fdi_aggregate$gdpgrowth_1
#                       + fdi_aggregate$gdpgrowth_2 * GFC, 
#                       data = fdi_aggregate)
# 
# summary(lm_gr_AR1.1.3.4) # as expected, does not work
# 
# lm_gr_AR1.1.3.5 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                         fdi_aggregate$d.growth.fdi_1 
#                       + fdi_aggregate$d.growth.fdi_2 
#                       + fdi_aggregate$d.l.gdpgrowth_1
#                       + fdi_aggregate$d.l.gdpgrowth_2 * GFC, 
#                       data = fdi_aggregate)
# 
# summary(lm_gr_AR1.1.3.5) # as expected, does not work
# 
# lm_gr_AR1.1.3.6 <- lm(fdi_aggregate$d.l.fdi_1 ~ 
#                         fdi_aggregate$d.growth.fdi_1 
#                       + fdi_aggregate$d.growth.fdi_2 * GFC
#                       + fdi_aggregate$gdpgrowth_1 * GFC, 
#                       data = fdi_aggregate)
# 
# summary(lm_gr_AR1.1.3.6) # this is the correct specification
# 
# 
# # Try for just the series
# 
# # sctest(fdi_aggregate$`Foreign direct investment, net inflows (% of # GDP)`, type = "Chow", Year = 2009) # says  no, p-value of 0.43
# # Does not work
# 
# ## Forecast Forward 5 Years FDI with Confidence Interval
# # This procedure does not work well
# lm_gr_ADL1.2$fitted.values
# 
# lm_gr_ADL1.2.ts <- ts(as.numeric(lm_gr_ADL1.2$fitted.values, start = # 1970, frequency = 1))
# 
# fdi_forecast2 <- predict(lm_gr_ADL1.2$fitted.values.ts)
# fdi_forecast2
# 
# predpoint <- data.frame(p)

## Use ARIMA Model to Try to Replicate Result from ADF
# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/
# https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials

summary(adf2.1)

fdi.ts <- ts(fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP., start = c(1969,1), frequency = 1)

fdi_components <- decompose(fdi.ts)

fdi.arima <- arima(fdi.ts, order = c(0,1,1))
fdi.arima

fdi_forecast1 <- predict(fdi.arima, n.ahead = 5)
fdi_forecast1

fdi.arima2 <- arima(fdi.ts, order = c(1,1,2))
fdi.arima2

fdi_forecast2 <- predict(fdi.arima2, n.ahead = 5)
fdi_forecast2

fdi.growth.ts <- ts(fdi_aggregate$d.l.fdi_1, start = 1969, frequency = 1)


fdi.growth.arima <- arima(fdi.growth.ts, order = c(0,1,1))

fdi_forecast1.1 <- predict(fdi.growth.arima, n.ahead = 5)
fdi_forecast1.1

fdi.growth.arima2 <- arima(fdi.growth.ts, order = c(1,1,2))
fdi.growth.arima2

fdi_forecast1.2 <- predict(fdi.growth.arima2, n.ahead = 5)
fdi_forecast1.2 # this works

fdi.growth.arima3 <- arima(fdi.growth.ts, order = c(1,2,2))
fdi.growth.arima3

fdi_forecast1.3 <- predict(fdi.growth.arima3, n.ahead = 5)
fdi_forecast1.3 # this works

fdi.growth.arima4 <- arima(fdi.growth.ts, order = c(2,2,2))
fdi.growth.arima4

fdi_forecast1.4 <- predict(fdi.growth.arima3, n.ahead = 5)
fdi_forecast1.4 # this works

fdi.growth.arima1.5 <- arima(fdi.growth.ts, order = c(2,1,2))
fdi.growth.arima1.5

fdi.growth.arima1.6 <- arima(fdi.growth.ts, order = c(3,1,2))
fdi.growth.arima1.6

fdi.growth.arima1.7 <- arima(fdi.growth.ts, order = c(1,2,3))
fdi.growth.arima1.7

fdi_forecast1.7 <- predict(fdi.growth.arima1.7, n.ahead = 5)
fdi_forecast1.7 # gives similar results to 1.3; worse AIC

#futurVal <- forecast.Arima(fdi_forecast1.2, h=10, level=c(99.5))

#plot.forecast(fdi_forecast1.2)


fdi_forecast1.3


# Create a new dataframe to take the growth rates and multiply them by the FDI rates in the previous year to create the forecast

fdi_forecast_df <- data.frame("Year" = 1969:2023)
fdi_forecast_df$Year <- ts(fdi_forecast_df$Year, start=c(1969,1), end = c(2023,1), frequency = 1)
fdi_forecast_df <- rowr::cbind.fill(fdi_forecast_df, fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP., fdi_aggregate$d.l.fdi_1, fill = NA)
fdi_forecast_df$`FDI, net inflows (% of GDP)` <- fdi_forecast_df[,2]
fdi_forecast_df$`FDI, Growth Rate (annual %)` <- fdi_forecast_df[,3]
fdi_forecast_df[,2:3] <- NULL

fdi_forecast_df2 <- data.frame("Year" = 2018:2022)
fdi_forecast_df2$pred_growthrate <- fdi_forecast1.3$pred
fdi_forecast_df2$pred_growthrate_sterrors <- fdi_forecast1.3$se
fdi_forecast_df <- join(fdi_forecast_df, fdi_forecast_df2, by = "Year", type = "left", match = "all")

# Add 2017-2018 Growth Rate (from WB)
# 2017 FDI, net inflows (% of GDP): 2.561
# 2018 FDI, net inflows (% of GDP): 1.389
# 2017-2018 % Change: -45.7633734
fdi_forecast_df[49,4] <- -45.7633734

# Predict FDI on the level
fdi_forecast_df$pred_fdi <- (Lag(fdi_forecast_df$`FDI, net inflows (% of GDP)`, 1) *(1 + (Lag(fdi_forecast_df$pred_growthrate, 1) / 100)))

fdi_forecast_df$pred_fdi2 <- (Lag(fdi_forecast_df$pred_fdi, 1) *(1 + (Lag(fdi_forecast_df$pred_growthrate, 1) / 100)))

fdi_forecast_df$pred_fdi3 <- (Lag(fdi_forecast_df$pred_fdi2, 1) *(1 + (Lag(fdi_forecast_df$pred_growthrate, 1) / 100)))

fdi_forecast_df$pred_fdi4 <- (Lag(fdi_forecast_df$pred_fdi3, 1) *(1 + (Lag(fdi_forecast_df$pred_growthrate, 1) / 100)))

fdi_forecast_df$pred_fdi5 <- (Lag(fdi_forecast_df$pred_fdi4, 1) *(1 + (Lag(fdi_forecast_df$pred_growthrate, 1) / 100)))

fdi_forecast_df$pred_fdi6 <- (Lag(fdi_forecast_df$pred_fdi5, 1) *(1 + (Lag(fdi_forecast_df$pred_growthrate, 1) / 100)))

fdi_forecast_df[51,7] <- NA
fdi_forecast_df[52,8] <- NA
fdi_forecast_df[53,9] <- NA
fdi_forecast_df[54,10] <- NA
fdi_forecast_df[55,11] <- NA

fdi_forecast_df$pred_fdi_final <- apply(fdi_forecast_df[6:11], 1, function(x) paste(x[!is.na(x)], collapse = ""))

fdi_forecast_df[6:11] <- NULL
fdi_forecast_df[1:49,6] <- NA
fdi_forecast_df[50,2] <- 	2.1878147

fdi_forecast_df$pred_fdi_final <- as.numeric(fdi_forecast_df$pred_fdi_final)

# Add +/- SE to growth rates and then translate to FDI on the level

fdi_forecast_df$se_high <- Lag(fdi_forecast_df$pred_growthrate, 1) + Lag(fdi_forecast_df$pred_growthrate_sterrors,1)
fdi_forecast_df$se_low <- Lag(fdi_forecast_df$pred_growthrate, 1) - Lag(fdi_forecast_df$pred_growthrate_sterrors,1)

# Forecast FDI on the level using SEs

fdi_forecast_df$pred_se_high <- Lag(fdi_forecast_df$pred_fdi_final,1) * (1 + (fdi_forecast_df$se_high / 100))
fdi_forecast_df$pred_se_low <- Lag(fdi_forecast_df$pred_fdi_final,1) * (1 + (fdi_forecast_df$se_low / 100))

fdi_forecast_df$pred_se_high2 <- Lag(fdi_forecast_df$pred_se_high,1) * (1 + (fdi_forecast_df$se_high / 100))
fdi_forecast_df$pred_se_low2 <- Lag(fdi_forecast_df$pred_se_low,1) * (1 + (fdi_forecast_df$se_low / 100))

fdi_forecast_df$pred_se_high3 <- Lag(fdi_forecast_df$pred_se_high2,1) * (1 + (fdi_forecast_df$se_high / 100))
fdi_forecast_df$pred_se_low3 <- Lag(fdi_forecast_df$pred_se_low2,1) * (1 + (fdi_forecast_df$se_low / 100))

fdi_forecast_df$pred_se_high4 <- Lag(fdi_forecast_df$pred_se_high3,1) * (1 + (fdi_forecast_df$se_high / 100))
fdi_forecast_df$pred_se_low4 <- Lag(fdi_forecast_df$pred_se_low3,1) * (1 + (fdi_forecast_df$se_low / 100))

fdi_forecast_df$pred_se_high5 <- Lag(fdi_forecast_df$pred_se_high4,1) * (1 + (fdi_forecast_df$se_high / 100))
fdi_forecast_df$pred_se_low5 <- Lag(fdi_forecast_df$pred_se_low4,1) * (1 + (fdi_forecast_df$se_low / 100))

fdi_forecast_df[52:55,9:10] <- NA
fdi_forecast_df[53:55,11:12] <- NA
fdi_forecast_df[54:55,13:14] <- NA
fdi_forecast_df[55,15:16] <- NA

fdi_forecast_df$pred_se_high <- apply(fdi_forecast_df[c(9,11,13,15,17)], 1, function(x) paste(x[!is.na(x)], collapse = ""))
fdi_forecast_df[,c(11,13,15,17)] <- NULL

fdi_forecast_df$pred_se_low <- apply(fdi_forecast_df[10:14], 1, function(x) paste(x[!is.na(x)], collapse = ""))
fdi_forecast_df[,11:14] <- NULL
fdi_forecast_df[1:50,9:10] <- NA
fdi_forecast_df[50, 9:10] <- 2.1878147

fdi_forecast_df$pred_se_high <- as.numeric(fdi_forecast_df$pred_se_high)
fdi_forecast_df$pred_se_low <- as.numeric(fdi_forecast_df$pred_se_low)

save(fdi_forecast_df, file = "R Data Sets/FDI Inputs/FDI Forecast DF")

# Chart FDI forecast

fdi_forecast_df$Year <- as.factor(fdi_forecast_df$Year)

fdi_forecast_df$Year2 <- as.POSIXct(fdi_forecast_df$Year, format = "%Y")

save(fdi_forecast_df, file = "R Data Sets/FDI Inputs/FDI Forecast DF")

jpeg("Plots/Forecasting Analysis/ARIMA Model 1.3.jpg", width = 500, height = 500)
ggplot() +
  geom_line(data = fdi_forecast_df, aes(x = fdi_forecast_df$Year2, y = fdi_forecast_df$`FDI, net inflows (% of GDP)`), size = 2) +
  geom_line(data = fdi_forecast_df, aes(x = fdi_forecast_df$Year2, y = fdi_forecast_df$pred_fdi_final), size = 2, color = "red", linetype = "dotted") +
  geom_ribbon(data = fdi_forecast_df, aes(x = fdi_forecast_df$Year2, ymin = fdi_forecast_df$pred_se_low, ymax = fdi_forecast_df$pred_se_high), size = 2, fill = "pink", alpha = 0.4) +
  scale_y_continuous("FDI, Net Inflows (% of GDP)") +
  scale_x_discrete("Year") +
  scale_x_datetime(breaks = date_breaks("3 years"), 
                   labels = date_format("%Y")) +
  ggtitle("ARIMA Forecast Model for World FDI Inflows, 1970 - 2023") +
  theme(axis.text.x = rotatedAxisElementText(90, "x")) +
  theme_tufte() +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0.5),
        axis.text.x = rotatedAxisElementText(90, "x"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1.0, "cm"),
        legend.spacing.x = unit(0.5, "cm")
        )
dev.off()

# Try Chow Test for 2006

Year <- fdi_aggregate$Year

sctest(lm_gr_AR2.1, type = "Chow", Year = 2009) # says no, p-value of 0.2441

sctest(lm_gr_AR2.1, type = "Chow", Year = 2006) # says no, p-value of 0.2441