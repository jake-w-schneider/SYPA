####################################################################

## World Bank MIGA
## This  file:
##  - Reads in full governance and economics datasets
##  - Cleans data sets
##  - Splits data into training, testing and lockbox
##
## Output:
##  - Components: 
##    1. CPIA_Final.RData
##    2. CSDR_Final.RData
##    3. Macro_Final.RData
##    4. PolityIV_Final.RData
##    5. WGI_Final.RData
##    6. ...
##  - FDI Inputs: FDI_Input.RData
## Author: Jake Schneider
## User: Jake Schneider
##Date Created:  06/19/2019
## Last Modified: 07/10/2019

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
#install.packages("Hmisc")
#install.packages("DataCombine")
#install.packages("panelaggregation")

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
library(Hmisc)
library(DataCombine)
library(panelaggregation)

## Setting a working directory 

getwd()

setwd("/Users/jschneids13/Desktop/World Bank - MIGA STT/Projects/Investment Analysis/MIGA-Private-/Full Research - By Country")

## Read in Dataset

country_list <- read_excel("Inputs/FDI Input Data.xlsx", sheet = "Country List")

macro <- read_excel("Inputs/FDI Input Data.xlsx", sheet = "Macro")

cpia <- read_excel("Inputs/FDI Input Data.xlsx", sheet = "CPIA")

wgi <- read_excel("Inputs/FDI Input Data.xlsx", sheet = "Worldwide Governance Indicators")

polity4 <- read_excel("Inputs/FDI Input Data.xlsx", sheet = "PolityIV")

csdr <- read_excel("Inputs/FDI Input Data.xlsx", sheet = "CSDR")

port_infrastracture <- read_csv("Inputs/Ports Infrastracture Data.csv")

price_level <- read_csv("Inputs/Price Level Data.csv")

geography_data <- read_csv("Inputs/phys_geo.csv")

population_data <- read_csv("Inputs/Population Data.csv")

## Data Cleaning: Macro Data Set

# Rename Variable Columns and Omit Series Code

colnames(macro)

setnames(macro, c("Country Name", "Country Code", "Series Name"), c("Country", "Code", "Indicator"))

macro$`Series Code` <- NULL

colnames(macro)

# Reshape

macro_long <- melt(setDT(macro), id.vars = c("Country", "Code", "Indicator"), variable.name = "Year")

macro_wide <- dcast(macro_long, Country + Code + Year ~ Indicator, fun.aggregate = mean)

# Drop NaN and Rename Data Set as Final

macro_final <- macro_wide[-c(34)]

# Save Macro Final Dataset

save(macro_final, file = "R Data Sets/Components/Macro_Final.RData")

# Merge Macro with Country List

colnames(country_list)
setnames(country_list, 
         c("country", "country_code", "region", "income_group", "lending_category", "other"), 
         c("Country", "Code", "Region","Income Group", "Lending Category", "Other"))
colnames(country_list)

fdi_input1 <- join(country_list, macro_final, by = "Code", type = "left", match = "all")

# Save FDI Input 1 Dataset

save(fdi_input1, file = "R Data Sets/FDI Inputs/FDI_Input.RData")


## Data Cleaning: CPIA Data Set

# Rename Variable Columns and Omit Series Code

colnames(cpia)

setnames(cpia, c("Country Name", "Country Code", "Series Name"), c("Country", "Code", "Indicator"))

setnames(cpia, "...18", "2018")

cpia$`Series Code` <- NULL

colnames(cpia)

# Reshape

cpia_long <- melt(setDT(cpia), id.vars = c("Country", "Code", "Indicator"), variable.name = "Year")

str(cpia_long)

cpia_wide <- dcast(cpia_long, Country + Code + Year ~ Indicator, fun.aggregate = mean)

# Rename Data Set as Final

cpia_final <- cpia_wide

# Save CPIA Final Dataset

save(cpia_final, file = "R Data Sets/Components/CPIA_Final.RData")

# Merge FDI Input with CPIA

fdi_input2 <- join(fdi_input1, cpia_final, by = c("Country", "Code", "Year"), type = "left", match = "all")

# Save FDI Input 2 Dataset

save(fdi_input2, file = "R Data Sets/FDI Inputs/FDI_Input.RData")

## Data Cleaning: WGI Data Set

# Rename Variable Columns and Omit Series Code

colnames(wgi)

setnames(wgi, c("country", "code", "indicator"), c("Country", "Code", "Indicator"))

colnames(wgi)

# Reshape

wgi_long <- melt(wgi, id.vars = c("Country", "Code", "Indicator"), variable.name = "Year")

str(wgi_long)

wgi_wide <- dcast(wgi_long, Country + Code + Year ~ Indicator, fun.aggregate = mean)

# Rename Data Set as Final

wgi_final <- wgi_wide

# Save WGI Final Dataset

save(wgi_final, file = "R Data Sets/Components/WGI_Final.RData")

# Merge FDI Input with CPIA

fdi_input3 <- join(fdi_input2, wgi_final, by = c("Country", "Code", "Year"), type = "left", match = "all")

# Save FDI Input 3 Dataset

save(fdi_input3, file = "R Data Sets/FDI Inputs/FDI_Input.RData")

## Data Cleaning: PolityIV Data Set

# Rename Variable Columns and Omit Series Code

colnames(polity4)

setnames(polity4, c("country", "code", "indicator"), c("Country", "Code", "Indicator"))

colnames(polity4)

# Reshape

polity4_long <- melt(polity4, id.vars = c("Country", "Code", "Indicator"), variable.name = "Year")

str(polity4_long)

polity4_wide <- dcast(polity4_long, Country + Code + Year ~ Indicator, fun.aggregate = mean)

# Rename Data Set as Final

polity4_final <- polity4_wide

# Save WGI Final Dataset

save(polity4_final, file = "R Data Sets/Components/Polity4_Final.RData")

# Merge FDI Input with CPIA

fdi_input4 <- join(fdi_input3, polity4_final, by = c("Country", "Code", "Year"), type = "left", match = "all")

# Save FDI Input 4 Dataset

save(fdi_input4, file = "R Data Sets/FDI Inputs/FDI_Input.RData")

## Data Cleaning: Country Sovereign Debt Rating (CSDR) Data Set

# Rename Variable Columns and Omit Series Code

colnames(csdr)

setnames(csdr, c("Country Code"), c("Code"))

colnames(csdr)

# Reshape

csdr <- melt(csdr, id.vars = c("Country", "Code", "Indicator"), variable.name = "Year")

csdr$value <- as.numeric(as.character(csdr$value))
str(csdr)

csdr_wide <- dcast(csdr, Country + Code + Year ~ Indicator, mean, value.var = "value")

# Rename Data Set as Final

csdr_final <- csdr_wide

# Save CSDR Final Dataset

save(csdr_final, file = "R Data Sets/Components/CSDR_Final.RData")

# Merge FDI Input with CPIA

fdi_input5 <- join(fdi_input4, csdr_final, by = c("Country", "Code", "Year"), type = "left", match = "all")

# Save FDI Input 5 Dataset

save(fdi_input5, file = "R Data Sets/FDI Inputs/FDI_Input.RData")

## Data Cleaning: UNCTAD Comtrade Dataset

# Function to read in data

comtrade_list <- as.character(
  excel_sheets("Inputs/UNCTAD Comtrade Data.xlsx"))

str(comtrade_list)
typeof(comtrade_list)

comtrade <- vector("list", length(comtrade_list))
comtrade_long <- data.frame()

for (i in seq_along(comtrade_list)) {
  comtrade <- 
    as.data.frame(lapply(i,
      read_excel, path = "Inputs/UNCTAD Comtrade Data.xlsx"))
  print(comtrade)
  comtrade_long <- rbind(comtrade_long, comtrade)
  print(comtrade_long)
}

# Rename Variable Columns and Omit Series Code

colnames(comtrade_long)

comtrade_long <- comtrade_long[c(2, 8, 10:11, 32)]

setnames(comtrade_long, c("Reporter", "Reporter.ISO", "Trade.Value..US..", "Trade.Flow"), c("Country", "Code", "Value", "Indicator"))

colnames(comtrade_long)

# Reshape

comtrade_wide <- dcast(comtrade_long, Country + Code + Year ~ Indicator, value.var = "Value")

# Rename Data Set as Final

comtrade_final <- comtrade_wide

# Save Comtrade Final Dataset

save(comtrade_final, file = "R Data Sets/Components/Comtrade_Final.RData")

# Merge FDI Input with Comtrade

fdi_input6 <- join(fdi_input5, comtrade_final, by = c("Country", "Code", "Year"), type = "left", match = "all")

# Save FDI Input 5 Dataset

save(fdi_input6, file = "R Data Sets/FDI Inputs/FDI_Input.RData")

## Port Infrastracuture Data

# Rename Variable Columns and Omit Series Code

colnames(port_infrastracture)

port_infrastracture$`Indicator Code` <- NULL

setnames(port_infrastracture, c("Country Name", "Country Code", "Indicator Name"), c("Country", "Code", "Indicator"))

colnames(port_infrastracture)

# Reshape

port_infrastracture <- melt(port_infrastracture, id.vars = c("Country", "Code", "Indicator"), variable.name = "Year")

str(port_infrastracture)

port_infrastracture <- dcast(port_infrastracture, Country + Code + Year ~ Indicator, mean, value.var = "value")

# Rename Data Set as Final

port_infrastracture_final <- port_infrastracture

# Save Port Infrastracture Final Dataset

save(port_infrastracture_final, file = "R Data Sets/Components/Port_Infrastracture_Final.RData")

# Merge FDI Input with Port Infrastracture

fdi_input7 <- join(fdi_input6, port_infrastracture_final, by = c("Country", "Code", "Year"), type = "left", match = "all")

# Save FDI Input 5 Dataset

save(fdi_input7, file = "R Data Sets/FDI Inputs/FDI_Input.RData")

## Data Cleaning: Price Level Data

# Rename Variable Columns and Omit Series Code

colnames(price_level)

price_level$`Indicator Code` <- NULL

setnames(price_level, c("Country Name", "Country Code", "Indicator Name"), c("Country", "Code", "Indicator"))

colnames(price_level)

# Reshape

price_level <- melt(price_level, id.vars = c("Country", "Code", "Indicator"), variable.name = "Year")

str(price_level)

price_level <- dcast(price_level, Country + Code + Year ~ Indicator, mean, value.var = "value")

# Rename Data Set as Final

price_level_final <- price_level

# Save Port Infrastracture Final Dataset

save(price_level_final, file = "R Data Sets/Components/Price_Level_Final.RData")

# Merge FDI Input with Port Infrastracture

fdi_input_panel <- join(fdi_input7, price_level_final, by = c("Country", "Code", "Year"), type = "left", match = "all")

# Save FDI Input 8 Dataset

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

## Data Cleaning: Population Data

# Rename Variable Columns and Omit Series Code

colnames(population_data)

population_data$`Series Code` <- NULL

setnames(population_data, 
         c("Country Name", 
          "Country Code", 
          "Series Name"), 
         c("Country", 
           "Code", 
           "Indicator"))

# Reshape

population_data <- melt(population_data, 
                        id.vars = c("Country", "Code", "Indicator"),
                        variable.name = "Year")
population_data$value <- as.numeric(population_data$value)
population_data <- dcast(population_data, 
                         Country + Code + Year ~ Indicator, 
                         mean, 
                         value.var = "value")

save(population_data, 
     file = "R Data Sets/Components/Population.RData")

fdi_input_panel <- join(fdi_input_panel, 
                        population_data, 
                        by = c("Country", "Code", "Year"), 
                        type = "left", 
                        match = "all")

save(fdi_input_panel, 
     file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

## Atlas of Economic Complexiy

load("R Data Sets/Components/Atlas of Economic Complexity_rankings.Rdata")

atlas_ec <- table

# Rename Variable Columns and Omit Series Code
colnames(atlas_ec)

atlas_ec$location_id <- NULL


setnames(atlas_ec, c("year", "code"), c("Year", "Code"))

colnames(atlas_ec)

# Rename Data Set as Final

atlas_ec_final <- atlas_ec

# Save Final Dataset

save(atlas_ec_final, file = "R Data Sets/Components/Atlas_Ec_Final.RData")

# Merge Data Sets

fdi_input_panel <- join(fdi_input_panel, atlas_ec_final, by = c("Code", "Year"), type = "left", match = "all")

# Save FDI Dataset

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

## Data Cleaning: Geography Data

# Change names

colnames(geography_data)
setnames(geography_data, c("country", "wbcode"), c("Country", "Code"))
colnames(geography_data)

# Merge Data Sets

fdi_input_panel <- join(fdi_input_panel, geography_data, by = c("Code", "Country"), type = "left", match = "all")

# Save FDI Dataset

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

## Data from IFC: Labor and Infrastracture Quality (?)

## Review Data

summary(fdi_input_panel)
fdi_input_panel <- fdi_input_panel[,-c(7)]
summary(fdi_input_panel)

which(is.na(fdi_input_panel$Year))

fdi_input_panel[9501,]

fdi_input_panel <- fdi_input_panel[-c(9501),]

summary(fdi_input_panel)

which.max(fdi_input_panel$`Central government debt, total (% of GDP)`)
fdi_input_panel[8902, c(1,2,7,8)]

which.min(fdi_input_panel$`Current account balance (% of GDP)`)
fdi_input_panel[5270:5280, c(1,2,7,10)]

which.max(fdi_input_panel$`Foreign direct investment, net (BoP, current US$)`)
fdi_input_panel[10330:10343, c(1,2,7,12)]

setnames(fdi_input_panel, c("Export", "Import", "Re-Export", "Re-Import"), c("Exports of Commodities to the World, US$", "Imports of Commodities to the World, US$", "Re-Exports of Commodities to the World, US$", "Re-Imports of Commodities to the World, US$"))

colnames(fdi_input_panel)

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input.RData")

fdi_input_panel$`Exports of Commodities to the World, (% of GDP)` <- (fdi_input_panel$`Exports of Commodities to the World, US$` / fdi_input_panel$`GDP (current US$)`)

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input.RData")

## Data Cleaning

# Set Data as Panel Data

head(fdi_input_panel)

fdi_input_panel <- pdata.frame(fdi_input_panel, index = c("Country", "Year"), drop.index = FALSE, row.names = TRUE)
head(fdi_input_panel)

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

# Analyze number of NA / Find Complete Cases

colnames(fdi_input_panel)

fdi_input_panel[!complete.cases(fdi_input_panel),]
fdi_input_panel[complete.cases(fdi_input_panel),] # no observations
colSums(is.na(fdi_input_panel)) # lots of missing values by collumn

fdi_input_panel[complete.cases(fdi_input_panel[,c(13 ,8 ,10, 17, 23, 24, 37, 41, 59, 74, 79, 80, 81, 82, 83, 84, 85, 86, 87)])] # no observations with complete cases

colnames(fdi_input_panel)

fdi_input_panel[complete.cases(fdi_input_panel[,c(13 ,8 ,10, 17, 23, 24, 37, 41, 59, 74, 79, 80, 81, 82, 83, 84, 85, 86)])] # no observations with complete cases

colnames(fdi_input_panel)
# fdi_input_panel[1:10,c(1, 81)]

fdi_input_panel[complete.cases(fdi_input_panel[,c(13 ,8 ,10, 17, 23, 24, 37, 41, 59, 74, 79, 80, 81, 82, 83, 84, 85)])] # no observations with complete cases

colnames(fdi_input_panel)
# fdi_input_panel[1:10,c(1, 81)]

# fdi_input_panel[complete.cases(fdi_input_panel[,c(11 ,6 ,8, 15, 21, 22, 35, 39, 57, 72, 77, 78, 79, 80, 81)])] # throws an error

# Recode Variables in PolityIV Database

colnames(fdi_input_panel)

fdi_input_panel$autoc[fdi_input_panel$autoc == 66 | fdi_input_panel$autoc == -66 ] <- NA
describe(fdi_input_panel$autoc)

fdi_input_panel$democ[fdi_input_panel$democ == 66 | fdi_input_panel$democ == -66 ] <- NA
describe(fdi_input_panel$democ)

fdi_input_panel$exconst[fdi_input_panel$exconst == 66 | fdi_input_panel$exconst == -66 ] <- NA
describe(fdi_input_panel$exconst)

fdi_input_panel$exrec[fdi_input_panel$exrec == 66 | fdi_input_panel$exrec == -66 ] <- NA
describe(fdi_input_panel$exrec)

fdi_input_panel$parcomp[fdi_input_panel$parcomp == 66 | fdi_input_panel$parcomp == -66 ] <- NA
describe(fdi_input_panel$parcomp)

fdi_input_panel$parreg[fdi_input_panel$parreg == 66 | fdi_input_panel$parreg == -66 ] <- NA
describe(fdi_input_panel$parreg)

fdi_input_panel$polcomp[fdi_input_panel$polcomp == 66 | fdi_input_panel$polcomp == -66 ] <- NA
describe(fdi_input_panel$polcomp)

fdi_input_panel$polity[fdi_input_panel$polity == 66 | fdi_input_panel$polity == -66 ] <- NA
describe(fdi_input_panel$polity)

fdi_input_panel$polity2[fdi_input_panel$polity2 == 66 | fdi_input_panel$polity2 == -66 ] <- NA
describe(fdi_input_panel$polity2)

fdi_input_panel$xconst[fdi_input_panel$xconst == 66 | fdi_input_panel$xconst == -66 ] <- NA
describe(fdi_input_panel$xconst)

fdi_input_panel$xrcomp[fdi_input_panel$xrcomp == 66 | fdi_input_panel$xrcomp == -66 ] <- NA
describe(fdi_input_panel$xrcomp)

fdi_input_panel$xropen[fdi_input_panel$xropen == 66 | fdi_input_panel$xropen == -66 ] <- NA
describe(fdi_input_panel$xropen)

fdi_input_panel$xrreg[fdi_input_panel$xrreg == 66 | fdi_input_panel$xrreg == -66 ] <- NA
describe(fdi_input_panel$xrreg)

colnames(fdi_input_panel)

fdi_input_panel$csdr_avg <- rowMeans(fdi_input_panel[,c(77:79)], na.rm = TRUE)
describe(fdi_input_panel$csdr_avg)

fdi_input_panel$csdr_avg <- as.numeric(fdi_input_panel$csdr_avg)
fdi_input_panel$csdr_avg[is.nan(fdi_input_panel$csdr_avg)] <- NA
describe(fdi_input_panel$csdr_avg)

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

# Visualize Missing Values
# https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html

jpeg("Plots/Missing Data/Missing Data Visualization - All.jpg", width = 1500, height = 1500)
vis_miss(fdi_input_panel, warn_large_data = FALSE) # too large
dev.off()

jpeg("Plots/Missing Data/Missing Data Visualization - Key Variables.jpg", width = 1500, height = 1500)
vis_miss(fdi_input_panel[,c(13 ,8 ,10, 17, 23, 24, 37, 41, 59, 74, 79:94)])
dev.off()

jpeg("Plots/Missing Data/Missing Upset Visualization - Base.jpg", width = 1500, height = 1500)
gg_miss_upset(fdi_input_panel)
dev.off()

jpeg("Plots/Missing Data/Missing Upset Visualization - All.jpg", width = 1500, height = 1500)
gg_miss_upset(fdi_input_panel[,c(13 ,8 ,10, 17, 23, 24, 37, 41, 59, 74, 79:94)], nsets = 10, nintersects = NA)
dev.off()

# jpeg("Plots/Missing Upset Visualization - All By Year.jpg", width = # 1500, height = 1500)
# gg_miss_upset(fdi_input_panel[,c(11 ,6 ,8, 15, 21, 22, 35, 39, 57, 72, # 77, 78, 79, 80, 81, 82, 83, 84, 85)], nsets = 10, nintersects = NA)
# dev.off()

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

## Complete Cases

fdi_complete_cases <- fdi_input_panel[complete.cases(fdi_input_panel$`Foreign direct investment, net inflows (% of GDP)` +
          fdi_input_panel$`Central government debt, total (% of GDP)`
        + fdi_input_panel$sp 
        + fdi_input_panel$mdy 
        + fdi_input_panel$fitch
        + fdi_input_panel$csdr_avg
        + fdi_input_panel$`GDP per capita (constant 2010 US$)`
        + fdi_input_panel$`GDP growth (annual %)` 
        + fdi_input_panel$`Exports of Commodities to the World, (% of GDP)`), ]

save(fdi_complete_cases, file = "R Data Sets/FDI Inputs/FDI_Complete_Cases.RData")

## Graphs of Variable Relationships

# Aggregating Data 

fdi_aggregate_country <- aggregate(fdi_input_panel[,8:95], list(fdi_input_panel$Country), mean, na.rm = TRUE)

# Plot Data

jpeg("Plots/Scatter Plots/Scatter Plot 1 -- FDI and Debt.jpg", width = 1500, height = 1500)
plot(fdi_aggregate_country$`Central government debt, total (% of GDP)`, 
     fdi_aggregate_country$`Foreign direct investment, net inflows (% of GDP)`, 
     main = "Variable Relationship",
     pch = 19,
     xlab = "Central government debt, total (% of GDP)",
     ylab = "Foreign direct investment, net inflows (% of GDP)",
     xlim = c(0, 150),
     ylim = c(0, 20))
dev.off()

describe(fdi_aggregate_country$csdr_avg)

jpeg("Plots/Scatter Plots/Scatter Plot 2 -- FDI and CSDR.jpg", width = 1500, height = 1500)
plot(fdi_aggregate_country$csdr_avg, 
     fdi_aggregate_country$`Foreign direct investment, net inflows (% of GDP)`,
     pch = 19,
     main = "Variable Relationship",
     xlab = "Country Sovereign Debt Rating",
     ylab = "Foreign direct investment, net inflows (% of GDP)",
     xlim = c(0, 20),
     ylim = c(0, 50)
)
dev.off()

jpeg("Plots/Scatter Plots/Scatter Plot 3 -- FDI and GDP Per Capita.jpg", width = 1500, height = 1500)
plot(fdi_aggregate_country$`GDP per capita (constant 2010 US$)`, 
     fdi_aggregate_country$`Foreign direct investment, net inflows (% of GDP)`,
     pch = 19,
     main = "Variable Relationship",
     xlab = "GDP per capita (constant 2010 US$",
     ylab = "Foreign direct investment, net inflows (% of GDP)",
     #xlim = c(0, 20),
     ylim = c(0, 50)
)
dev.off()

jpeg("Plots/Scatter Plots/Histogram GDP Per Capita.jpg", width = 1500, height = 1500)
hist(fdi_aggregate_country$`GDP per capita (constant 2010 US$)`)
dev.off()

fdi_aggregate_country$l.gdp_per_capita <- log(fdi_aggregate_country$`GDP per capita (constant 2010 US$)`)

jpeg("Plots/Scatter Plots/Histogram GDP Per Capita2.jpg", width = 1500, height = 1500)
hist(fdi_aggregate_country$l.gdp_per_capita)
dev.off()

fdi_input_panel$l.gdp_per_capita <- log(fdi_input_panel$`GDP per capita (constant 2010 US$)`)

fdi_input_panel$l.infrastracture_quality <- log(fdi_input_panel$`Quality of port infrastructure, WEF (1=extremely underdeveloped to 7=well developed and efficient by international standards)`)

fdi_input_panel$l.price_level_PPP <- log(fdi_input_panel$`Price level ratio of PPP conversion factor (GDP) to market exchange rate`)

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

fdi_input_panel$gdp_growth2 <- (fdi_input_panel$`GDP growth (annual %)`^2)

fdi_input_panel$gdp_per_capita2 <- (fdi_input_panel$`GDP per capita (constant 2010 US$)`^2)

fdi_input_panel$csdr_avg2 <- (fdi_input_panel$csdr_avg^2)
fdi_input_panel$debt2 <- (fdi_input_panel$`Central government debt, total (% of GDP)`^2)


fit <- lm(fdi_input_panel$`Foreign direct investment, net inflows (% of GDP)` ~ fdi_input_panel$`GDP growth (annual %)` + fdi_input_panel$gdp_growth2)

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

jpeg("Plots/Scatter Plots/Scatter Plot 4 -- FDI and GDP Growth.jpg", width = 1500, height = 1500)
plot(fdi_aggregate_country$`GDP growth (annual %)`, 
     fdi_aggregate_country$`Foreign direct investment, net inflows (% of GDP)`,
     pch = 19,
     main = "Variable Relationship",
     xlab = "GDP growth (annual %)",
     ylab = "Foreign direct investment, net inflows (% of GDP)",
     #xlim = c(0, 20),
     ylim = c(0, 50)
)
abline(lm(fdi_input_panel$`Foreign direct investment, net inflows (% of GDP)` ~ fdi_input_panel$`GDP growth (annual %)`))
dev.off()

# jpeg("Plots/Scatter Plots/Scatter Plot 4 -- FDI and GDP Growth1.jpg", # width = 1500, height = 1500)
# plot(fdi_aggregate_country$`GDP growth (annual %)`, 
#      fdi_aggregate_country$`Foreign direct investment, net inflows (% # of GDP)`,
#      pch = 19,
#      main = "Variable Relationship",
#      xlab = "GDP growth (annual %)",
#      ylab = "Foreign direct investment, net inflows (% of GDP)",
#      #xlim = c(0, 20),
#      ylim = c(0, 50)
# )
# abline(lm(fdi_input_panel$`Foreign direct investment, net inflows (% of # GDP)` ~ x_growth))
# dev.off()

# jpeg("Plots/Scatter Plots/Scatter Plot 4 -- FDI and GDP Growth2.jpg", # width = 1500, height = 1500)
# plot(fdi_aggregate_country$`GDP growth (annual %)`, 
#      fdi_aggregate_country$`Foreign direct investment, net inflows (% # of GDP)`,
#      pch = 19,
#      main = "Variable Relationship",
#      xlab = "GDP growth (annual %)",
#      ylab = "Foreign direct investment, net inflows (% of GDP)",
#      #xlim = c(0, 20),
#      ylim = c(0, 50)
# )
# lines(sort(fdi_input_panel$`GDP growth (annual %)`, fitted(fit), col = # "red", type = "b"))
# dev.off()

jpeg("Plots/Scatter Plots/Scatter Plot 5 -- FDI and Port Infrastracture", width = 1500, height = 1500)
plot(fdi_aggregate_country$`Quality of port infrastructure, WEF (1=extremely underdeveloped to 7=well developed and efficient by international standards)`, 
     fdi_aggregate_country$`Foreign direct investment, net inflows (% of GDP)`,
     pch = 19,
     main = "Variable Relationship",
     xlab = "Port Infrastracture",
     ylab = "Foreign direct investment, net inflows (% of GDP)",
     #xlim = c(0, 20),
     ylim = c(0, 50)
)
dev.off()

jpeg("Plots/Scatter Plots/Scatter Plot 5 -- FDI and Price Level", width = 1500, height = 1500)
plot(fdi_aggregate_country$`Price level ratio of PPP conversion factor (GDP) to market exchange rate`, 
     fdi_aggregate_country$`Foreign direct investment, net inflows (% of GDP)`,
     pch = 19,
     main = "Variable Relationship",
     xlab = "Port Infrastracture",
     ylab = "Foreign direct investment, net inflows (% of GDP)",
     #xlim = c(0, 20),
     ylim = c(0, 50)
)
dev.off()

# Creating Factor Variables for Region, Income, Etc.

fdi_input_panel$`Sub-Saharan Africa` <- as.numeric(fdi_input_panel$Region == "Sub-Saharan Africa" )

fdi_input_panel$`IDA` <- as.numeric(fdi_input_panel$`Lending Category` == "IDA" )

fdi_input_panel$`IBRD` <- as.numeric(fdi_input_panel$`Lending Category` == "IBRD" )

fdi_input_panel$`Not IDA or IBRD` <- as.numeric(fdi_input_panel$`Lending Category` != "IDA" & fdi_input_panel$`Lending Category` != "IBRD" )

fdi_input_panel$Region <- as.factor(fdi_input_panel$Region)
fdi_input_panel$Country <- as.factor(fdi_input_panel$Country)
fdi_input_panel$`Income Group` <- as.factor(fdi_input_panel$`Income Group`)
fdi_input_panel$`Lending Category` <- as.factor(fdi_input_panel$`Lending Category`)
fdi_input_panel$Other <- as.factor(fdi_input_panel$Other)

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

# Create Lagged Variables
# http://www.econ.uiuc.edu/~econ508/R/e-ta11_R.html#fn1

#fdi_input_panel <- fdi_input_panel[order(Country,
#                                         Year),]
#
#fdi_input_panel <- slide(data = fdi_input_panel, 
#                         Var = "Foreign direct investment, net inflows (% of GDP)",
#                         GroupVar = "Country",
#                         NewVar = "fdi_1",
#                         slideBy = 1) #first lag

# Lagged FDI

fdi_input_panel$fdi_1 <- Lag(fdi_input_panel$`Foreign direct investment, net inflows (% of GDP)`, -1)

fdi_input_panel$fdi_2 <- Lag(fdi_input_panel$`Foreign direct investment, net inflows (% of GDP)`, -2)

fdi_input_panel$fdi_3 <- Lag(fdi_input_panel$`Foreign direct investment, net inflows (% of GDP)`, -3)

# Lagged GDP per capita

fdi_input_panel$gdpcap_1 <- Lag(fdi_input_panel$`GDP per capita (constant 2010 US$)`, -1)

fdi_input_panel$gdpcap_2 <- Lag(fdi_input_panel$`GDP per capita (constant 2010 US$)`, -2)

fdi_input_panel$gdpcap_3 <- Lag(fdi_input_panel$`GDP per capita (constant 2010 US$)`, -3)

# Lagged GDP growth

fdi_input_panel$gdpgrowth_1 <- Lag(fdi_input_panel$`GDP growth (annual %)`, -1)

fdi_input_panel$gdpgrowth_2 <- Lag(fdi_input_panel$`GDP growth (annual %)`, -2)

fdi_input_panel$gdpgrowth_3 <- Lag(fdi_input_panel$`GDP growth (annual %)`, -3)

# Lagged CSDR

fdi_input_panel$csdr_1 <- Lag(fdi_input_panel$csdr_avg, -1)

fdi_input_panel$csdr_2 <- Lag(fdi_input_panel$csdr_avg, -2)

fdi_input_panel$csdr_3 <- Lag(fdi_input_panel$csdr_avg, -3)

# Lagged Current Account

fdi_input_panel$ca_1 <- Lag(fdi_input_panel$`Current account balance (% of GDP)`, -1)

fdi_input_panel$ca_2 <- Lag(fdi_input_panel$`Current account balance (% of GDP)`, -2)

fdi_input_panel$ca_3 <- Lag(fdi_input_panel$`Current account balance (% of GDP)`, -3)

# Central Government Debt

fdi_input_panel$cgd_1 <- Lag(fdi_input_panel$`Central government debt, total (% of GDP)`, -1)

fdi_input_panel$cgd_2 <- Lag(fdi_input_panel$`Central government debt, total (% of GDP)`, -2)

fdi_input_panel$cgd_3 <- Lag(fdi_input_panel$`Central government debt, total (% of GDP)`, -3)

# # Manually Lag FDI to start with 
# 
# n <- length(fdi_input_panel[,1])
# fdi_input_panel <- cbind(fdi_input_panel[3:n, 13], 
#                          fdi_input_panel[3:n, 13] - fdi_input_panel[2# :(n-1), 13], 
#                          fdi_input_panel[2:(n-1), 13] - fdi_input_panel# [1:(n-2), 13])

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

## Create Percent Changes

# Create logged variables

fdi_input_panel$l.fdi <- log(fdi_input_panel$`Foreign direct investment, net inflows (% of GDP)`)
fdi_input_panel$l.fdi_1 <- log(fdi_input_panel$fdi_1)
fdi_input_panel$l.fdi_2 <- log(fdi_input_panel$fdi_2)
fdi_input_panel$l.fdi_3 <- log(fdi_input_panel$fdi_3)

fdi_input_panel$l.cgd <- log(fdi_input_panel$`Central government debt, total (% of GDP)`)
fdi_input_panel$l.cgd_1 <- log(fdi_input_panel$cgd_1)
fdi_input_panel$l.cgd_2 <- log(fdi_input_panel$cgd_2)
fdi_input_panel$l.cgd_3 <- log(fdi_input_panel$cgd_3)

fdi_input_panel$l.gdpgrowth <- log(fdi_input_panel$`GDP growth (annual %)`)
fdi_input_panel$l.gdpgrowth_1 <- log(fdi_input_panel$gdpgrowth_1)
fdi_input_panel$l.gdpgrowth_2 <- log(fdi_input_panel$gdpgrowth_2)
fdi_input_panel$l.gdpgrowth_3 <- log(fdi_input_panel$gdpgrowth_3)

# Remove NaNs in GDP growth

fdi_input_panel$l.fdi <- as.numeric(fdi_input_panel$l.fdi)
fdi_input_panel$l.fdi[is.nan(fdi_input_panel$l.fdi)] <- NA
sum(is.nan(fdi_input_panel$l.fdi))

fdi_input_panel$l.fdi_1 <- as.numeric(fdi_input_panel$l.fdi_1)
fdi_input_panel$l.fdi_1[is.nan(fdi_input_panel$l.fdi_1)] <- NA
sum(is.nan(fdi_input_panel$l.fdi_1))

fdi_input_panel$l.fdi_2 <- as.numeric(fdi_input_panel$l.fdi_2)
fdi_input_panel$l.fdi_2[is.nan(fdi_input_panel$l.fdi_2)] <- NA
sum(is.nan(fdi_input_panel$l.fdi_2))

fdi_input_panel$l.fdi_3 <- as.numeric(fdi_input_panel$l.fdi_3)
fdi_input_panel$l.fdi_3[is.nan(fdi_input_panel$l.fdi_3)] <- NA
sum(is.nan(fdi_input_panel$l.fdi_3))

fdi_input_panel$l.gdpgrowth <- as.numeric(fdi_input_panel$l.gdpgrowth)
fdi_input_panel$l.gdpgrowth[is.nan(fdi_input_panel$l.gdpgrowth)] <- NA
sum(is.nan(fdi_input_panel$l.gdpgrowth))

fdi_input_panel$l.gdpgrowth_1 <- as.numeric(fdi_input_panel$l.gdpgrowth_1)
fdi_input_panel$l.gdpgrowth_1[is.nan(fdi_input_panel$l.gdpgrowth_1)] <- NA
sum(is.nan(fdi_input_panel$l.gdpgrowth_1))

fdi_input_panel$l.gdpgrowth_2 <- as.numeric(fdi_input_panel$l.gdpgrowth_2)
fdi_input_panel$l.gdpgrowth_2[is.nan(fdi_input_panel$l.gdpgrowth_2)] <- NA
sum(is.nan(fdi_input_panel$l.gdpgrowth_2))

fdi_input_panel$l.gdpgrowth_3 <- as.numeric(fdi_input_panel$l.gdpgrowth_3)
fdi_input_panel$l.gdpgrowth_3[is.nan(fdi_input_panel$l.gdpgrowth_3)] <- NA
sum(is.nan(fdi_input_panel$l.gdpgrowth_3))

# Take difference of logs to replicate percent changes

fdi_input_panel$d1.l.fdi <- fdi_input_panel$l.fdi - fdi_input_panel$l.fdi_1
fdi_input_panel$d2.l.fdi <- fdi_input_panel$l.fdi_1 - fdi_input_panel$l.fdi_2
fdi_input_panel$d3.l.fdi <- fdi_input_panel$l.fdi_2 - fdi_input_panel$l.fdi_3

fdi_input_panel$d1.l.fdi <- as.numeric(fdi_input_panel$d1.l.fdi)
fdi_input_panel$d1.l.fdi[is.nan(fdi_input_panel$d1.l.fdi)] <- NA
sum(is.nan(fdi_input_panel$d1.l.fdi))

fdi_input_panel$d2.l.fdi <- as.numeric(fdi_input_panel$d2.l.fdi)
fdi_input_panel$d2.l.fdi[is.nan(fdi_input_panel$d2.l.fdi)] <- NA
sum(is.nan(fdi_input_panel$d2.l.fdi))

fdi_input_panel$d3.l.fdi <- as.numeric(fdi_input_panel$d3.l.fdi)
fdi_input_panel$d3.l.fdi[is.nan(fdi_input_panel$d3.l.fdi)] <- NA
sum(is.nan(fdi_input_panel$d3.l.fdi))

fdi_input_panel$d1.l.cgd <- fdi_input_panel$l.cgd - fdi_input_panel$l.cgd_1
fdi_input_panel$d2.l.cgd <- fdi_input_panel$l.cgd_1 - fdi_input_panel$l.cgd_2
fdi_input_panel$d3.l.cgd <- fdi_input_panel$l.cgd_2 - fdi_input_panel$l.cgd_3

fdi_input_panel$d1.l.gdpgrowth <- fdi_input_panel$l.gdpgrowth - fdi_input_panel$l.gdpgrowth_1
fdi_input_panel$d2.l.gdpgrowth <- fdi_input_panel$l.gdpgrowth_1 - fdi_input_panel$l.gdpgrowth_2
fdi_input_panel$d3.l.gdpgrowth <- fdi_input_panel$l.gdpgrowth_2 - fdi_input_panel$l.gdpgrowth_3

sum(is.nan(fdi_input_panel$d1.l.fdi))

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

# Create weighted average FDI (net inflows) by country and year

fdi_input_panel$fdi_gdp_current <- fdi_input_panel$`GDP (current US$)` * fdi_input_panel$`Foreign direct investment, net inflows (% of GDP)`

#fdi_aggregate_year <- aggregate(fdi_input_panel$`GDP (current US$)`, by = list(fdi_input_panel$Year), sum, na.rm = TRUE)
# This does not work -- totals do not sum to World Bank reported totals. This is because the World Bank uses imputation methods to account for missing values.
# https://datahelpdesk.worldbank.org/knowledgebase/articles/198549-what-methods-are-used-to-calculate-aggregates-for

fdi_aggregate_world <- data.frame("Year" = 1969:2018,
                                  "World_GDP_(current$)" =
                                    c(2690560536122.40,	
                         2959099054887.35,
                         3271256318982.05,
                         3775048731722.90,
                         4605996164735.39,
                         5312389670516.26,
                         5916289803066.25,
                         6433942708103.73,
                         7273019218039.74,
                         8579238028282.67,
                         9964745487381.89,
                         11219095862038.20,
                         11620261279519.10,
                         11511155739748.20,
                         11740325468622.60,
                         12182908555023.30,
                         12796627959404.20,
                         15123897556213.60,
                         17205072772850.30,
                         19249897125271.90,
                         20098904133250.00,
                         22603413574265.30,
                         23942604354923.20,
                         25426088202627.30,
                         25838435865582.80,
                         27753161432235.70,
                         30865107530425.10,	
                         31549410906366.30,
                         31435984608120.10,
                         31367308863286.80,
                         32529369718064.50,
                         33581572220367.20,
                         33382508312956.70,
                         34669305041900.60,
                         38899898143812.50,
                         43811775548435.20,
                         47458960474871.50,
                         51442677941526.40,
                         57968740283107.80,
                         63616067321080.90,
                         60340070387368.00,
                         66036918504601.70,
                         73357858363945.80,
                         75045782254546.90,
                         77189953892236.60,
                         79296761326307.40,
                         75002374281095.50,
                         76103531034285.00,
                         80885580989211.60,
                         85790820876816.10))

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

# merge many to one

fdi_input_panel <- join(fdi_input_panel, fdi_aggregate_world, by = c("Year"), type = "left", match = "all")

# divide FDI * GDP / World GDP

fdi_input_panel$`Weighted Average FDI, Net Inflows (%)` <- fdi_input_panel$fdi_gdp_current / fdi_input_panel$World_GDP_.current..

save(fdi_input_panel, file = "R Data Sets/FDI Inputs/FDI_Input_Panel.RData")

# analyze chart to see if correct

## Trends 

# Aggregating Data 

# By Country

fdi_aggregate_country <- aggregate(fdi_input_panel[,c(8:100, 124:125)], list(fdi_input_panel$Country), mean, na.rm = TRUE)

fdi_aggregate_country <- as.numeric(fdi_aggregate_country)
fdi_aggregate_country[is.na(fdi_aggregate_country)] <- NA

save(fdi_aggregate_country, file = "R Data Sets/FDI Inputs/FDI_Aggregate_Country.RData")

is.na(fdi_aggregate_country$Foreign.direct.investment..net.inflows....of.GDP.)

# By Country, 2007-2017

fdi_aggregate_country2 <- aggregate(fdi_input_panel[,c(8:87, 110:111)], list(fdi_input_panel$Country),
                                   mean, 
                                   data = (fdi_input_panel$Year >= 2007 & fdi_input_panel$Year <= 2017),
                                   na.rm = TRUE)

fdi_aggregate_country2 <- as.numeric(fdi_aggregate_country)
fdi_aggregate_country2[is.na(fdi_aggregate_country2)] <- NA

save(fdi_aggregate_country2, file = "R Data Sets/FDI Inputs/FDI_Aggregate_Country2.RData")

# By Country, 2007-2017 & Low- and Lower-Middle Income

fdi_aggregate_country3 <- aggregate(fdi_input_panel[,c(8:87, 110:111)], list(fdi_input_panel$Country),
                                    mean, 
                                    data = (fdi_input_panel$Year >= 2007 & fdi_input_panel$Year <= 2017 & fdi_input_panel$Income.Group == "Low income" & fdi_input_panel$Income.Group == "Lower middle income"),
                                    na.rm = TRUE)

fdi_aggregate_country3 <- as.numeric(fdi_aggregate_country)
fdi_aggregate_country3[is.na(fdi_aggregate_country3)] <- NA

save(fdi_aggregate_country3, file = "R Data Sets/FDI Inputs/FDI_Aggregate_Country3.RData")

# By Region By Year

fdi_aggregate_region <- aggregate(fdi_input_panel[,8:80], by = list(fdi_input_panel$Region, fdi_input_panel$Year), mean, na.rm = TRUE)

fdi_aggregate_region <- as.data.frame(fdi_aggregate_region)
fdi_aggregate_region[is.na(fdi_aggregate_region)] <- NA

save(fdi_aggregate_region, file = "R Data Sets/FDI Inputs/FDI_Aggregate_Region.RData")

# By Region By Year 2

# fdi_netinflows <- fdi_input_panel$`Foreign direct investment, net # inflows (% of GDP)`
# gdp_currentppp <- fdi_input_panel$`GDP, PPP (current international $)`
# 
# fdi_aggregate_region2 <- ddply(fdi_input_panel, 
#                                .(fdi_input_panel$Region, 
#                                  fdi_input_panel$Year),
#                                function(x) data.frame(
#                                  weighted_fdi = 
#                                    weighted.mean(x$fdi_netinflows,
#                                                  x$gdp_currentppp, 
#                                                  na.rm = TRUE
#                                                  )))
# 
# 
# fdi_aggregate_region2 <- aggregate(fdi_input_panel$`GDP (current US$)`# , by = list(fdi_input_panel$Region, fdi_input_panel$Year), sum, na.rm # = TRUE)
# 
# setnames(fdi_aggregate_region2, c("x"), c("GDP (current US$) (sum)"))
# 
# fdi_aggregate_region <- join(fdi_aggregate_region, fdi_aggregate_regio# n2, by = c("Group.1", "Group.2"), type = "left", match = "all")
# 
# fdi_aggregate_region[,155]
# 
# fdi_aggregate_region$`FDI x Current GDP` <- fdi_aggregate_region[,6] * # fdi_aggregate_region[,155]
# 
# # fdi_aggregate_year <- aggregate(fdi_input_panel$`GDP (current US$)`, # by = list(fdi_input_panel$Year), sum, na.rm = TRUE)
# 
# setnames(fdi_aggregate_year, c("Group.1","x"), c("Group.2","GDP_region# _annual"))
# 
# fdi_aggregate_year <- fdi_aggregate_year[rep(row.names(fdi_aggregate_y# ear), 7),]
# 
# fdi_aggregate_year$Group.1 <- fdi_aggregate_region$Group.1
# 
# fdi_aggregate_region <- join(fdi_aggregate_region, fdi_aggregate_year, # by = c("Group.1", "Group.2"), type = "left", match = "all")

# fdi_aggregate_region$weighted_avg_fdi <- (fdi_aggregate_region$`FDI x # Current GDP` / fdi_aggregate_region$GDP_region_annual) * 100
#   
# save(fdi_aggregate_region, file = "R Data Sets/FDI Inputs/FDI_Aggregat# e_Region.RData")

# By Region By Year 3

fdi_aggregate_region3 <- aggregate(fdi_input_panel$`Weighted Average FDI, Net Inflows (%)`, by = list(fdi_input_panel$Region, fdi_input_panel$Year), sum, na.rm = TRUE)

setnames(fdi_aggregate_region3, c("x"), c("Weighted Average FDI, Net Inflows (%), By Region"))

fdi_aggregate_region <- join(fdi_aggregate_region, fdi_aggregate_region3, by = c("Group.1", "Group.2"), type = "left", match = "all")

save(fdi_aggregate_region, file = "R Data Sets/FDI Inputs/FDI_Aggregate_Region.RData")

# By Lending Category By Year

fdi_aggregate_lending_category <- aggregate(fdi_input_panel[,8:80], by = list(fdi_input_panel$Lending.Category, fdi_input_panel$Year), mean, na.rm = TRUE)

fdi_aggregate_lending_category <- as.data.frame(fdi_aggregate_lending_category)
fdi_aggregate_lending_category[is.na(fdi_aggregate_lending_category)] <- NA

save(fdi_aggregate_lending_category, file = "R Data Sets/FDI Inputs/FDI_Aggregate_Lending_Category.RData")

# By Lending Category By Year 2

# fdi_aggregate_lending_category2 <- aggregate(fdi_input_panel$`Weighted # Average FDI, Net Inflows (%)`, by = list(fdi_input_panel$`Lending # Category`, fdi_input_panel$Year), sum, na.rm = TRUE)
# 
# setnames(fdi_aggregate_lending_category2, c("x"), c("Weighted Average # FDI, Net Inflows (%), By Lending Category"))
# 
# fdi_aggregate_lending_category <- join(fdi_aggregate_lending_category, # fdi_aggregate_lending_category2, by = c("Group.1", "Group.2"), type = # "left", match = "all")
# 
# fdi_aggregate_lending_category$`Weighted Average FDI, Net Inflows (%), # By Lending Category` <- fdi_aggregate_lending_category$`Weighted # Average FDI, Net Inflows (%), By Lending Category` * 100
# 
# save(fdi_aggregate_region, file = "R Data Sets/FDI Inputs/FDI_Aggregat# e_Region.RData")
# 
# By Income Group By Year

fdi_aggregate_income_group <- aggregate(fdi_input_panel[,8:80], by = list(fdi_input_panel$Income.Group, fdi_input_panel$Year), mean, na.rm = TRUE)

fdi_aggregate_income_group <- as.data.frame(fdi_aggregate_income_group)
fdi_aggregate_income_group[is.na(fdi_aggregate_income_group)] <- NA
fdi_aggregate$Year <- fdi_aggregate$Group.1

save(fdi_aggregate_income_group, file = "R Data Sets/FDI Inputs/FDI_Aggregate_Income_Group.RData")

## Time Series Forecasting: Aggregate Data for the World By Year

# By Year for the World

fdi_aggregate <- aggregate(fdi_input_panel[,10:80], by = list(fdi_input_panel$Year), mean, na.rm = TRUE)

fdi_aggregate <- as.data.frame(fdi_aggregate)
fdi_aggregate[is.na(fdi_aggregate)] <- NA

save(fdi_aggregate, file = "R Data Sets/FDI Inputs/FDI_Aggregate.RData")

# Create Log FDI

fdi_aggregate$l.fdi <- log(fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP.)

# Create Lagged and Differenced Variables for Aggregate

fdi_aggregate$fdi_1 <- Lag(fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP., -1)

fdi_aggregate$fdi_2 <- Lag(fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP., -2)

fdi_aggregate$fdi_3 <- Lag(fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP., -3)

fdi_aggregate$fdi_4 <- Lag(fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP., -4)

fdi_aggregate$fdi_5 <- Lag(fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP., -5)

fdi_aggregate$fdi_6 <- Lag(fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP., -6)

fdi_aggregate$fdi_7 <- Lag(fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP., -7)

fdi_aggregate$fdi_8 <- Lag(fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP., -8)

fdi_aggregate$fdi_9 <- Lag(fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP., -9)

fdi_aggregate$fdi_10 <- Lag(fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP., -10)

fdi_aggregate$d1.fdi <- fdi_aggregate$Foreign.direct.investment..net.inflows....of.GDP. - fdi_aggregate$fdi_1

fdi_aggregate$d2.fdi <- fdi_aggregate$fdi_1 - fdi_aggregate$fdi_2
fdi_aggregate$d2.fdi2 <- Lag(fdi_aggregate$d1.fdi,-1)
fdi_aggregate$d3.fdi <- fdi_aggregate$fdi_2 - fdi_aggregate$fdi_3
fdi_aggregate$d3.fdi2 <- Lag(fdi_aggregate$d1.fdi,-2)
fdi_aggregate$d4.fdi <- fdi_aggregate$fdi_3 - fdi_aggregate$fdi_4
fdi_aggregate$d5.fdi <- fdi_aggregate$fdi_4 - fdi_aggregate$fdi_5
fdi_aggregate$d6.fdi <- fdi_aggregate$fdi_5 - fdi_aggregate$fdi_6
fdi_aggregate$d7.fdi <- fdi_aggregate$fdi_6 - fdi_aggregate$fdi_7
fdi_aggregate$d8.fdi <- fdi_aggregate$fdi_7 - fdi_aggregate$fdi_8
fdi_aggregate$d9.fdi <- fdi_aggregate$fdi_8 - fdi_aggregate$fdi_9
fdi_aggregate$d10.fdi <- fdi_aggregate$fdi_9 - fdi_aggregate$fdi_10

fdi_aggregate$dd1.fdi <- fdi_aggregate$d1.fdi - fdi_aggregate$d2.fdi
fdi_aggregate$dd2.fdi <- fdi_aggregate$d2.fdi - fdi_aggregate$d3.fdi


save(fdi_aggregate, file = "R Data Sets/FDI Inputs/FDI_Aggregate.RData")

# Create Log of Lags

fdi_aggregate$l.fdi_1 <- log(fdi_aggregate$fdi_1)
fdi_aggregate$l.fdi_2 <- log(fdi_aggregate$fdi_2)
fdi_aggregate$l.fdi_3 <- log(fdi_aggregate$fdi_3)
fdi_aggregate$l.fdi_4 <- log(fdi_aggregate$fdi_4)
fdi_aggregate$l.fdi_5 <- log(fdi_aggregate$fdi_5)
fdi_aggregate$l.fdi_6 <- log(fdi_aggregate$fdi_6)
fdi_aggregate$l.fdi_7 <- log(fdi_aggregate$fdi_7)
fdi_aggregate$l.fdi_8 <- log(fdi_aggregate$fdi_8)
fdi_aggregate$l.fdi_9 <- log(fdi_aggregate$fdi_9)
fdi_aggregate$l.fdi_10 <- log(fdi_aggregate$fdi_10)

# Create Growth Rates

fdi_aggregate$d.l.fdi_1 <- 100 * (fdi_aggregate$l.fdi_1 - fdi_aggregate$l.fdi)
fdi_aggregate$d.l.fdi_2 <- 100 * (fdi_aggregate$l.fdi_2 - fdi_aggregate$l.fdi_1)
fdi_aggregate$d.l.fdi_3 <- 100 * (fdi_aggregate$l.fdi_3 - fdi_aggregate$l.fdi_2)
fdi_aggregate$d.l.fdi_4 <- 100 * (fdi_aggregate$l.fdi_4 - fdi_aggregate$l.fdi_3)
fdi_aggregate$d.l.fdi_5 <- 100 * (fdi_aggregate$l.fdi_5 - fdi_aggregate$l.fdi_4)
fdi_aggregate$d.l.fdi_6 <- 100 * (fdi_aggregate$l.fdi_6 - fdi_aggregate$l.fdi_5)

save(fdi_aggregate, file = "R Data Sets/FDI Inputs/FDI_Aggregate.RData")

# Create difference of growth rates

fdi_aggregate$d.growth.fdi_1 <- fdi_aggregate$d.l.fdi_2 - fdi_aggregate$d.l.fdi_1

fdi_aggregate$d.growth.fdi_2 <- fdi_aggregate$d.l.fdi_3 - fdi_aggregate$d.l.fdi_2

fdi_aggregate$d.growth.fdi_3 <- fdi_aggregate$d.l.fdi_4 - fdi_aggregate$d.l.fdi_3

fdi_aggregate$d.growth.fdi_4 <- fdi_aggregate$d.l.fdi_5 - fdi_aggregate$d.l.fdi_4

fdi_aggregate$d.growth.fdi_5 <- fdi_aggregate$d.l.fdi_6 - fdi_aggregate$d.l.fdi_5

save(fdi_aggregate, file = "R Data Sets/FDI Inputs/FDI_Aggregate.RData")

# ## Create other lagged and growth rate variables for ADL model
# 
# # Central Gov Debt
# 
# # CSDR
# 
# fdi_aggregate$csdr_1 <- Lag(fdi_aggregate$csdr_avg, -1)
# fdi_aggregate$csdr_2 <- Lag(fdi_aggregate$csdr_avg, -2)
# fdi_aggregate$csdr_3 <- Lag(fdi_aggregate$csdr_avg, -3)
# fdi_aggregate$csdr_4 <- Lag(fdi_aggregate$csdr_avg, -4)
# fdi_aggregate$csdr_5 <- Lag(fdi_aggregate$csdr_avg, -5)
# fdi_aggregate$csdr_6 <- Lag(fdi_aggregate$csdr_avg, -6)
# 
# fdi_aggregate$l.csdr_1 <- log(fdi_aggregate$csdr_1)
# fdi_aggregate$l.csdr_2 <- log(fdi_aggregate$csdr_2)
# fdi_aggregate$l.csdr_3 <- log(fdi_aggregate$csdr_3)
# fdi_aggregate$l.csdr_4 <- log(fdi_aggregate$csdr_4)
# fdi_aggregate$l.csdr_5 <- log(fdi_aggregate$csdr_5)
# fdi_aggregate$l.csdr_6 <- log(fdi_aggregate$csdr_6)
# 
# fdi_aggregate$d.l.csdr_1 <- 100 * (fdi_aggregate$l.csdr_1 - fdi_aggreg# ate$l.fdi_2)
# fdi_aggregate$d.l.csdr_2 <- 100 * (fdi_aggregate$l.csdr_2 - fdi_aggreg# ate$l.fdi_3)
# fdi_aggregate$d.l.csdr_3 <- 100 * (fdi_aggregate$l.csdr_3 - fdi_aggreg# ate$l.fdi_4)
# fdi_aggregate$d.l.csdr_4 <- 100 * (fdi_aggregate$l.csdr_4 - fdi_aggreg# ate$l.fdi_5)
# fdi_aggregate$d.l.csdr_5 <- 100 * (fdi_aggregate$l.csdr_5 - fdi_aggreg# ate$l.fdi_6)
# 
# save(fdi_aggregate, file = "R Data Sets/FDI Inputs/FDI_Aggregate# .RData")
# 
# # GDP Per Capita
# 
# fdi_aggregate$gdpcap_1 <- Lag(fdi_aggregate$`GDP per capita (constant # 2010 US$)`, -1)
# fdi_aggregate$gdpcap_2 <- Lag(fdi_aggregate$`GDP per capita (constant # 2010 US$)`, -2)
# fdi_aggregate$gdpcap_3 <- Lag(fdi_aggregate$`GDP per capita (constant # 2010 US$)`, -3)
# fdi_aggregate$gdpcap_4 <- Lag(fdi_aggregate$`GDP per capita (constant # 2010 US$)`, -4)
# fdi_aggregate$gdpcap_5 <- Lag(fdi_aggregate$`GDP per capita (constant # 2010 US$)`, -5)
# fdi_aggregate$gdpcap_6 <- Lag(fdi_aggregate$`GDP per capita (constant # 2010 US$)`, -6)
# 
# fdi_aggregate$l.gdpcap_1 <- log(fdi_aggregate$gdpcap_1)
# fdi_aggregate$l.gdpcap_2 <- log(fdi_aggregate$gdpcap_2)
# fdi_aggregate$l.gdpcap_3 <- log(fdi_aggregate$gdpcap_3)
# fdi_aggregate$l.gdpcap_4 <- log(fdi_aggregate$gdpcap_4)
# fdi_aggregate$l.gdpcap_5 <- log(fdi_aggregate$gdpcap_5)
# fdi_aggregate$l.gdpcap_6 <- log(fdi_aggregate$gdpcap_6)
# 
# fdi_aggregate$d.l.gdpcap_1 <- 100 * (fdi_aggregate$l.gdpcap_1 - # fdi_aggregate$l.gdpcap_2)
# fdi_aggregate$d.l.gdpcap_2 <- 100 * (fdi_aggregate$l.gdpcap_2 - # fdi_aggregate$l.gdpcap_3)
# fdi_aggregate$d.l.gdpcap_3 <- 100 * (fdi_aggregate$l.gdpcap_3 - # fdi_aggregate$l.gdpcap_4)
# fdi_aggregate$d.l.gdpcap_4 <- 100 * (fdi_aggregate$l.gdpcap_4 - # fdi_aggregate$l.gdpcap_5)
# fdi_aggregate$d.l.gdpcap_5 <- 100 * (fdi_aggregate$l.gdpcap_5 - # fdi_aggregate$l.gdpcap_6)
# 
# save(fdi_aggregate, file = "R Data Sets/FDI Inputs/FDI_Aggregate# .RData")
# 
# # GDP Growth
# # Note: GDP Growth is already a growth rate so no need to calculate
# 
# fdi_aggregate$gdpgrowth_1 <- Lag(fdi_aggregate$`GDP growth (annual %)`# , -1)
# fdi_aggregate$gdpgrowth_2 <- Lag(fdi_aggregate$`GDP growth (annual %)`# , -2)
# fdi_aggregate$gdpgrowth_3 <- Lag(fdi_aggregate$`GDP growth (annual %)`# , -3)
# fdi_aggregate$gdpgrowth_4 <- Lag(fdi_aggregate$`GDP growth (annual %)`# , -4)
# fdi_aggregate$gdpgrowth_5 <- Lag(fdi_aggregate$`GDP growth (annual %)`# , -5)
# fdi_aggregate$gdpgrowth_6 <- Lag(fdi_aggregate$`GDP growth (annual %)`# , -6)
# 
# save(fdi_aggregate, file = "R Data Sets/FDI Inputs/FDI_Aggregate# .RData")
# 
# # Exports of Commodities to the World
# 
# fdi_aggregate$exports_1 <- Lag(fdi_aggregate$`Exports of Commodities # to the World, (% of GDP)`, -1)
# fdi_aggregate$exports_2 <- Lag(fdi_aggregate$`Exports of Commodities # to the World, (% of GDP)`, -2)
# fdi_aggregate$exports_3 <- Lag(fdi_aggregate$`Exports of Commodities # to the World, (% of GDP)`, -3)
# fdi_aggregate$exports_4 <- Lag(fdi_aggregate$`Exports of Commodities # to the World, (% of GDP)`, -4)
# fdi_aggregate$exports_5 <- Lag(fdi_aggregate$`Exports of Commodities # to the World, (% of GDP)`, -5)
# fdi_aggregate$exports_6 <- Lag(fdi_aggregate$`Exports of Commodities # to the World, (% of GDP)`, -6)
# 
# fdi_aggregate$l.exports_1 <- log(fdi_aggregate$exports_1)
# fdi_aggregate$l.exports_2 <- log(fdi_aggregate$exports_2)
# fdi_aggregate$l.exports_3 <- log(fdi_aggregate$exports_3)
# fdi_aggregate$l.exports_4 <- log(fdi_aggregate$exports_4)
# fdi_aggregate$l.exports_5 <- log(fdi_aggregate$exports_5)
# fdi_aggregate$l.exports_6 <- log(fdi_aggregate$exports_6)
# 
# fdi_aggregate$d.l.exports_1 <- 100 * (fdi_aggregate$l.exports_1 - # fdi_aggregate$l.exports_2)
# fdi_aggregate$d.l.exports_2 <- 100 * (fdi_aggregate$l.exports_2 - # fdi_aggregate$l.exports_3)
# fdi_aggregate$d.l.exports_3 <- 100 * (fdi_aggregate$l.exports_3 - # fdi_aggregate$l.exports_4)
# fdi_aggregate$d.l.exports_4 <- 100 * (fdi_aggregate$l.exports_4 - # fdi_aggregate$l.exports_5)
# fdi_aggregate$d.l.exports_5 <- 100 * (fdi_aggregate$l.exports_5 - # fdi_aggregate$l.exports_6)
# 
# save(fdi_aggregate, file = "R Data Sets/FDI Inputs/FDI_Aggregate# .RData")
# 
# # Current Account Balance
# 
# fdi_aggregate$ca_1 <- Lag(fdi_aggregate$`Current account balance (% of # GDP)`, -1)
# fdi_aggregate$ca_2 <- Lag(fdi_aggregate$`Current account balance (% of # GDP)`, -2)
# fdi_aggregate$ca_3 <- Lag(fdi_aggregate$`Current account balance (% of # GDP)`, -3)
# fdi_aggregate$ca_4 <- Lag(fdi_aggregate$`Current account balance (% of # GDP)`, -4)
# fdi_aggregate$ca_5 <- Lag(fdi_aggregate$`Current account balance (% of # GDP)`, -5)
# fdi_aggregate$ca_6 <- Lag(fdi_aggregate$`Current account balance (% of # GDP)`, -6)
# 
# save(fdi_aggregate, file = "R Data Sets/FDI Inputs/FDI_Aggregate# .RData")
# 
# # Quality of Port Infrastracture
# 
# fdi_aggregate$port_1 <- Lag(fdi_aggregate$`Quality of port infrastruct# ure, WEF (1=extremely underdeveloped to 7=well developed and efficient # by international standards)`, -1)
# fdi_aggregate$port_2 <- Lag(fdi_aggregate$`Quality of port infrastruct# ure, WEF (1=extremely underdeveloped to 7=well developed and efficient # by international standards)`, -2)
# fdi_aggregate$port_3 <- Lag(fdi_aggregate$`Quality of port infrastruct# ure, WEF (1=extremely underdeveloped to 7=well developed and efficient # by international standards)`, -3)
# fdi_aggregate$port_4 <- Lag(fdi_aggregate$`Quality of port infrastruct# ure, WEF (1=extremely underdeveloped to 7=well developed and efficient # by international standards)`, -4)
# fdi_aggregate$port_5 <- Lag(fdi_aggregate$`Quality of port infrastruct# ure, WEF (1=extremely underdeveloped to 7=well developed and efficient # by international standards)`, -5)
# fdi_aggregate$port_6 <- Lag(fdi_aggregate$`Quality of port infrastruct# ure, WEF (1=extremely underdeveloped to 7=well developed and efficient # by international standards)`, -6)
# 
# save(fdi_aggregate, file = "R Data Sets/FDI Inputs/FDI_Aggregate# .RData")
# 
# # Price Level
# 
# # SITC_ECI
# 
# # Polity2
# 
# fdi_aggregate$pol2_1 <- Lag(fdi_aggregate$polity2, -1)
# fdi_aggregate$pol2_2 <- Lag(fdi_aggregate$polity2, -2)
# fdi_aggregate$pol2_3 <- Lag(fdi_aggregate$polity2, -3)
# fdi_aggregate$pol2_4 <- Lag(fdi_aggregate$polity2, -4)
# fdi_aggregate$pol2_5 <- Lag(fdi_aggregate$polity2, -5)
# fdi_aggregate$pol2_6 <- Lag(fdi_aggregate$polity2, -6)
# 
# save(fdi_aggregate, file = "R Data Sets/FDI Inputs/FDI_Aggregate# .RData")
# 
## Prediction: Splitting Data into Lockbox, Training and Testing







