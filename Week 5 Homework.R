#Week 5 Homework
# IST 387
# Vincent Marks

install.packages("imputeTS")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(MASS)
library(imputeTS)

######
#A.
air <- airquality

######
#B.
?airquality
# Using Data from New York Air Quality Measurements,
# the outcome variable - Ozone(mean amount of ozone in parts per billion/at 1300 to 1500 hours)
# predictor variables: 
#Solar Radiation(frequency band/0800-1200 hours), 
#Wind(average speed MPH at 0700 and 1000 hours),
#Temp(max daily temp in F)

######
#C.
air[!complete.cases(airquality),]

######
#D.
airqualityComplete <- na_interpolation(airquality)
air[!complete.cases(airqualityComplete),]

######
#E.

#This plot does not suggest a linear relationship between Solar R and Ozone
plot(airqualityComplete$Solar.R,airqualityComplete$Ozone)

#This plotting of data points on this plot does suggest the presence of linear relationship
#between the two axis, although there are a significant amount of outliers
plot(airqualityComplete$Wind, airqualityComplete$Ozone)

#This plot depicts a strong linear relationship between Temp and Ozone 
plot(airqualityComplete$Temp, airqualityComplete$Ozone)

######
#F.
lmWind <- lm(Ozone ~ Wind, data=airqualityComplete)
summary(lmWind)
# coefficient/beta weight: -4.5925
# P value: 2.15e-11 ***
# Adjusted R-Squared: .2527
# This report suggests that Wind is influential on the model but
# the P-value is very low and therefore, the model is not great.
# adding additional variables to the model could increase the quality.

######
#G.
lmAll <- lm(Ozone ~ Wind + Solar.R + Temp, data=airqualityComplete)
summary(lmAll)

######
#H. 
# the adjusted R-Squared value increased to .4207. This is better, but 
# the model is still poor in quality. 
# The significant predictors in the model are:
# Wind: 3.40e-05 ***
# Temp: 2.49e-09 ***
# Solar.R is not statistically significant