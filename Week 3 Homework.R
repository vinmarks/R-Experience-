#Week 3 Homework
#Vincent Marks
#IST 387
#Load Libraries
library(vctrs)
library(knitr)
library(dplyr)
library(tidyverse)
library(readxl)
library(RCurl)   # For access to Internet data
library(jsonlite) # For decoding JSON

dataSet <- getURL("https://ist387.s3.us-east-2.amazonaws.com/data/role.json")
readLines <- jsonlite::fromJSON(dataSet)
df <- readLines$objects$person

######
#A
view(df)

######
#B
# 1. The dataset pertains to the personal information of elected persons.
#       The information provided links each official with their online presences as well.
# 2.
nrow(df)
# There are 100 rows. Found either by running the nrow() function or by noting the amount of
# observations associated with the data frame in the environment window.
# The data within the rows represents the various information associated with each individual
# of interest. 
# 3.
ncol(df)
#There are 17 rows as denoted by the environment window's description of the data frame as having 17 variables.
# the columns represent the categories or attributes of data associated with each individual.

######
#C. Answer = 24
femaleSenators <- df %>%
  filter(gender == 'female') %>%
  nrow()
print(femaleSenators)

######
#D. Answer = 73
ytAccount <- df %>%
  filter(youtubeid != "") %>%
  nrow()
print(ytAccount)

######
#E. Answer = 16
ytFemale <- df %>%
  filter(gender == 'female',
         youtubeid != "") %>% 
  nrow()
print(ytFemale)

######
#F.
youtubeWomen <- df[df$gender == "female" & df$youtubeid != "", ]
# creation of youtubeWomen created rows void of information . the na.omit() function eliminates those rows.
youtubeWomen <- na.omit(youtubeWomen)

######
#G. This code will create a new attribute within the youtubeWomen data frame
# that extracts the first four elements of the birthday attributes which, in this case, is the year of birth
# the substr() function is guided by the index values of the dersired elements.
# the desired start and stop p index values are seperated by the comma
youtubeWomen$year <- substr(youtubeWomen$birthday,1,4)
#cast the characters within each row of 'year' as integers for later use
youtubeWomen$year <- as.integer(youtubeWomen$year)

######
#H. 
mean(youtubeWomen$year)


######
#I.
# The distribution of the data is relatively normal. It suggests that an online presence via youtube is more common 
# among the younger senators.  
hist(youtubeWomen$year)

