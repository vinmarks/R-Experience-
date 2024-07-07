#Week 4 Homework
#Vincent Marks
#IST 387
library(tidyverse)
library(ggplot2)
library(ggmap)
library(jsonlite)
library(readr)


#######
#A.
url="https://ist387.s3.us-east-2.amazonaws.com/data/cities.json"
pop <- jsonlite::fromJSON(url)

#######
#B.
#pop$population needs to be converted from chr to num
pop$population <- as.numeric(pop$population)
mean(pop$population)

#######
#C.
min(pop$population)

smallCity <- which.min(pop$population)
smallCity

pop$population[which.min(pop$population)]
#creates a subset using the index determined by the which.min() function
smallState <-pop$state[which.min(pop$population)]
smallState

######
#D.
abbr <- read_csv(url("https://ist387.s3.us-east-2.amazonaws.com/data/states.csv"))

######
#E.
#colnames() function permits the changing of column names
#after referencing the dataframe the desired column is placed within square brackets
colnames(abbr)[1] = "state"

#F.
dfNew <- merge(pop, abbr, by = "state")

######
#G.
#Merging abbr into pop created a dataframe that now includes abbreviations for the states 
# in the dataframe

######
#H.
states <- map_data("state")
map.simple <- ggplot(states) +
  geom_polygon(color="black", fill="white",
               aes(x=long,y=lat, group=group)) +
  geom_point(data=dfNew,
             aes(x=longitude,y=latitude, color="red"))
  coord_map()
map.simple
#I.
#This map is too busy. It is also difficult to distinguish each individual point from
# each other

#####
# Step 4

#J
# Creating a new data frame utilizing population from dfNew
dfSimple <- aggregate(dfNew$population,
                      #sorting by state column
                     by = list(dfNew$state),
                     #applying the sum function
                     FUN = sum)
#dfSimple has 51 rows, one for each state (including DC)
#creating new column  
dfSimple$name <- dfSimple$Group.1
#nullifying the new column
dfSimple$Group.1 <- NULL
dfSimple$statePop <- dfSimple$x
dfSimple$x <- NULL 

######
#K
#which.min/which.max used to determine the name of the state with the lowest and highest population
smallState <- dfSimple$name[which.min(dfSimple$statePop)]
smallState
bigState <- dfSimple$name[which.max(dfSimple$statePop)]
bigState

######
#Step 5
#K.

#create a new column called region that borrows info from the state name and then cast it to lowercase
dfSimple$region <- dfSimple$name
dfSimple$region <- tolower(dfSimple$region)
#merge states and dfSimple by region
mapData <- merge(states, dfSimple, by="region")

#create the map, shaded by population
myMap <- ggplot(mapData, aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=statePop),
               color = "white")
myMap






