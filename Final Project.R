#IST 387 Final Project
#Vincent Marks

install.packages("rworldmap")
library(tidyverse)
library(readr)
library(ggplot2)
library(ggmap)
library(dplyr)
library(rworldmap)
library(caret)
library(rpart)
library(rpart.plot)

######
#1.
#import Resort01.csv. RStudio monitors "Documents" folder, copy file there after
#download
dataBookings <- read.csv("Resort01.csv")
#40060 Observations of 20 Variables
#Customer related data
bookings <- as.data.frame(dataBookings)

######
#2.
#returns the number of NA entries. Result is 0
sum(is.na(bookings))
#returns the number of NA entries sorted by column. Result is 0 per column
sapply(bookings, function(x) sum(is.na(x)))
#returns a boolean for presence of data
complete.cases(bookings)
#
bookings[!complete.cases(bookings),]

######
#3.
#displays the number of stays that included a specific number weekend nights
table(bookings$StaysInWeekendNights)
#displays the number of canceled and not canceled stays
#the number 
table(bookings$IsCanceled)
#displays the number of stays booked with babies and no babies
#a large majority of stays included 0 baby guests
table(bookings$Babies)

######
#4.

#This histogram provides insight into the relationship between the amount of leadtime when booking and the rate of cancellation. The data suggests that the 
#shorter the leadtime, the less cancellations
ggplot(bookings, aes(x = LeadTime, fill = factor(IsCanceled))) +
  geom_histogram(position = "identity", binwidth = 10, color = "black", alpha = 0.7) +
  labs(title = "Leadtime/Cancellation Status", x = "Leadtime(Days)", y = "Number of Bookings") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()

#This histogram suggests a correlation between a lower rate of cancellation occurance when the customer has specific requests upon
#booking.
ggplot(bookings, aes(x = TotalOfSpecialRequests, fill = factor(IsCanceled))) +
  geom_histogram(position = "identity", binwidth = 1, color = "black", alpha = 0.7) +
  labs(title = "Number of Requests/Cancellation Status", x = "Number of Requests", y = "Number of Bookings") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()

#This histogram is fairly conclusive in eluding to the fact that far less cancellations occur when the booking undergoes
#at least one change
ggplot(bookings, aes(x = BookingChanges, fill = factor(IsCanceled))) +
  geom_histogram(position = "identity", binwidth = 1, color = "black", alpha = 0.7) +
  labs(title = "Number of Booking Changes/Cancellation Status", x = "Number of Changes", y = "Number of Bookings") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()

######
#5.
#PRT has the most bookings
byCountry <- table(bookings$Country)
which.max(byCountry)

######
#6.

country <- aggregate(IsCanceled ~ Country, data = bookings, FUN = sum)
colnames(country) <- c("name","numCanceled")


country$numCanceled[which.max(country$numCanceled)]
#creates a subset using the index determined by the which.max() function
mostCancel <-country$name[which.max(country$numCanceled)]
mostCancel

#######
#7.
sPDF <- joinCountryData2Map(country, joinCode="ISO3", 
                            nameJoinColumn="name") 
map<-mapCountryData(sPDF, nameColumnToPlot='numCanceled', 
                    catMethod="logFixedWidth")
#This map appears as a "heat map" where the color placed over a country represents the data pertaining to each country within a range.
#The range reflects the numCanceled column and ranges from 1 to 7438. This range is illustrated through the progression of color 
#from yellow to red. This provides the viewer with a quick and efficient reference relevant to the number of cancellations by country.

######
#8.
book <- data.frame(leadTime=bookings$LeadTime, 
                   staysWeekend=bookings$StaysInWeekendNights, 
                   staysWeek=bookings$StaysInWeekNights, 
                   adults=bookings$Adults, 
                   children=bookings$Children, 
                   babies=bookings$Babies, 
                   prevCancellations=bookings$PreviousCancellations, 
                   specialRequests=bookings$TotalOfSpecialRequests, 
                   canceled=as.factor(bookings$IsCanceled)) 

bookTrainList <- createDataPartition(y=book$canceled,p=.70,list=FALSE)

######
#9.

bookTrainSet <- book[bookTrainList,]
bookTestSet <- book[-bookTrainList,]

#rpart 
cartTree <- rpart(canceled ~ ., data=bookTrainSet)
prp(cartTree, faclen=0, cex=.8,extra=1)
predictValuesRPART <- predict(cartTree, newdata=bookTestSet, type="class")
confusionRPART <- confusionMatrix(predictValuesRPART, as.factor(bookTestSet$canceled))
confusionRPART
varImp(cartTree)

#SVM
set.seed(123)
fit2 <- train(canceled ~ ., data=bookTrainSet, method="svmRadial",preProc=c("center","scale"))
predictValuesSVM <- predict(fit2,newdata=bookTestSet)
confusionSVM <- confusionMatrix(predictValuesSVM, bookTestSet$canceled)
confusionSVM


#SVM's employ boundaries that are sensitive to elements that create error.
#This concept is called a cost parameter, the higher the value the more specialized 
#the model. Allowing bend within these vectors results in a reduced number of
#mis-classifications in regard to the data plots associated with the model. This flexibilty 
#is highlighted by comparison to a linear model, where the regression line is not flexible
#and data points fall on either side of it. In this case, the SVM model is able to give extra
#consideration to a data point when classifying it.

######
#10.
#The research conducted shows that 38.4% of bookings result in cancellation. Firstly,
# information was acquired by comparing the data associated with canceled and non-canceled bookings.
#There is a strong correlation between non-cancelled bookings and short leadtime, 
#the data suggests that bookings with longer lead time have a higher cancellation rate.
#Also, analysis of the data found that bookings in which the guest makes requests, or changes
#to their booking have a higher tendency to be a completed booking, rather than a cancellation. 
#It was found that cancellations occur most at properties within the country of Portugal.
#Upon utilization of a CART, the model suggests that the first predictive quality associated with
#cancellations based on the data provided is a leadtime of 18 days.Previous Cancellations, special requests, and children 
#are also considerations generated by the model. This model has an accuracy of 74.5%. 
#Given this information, any attempt to reduce cancellations should be concerned with leadtime.
#should a booking have a lead time greater than 18 days(perhaps round to 20 for convenience), imposing
#a cancellation fee could deter cancellations. Additionally, encouraging guest customization of a booking
#could result in fewer cancellations as two variables involving customization resulted
#in a lower cancellation rate when present. Perhaps the flexibility provided in these transactions,
#allowed the guest to keep the booking rather than cancel due to unforeseen changes in requirements.


