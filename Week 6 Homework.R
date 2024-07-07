#Week 6 Homework
#Vincent Marks
#IST 387

install.packages("kernlab")
library(tidyverse)
library(ggplot2)
library(caret)
library(kernlab)

data("GermanCredit")
#subset with the first ten columns of GermanCredit
subCredit <- GermanCredit[,1:10]

######
#1.
#displays the 10 chosen attributes and the data types consisting of int, num, and
#Factor
str(subCredit)
#describes the use of 62 attributes to determine the predicted classification of 'good'
#or 'bad' credit
help("GermanCredit")

######
#2.
#creating the training list via the createDataPartition function
#taking 40% of data from the Class attribute
#list=FALSE returns a numeric vector of indices, ensuring that each element
#correspondes to the original row indices of the original dataset
trainList <- createDataPartition(y=subCredit$Class,p=.40,list=FALSE)

######
#3. 
#trainList contains 400 case numbers

######
#4.
#trainList consists of 400 rows and 2 columns. The first column pertain to the indices of the 
#data as it now pertains to trainList. The second column's(named 'ReSample') data per row correspondes to the indice 
#from the original data set(subCredit). 40% of subCredit was partitioned based on subCredit$Class, this partition was done randomly.
#set.seed() would increase one's ability to gather unique partitions in the future.
#the first column states the sequential order in which the partition occured, 'ReSample' is the row number within subCredit$Class that
#the data was collected from. 

######
#5.
#trainSet contains 400 observations of 10 variables.
#These 10 variables are the same as the ten variables in subCredit
trainSet <- subCredit[trainList,]

######
#6.
#-trainList takes the 60% of data that was not copied from subCredit to make
# trainList.
#testSet is a dataframe consisting of 600 observations of 10 variables
testSet <- subCredit[-trainList,]

######
#7.
boxplot(Duration ~ Class, data=subCredit,
        varwidth = TRUE, log = "y", las=1)
boxplot(Amount ~ Class, data=subCredit,
        varwidth = TRUE, log = "y", las=1)
boxplot(InstallmentRatePercentage ~ Class, data=subCredit,
        varwidth = TRUE, log = "y", las=1)
boxplot(ResidenceDuration ~ Class, data=subCredit,
        varwidth = TRUE, log = "y", las=1)
boxplot(Age ~ Class, data=subCredit,
        varwidth = TRUE, log = "y", las=1)
boxplot(NumberExistingCredits ~ Class, data=subCredit,
        varwidth = TRUE, log = "y", las=1)
boxplot(NumberPeopleMaintenance ~ Class, data=subCredit,
        varwidth = TRUE, log = "y", las=1)
#these last two attributes are binary (yes or no)
boxplot(Telephone ~ Class, data=subCredit,
        varwidth = TRUE, log = "y", las=1)
boxplot(ForeignWorker ~ Class, data=subCredit,
        varwidth = TRUE, log = "y", las=1)

######
#8.
set.seed(124)
#same as SVM train(), except for the Kernal Argument. "rbfdot" is commonly used
svmModel <- ksvm(Class ~ ., data=subCredit, kernel="rbfdot")
svmModel

######
#9.
# running line 84 (svmModel) returns this information:

#==============================================
#Support Vector Machine object of class "ksvm" 

#SV type: C-svc  (classification) 
#parameter : cost C = 1 

#Gaussian Radial Basis kernel function. 
#Hyperparameter : sigma =  0.0971561678360502 

#Number of Support Vectors : 652 

#Objective Function Value : -552.1462 
#Training error : 0.256 
#=============================================

# the lower the training error value, the better the performance. This means that 
# the model has an accuracy of about 75% which is pretty good. 

#######
#10.
prediction <- predict(svmModel, newdata = trainSet, type="response")


#######
#11.
#attempting to use 'trainSet' as a whole generated errors. Once the
#'Class' attribute was specified, the table function generated a result.
#running str() identified both 'trainSet$Class' 'prediction' as factors, 
#thus enabling the table function to operate; whereas 'trainSet' is a dataframe. 
str(trainSet$Class)
str(prediction)
#table() takes two arguements, Actual and Prediction
confusion <- table(Actual = trainSet$Class, Prediction = prediction)

#######
#12.
#the table function generated this confusion matrix:
#Actual Bad Good
#Bad   26   94
#Good  15  265

diag(confusion)
#results of diag()
#shows the true positives of the matrix
# Bad Good 
# 26  265

sum(confusion)
#Result:400

#the accuracy of the model can be found by dividing the sum of the true positives 
#by the sum of the entire matrix
#Answer: 72.75%

#create a list containing the results of the 'confusion' confusion matrix
predictionResults <- confusionMatrix(confusion)
#extract the data relevant to "Accuracy"
predictionResults$overall["Accuracy"]
#Answer: 0.7275

#######
#13.
confusionMat <- confusionMatrix(trainSet$Class, prediction)
confusionMat
#Results:
#        Reference
#Prediction Bad Good
#     Bad   26   94
#     Good  15  265
#Accuracy : 0.7275

