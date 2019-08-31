rm(list=ls())
#Connect to a dataset in R

getwd()
setwd("C:/Users/vdwivedi/Documents/INSOFE/7402c_Machine Learning/Lab")
# Or use this function : setwd("C:\\Users\\vdwivedi\\Documents\\INSOFE\\7402c_Machine Learning\\Lab")
getwd()

toyota_data <- read.csv("Toyota_SimpleReg.csv",header= TRUE, sep=",")
class(toyota_data)
#dim(toyota_data)
#colnames(toyota_data)


#Function to output number of rows, number of columns, column names by returning in a list

 summary_function <- function(df){
   return(list(nrow(df),ncol(df),colnames(df)))
 }
 
sum_toyota <- summary_function(toyota_data)
sum_toyota

#Look at structure details of data frame
str(toyota_data)  #to check data type, to see if their is any categorical variable that needs to be changed

#Look at summary details of data frame
summary(toyota_data)

# Mean for age has left skewness as min is 60 behind min, versus max is just 24 away from mean
# Price has a +ve skew and max is outlier

#Get the head for the dataframe
head(toyota_data)

#Check if ID is unique or not
length(unique(toyota_data$Id))

#Name of cars which has top 5 counts.
unique(toyota_data$Model)
count(toyota_data$Model)


sort(table(toyota_data$Model),decreasing = TRUE)[1:5]
#Table is same as value_count -> it counts the number for the attributes

#Rename column car  model to car
#names(toyota_data)[2] = 'Model'
names(toyota_data)[4] ='Age'


#Find NA values in every column
sapply(toyota_data,function(x) sum(is.na(x)))

# Or apply below code
apply(toyota_data,2,function(x) sum(is.na(x)))


#Doing plotting in R
#Scatter plot for price and age
plot(x= toyota_data$Age,y=toyota_data$Price, main = "Price-Age Comparison",
     xlab = "Age of Car (in Months)", ylab="Price of the car",col='blue' )

# Method 2 :plot(x= toyota_data$Age_06_15,y=toyota_data$Price, main = "Price-Age Comparison",
#     xlab = "Age of Car (in Months)", ylab="Price of the car",pch = 19, frame = FALSE)


#Finding the correlation coefficient
library(corrplot)
corrplot(corr= cor(toyota_data[c('Price','Age')]),method='number')
# if we had passed
corrplot(corr= cor(toyota_data$Price,toyota_data$Age),method='number')
#cor needs matric and while we are passing just x and y it is not helping in generating the details
#rather it is just outputting only 1 value

#to visualize correlation using any color method with circle
corrplot(corr= cor(toyota_data[c('Price','Age')]),method='circle')


#Drop id and model columns
#Method 1
cols_drop = c('Id', 'Model')
req_cols = colnames(toyota_data)
req_cols = req_cols[!req_cols %in% cols_drop]
toyota_data = toyota_data[,req_cols]
colnames(toyota_data)
head(toyota_data)

#Method 2
columns_drop =c('Id','Model')
toyota_data[columns_drop]< -NULL
names(toyota_data)


#Split the data in train and test
#set.seed(1234) #to be able to replicate same data again if we do on next day
#rows<-1:nrow(toyota_data) # to get the number of rows

# To get the indecies of rows that need to go into train
#trainrows<-sample(rows,0.7*nrow(toyota_data)) 

# Subset the required row indecies to get train data
#train<-toyota_data[trainrows,]

# Subset by excluding the train rows to get test data
#test<-toyota_data[-trainrows,]

#Check the train and test data frames
#train
#test


#Method 2 to split the data in train and test
library(caTools)
sample_index = sample.split(toyota_data$Price,SplitRatio = 0.7)
train <- toyota_data[sample_index==TRUE,]
test <- toyota_data[sample_index==FALSE,]

#use create data partition in carat library

#Create model ( lm (formula = y~x, data = trainining data))
LinReg <- lm(formula= Price ~ Age,data=train)
class(LinReg)
LinReg$residuals
LinReg$coefficients

#Summarize details for the model built
summary(LinReg)


#normalcy test -> saphiro

#Plot the graphs in 1 slide itself
par(mfrow=c(2,2))
plot(LinReg)

#Checking the 4 assumptions here.
#1 No pattern is observed in 1st graph - red line =0 and it increases a bit
# but towards end the data has increased, heteroscadistic
# plot 3, hetero scadistic
#2 Q-Q plot , should be on 45 degree line to tell its normally distributed but here we have some outliers
#4 tells outliers 


# How to evaluate the model
# first make prediction
train_predictions <- predict(object = LinReg, newdata=train)

#performance metrices to see if ur train_prediction is doing good
library(DMwR) #data manipulation with R
regr.eval(trues=train$Price,train_predictions)
#mae - how far in absolute terms is the actual point from predictions.
#mape- mean absolute percent error

#evaluate test predictions as well
regr.eval(trues = test$Price , preds = predict(object =LinReg, newdata=test))

#effect of metrics difference of train and test will depend on what problem u r working on