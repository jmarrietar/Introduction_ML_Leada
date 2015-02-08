setwd("/Users/josemiguelarrieta/Documents/Introduction_ML_Leada/Titanic Project")

######
#DATA#
######
#The schema for both of the datasets is provided below:
#PassengerId - A unique ID for each passenger in the dataset
#Survived - 0:Died, 1:Survived
#Pclass - A proxy for passenger class, 1 being the highest class and 3 the lowest.
#SibSp - The sum total of the number of siblings or spouses aboard with the passenger
#Parch - The sum total of the number of parents or children aboard with the passenger
#Fare - Fare price
#Embarked - Port departed from: Cherbourg, Queenstown, Southhampton

train_data<-read.csv('train.csv',header=T)
test_data<-read.csv('test.csv',header=T)

#Rounded to a whole number what is the average age of the all of the passengers in the Train dataset?
mean(train_data$Age,na.rm=T)

#How many missing age values are in the Train dataset?
sum(is.na(train_data$Age))

######################
#EXPLORATORY ANALYSIS#
######################

