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

train_data$Pclass<-as.factor(train_data$Pclass)
test_data$Pclass<-as.factor(test_data$Pclass)

######################
#EXPLORATORY ANALYSIS#
######################
summary(train_data$Sex)

#Proportion of each sex that survived in total. 
prop.table(table(train_data$Sex, train_data$Survived))

#The proportion of each sex that survived, as separate groups
prop.table(table(train_data$Sex, train_data$Survived),1)

aggregate(Survived ~ Child + Sex, data=train_data, FUN=sum)


# Basic Scatterplot Matrix
pairs(~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train_data, 
      main="Simple Scatterplot Matrix")
#Bar Plots
#Aggregation (Group by) 
#Survivers by Class
class_survivers<-aggregate(train_data$Survived, by=list(train_data$Pclass), FUN=sum)
class_survivers
barplot(class_survivers$x,names.arg=c("high", "middle", "low"),xlab="class",ylab="survivers")
#Survivers by Gender
gender_survivers<-aggregate(train_data$Survived, by=list(train_data$Sex), FUN=sum)
gender_survivers
barplot(gender_survivers$x,names.arg=c("female","male"),xlab="class",ylab="survivers")

#Scatterplots 
boxplot(Age~Survived,data=train_data,col="red")
boxplot(Fare~Survived,data=train_data,col="red")

plot(train_data$Fare)
plot(train_data$Sex)
plot(train_data$SibSp)

table(train_data$Survived,train_data$Sex)
table(train_data$Pclass,train_data$Survived)

#Gender is very important. and Pclass
#####################
#Logistic Regression#
#####################

LogRegression=glm(Survived~Pclass+Sex+Age+SibSp, data = train_data, family = "binomial")
summary(LogRegression)

predictLog = predict(LogRegression, newdata=test_data, type="response")


#Change NA to 0 or to 1s
predictLog[is.na(predictLog)] <- 0   #0.65
#predictLog[is.na(predictLog)] <- 1     #0.75
predictLog<-as.numeric(predictLog >= 0.5)
df = data.frame(test_data$PassengerId,predictLog) 
colnames(df) <- c("PassengerId","Survived")

write.csv(df, file = "MyData.csv",row.names = FALSE)

#######
#Trees#
#######

library(rpart)
library(rpart.plot)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
TitanicTree<-rpart(Survived~Pclass+Sex+Age+SibSp, data = train_data,method="class",minbucket=25)
TitanicTree2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train_data, method="class")
prp(TitanicTree)
fancyRpartPlot(TitanicTree2)
#Predictions 
PredictCART= predict(TitanicTree,newdata=test_data,type="class")
Prediction <- predict(TitanicTree2, test_data, type = "class")
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Prediction)
write.csv(submit, file = "MyDataCart.csv", row.names = FALSE)

#--------------------------------------------------------
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)


#--------------------------------------------------------
###############
#Random Forest#
###############

#cross-validation packages
library(caret)
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.3,0.01)) 

# Perform the cross validation
train(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data = train_data, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
TitanicTreeCV = rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data = train_data, method="class", cp = 0.01)
prp(TitanicTreeCV)
# Make predictions
PredictCV = predict(TitanicTreeCV, newdata = test_data, type = "class")
df3 = data.frame(test_data$PassengerId,PredictCV)
colnames(df3) <- c("PassengerId","Survived")
write.csv(df3, file = "MyDataForest.csv",row.names = FALSE)


#Prediction all Womans Survived 
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

summary(train_data$Age)
#we could assume that the 177 missing values are the average age of the rest of the passengers, ie. late twenties.

#Convert Fare To Categorical Value. 
train_data$Fare2 <- '30+'
train_data$Fare2[train_data$Fare < 30 & train_data$Fare >= 20] <- '20-30'
train_data$Fare2[train_data$Fare < 20 & train_data$Fare >= 10] <- '10-20'
train_data$Fare2[train_data$Fare < 10] <- '<10'

#Convert Fare To Categorical Value. 
test_data$Fare2 <- '30+'
test_data$Fare2[test_data$Fare < 30 & test_data$Fare >= 20] <- '20-30'
test_data$Fare2[test_data$Fare < 20 & test_data$Fare >= 10] <- '10-20'
test_data$Fare2[test_data$Fare < 10] <- '<10'

#Aggregate Function 
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train_data, FUN=function(x) {sum(x)/length(x)})

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)



#####################
#Feature Engineering#
#####################
#Explore the Titles of Names. 

#An easy way to perform the same processes on both datasets at the same time is to merge them.
test_data$Survived <- NA
combi <- rbind(train_data, test_data)

#Factor to String. 
combi$Name <- as.character(combi$Name)

#split 
strsplit(combi$Name[1], split='[,.]')[[1]][2]

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

#Quitar el espacio en blanco antes del titulo. 
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

#Combinar Mandame and Mademoiselle
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

#De esta forma puedo cambiar el texto a otro sin necesidad de usar lo de kenneth. 
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#Change back to factor Variables. 
combi$Title <- factor(combi$Title)

#Seems reasonable to assume that a large family might have trouble tracking down little Johnny as they all scramble to get off the sinking ship, 
#so let’s combine the two variables into a new one, FamilySize:




