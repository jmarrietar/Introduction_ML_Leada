setwd("C:/Users/Alfonso/Desktop/JOM/Introduction_Machine_Learnin_TeamLeada/Classification_Trees")

##############################
#Supervised Machine Learning##
##############################
# search for patterns in labeled data to produce a model to make predictions 
#on future unlabeled data.

#Data-> Flag data taken from UC Irvine Machine Learning database.

data<-read.csv('flag_data.csv',header=T)

#Split Data 
traindata <- data[1:150, ]
testdata <- data[151:194, -6]

#Install and load packages. 
install.packages('rpart')
library('rpart')

#Idea of the Model-> Predict RELIGION of a country. 

#####################
#Clasification Trees#
#####################

#Advantages: 
#- Little data preparation and cleaning and specifically can be used with missing data
#- Easy to interpret and explain to others!

#Building a Model: 

model<-rpart(religion~circles+crosses+saltires+quarters+sunstars+crescent+triangle,data=traindata,method='class',control=rpart.control(minsplit=10))

#Method=class-> Because our response variable is a classification of religion categories. 

quiz_model<-rpart(religion~landmass+population+language,data=traindata,method='class')

#model in text form 
model 

#plot model 
plot(model)
text(model)

##################
#CROSS-VALIDATION#
##################

#This is done multiple times to prevent variability

#Create a subset on the training data 
subset<-traindata[1:10,-7]
train_subset<-traindata[11:150,]

subset_two <- traindata[11:20, ]
#Save the religions of subset into subset_religions. 

subset_religions<-traindata[1:10,7]
subset_religions2<-traindata[11:20, 7]

#Build a model-> Build model on the rest of the training_Data
cv_model<-rpart(religion~circles+crosses+saltires+quarters+sunstars+crescent+triangle,data=train_subset,method='class',control=rpart.control(minsplit=10))

#Make Predictions on subset data. 
cv_predictions<-predict(cv_model,newdata=subset,type='class')

cv_predictions_two<-predict(cv_model,newdata=subset_two,type='class')

cv_predictions<-as.vector(cv_predictions)

cv_predictions_two<-as.vector(cv_predictions_two)

#ACCURATE 
# we cross-validate our results by performing matching and sum the boolean vector.
sum(cv_predictions==subset_religions)

sum(cv_predictions_two==subset_religions2)

######################
#Complexity Parameter#
######################




