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



