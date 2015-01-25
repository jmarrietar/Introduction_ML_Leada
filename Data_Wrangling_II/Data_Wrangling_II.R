setwd("C:/Users/Alfonso/Desktop/JOM/Introduction_Machine_Learnin_TeamLeada/Data_Wrangling_II")

#Data: Flag DataBase from UC irvine Machine Learning database. 
data<-read.csv('flag_data.csv',header=T,stringsAsFactors=F)

#-------------------------------------------------------------------------------------------
#gsub() and sub() Functions
#Perform replacements on matches the function finds. 
#sub()-> perform replacement on the first match. 
#gsub()-> Perform replacements on all of the matches. 

#In this example i need to encode the 'landmass' column for easier analysis. 
gsub('Asia',5,data$landmass,fixed=TRUE)
#Note: Fixed argument = FALSE would match and replace strings such as 'Asias' and 'Asian'
data$landmass<-gsub('Asia',5,data$landmass,fixed=TRUE)
data$landmass<-gsub('N.America',1,data$landmass,fixed=TRUE)
data$landmass<-gsub('S.America',2,data$landmass,fixed=TRUE)
data$landmass<-gsub('Europe',3,data$landmass,fixed=TRUE)
data$landmass<-gsub('Africa',4,data$landmass,fixed=TRUE)
data$landmass<-gsub('Oceania',6,data$landmass,fixed=TRUE)
#landmass  1=N.America, 2=S.America, 3=Europe, 4=Africa, 5=Asia, 6=Oceania
#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#grep() Function.
#Identify INDICES where a Pattern is matched in a character vector. 
#Instead of Replacing it returns the Index.

grep('5',data$landmass,fixed=T)

data[grep('Puerto-Rico',data$name,fixed=T),]
grep('Hong-Kong',data$name,fixed=T)
#--------------------------------------------------------------------------------------------

#Note: grep() and gsub() are really useful when we want to find patterns that
#are more complex that strings. 

#--------------------------------------------------------------------------------------------
#Regular Expressions.
#When are useful? -> Supose we wnted all of the countries which had a hyphen(-) in their name. 



#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------
#Indicator Variabes. 