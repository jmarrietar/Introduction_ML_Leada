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
#Special text string for describing a SEARCH PATTERN. 
#When are useful? -> Supose we wnted all of the countries which had a hyphen(-) in their name. 
#Example: In Spam Filters

#[[:alpha:]] -> All alphabetic. 
#[[:digit:]] -> Digits 0123456789
#[[:alnum:]] -> All alphabetic and digits
#[[:lower:]] -> Lower case alphabetic
#[[:upper:]] -> Upper case alphabetic

first_names<-c('Alex','randle?','Joseph','Michael?','david','Charlie')

#Matches any pattern that has a single alphabetic character
grep('[[:alpha:]]',first_names)
#Meta Character ^ defines what the first character in the pattern must be. 

grep('^[[:lower:]]',first_names)

explicit_Words<-c('v1agra','vi@gra','viagra')
grep('[[:alnum:]]+',explicit_Words)

#Returns correctly the index for 'viagra in explicit_Words '
which(explicit_words=='viagra')

grep('^[a-z]+[1-@][a-z]',explicit_words)

#The regular expressions [a-zA-Z0-9] and [[:alnum:]] are synonymus

#Following regex match the first names which a question mark after it.
first_names
grep('^[[:alpha:]]+\\?',first_names)
#How many country names have two hyphens?
grep('[[:alpha:]]+\\-+[[:alpha:]]+\\-',data$name)
#--------------------------------------------------------------------------------------------



#--------------------------------------------------------------------------------------------
#Creating Variables Syntax. 

#Suppose in the flag dataset you wanted to identify the flags which have the following characteristics:
  
#Now create an indicator variable for flags which have the following characteristics and name the column 'AM20CHR'
#landmass = 1 or 2
#population > 20
#religion = 1

#An indicator variable is one that is either 0 or 1. It is also more commonly known as a dummy variable.
#First we create the new column 'AF50CR' in the data and put all zeroes

data$AM20CHR<-0

identified <- which(data$landmass ==1 & data$population > 20 & data$religion == 1)
data$AM20CHR[identified]=1

unos<-which(data$AM20CHR==1)
sum(data[unos,]$population)
