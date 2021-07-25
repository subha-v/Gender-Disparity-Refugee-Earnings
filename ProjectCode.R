###########################
# Polygence Project #
# Date: 7/24/21         #
# Subha        #
###########################

install.packages('ggplot2')
install.packages('readxl')
install.packages("lattice")
install.packages('dplyr')
install.packages('ggiraphExtra')
install.packages('stargazer')
install.packages('xtable')
install.packages("kableExtra")
install.packages('tidyverse')
install.packages('lmtest')
install.packages('mosaic')
library(mosaic)
library(lmtest)
library("kableExtra")
library('xtable')
library('stargazer')
library('dplyr')
library('readxl')
library('ggplot2')
library('lattice')
library(ggiraphExtra)
library(tidyverse)

install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")

##########################
#    Loading Data          #
##########################

dev.off() #this removes current plots
(rm(list = ls())) #this removes everything in environment(the spreadsheets)
getwd() # this makes sure you're in the correct working directory
asr_2018 <- read_excel("asr_2018.xls") #loading in the data
gii <- read_excel("gii.xls")

##########################
#   Cleaning Data           #
##########################

asr_2018$qn8b<-as.numeric(gsub("don't know","",asr_2018$qn8b))
asr_2018$qn8b<-as.numeric(gsub("refused","",asr_2018$qn8b))

asr_2018$qn8a<-as.numeric(gsub("don't know","",asr_2018$qn8a))
asr_2018$qn8a<-as.numeric(gsub("refused","",asr_2018$qn8a))

asr_2018$qn6b<- as.numeric(gsub("don't know","",asr_2018$qn6b))

asr_2018$qn38b <- as.numeric(gsub("don't know","", asr_2018$qn38b))

#removing any non integer values in hours worked per week
asr_2018$qn18b <- as.numeric(gsub("don't know", "", asr_2018$qn18b))
asr_2018$qn18b <- as.numeric(gsub("refused", "", asr_2018$qn18b))

#removing any non integer values in the amount earned in past year variable (qn18c)
asr_2018$qn18c <- as.numeric(gsub("don't know", "", asr_2018$qn18c))
asr_2018$qn18c <- as.numeric(gsub("refused", "", asr_2018$qn18c))

#removing any non integer values in dollars per hour at job
asr_2018$dollarsPerHourAtJob <- as.numeric(gsub("don't know", "", asr_2018$dollarsPerHourAtJob))
asr_2018$dollarsPerHourAtJob <- as.numeric(gsub("refused", "", asr_2018$dollarsPerHourAtJob))

#removing any non integer values in how many years of schooling in asr_2018
asr_2018$howManyYearsOfSchoolingBeforeUS <- as.numeric(gsub("don't know", "", asr_2018$howManyYearsOfSchoolingBeforeUS))
asr_2018$howManyYearsOfSchoolingBeforeUS <- as.numeric(gsub("refused", "", asr_2018$howManyYearsOfSchoolingBeforeUS))
asr_2018$howManyYearsOfSchoolingBeforeUS <- as.numeric(gsub("20 or more", "20",asr_2018$howManyYearsOfSchoolingBeforeUS))

#removing any non integer values in how many years of schooling in asr_2018_female

asr_2018_female$howManyYearsOfSchoolingBeforeUS <- as.numeric(gsub("don't know", "", asr_2018_female$howManyYearsOfSchoolingBeforeUS))
asr_2018_female$howManyYearsOfSchoolingBeforeUS <- as.numeric(gsub("refused", "", asr_2018_female$howManyYearsOfSchoolingBeforeUS))
asr_2018_female$howManyYearsOfSchoolingBeforeUS <- as.numeric(gsub("20 or more", "", asr_2018_female$howManyYearsOfSchoolingBeforeUS))

#removing any non integer values in how many of years of schooling in asr_2018_male
asr_2018_male$howManyYearsOfSchoolingBeforeUS <- as.numeric(gsub("don't know", "", asr_2018_male$howManyYearsOfSchoolingBeforeUS))
asr_2018_male$howManyYearsOfSchoolingBeforeUS <- as.numeric(gsub("refused", "", asr_2018_male$howManyYearsOfSchoolingBeforeUS))
asr_2018_male$howManyYearsOfSchoolingBeforeUS <- as.numeric(gsub("20 or more", "", asr_2018_male$howManyYearsOfSchoolingBeforeUS))

#removing any non integer values in the age variable
asr_2018$age <- as.numeric(gsub("refused", "", asr_2018$age))
asr_2018$age <- as.numeric(gsub("don't know", "", asr_2018$age))
asr_2018$age <- as.numeric(gsub("less than 1 year", "", asr_2018$age))
asr_2018$age <- as.numeric(gsub("75 or older", "", asr_2018$age))

#removing any 0 values from the GI index since they are not given! MEWMEWMEWMEWMEW
asr_2018$GI <- as.numeric(gsub("0", "", asr_2018$GI))

#removing any bad values in the resettled region
asr_2018$originalUSRegionWhenResettled <- as.numeric(gsub("refused", "", asr_2018$originalUSRegionWhenResettled))

#########################
#     Creating Dummy Variables        #
#########################

asr_2018$number[asr_2018$qn8b=="bi-weekly"]<-104
asr_2018$number[asr_2018$qn8b=="weekly"]<-52
asr_2018$number[asr_2018$qn8b=="monthly"]<-12
asr_2018$number[asr_2018$qn8b=="annually"]<-1


# dummy variable for gender
# 0 is female and 1 is male
asr_2018$gender_dummy <- ifelse(asr_2018$gender == 'male',1,0)   

asr_2018$somali <- ifelse(asr_2018$countryOfBirth == 'somalia',1,0)

#creating GI index variable
asr_2018$GI <- ifelse(asr_2018$countryOfBirth %in% c("united states", "thailand", "el salvador"), 1, 0)
asr_2018$GI <- (asr_2018$countryOfBirth %in% c("burma", "nepal", "iran", "bhutan") 2)
#
#' asr_2018$GII[asr_2018$countryOfBirth=="united states"]<-0.204*10
#' asr_2018$GII[asr_2018$countryOfBirth=="cuba"]<-0.304*10
#' asr_2018$GII[asr_2018$countryOfBirth=="thailand"]<-0.359*10
#' asr_2018$GII[asr_2018$countryOfBirth=="el salvador"]<-0.383*10
#' asr_2018$GII[asr_2018$countryOfBirth=="bhutan"]<-0.421*10
#' asr_2018$GII[asr_2018$countryOfBirth=="nepal"]<-0.452*10
#' asr_2018$GII[asr_2018$countryOfBirth=="iran"]<-0.459*10
#' asr_2018$GII[asr_2018$countryOfBirth=="burma"]<-0.478*10
#' asr_2018$GII[asr_2018$countryOfBirth=="syria"]<-0.482*10
#' asr_2018$GII[asr_2018$countryOfBirth=="democratic republic of the congo"]<-0.57*10
#' asr_2018$GII[asr_2018$countryOfBirth=="iraq"]<-0.577*10
#' asr_2018$GII[asr_2018$countryOfBirth=="afghanistan"]<-0.655*10
#' asr_2018$GII[asr_2018$countryOfBirth=="somalia"]<-0.776*10
#' asr_2018$GII[asr_2018$countryOfBirth=="eritrea"]<-0.517*10
#' 

#ECONP
asr_2018$econp[asr_2018$countryOfBirth=="cuba"]<-0.627*10
asr_2018$econp[asr_2018$countryOfBirth=="thailand"]<-0.763*10
asr_2018$econp[asr_2018$countryOfBirth=="el salvador"]<-0.582*10
asr_2018$econp[asr_2018$countryOfBirth=="bhutan"]<-0.619*10
asr_2018$econp[asr_2018$countryOfBirth=="nepal"]<-0.465*10
asr_2018$econp[asr_2018$countryOfBirth=="iran"]<-0.376*10
asr_2018$econp[asr_2018$countryOfBirth=="burma"]<-0.738*10
asr_2018$econp[asr_2018$countryOfBirth=="syria"]<-0.273*10
asr_2018$econp[asr_2018$countryOfBirth=="democratic republic of the congo"]<-0.613*10
asr_2018$econp[asr_2018$countryOfBirth=="iraq"]<-0.264*10
#asr_2018$econp[asr_2018$countryOfBirth=="somalia"]<-0.408*10 #mauritania
asr_2018$econp[asr_2018$countryOfBirth=="eritrea"]<-0.606*10 #ethiopia


#HDI
asr_2018$hdi[asr_2018$countryOfBirth=="cuba"]<-0.777*10
asr_2018$hdi[asr_2018$countryOfBirth=="thailand"]<-0.755*10
asr_2018$hdi[asr_2018$countryOfBirth=="el salvador"]<-0.674*10
asr_2018$hdi[asr_2018$countryOfBirth=="bhutan"]<-0.612*10
asr_2018$hdi[asr_2018$countryOfBirth=="nepal"]<-0.574*10
asr_2018$hdi[asr_2018$countryOfBirth=="iran"]<-0.798*10
asr_2018$hdi[asr_2018$countryOfBirth=="burma"]<-0.578*10
asr_2018$hdi[asr_2018$countryOfBirth=="syria"]<-0.536*10
asr_2018$hdi[asr_2018$countryOfBirth=="democratic republic of the congo"]<-0.457*10
asr_2018$hdi[asr_2018$countryOfBirth=="iraq"]<-0.685*10
#asr_2018$hdi[asr_2018$countryOfBirth=="somalia"]<-0.520*10 #mauritania
asr_2018$hdi[asr_2018$countryOfBirth=="eritrea"]<-0.440*10 #this is acc eritrea


#GGP
asr_2018$ggp[asr_2018$countryOfBirth=="cuba"]<-0.749*10
asr_2018$ggp[asr_2018$countryOfBirth=="thailand"]<-0.702*10
asr_2018$ggp[asr_2018$countryOfBirth=="el salvador"]<-0.690*10
asr_2018$ggp[asr_2018$countryOfBirth=="bhutan"]<-0.638*10
asr_2018$ggp[asr_2018$countryOfBirth=="nepal"]<-0.671*10
asr_2018$ggp[asr_2018$countryOfBirth=="iran"]<-0.589*10
asr_2018$ggp[asr_2018$countryOfBirth=="burma"]<-0.690*10
asr_2018$ggp[asr_2018$countryOfBirth=="syria"]<-0.568*10
asr_2018$ggp[asr_2018$countryOfBirth=="democratic republic of the congo"]<-0.582*10
asr_2018$ggp[asr_2018$countryOfBirth=="iraq"]<-0.551*10
asr_2018$ggp[asr_2018$countryOfBirth=="eritrea"]<-0.656*10 #this is ethiopia

#somalia not considered in ^


#GDP per capita
asr_2018$gdpc[asr_2018$countryOfBirth=="thailand"]<-7295
asr_2018$gdpc[asr_2018$countryOfBirth=="el salvador"]<-4068
asr_2018$gdpc[asr_2018$countryOfBirth=="bhutan"]<-3243
asr_2018$gdpc[asr_2018$countryOfBirth=="nepal"]<-1039
asr_2018$gdpc[asr_2018$countryOfBirth=="iran"]<-5417
asr_2018$gdpc[asr_2018$countryOfBirth=="burma"]<-1418
asr_2018$gdpc[asr_2018$countryOfBirth=="syria"]<-348.04
asr_2018$gdpc[asr_2018$countryOfBirth=="democratic republic of the congo"]<-2224
asr_2018$gdpc[asr_2018$countryOfBirth=="iraq"]<-5834
asr_2018$gdpc[asr_2018$countryOfBirth=="eritrea"]<-332
asr_2018$gdpc[asr_2018$countryOfBirth == "somalia"]<-332


#GI by category
asr_2018$GI[asr_2018$countryOfBirth =='burma']<-2
asr_2018$GI[asr_2018$countryOfBirth =='nepal']<-2
asr_2018$GI[asr_2018$countryOfBirth =='iran']<-2
asr_2018$GI[asr_2018$countryOfBirth =='bhutan']<-2

asr_2018$GI[asr_2018$countryOfBirth =='democratic republic of the congo']<-3
asr_2018$GI[asr_2018$countryOfBirth =='iraq']<-3
asr_2018$GI[asr_2018$countryOfBirth =='eritrea']<-3
asr_2018$GI[asr_2018$countryOfBirth =='afghanistan']<-3
asr_2018$GI[asr_2018$countryOfBirth =='somalia']<-3
asr_2018$GI[asr_2018$countryOfBirth =='syria']<-3

asr_2018$englishSkill_dummy <- ifelse(asr_2018$howWellDoTheySpeakNow == 'not at all',0,1)
asr_2018$englishSkill_dummy[asr_2018$howWellDoTheySpeakNow =='well']<-2
asr_2018$englishSkill_dummy[asr_2018$howWellDoTheySpeakNow =='very well']<-3

asr_2018$GII[asr_2018$countryOfBirth=="cuba"]<-0.312*10
asr_2018$GII[asr_2018$countryOfBirth=="thailand"]<-0.377*10
asr_2018$GII[asr_2018$countryOfBirth=="el salvador"]<-0.397*10
asr_2018$GII[asr_2018$countryOfBirth=="bhutan"]<-0.436*10
asr_2018$GII[asr_2018$countryOfBirth=="nepal"]<-0.476*10
asr_2018$GII[asr_2018$countryOfBirth=="iran"]<-0.492*10
asr_2018$GII[asr_2018$countryOfBirth=="burma"]<-0.458*10
asr_2018$GII[asr_2018$countryOfBirth=="syria"]<-0.547*10
asr_2018$GII[asr_2018$countryOfBirth=="democratic republic of the congo"]<-0.579*10
asr_2018$GII[asr_2018$countryOfBirth=="iraq"]<-0.540*10
asr_2018$GII[asr_2018$countryOfBirth=="somalia"]<-0.560*10 #sudan
asr_2018$GII[asr_2018$countryOfBirth=="eritrea"]<-0.508*10 #ethiopia


#########################
#    Creating Spreadsheets            #
#########################

asr_2018_university <- asr_2018[asr_2018$highestDegreeBeforeUS=="university degree (other than medical)",]

#this creates two spreadsheets, one for all females, and one for all males
asr_2018_male <- asr_2018[asr_2018$gender=="male",]
asr_2018_female <- asr_2018[asr_2018$gender=="female",]
#english level
asr_2018_female_verywell <- asr_2018_female[asr_2018_female$howWellDoTheySpeakNow=="very well",]
asr_2018_female_well <- asr_2018_female[asr_2018_female$howWellDoTheySpeakNow=="well",]
asr_2018_female_notwell <- asr_2018_female[asr_2018_female$howWellDoTheySpeakNow=="not well",]
asr_2018_female_notatall <- asr_2018_female[asr_2018_female$howWellDoTheySpeakNow=="not at all",]

asr_2018_male_verywell <- asr_2018_male[asr_2018_male$howWellDoTheySpeakNow=="very well",]
asr_2018_male_well <- asr_2018_male[asr_2018_male$howWellDoTheySpeakNow=="well",]
asr_2018_male_notwell <- asr_2018_male[asr_2018_male$howWellDoTheySpeakNow=="not well",]
asr_2018_male_notatall <- asr_2018_male[asr_2018_male$howWellDoTheySpeakNow=="not at all",]

#creating syrian spreadsheet
asr_2018_syria <- asr_2018[asr_2018$countryOfBirth=="syria",]
#creating syrian spreadsheet for females
asr_2018_syria_female <- asr_2018_syria[asr_2018_syria$gender=="female",]
#creating syrian spreadsheet for males
asr_2018_syria_male <- asr_2018_syria[asr_2018_syria$gender=="male",]

#creating iraq spreadsheet
asr_2018_iraq <- asr_2018[asr_2018$countryOfBirth=="iraq",]
#creating iraq spreadsheet for females
asr_2018_iraq_female <- asr_2018_iraq[asr_2018_iraq$gender=="female",]
#creating iraq spreadsheet for males
asr_2018_iraq_male <- asr_2018_iraq[asr_2018_iraq$gender=="male",]

#creating eritrea spreadsheet
asr_2018_eritrea <- asr_2018[asr_2018$countryOfBirth=="eritrea",]
#creating eritrea spreadsheet for females
asr_2018_eritrea_female <- asr_2018_eritrea[asr_2018_eritrea$gender=="female",]
#creating eritrea spreadsheet for males
asr_2018_eritrea_male <- asr_2018_eritrea[asr_2018_eritrea$gender=="male",]

#creating thai spreadsheets
asr_2018_thailand <- asr_2018[asr_2018$countryOfBirth=="thailand",]

#creating US spreadsheets
asr_2018_US <- asr_2018[asr_2018$countryOfBirth == "united states",]

#creating cuba
asr_2018_cuba <- asr_2018[asr_2018$countryOfBirth == "cuba",]

#el salvador
asr_2018_elsalvador <- asr_2018[asr_2018$countryOfBirth == "el salvador",]

#creating gender inequality spreadsheet
GI_1 <- rbind(asr_2018_US, asr_2018_cuba, asr_2018_thailand, asr_2018_elsalvador)

#creating GI 2 spreadsheet
asr_2018_burma <- asr_2018[asr_2018$countryOfBirth == "burma",]
asr_2018_nepal <- asr_2018[asr_2018$countryOfBirth == "nepal",]
asr_2018_iran <- asr_2018[asr_2018$countryOfBirth == "iran",]
asr_2018_bhutan <- asr_2018[asr_2018$countryOfBirth == "bhutan",]
GI_2 <- rbind(asr_2018_burma, asr_2018_nepal, asr_2018_iran, asr_2018_bhutan)

#creating GI 3 spreadsheet
asr_2018_congo <- asr_2018[asr_2018$countryOfBirth == "democratic republic of the congo",]
asr_2018_iraq <- asr_2018[asr_2018$countryOfBirth == "iraq",]
asr_2018_afghanistan <- asr_2018[asr_2018$countryOfBirth == "afghanistan",]
asr_2018_somalia <- asr_2018[asr_2018$countryOfBirth == "somalia",]
GI_3 <- rbind(asr_2018_congo, asr_2018_iraq, asr_2018_afghanistan, asr_2018_syria, asr_2018_somalia, asr_2018_eritrea)

asr_2018_somalia_women <- asr_2018_somalia[asr_2018_somalia$gender == "female",]
asr_2018_somalia_men <- asr_2018_somalia[asr_2018_somalia$gender == "male",]

NoSomalia <- rbind(asr_2018_bhutan, asr_2018_burma, asr_2018_syria, asr_2018_eritrea, asr_2018_bhutan, asr_2018_iran, asr_2018_iraq, asr_2018_nepal, asr_2018_elsalvador, asr_2018_thailand, asr_2018_congo, asr_2018_US)


#creating spreadsheets by current english skill
asr_2018_verywell <- asr_2018[asr_2018$howWellDoTheySpeakNow=="very well",]
asr_2018_well <- asr_2018[asr_2018$howWellDoTheySpeakNow=="well",]
asr_2018_notwell <- asr_2018[asr_2018$howWellDoTheySpeakNow=="not well",]
asr_2018_notatall <- asr_2018[asr_2018$howWellDoTheySpeakNow=="not at all",]

asr_2018_afghan <-asr_2018[asr_2018$countryOfBirth=="afghanistan",]


#########################
#         Plots             #
#########################

plot(asr_2018$GII[asr_2018$gender=="female"], asr_2018$dollarsPerHourAtJob[asr_2018$gender=="female"], col="blue",)
points(asr_2018$GII[asr_2018$gender=="male"], asr_2018$dollarsPerHourAtJob[asr_2018$gender=="male"], col="red",)

reg0 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$GII+asr_2018$gender+asr_2018$GII*asr_2018$gender)
summary(reg0)
coef(reg0)

ggplot(asr_2018_female,aes(y=dollarsPerHourAtJob,x=howManyYearsOfSchoolingBeforeUS))+geom_point()+geom_smooth(method="lm")+ labs(title="Female Years of Schooling \n vs Dollars Per Hour at Job", x="Years of Schooling Before Entering US", y="Dollars Per Hour at Job")+ coord_cartesian(ylim=c(0, 60))
ggplot(asr_2018_male,aes(y=dollarsPerHourAtJob,x=howManyYearsOfSchoolingBeforeUS))+geom_point()+geom_smooth(method="lm")+ labs(title="Male Years of Schooling \n vs Dollars Per Hour at Job", x="Years of Schooling Before Entering US", y="Dollars Per Hour at Job")+ coord_cartesian(ylim=c(0, 60))

abline(a=15.46, b=-0.09, col="red",lwd=3)
abline(a=15.56, b=-0.710, col="blue", lwd=3)

y1 <- 9.147 + 7.19*x
y2 <-
  plot.new+
  plot(a=9.147, b=7.19, col="red",lwd=3, xlim=c(0,1))
plot(a=10.5, b=2.16, col="blue", lwd=3, xlim=c(0,1))


 puts the years of schooling vs dollars per hour at job for male and female and graphs them on separate graphs
  par(mfrow=c(1,2))
boys<-plot(dollarsPerHourAtJob ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_male, pch=19, ylab="Dollars Per Hour At Job", 
           xlab="Years Of Schooling Before Entering The US", main="Male", las=1, ylim=c(0,100))
abline(boys)
plot(dollarsPerHourAtJob ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_female, pch=19, ylab="Dollars Per Hour At Job", 
     xlab="Years Of Schooling Before Entering The US", main="Female", las=1, ylim=c(0,100))


#general graph for years of schooling vs dollars per hour
xyplot(dollarsPerHourAtJob ~ howManyYearsOfSchoolingBeforeUS, data = asr_2018,
       type = c("p", "g", "smooth"),
       xlab = "Years Of Schooling Before Entering US", ylab = "Dollars Per Hour At Job")


#graphs the relation between years of schooling and average number of hours worked
par(mfrow=c(1,2))
plot(qn18b ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_male, pch=19, ylab="Average Number Of Hours Worked Per Week", 
     xlab="Years Of Schooling Before Entering The US", main="Male", las=1,ylim=c(0,90))
model4<-lm(qn18b ~ howManyYearsOfSchoolingBeforeUS)
summary(model4)
plot(qn18b ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_female, pch=19, ylab="Average Number Of Hours Worked Per Week", 
     xlab="Years Of Schooling Before Entering The US", main="Female", las=1,ylim=c(0,90))
summary(model5)

#graphs the relation between years of schooling and amount earned per past year
par(mfrow=c(1,2))
plot(qn18c ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_male, pch=19, ylab="Income In Past Year", 
     xlab="Years Of Schooling Before Entering The US", main="Male", las=1)

plot(qn18c ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_female, pch=19, ylab="Income In Past Year", 
     xlab="Years Of Schooling Before Entering The US", main="Female", las=1)


# this puts the years of schooling vs dollars per hour at job for eritrean male and female and graphs them on separate graphs
par(mfrow=c(1,2))
plot(dollarsPerHourAtJob ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_eritrea_male, pch=19, ylab="Dollars Per Hour At Job", 
     xlab="Years Of Schooling Before Entering The US", main="Eritrean Male", las=1)

plot(dollarsPerHourAtJob ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_eritrea_female, pch=19, ylab="Dollars Per Hour At Job", 
     xlab="Years Of Schooling Before Entering The US", main="Eritrean Female", las=1, ylim=c(0,20))


# this puts the years of schooling vs dollars per hour at job for iraqi male and female and graphs them on separate graphs. also accounts for having the same y-axis
par(mfrow=c(1,2))
plot(dollarsPerHourAtJob ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_iraq_male, pch=19, ylab="Dollars Per Hour At Job", 
     xlab="Years Of Schooling Before Entering The US", main="Iraqi Male", las=1,ylim=c(0,65))
plot(dollarsPerHourAtJob ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_iraq_female, pch=19, ylab="Dollars Per Hour At Job", 
     xlab="Years Of Schooling Before Entering The US", main="Iraqi Female", las=1, ylim=c(0,65))


# this puts the years of schooling vs dollars per hour at job for iraqi male and female and graphs them on separate graphs. also accounts for having the same y-axis
par(mfrow=c(1,2))
plot(dollarsPerHourAtJob ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_syria_male, pch=19, ylab="Dollars Per Hour At Job", 
     xlab="Years Of Schooling Before Entering The US", main="Syrian Male", las=1)
plot(dollarsPerHourAtJob ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_syria_female, pch=19, ylab="Dollars Per Hour At Job", 
     xlab="Years Of Schooling Before Entering The US", main="Syrian Female", las=1, ylim=c(0,35))


# this puts the years of schooling vs dollars per hour at job for iraqi male and female and graphs them on separate graphs. also accounts for having the same y-axis
par(mfrow=c(1,2))
plot(dollarsPerHourAtJob ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_thailand_male, pch=19, ylab="Dollars Per Hour At Job", 
     xlab="Years Of Schooling Before Entering The US", main="Syrian Male", las=1)
plot(dollarsPerHourAtJob ~ howManyYearsOfSchoolingBeforeUS, data=asr_2018_thailand_female, pch=19, ylab="Dollars Per Hour At Job", 
     xlab="Years Of Schooling Before Entering The US", main="Syrian Female", las=1)

#prints dollars per hour vs years of schooling before US with the regression line
ggplot(asr_2018_female,aes(y=dollarsPerHourAtJob,x=howManyYearsOfSchoolingBeforeUS,color=factor(howWellDoTheySpeakNow)))+geom_point()+stat_smooth(method="lm",se=FALSE)+ coord_cartesian(ylim=c(0, 60))
ggplot(asr_2018_male,aes(y=dollarsPerHourAtJob,x=howManyYearsOfSchoolingBeforeUS,color=factor(howWellDoTheySpeakNow)))+geom_point()+stat_smooth(method="lm",se=FALSE)+ coord_cartesian(ylim=c(0, 60))

ggplot(asr_2018, aes(y=dollarsPerHourAtJob, x=howManyYearsOfSchoolingBeforeUS, color=factor(gender)))+geom_point()+stat_smooth(method="lm", se=FALSE)+ coord_cartesian(ylim=c(0, 60))
#prints graphs for each GI index
ggplot(GI_1,aes(y=dollarsPerHourAtJob,x=howManyYearsOfSchoolingBeforeUS,color=factor(gender)))+geom_point()+stat_smooth(method="lm",se=FALSE)
ggplot(GI_2,aes(y=dollarsPerHourAtJob,x=howManyYearsOfSchoolingBeforeUS,color=factor(gender)))+geom_point()+stat_smooth(method="lm",se=FALSE)
ggplot(GI_3,aes(y=dollarsPerHourAtJob,x=howManyYearsOfSchoolingBeforeUS,color=factor(gender)))+geom_point()+stat_smooth(method="lm",se=FALSE)



#########################
#       Regressions and Summaries        #
#########################
reg<-lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender + asr_2018$howManyYearsOfSchoolingBeforeUS + asr_2018$age + asr_2018$highestDegreeBeforeUS + asr_2018$howWellDoTheySpeakNow+ asr_2018$countryOfCitizenship+asr_2018$workBeforeUS)
summary(reg)
summary(reg)$coefficient
confint(reg, conf.level=0.95)
plot(reg)

#hdi
regh <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender + asr_2018$hdi)
summary(regh)

reghnew <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$GII + asr_2018$gender + asr_2018$howManyYearsOfSchoolingBeforeUS)
summary(reghnew)


#####
#Regressions using the different mechanisms (HDI, GII, etc.)
######

#HDI

regh1 <-lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender*asr_2018$hdi + asr_2018$gender + asr_2018$hdi)
summary(regh1)

regh2 <-lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender*asr_2018$hdi + asr_2018$gender + asr_2018$hdi + asr_2018$howManyYearsOfSchoolingBeforeUS)
summary(regh2)

stargazer(regh1, out='latex')
stargazer(regh2, out = 'latex')

regh3 <- lm(asr_2018$dollarsPerHourAtJob ~asr_2018$gender + asr_2018$hdi + asr_2018$howManyYearsOfSchoolingBeforeUS)
summary(regh3)

stargazer(regh3, out='latex')

#idk add an appendix for numppl or something
#GGP

yuh0 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender*asr_2018$ggp + asr_2018$gender + asr_2018$ggp)
summary(yuh0)

yuh1 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender*asr_2018$ggp + asr_2018$gender + asr_2018$ggp + asr_2018$howManyYearsOfSchoolingBeforeUS)
summary(yuh1)

stargazer(yuh0, yuh1, out='latex')

regg <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender + asr_2018$howManyYearsOfSchoolingBeforeUS + asr_2018$ggp)
summary(regg)


#Using all of the mechanisms together.

billiejean <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender + asr_2018$hdi + asr_2018$ggp + asr_2018$howManyYearsOfSchoolingBeforeUS)
summary(billiejean)

billiejean2 <- lm (asr_2018$hdi ~ asr_2018$ggp)
summary(billiejean2)

billiewhat <- lm(asr_2018$hdi ~ asr_2018$GII)
summary(billiewhat)

billiejean3 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$hdi*asr_2018$gender + asr_2018$gender + asr_2018$hdi + asr_2018$howManyYearsOfSchoolingBeforeUS + asr_2018$ggp)
summary(billiejean3)

wer <- lm(asr_2018$ggp ~ asr_2018$hdi)
summary(wer)

#GDP per capita!

hai <- lm(asr_2018$gdpc ~asr_2018$ggp)
summary(hai)

######

yuh2 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender*asr_2018$hdi + asr_2018$gender + asr_2018$hdi + asr_2018$howManyYearsOfSchoolingBeforeUS)
summary(yuh2)


stargazer(regh, regh2, type = 'latex', out = 'hi')

rego <- lm(asr_2018$annualIncome~asr_2018$gender)
summary(rego)


#trying to use econp
reg0 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$econp + asr_2018$gender)
summary(reg0)

#numppl
reg0 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$numppl*asr_2018$gender + asr_2018$gender + asr_2018$numppl)
summary(reg0)

#trying to use gii
reg0 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$GII+asr_2018$gender+asr_2018$econp)
summary(reg0)
coef(reg0)
stargazer(reg0, type = 'latex', out='hi')

somalireg <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender*asr_2018$somali + asr_2018$somali + asr_2018$gender)
summary(somalireg)

somalireg2 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender + asr_2018$somali + asr_2018$howManyYearsOfSchoolingBeforeUS)
summary(somalireg2)

somalireg3 <- lm(asr_2018_somalia$dollarsPerHourAtJob ~ asr_2018_somalia$gender+ asr_2018_somalia$howManyYearsOfSchoolingBeforeUS)
summary(somalireg3)

somalireg4 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender*asr_2018$somali + asr_2018$somali + asr_2018$gender + asr_2018$howManyYearsOfSchoolingBeforeUS)
summary(somalireg4)

reg1 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender*asr_2018$GII + asr_2018$gender + asr_2018$GII + asr_2018$howManyYearsOfSchoolingBeforeUS)
summary(reg1)

#gender
reg1 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender)
summary(reg1)

#gender + years
reg2 <- lm(asr_2018$dollarsPerHourAtJob~asr_2018$howManyYearsOfSchoolingBeforeUS +asr_2018$gender)
summary(reg2)

#gender + english skill + years
reg3 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender + asr_2018$howWellDoTheySpeakNow + asr_2018$howManyYearsOfSchoolingBeforeUS)
summary(reg3)

#gender + work + years + english level + highest degree
reg4 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender +  asr_2018$howManyYearsOfSchoolingBeforeUS + asr_2018$howWellDoTheySpeakNow+ asr_2018$highestDegreeBeforeUS)
summary(reg4)

reg5 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender +  asr_2018$howManyYearsOfSchoolingBeforeUS + asr_2018$howWellDoTheySpeakNow+ asr_2018$highestDegreeBeforeUS+asr_2018$age)
summary(reg5)

reg6 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$gender +  asr_2018$howManyYearsOfSchoolingBeforeUS + asr_2018$howWellDoTheySpeakNow+ asr_2018$highestDegreeBeforeUS+asr_2018$age+asr_2018$numppl)
summary(reg6)

mew <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$howWellDoTheySpeakNow)
summary(mew)

pbhat <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$highestDegreeBeforeUS)
summary(pbhat)
stargazer(pbhat, type = 'latex', out='hi')

stargazer(reg1, reg2, reg3, reg4, reg5, type = 'latex', out = 'my_reg_table.html')

bptest(reg5)

#making same regression w interaction variable
reg7 <- lm(asr_2018$dollarsPerHourAtJob~asr_2018$gender + asr_2018$howManyYearsOfSchoolingBeforeUS + asr_2018$howManyYearsOfSchoolingBeforeUS +asr_2018$gender)
summary(reg7)

reg16 <- lm(asr_2018_female$dollarsPerHourAtJob ~ asr_2018_female$howManyYearsOfSchoolingBeforeUS + asr_2018_female$countryOfBirth)
summary(reg16)

#gendered schooling
reg11 <- lm(asr_2018$dollarsPerHourAtJob ~ asr_2018$GI + asr_2018$gender + asr_2018$howManyYearsOfSchoolingBeforeUS)
summary(reg11)
stargazer(reg11, type = 'latex', out = 'my_reg_table.html')

#linear correlation between dollars per hour at job vs years of school for female
cor(asr_2018_female$dollarsPerHourAtJob,asr_2018_female$howManyYearsOfSchoolingBeforeUS, method="pearson", use = "complete.obs")
#linear correlation between dollars per hour at job vs years of school for males
cor(asr_2018_male$dollarsPerHourAtJob, asr_2018_male$howManyYearsOfSchoolingBeforeUS, method="pearson", use = "complete.obs")

#########################
#       Descriptive Statistics        #
#########################

#finding the average years of school for females
mean(asr_2018_female$howManyYearsOfSchoolingBeforeUS, na.rm = TRUE)
#finding the average years of school for males
mean(asr_2018_male$howManyYearsOfSchoolingBeforeUS, na.rm = TRUE)
sd(asr_2018_female$howManyYearsOfSchoolingBeforeUS, na.rm=TRUE)
sd(asr_2018_male$howManyYearsOfSchoolingBeforeUS, na.rm =TRUE)

#finding the average for the dollars per hour vs the english skill
mean(asr_2018_verywell$dollarsPerHourAtJob, na.rm = TRUE)
mean(asr_2018_well$dollarsPerHourAtJob, na.rm = TRUE)
mean(asr_2018_notwell$dollarsPerHourAtJob, na.rm = TRUE)
mean(asr_2018_notatall$dollarsPerHourAtJob, na.rm = TRUE)

#finding the average for number of hours worked in a week for females
mean(asr_2018_female$qn18b, na.rm = TRUE)
mean(asr_2018_male$qn18b, na.rm = TRUE)
sd(asr_2018_male$qn18b, na.rm = TRUE)
sd(asr_2018_female$qn18b, na.rm = TRUE)

#female and males degree before US
obj1 <- table(asr_2018_female$highestDegreeBeforeUS)
obj2 <- table(asr_2018_male$highestDegreeBeforeUS)

kbl(obj1, "latex")
kbl(obj2, "latex")

obj3 <- table(asr_2018$countryOfBirth)
kbl(obj3, "latex")

#female and males work in the US
table(asr_2018_female$qn19b)
table(asr_2018_male$qn19b)

#female and male attend a job last week
table(asr_2018_female$workAJobLastWeek)
table(asr_2018_male$workAJobLastWeek)

table(asr_2018$gender)

#average age
mean(asr_2018_female$age, na.rm=TRUE)
mean(asr_2018_male$age, na.rm=TRUE)
sd(asr_2018_female$age, na.rm= TRUE)
sd(asr_2018_male$age, na.rm=TRUE)

table(asr_2018_female$howWellDidTheySpeakEnglishBeforeUS)
table(asr_2018_male$howWellDidTheySpeakEnglishBeforeUS)


mean(asr_2018_female_notatall$dollarsPerHourAtJob, na.rm = TRUE)
mean(asr_2018_female_notwell$dollarsPerHourAtJob, na.rm = TRUE)
mean(asr_2018_female_well$dollarsPerHourAtJob, na.rm = TRUE)
mean(asr_2018_female_verywell$dollarsPerHourAtJob, na.rm = TRUE)
#

mean(asr_2018_male_notatall$dollarsPerHourAtJob, na.rm = TRUE)
mean(asr_2018_male_notwell$dollarsPerHourAtJob, na.rm = TRUE)
mean(asr_2018_male_well$dollarsPerHourAtJob, na.rm = TRUE)
mean(asr_2018_male_verywell$dollarsPerHourAtJob, na.rm = TRUE)
#

table(asr_2018$countryOfBirth)

table2 <- table(asr_2018_university$howWellDidTheySpeakEnglishBeforeUS)
kbl(table2, "latex")

table1 <- table(asr_2018$howWellDidTheySpeakEnglishBeforeUS)
kbl(table1, "latex")

mean(asr_2018$howManyYearsOfSchoolingBeforeUS, na.rm=TRUE)
mean(asr_2018$age, na.rm = TRUE)
mean(asr_2018$numppl, na.rm=TRUE)
mean(asr_2018$qn6b, na.rm=TRUE)
#mean(asr_2018, na.rm=TRUE) #how much money did they recieve per month
mean(asr_2018$qn38b, na.rm=TRUE)


sd(asr_2018$numppl, na.rm=TRUE)

reg2 <- lm(asr_2018$qn6b ~ asr_2018$numppl)
summary(reg2)     


fem <-hist(asr_2018_female$dollarsPerHourAtJob,breaks=20, xlim=c(0,30), main = "", xlab = "Dollars per Hour at Job")
man <- hist(asr_2018_male$dollarsPerHourAtJob, breaks=35, xlim = c(0,30), main = "", xlab = "Dollars per Hour at Job")

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

plot(fem, col = c2, ylim=c(0,300), xlim=c(0,30), breaks=35) # Plot 1st histogram using a transparent color
plot(man, col = c1, add = TRUE, breaks=35) # Add 2nd histogram using different color

#Statistics for Table 2
favstats(asr_2018_female$dollarsPerHourAtJob)
quantile(asr_2018_female$dollarsPerHourAtJob, na.rm=TRUE)
quantile(asr_2018_male$dollarsPerHourAtJob, na.rm=TRUE)
median(asr_2018_female$dollarsPerHourAtJob, na.rm=TRUE)
median(asr_2018_male$dollarsPerHourAtJob, na.rm=TRUE)
mean(asr_2018_female$dollarsPerHourAtJob, na.rm=TRUE)
mean(asr_2018_male$dollarsPerHourAtJob, na.rm=TRUE)

mean(asr_2018_somalia_women$dollarsPerHourAtJob, na.rm=TRUE)
mean(asr_2018_somalia_men$dollarsPerHourAtJob, na.rm=TRUE)

mean(asr_2018_somalia_women$howManyYearsOfSchoolingBeforeUS, na.rm=TRUE)
mean(asr_2018_somalia_men$howManyYearsOfSchoolingBeforeUS, na.rm=TRUE)


#Country regressions
somalia <- lm(asr_2018_somalia$dollarsPerHourAtJob ~ asr_2018_somalia$gender+asr_2018_somalia$howManyYearsOfSchoolingBeforeUS)
summary(somalia)
coef(somalia)

nosomalia <- lm(NoSomalia$dollarsPerHourAtJob ~ NoSomalia$gender + NoSomalia$howManyYearsOfSchoolingBeforeUS)
summary(nosomalia)

bhutan <- lm(asr_2018_bhutan$dollarsPerHourAtJob ~ asr_2018_bhutan$gender+asr_2018_bhutan$howManyYearsOfSchoolingBeforeUS)
summary(bhutan)

iraq <- lm(asr_2018_iraq$dollarsPerHourAtJob ~ asr_2018_iraq$gender+asr_2018_iraq$howManyYearsOfSchoolingBeforeUS)
summary(iraq)

iran <- lm(asr_2018_iran$dollarsPerHourAtJob ~ asr_2018_iran$gender+asr_2018_iran$howManyYearsOfSchoolingBeforeUS)
summary(iran)

table(asr_2018$somali)

ggplot() + xlim(0,20)+
  geom_function(fun = function(x) 11.43+0.22*x) + #somalia
  geom_function(fun=function(x) 9.7499 + 0.2180*x)

mean(NoSomalia$dollarsPerHourAtJob, na.rm=TRUE)
mean(asr_2018_somalia$dollarsPerHourAtJob, na.rm=TRUE)
ggplot() + xlim(0,20)+
  geom_function(fun = function(x) 11.43+0.22*x) + #somalia
  geom_function(fun = function(x) 11.12+0.19873*x) + #bhutan
  geom_function(fun = function(x) 8.23621+0.28256*x) + #iraq
  geom_function(fun = function(x) 4.6108+0.5734*x) + #iran
  