#loading packages
library(readxl)
library(writexl)
library(dplyr)
library(tidyverse)
library(janitor)
library("rstudioapi")
library(openxlsx)
library(mudata2)
library(ggplot2)

cyclistdata<-read_excel("C:/Users/Janice/Desktop/Cyclist-initial.xlsx")
view(cyclistdata)
#getting to know the structure of the data
#glimpse(cyclistdata)
#names(cyclistdata)

#deleting columns
data1<-subset(cyclistdata, select = -c(start,end,`Critical Mass Nairobi, (A group of cyclists who ride together every last Saturday of the month with a goal of having  many people on bicycles and transform Nairobi into a cycling city)  invites you to take this survey to collect data on road users (cyclists, motorists, pedestrians, motorcyclists etc)  perception towards cycling on Kenyan roads.`,`This will help in the development of a public education document, programs, and policies to educate/sensitize road users about the safety of pedal cyclists who share the road with other motorized means of transport.`,`In case you have any queries, reach out to us via,  criticalmassnairobi@gmail.com, info@edutab.africa, or call/text +254 725291116.`,`Thank you.`))
#data1
data1a<-subset(data1, select = -c(`Are you a?`,`What are your primary reasons for cycling?`,`how did you respond?`))

 #deleting spaces with NAN>50%
Dat<-data1a[,!sapply(data1a, function(x)mean(is.na(x)))>0.5]

Dat<-as_tibble(Dat)

 #renaming headers
names(Dat)[names(Dat) == "Gender of the participant"] <- "Gender"
names(Dat)[names(Dat) == "Age of the participant"] <- "Age"
names(Dat)[names(Dat) == "Do you have any form of disability?"] <- "Disability"
names(Dat)[names(Dat) == "Are you a?/Motorist"] <- "Motorist"
names(Dat)[names(Dat) == "Are you a?/Motorcyclist"] <- "Motorcyclist"
names(Dat)[names(Dat) == "Are you a?/Cyclist"] <- "Cyclist"
names(Dat)[names(Dat) == "Are you a?/Pedestrian"] <- "Pedestrian"
names(Dat)[names(Dat) == "Are you a?/Other"] <- "other"
names(Dat)[names(Dat) == "Estimate the average amount of time you cycle in a week ?"] <- "Average cycle time"
names(Dat)[names(Dat) == "How Frequently do you cycle in a week?"] <- "frequently cycle"
names(Dat)[names(Dat) == "What are your primary reasons for cycling?/Fitness"] <- "Fitness"
names(Dat)[names(Dat) == "What are your primary reasons for cycling?/Sport"] <- "Sport"
names(Dat)[names(Dat) == "What are your primary reasons for cycling?/Commuting to Work"] <- "Work"
names(Dat)[names(Dat) == "What are your primary reasons for cycling?/Commuting to School"] <- "School"
names(Dat)[names(Dat) == "What are your primary reasons for cycling?/Sociability of cycling"] <- "Sociability of cycling"
names(Dat)[names(Dat) == "What are your primary reasons for cycling?/Other specify"] <- "other cycling reasons"

# col delete
Dat<-subset(Dat, select = -c(`frequently cycle`,other))

Dat















