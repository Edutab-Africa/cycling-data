suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(writexl))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(mudata2))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library("rstudioapi"))
suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(dplyr))


setwd(dirname(getActiveDocumentContext()$path))


########
Cycling<-read_excel("cyclist_initial.xlsx",sheet = "Cycling_Nairobi")
Cycling<-Cycling %>%
  rename("Gender"="Gender of the participant" ,"Other_Gender"="Please specify your other Gender?",
         "Age"="Age of the participant" ,"Disability"="Do you have any form of disability?",
         "form_disability"="if Yes,what form of disability?","Are_you"="Are you a?",
         "motorist"="Are you a?/Motorist","motorcyclist"="Are you a?/Motorcyclist",
         "Cyclist"="Are you a?/Cyclist","Pedestrian"="Are you a?/Pedestrian",
         "Other"="Are you a?/Other" ,"specify_other"="specify what other way you are?",
         "average_time_cycle"="Estimate the average amount of time you cycle in a week ?",
         "frequently_cycle"="How Frequently do you cycle in a week?",
         "Other_frequently_cycle"="specify how other frequently you cycle in a week",
         "Reason_cycling"="What are your primary reasons for cycling?",
         "Fitness"="What are your primary reasons for cycling?/Fitness",
         "sport"="What are your primary reasons for cycling?/Sport",
         "commuting_to_work"="What are your primary reasons for cycling?/Commuting to Work",
         "commuting_to_school"="What are your primary reasons for cycling?/Commuting to School",
         "sociability_cycling"="What are your primary reasons for cycling?/Sociability of cycling",
         "other_reason"="What are your primary reasons for cycling?/Other specify",
         "other_use_cycling"="specify other use of cycling",
         "member_cycling_club"="Are you a member of any cycling club in Kenya?",
         "rate"="Please rate the following :",
         "Install_bike_facilities"="Installation of bike Facilities & parking",
         "Bicycle_lanes"="Developing of bicycle lanes - Construction of Cycling Lanes",
         "Education_awareness"="Cycling Education and awareness programs",
         "Master_plan"="Introducing a cycling master plan",
         "Safety_checks"="I perform all road safety checks when driving, e.g. mirror use, blind spot checks, etc",
         "Blame_for_collision"="In most event of car and cyclist collision who carries the blame?",
         "Reason_blame"="Reason for the above answer?",
         "Law_abiding"="Car drivers are typically more law-abiding than cyclists?",
         "Non_motorized_policies"="Are you aware if there exist non-motorized transport policies in Nairobi?",
         "Cycling_safe"="Do you consider cycling in Nairobi safe?",
         "Barriers"="From the list of the following barriers to cycling, please indicate  the level of the following:",
         "Driving_culture"="(A). Motorists' attitudes and poor driving culture",
         "Bad_weather"="(B).Bad weather",
         "Business_Activities"="(C).Encroachment of business activities",
         "Crime_safety"="(D).Crime and safety",
         "Poor_Infrastructure"="(E). Poor infrastructure ( Drainage, Road design & lack of signages)" ,
         "Sexual_harassment"="(F). Sexual harassment",
         "Boda"="(G). Boda Bodas",
         "Describe_cycling_infrastructutre"="In your own words, how do you describe Nairobi's cycling infrastructure",
         "Roadusers_by_motorists"="Do you think cyclists are considered road users by motorists?",
         "Reason"="Give a reason for your response above.",
         "Training_Courses"="Have you taken any bicycle training courses in Kenya?",
         "Safety_Education"="How do you feel about introducing cycling safety education?",
         "Diverse_cycling_society"="What change would you like to see  to ensure we have a diverse cycling community regarding gender, age, and social status?",
         "cycling_accident"="Have you ever been involved in a cycling related accident or a near-miss event where you feared for your physical health?",
         "Cause_of_accident"="Based on above response, what was the cause of  the cycling related accident or a near-miss event where you feared for your physical health?",
         "Other_causes_accident"="Specify the other cause of the accident  or a near-miss event where you feared for your physical health?",
         "Respond"="how did you respond?",
         "Yell"="how did you respond?/Yell",
         "Hoot"="how did you respond?/Hoot",
         "Flash_lights"="how did you respond?/Flash car Lights",
         "Nothing"="how did you respond?/Nothing",
         "From_motorized_to_cycling"=". Name one factor that would make you change from motorized means of transportation to cycling on a daily basis:",
         "Suggestion"="Any suggestions for improving cycling in Nairobi"  ) 
  
  
  
  ##(Cycling)

##deleting spaces with NAN>50%
Cycling1<-Cycling[,!sapply(Cycling, function(x)mean(is.na(x)))>0.5]


#deleting columns
Cycling1<-subset(Cycling1, select = -c(Are_you,Reason_cycling,frequently_cycle,Respond))
Cycling1
