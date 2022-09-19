suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(writexl))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(mudata2))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library("rstudioapi"))
suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(formattable))
suppressPackageStartupMessages(library(latticeExtra))
suppressPackageStartupMessages(library(barplot3d))

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

Demographic_Gender_Age <- Cycling %>%
  select(Gender,Age,Disability,motorist,motorcyclist,Cyclist,Pedestrian,Other ) %>%
  pivot_longer (!c(Gender, Age,Disability), names_to = "Are_you_a", values_to = "Responses")%>%
  filter(Responses != "0")%>%
  group_by(Age) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(415), 3)) %>% 
  arrange(desc(freq))
ggplot(Demographic_Gender_Age,aes(x = Age,
                                  y = freq,
                                  fill = Age,
                                  label=scales::percent(freq))) +
  geom_bar(position="dodge",stat="identity",width = 0.3) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise")) +
  facet_wrap(~Age)+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 8))+
  geom_text(nudge_y= .01,
            color="black",
            size = 4,
            fontface="bold")+
  labs(x = "Age and gender",
       title = "Percentage of respondents by age and gender")+
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(size = 20),
        axis.text.x = element_text(color = "black", size = 12,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 15,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))
  print(Demographic_Gender_Age)
  ggsave("Number of road users when grouped by gender and age.png")



 Demographic_Gender<- Cycling %>%
  select(Gender,motorist,motorcyclist,Cyclist,Pedestrian,Other ) %>%
   pivot_longer (!c(Gender), names_to = "Are_you_a", values_to = "Responses")%>%
   filter(Gender != "Prefer not to say")%>%
   filter(Responses != "0")%>%
   group_by(Are_you_a,Gender) 
   
   
   ggplot(Demographic_Gender,aes(x=Are_you_a, fill=Are_you_a))+
   geom_bar(position="dodge",width = 0.6) +
     scale_fill_manual(values = c("mediumseagreen","chartreuse2","chocolate2",
                                  "antiquewhite2","darkgoldenrod2"))+
   facet_wrap(~Gender)+
   labs(x = "Road users",
        title = "Number of road users when grouped by gender")+
     scale_y_continuous(breaks=seq(0,400,50)) +
   theme(legend.position = "none",
         panel.spacing = unit(2, "lines"),
         strip.text.x = element_text(size = 30),
         axis.text.x = element_text(color = "black", size = 12,angle = 0,face = "bold"),
         axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
         axis.title.x = element_text(colour="black", size = 15,face = "bold"),
         axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
         legend.title = element_text(color = "black", size = 15,face = "bold"),
         legend.text = element_text(color = "black", size = 15,face = "bold"),
         plot.title = element_text(face = "bold",hjust = 0.5))
   print(Demographic_Gender)
   ggsave("Number of road users when grouped by gender.png")
   
   
   # ggplot(s, aes(y = mean_p, x = Gender, fill = Gender)) +
   #   geom_bar(position="dodge", stat="identity",width = 0.3) +
   #   scale_fill_manual(values = c("lightsalmon2","darkturquoise")) +
   #   facet_wrap(~Assessment) +
   #   labs(y = "Mean percentage",
   #        x = "Gender",
   #        title = "Level 1 Assessment: Mean percentage by Gender") +
   #   scale_y_continuous(breaks=seq(0,70,10)) +
   #   geom_text(mapping = aes(Gender,mean_p, label = paste0("% = ",mean_p)),vjust=-1,
   #             hjust=.5,
   #             size=4)+
   #   theme(axis.text.x = element_text(color = "black", size = 15,angle = 0),
   #         axis.text.y = element_text(color="black", size = 15, angle = 0),
   #         axis.title.x = element_text(colour="black", size = 15),
   #         axis.title.y = element_text(colour="black", size = 15,hjust = 0.5),
   #         legend.title = element_text(color = "black", size = 15),
   #         legend.text = element_text(color = "black", size = 15),
   #         plot.title = element_text(face = "bold",hjust = 0.5),
   #         legend.position = "none",
   #         strip.text.x = element_text(
   #           size = 12,face = "bold")) 
   
   Demographic_Age <- Cycling %>%
     select(Age,motorist,motorcyclist,Cyclist,Pedestrian,Other ) %>%
     pivot_longer (!c(Age), names_to = "Are_you_a", values_to = "Responses")%>%
     filter(Responses != "0")
   
   ggplot(Demographic_Age,aes(x=Are_you_a, fill=Are_you_a))+
     geom_bar(position="dodge",width = 0.6) +
     scale_fill_manual(values = c("mediumseagreen","chartreuse2","chocolate2",
                                  "antiquewhite2","darkgoldenrod2"))+
     facet_wrap(~Age)+
     labs(x = "Road users",
          title = "Number of road users when grouped by Age")+
     scale_y_continuous(breaks=seq(0,400,50)) +
     theme(legend.position = "none",
           strip.text.x = element_text(size = 30),
           panel.spacing = unit(2, "lines"),
           axis.text.x = element_text(color = "black", size = 7,angle = 0,face = "bold"),
           axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
           axis.title.x = element_text(colour="black", size = 15,face = "bold"),
           axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
           legend.title = element_text(color = "black", size = 15,face = "bold"),
           legend.text = element_text(color = "black", size = 15,face = "bold"),
           plot.title = element_text(face = "bold",hjust = 0.5))
   print(Demographic_Age)
   ggsave("Number of road users when grouped by age.png")
 
  
 #   group_by(Gender) %>%
 #   summarise(n=n())%>%
 #   mutate(Perc = round(n / sum(n), 3)) %>% 
 #   arrange(desc(Perc))
 #   
 #   as.data.frame(Demographic)
 # write.csv(Demographic, "Gender.csv")
 # 
   
   
 
 
 #summarise(n = mean(percent_rank(Cyclist)), n = n())
   #summarise(n=n())
 #summarise(Total = n())%>%
   

names(Cycling)
Road_user<- Cycling%>%
  select(Gender,Age,Disability,Are_you)%>%
  group_by(Are_you)%>%
  summarise(n=n())
as.data.frame(Road_user)
write.csv(Road_user, "Road_users1.csv")

  

####### Time of cycling 
   Time<-Cycling%>%
     select(Gender,Age,Disability,frequently_cycle,average_time_cycle)%>%
     group_by(Gender,frequently_cycle)%>%
     filter(frequently_cycle != "NA")%>%
     summarise(count=n())
   ggplot(Time,aes(x=frequently_cycle,y=count, fill=frequently_cycle))+
     geom_bar(position="dodge",stat="identity",width = 0.6) +
     scale_fill_manual(values = c("mediumseagreen","chartreuse2","chocolate2",
                                  "darkgoldenrod2"))+
     facet_wrap(~Gender)+
     labs(x = "frequently_cycle",
          title = "How frequently the respondents cycle in a week")+
     scale_y_continuous(breaks=seq(0,400,50)) +
     theme(legend.position = "none",
           strip.text.x = element_text(size = 30),
           panel.spacing = unit(2, "lines"),
           axis.text.x = element_text(color = "black", size = 7,angle = 0,face = "bold",hjust = 0.5),
           axis.text.y = element_text(color="black", size = 15, angle = 0,face = "bold"),
           axis.title.x = element_text(colour="black", size = 15,face = "bold"),
           axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
           legend.title = element_text(color = "black", size = 15,face = "bold"),
           legend.text = element_text(color = "black", size = 15,face = "bold"),
           plot.title = element_text(face = "bold",hjust = 0.5))
   print(Demographic_Age)
   ggsave("frequently_cycle.png")
   
   
   
   
   
   
   
   as.data.frame(Time)
   write.csv(Time, "Time.csv")
     
     
     
   
   
  

