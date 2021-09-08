################################### loading the data ################################## 
hrdat<-read.csv("ibmhrdata.csv")

############################## loading necessary libraries ############################
library(dplyr)
library(ggplot2)
library(reshape)
library(plotly)
############################# data pre-processing ######################################
names(hrdat)[1] <- "Age"
hrdat1<-hrdat
sapply(hrdat1,function(x) sum(is.na(x)))  ###to check the no. of NA(s) in different columns of the dataset
############################# Department ############################# 
df29<-hrdat1%>%group_by(Department,Attrition)%>%summarise(Count=n())
df30<-df29%>%group_by(Department)%>%summarise(tot=sum(Count))
df29<-merge(df29,df30,by="Department")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df29,aes(x=factor(Department),y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Department in Attrition')+
  labs(x="Department",y="Proportion")

#############################Age ############################# 
ggplot(hrdat1,aes(x=Age, fill=factor(Attrition))) + 
  geom_density(alpha=0.5) + scale_fill_discrete(name="Attrition",labels=c("No","Yes")) + 
  ggtitle('Effect of Age in Attrition') +
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  labs(x="Age",y="Density") 


#############################  MaritalStatus ############################# 
df7<-hrdat1%>%group_by(MaritalStatus,Attrition)%>%summarise(Count=n())
df8<-df7%>%group_by(MaritalStatus)%>%summarise(tot=sum(Count))
df7<-merge(df7,df8,by="MaritalStatus")%>%mutate(Proportion=Count/tot)

#plotting the count
ggplot(hrdat1,aes(Attrition)) + 
  geom_bar(color='black', aes(fill=factor(Attrition)), alpha=0.7, position = 'dodge') + 
  facet_wrap(~MaritalStatus) + ggtitle('Effect of Marital Status in Attrition') + 
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  scale_fill_manual(values = c("Purple","Magenta"))+
  labs(x="Marital Status",y="Count")
#plotting the proportion
ggplot(df7,aes(x=MaritalStatus,y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(name="Attrition",values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Marital Status in Attrition')+
  labs(x="Marital Status",y="Proportion")

###optional##########################  Gender ############################# 
df5<-hrdat1%>%group_by(Gender,Attrition)%>%summarise(Count=n())
df6<-df5%>%group_by(Gender)%>%summarise(tot=sum(Count))
df5<-merge(df5,df6,by="Gender")%>%mutate(Proportion=Count/tot)

###plotting the count
ggplot(hrdat1,aes(Attrition)) + 
  geom_bar(color='black', aes(fill=factor(Attrition)), alpha=0.7, position = 'dodge') + 
  facet_wrap(~Gender) + ggtitle('Effect of Gender in Attrition') + 
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  scale_fill_manual(values = c("Purple","Magenta"))+
  labs(x="Gender",y="Count")
#plotting the proportion
ggplot(df5,aes(x=Gender,y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Gender in Attrition')+
  labs(x="Gender",y="Proportion")

############################# BusinessTravel #############################
df1<-hrdat1%>%group_by(BusinessTravel,Attrition)%>%summarise(Count=n())
df2<-df1%>%group_by(BusinessTravel)%>%summarise(tot=sum(Count))
df1<-merge(df1,df2,by="BusinessTravel")%>%mutate(Proportion=Count/tot)
df1$BusinessTravel <- factor(df1$BusinessTravel,levels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"))

ggplot(df1,aes(x=BusinessTravel,y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Business Travel in Attrition')+
  labs(x="Category of Business Travel",y="Proportion")

############################# MonthlyIncome #############################
ggplot(hrdat1,aes(x=MonthlyIncome, fill=factor(Attrition))) + 
  geom_density(alpha=0.4) + scale_fill_discrete(name="Attrition",labels=c("No","Yes")) + 
  ggtitle('Effect of Monthly Income in Attrition')+
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  labs(x="Monthly Income in USD",y="Density")
ggplot(hrdat1, aes(MonthlyIncome, after_stat(density), color = Attrition)) +
  geom_freqpoly(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
  ggtitle('Effect of Monthly Income in Attrition')+
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  labs(x="Monthly Income in USD",y="Density")
ggplot(hrdat1, aes(MonthlyIncome, after_stat(density), color = Attrition)) +
  geom_freqpoly(binwidth = 2000) +
  ggtitle('Effect of Monthly Income in Attrition')+
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  labs(x="Monthly Income in USD",y="Density")

###optional########################## DistanceFromHome #############################
# plotting the count
ggplot(hrdat1, aes(DistanceFromHome, color = Attrition)) +
  geom_freqpoly(binwidth = 1)+
  ggtitle('Effect of Distance From Home in Attrition')+
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  labs(x="Distance From Home",y="Density")
# plotting the proportion
ggplot(hrdat1, aes(DistanceFromHome, after_stat(density), color = Attrition)) +
  geom_freqpoly(binwidth = 1)+
  ggtitle('Effect of Distance From Home in Attrition')+
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  labs(x="Distance From Home",y="Density")

############################# Education #############################
# 1 'Below College' 2 'College'  3 'Bachelors' 4 'Masters'  5 'PhD'
df13<-hrdat1%>%group_by(Education,Attrition)%>%summarise(Count=n())
df14<-df13%>%group_by(Education)%>%summarise(tot=sum(Count))
df13<-merge(df13,df14,by="Education")%>%mutate(Proportion=Count/tot)

#plotting the proportion 

ggplot(df13,aes(x=factor(Education),y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Education in Attrition')+
  labs(x="Education",y="Proportion") +scale_x_discrete(breaks=c("1","2","3","4","5"),labels=c('Below College','College','Bachelors','Masters','PhD'))

###optional########################## EducationField ############################# 
df27<-hrdat1%>%group_by(EducationField,Attrition)%>%summarise(Count=n())
df28<-df27%>%group_by(EducationField)%>%summarise(tot=sum(Count))
df27<-merge(df27,df28,by="EducationField")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df27,aes(x=factor(EducationField),y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Education Field in Attrition')+
  labs(x="Education Field",y="Proportion")

#############################  Work Environment Satisfaction ############################# 
# 1 'Low'
# 2 'Medium'
# 3 'High'
# 4 'Very High'
df25<-hrdat1%>%group_by(EnvironmentSatisfaction,Attrition)%>%summarise(Count=n())
df26<-df25%>%group_by(EnvironmentSatisfaction)%>%summarise(tot=sum(Count))
df25<-merge(df25,df26,by="EnvironmentSatisfaction")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df25,aes(x=factor(EnvironmentSatisfaction),y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Work Environment Satisfaction in Attrition')+
  labs(x="Work Environment Satisfaction",y="Proportion") +scale_x_discrete(breaks=c("1","2","3","4"),labels=c("1"='Low',"2"='Medium',"3"='High',"4"='Very High'))

#############################  JobInvolvement ############################# 
# 1 'Low'
# 2 'Medium'
# 3 'High'
# 4 'Very High'
df23<-hrdat1%>%group_by(JobInvolvement,Attrition)%>%summarise(Count=n())
df24<-df23%>%group_by(JobInvolvement)%>%summarise(tot=sum(Count))
df23<-merge(df23,df24,by="JobInvolvement")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df23,aes(x=factor(JobInvolvement),y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Job Involvement in Attrition')+
  labs(x="Job Involvement",y="Proportion") +scale_x_discrete(breaks=c("1","2","3","4"),labels=c("1"='Low',"2"='Medium',"3"='High',"4"='Very High'))

#############################  JobSatisfaction ############################# 
# 1 'Low'
# 2 'Medium'
# 3 'High'
# 4 'Very High'
df21<-hrdat1%>%group_by(JobSatisfaction,Attrition)%>%summarise(Count=n())
df22<-df21%>%group_by(JobSatisfaction)%>%summarise(tot=sum(Count))
df21<-merge(df21,df22,by="JobSatisfaction")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df21,aes(x=factor(JobSatisfaction),y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Job Satisfaction in Attrition')+
  labs(x="Job Satisfaction",y="Proportion") +scale_x_discrete(breaks=c("1","2","3","4"),labels=c("1"='Low',"2"='Medium',"3"='High',"4"='Very High'))

#############################  JobLevel #############################
df19<-hrdat1%>%group_by(JobLevel,Attrition)%>%summarise(Count=n())
df20<-df19%>%group_by(JobLevel)%>%summarise(tot=sum(Count))
df19<-merge(df19,df20,by="JobLevel")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df19,aes(x=factor(JobLevel),y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Job Level in Attrition')+
  labs(x="Job Level",y="Proportion")  

############################# JobRole ############################# 
df17<-hrdat1%>%group_by(JobRole,Attrition)%>%summarise(Count=n())
df18<-df17%>%group_by(JobRole)%>%summarise(tot=sum(Count))
df17<-merge(df17,df18,by="JobRole")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df17,aes(x=factor(JobRole),y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),axis.text.x = element_text(angle = 90),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Job Role in Attrition')+
  labs(x="Job Role",y="Proportion")  
 
#############################  OverTime ############################# 
df3<-hrdat1%>%group_by(OverTime,Attrition)%>%summarise(Count=n())
df4<-df3%>%group_by(OverTime)%>%summarise(tot=sum(Count))
df3<-merge(df3,df4,by="OverTime")%>%mutate(Proportion=Count/tot)

#plotting the count
ggplot(hrdat1,aes(Attrition)) + 
  geom_bar(color='black', aes(fill=factor(Attrition)), alpha=0.7, position = 'dodge') + 
  facet_wrap(~OverTime) + ggtitle('Effect of Overtime in Attrition') + 
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  scale_fill_manual(values = c("Purple","Magenta"))+
  labs(x="OverTime",y="Count")
  
#plotting the proportion
ggplot(df3,aes(x=OverTime,y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of OverTime in Attrition')+
  labs(x="OverTime",y="Proportion")


############################# RelationshipSatisfaction #############################
# 1 'Low'
# 2 'Medium'
# 3 'High'
# 4 'Very High'
df9<-hrdat1%>%group_by(RelationshipSatisfaction,Attrition)%>%summarise(Count=n())
df10<-df9%>%group_by(RelationshipSatisfaction)%>%summarise(tot=sum(Count))
df9<-merge(df9,df10,by="RelationshipSatisfaction")%>%mutate(Proportion=Count/tot)

#plotting the proportion
ggplot(df9,aes(x=RelationshipSatisfaction,y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Relationship Satisfaction in Attrition')+
  labs(x="Relationship Satisfaction",y="Proportion")

############################# TotalWorkingYears #############################
ggplot(hrdat1,aes(x=TotalWorkingYears, fill=factor(Attrition))) + 
  geom_density(alpha=0.5) + 
  scale_fill_discrete(name="Attrition",labels=c("No","Yes")) + 
  ggtitle('Effect of "Total Working Years" in Attrition')+
  labs(x="Total Working Years",y="Density")+
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF")) 

############################# WorkLifeBalance #############################
# 1 'Bad'
# 2 'Good'
# 3 'Better'
# 4 'Best'
df11<-hrdat1%>%group_by(WorkLifeBalance,Attrition)%>%summarise(Count=n())
df12<-df11%>%group_by(WorkLifeBalance)%>%summarise(tot=sum(Count))
df11<-merge(df11,df12,by="WorkLifeBalance")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df11,aes(x=factor(WorkLifeBalance),y=Proportion)) +
  geom_col(aes(fill=Attrition),color="white",position = "dodge2") +
  scale_fill_manual(values = c("Purple","Magenta")) +
  theme(axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Work Life Balance in Attrition')+
  labs(x="Work Life Balance",y="Proportion") +scale_x_discrete(breaks=c("1","2","3","4"),labels=c("1"='Bad',"2"='Good',"3"='Better',"4"='Best'))

############################# YearsAtCompany #############################
ggplot(hrdat1,aes(x=YearsAtCompany, fill=factor(Attrition))) + 
  geom_density(alpha=0.5) + 
  scale_fill_discrete(name="Attrition",labels=c("No","Yes")) + 
  ggtitle('Effect of "Years at company" in Attrition')+
  labs(x="Years at company",y="Density")+
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF")) 

###optional########################## YearsSinceLastPromotion #############################  
ggplot(hrdat1,aes(x=YearsSinceLastPromotion, fill=factor(Attrition))) + 
  geom_density(alpha=0.5) + 
  scale_fill_discrete(name="Attrition",labels=c("No","Yes")) + 
  ggtitle('Effect of "Years Since Last Promotion" in Attrition')+
  labs(x="Years since last promotion",y="Density")+
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))

############################# YearsInCurrentRole ############################# 
ggplot(hrdat1,aes(x=YearsInCurrentRole, fill=factor(Attrition))) + 
  geom_density(alpha=0.5) + 
  scale_fill_discrete(name="Attrition",labels=c("No","Yes")) + 
  ggtitle('Effect of "Years In Current Role" in Attrition')+
  labs(x="Years in current role",y="Density")+
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF")) 

############################# YearsWithCurrManager #############################
ggplot(hrdat1,aes(x=YearsWithCurrManager, fill=factor(Attrition))) + 
  geom_density(alpha=0.5) + 
  scale_fill_discrete(name="Attrition",labels=c("No","Yes")) + 
  ggtitle('Effect of "Years With Curr Manager" in Attrition')+
  labs(x="Years with curre nt manager",y="Density")+
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF")) 
