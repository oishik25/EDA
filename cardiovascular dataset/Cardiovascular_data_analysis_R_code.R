################################### loading the data ################################## 
card <- read.csv('cardio_train.csv',sep=';')

############################## loading necessary libraries ############################
library(dplyr)
library(ggplot2)
library(cowplot)
library(reshape)

################################### modifying the data ################################
card1<-card
head(card1)
sapply(card1,function(x) sum(is.na(x)))                             # no NA values

#column - 'age'
card1 <- within(card1, age <- age%/%365); head(card1) 
max(card1$age)
min(card1$age)

#column - 'height', 'weight', 'ap_hi', 'ap_lo'
card1 %>% select(height,weight,ap_hi,ap_lo) %>% apply(.,2,max)
card1 %>% select(height,weight,ap_hi,ap_lo) %>% apply(.,2,min)
card1 %>% select(ap_hi,ap_lo) %>% apply(.,2,function(x) sum(x<0))   # number of negative values seperately in both columns
sum(card1$ap_hi<0 | card1$ap_lo<0)                                  # number of rows with ap_hi or ap_lo being negative

#new columns - 'BMI', 'BP', 'Pulse_pressure', 'Age_group', 'Tension_class', 'MAP', 'BMI_status'
card1<- card1 %>% mutate(BMI = weight/(height*height)*100*100)
card1$BMI <- round(card1$BMI,digits = 2)
card1<-card1 %>% filter(BMI<250)
card1 <- card1 %>% mutate(Pulse_pressure = ap_hi-ap_lo)
card1<-card1 %>% filter(Pulse_pressure>=10)
card1 <- card1 %>% mutate(Age_group = cut(age,breaks=c(20,30,40,50,60,70),include.lowest = T))
card1<-card1 %>% mutate(MAP = ap_lo+(Pulse_pressure)/3)
f1 <- function(x,y){
  if(x<120 & y<80){
    return(1)            # 1 --> Normal
  }
  else if(x<139 | y<89){
    return(2)            # 2 --> Prehypertension
  }
  else if(x<159 | y<99){
    return(3)            # 3 --> Stage 1 Hypertension
  }
  else{
    return(4)            # 4 --> Stage 2 Hypertension
  }
}
card1<-card1 %>% mutate(Tension_class=mapply(f1,ap_hi,ap_lo))
######BMI	Weight Status
#Below 18.5	Underweight ------------->1
#18.5-24.9	Normal      ------------->2
#25.0-29.9	Overweight  ------------->3
#30.0 and Above	Obese   ------------->4
card1$bmi_status<-ifelse(card1$BMI<18.5,1,ifelse((card1$BMI<25)&(card1$BMI>=18.5),2,ifelse((card1$BMI<30)&(card1$BMI>=25),3,4)))
################################## data with positive cardiovascular diseases #############################
card_yes<-card1 %>% filter(cardio==1)     
nrow(card_yes)

######################################### Cleaning the data #######################################
# boxplot to check outliers
card1melt<-melt(card1[,c(3,6,7,14,15,17)],id.var="gender")
##### PLOT 1: Type G. 4 or more variables
boxplot(card1[,c(6,7,14,15,17)])

##### PLOT2: TYPE G. 4 or more variables
ggplot(card1melt, aes(y=value, x=variable)) +
  geom_boxplot(aes(fill=as.factor(gender))) +
  scale_fill_discrete(name="gender",labels=c("F","M")) + 
  ylim(0,250) +
  labs(title = 'Outlier check (Before cleaning)')


outliers <- function(x) {
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  IQR = Q3-Q1
  lower = Q1 - (IQR*1.5)
  upper = Q3 + (IQR*1.5)
  x > upper | x < lower
}

remove_outliers <- function(card3, cols = names(card3)) {
  for (i in cols) {
    card3 <- card3[!outliers(card3[[i]]),]
  }
  card3
}

card2 <- remove_outliers(card1,c('ap_hi','ap_lo','BMI','Pulse_pressure','MAP'))

# boxplot for double check
card2melt<-melt(card2[,c(3,6,7,14,15,17)],id.var="gender")

##### PLOT 3: Type G. 4 or more variables
ggplot(card2melt, aes(variable,value)) + geom_boxplot(aes(fill=as.factor(gender))) +
  labs(title = 'Outlier check (After cleaning)') +  scale_fill_discrete(name="gender",labels=c("F","M"))
##### PLOT 4: Type G. 4 or more variables
ggplot(card2melt, aes(variable,value)) + geom_violin(aes(fill=as.factor(gender))) + scale_fill_discrete(name="gender",labels=c("F","M")) + ggtitle('outliers with distribution (after cleaning)')

########################################### plotting for insights #################################
# heatmap of the whole dataset
card3<-card2
cormat <- round(cor(subset(card3, select = -c(Age_group))),2)
melted_cormat <- melt(cormat,na.rm=TRUE)
head(melted_cormat)
##### PLOT 5: G. 4 or more variables
hmap_card<-ggplot(data = melted_cormat, aes(x=X2, y=X1, fill=value)) + geom_tile(color = "white")+
  geom_text(aes(X2,X1, label = value), color = "black", size = 4)+
  scale_fill_gradient2(low = "yellow", high = "green", 
                       limit = c(-1,1), space = "Lab", 
                       name="  Pearson\nCorrelation")+ #The function scale_fill_gradient2 is used with the argument limit = c(-1,1) as correlation coefficients range from -1 to 1.
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()  #coord_fixed() : this function ensures that one unit on the x-axis is the same length as one unit on the y-axis.
hmap_card+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0))

####### categorizing the entries of the categorical columns 
card2$gender <- factor(card2$gender,levels = c('1','2'), labels= c('F','M'))
card2$smoke <- factor(card2$smoke,levels = c('0','1'), labels= c('no','yes'))
card2$alco <- factor(card2$alco,levels = c('0','1'), labels= c('no','yes'))
card2$active <- factor(card2$active,levels = c('0','1'), labels= c('no','yes'))
card2$Tension_class<-factor(card2$Tension_class,levels = c('1','2','3','4'), labels= c('Normal','Prehypertension','Stage 1 Hypertension','Stage 2 Hypertension'))
card2$bmi_status<-factor(card2$bmi_status,levels = c('1','2','3','4'), labels= c('Underweight','Normal','Overweight','Obese'))
head(card2)

#male female height weight comparison w.r.t. gender 
##### PLOT 6:F. three variables (one categorical and two continuous)
ggplot(card2,aes(x=height,weight)) + geom_point(aes(colour=gender),alpha=0.6,size=1) + ggtitle('Male Female mass comparison') +theme_classic()

#alcohol vs cardiovascular diseases
##### PLOT 7:E. two variables (both categorical)

df1<-card2%>%group_by(alco,cardio)%>%summarise(Count=n())
df2<-df1%>%group_by(alco)%>%summarise(tot=sum(Count))
df1<-merge(df1,df2,by="alco")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df1,aes(x=factor(alco),y=Proportion)) +
  geom_col(aes(fill=factor(cardio)),color="white",width = 0.5) +
  scale_fill_manual(name="Cardio",labels=c("No","Yes"),values = c("#004C99","#A70042")) +
  theme(title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
        axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Alcohol in Cardiovascular Diseases')+
  labs(x="Alcohol",y="Proportion")

#smoke vs cardiovascular diseases
##### PLOT 8:E. two variables (both categorical)

df3<-card2%>%group_by(smoke,cardio)%>%summarise(Count=n())
df4<-df3%>%group_by(smoke)%>%summarise(tot=sum(Count))
df3<-merge(df3,df4,by="smoke")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df3,aes(x=factor(smoke),y=Proportion)) +
  geom_col(aes(fill=factor(cardio)),color="white",width = 0.5) +
  scale_fill_manual(name="Cardio",labels=c("No","Yes"),values = c("#004C99","#A70042")) +
  theme(title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
        axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Smoking in Cardiovascular Diseases')+
  labs(x="Smoking",y="Proportion")

#active vs cardiovascular diseases
##### PLOT 9:E. two variables (both categorical)

df5<-card2%>%group_by(active,cardio)%>%summarise(Count=n())
df6<-df5%>%group_by(active)%>%summarise(tot=sum(Count))
df5<-merge(df5,df6,by="active")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df5,aes(x=factor(active),y=Proportion)) +
  geom_col(aes(fill=factor(cardio)),color="white",width=0.5) +
  scale_fill_manual(name="Cardio",labels=c("No","Yes"),values = c("#004C99","#A70042")) +
  theme(title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
        axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Activity in Cardiovascular Diseases')+
  labs(x="Activity",y="Proportion")

#BMI vs cardiovascular diseases
##### PLOT 10:D. two variables (one categorical and one continuous)

ggplot(card2, aes(BMI, after_stat(density), color = factor(cardio))) +
  geom_freqpoly(size=1,binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
  ggtitle('Effect of BMI in cardiovascular disease')+
  theme(title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
        panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  labs(x="BMI",y="Density")+
  scale_color_discrete(name="Cardio",labels=c("Absence","Presence"))

#tension_scale vs cardiovascular diseases
##### PLOT 11:E. two variables (both categorical) 

df7<-card2%>%group_by(Tension_class,cardio)%>%summarise(Count=n())
df8<-df7%>%group_by(Tension_class)%>%summarise(tot=sum(Count))
df7<-merge(df7,df8,by="Tension_class")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df7,aes(x=Tension_class,y=Proportion)) +
  geom_col(aes(fill=factor(cardio)),color="white",width = 0.5) +
  scale_fill_manual(name="Cardio",labels=c("No","Yes"),values = c("#004C99","#A70042")) +
  theme(title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
        axis.ticks.x = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Tension_class in Cardiovascular Diseases')+
  labs(x="Tension_class",y="Proportion")

#bmi vs tension_class
##### PLOT 12:D. two variables (one categorical and one continuous)
ggplot(card2,aes(BMI,fill=Tension_class)) + 
  geom_density(alpha=0.4) + 
  theme(title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
        title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
        panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  labs(title='Change in BMI density grouped by Tension class') 
  

#bmi vs tension_class
##### PLOT 13:F. three variables (two categorical and one continuous)
ggplot(card2,aes(BMI,fill=Tension_class))+
  geom_density(alpha=0.4)+
  facet_wrap(~cardio) + labs(title='Association between BMI and Tension class')+
  theme(title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
  panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
  legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
  legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))
  

#Age_group vs cardiovascular diseases
##### PLOT 14:E. two variables (both categorical)
ggplot(card2,aes(Age_group)) + 
  geom_bar(color='black',aes(fill=Age_group),show.legend = F,alpha=0.7, position = 'dodge') + 
  facet_wrap(~cardio) + ggtitle('Cardio Count in different age groups')+
  theme(title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
        panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"))

#age vs tension_class
##### PLOT 15:D. two variables (one discrete and one categorical)
ggplot(card2,aes(age,fill=Tension_class)) + 
  geom_density(alpha=0.4)+ labs(title='Association between age and Tension class')+
  theme(title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
        panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))


#Cholesterol count grouped by presence/absence of cardiovascular diseases
##### PLOT 16:F. three variables (all categorical)
ggplot(card2) + geom_bar(aes(x = factor(cholesterol),fill=as.factor(cardio)),position = 'dodge') + scale_fill_discrete(name='Cardio') +
  facet_wrap(~gender)+ 
  labs(title='Cholesterol grouped by presence/absence of cardiovascular diseases')+
  theme(title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
        panel.grid = element_blank(), plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  scale_x_discrete(breaks=c("1","2","3"),labels=c("1"='Low',"2"='Medium',"3"='High'))+
  labs(x="Cholesterol",y="Proportion")  
  



#glucose vs cardiovascular diseases
##### PLOT 17:E. two variables (both categorical)
df9<-card2%>%group_by(gluc,cardio)%>%summarise(Count=n())
df10<-df9%>%group_by(gluc)%>%summarise(tot=sum(Count))
df9<-merge(df9,df10,by="gluc")%>%mutate(Proportion=Count/tot)

#plotting the proportion

ggplot(df9,aes(x=factor(gluc),y=Proportion)) +
  geom_col(aes(fill=factor(cardio)),color="white",width = 0.5) +
  scale_fill_manual(name="Cardio",labels=c("No","Yes"),values = c("#004C99","#A70042")) +
  scale_x_discrete(breaks=c("1","2","3"),labels=c("1"='Low',"2"='Medium',"3"='High'))+
  theme(title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Glucose in Cardiovascular Diseases')+
  labs(x="Glucose",y="Proportion")


#bmi_status vs cardiovascular diseases
##### PLOT 18:E. two variables (both categorical)
df11<-card2%>%group_by(bmi_status,cardio)%>%summarise(Count=n())
df12<-df11%>%group_by(bmi_status)%>%summarise(tot=sum(Count))
df11<-merge(df11,df12,by="bmi_status")%>%mutate(Proportion=Count/tot)

ggplot(df11,aes(x=bmi_status,y=Proportion)) +
  geom_col(aes(fill=factor(cardio)),color="white",width = 0.5) +
  scale_fill_manual(name="Cardio",labels=c("No","Yes"),values = c("#004C99","#A70042")) +
  theme(title=element_text(size=16,face="bold"),axis.text=element_text(size=12,face="bold"),legend.text = element_text(size=10,face="bold"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#E3CDBF",color = "#E3CDBF"),
        legend.background = element_rect( fill = "#E3CDBF",color = "#E3CDBF"),
        legend.key = element_rect(fill = "#E3CDBF",color = "#E3CDBF"))+
  ggtitle('Effect of Obesity in Cardiovascular Diseases')+
  labs(x="bmi_status",y="Proportion")






