library(ggplot2)
library(gtools)
library(caTools)

d<- read.csv('KaggleV2-May-2016.csv')
head(d)
str(d)
d[d$Scholarship == 0,]$Scholarship <- "No"
d[d$Scholarship == 1,]$Scholarship <- "Yes"
d$Scholarship <- as.factor(d$Scholarship)

d[d$Hipertension == 0,]$Hipertension <- "No"
d[d$Hipertension == 1,]$Hipertension <- "Yes"
d$Hipertension <- as.factor(d$Hipertension)

d[d$Diabetes == 0,]$Diabetes <- "No"
d[d$Diabetes == 1,]$Diabetes <- "Yes"
d$Diabetes <- as.factor(d$Diabetes)

d[d$Alcoholism == 0,]$Alcoholism <- "No"
d[d$Alcoholism == 1,]$Alcoholism <- "Yes"
d$Alcoholism <- as.factor(d$Alcoholism)

d[d$SMS_received == 0,]$SMS_received <- "No"
d[d$SMS_received == 1,]$SMS_received <- "Yes"
d$SMS_received <- as.factor(d$SMS_received)

summary(d)

#AGE VS. NO SHOW
#remove negative age value
d <-d [!(d$Age<0),]
d <-d [!(d$Age>100),]
summary(d$Age)
summary(d)
ggplot(d,aes(x=Age)) + 
        geom_histogram(data=subset(d,No.show == 'No'),fill = '#00BFC4', alpha = 0.8, bins = 40) +
        geom_histogram(data=subset(d,No.show == 'Yes'),fill = '#F8766D', alpha = 0.8, bins = 40) +
        ggtitle('Age vs. No Show Histogram')+
        theme_bw()
ggplot(d, aes(x = No.show, y = Age, fill = No.show))+
        geom_boxplot()+ 
        ggtitle("Age vs. No Show Boxplot")
#From the box plot, the mean age is higher for those who showed up.

d$Age<- quantcut(d$Age)
prop.table(table(d$Age, d$No.show),1)
ggplot(d, aes(x=Age, y = No.show, fill= No.show))+
        geom_bar(stat="identity")+
        ggtitle("Age split vs. No show Bar Diagram")
#From the table and the bar diagram, we can conclude that being older = more likely to show up to appointments, apparently.

t.test(d$Age ~ d$No.show)
#So there is significant difference in age of patient those who showed up and those who did not show up.

#GENDER VS. NO SHOW
# replace levels
levels(d$Gender)[levels(d$Gender)=="M"] <- "Male"
levels(d$Gender)[levels(d$Gender)=="F"] <- "Female"
# count gender
ggplot(d, aes(x = Gender,fill = Gender))+
        geom_bar()+
        ggtitle("Gender Bar Diagram")
#there are more females included in the dataset. 

ggplot(d)+
        geom_bar(aes(x = Gender, fill = No.show))+
        ggtitle("Gender VS.No Show Basic Bar Diagram")
        
prop.table(table(d$Gender, d$No.show), margin = 1)
#there isn't a big difference between the female and male related to the no show frequency. 
ggplot(d)+geom_bar(aes(Gender, fill = No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("Gender VS. No show Histogram") +
        theme_bw()

chisq.test(table(d$Gender,d$No.show))
# since the p-value is more than 0.05, so the gender difference is not significant.

#AGE AND GENDER VS. NO SHOW

ggplot(d, aes(Age, Gender, fill = No.show), position = fill)+
      geom_bar(stat="identity")+
      facet_wrap(~Gender)+
        ggtitle("Age Split and Gender VS. No show") +
      theme_bw()

#this means that young males are less likely to miss the appointments than young females, howvere, male adults are less likely to show-up than female adults. 
#When we analysis gender individully, the gender differences are trivial, but when we analyze gender with age, they do have influence in the show-up rate. 


# Scholarship VS. No show
ggplot(d, aes(Scholarship, fill= No.show))+
        geom_bar()
ggplot(d)+geom_bar(aes(Scholarship, fill = No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("Scholarship VS. No show") +
        theme_bw()
chisq.test(table(d$Scholarship,d$No.show))
# since the p-value is less than 0.05, so the scholarship difference is significant.
# no-scholarship means not low income, and scholarshi means low income.
# low income are more likely to miss the appointment 

#Hipertension VS. No show
ggplot(d)+geom_bar(aes(Hipertension, fill =No.show))
ggplot(d)+geom_bar(aes(Hipertension, fill = No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("Hipertension VS. No show") +
        theme_bw()
chisq.test(table(d$Hipertension,d$No.show))
# since the p-value is less than 0.05, so the hypertension difference is significant.


#Diabetes VS. No show
ggplot(d)+geom_bar(aes(Diabetes, fill =No.show))
ggplot(d)+geom_bar(aes(Diabetes, fill = No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("Diabetes VS. No show") +
        theme_bw()
chisq.test(table(d$Diabetes,d$No.show))
# since the p-value is less than 0.05, so the diabetes difference is significant.

#Alcoholism VS. No show
ggplot(d)+geom_bar(aes(Alcoholism, fill =No.show))
ggplot(d)+geom_bar(aes(Alcoholism, fill = No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("Alcoholism VS. No show") +
        theme_bw()
chisq.test(table(d$Alcoholism,d$No.show))
# since the p-value is more than 0.05, so the alcoholism difference is not significant.

#Handicap VS. No show
ggplot(d)+geom_bar(aes(Handcap, fill =No.show))
ggplot(d)+geom_bar(aes(Handcap, fill = No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("Handcap VS. No show") +
        theme_bw()
chisq.test(table(d$Handcap,d$No.show))
# since the p-value is more than 0.05, so the handicap difference is not significant.

#Sms_received VS. No show
ggplot(d)+geom_bar(aes(SMS_received, fill =No.show))
ggplot(d)+geom_bar(aes(SMS_received, fill =No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("SMS_receivedVS. No show") +
        theme_bw()
chisq.test(table(d$SMS_received,d$No.show))
# since the p-value is less than 0.05, so the sms_received difference is significant.


#IN CONCLUSION, p value is significant for scholarship, hypertension, diabetes, sms_received group.

d$day <- weekdays(as.Date(d$AppointmentDay))
ggplot(d)+geom_bar(aes(day, fill = day))+
        ggtitle("Number of Appointment")+
        ylab('Count')+
        xlab('Day')+
        theme(plot.title = element_text(hjust = 0.5))
#Total number of appointments made on Saturday is the lowest one, and Tuesdays and Wednesday are the highest two. 

ggplot(d)+geom_bar(aes(day, fill = No.show), position = position_fill())+
        ggtitle("Appointment vs No Show")+
        ylab('Proportion')+
        xlab('Day')+
        theme(plot.title = element_text(hjust = 0.5))
#Saturday's no show proportion is lightly higher than other days. (Might be the result of the smallest amount of appointments)
chisq.test(table(d$No.show,d$day))
#Since p value is significant that means showing up in appointment day is dependent on which day the appointment is.


#prediction 
#logit regression 

split <- sample.split(d, SplitRatio = 0.8)
split
train<- subset(d,split=="TRUE")
test<- subset(d, split== "FALSE")


mylogit<-glm(No.show ~ Age + Gender + Scholarship + Hipertension + Diabetes + Alcoholism + Handcap + SMS_received + day + No.show, data = train, family = "binomial")
summary(mylogit)

res<- predict(mylogit, test, type= "response")

res<- predict(mylogit,train, type = "response")

ll.null<- mylogit$null.deviance/-2
ll.proposed<- mylogit$deviance/-2
(ll.null- ll.proposed)/ ll.null
#this is the overall effective size
1- pchisq(2*(ll.proposed- ll.null), df=(length(mylogit$coefficients)-1))



