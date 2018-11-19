#Data import and Management 
#Loading all appropriate packages required for running code
library(ggplot2)
library(gtools)
library(caTools)
library(dplyr)

#Data processing
##Reading the dataset 
d<- read.csv('KaggleV2-May-2016.csv')
head(d)
str(d)

##Treating variables types: change all the binary variables to “factor” type
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

##Data cleaning: remove vaules that are negative and greater than 100
d <-d [!(d$Age<0),]
d <-d [!(d$Age>100),]
summary(d$Age)
summary(d)

#Explortory Analysis
#AGE VS. NO SHOW
##To figure out the relationship between different age group and the absence of the appointment by plotting and statistics 

ggplot(d,aes(x=Age)) + 
        geom_histogram(data=subset(d,No.show == 'No'),fill = '#00BFC4', alpha = 0.8, bins = 40) +
        geom_histogram(data=subset(d,No.show == 'Yes'),fill = '#F8766D', alpha = 0.8, bins = 40) +
        ggtitle('Age vs. No Show Histogram')+
        theme_bw()
## It is hard to tell from this plot the specific relationship between the age differences and the no-show rate, therefore, boxplot is drawn next

ggplot(d, aes(x = No.show, y = Age, fill = No.show))+
        geom_boxplot()+ 
        ggtitle("Age vs. No Show Boxplot")
##From the box plot, the mean age is higher for those who showed up.Next, Split the age to smaller groups 

##Independent two sample t-test
t.test(d$Age ~ d$No.show)
#Since the p-value< 0.05, therefore, there is significant difference in age of patient those who showed up and those who did not show up. Next, Split the age to smaller groups 

d$Age<- quantcut(d$Age)
prop.table(table(d$Age, d$No.show),1)
ggplot(d, aes(x=Age, y = No.show, fill= No.show))+
        geom_bar(stat="identity")+
        ggtitle("Age split vs. No show Bar Diagram")
#From the table and the bar diagram, it can be concluded that being older = more likely to show up to appointments, apparently.



#GENDER VS. NO SHOW
##To figure out the relationship between different gender and the absence of the appointment by plotting and statistics 

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
##Using bar diagram to compare female and male in different age groups’ no-show rate 

gender <- rep(c("Male","Female"), each=8)
age <- rep(c("[0,18]", "(18,37]", "(37,55]", "(55,100]"), times=4)
no.show <- rep(rep(c("Yes","No"), each=4), times=2)
freq <- c()
for (i in 1:16){
  freq <- c(freq, length(d$AppointmentID[(d$Gender==gender[i])&(d$Age==age[i])&d$No.show==no.show[i]]))
}

df <- data.frame(gender=gender,age=age,no.show=no.show,freq=freq)


pseh <- mutate(group_by(df, gender, age), Prop = freq / sum(freq))
ggplot(pseh) +
  geom_col(aes(age, Prop, fill = no.show)) +
  facet_wrap(~gender, nrow = 1)

test <- mutate(group_by(df, gender, age), Prop = freq / sum(freq))
ggplot(test) +
  geom_col(aes(age, Prop, fill = no.show)) +
  facet_wrap(~gender, nrow = 1)+
  ggtitle("Gender and Age Split  VS. No show Histogram") 

#Graphs indicate that the difference in no-show is not much in different age group for male and female
#When we analysis age individually, the age differences are significant, but when we analyze gender with age, they do not have influence in the show-up rate. 


# Scholarship VS. No show
##To figure out the relationship between rich/poor people and the absence of the appointment by plotting and statistics 

ggplot(d, aes(Scholarship, fill= No.show))+
        geom_bar()
ggplot(d)+geom_bar(aes(Scholarship, fill = No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("Scholarship VS. No show") +
        theme_bw()
chisq.test(table(d$Scholarship,d$No.show))
# since the p-value is less than 0.05, so the scholarship difference is significant.
# no-scholarship means not low income, and scholarship means low income.
# low income is more likely to miss the appointment 

#Hypertension VS. No show
##To figure out the relationship between people with/without hyperension and the absence of the appointment by plotting and statistics 

ggplot(d)+geom_bar(aes(Hipertension, fill =No.show))
ggplot(d)+geom_bar(aes(Hipertension, fill = No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("Hipertension VS. No show") +
        theme_bw()
chisq.test(table(d$Hipertension,d$No.show))
# since the p-value is less than 0.05, so the hypertension difference is significant.


#Diabetes VS. NO show
##To figure out the relationship between people with/without diabetes and the absence of the appointment by plotting and statistics 

ggplot(d)+geom_bar(aes(Diabetes, fill =No.show))
ggplot(d)+geom_bar(aes(Diabetes, fill = No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("Diabetes VS. No show") +
        theme_bw()
chisq.test(table(d$Diabetes,d$No.show))
# since the p-value is less than 0.05, so the diabetes difference is significant.

#Alcoholism VS. No show
## To figure out the relationship between people with/without alcoholism and the absence of the appointment by plotting and statistics

ggplot(d)+geom_bar(aes(Alcoholism, fill =No.show))
ggplot(d)+geom_bar(aes(Alcoholism, fill = No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("Alcoholism VS. No show") +
        theme_bw()
chisq.test(table(d$Alcoholism,d$No.show))
# since the p-value is more than 0.05, so the alcoholism difference is not significant.

#Handicap VS. No show
## To figure out the relationship between people with/without disabilities and the absence of the appointment by plotting and statistics

ggplot(d)+geom_bar(aes(Handcap, fill =No.show))
ggplot(d)+geom_bar(aes(Handcap, fill = No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("Handcap VS. No show") +
        theme_bw()
chisq.test(table(d$Handcap,d$No.show))
# since the p-value is more than 0.05, so the handicap difference is not significant.

#Sms_received VS. No show
## To figure out the relationship between people receive the message reminder or not and the absence of the appointment by plotting and statistics

ggplot(d)+geom_bar(aes(SMS_received, fill =No.show))
ggplot(d)+geom_bar(aes(SMS_received, fill =No.show), position = position_fill())+
        ylab('Proportion')+
        ggtitle("SMS_receivedVS. No show") +
        theme_bw()
chisq.test(table(d$SMS_received,d$No.show))
# since the p-value is less than 0.05, so the sms_received difference is significant.


#IN CONCLUSION, p value is significant for scholarship, hypertension, diabetes, sms_received group.


#Appointment Day vs No-show
## To figure out the relationship between different weekday and the absence of the appointment by plotting and statistics

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
#logit regression: since the no-show variable is a binary variable

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



