### Revenue analysis on Google store dataset
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggcorrplot)
library(ggplot2)
library(MASS)
data<-read_csv("/data.csv")
data.cols<-c(1:5,7:10,13,15,25:31,33,37:43,46:56)
data.sub<-subset(data[data.cols])
write.csv(data.sub,file="data.sub.csv")
data.sub$transactionRevenue<-replace(data.sub$transactionRevenue,is.na(data.sub$transactionRevenue),0)
data.sub$date<-ymd(data.sub$date)
data.sub<-separate(data.sub,date,into=c("year", "month", "day"), sep="-")

#Transaction
b.data<-data.sub[which(data.sub$transactionRevenue!=0),]
t1<-ggplot(b.data,aes(x=transactionRevenue))+geom_histogram(bins =50,colour="black",fill="grey")+ggtitle("TransactionRevenue(excluding 0)")+theme(plot.title=element_text(hjust=0.5))
t2<-ggplot(data.sub,aes(x=transactionRevenue))+geom_histogram(bins =50,colour="black",fill="grey")+ggtitle("TransactionRevenue(including 0)")+theme(plot.title=element_text(hjust=0.5))
grid.arrange(t2, t1, ncol=2)
#Month
m1<-ggplot(b.data,aes(x=month))+geom_bar(colour="black",fill="grey")+labs(title="Monthly TransactionRevenue(excluding 0)")+theme(plot.title=element_text(hjust=0.5))
m2<-ggplot(data.sub,aes(x=month))+geom_bar(colour="black",fill="grey")+labs(title="Monthly TransactionRevenue(including 0)")+theme(plot.title=element_text(hjust=0.5))
grid.arrange(m2, m1, ncol=2)
m.sum<-data.frame(aggregate(b.data["transactionRevenue"], by=b.data["month"], sum))
ggplot(data=m.sum,aes(x=month,y=transactionRevenue))+geom_bar(colour="black",fill="grey",stat="identity")+ggtitle("Monthly TransactionRevenue")+geom_hline(aes(yintercept=mean(transactionRevenue)),colour='red')+theme(plot.title=element_text(hjust=0.5))

#Day
d1<-ggplot(b.data,aes(x=day))+geom_bar(colour="black",fill="grey")+labs(title="Daily TransactionRevenue(excluding 0)")+theme(plot.title=element_text(hjust=0.5))
d2<-ggplot(data.sub,aes(x=day))+geom_bar(colour="black",fill="grey")+labs(title="Daily TransactionRevenue(including 0)")+theme(plot.title=element_text(hjust=0.5))
grid.arrange(d2, d1, ncol=2)
d.sum<-data.frame(aggregate(b.data["transactionRevenue"], by=b.data["day"], sum))
ggplot(data=d.sum,aes(x=day,y=transactionRevenue))+geom_bar(colour="black",fill="grey",stat="identity")+ggtitle("Daily TransactionRevenue")+geom_hline(aes(yintercept=mean(transactionRevenue)),colour='red')+theme(plot.title=element_text(hjust=0.5))

#Channel
c1<-ggplot(b.data,aes(x=channelGrouping))+geom_bar(colour="black",fill="grey")+labs(title="Channel TransactionRevenue(excluding 0)")+theme(plot.title=element_text(hjust=0.5))+theme(axis.text.x=element_text(angle=90,hjust=1))
c2<-ggplot(data.sub,aes(x=channelGrouping))+geom_bar(colour="black",fill="grey")+labs(title="Channel TransactionRevenue(including 0)")+theme(plot.title=element_text(hjust=0.5))+theme(axis.text.x=element_text(angle=90,hjust=1))
grid.arrange(c2, c1, ncol=2)
d.sum<-data.frame(aggregate(b.data["transactionRevenue"], by=b.data["channelGrouping"], sum))
ggplot(data=d.sum,aes(x=channelGrouping,y=transactionRevenue))+geom_bar(colour="black",fill="grey",stat="identity")+ggtitle("Channel TransactionRevenue")+geom_hline(aes(yintercept=mean(transactionRevenue)),colour='red')+theme(plot.title=element_text(hjust=0.5))
c.data<-data.sub
c.data$transactionRevenue<-if_else(c.data$transactionRevenue!=0,1,0)
ggplot(c.data)+aes(x=channelGrouping,fill=factor(transactionRevenue))+geom_bar(position="fill")+labs(title="Relative Proportions for Channel")+theme(plot.title=element_text(hjust=0.5))+theme(axis.text.x=element_text(angle=90,hjust=1))

#System
s1<-ggplot(b.data,aes(x=operatingSystem))+geom_bar(colour="black",fill="grey")+labs(title="Device TransactionRevenue(excluding 0)")+theme(plot.title=element_text(hjust=0.5))+theme(axis.text.x=element_text(angle=90,hjust=1))
s2<-ggplot(data.sub,aes(x=operatingSystem))+geom_bar(colour="black",fill="grey")+labs(title="Device TransactionRevenue(including 0)")+theme(plot.title=element_text(hjust=0.5))+theme(axis.text.x=element_text(angle=90,hjust=1))
grid.arrange(s2, s1, ncol=2)
d.sum<-data.frame(aggregate(b.data["transactionRevenue"], by=b.data["operatingSystem"], sum))
ggplot(data=d.sum,aes(x=operatingSystem,y=transactionRevenue))+geom_bar(colour="black",fill="grey",stat="identity")+ggtitle("Device TransactionRevenue")+geom_hline(aes(yintercept=mean(transactionRevenue)),colour='red')+theme(plot.title=element_text(hjust=0.5))
ggplot(c.data)+aes(x=operatingSystem,fill=factor(transactionRevenue))+geom_bar(position="fill")+labs(title="Relative Proportions for Device")+theme(plot.title=element_text(hjust=0.5))+theme(axis.text.x=element_text(angle=90,hjust=1))

#Continent
ggplot(c.data)+aes(x=continent,fill=factor(transactionRevenue))+geom_bar(position="fill")+labs(title="Relative Proportions for continent")+theme(plot.title=element_text(hjust=0.5))

#ANOVA
mod1<-aov(transactionRevenue~month,data=data.sub)
summary(mod1)
mod2<-aov(transactionRevenue~day,data=data.sub)
summary(mod2)

#Logistic
n.data<-sample_frac(data.sub[which(data.sub$fullVisitorId!=b.data$fullVisitorId),],0.015)
l.data<-rbind(n.data,b.data)
l.data$month<-as.numeric(l.data$month)
l.data$day<-as.numeric(l.data$day)
l.data$channelGrouping<-as.numeric(as.factor(l.data$channelGrouping))
l.data$browser<-as.numeric(as.factor(l.data$browser))
l.data$operatingSystem<-as.numeric(as.factor(l.data$operatingSystem))
l.data$isMobile<-as.numeric(as.factor(l.data$isMobile))
l.data$deviceCategory<-as.numeric(as.factor(l.data$deviceCategory))
l.data$continent<-as.numeric(as.factor(l.data$continent))
l.data$subContinent<-as.numeric(as.factor(l.data$subContinent))
l.data$medium<-as.numeric(as.factor(l.data$medium))
lg.cols<-c(2,4:5,9,11:16,24,35:36,39)
lg.data<-subset(l.data[lg.cols])
lg.data$transactionRevenue<-if_else(lg.data$transactionRevenue!=0,1,0)
corr<-round(cor(lg.data),2)
ggcorrplot(corr,hc.order=TRUE,outline.col="white",colors = c("#6D9EC1", "white", "#E46726"),type="lower",lab=TRUE)

fit=glm(transactionRevenue~.,data=lg.data,family="binomial")
summary(fit)
sm<-summary(fit)
co<-sm$coefficients[,"Estimate"]
se<-sm$coefficients[,"Std. Error"]
ci <- confint.default(fit)

cbind(co, se, ci)
anova(fit, test="Chisq")
step<-stepAIC(fit,direction=c("both"))
summary(step)

