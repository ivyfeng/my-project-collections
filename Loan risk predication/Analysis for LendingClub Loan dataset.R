
traindata<-read.csv("loan_traindata.csv",header = TRUE)

names(traindata)=c("id","y","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10")

head(traindata)

traindata$y=as.factor(traindata$y)
##select<-sample(1:nrow(traindata),length(traindata$id)*0.1) 

select<-sample(1:nrow(traindata),length(traindata$id)*0.1)
train=traindata[select,]

testdata<-sample(1:nrow(traindata[-select,]),length(traindata$id)*0.1)
test=traindata[testdata,]



library(rpart)
tc<-rpart.control(minsplit = 50,minbucket = 20,maxdepth = 30,xval =10,cp = 0.001)

formular = y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10
##rpart.mod=rpart(formular,data = train,method = "class",parms = list(split="gini"),control = tc)

model.CART <- rpart(y~.,data = train)

results.CART<-predict(model.CART,newdata=test)





##rpart.mod=rpart(formular,data = train,parms = list(prior=c(0.6,0.4),loss=matrix(c(0,1,2,0),nrow=2),split="gini"),control = tc)



rpart.mod$cp
plotcp(rpart.mod)
rpart.mod.pru<-prune(rpart.mod,cp=0.001652621)
rpart.mod.pru$cp

rpart.plot(rpart.mod.pru,branch=1,extra = 102,under = TRUE,faclen = 0,cex = 0.7,main="CART")
plot(rpart.mod.pru)
text(rpart.mod.pru,all = TRUE,digits = 7,use.n = TRUE,cex=0.9,xpd=TRUE)


rpart.mod.pru$variable.importance

test.pre<-predict(rpart.mod.pru,test,type="class")
table(test.pre)
table(test.pre, test$y)
test$pre=0
test$pre[which(test$pre_p>0.431)]=1

##install.packages("caret")
library(caret)
confusionMatrix(test$pre,test$y,positive='1')


#####  second time for modeling 
traindata<-read.csv("loan_traindata.csv",header = TRUE)

