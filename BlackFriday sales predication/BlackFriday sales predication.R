library(tidyverse)
library(randomForest)
library(tree)
library(e1071)
library(arules)

#clean data
blackfriday <- read_csv(".../BlackFriday.csv")

head(blackfriday)

summary(blackfriday)


for (i in 1:18){
  blackfriday[,paste0(i)]=ifelse(blackfriday$Product_Category_1==i,1,
                                 ifelse(blackfriday$Product_Category_2 == i,1,
                                        ifelse(blackfriday$Product_Category_3 == i, 1, 0)))
}

blackfriday[is.na(blackfriday)] <- 0

names(blackfriday)[13:30] <- c("c1","c2","c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11",
                               "c12", "c13", "c14", "c15", "c16", "c17", "c18")

bf_c2p <- blackfriday
bf2 <- bf_c2p %>% group_by(User_ID,Gender,Age,Occupation,City_Category,Stay_In_Current_City_Years,Marital_Status) %>% summarise(Purchase = sum(Purchase))
bf2$Purchase = ifelse(bf2$Purchase < 234914,"basic",
                      ifelse(bf2$Purchase < 512612, "premium",
                             ifelse(bf2$Purchase < 1099005, "silver", "gold")))

bf2 <- as.data.frame(unclass(bf2))
bfa <- bf_c2p %>% group_by(User_ID,Gender,Age,Occupation,City_Category,Stay_In_Current_City_Years,Marital_Status) %>% summarise(c1=sum(c1),c2=sum(c2),c3=sum(c3),c4=sum(c4),c5=sum(c5),c6=sum(c6),c7=sum(c7),c8=sum(c8),c9=sum(c9),c10=sum(c10)
                                                                                                                                ,c11=sum(c11),c12=sum(c12),c13=sum(c13),c14=sum(c14),c15=sum(c15),c16=sum(c16),c17=sum(c17),c18=sum(c18))
bfc <- bfa
bfc$Purchase <- bf2$Purchase
bff <- as.data.frame(unclass(bfc))

#predicting purchase level
set.seed(1)
train <- sample(1:nrow(bf2), 4/5*nrow(bf2))
#random forest
#find ntree
ntree_to_test <- seq(from = 500, to = 3000, by = 100)
result_rf <- rep(NA, length(ntree_to_test))
for (i in 1:length(ntree_to_test)){
  rf.bff <- randomForest(Purchase ~ .-User_ID,data = bff, subset = train, 
                           ntree = ntree_to_test[i])
  pr.bff <- predict(rf.bff, newdata = bff[-train,] )
  result <- mean(pr.bff == bff[-train,]$Purchase)
  result_rf[i] <- result
}
#using consumer behavior
rf <-randomForest(Purchase ~ .-User_ID,data = bff, subset = train, 
                  ntree = ntree_to_test[which.max(result_rf)],importance = TRUE)
imp<-importance(rf,type=1)
varImpPlot(rf,type=1)
pr.rf <- predict(rf, newdata = bff[-train,])
mean(pr.rf == bff[-train,]$Purchase)
table(pr.rf,bff[-train,]$Purchase,dnn = list('predict', 'actual'))
#using only basic information
rf2 <-randomForest(Purchase ~ Age+Gender+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status,
                   data = bff, subset = train)
pr.rf2 <- predict(rf2, newdata = bff[-train,])
table(pr.rf2, bff[-train,]$Purchase, dnn = list('predict', 'actual'))
mean(pr.rf2 == bff[-train,]$Purchase)


#Decision tree
#using consumer behavior factors
tree.bff <- tree(Purchase~.-User_ID, data=bff, subset = train)
pr.tree<-predict(tree.bff, newdata = bff[-train,],type = "class")
table(pr.tree, bff[-train,]$Purchase)
mean(pr.tree==bff[-train,]$Purchase)
cv.t <- cv.tree(tree.bff,FUN=prune.misclass)
cv.t
plot(cv.t$size, cv.t$dev, type = "b")
prune.t <- prune.misclass(tree.bff,best = 10)
pr.prune <- predict(prune.t, newdata = bff[-train,],type = "class")
mean(pr.prune == bff[-train,]$Purchase)
table(pr.tree, bff[-train,]$Purchase,dnn = list('predict', 'actual'))
#using only basic information
tree.bff2 <- tree(Purchase~Age+Gender+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status, data=bff, subset = train)
pr.tree2<-predict(tree.bff2, newdata = bff[-train,],type = "class")
table(pr.tree2, bff[-train,]$Purchase,dnn = list('predict', 'actual'))
mean(pr.tree2==bff[-train,]$Purchase)
#SVM
#using consumer behavior
svm.bff <- svm(Purchase~.-User_ID, data=bff, subset = train,kernel ="linear", cost =0.1,scale =FALSE)
pr.svm.bff <- predict(svm.bff, newdata =bff[-train,] )
table(pr.svm.bff,bff[-train,]$Purchase )
mean(pr.svm.bff == bff[-train,]$Purchase)

svm_tune <- tune(svm, Purchase ~ .-User_ID ,data = bff[train,],kernel ="linear",
                 ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
summary(svm_tune)
best_svm <- svm_tune$best.model

pr.best.svm <- predict(best_svm, newdata = bff[-train,])
mean(pr.best.svm == bff[-train,]$Purchase)
table(pr.best.svm,bff[-train,]$Purchase,dnn = list('predict', 'actual'))
#using only basic information
svm.bff2 <- svm(Purchase~Age+Gender+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status,
                data=bff, subset = train,kernel ="linear", cost =0.1,scale =FALSE)
pr.svm.bff2 <- predict(svm.bff2, newdata = bff[-train,])
table(pr.svm.bff2,bff[-train,]$Purchase,dnn = list('predict', 'actual') )
mean(pr.svm.bff2 == bff[-train,]$Purchase)

#predicting purchase intention
#data processing
bff7 <- bff
bff7$c7 <- ifelse(bff7$c7 == 0,0,1)
bff7$c7 <- as.factor(bff7$c7)

bff10 <- bff
bff10$c10 <- ifelse(bff10$c10 == 0,0,1)
bff10$c10 <- as.factor(bff10$c10)

bff12 <- bff
bff12$c12 <- ifelse(bff12$c12 == 0,0,1)
bff12$c12 <- as.factor(bff12$c12)
#using consumer behavior
bag.c12 <- randomForest(c12 ~ .-User_ID-c12, 
                       data = bff12, subset = train,importance=TRUE)
bag.c7 <- randomForest(c7 ~ .-User_ID-c7, 
                        data = bff7, subset = train,importance=TRUE)
bag.c10 <- randomForest(c10 ~ .-User_ID-c10, 
                        data = bff10, subset = train,importance=TRUE)

importance(bag.c12,type = 1)
varImpPlot(bag.c12,type = 1, main = "c12")
varImpPlot(bag.c7,type = 1,main = "c7")
varImpPlot(bag.c10,type = 1,main = "c10")

pr.bagc12 <- predict(bag.c12, newdata = bff12[-train,])
table(pr.bagc12, bff12[-train, ]$c12,dnn = list('predict', 'actual'))
mean(pr.bagc12 == bff12[-train,]$c12)

pr.bagc10 <- predict(bag.c10, newdata = bff10[-train,])
table(pr.bagc10, bff10[-train, ]$c10,dnn = list('predict', 'actual'))
mean(pr.bagc10 == bff10[-train,]$c10)

pr.bagc7 <- predict(bag.c7, newdata = bff[-train,])
table(pr.bagc7, bff7[-train, ]$c7,dnn = list('predict', 'actual'))
mean(pr.bagc7 == bff7[-train,]$c7)
#using only basic information
bag.c122 <- randomForest(c12 ~ Age+Gender+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status, 
                        data = bff12, subset = train,importance=TRUE)
bag.c72 <- randomForest(c7 ~ Age+Gender+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status, 
                       data = bff7, subset = train,importance=TRUE)
bag.c102 <- randomForest(c10 ~ Age+Gender+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status, 
                        data = bff10, subset = train,importance=TRUE)

pr.bagc122 <- predict(bag.c122, newdata = bff12[-train,])
table(pr.bagc122, bff12[-train, ]$c12,dnn = list('predict', 'actual'))
mean(pr.bagc122 == bff12[-train,]$c12)

pr.bagc102 <- predict(bag.c102, newdata = bff10[-train,])
table(pr.bagc102, bff10[-train, ]$c10,dnn = list('predict', 'actual'))
mean(pr.bagc102 == bff10[-train,]$c10)

pr.bagc72 <- predict(bag.c72, newdata = bff[-train,])
table(pr.bagc72, bff7[-train, ]$c7,dnn = list('predict', 'actual'))
mean(pr.bagc72 == bff7[-train,]$c7)