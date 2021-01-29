bank <- read.csv("bank-full.csv",sep = ";")
View(bank)
colnames(bank)
head(bank)
str(bank)
library(dplyr)
###charactertofactor#
bank[ sapply(bank, is.character)] <- lapply(bank[sapply(bank, is.character)],as.factor)

bank <- as.data.frame(bank)
class(bank)

model<-lm(y~., data = bank)
pred <- predict(model,bank)
plot(age,pred)
plot(pred)

model1<-glm(y~., data = bank, family = "binomial")
exp(coef(model1))

prob <- predict(model1,bank,type = "response")
summary(model1)

confusion <- table(prob>0.5,bank$y)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
1-Accuracy #Error Rate
sum(confusion[cbind(2:1, 1:2)])/sum(confusion)#Error Rate

pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")
bank[,"prob"] <- prob
bank[,"pred_values"] <- pred_values
bank[,"yes_no"] <- yes_no

View(bank)

table(bank$y,bank$pred_values)

# ROC Curve 
install.packages("ROCR")
library(ROCR)

rocrpred<-prediction(prob,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
str(rocrperf)
#plot(rocrperf,colorize=T,text.adj=c(0.5,NA))
plot(rocrperf,colorize=T,text.adj = c(0.5, 0.5))
