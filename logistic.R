bank <- read.csv("bank-full.csv",sep = ";")
View(bank)
colnames(bank)
head(bank)
str(bank)
library(dplyr)
###charactertofactor#
bank[ sapply(bank, is.character)] <- lapply(bank[sapply(bank, is.character)],as.factor)

bankmodel<-glm(y~., data = bank,family = binomial)
summary(bankmodel)

##confusion matrix
prob <- predict(bankmodel,type=c("response"),bank)
View(prob)
confusion<-table(prob>0.5,bank$y)
confusion

confusion# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy#90.18
1-Accuracy #Error Rate
sum(confusion[cbind(2:1, 1:2)])/sum(confusion)#Error Rate


# ROC Curve 
#install.packages("ROCR")
#install.packages("ROCR", dependencies = T)
library(ROCR)
rocrpred<-prediction(prob,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
#plot(rocrperf,colorize=T,text.adj=c(0.5,NA))
plot(rocrperf,colorize=T,text.adj = c(0.5, 0.5))
