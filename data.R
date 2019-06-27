rm(list=ls(all=TRUE))
setwd("D:/AI/ML/logisticlab/Data.csv")
data=read.csv("data.csv")
str(data)
summary(data)
table(data$Target)
table(data$FD_ind1)
table(data$FD_ind3)
table(data$FD_ind5)
table(data$NumberOfDependents)
data$Target=as.factor(data$Target)
data$FD_ind1=as.factor(data$FD_ind1)
data$FD_ind3=as.factor(data$FD_ind3)
data$FD_ind5=as.factor(data$FD_ind5)
data$NumberOfDependents=as.factor(data$NumberOfDependents)
sum(is.na(data))
data=na.omit(data)
install.packages("caret")
library(caret)
trainindex=sample(x=1:nrow(data),size=0.70*nrow(data))
train_data=data[trainindex,]
test_data=data[-trainindex,]
model1 <- glm(Target ~ ., data = train_data, family = "binomial")
summary(model1)
library(MASS)
step3=stepAIC(model1,direction="both")
model2=glm(formula=Target ~ age + FD_ind1 + DebtRatio + MonthlyIncome + FD_ind2 + 
             FD_ind3 + FD_ind4 + FD_ind5 + NumberOfDependents, data=train_data,family="binomial")
summary(model2)
library(car)
vif(model2)

prob_train <- predict(model2, type = "response")
library(ROCR)
pred <- prediction(prob_train, train_data$Target)
perf <- performance(pred, measure="tpr", x.measure="fpr")
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)
cutoffs <- data.frame(cut= perf@alpha.values[[1]], fpr= perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
plot(perf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

pred_class <- ifelse(prob_train> 0.1, "yes", "no")
table(train_data$Target,pred_class)
model3=glm(formula=Target ~ age + FD_ind1 + DebtRatio + MonthlyIncome + FD_ind2 + 
             FD_ind3 + FD_ind4 + FD_ind5 + NumberOfDependents, data=test_data,family="binomial")
prob_test <- predict(model3, test_data, type = "response")

preds_test <- ifelse(prob_test > 0.1, 1, 0)
test_data_labs <- test_data$Target

preds_test=as.factor(preds_test)
confusionMatrix(preds_test, test_data$Target)
str(preds_test)
str(test_data$Target)
confusionMatrix(preds_test, test_data$Target)
