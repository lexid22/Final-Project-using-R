install.packages("NHANES")
install.packages("mdsr")
install.packages("ddplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("rpart")
install.packages('randomForest')
install.packages("nnet")
library(nnet)
library(randomForest)
library(rpart)
library(NHANES)
library(dplyr)
library(mdsr)
library(tidyr)
library(ggplot2)



#sleeptrouble
glimpse(NHANES$SleepTrouble)
nhanes1<-NHANES%>%select(Gender, Age, Race3, Education, HHIncome, BPDiaAve, BPSysAve, Depressed, SleepTrouble) %>% na.omit()
levels(nhanes1$SleepTrouble)<-c(0,1)
summary(nhanes1$SleepTrouble)

set.seed(123)
index = sort(sample(nrow(nhanes1), nrow(nhanes1) * 0.8))
train<-nhanes1[index,]
test<-nhanes1[index,]
train$SleepTrouble

formula<- as.formula("SleepTrouble ~Gender + Age + Race3 + Education + HHIncome + BPDiaAve + BPSysAve + Depressed" )
classifier_log<-glm(formula, data = train, family = 'binomial')
classifier_log

#dtree
classifier_tree<- rpart(formula, data = train)
classifier_tree


#randforest
classifier_forest<- randomForest(formula, data = train)
classifier_forest
sum(diag(classifier_forest$confusion)) / nrow(train)



#accuracy of null model
mod_null<-train%>%group_by(SleepTrouble)%>%summarize(N=n())%>%mutate(percent = N/sum(N))
mod_null

#acuracy of log_reg
prob_pred = predict(classifier_log, type = 'response', newdata = train[-9])
y_hat_train = ifelse(prob_pred >= 0.5, 1, 0)
cm_lr = table(train$SleepTrouble, y_hat_train)
cm_lr
sum(diag(cm_lr)) / nrow(train)


#accuracy of dtree
pred_dtree = predict(classifier_tree, newdata = train[-9], type = "class")
cm<-table(train$SleepTrouble, pred_dtree)
cm
sum(diag(cm)) / nrow(train)


#accuracy of randforest
sum(diag(classifier_forest$confusion)) / nrow(train)


#null mod visualization
ggplot(data = test, aes(x = Age, y = BPSysAve))+geom_point(aes(color = SleepTrouble))
+ggtitle("Null model (test set)") +theme(plot.title = element_text(hjust = 0.5))
 
#log mod visualization
formulaAB<- as.formula("SleepTrouble ~ Age + BPSysAve")
testAB = test%>%select(Age, BPSysAve, SleepTrouble)
trainAB<-train%>%select(Age, BPSysAve, SleepTrouble)
trainAB[-3]<-scale(trainAB[-3])
testAB[-3]<-scale(testAB[-3])
Age = seq(min(testAB[, 1])- 1, max(testAB[, 1]) + 1, by = 0.01)
BPSysAve = seq(min(testAB[, 2])- 1, max(testAB[, 2]) + 1, by = 0.01)
grid_set = expand.grid(Age, BPSysAve)
colnames(grid_set) = c('Age', 'BPSysAve') 
classifier_logAB<-glm(formulaAB, data = trainAB, family = 'binomial')
pred_logisticAB = predict(classifier_logAB, type = 'response', newdata = grid_set)
yhat_AB = if_else(pred_logisticAB >= 0.5, 1, 0)
plot(testAB[, 3],
     main = 'Logistic Regression (test set)',
     xlab = 'Age', ylab = 'BP Systolic',
     xlim = range(Age), ylim = range(BPSysAve))
contour(Age, BPSysAve, matrix(as.numeric(yhat_AB), length(Age), length(BPSysAve)), add = TRUE)
points(grid_set, pch = 15, col = ifelse(yhat_AB == 1, 'green', 'lightblue'))
points(testAB, pch = 16, bg = ifelse(testAB[, 3] == "Yes", 'blue', 'yellow') )


#dtree graph
formulaAB<- as.formula("SleepTrouble ~ Age + BPSysAve")
testAB = test%>%select(Age, BPSysAve, SleepTrouble)
trainAB<-train%>%select(Age, BPSysAve, SleepTrouble)
trainAB[-3]<-scale(trainAB[-3])
testAB[-3]<-scale(testAB[-3])
Age = seq(min(testAB[, 1])- 1, max(testAB[, 1]) + 1, by = 0.01)
BPSysAve = seq(min(testAB[, 2])- 1, max(testAB[, 2]) + 1, by = 0.01)
grid_set = expand.grid(Age, BPSysAve)
colnames(grid_set) = c('Age', 'BPSysAve')
classifier_tree<-rpart(formulaAB, data = trainAB)
pred_tree = predict(classifier_tree, newdata = grid_set, type = "class")
levels(pred_tree)<-c(0,1)
plot(testAB[, 3],
     main = 'Decision trees Regression (test set)',
     xlab = 'Age', ylab = 'BP Systolic',
     xlim = range(Age), ylim = range(BPSysAve))
contour(Age, BPSysAve, matrix(as.numeric(pred_tree), length(Age), length(BPSysAve)), add = TRUE)
points(grid_set, pch = 15, col = ifelse(pred_tree == 1, 'lightblue', 'orange'))
points(testAB, pch = 16, bg = ifelse(testAB[, 3] == "Yes", 'green', 'red'))


#randforest graph
formulaAB<- as.formula("SleepTrouble ~ Age + BPSysAve")
testAB = test%>%select(Age, BPSysAve, SleepTrouble)
trainAB<-train%>%select(Age, BPSysAve, SleepTrouble)
trainAB[-3]<-scale(trainAB[-3])
testAB[-3]<-scale(testAB[-3])
Age = seq(min(testAB[, 1])- 1, max(testAB[, 1]) + 1, by = 0.01)
BPSysAve = seq(min(testAB[, 2])- 1, max(testAB[, 2]) + 1, by = 0.01)
grid_set = expand.grid(Age, BPSysAve)
colnames(grid_set) = c('Age', 'BPSysAve')
classifier_forest-randomForest(formulaAB, data = trainAB, ntree = 500, mtry = 2)
pred_forest = predict(classifier_forest, newdata = grid_set, type = "class")
levels(pred_forest)<-c(0,1)
plot(testAB[, 3],
     main = 'Random Forest Classifier (test set)',
     xlab = 'Age', ylab = 'BP Systolic',
     xlim = range(Age), ylim = range(BPSysAve))
contour(Age, BPSysAve, matrix(as.numeric(pred_forest), length(Age), length(BPSysAve)), add = TRUE)
points(grid_set, pch = 15, col = ifelse(pred_tree == 1, 'lightblue', 'orange'))
points(testAB, pch = 16, bg = ifelse(testAB[, 3] == "Yes", 'green', 'red'))







