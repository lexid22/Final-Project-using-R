install.packages("NHANES")
install.packages("mdsr")
install.packages("ddplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("rpart")
install.packages("randomForest")
library(NHANES)
library(dplyr)
library(mdsr)
library(tidyr)
library(ggplot2)
library(rpart)
library(randomForest)




glimpse(NHANES$SleepTrouble)
nhanes1<-NHANES%>%select(Gender, Age, Education, HHIncome, BPDiaAve, BPSysAve, Depressed, SleepTrouble) %>% na.omit()
set.seed(123)
index = sort(sample(nrow(nhanes1), nrow(nhanes1) * 0.75))
train<-nhanes1[index,]
test<-nhanes1[index,]
glimpse(test)
glimpse(train)


#null mod accuracy
mod_null<-train%>%group_by(SleepTrouble)%>%summarize(N=n())%>%mutate(percent = N/sum(N))
mod_null
mod_null_test<-train%>%group_by(SleepTrouble)%>%summarize(N=n())%>%mutate(percent = N/sum(N))
mod_null_test

#null mod graph
ggplot(data = test, aes(x = Age, y = BPSysAve))+geom_point(aes(color = SleepTrouble))+ggtitle("Scatterplot of age and BPSysAve (test)")+theme(plot.title = element_text(hjust = 0.5))


#logistic reg
formula<- as.formula("SleepTrouble ~Gender + Age + Education + HHIncome + BPDiaAve + BPSysAve + Depressed" )
classifier_log_train<-glm(formula, data = train, family = 'binomial')
classifier_log_train
classifier_log_test<-glm(formula, data = test, family = 'binomial')
classifier_log_test

#log reg accuracy
prob_pred_train = predict(classifier_log_train, type = 'response', newdata = train[-7])
y_hat_train = ifelse(prob_pred_train >= 0.5, 1, 0)
cm_lr = table(train$SleepTrouble, data = y_hat_train)
cm_lr
sum(diag(cm_lr)) / nrow(train)
prob_pred_test = predict(classifier_log_test, type = 'response', newdata = test[-7])
y_hat_test = ifelse(prob_pred_test >= 0.5, 1, 0)
cm_lr = table(test$SleepTrouble, y_hat_test)
cm_lr
sum(diag(cm_lr)) / nrow(test)

formula<-as.formula("SleepTrouble~Age+BPSysAve")
testAB = test%>%select(Age, BPSysAve, SleepTrouble)
trainAB<-train%>%select(Age, BPSysAve, SleepTrouble)
trainAB[-3]<-scale(trainAB[-3])
testAB[-3]<-scale(testAB[-3])
Age = seq(min(testAB[,1]) - 1, max(testAB[,1]) + 1, by = 0.01)
BPSysAve = seq(min(testAB[,2]) - 1, max(testAB[,2]) + 1, by = 0.01)
grid_set = expand.grid(Age, BPSysAve)
colnames(grid_set) = c('Age', 'BPSysAve')
classifier_logAB<-glm(formulaAB, data = train, family = 'binomial')
pred_logisticAB = predict(classifier_logAB, type = 'response', newdata = grid_set)
yhat_AB = if_else(pred_logisticAB >= 0.5, 1, 0)


#plot logreg
plot(testAB[,-3],main = "Logistic Regression (test)", xlab = 'Age', ylab = 'BP Sysstolic', xlim = range(Age), ylim = range(BPSysAve))
contour(Age, BPSysAve, matrix(as.numeric(yhat_AB), length(Age), length(BPSysAve)), add = TRUE)
points(grid_set, pch = 21, col = ifelse(yhat_AB == 1, 'lightblue', 'orange'))
points(testAB, pch = 21, bg = ifelse(testAB[, 3] == "Yes", 'blue', 'tomato'))


#dtree
classifier_tree_train<-glm(formula, data = train)
classifier_tree_train
classifier_tree_test<-glm(formula, data = test)
classifier_tree_test

#dtree accuracy
pred_tree_train = predict(classifier_tree_train, newdata = train[-7], type = 'class')
cm<-table(train$SleepTrouble, pred_dtree_train)
cm
sum(diag(cm)) / nrow(train)

classifier_tree<-rpart(formulaAB, data = trainAB)
pred_tree = predict(classifier_tree, newdata = grid_set, type = 'class')


#plot dtree
plot(testAB[,-3],main = "Decision tree regression model", xlab = 'Age', ylab = 'BP Sysstolic', xlim = range(Age), ylim = range(BPSysAve))
contour(Age, BPSysAve, matrix(as.numeric(pred_tree), length(Age), length(BPSysAve)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(pred_dtree == 'Yes', 'lightblue', 'orange'))
points(testAB, pch = 21, bg = ifelse(testAB[, 3] == "Yes", 'blue', 'red'))


#randforest
classifier_forest_train<-randomForest(formula, data = train)
classifier_forest_train
classifier_forest_test<-randomForest(formula, data = test)
classifier_forest_test

#randforest accuracy
sum(diag(classifier_forest_train$confusion)) / nrow(train)
sum(diag(classifier_forest_test$confusion)) / nrow(test)

classifier_forest<-randomForest(formulaAB, data = trainAB, ntree = 500, mtry = 2)
pred_forest = predict(classifier_forest, newdata = grid_set, type = 'class')

#randforest graph
plot(testAB[,-3],main = "Random Forest Classifier (test)", xlab = 'Age', ylab = 'BP Sysstolic', xlim = range(Age), ylim = range(BPSysAve))
contour(Age, BPSysAve, matrix(as.numeric(pred_forest), length(Age), length(BPSysAve)), add = TRUE)
points(grid_set, pch = 21, col = ifelse(pred_forest == 'Yes', 'lightblue', 'orange'))
points(testAB, pch = 21, bg = ifelse(testAB[, 3] == "Yes", 'blue', 'tomato'))


