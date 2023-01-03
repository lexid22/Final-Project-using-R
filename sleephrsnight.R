install.packages("NHANES")
install.packages("mdsr")
install.packages("ddplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("partykit")
library(NHANES)
library(dplyr)
library(mdsr)
library(tidyr)
library(ggplot2)

#sleephrsnight
glimpse(NHANES$SleepHrsNight)
nhanes1<-NHANES%>%select(Age, BPDiaAve, BPSysAve, Depressed, SleepHrsNight) %>% na.omit()
glimpse(nhanes1)
summary(nhanes1$SleepHrsNight)

set.seed(123)
index = sort(sample(nrow(nhanes1), nrow(nhanes1) * 0.8))
train<-nhanes1[index,]
test<-nhanes1[index,]
glimpse(test)
glimpse(train)


#null model
classifier_null<-lm(SleepHrsNight~1, data = train)
summary(classifier_null)
null_pred<-predict(classifier_null, newdata = train[-4])
SSE = var(classifier_null$residuals)
SST = var(classifier_null$model)[1]
RSq = 1-SSE/SST
RSq
RMSE_null<-sqrt(mean((train$SleepHrsNight-null_pred)^2))
RMSE_null
train_null<-train%>%mutate(predict = null_pred)
ggplot(train_null, aes(y=predict, x = SleepHrsNight))+geom_point()+geom_abline()+labs(x="Actual SleepHrsNight", y="Predicted SleepHrsNight", title = "Actual vs Predicted")+theme(plot.title = element_text(hjust = 0.5))


#multiple reg model
nhanes1<-NHANES%>%select(Age, BPSysAve, Depressed, SleepHrsNight) %>% na.omit()
glimpse(nhanes1)
set.seed(123)
index = sort(sample(nrow(nhanes1), nrow(nhanes1) * 0.8))
train<-nhanes1[index,]
test<-nhanes1[index,]
glimpse(test)
glimpse(train)
formula<-as.formula("SleepHrsNight~Age+BPSysAve+Depressed")
classifier_mlr<-lm(formula, data = train)
mlr_pred<-predict(classifier_mlr, data = train[-4])
summary(classifier_mlr)
RMSE_mlr<-sqrt(mean((train$SleepHrsNight-mlr_pred)^2))
RMSE_mlr
train_mlr<-train%>%mutate(predict = mlr_pred)
ggplot(train_mlr, aes(y=predict, x = SleepHrsNight))+geom_point()+geom_abline()+labs(x="Actual SleepHrsNight", y="Predicted SleepHrsNight", title = "MLR Actual vs Predicted")+theme(plot.title = element_text(hjust = 0.5))


#regression tree
nhanes1<-NHANES%>%select(Age, BPSysAve, Depressed, SleepHrsNight) %>% na.omit()
glimpse(nhanes1)
set.seed(123)
index = sort(sample(nrow(nhanes1), nrow(nhanes1) * 0.8))
train<-nhanes1[index,]
test<-nhanes1[index,]
glimpse(test)
glimpse(train)
formula<-as.formula("SleepHrsNight~Age+BPSysAve+Depressed")
classifier_rt<- rpart(formula, data = train, method = "anova")
classifier_rt
rtree_pred = predict(classifier_rt, newdata = train[-4])
RMSE_rt<-RMSE_mlr<-sqrt(mean((train$SleepHrsNight-rtree_pred)^2))
RMSE_rt
train_tree<-train%>%mutate(predict = rtree_pred)
ggplot(train_tree, aes(y=predict, x = SleepHrsNight))+geom_point(aes(color = SleepHrsNight))+geom_abline()+labs(x="Actual SleepHrsNight", y="Predicted SleepHrsNight", title = "MLR Actual vs Predicted")+theme(plot.title = element_text(hjust = 0.5))
head(train_tree[,4:5],10)
#reg tree model classifier
library(rpart.plot)
rpart.plot(classifier_rt, main = "regression tree classifier")


#randforest
nhanes1<-NHANES%>%select(Age, BPSysAve, Depressed, SleepHrsNight) %>% na.omit()
glimpse(nhanes1)
set.seed(123)
index = sort(sample(nrow(nhanes1), nrow(nhanes1) * 0.8))
train<-nhanes1[index,]
test<-nhanes1[index,]
glimpse(test)
glimpse(train)
formula<-as.formula("SleepHrsNight~Age+BPSysAve+Depressed")
library(randomForest)
classifier_forest<- randomForest(formula, data = train)
classifier_forest
sum(diag(classifier_forest$confusion)) / nrow(train)
forest_pred<-predict(classifier_forest, newdata = train[-4])
RMSE_forest<-sqrt(mean((train$SleepHrsNight-forest_pred)^2))
RMSE_forest
train_forest<-train%>%mutate(predict = forest_pred)
ggplot(train_forest, aes(y=predict, x = SleepHrsNight))+geom_point()+geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+labs(x="Actual SleepHrsNight", y="Predicted SleepHrsNight", title = "Actual vs Predicted Plot Random Forest")+theme(plot.title = element_text(hjust = 0.5))
library(partykit)
plot(ctree(formula, data = train), main = "Random forest for a sample tree")








#accuracy of null model
mod_null2<-train%>%group_by(SleepHrsNight)%>%summarize(N=n())%>%mutate(percent = N/sum(N))
mod_null2

#acuracy of log_reg
prob_pred2 = predict(classifier_log, type = 'response', newdata = train[-9])
y_hat_train2 = ifelse(prob_pred >= 0.5, 1, 0)
cm_lr2 = table(train$SleepHrsNight, y_hat_train2)
cm_lr2

#accuracy of dtree
pred_dtree2 = predict(classifier_tree2, newdata = train[-9], type = "class")
cm2<-table(train$SleepHrsNight, pred_dtree2)
cm2

#accuracy of randforest
sum(diag(classifier_forest2$confusion)) / nrow(train)


#null mod visualization
ggplot(data = test, aes(x = Age, y = BPSysAve))+geom_point(aes(color = SleepHrsNight))
+ggtitle("Null model (test set)") +theme(plot.title = element_text(hjust = 0.5))

#log mod visualization
formulaAB2<- as.formula("SleepHrsNight ~ Age + BPSysAve")
testAB2 = test%>%select(Age, BPSysAve, SleepHrsNight)
trainAB2<-train%>%select(Age, BPSysAve, SleepHrsNight)
trainAB2[-3]<-scale(trainAB2[-3])
testAB2[-3]<-scale(testAB2[-3])
Age = seq(min(testAB2[, 1])- 1, max(testAB2[, 1]) + 1, by = 0.01)
BPSysAve = seq(min(testAB2[, 2])- 1, max(testAB2[, 2]) + 1, by = 0.01)
grid_set = expand.grid(Age, BPSysAve)
colnames(grid_set) = c('Age', 'BPSysAve') 
classifier_logAB2<-glm(formulaAB2, data = trainAB2, family = 'binomial')
pred_logisticAB2 = predict(classifier_logAB2, type = 'response', newdata = grid_set)
yhat_AB2 = if_else(pred_logisticAB2 >= 0.5, 1, 0)
plot(testAB2[, 3],
     main = 'Logistic Regression (test set)',
     xlab = 'Age', ylab = 'BP Systolic',
     xlim = range(Age), ylim = range(BPSysAve))
contour(Age, BPSysAve, matrix(as.numeric(yhat_AB2), length(Age), length(BPSysAve)), add = TRUE)
points(grid_set, pch = 15, col = ifelse(yhat_AB2 == 1, 'green', 'lightblue'))
points(testAB2, pch = 16, bg = ifelse(testAB2[, 3] == "Yes", 'blue', 'yellow'))


