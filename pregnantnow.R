install.packages("NHANES")
install.packages("mdsr")
install.packages("ddplyr")
install.packages("tidyr")
install.packages("ggplot2")
library(NHANES)
library(dplyr)
library(mdsr)
library(tidyr)
library(ggplot2)

#pregnant Now
glimpse(NHANES$PregnantNow)
nhanes1<-NHANES%>%select(Gender, Age, HHIncome, BPDiaAve, BPSysAve, Depressed, PregnantNow) %>% na.omit()
summary(nhanes1$PregnantNow)



set.seed(123)
index = sort(sample(nrow(nhanes1), nrow(nhanes1) * 0.8))
train<-nhanes1[index,]
test<-nhanes1[index,]
train$PregnantNow
glimpse(test)
glimpse(train)


#accuracy of nullmod
mod_null<-train%>%group_by(PregnantNow)%>%summarize(N=n())%>%mutate(percent = N/sum(N))
mod_null

#nullmod graph
ggplot(data = test, aes(x = Age, y = BPDiaAve))+geom_point(aes(color = PregnantNow))
+ggtitle("Scatterplot-Age vs BPDiaAve (test set)") +theme(plot.title = element_text(hjust = 0.5))


#logreg
formula<- as.formula("PregnantNow ~Gender + Age + HHIncome + BPDiaAve + BPSysAve + Depressed" )
library(nnet)
classifier_log<-multinom(formula, data = train)
classifier_log


#logreg accuracy
prob_pred = predict(classifier_log, type = 'class', newdata = train[-7])
cm_lr = table(train$PregnantNow, prob_pred)
cm_lr
sum(diag(cm_lr)) / nrow(train)

formulaAB<-as.formula("PregnantNow~Age + BPDiaAve")
testAB = test%>%select(Age, BPDiaAve, PregnantNow)
trainAB = train%>%select(Age, BPDiaAve, PregnantNow)
testAB[-3]<-scale(testAB[-3])
trainAB[-3]<-scale(trainAB[-3])
Age = seq(min(testAB[, 1])- 1, max(testAB[, 1]) + 1, by = 0.01)
BPDiaAve = seq(min(testAB[, 2])- 1, max(testAB[, 2]) + 1, by = 0.01)
grid_set = expand.grid(Age, BPDiaAve)
colnames(grid_set) = c('Age', 'BPDiaAve') 

classifier_logAB<-multinom(formulaAB, data = trainAB)
pred_logisticAB = predict(classifier_logAB, type = 'class', newdata = grid_set)

#logmod graph
ggplot(data = grid_set, aes(x = Age, y = BPDiaAve))+
  geom_tile(aes(fill = pred_logisticAB))+
  scale_fill_manual(values = c("No" = "orange", "Yes" = "green", "unknown" = "lightblue"))
  geom_point(aes(color = PregnantNow), data = testAB)+
  scale_color_manual(values = c("No" = "red", "Yes" = "darkgreen", "unknown" = "blue"))+
  ggtitle("Logistic Regression (test)")+
  theme(plot.title = element_text(hjust = 0.5))


#dtree
install.packages("rpart")
library(rpart)
classifier_tree<- rpart(formula, data = train)
classifier_tree

#accuracy of dtree
pred_dtree = predict(classifier_tree, newdata = train[-7], type = "class")
cm<-table(train$PregnantNow, pred_dtree)
cm
sum(diag(cm)) / nrow(train)

classifier_tree<-rpart(formulaAB, data = trainAB) 
pred_tree = predict(classifier_tree, newdata = grid_set, type = "class")

#dtree graph
ggplot(data = grid_set, aes(x = Age, y = BPDiaAve))+
  geom_tile(aes(fill = pred_tree), color = NA)+
  scale_fill_manual(values = c("No" = "orange", "Yes" = "green", "unknown" = "lightblue"))
geom_point(aes(color = PregnantNow), data = testAB)+scale_color_manual(values = c("No" = "red", "Yes" = "darkgreen", "unknown" = "blue"))+ggtitle("Decision tree classifier(test)")+theme(plot.title = element_text(hjust = 0.5))


#randforest
install.packages('randomForest')
library(randomForest)
classifier_forest
sum(diag(classifier_forest$confusion)) / nrow(train)


library(tibble)
importance(classifier_forest)%>%as.data.frame()%>%rownames_to_column()%>%arrange(desc(MeanDecreaseGini))

classifier_forest<-randomForest(formulaAB, data = trainAB, ntree = 201, mtry = 2)
pred_forest = predict(classifier_forest, newdata = grid_set, type = "class")

#randforest graph
ggplot(data = grid_set, aes(x = Age, y = BPDiaAve))+geom_tile(aes(fill = pred_forest), color = NA)+scale_fill_manual(values = c("No" = "orange", "Yes" = "green", "unknown" = "lightblue"))
geom_point(aes(color = PregnantNow), data = testAB)+scale_color_manual(values = c("No" = "red", "Yes" = "darkgreen", "unknown" = "blue"))+ggtitle("Random Forest classifier(test)")+theme(plot.title = element_text(hjust = 0.5))




















formulaAB<- as.formula("PregnantNow ~ Age + BPSysAve")
testAB = test%>%select(Age, BPSysAve, )
trainAB<-train%>%select(Age, BPSysAve, PregnantNow)
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





