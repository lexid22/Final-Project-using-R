install.packages("NHANES")
library(tidymodels)
library(tidyverse)
library(yardstick)
library(NHANES)
install.packages('ranger')
library(ranger)
install.packages('randomForest')
library(randomForest)

#data prep
glimpse(NHANES)
table(NHANES$SleepTrouble)
tally(~SleepTrouble,data=NHANES, format = "percent")
dim(NHANES)
NHANES['SleepTrouble']
n<-nrow(NHANES)
table(NHANES$SleepTrouble, useNA='always')
table(NHANES$Age, useNA='always')
table(NHANES$PregnantNow, useNA = "always")
NHANES %>% count(SleepTrouble) %>% mutate(pct = n/sum(n))
data<- NHANES %>% drop_na(SleepTrouble)
n2<-nrow(data)
table(data$SleepTrouble, useNA='always')
table(data$Age, useNA='always')
data %>% count(SleepTrouble) %>% mutate(pct = n2/sum(n2))
table(data$SleepTrouble, useNA='always')
data$SleepTrouble<- as.factor(data$SleepTrouble)
NHANES$SleepTrouble<- as.factor(NHANES$SleepTrouble)
NHANES$PregnantNow<- as.factor(NHANES$SleepTrouble)


NHANES %>% count(SleepTrouble) %>% mutate(pct = n/sum(n))



#Sleeptrouble Null_model
mod_null<-logistic_reg(mode = "classification")%>%set_engine("glm")%>%fit(SleepTrouble ~ 1, data = NHANES)
pred<-NHANES %>% select(SleepTrouble, MaritalStatus) %>% bind_cols(predict(mod_null, new_data=NHANES,type = "class")) %>% rename(SleepTrouble_null = .pred_class)
accuracy(pred,SleepTrouble,SleepTrouble_null)
confusion_null<-pred %>% conf_mat(truth = SleepTrouble, estimate = SleepTrouble_null)
confusion_null

#SleepTrouble reg_mod
mod_log_1<-logistic_reg(mode = "classification")%>%set_engine("glm")%>%fit(SleepTrouble ~ MaritalStatus, data = NHANES)
train_plus<- data %>% mutate(sleep_data = as.integer(SleepTrouble == 1))
ggplot(train_plus, aes(x = Age, y = sleep_data))+geom_count(position = position_jitter(),alpha = 0.5)+
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "dodgerblue", lty = 2, se = FALSE)+
  geom_hline(aes(yintercept = 0.5), linetype = 3)+
scale_x_log10()
pred<-NHANES %>% select(SleepTrouble, MaritalStatus) %>% bind_cols(predict(mod_log_1, new_data=NHANES,type = "class")) %>% rename(sleep_log_1 = .pred_class)
confusion_log_1<-pred %>% conf_mat(truth = SleepTrouble, estimate = sleep_log_1)
confusion_log_1
accuracy(pred, SleepTrouble, sleep_log_1)



#prep dtree and randforest
form<-as.formula("SleepTrouble ~ Age + Gender + BMI + MaritalStatus + Work + HHIncome + Education")

#sleeptrouble dtree
mod_dtree<- decision_tree(mode = 'classification') %>% set_engine('rpart') %>% fit(form, data = NHANES)
mod_dtree
pred3<-NHANES %>% select(SleepTrouble) %>% bind_cols(predict(mod_dtree, new_data=NHANES,type = "class")) %>% rename(SleepTroubleTree = .pred_class)
accuracy(pred3,SleepTrouble,SleepTroubleTree)
confusion_null<-pred3 %>% conf_mat(truth = SleepTrouble, estimate = SleepTroubleTree)
confusion_null
#sleeptrouble randforest
mod_forest<-rand_forest(mode = "classification", mtry =  3, trees = 201) %>% set_engine("randomForest") %>% fit(form, data = train)
pred<- pred %>% bind_cols(predict(mod_forest, new_data = train, type = "class")) %>% rename(SleepTrouble = .pred_class)
pred4 <- pred3 %>% bind_cols(predict(mod_forest, new_data = train, type = "class")) %>%rename(SleepTrouble_forest = .pred_class)


#sleephrs mod_null
NHANES$SleepHrsNight<- as.factor(NHANES$SleepHrsNight)
mod_null<-logistic_reg(mode = "classification")%>%set_engine("glm")%>%fit(SleepHrsNight ~ 1, data = NHANES)
pred<-NHANES %>% select(SleepHrsNight, MaritalStatus) %>% bind_cols(predict(mod_null, new_data=NHANES,type = "class")) %>% rename(SleepHrsNight_null = .pred_class)
accuracy(pred,SleepHrsNight,SleepHrsNight_null)
confusion_null<-pred %>% conf_mat(truth = SleepHrsNight, estimate = SleepHrsNight_null)
confusion_null

#Sleephrs reg_mod
mod_log_1<-logistic_reg(mode = "classification")%>%set_engine("glm")%>%fit(SleepHrsNight ~ MaritalStatus, data = NHANES)
train_plus<- data %>% mutate(sleep_data = as.integer(SleepHrsNight == 1))
ggplot(train_plus, aes(x = MaritalStatus, y = sleep_data))+geom_count(position = position_jitter(),alpha = 0.5)+
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "dodgerblue", lty = 2, se = FALSE)+
  geom_hline(aes(yintercept = 0.5), linetype = 3)+
  scale_x_log10()
pred2<-NHANES %>% select(SleepHrsNight, MaritalStatus) %>% bind_cols(predict(mod_log_1, new_data=NHANES,type = "class")) %>% rename(sleephrs_log_reg = .pred_class)
confusion_log_1<-pred2 %>% conf_mat(truth = SleepHrsNight, estimate = sleephrs_log_reg)
confusion_log_1
accuracy(pred2, SleepHrsNight, sleephrs_log_reg)

#sleephrs dtree
mod_dtree<- decision_tree(mode = 'classification') %>% set_engine('rpart') %>% fit(SleepHrsNight ~ Age, data = NHANES)
mod_dtree
pred3<-NHANES %>% select(SleepHrsNight, Age) %>% bind_cols(predict(mod_dtree, new_data=NHANES,type = "class")) %>% rename(SleepHrsTree = .pred_class)
accuracy(pred3,SleepHrsNight,SleepHrsTree)
confusion_null<-pred3 %>% conf_mat(truth = SleepHrsNight, estimate = SleepHrsTree)
confusion_null

#randforest sleephrs
mod_forest<-rand_forest(mode = "classification", mtry =  3, trees = 201) %>% set_engine("randomForest") %>% fit(form, data = train)
pred<- pred %>% bind_cols(predict(mod_forest, new_data = train, type = "class")) %>% rename(SleepHrsNight = .pred_class)


#split data train/test
set.seed(364)
n<-nrow(NHANES)
sleep_parts<- data %>% initial_split(prop = 0.75)
train<-sleep_parts %>% training()
test<-sleep_parts %>% testing()
train %>% count(SleepTrouble) %>% mutate(pct = n/sum(n)) 


#pregnant null mod
mod_null<-logistic_reg(mode = "classification")%>%set_engine("glm")%>%fit(PregnantNow ~ 1, data = NHANES)
pred4<-NHANES %>% select(PregnantNow, MaritalStatus) %>% bind_cols(predict(mod_null, new_data=NHANES,type = "class")) %>% rename(Pregnant_null = .pred_class)
accuracy(pred4,PregnantNow,Pregnant_null)
confusion_null<-pred4 %>% conf_mat(truth = PregnantNow, estimate = Pregnant_null)
confusion_null

#pregnant reg_mod
mod_log_1<-logistic_reg(mode = "classification")%>%set_engine("glm")%>%fit(PregnantNow ~ MaritalStatus, data = NHANES)
train_plus<- data %>% mutate(pregnant_data = as.integer(PregnantNow == 1))
ggplot(train_plus, aes(x = MaritalStatus, y = pregnant_data))+geom_count(position = position_jitter(),alpha = 0.5)+
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "dodgerblue", lty = 2, se = FALSE)+
  geom_hline(aes(yintercept = 0.5), linetype = 3)+
  scale_x_log10()
pred4<-NHANES %>% select(PregnantNow, MaritalStatus) %>% bind_cols(predict(mod_log_1, new_data=NHANES,type = "class")) %>% rename(pregnant_log_1 = .pred_class)
confusion_log_1<-pred4 %>% conf_mat(truth = PregnantNow, estimate = pregnant_log_1)
confusion_log_1
accuracy(pred4, PregnantNow, pregnant_log_1) 
