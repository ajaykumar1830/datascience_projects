#Assignment on Machine Learning 
#Read the data from the csv file
library(ggplot2)
commuter_data <- read.csv("C:/Users/AK57630/Downloads/GreatLakes/Text Analytics/Cars.csv")
commuter_data$travel_by_car <- ifelse(commuter_data$Transport == "Car", "Yes", "No")
commuter_data$travel_by_car_binary <- ifelse(commuter_data$travel_by_car == "No", 0, 1)
commuter_data$male_female_binary <- ifelse(commuter_data$Gender == "Male", 1, 0)
View(commuter_data)

library(data.table)

agebreaks <- c(20,25,30,35,40,45)
agelabels <- c("20-24","25-29","30-34",
               "35-39","40-44")

setDT(commuter_data)[ , agegroups := cut(Age, 
                                breaks = agebreaks, 
                                right = FALSE, 
                                labels = agelabels)]

ggplot(data = commuter_data,aes(x=agegroups,fill=factor(travel_by_car)))+geom_bar(position = "dodge")

ggplot(data = commuter_data,aes(x=Gender,fill=factor(travel_by_car)))+geom_bar(position = "dodge")
commuter_data$MBA_degree <- ifelse(commuter_data$MBA == 0, "No", "Yes")
ggplot(data = commuter_data,aes(x=MBA_degree,fill=factor(travel_by_car)))+geom_bar(position = "dodge")
commuter_data$IsEngineer <- ifelse(commuter_data$Engineer == 0, "No", "Yes")
ggplot(data = commuter_data,aes(x=IsEngineer,fill=factor(travel_by_car)))+geom_bar(position = "dodge")
commuter_data$haslicense <- ifelse(commuter_data$license == 0, "No", "Yes")
ggplot(data = commuter_data,aes(x=haslicense,fill=factor(travel_by_car)))+geom_bar(position = "dodge")
ggplot(data = commuter_data,aes(x=Transport))+geom_bar()

#Percentage employees travelling by car
table(commuter_data$travel_by_car)
Percent_travel_by_car <-prop.table(table(commuter_data$travel_by_car))
Percent_travel_by_car
#commuter_data_temp <- commuter_data
commuter_data <- commuter_data[,c(1,12,3,4,5,6,7,8,11,9,10,2,13,14,15)]
View(commuter_data)
commuter_data[,10:15] = NULL
na.omit(commuter_data)
#Split train and Test data

library(caTools)       
library(class)
split <- sample.split(commuter_data$travel_by_car, SplitRatio = 0.75)
commuter_data_train <- subset(commuter_data, split == TRUE)
commuter_data_test <- subset(commuter_data, split == FALSE)
View(commuter_data_test)
View(commuter_data_train)
#KNN algorithn for classification
knn_fit<- knn(train = commuter_data_train[,1:8], test = commuter_data_test[,1:8], cl= commuter_data_train$travel_by_car_binary,k = 5,prob=TRUE) 

table(commuter_data_test$travel_by_car_binary,knn_fit)
library(MASS)
#Logistic Regression

commuter_logistic <- glm(travel_by_car_binary ~., data=commuter_data_train, family=binomial(link="logit"))
optimized_model <- stepAIC(commuter_logistic, direction="both")
commuter_data_test$log.pred<-predict(optimized_model, commuter_data_test[1:8], type="response")
table(commuter_data_test$travel_by_car_binary,commuter_data_test$log.pred>0.5)

library(e1071)

#Naive Bayes
nb_commuter<-naiveBayes(x=commuter_data_train[,1:8], y=as.factor(commuter_data_train$travel_by_car_binary))

pred_nb<-predict(nb_commuter,newdata = commuter_data_test[,1:8])

table(commuter_data_test$travel_by_car_binary,pred_nb)

library(gbm)
library(caret)

#Gradient Boosting Method
commuter.fit <- gbm(
  formula = travel_by_car_binary ~ .,
  distribution = "bernoulli",
  data = commuter_data_train,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

commuter_data_test$pred.class <- predict(commuter.fit, commuter_data_test, type = "response")

table(commuter_data_test$travel_by_car_binary,commuter_data_test$pred.class>0.5)

#Xgboost method
library(xgboost)
commuter_features_train<-as.matrix(commuter_data_train[,1:8])
commuter_label_train<-as.matrix(commuter_data_train$travel_by_car_binary)
commuter_features_test<-as.matrix(commuter_data_test[,1:8])

xgb.fit <- xgboost(
  data = commuter_features_train,
  label = commuter_label_train,
  eta = 0.001,
  max_depth = 3,
  min_child_weight = 3,
  nrounds = 10000,
  nfold = 5,
  objective = "binary:logistic",  # for regression models
  verbose = 0,               # silent,
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)
xgb.importance(model = xgb.fit)
commuter_data_test$xgb.pred.class <- predict(xgb.fit, commuter_features_test)
table(commuter_data_test$travel_by_car_binary,commuter_data_test$xgb.pred.class>0.5)

library(DMwR)

table(commuter_data$travel_by_car_binary)

smote.train<-subset(commuter_data, split == TRUE)
smote.test<-subset(commuter_data, split == FALSE)

smote.train$travel_by_car_binary<-as.factor(smote.train$travel_by_car_binary)
balanced.commuter_data <- SMOTE(travel_by_car_binary ~., smote.train, perc.over = 4800, k = 5, perc.under = 500)
table(balanced.commuter_data$travel_by_car_binary)
1274/6240