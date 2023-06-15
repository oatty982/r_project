# -*- coding: utf-8 -*-

# -- Sheet --

# Titanic Project (Logistic Regression)


#### Titanic Project
library(tidyverse)
titanic <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")

as_tibble(titanic)

# ก่อนเปลี่ยน Sex เป็น factor
glimpse(titanic)

## Drop NA
titanic <- na.omit(titanic)

## Change chr to fct
titanic$Sex <- if_else(titanic$Sex == "male", 1, 0)
titanic$Sex <- as.factor(titanic$Sex)
glimpse(titanic)

# **Split data**


# set.seed คือเลขสุ่มแบบทำซ้ำ
set.seed(22)

# nrow จะได้จำนวน row ทั้งหมด ใน titanic เข้าไปใน n
n <- nrow(titanic) # 714

# sample(x, size, replace = FALSE, prob = NULL)
# sample สุ่มข้อมูลตัวอย่างใน data set titanic
# https://www.digitalocean.com/community/tutorials/sample-in-r

id <- sample(1:n, size = n*0.7) # train70, test 30

train_data <- titanic[id, ]
test_data <- titanic[-id, ]

# **Train Model**
# URL : https://www.statmethods.net/advstats/glm.html


# Logistic Regression = binomial
# https://www.datacamp.com/tutorial/generalized-linear-models
train_model <- glm(Survived ~ Pclass + Sex + Age + Parch + SibSp, 
                   data = train_data, 
                   family = "binomial")
summary(train_model)

# Predict & Evaluate Train Model
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/predict.glm.html

train_data$prob_survived <- predict(train_model, type = "response")
train_data$prob_survived

train_data$pred_survived <- ifelse(train_data$prob_survived >= 0.5, 1, 0)
train_data$pred_survived

glimpse(train_data)

# Confusion metric
conM_train <- table(train_data$pred_survived, train_data$Survived,
                    dnn = c("Predicted", "Actual"))
conM_train

# Evaluate train model
acc_train<- (conM_train[1,1] + conM_train[2,2]) / sum(conM_train)
acc_train

prec_train <- conM_train[2,2] / (conM_train[2,1] + conM_train[2,2]) 
prec_train

rec_train <- conM_train[2,2] / (conM_train[1,2] + conM_train[2,2])
rec_train

f1_train <- 2*(prec_train*rec_train) / (prec_train+rec_train)
f1_train

# **Test Model**


test_model <-glm(Survived ~ Pclass + Sex + Age + Parch + SibSp, 
                 data = test_data, 
                 family = "binomial")
summary(test_model)

# Predict & Evaluate Test Model
test_data$prob_survived <- predict(test_model, type = "response")
test_data$pred_survived <- ifelse(test_data$prob_survived >= 0.5, 1, 0)
glimpse(test_data)

# Confusion metric
conM_test <- table(test_data$pred_survived, test_data$Survived,
                    dnn = c("Predicted", "Actual"))
conM_test

# Evaluate test model
acc_test<- (conM_test[1,1] + conM_test[2,2]) / sum(conM_test)
prec_test <- conM_test[2,2] / (conM_test[2,1] + conM_test[2,2]) 
rec_test <- conM_test[2,2] / (conM_test[1,2] + conM_test[2,2]) 
f1_test <- 2*(prec_test*rec_test) / (prec_test+rec_test)

# **Train model and Test model**


# Train model
data.frame(Accuracy_train = acc_train, 
           Precision_train = prec_train, 
           Recall_train = rec_train, 
           F1_train = f1_train)

# Test model
data.frame(Accuracy_test = acc_test, 
           Precision_test = prec_test, 
           Recall_test = rec_test, 
           F1_test = f1_test) 

