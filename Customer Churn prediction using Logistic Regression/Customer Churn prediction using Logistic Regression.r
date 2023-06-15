# -*- coding: utf-8 -*-

# -- Sheet --

# # Customer Churn prediction using Logistic Regression


# by Patcharapon Chaiporn


# **Import Library**


library(tidyverse)
library(ggplot2)
library(corrplot) #install.packages("corrplot")

# **Data Preparation**


churn <- read_csv("churn.csv")
glimpse(churn)

# Check missing value
colSums(is.na(churn))

# Change churn, internationalplan and voicemailplan into factor
churn$churn <- ifelse(churn$churn == "No", 0 ,1)
churn$internationalplan <- ifelse(churn$internationalplan == "no", 0 ,1)
churn$voicemailplan <- ifelse(churn$voicemailplan == "no", 0 ,1)

churn$churn <- as.factor(churn$churn)
churn$internationalplan <- as.factor(churn$internationalplan)
churn$voicemailplan <- as.factor(churn$voicemailplan)

# **Data Exploration**


ggplot(churn, aes(churn, fill = churn)) +
  geom_bar() +
  labs(title = "Numbers of Churn Customer")+
  theme(plot.title = element_text(hjust = 0.5))
  hist(churn$accountlength)

ggplot(churn, aes(internationalplan, fill = churn)) +
  geom_bar() +
  labs(title = "Numbers of Churn Customer in each international plan")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(voicemailplan, fill = churn)) +
  geom_bar() +
  labs(title = "Numbers of Churn Customer in each voice mail plan")+
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(churn, aes(numbervmailmessages)) +
  geom_histogram(binwidth = 10) + 
  labs(title = "Histogram of number vmail message")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totaldayminutes,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of totaldayminutes and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totaldaycalls,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of totaldaycalls and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totaldaycharge,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of totaldaycharge and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totaleveminutes,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of totaleveminutes and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totalevecalls,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of totalevecalls and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totalevecharge,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of totalevecharge and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totalnightminutes,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of totalnightminutes and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totalnightcalls,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of totalnightcalls and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totalnightcharge,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of totalnightcharge and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totalintlminutes,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of totalintlminutes and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totalintlcalls,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of totalintlcalls and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totalintlcharge,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of totalintlcharge and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(numbercustomerservicecalls,churn, fill = churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of numbercustomerservicecalls and churn type")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totaldayminutes, totaldaycalls)) +
  geom_point() +
  facet_grid(churn$churn) +
  labs(title = "Total day calls vs minutes between churn and non churn customer")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totaleveminutes, totalevecalls)) +
  geom_point() +
  facet_grid(churn$churn) +
  labs(title = "Total evening calls vs minutes between churn and non churn customer")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(churn, aes(totalnightminutes, totalnightcalls)) +
  geom_point() +
  facet_grid(churn$churn) +
  labs(title = "Total night calls vs minutes between churn and non churn customer")+
  theme(plot.title = element_text(hjust = 0.5))

# **Finding Correlation**


num_cols <- unlist(lapply(churn, is.numeric))
CR <- cor(churn[, num_cols])
corrplot(CR, method = "color",type="lower")

# drop some columns that highly relate to each others
churn <- churn %>% 
  select(-c(voicemailplan,totaldaycharge, totalevecharge, totalnightcharge, totalintlcharge))

# **Split data**


set.seed(89)
n <- nrow(churn)
id <- sample(1:n, size = n*0.8)
train_data <- churn[id, ]
test_data <- churn[-id, ]
nrow(train_data)
nrow(test_data)

prop.table(table(train_data$churn))

# **Train model**


trained_model <- glm(churn ~ internationalplan
                      + totaldayminutes + totaleveminutes + totalnightminutes
                      + totalintlminutes + totalintlcalls + numbercustomerservicecalls
                      , data = train_data, family = "binomial")
summary(trained_model)
train_data$prob_churn <- predict(trained_model,type="response")
train_data$pred_churn <- ifelse(train_data$prob_churn>=0.5,1,0)

# **Test Model**


test_data$prob_churn <- predict(trained_model,newdata = test_data, type="response")
test_data$pred_churn <- ifelse(test_data$prob_churn>=0.5,1,0)

# **Model Evaluation**


train_conM <- table(train_data$pred_churn, train_data$churn, 
                    dnn =c("Predicted", "Actual"))

test_conM <- table(test_data$pred_churn, test_data$churn, 
                   dnn =c("Predicted", "Actual"))

train_acc <- (train_conM[1,1] + train_conM[2,2])/ sum(train_conM)
test_acc <- (test_conM[1,1] + test_conM[2,2])/ sum(test_conM)

train_prec <- (train_conM[2,2]/(train_conM[2,1] + train_conM[2,2]))
test_prec <- (test_conM[2,2]/(test_conM[2,1] + test_conM[2,2]))

train_rec <- (train_conM[2,2]/(train_conM[1,2] + train_conM[2,2]))
test_rec <- (test_conM[2,2]/(test_conM[1,2] + test_conM[2,2]))

train_f1 <- 2 * (train_prec*train_rec / (train_prec+train_rec))
test_f1 <- 2 * (test_prec*test_rec / (test_prec+test_rec))

cat("Train Accuracy:", train_acc, "| Test Accuracy:", test_acc ,"\n")

cat("Train Precision:",train_prec, "| Test Precision:", test_prec,"\n")

cat("Train Recall:", train_rec, "| Test Recall:", test_rec,"\n")

cat("Train F1:", train_f1, "| Test F1:", test_f1)



