# -*- coding: utf-8 -*-

# -- Sheet --
install.packages("tidyverse")
install.packages("ggplot2")

# # Logistic Regression Analysis & K-Mean Cluster Analysis


# **1. Getting data**
# 
# link : https://data.world/earino/churn/workspace/file?filename=churn.csv


# import library
library(tidyverse)
library(ggplot2)

# import data
churn_df <- read.csv("/Users/macintosh/Desktop/r_project/Logistic Regression Analysis & K-Mean Cluster Analysis/churn.csv", stringsAsFactors = TRUE)

head(churn_df)

# **2. Exploratory Data Analysis**



# Counting the number of missing values 

cat("Missing values =",sum(is.na(churn_df)))

# Explore data
glimpse(churn_df)

# summary statistics
summary(churn_df)

# **Visualizing Statistics**


# no. of churners and non churners
ggplot(churn_df, aes(churn, fill = churn)) +
  geom_bar() +
  labs(title = "No of churners and Non churners") +
  theme_classic()

# No of international plan
ggplot(churn_df, aes(internationalplan , fill = internationalplan)) +
  geom_bar() +
  labs(title = "No of international plan") +
  theme_classic()

# Portion of international plan Vs Churn
ggplot(churn_df, aes(internationalplan , fill = churn)) +
  geom_bar(position =  "fill") +
  labs(title = "Portion of international plan Vs Churn", y = "Portion") +
  theme_classic()

# **ลูกค้าที่มี international plan มีแนวโน้มที่จะ churn มากเกือบครึ่งนึง**


# no of voice mail plan
ggplot(churn_df, aes(voicemailplan , fill = voicemailplan)) +
  geom_bar() +
  labs(title = "No of voice mail plan") +
  theme_classic()

# Portion of voice mail plan Vs Churn
ggplot(churn_df, aes(voicemailplan , fill = churn)) +
  geom_bar(position =  "fill") +
  labs(title = "Portion of voice mail plan Vs Churn", y = "Portion") +
  theme_classic()

# **ลูกค้าที่มี voice plan มีแนวโน้มที่จะ churn น้อย**


# Customer service call Vs Churn
ggplot(churn_df, aes(x = numbercustomerservicecalls , fill = churn)) +
  geom_bar(position = "fill") +
  labs( title = "Customer service call Vs Churn", y = "Portion", x= "Number of customer service calls") +
  scale_x_discrete(limits =(c("1","2","3","4","5","6","7","8","9")))+
  theme_classic()

# **ลูกค้าที่โทรติดต่อฝ่ายดูแลลูกค้าตั้งแต่หมายเลข 4 ขึ้นไปมีแนวโน้มที่จะ churn สูง และลูกค้าที่ติดต่อหมายเลข 9 มีแนวโน้มที่จะ churn 100%**


# check variables distribution of day
  ggplot(churn_df, aes(x= churn, y=totaldaycalls, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point") +
    labs(title = "Total day calls")+
    theme_classic()
  
  ggplot(churn_df, aes(x= churn, y=totaldayminutes, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point") +
    labs(title = "Total day minutes")+
    theme_classic()
  
  ggplot(churn_df, aes(x= churn, y=totaldaycharge, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point")+
    labs(title = "Total day charge")+
    theme_classic()

# check variables distribution of evening
  ggplot(churn_df, aes(x= churn, y=totaleveminutes, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point") +
    labs(title = "Total evening minutes")+
    theme_classic()
  
  ggplot(churn_df, aes(x= churn, y=totalevecalls, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point") +
    labs(title = "Total evening calls")+
    theme_classic()
  
  ggplot(churn_df, aes(x= churn, y=totalevecharge, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point")+
    labs(title = "Total evening charge")+
    theme_classic()

# check variables distribution of night
  ggplot(churn_df, aes(x= churn, y=totalnightminutes, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point") +
    labs(title = "Total night minutes")+
    theme_classic()
  
  ggplot(churn_df, aes(x= churn, y=totalnightcalls, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point") +
    labs(title = "Total night calls")+
    theme_classic()
  
  ggplot(churn_df, aes(x= churn, y=totalnightcharge, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point")+
    labs(title = "Total night charge")+
    theme_classic()

# check variables distribution of international
  ggplot(churn_df, aes(x= churn, y=totalintlminutes, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point") +
    labs(title = "Total international minutes")+
    theme_classic()
  
  ggplot(churn_df, aes(x= churn, y=totalintlcalls, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point") +
    labs(title = "Total international calls")+
    theme_classic()
  
  ggplot(churn_df, aes(x= churn, y=totalintlcharge, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point")+
    labs(title = "Total international charge")+
    theme_classic()

# convert data to factor - 0 not churn ,1 is churn
churn_df <- churn_df %>%
  mutate(churn = ifelse(churn == "No", 0, 1),
         internationalplan = ifelse(internationalplan == "no",0,1),
         voicemailplan = ifelse(voicemailplan == "no",0,1)) %>%
  mutate(across(c(churn, internationalplan, voicemailplan), as.factor))

# create totalminutes, totalcalls, totalcharge
churn_df <- churn_df %>%
  mutate(totalminutes = totaldayminutes+totaleveminutes+totalnightminutes,
         totalcalls = totaldaycalls+totalevecalls+totalnightcalls,
         totalcharge = totaldaycharge+totalevecharge+totalnightcharge)

# Explore data
glimpse(churn_df)

# box plot of totalminutes, totalcalls, totalcharge
  ggplot(churn_df, aes(x= churn, y=totalminutes, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point") +
    labs(title = "Total  minutes")+
    theme_classic()
  
  ggplot(churn_df, aes(x= churn, y=totalcalls, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point") +
    labs(title = "Total  calls") +
    theme_classic()
  
  ggplot(churn_df, aes(x= churn, y=totalcharge, color = churn)) +
    geom_boxplot()+
    stat_summary(fun = mean, geom = "point")+
    labs(title = "Total  charge") +
    theme_classic()

# relationship between total minutes and total charge
ggplot(churn_df, aes(totalminutes, totalcharge, color = churn)) +
  geom_point(position = "jitter", alpha = 0.4)+
  theme_classic()

# **3. Model building**


# 1. Split the data to train and test sets


# Split the data to train and test sets
set.seed(90)
n <- nrow(churn_df)
split <- sample(1:n, size = n*0.7)
train <- churn_df[split,]
test <- churn_df[-split,]

# 2. Calculate the baseline accuracy


# Calculate the baseline accuracy
cat("Baseline accuracy")
prop.table(table(train$churn))

# 3. Fitting the model


# Fitting the model
glm_model <- glm(churn ~ ., data = train, family = "binomial")
  # model summary on all variable
summary(glm_model)

  # model summary on statistically significant variables
glm_model <- glm(churn ~ internationalplan + voicemailplan + numbervmailmessages + totalintlcalls + numbercustomerservicecalls, data = train, family = "binomial")
summary(glm_model)

# 5. Train Model Evaluation - confusion matrix




# Train Model Evaluation - confusion matrix 
train_matrix <- table(train$churn, train$predicted, dnn = c("Actual", "Prediected"))
print(train_matrix)
TP <- train_matrix[2,2]
FP <- train_matrix[1,2]
FN <- train_matrix[2,1]
TN <- train_matrix[1,1]

train_accuracy <- (TP + TN) / (TP+FP+FN+TN)
train_precision <- TP / (TP+FP)
train_recall <- TP / (TP+FN)
train_f1score <- 2*(train_precision*train_recall) / (train_precision+train_recall)
cat("\nTrain Model Evaluation",
    "\nAccuracy: ",train_accuracy,
    "\nRecall:   ",train_recall,
    "\nPrecision:",train_precision,
    "\nF1-Score: ", train_f1score)

# 6.Testing model


# Testing model
test$probability <- predict(glm_model, newdata = test, type = "response")
test$predicted <- ifelse(test$probability >= 0.5, 1, 0)

# Test accuracy
cat("Test Accuracy : ",mean(test$churn == test$predicted) )

# 7. Test Model Evaluation - confusion matrix


# Test Model Evaluation - confusion matrix
train_matrix <- table(test$churn, test$predicted, dnn = c("Actual", "Prediected"))
print(train_matrix)
TP <- train_matrix[2,2]
FP <- train_matrix[1,2]
FN <- train_matrix[2,1]
TN <- train_matrix[1,1]

test_accuracy <- (TP + TN) / (TP+FP+FN+TN)
test_precision <- TP / (TP+FP)
test_recall <- TP / (TP+FN)
test_f1score <- 2*(test_precision*test_recall) / (test_precision+test_recall)
cat("\nTest Model Evaluation",
    "\nAccuracy: ",test_accuracy,
    "\nRecall:   ",test_recall,
    "\nPrecision:",test_precision,
    "\nF1-Score: ", test_f1score)

# **3.2 K-Mean Cluster Analysis**


# 1. Explore data


## Explore data
  #totalminutes and totalcharge
ggplot(churn_df, aes(totalminutes, totalcharge, color = churn)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm", color = "red")+
  theme_minimal()
  
ggplot(churn_df, aes(totalminutes, totalcharge, color = churn)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~churn)+
  theme_minimal()

# 2. Fit K-mean Model


# k = 2:5
# for loop k from 2 to 5
churn_df2 <- churn_df 
km_models <- list()
set.seed(42)
for (k in 2:5){
  model <- kmeans(churn_df2[, 1:21 ], centers = k)
  km_models[[k]] <- model
}

# 3. model result


## see model result
summary(km_models)

churn_df2 <- churn_df2 %>%
  mutate(k2 = km_models[[2]]$cluster,
         k3 = km_models[[3]]$cluster,
         k4 = km_models[[4]]$cluster,
         k5 = km_models[[5]]$cluster)

ggplot(churn_df2, aes(totalminutes, totalcharge, col = k2))+
  geom_point()+
  theme_minimal()
ggplot(churn_df2, aes(totalminutes, totalcharge, col = k3))+
  geom_point()+
  theme_minimal()
ggplot(churn_df2, aes(totalminutes, totalcharge, col = k4))+
  geom_point()+
  theme_minimal()
ggplot(churn_df2, aes(totalminutes, totalcharge, col = k5))+
  geom_point()+
  theme_minimal()

ggplot(churn_df2, aes(totalminutes, totalcharge, col = k3))+
  geom_point()+
  theme_minimal()+
  facet_wrap(~churn)
cat("0 = not churn",
    "\n1 = is churn") 

