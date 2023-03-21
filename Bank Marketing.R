library(caret)
library(tidyverse)
library(dplyr)
library(purrr)

#DataVis

data_job <- bank_full %>% group_by(job) %>% summarize(n = n(), count = sum(y=="yes")) 
data_job <- data.frame(job = data_job$job, count = data_job$count) %>% mutate(job = reorder(job, count))
ggplot(data_job, aes(x=job, y=count)) + geom_bar(stat="identity")

data_edu <- bank_full %>% group_by(education) %>% summarize(n = n(), count = sum(y=="yes"))
data_edu <- data.frame(education = data_edu$education, count = data_edu$count) %>% mutate(education = reorder(education, count))
ggplot(data_edu, aes(x=education, y=count)) + geom_bar(stat="identity")

data_mar <- bank_full %>% group_by(marital) %>% summarize(n = n(), count = sum(y=="yes"))
data_mar <- data.frame(marital = data_mar$marital, count = data_mar$count) %>% mutate(marital = reorder(marital, count))
ggplot(data_mar, aes(x=marital, y=count)) + geom_bar(stat="identity")

data_age <- bank_full %>% group_by(age) %>% summarize(n=n(), count = sum(y=="yes"))
data_age <- data.frame(age = data_age$age, count = data_age$count) 
ggplot(data_age, aes(x=age, y=count)) + geom_bar(stat = "identity")

data_month <- bank_full %>% group_by(month) %>% summarize(n=n(), count = sum(y=="yes"))
data_month <- data.frame(month = data_month$month, count = data_month$count) 
ggplot(data_month, aes(x=month, y=count)) + geom_bar(stat = "identity")


house <- bank_full %>% group_by(housing) %>% summarize(n=n(), count = sum(y=="yes"))
house <- data.frame(housing = house$housing, count = house$count) %>% mutate(housing = reorder(housing, count))
ggplot(house, aes(x=housing, y=count)) + geom_bar(stat = "identity")

loans <- bank_full %>% group_by(loan) %>% summarize(n=n(), count = sum(y=="yes"))
loans <- data.frame(loan = loans$loan, count = loans$count) %>% mutate(loans = reorder(loans, count))
ggplot(loans, aes(x=loan, y=count)) + geom_bar(stat = "identity")

 ###More DataVis

bank_full %>% ggplot(aes(x=job, y=balance, fill=y)) + geom_boxplot()

bank_full %>% ggplot(aes(x=education, y=balance, fill=y)) + geom_boxplot()

bank_full %>% ggplot(aes(x=job, y=age)) + geom_boxplot()

#models


set.seed(2020, sample.kind="Rounding")
y <- bank_full$y
test_index <- createDataPartition(y, times = 1, p = 0.25, list = FALSE)
test_set <- bank_full[test_index,]
train_set <- bank_full[-test_index,]

train_set_numeric <- train_set %>% mutate(y = ifelse(y=="yes",1,0)) %>% mutate(y = factor(y))
test_set_numeric <- test_set %>% mutate(y = ifelse(y=="yes",1,0)) %>% mutate(y = factor(y))

train_rpart1 <- train(y ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data=train_set_numeric)
confusionMatrix(predict(train_rpart, test_set_numeric), test_set_numeric$y)$overall["Accuracy"]
plot(train_rpart1$finalModel, margin = 0.1)
text(train_rpart1$finalModel, cex = 0.75)



#Cleaning Data
train_set_numeric$duration <- NULL
test_set_numeric$duration <- NULL
train_set_numeric$poutcome <- NULL
test_set_numeric$poutcome <- NULL


# LDA
set.seed(1, sample.kind="Rounding")
train_lda <- train(y ~ ., method = "lda", data = train_set_numeric)
confusionMatrix(predict(train_lda, test_set_numeric), test_set_numeric$y)$overall["Accuracy"]

#QDA
set.seed(1, sample.kind="Rounding")
train_qda <- train(y~., method = "qda", data=train_set_numeric)
confusionMatrix(predict(train_qda, test_set_numeric), test_set_numeric$y)$overall["Accuracy"]
#GLM
set.seed(1, sample.kind="Rounding")
train_glm <- train(y~., method="glm", data=train_set_numeric)
confusionMatrix(predict(train_glm, test_set_numeric), test_set_numeric$y)$overall["Accuracy"]

#Rpart
set.seed(10, sample.kind="Rounding")
train_rpart2 <- train(y ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data=train_set_numeric)
confusionMatrix(predict(train_rpart2, test_set_numeric), test_set_numeric$y)$overall["Accuracy"]
plot(train_rpart2$finalModel, margin = 0.1)
text(train_rpart2$finalModel, cex = 0.75)

varImp(train_rpart2)

