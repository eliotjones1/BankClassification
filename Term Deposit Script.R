## Setup
temp <- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip",temp, mode="wb")
unzip(temp, "bank-full.csv")
unlink(temp)
bank.df <- read.table("bank-full.csv", sep=";", header=T)
str(bank.df)
bank_full <- bank.df

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("data.table", repos = "http://cran.us.r-project.org")

## Basics
mean(bank_full$age)
sd(bank_full$age)

mean(bank_full$balance)
sd(bank_full$balance)

## Percentage Plots

data_job <- bank_full %>% group_by(job) %>% summarize(n = n(), percent = (sum(y=="yes")/n)*100)
data_job <- data.frame(job = data_job$job, percent = data_job$percent) %>% mutate(job = reorder(job, percent))
ggplot(data_job, aes(x=job, y=percent)) + geom_bar(stat="identity")

data_age <- bank_full %>% group_by(age) %>% summarize(n=n(), percent = (sum(y=="yes")/n*100))
data_age <- data.frame(age = data_age$age, percent = data_age$percent)
ggplot(data_age, aes(x=age, y=percent)) + geom_bar(stat = "identity")

data_mar <- bank_full %>% group_by(marital) %>% summarize(n = n(), percent = (sum(y=="yes")/n)*100)
data_mar <- data.frame(marital = data_mar$marital, percent = data_mar$percent) %>% mutate(marital = reorder(marital, percent))
ggplot(data_mar, aes(x=marital, y=percent)) + geom_bar(stat="identity")

data_month <- bank_full %>% group_by(month) %>% summarize(n=n(), percent = (sum(y=="yes")/n)*100)
data_month <- data.frame(month = data_month$month, percent = data_month$percent) 
ggplot(data_month, aes(x=month, y=percent)) + geom_bar(stat = "identity")

data_edu <- bank_full %>% group_by(education) %>% summarize(n = n(), percent = (sum(y=="yes")/n)*100)
data_edu <- data.frame(education = data_edu$education, percent = data_edu$percent) %>% mutate(education = reorder(education, percent))
ggplot(data_edu, aes(x=education, y=percent)) + geom_bar(stat="identity")

## Count Plots

data_job <- bank_full %>% group_by(job) %>% summarize(n = n(), count = sum(y=="yes")) 
data_job <- data.frame(job = data_job$job, count = data_job$count) %>% mutate(job = reorder(job, count))
ggplot(data_job, aes(x=job, y=count)) + geom_bar(stat="identity")

data_age <- bank_full %>% group_by(age) %>% summarize(n=n(), count = sum(y=="yes"))
data_age <- data.frame(age = data_age$age, count = data_age$count) 
ggplot(data_age, aes(x=age, y=count)) + geom_bar(stat = "identity")

data_mar <- bank_full %>% group_by(marital) %>% summarize(n = n(), count = sum(y=="yes"))
data_mar <- data.frame(marital = data_mar$marital, count = data_mar$count) %>% mutate(marital = reorder(marital, count))
ggplot(data_mar, aes(x=marital, y=count)) + geom_bar(stat="identity")

data_month <- bank_full %>% group_by(month) %>% summarize(n=n(), count = sum(y=="yes"))
data_month <- data.frame(month = data_month$month, count = data_month$count) 
ggplot(data_month, aes(x=month, y=count)) + geom_bar(stat = "identity")

data_edu <- bank_full %>% group_by(education) %>% summarize(n = n(), count = sum(y=="yes"))
data_edu <- data.frame(education = data_edu$education, count = data_edu$count) %>% mutate(education = reorder(education, count))
ggplot(data_edu, aes(x=education, y=count)) + geom_bar(stat="identity")

## Housing and Loan Plots

house <- bank_full %>% group_by(housing) %>% summarize(n=n(), count = sum(y=="yes"))
house <- data.frame(housing = house$housing, count = house$count) %>% mutate(housing = reorder(housing, count))
ggplot(house, aes(x=housing, y=count)) + geom_bar(stat = "identity")

l1s <- bank_full %>% group_by(loan) %>% summarize(n=n(), count = sum(y=="yes"))
l1s <- data.frame(loan = l1s$loan, count = l1s$count) %>% mutate(loan = reorder(loan, count))
ggplot(l1s, aes(x=loan, y=count)) + geom_bar(stat = "identity")

## Boxplots with Age and Balance

bank_full %>% ggplot(aes(x=job, y=balance, fill=y)) + geom_boxplot(position=position_dodge(1))

bank_full %>% ggplot(aes(x=education, y=balance, fill=y)) + geom_boxplot(position=position_dodge(1))

bank_full %>% ggplot(aes(x=job, y=age)) + geom_boxplot(position=position_dodge(1))

## Data Partitioning

set.seed(2020, sample.kind="Rounding")
y <- bank_full$y
test_index <- createDataPartition(y, times = 1, p = 0.25, list = FALSE)
test_set <- bank_full[test_index,]
train_set <- bank_full[-test_index,]

train_set_numeric <- train_set %>% mutate(y = ifelse(y=="yes",1,0)) %>% mutate(y = factor(y))
test_set_numeric <- test_set %>% mutate(y = ifelse(y=="yes",1,0)) %>% mutate(y = factor(y))

## Rpart with Duration

train_rpart1 <- train(y ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data=train_set_numeric)
confusionMatrix(predict(train_rpart, test_set_numeric), test_set_numeric$y)$overall["Accuracy"]
plot(train_rpart1$finalModel, margin = 0.1)
text(train_rpart1$finalModel, cex = 0.75) 

# Algorithms

#Cleaning Data
train_set_numeric$duration <- NULL
test_set_numeric$duration <- NULL
train_set_numeric$poutcome <- NULL
test_set_numeric$poutcome <- NULL

# LDA
set.seed(1, sample.kind="Rounding")
train_lda <- train(y ~ ., method = "lda", data = train_set_numeric)

#QDA
set.seed(1, sample.kind="Rounding")
train_qda <- train(y~., method = "qda", data=train_set_numeric)
#GLM
set.seed(1, sample.kind="Rounding")
train_glm <- train(y~., method="glm", data=train_set_numeric)

#Rpart
set.seed(10, sample.kind="Rounding")
train_rpart2 <- train(y ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data=train_set_numeric)


#Accuracy
confusionMatrix(predict(train_lda, test_set_numeric), test_set_numeric$y)$overall["Accuracy"]
confusionMatrix(predict(train_qda, test_set_numeric), test_set_numeric$y)$overall["Accuracy"]
confusionMatrix(predict(train_glm, test_set_numeric), test_set_numeric$y)$overall["Accuracy"]
confusionMatrix(predict(train_rpart2, test_set_numeric), test_set_numeric$y)$overall["Accuracy"]

## Rpart Confusion Matrix

confusionMatrix(predict(train_rpart2, test_set_numeric), test_set_numeric$y)

## varImp and Decision Tree

varImp(train_rpart2)
plot(train_rpart2$finalModel, margin = 0.1)
text(train_rpart2$finalModel, cex = 0.75) 

## Rpart Precision
confusionMatrix(predict(train_rpart2, test_set_numeric), test_set_numeric$y)$byClass["Precision"]

