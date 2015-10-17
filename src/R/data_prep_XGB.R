setwd("/Volumes/HDD/Kaggle/Rossman")
library(readr)
library(randomForest)
library(sqldf)
library(caret)
library(mlbench)

set.seed(616)

cat("reading the train and test data\n")
train0 <- read_csv("train.csv")
test0  <- read_csv("test.csv")
store0 <- read_csv("store.csv")

train <- merge(train0,store0)
test <- merge(test0,store0)

# seperating out the elements of the date column for the train set
train$month <- as.integer(format(train$Date, "%m"))
train$year <- as.integer(format(train$Date, "%y"))
train$day <- as.integer(format(train$Date, "%d"))
train$quarter <- ceiling(train$month/3)

# Remove date column
train <- train[,-3]

# seperating out the elements of the date column for the test set
test$month <- as.integer(format(test$Date, "%m"))
test$year <- as.integer(format(test$Date, "%y"))
test$day <- as.integer(format(test$Date, "%d"))
test$quarter <- ceiling(test$month/3)

# Remove date column
test <- test[,-4]

# Print character vars
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    cat(paste0("Converting ", f, " to factor\n"))
    # levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- factor(train[[f]])
    test[[f]]  <- factor(test[[f]])
  }
}

cat(paste0("Converting Store to factor\n"))
train$Store <- factor(train$Store)
test$Store <- factor(test$Store)

cat(paste0("Converting DayOfWeek to factor\n"))
train$DayOfWeek <- factor(train$DayOfWeek)
test$DayOfWeek <- factor(test$DayOfWeek)

cat(paste0("Converting month to factor\n"))
train$month <- factor(train$month)
test$month <- factor(test$month)

cat(paste0("Converting day to factor\n"))
train$day <- factor(train$day)
test$day <- factor(test$day)

cat(paste0("Converting quarter to factor\n"))
train$quarter <- factor(train$quarter)
test$quarter <- factor(test$quarter)

# Generate lags
annualSales <- sqldf("select Store, year, sum(Sales) as annSales
                     from train
                     group by Store, year
                     ")
annualMonthSales <- sqldf("select Store, year, month, sum(Sales) as annMonthSales
                      from train
                      group by Store, year, month
                      ")
annualQSales <- sqldf("select Store, year, quarter, sum(Sales) as annQSales
                     from train
                     group by Store, year, quarter
                     ")
monthyAvg <- sqldf("select Store, month, avg(Sales) as monthlyAvg
                     from train
                     group by Store, month
                     ")
QAvg <- sqldf("select Store, quarter, avg(Sales) as QAvg
                     from train
                   group by Store, quarter
                   ")

## Merge with training dataset
train<-sqldf("SELECT a.*, b.annSales as PriorYearSales
                 FROM train a 
                 LEFT JOIN annualSales b ON a.Store = b.Store AND a.year = b.year +1
             ")
train<-sqldf("SELECT a.*, b.annMonthSales as PriorYearMonthSales
                 FROM train a 
             LEFT JOIN annualMonthSales b ON a.Store = b.Store AND a.year = b.year +1 and a.month = b.month
             ")
train<-sqldf("SELECT a.*, b.annQSales as PriorYearQSales
                 FROM train a 
             LEFT JOIN annualQSales b ON a.Store = b.Store AND a.year = b.year +1 and a.quarter = b.quarter
             ")
train<-sqldf("SELECT a.*, b.monthlyAvg as monthlyAvgSales
                 FROM train a 
             LEFT JOIN monthyAvg b ON a.Store = b.Store and a.month = b.month
             ")
train<-sqldf("SELECT a.*, b.QAvg as QAvgSales
             FROM train a 
             LEFT JOIN QAvg b ON a.Store = b.Store AND a.quarter = b.quarter
             ")

## Merge with testing dataset
test<-sqldf("SELECT a.*, b.annSales as PriorYearSales
             FROM test a 
             LEFT JOIN annualSales b ON a.Store = b.Store AND a.year = b.year +1
             ")
test<-sqldf("SELECT a.*, b.annMonthSales as PriorYearMonthSales
             FROM test a 
             LEFT JOIN annualMonthSales b ON a.Store = b.Store AND a.year = b.year +1 and a.month = b.month
             ")
test<-sqldf("SELECT a.*, b.annQSales as PriorYearQSales
             FROM test a 
             LEFT JOIN annualQSales b ON a.Store = b.Store AND a.year = b.year +1 and a.quarter = b.quarter
             ")
test<-sqldf("SELECT a.*, b.monthlyAvg as monthlyAvgSales
             FROM test a 
             LEFT JOIN monthyAvg b ON a.Store = b.Store and a.month = b.month
             ")
test<-sqldf("SELECT a.*, b.QAvg as QAvgSales
             FROM test a 
             LEFT JOIN QAvg b ON a.Store = b.Store AND a.quarter = b.quarter
             ")

feature.names <- names(train)[c(2, 5:26)]
cat("Feature Names\n")
feature.names

cat("checking all stores are accounted for\n")
length(unique(train$Store))

# Create holdout
inTraining <- createDataPartition(train$Sales, p = .7, list = FALSE)
training <- train[inTraining,c("Sales", feature.names)]
testing  <- train[-inTraining,c("Sales", feature.names)]

# RMPSE eval function
RMPSE<- function(data, lev = NULL, model = NULL) {
  elab <- exp(as.numeric(data$obs))-1
  epreds <- exp(as.numeric(data$pred))-1
  err <- sqrt(mean((epreds/elab-1)^2))
  names(err) <- c("RMPSE")
  err
}

# train on log
# split into customers and avg $ per customer?

rfFit0 <- train(Sales ~ ., data = training,method="rf",
                trControl=trainControl(method="cv",number=3)
                #,prox=TRUE
                #,allowParallel=TRUE
                )
rfFit0

xgbFit0 <- train(Sales ~ ., data = training,method="xgbTree",
                trControl=trainControl(method="cv",number=3)
)
xgbFit0

predictions <- predict(xgbFit0, testing)

length(predictions)
length(testing[,1])

nrow(predictions)
length(training[,1])

# summarize results
confusionMatrix(predictions$Sales, training$Sales)

models <- list(gbm = gbmFit0)
pred <- extractPrediction(models, testX = testing[,-1], testY = testing[, 1])
OOS <-data.frame(obs=testing$Sales, pred=predict(gbmFit0, newdata = testing))
