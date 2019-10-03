# Set Working Directory
setwd("C:/Users/yyj94/Desktop/펀다")

# Load Library
library(tidyverse)
library(xgboost)

# Load Data
load(file = 'data.Rdata')

# Check data
str(data)

# Change store_id character to factor
data$store_id <- as.factor(data$store_id)

# Change transacted_date factor to Date
data$transacted_date <- as.Date(data$transacted_date)

# Difference of day = 1002 days
start <- as.Date('2016-06-01')
end <- as.Date('2019-02-28')
end - start


# weekday, weekend
mon <- seq(from = as.Date("2016-06-06"), to = as.Date("2019-02-28"), by = 7)
tue <- seq(from = as.Date("2016-06-07"), to = as.Date("2019-02-28"), by = 7)
wen <- seq(from = as.Date("2016-06-01"), to = as.Date("2019-02-28"), by = 7)
thu <- seq(from = as.Date("2016-06-02"), to = as.Date("2019-02-28"), by = 7)
fri <- seq(from = as.Date("2016-06-03"), to = as.Date("2019-02-28"), by = 7)
sat <- seq(from = as.Date("2016-06-04"), to = as.Date("2019-02-28"), by = 7)
sun <- seq(from = as.Date("2016-06-05"), to = as.Date("2019-02-28"), by = 7)
weekday <- c(mon, tue, wen, thu, fri)
weekend <- c(sat, sun)

# All day
all <- seq(from = as.Date('2016-06-01'), to = as.Date('2019-02-28'), by = 1)

# Prediction period is 1003day
# So, separate 1003 by 91 and 92
start <- as.Date('2019-03-01')
end <- as.Date('2019-05-31')
end - start

# 1003 =  92 * 2 + 91 * 9 = 1003 
# make separate index
start <- c(1, 93, seq(from=185, to=length(all), by = 91), 1004)

# Number of store is 1967
num <- data$store_id %>% unique()

# Final DataFrame
final <- data.frame()
for(i in num) { 
  temp <- data.frame()
  for(j in 1:11) {
    day_len <- seq(from = all[start[j]], to = all[start[j+1]-1], by = 1)
    
    total <- data %>% 
      dplyr::filter((store_id == i) & (transacted_date %in% day_len)) 
    
    earn_mon <- total %>% 
      dplyr::filter(transacted_date %in% mon) %>% 
      summarise(amount_mon = sum(amount))
    
    earn_tue <- total %>% 
      dplyr::filter(transacted_date %in% tue) %>% 
      summarise(amount_tue = sum(amount))
    
    earn_wen <- total %>% 
      dplyr::filter(transacted_date %in% wen) %>% 
      summarise(amount_wen = sum(amount))
    
    earn_thu <- total %>% 
      dplyr::filter(transacted_date %in% thu) %>% 
      summarise(amount_thu = sum(amount))
    
    earn_fri <- total %>% 
      dplyr::filter(transacted_date %in% fri) %>% 
      summarise(amount_fri = sum(amount))
    
    earn_sat <- total %>% 
      dplyr::filter(transacted_date %in% sat) %>% 
      summarise(amount_sat = sum(amount))
    
    earn_sun <- total %>% 
      dplyr::filter(transacted_date %in% sun) %>% 
      summarise(amount_sun = sum(amount))
    
    result <- total %>% 
      summarise(SUM = sum(amount), MEAN = mean(amount), MEDIAN = median(amount), MEAN_install = mean(installment_term))
                
    result$MON = earn_mon$amount_mon 
    result$TUE = earn_tue$amount_tue 
    result$WEN = earn_wen$amount_wen
    result$THU = earn_thu$amount_thu 
    result$FRI = earn_fri$amount_fri 
    result$SAT = earn_sat$amount_sat
    result$SUN = earn_sun$amount_sun
    result$WEEKEND = (earn_sat$amount_sat + earn_sun$amount_sun)
    result$WEEKDAY = (earn_mon$amount_mon + earn_tue$amount_tue + earn_wen$amount_wen + earn_thu$amount_thu + earn_fri$amount_fri)
    
    result$store_id <- i
    
    temp <- rbind(temp, result)
  } 
  temp$SUM[1:10] <- temp$SUM[2:11]
  temp$SUM[11] <- 0
  
  final <- rbind(final, temp)
  
  cat(i, '번째 진행완료 \n')
}

# Check structure
str(final)

# Change store_id data type character to factor 
final$store_id <- as.factor(final$store_id)

# Test set = Before 3 month data want to predict each store
test <- final[seq(from=11, to=nrow(final), by=11), ]

#------ if before 3 month data has NA

# In test data, extract if data has NA
sum_zero_store_id <- test %>% 
  dplyr::filter(is.na(MEDIAN)) %>% 
  select(store_id)

# Select store_id
sum_zero_store_id <- sum_zero_store_id$store_id

# Treat NA missing values as mean
for(i in sum_zero_store_id) {
  a <- final %>% 
    dplyr::filter(store_id == 111) %>% 
    summarise(SUM = 0, MEAN = mean(MEAN, na.rm = TRUE), MEDIAN = median(MEDIAN, na.rm = TRUE), 
              MEAN_install = mean(MEAN_install, na.rm = TRUE),
              MON = mean(MON, na.rm = TRUE), TUE = mean(TUE, na.rm = TRUE), WEN = mean(WEN, na.rm = TRUE),
              THU = mean(THU, na.rm = TRUE), FRI = mean(FRI, na.rm = TRUE), SAT = mean(SAT, na.rm = TRUE),
              SUN = mean(SUN, na.rm = TRUE), WEEKEND = mean(WEEKEND, na.rm = TRUE), WEEKDAY = mean(WEEKDAY, na.rm = TRUE))
  
  test[test$store_id == i, ]$MEAN <- a$MEAN
  test[test$store_id == i, ]$MEDIAN <- a$MEDIAN
  test[test$store_id == i, ]$MEAN_install <- a$MEAN_install
  test[test$store_id == i, ]$MON <- a$MON
  test[test$store_id == i, ]$TUE <- a$TUE
  test[test$store_id == i, ]$WEN <- a$WEN
  test[test$store_id == i, ]$THU <- a$THU
  test[test$store_id == i, ]$THU <- a$THU
  test[test$store_id == i, ]$FRI <- a$FRI
  test[test$store_id == i, ]$SAT <- a$SAT
  test[test$store_id == i, ]$SUN <- a$SUN
  test[test$store_id == i, ]$WEEKEND <- a$WEEKEND
  test[test$store_id == i, ]$WEEKDAY <- a$WEEKDAY
}


# Exclude train data from final data
train <- final[-seq(from=11, to=nrow(final), by=11),]

# Exclue NA data from train data
train <- train %>% 
  dplyr::filter(!is.na(MEDIAN))


# Number of train data is 18511
nrow(train)

# Number of test data is 1967
nrow(test)

# Varaible setting 
yvar <- 'SUM'
Xvars <- c('MEAN', 'MEDIAN', 'MEAN_install', 'MON', 'TUE', 'WEN', 'THU', 'FRI', 'SAT', 'SUN', 'WEEKEND', 'WEEKDAY')


set.seed(123)
index <- sample(x = 1:2, 
                size = nrow(x = train), 
                prob = c(0.7, 0.3), 
                replace = TRUE) 

# if index == 1 allocate train data
train.data <- as.matrix(train[index == 1, Xvars])
train.label <- train[index == 1, yvar]

# if index == 2 allocate test data
test.data <- as.matrix(train[index == 2, Xvars])
test.label <- train[index == 2, yvar]

# Make xgb matrix
xgb.train <- xgb.DMatrix(data = train.data, label = train.label)
xgb.test <- xgb.DMatrix(data = test.data, label = test.label)

# Make real test data
real.test.data <- as.matrix(test[, Xvars])
real.test.label <- test[, yvar]
xgb.real_test <- xgb.DMatrix(data = real.test.data, label = real.test.label)

# Decide optimal round 
params <- list(booster = "gbtree", objective = "reg:linear", eta=0.2, gamma=1, max_depth=7, 
               min_child_weight=5, subsample=0.7, colsample_bytree=1)

params <- list(booster = "gblinear", objective = "reg:linear", eta=0.1, lambda=0.1, aplha=0.02154435)

xgbcv <- xgb.cv( params = params, data = xgb.train, nrounds = 100, nfold = 5, 
                 showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

min(xgbcv$best_iteration)


# 5-fold-cross_validation
cv.ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                      
  allowParallel = TRUE
)

# Make grid search parameter
xgb.grid <- expand.grid(nrounds = 200,
                        eta = c(0.1, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17),
                        max_depth = c(5, 6, 7),
                        gamma = c(0, 1),
                        colsample_bytree = c(0.5, 1),
                        min_child_weight = c(5, 7, 9),
                        subsample = c(0.5, 0.9, 1)
)

set.seed(321)

# Tuning grid search parameter
xgb_tune <- train(x = as.matrix(train %>% 
                                  select(Xvars)),
                  y = train$SUM,
                  method="xgbTree",
                  trControl=cv.ctrl,
                  tuneGrid=xgb.grid,
                  verbose=T,
                  metric="RMSE",
                  nthread =3
)

# Check final model
xgb_tune$finalModel

# Xgboost model training
xgb.fit <- xgb.train(
  booster = "gblinear",
  data = xgb.train, 
  objective = "reg:linear", 
  eval_metric = "mae",
  verbose = 1,
  eta = 0.1,
  alpha = 0.02154435,
  lambda = 0.1,
  gamma = 0,
  nrounds = 100,
  max_depth = 6,
  colsample_bytree = 1,
  min_child_weight = 5,
  subsample = 0.9,
  watchlist = list(val = xgb.test, train = xgb.train)
)


xgbpred <- predict(xgb.fit, xgb.real_test)

data.frame(xgbpred, submission$amount)

submission$amount <- xgbpred

write.csv(submission, file="submission.csv", row.names = FALSE)  

submission <- read.csv("submission.csv", header = TRUE)  

submission$amount <- xgbpred

write.csv(submission, file="submission.csv", row.names = FALSE)  

