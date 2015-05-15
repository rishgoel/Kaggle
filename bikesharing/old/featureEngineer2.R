options(stringsAsFactors = F)
library(lubridate)
library(randomForest)

# READ DATA FILES
train <- read.csv('Documents/Kaggle/bikesharing/data/train.csv')
test <- read.csv('Documents/Kaggle/bikesharing/data/test.csv')

# FEATURE ENGINEERING
featureEngineer <- function(data) {
  # datetime - hourly date + timestamp  
  data$datetime <- as.POSIXct(data$datetime)
  data$yr <- factor(year(data$datetime))
  data$mo <- factor(month(data$datetime))
  data$hr <- hour(data$datetime)
  
  data$daypart <- 1
  data[data$hr >= 4 & data$hr < 10, "daypart"] <- 2
  data[data$hr >= 10 & data$hr < 16, "daypart"] <- 3
  data[data$hr >= 16 & data$hr < 22, "daypart"] <- 4
  data$daypart <- factor(data$daypart)
  
  data$hr <- factor(data$hr)
  
  data$weekday <- factor(weekdays(data$datetime))
  data$sunday <- 0
  data[data$weekday == 'Sunday', 'sunday'] <- 1
  data$sunday <- factor(data$sunday)
  
  # season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
  data$season <- factor(data$season)
  
  # holiday - whether the day is considered a holiday
  data$holiday <- factor(data$holiday)
  # workingday - whether the day is neither a weekend nor holiday
  data$workingday <- factor(data$workingday)
  # weather - 1: Clear, Few clouds, Partly cloudy, Partly cloudy 
  #           2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
  #           3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds 
  #           4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
  data$weather <- factor(data$weather)
  
  # temp - temperature in Celsius
  # atemp - "feels like" temperature in Celsius
  # humidity - relative humidity
  # windspeed - wind speed
  # casual - number of non-registered user rentals initiated
  # registered - number of registered user rentals initiated
  # count - number of total rentals
  
  
  return(data)
  
}

train2 <- featureEngineer(train)

# FEATURE SELECTION
train2_fs_sample <- sample(1:nrow(train2),nrow(train2)*.6)
train2_fs_train <- train2[train2_fs_sample,]
train2_fs_test <- train2[-train2_fs_sample,]

features <- names(train2[, !names(train2) %in% c("datetime", "casual", "registered", "count")])

feature_log <- list()
mse_log <- c()
log <- 1
for(n in length(features):5) {
  print(n)
  features_combn <- combn(features, n)
  if(n == 14) {
    # TRAIN
    df <- train2_fs_train[, c(features_combn, "count")]    
    fit <- (lm(count~., df))
    # TEST
    df_test <- train2_fs_test[, c(features_combn, "count")]    
    y_predict <- predict(fit, newdata=df_test[, -ncol(df_test)])
    # MSE
    comparison <- data.frame(y_actual = df_test$count, y_predict = y_predict)   
    comparison[comparison$y_predict < 0, "y_predict"] <- 0
    # Log observations
    feature_log[[log]] <- features_combn
    rmse_log[log] <- sum((comparison$y_predict - comparison$y_actual)^2)/nrow(comparison)
    log <- log + 1    
  } else {    
    for(i in 1:dim(features_combn)[2]) {
      # TRAIN
      df <- train2_fs_train[, c(features_combn[,i], "count")]    
      fit <- (lm(count~., df))
      # TEST
      df_test <- train2_fs_test[, c(features_combn[,i], "count")]    
      y_predict <- predict(fit, newdata=df_test[, -ncol(df_test)])
      # MSE
      comparison <- data.frame(y_actual = df_test$count, y_predict = y_predict)   
      comparison[comparison$y_predict < 0, "y_predict"] <- 0
      # Log observations
      feature_log[[log]] <- features_combn[,i]
      rmse_log[log] <- sum((comparison$y_predict - comparison$y_actual)^2)/nrow(comparison)
      log <- log + 1
    }  
  }  
}










set.seed(4000)
fit_casual <- randomForest(casual~hr+yr+humidity+workingday+temp, train2, ntree=500, mtry=5, importance=T)
fit_reg <- randomForest(registered~hr+yr+humidity+workingday+temp, train2, ntree=500, mtry=5, importance=T)


#fit_casual <- lm(casual~hr+yr+humidity+workingday+temp, train2)
#fit_reg <- lm(registered~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+yr+mo+hr+weekday+daypart, train2)


fit_count <- randomForest(count~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+yr+mo+hr+weekday+daypart, train2, ntree=500, mtry=5, importance=T)


test2 <- featureEngineer(test)
test3 <- test2[, 2:ncol(test2)]


test$casual <- predict(fit_casual, newdata = test3)
test$registered <- predict(fit_reg, newdata = test3)

test$count <- round(test$casual + test$registered)


write.csv(test[, c("datetime", "count")], 'Documents/Kaggle/bikesharing/20150224_submission2.csv', row.names = F)


test$count <- predict(fit_count, newdata = test3)
