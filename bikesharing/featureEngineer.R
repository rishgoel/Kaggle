options(stringsAsFactors = F)
library(lubridate)
library(randomForest)

featureEngineer <- function(data) {
  
  # datetime - hourly date + timestamp  
  data$datetime <- as.POSIXct(data$datetime)
  data$yr <- factor(year(data$datetime))
  data$mo <- factor(month(data$datetime))
  data$hr <- factor(hour(data$datetime))
  data$weekday <- factor(weekdays(data$datetime))
  
  # season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
  data$season <- factor(data$season)
  
  # holiday - whether the day is considered a holiday
  # workingday - whether the day is neither a weekend nor holiday
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




train <- read.csv('Documents/Kaggle/bikesharing/data/train.csv')
test <- read.csv('Documents/Kaggle/bikesharing/data/test.csv')

train2 <- featureEngineer(train)

set.seed(4000)
fit_casual <- randomForest(casual~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+yr+mo+hr+weekday, train2, ntree=500, mtry=5, importance=T)
fit_reg <- randomForest(registered~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+yr+mo+hr+weekday, train2, ntree=500, mtry=5, importance=T)


test2 <- featureEngineer(test)
test3 <- test2[, 2:ncol(test2)]


test$casual <- predict(fit_casual, newdata = test3)
test$registered <- predict(fit_reg, newdata = test3)

test$count <- round(test$casual + test$registered)


write.csv(test[, c("datetime", "count")], 'Documents/Kaggle/bikesharing/20150224_submission1.csv', row.names = F)
