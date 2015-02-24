options(stringsAsFactors = F)
library(lubridate)
library(randomForest)

featureEngineer <- function(data) {
  
  # datetime - hourly date + timestamp  
  data$datetime <- as.POSIXct(data$datetime)
  data$hour <- hour(data$datetime)
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

train2 <- featureEngineer(train)

test <- read.csv('Documents/Kaggle/bikesharing/data/test.csv')




train3 <- train2[, c(2:9, 12:14)]
fit <- lm(count~.-count, train3)

test2 <- featureEngineer(test)
test3 <- test2[, 2:ncol(test2)]

y_ <- predict(fit, newdata = test3)


