### Bike Share Rental Analysis

**Siyuan Li, ID: 904884144**

***

#### 1 Introduction

Ride sharing companies like Uber and Lyft are great business models that provide convenient, affordable and efficient transportation options for customers who want to go to places without the hassle of owning or operating a vehicle. However, with the increasing number of automobiles, riding sharing in cars are not efficient enough especially in crowded and busy areas like cities' downtown. Therefore, bike sharing is a brilliant idea which provides people with another short range transportation option that allows them to travel without worrying about being stuck in traffic and maybe enjoy city view or even workout at the same. In fact, bike sharing programs in the United States started about 15 years before Uber's ride share program started. 

In this project for the UCLA's "Statistics 404 Statistical Computation and Programming" course, I will be investigating into the bike share rental data from "Capital Bikeshare" servicing Washington D.C. and surrounding areas beginning 2010. Capital Bikeshare was the largest bike sharing service in the United States when they started until Citi Bike for New York City started operations in 2013. Capital Bikeshare started from 10 stations and 120 bicycles in Washington D.C. and expanding into a bike share system that owns more than 429 stations and 2500 bicycles and also services Arlington County, Fairfax County, City of Alexandria in Virginia and Montgomery County in Maryland. 

My objective of the analysis is to find out the determining factor that drives the demand on bike share rentals, construct statistical models and then try to make prediction on rentals based on the information and models I have. My exploration and the analysis of the data will be performed in R, with a few functions written in C, as per requested.



#### 2 About the Data

The data I will be look into is downloaded and extracted from [Kaggle](https://www.kaggle.com/c/bike-sharing-demand/data). This bike share rental data of Capital Bikeshare only contains entries sampled from Washington D.C. spanning two years dating from January 1st, 2011 to December 19th, 2012. The dataset is also joined by the weather statistics for the corresponding date and time.

Due to being a competition dataset, complete data was divided into training set, containing only the entries from the 1st of every month to the 19th, and testing set, containing entries from the 20th to the end of month excluding some important predictor variables. In the data exploration and analysis, I will be using the training set for complete features and predictor variable.

​The resulting dataset I will be using contains 10886 observations and 12 variables. 

The variables are: 

- "datetime", containing hourly date in timestamp format; 
- "season", containing integers 1 to 4 representing "Winter", "Spring","Summer","Fall"; 
- "holiday", containing Boolean expressions in 1s and 0s representing whether the day of the observation is a holiday or not; 
- "workingday", containing Boolean expressions in 1s and 0s representing whether the day of the observation is a working day or not; 
- "weather", containing integers 1 to 4 representing four different lists of weather conditions:
  - 1: Clear or cloudy,
  - 2: Mists,
  - 3: Light rain or snow,
  - 4: Heavy rain, snow or even worse weather.
- "temp", containing values of temperature at the given time;
- "atemp", containing values of feeling temperature at the given time;
- ​"humidity", containing values of relative humidity level at the given time, in the scale of 1 to 100;
- "windspeed", containing values of wind speed, in mph (miles per hour);
- "casual", containing the count of non-registered user rentals, across all stations;
- "registered", containing the count of registered user rentals, across all stations;
- "count", containing the total count of rentals at the given hour, across all stations.

##### 2.1 Data Cleaning

A preliminary data cleaning is performed, converting hourly date variable to months, day of the week, and hour of the day. I also convert "holiday", "workingday", "weather" to factors to better represent their categorical nature. I only keep the "temp" variable and removed "atemp" variable since it is almost repetitive and not a relatively accurate statistic to acquire. I also remove the "casual" and "registered" variable from the dataset because they sum up to "count" and my analysis later will not use them.

​Upon examining the data, there appears to have values in the form of 0.0000s in the wind speed variable, I decided to simply remove the observations with these values as missing values because they occur randomly during the hour 0 to 6. My reason for removing these missing values instead of substituting them with other balancing values (such as mean of wind speed of the day) because I expect them to be relatively random values and replacing them with set values will cause inaccuracy in my analysis later.

​There are no other abnormalities that is present in the dataset. The result of data cleaning is a dataset with 9573 observations and 11 variables. A `head()` function output **(Figure 01)** can give an idea how the data structures after cleaning.



#### 3 Exploratory Data Analysis

Without building any model or making any predictions, lets first look at the data by itself.

I construct a data frame that summarizes the bike rental count base on the season, month, day of the week, hour of the day, is it a weekday, is it a holiday, and the type of weather, then calculating the mean of temperature, humidity, wind speed and rental count. The purpose of this summarization is to find a general relationship between variables regardless of which year the data is from (since the data spans two years and the business is growing.) 

##### 3.1 Visualization

Using the summarized data frame, we can visualize some of the features of the data without looking at a complex summary statistics. 

The boxplot of different seasons against bike rental count reveals that there is a seasonal trend with the rental count. Rental count is generally low in Winter and it peaks in Summer. Season can be one of the determining factors that affects bike rental count. **(Figure 02)**

​The line plot of hour of the day against bike rental count categorize by day of the week shows the difference of rental demand for weekday and weekend in different hours. The rental count remains active later in the midnight during the weekend than weekday. We can also see that the bike rental count has a dip at around 12 P.M. during weekdays, whereas around the same time during weekends shows peak of demand of the day. The peak of demand during weekdays is around 4 to 5 P.M. in the afternoon, possibly due to people are done working and in need of transportation to go home. **(Figure 03)**

Making a boxplot comparing holiday effects shows that the average amounts of rental are about the same regardless of being a holiday or not. Due to having a smaller sample size for holidays, the range of rental count is generally smaller than that of non-holidays. We can also see similar seasonality to the season v.s. rental count boxplot; Winter shows the lowest in rental count and Summer shows the highest. **(Figure 04)**

Plotting different type of weather against bike rental count indicates that the demand of bike rental is about the same in clear, cloudy or misty hours, with better overall count in better weather. Rainy and snowy days show significantly lower average rental count. I believe our dataset do not contain observations in really bad weathers so the boxplot for weather type 4 is a placeholder and do not have significant meaning. **(Figure 05)**

I also plot temperature, humidity, and wind speed against bike rental count categorized by different weather types. 

The temperature plot shows that generally, the warmer the temperature, the higher bike rental demand. However, in clear, cloudy, light rain or snow days, the rental count peaks at around 32 degrees Celsius; where as misty days, the rental demand peaks higher around 36 degrees Celsius. **(Figure 06)**

The humidity plot shows that generally, the higher the relative humidity, the lower bike rental demand. Although the curve for light rain or snow shows concavity, it is due to the smoothing of the data point by the ggplot function in R. **(Figure 07)**

The wind speed plot shows that although people enjoy gentle breeze in good weathers, the bike rental demand is significantly lower no matter the wind speed in light rain or snow weathers. **(Figure 08)**

From the previous plots, observe that there might be high correlations between different variables, namely, season and month, season and weather type, etc.. 

##### 3.2 Correlation

By preparing a correlation matrix, we can have a more straight forward view of what variables are strongly correlated and what is weakly correlated. **(Figure 09)**

We can clearly see from the matrix that hour and temperature has the strongest correlation to bike rental count while all variables considered. However, hour and temperature has a relatively high correlation between each other. We can disregard the significantly high correlation between season and month since it is only natural for them to have high correlation.

##### 3.3 Kernel Regression





#### 4 Model Building 

##### 4.1 Linear Model



##### 4.2 Generalized Linear Model

4.2.1 Poisson Count Statistics



4.2.2 Generalized Linear Model



##### 4.3 Generalized Addictive Model





#### 5 Prediction

##### 4.1 Linear Model



##### 4.2 Generalized Linear Model

4.2.1 Poisson Count Statistics



4.2.2 Generalized Linear Model



##### 4.3 Generalized Addictive Model





#### 6 Conclusion



#### Appendices

##### Figures

**01 head() of data**

![00](./plots/00_head.GIF)

**02 Boxplot, Season v.s. Rental Count**

![01](./plots/01_bike_rentals_by_season.png)

**03 Line plot, Hour of the Day, by Day of the Week v.s. Rental Count**

![02](./plots/02_bike_rentals_by_weekday_and_hour.png)

**04 Boxplot, Holiday, by Season v.s. Rental Count**

![03](./plots/03_bike_rentals_by_holiday_and_season.png)

**05 Boxplot, Type of Weather v.s. Rental Count**

![04](./plots/04_bike_rentals_by_weather.png)

**06 Line plot, Temperature, by type of weather v.s. Rental Count**

![05](./plots/05_bike_rentals_by_temperature_and_weather.png)

**07 Line plot, Humidity, by type of weather v.s. Rental Count**

![06](./plots/06_bike_rentals_by_humidity_and_weather.png)

**08 Line plot, Wind Speed, by type of weather v.s. Rental Count**

![07](./plots/07_bike_rentals_by_windspeed_and_weather.png)

**09 Correlation Matrix**

![001](./plots/00_correlation_matrix.png)

**10**



11



12



13



14



15



16





##### Code

R Code:

```R
### loading library ###
library(ggplot2)
library(plyr)
library(gam)
library(reshape)

### loading data ###
bike <- read.csv("./alternative_data/train.csv")
month <- as.integer(format(as.POSIXlt(bike.new$datetime), format = "%m"))
weekday <- as.integer(format(as.POSIXlt(bike.new$datetime), format = "%u"))
hour <- as.integer(format(as.POSIXlt(bike.new$datetime), format = "%H"))
bike <- data.frame(bike$season, month, weekday, hour, as.factor(bike$workingday), as.factor(bike$holiday), 
                   as.factor(bike$weather), bike$temp, bike$hum, bike$windspeed, bike$count)
names(bike) <- c("season", "month", "weekday", "hour", "isweekday", 
                 "isholiday", "weathertype", "temperature", "humidity", "windspeed", "count")

bike <- bike[which(bike$windspeed != 0.0000),]
head(bike, 5)
count.summary <- ddply(bike,.(season, month, weekday, hour, isweekday, isholiday, weathertype), summarise, 
                       temperature = mean(temperature), 
                       humidity = mean(humidity), 
                       windspeed = mean(windspeed), 
                       count = mean(count))
head(count.summary)

### assesments ###

# boxplot of rental v.s. season 
ggplot(count.summary, aes(x = season, y = count, fill = factor(season))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values = c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
                    name="Season:",
                    breaks=c(1, 2, 3, 4),
                    labels=c("Winter", "Spring", "Summer", "Fall"))

# line plot of rentals v.s. hour of day
ggplot(count.summary, aes(x = hour, y = count, color = as.factor(weekday))) +
  geom_smooth(method = "loess", fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("Hour of the Day") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_color_discrete("") +
  theme(plot.title = element_text(size = 11, face="bold"))


# boxplot of rental v.s. holiday 
ggplot(count.summary, aes(x = isholiday, y = count, fill = factor(season))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Is it Holiday?") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
                    name="Season:",
                    breaks=c(1, 2, 3, 4),
                    labels=c("Winter", "Spring", "Summer", "Fall")) +
  theme(plot.title = element_text(size = 11, face="bold"))

# boxplot of rentals v.s. weather
ggplot(count.summary, aes(x = weathertype, y = count, fill = weathertype)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Type of Weather") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values = c("#E74C3C", "#58D68D", "#5DADE2", "#F4D03F"), 
                    name = "Type of Weather:",
                    breaks = c(1, 2, 3, 4),
                    labels = c("\n Clear or Cloudy \n", 
                             "\n Mist \n", 
                             "\n Light Rain \n or Light Snow \n", 
                             "\n BAD WEATHER \n")) +
  theme(plot.title = element_text(size = 11, face="bold"))

# line plot of rental v.s. temperature
ggplot(count.summary, aes(x = temperature, y = count, color = weathertype)) +
  geom_smooth(fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("Temperature") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_color_discrete(name = "Type of Weather:",
                       breaks = c(1, 2, 3, 4),
                       labels = c("Clear or Cloudy", 
                                  "Mist", 
                                  "Light Rain or Snow", 
                                  "")) +
  theme(plot.title = element_text(size = 11, face="bold"))

# line plot of rental v.s. humidity
ggplot(count.summary, aes(x = humidity, y = count, color = weathertype)) +
  geom_smooth(method = 'loess', fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("Humidity") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_color_discrete(name = "Type of Weather:",
                       breaks = c(1, 2, 3, 4),
                       labels = c("Clear or Cloudy", 
                                  "Mist", 
                                  "Light Rain or Snow", 
                                  "")) +
  theme(plot.title = element_text(size = 11, face="bold"))

# line plot of rental v.s. wind speed
ggplot(count.summary, aes(x = windspeed, y = count, color = weathertype)) +
  geom_smooth(fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("Wind Speed") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_color_discrete(name = "Type of Weather:",
                       breaks = c(1, 2, 3, 4),
                       labels = c("Clear or Cloudy", 
                                  "Mist", 
                                  "Light Rain or Snow", 
                                  "")) +
  theme(plot.title = element_text(size = 11, face="bold"))

# correlation plot
bike.select <- data.frame(bike$season, bike$month, bike$weekday, bike$hour, as.integer(bike$isweekday),
                          as.integer(bike$isholiday), as.integer(bike$weather), bike$temperature,
                          bike$humidity, bike$windspeed, bike$count)
names(bike.select) <- c("season", "month", "weekday", "hour", "isweekday", 
                        "isholiday", "weathertype", "temperature", "humidity",
                        "windspeed", "count")

red=rgb(1,0,0); green=rgb(0,1,0); blue=rgb(0,0,1); white=rgb(1,1,1)
RtoWrange<-colorRampPalette(c(white, red ))
WtoGrange<-colorRampPalette(c(green, white)) 

ggsave("00_correlation_matrix.png", 
ggplot(melt(cor(bike.select)), aes(x = X1, y = X2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2("", low = WtoGrange(100), mid = RtoWrange(100), high = "gray") +
  geom_text(aes(label = round(value, 2))) +
  coord_flip() + 
  ggtitle("\n Correlation Matrix \n") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 0.6),
        axis.ticks.y = element_blank()) +
  xlab("") + ylab(""))

### Kernel Regression ###

dyn.load("nwkre.dll")

# hour
x <- as.double(bike$hour)
y <- as.double(bike$count)
b <- as.double(bw.nrd(x))
g <- as.double(seq(min(x),max(x), length = 1000))
m <- as.integer(length(g))
n <- as.integer(length(x))
hour.density <- .C("nwkre", m, n, x, y, b, g, ans = double(m))

mat <- matrix(0, nrow = 200, ncol = 1000)
dat <- cbind(x,y)

for (i in 1:200){
  sample = dat[sample(nrow(dat), 1000, replace = TRUE),]
  m.sample = as.integer(1000)
  n.sample = as.integer(1000)
  b.sample = as.double(bw.nrd(sample[,1]))
  density = .C("nwkre", 
               m, n.sample, as.double(sample[,1]), as.double(sample[,2]), b.sample, g, res = double(m))
  mat[i,] = density$res
}

mat2 <- matrix(0, nrow = 2, ncol = 1000)
for (i in 1:1000){
  mat2[1,i] = quantile(mat[,i], 0.025)
  mat2[2,i] = quantile(mat[,i], 0.975)
}
plot(c(min(x), max(x)), c(min(y), max(y)),type = "n", 
     main = "Hour of the Day v.s. Rental Count, with CI", 
     xlab = "Hour of the Day", 
     ylab = "Rental Count")
points(x,y, pch = ".")
lines(g, hour.density$ans)
lines(g, mat2[1,], lty = 2, col = "blue")
lines(g, mat2[2,], lty = 3, col = "red")
legend("topright", c("97.5% Upper Bound","Kernel Regression","2.5% Lower Bound"), 
       lty=c(3,1,2), cex=.8, col=c("red","black","blue"))

# temperature
x <- as.double(bike$temperature)
y <- as.double(bike$count)
b <- as.double(bw.nrd(x))
g <- as.double(seq(min(x),max(x), length = 1000))
m <- as.integer(length(g))
n <- as.integer(length(x))
temperature.density <- .C("nwkre_ep", m, n, x, y, b, g, ans = double(m))

mat <- matrix(0, nrow = 200, ncol = 1000)
dat <- cbind(x,y)

for (i in 1:200){
  sample = dat[sample(nrow(dat), 1000, replace = TRUE),]
  m.sample = as.integer(1000)
  n.sample = as.integer(1000)
  b.sample = as.double(bw.nrd(sample[,1]))
  density = .C("nwkre", 
               m, n.sample, as.double(sample[,1]), as.double(sample[,2]), b.sample, g, res = double(m))
  mat[i,] = density$res
}

mat2 <- matrix(0, nrow = 2, ncol = 1000)
for (i in 1:1000){
  mat2[1,i] = quantile(mat[,i], 0.025)
  mat2[2,i] = quantile(mat[,i], 0.975)
}
plot(c(min(x), max(x)), c(min(y), max(y)),type = "n", 
     main = "Temperature v.s. Rental Count, with CI", 
     xlab = "Temperature", 
     ylab = "Rental Count")
points(x,y, pch = ".")
lines(g, temperature.density$ans)
lines(g, mat2[1,], lty = 2, col = "blue")
lines(g, mat2[2,], lty = 3, col = "red")
legend("topright", c("97.5% Upper Bound","Kernel Regression","2.5% Lower Bound"), 
       lty=c(3,1,2), cex=.8, col=c("red","black","blue"))

# humidity
x <- as.double(bike$humidity)
y <- as.double(bike$count)
b <- as.double(bw.nrd(x))
g <- as.double(seq(min(x),max(x), length = 1000))
m <- as.integer(length(g))
n <- as.integer(length(x))
humidity.density <- .C("nwkre", m, n, x, y, b, g, ans = double(m))

mat <- matrix(0, nrow = 200, ncol = 1000)
dat <- cbind(x,y)

for (i in 1:200){
  sample = dat[sample(nrow(dat), 1000, replace = TRUE),]
  m.sample = as.integer(1000)
  n.sample = as.integer(1000)
  b.sample = as.double(bw.nrd(sample[,1]))
  density = .C("nwkre", 
               m, n.sample, as.double(sample[,1]), as.double(sample[,2]), b.sample, g, res = double(m))
  mat[i,] = density$res
}

mat2 <- matrix(0, nrow = 2, ncol = 1000)
for (i in 1:1000){
  mat2[1,i] = quantile(mat[,i], 0.025)
  mat2[2,i] = quantile(mat[,i], 0.975)
}
plot(c(min(x), max(x)), c(min(y), max(y)),type = "n", 
     main = "Humidity v.s. Rental Count, with CI", 
     xlab = "Humidity", 
     ylab = "Rental Count")
points(x,y, pch = ".")
lines(g, humidity.density$ans)
lines(g, mat2[1,], lty = 2, col = "blue")
lines(g, mat2[2,], lty = 3, col = "red")
legend("topright", c("97.5% Upper Bound","Kernel Regression","2.5% Lower Bound"), 
       lty=c(3,1,2), cex=.8, col=c("red","black","blue"))

# windspeed
x <- as.double(bike$windspeed)
y <- as.double(bike$count)
b <- as.double(bw.nrd(x))
g <- as.double(seq(min(x),max(x), length = 1000))
m <- as.integer(length(g))
n <- as.integer(length(x))
windspeed.density <- .C("nwkre", m, n, x, y, b, g, ans = double(m))

mat <- matrix(0, nrow = 200, ncol = 1000)
dat <- cbind(x,y)

for (i in 1:200){
  sample = dat[sample(nrow(dat), 1000, replace = TRUE),]
  m.sample = as.integer(1000)
  n.sample = as.integer(1000)
  b.sample = as.double(bw.nrd(sample[,1]))
  density = .C("nwkre", 
               m, n.sample, as.double(sample[,1]), as.double(sample[,2]), b.sample, g, res = double(m))
  mat[i,] = density$res
}

mat2 <- matrix(0, nrow = 2, ncol = 1000)
for (i in 1:1000){
  mat2[1,i] = quantile(mat[,i], 0.025)
  mat2[2,i] = quantile(mat[,i], 0.975)
}
plot(c(min(x), max(x)), c(min(y), max(y)),type = "n", 
     main = "Wind Speed v.s. Rental Count, with CI", 
     xlab = "Wind Speed", 
     ylab = "Rental Count")
points(x,y, pch = ".")
lines(g, windspeed.density$ans)
lines(g, mat2[1,], lty = 2, col = "blue")
lines(g, mat2[2,], lty = 3, col = "red")
legend("topright", c("97.5% Upper Bound","Kernel Regression","2.5% Lower Bound"), 
       lty=c(3,1,2), cex=.8, col=c("red","black","blue"))

dyn.unload("nwkre.dll")

### building models ###
# linear model
bike.lm <- lm(data = bike.new.train, count ~ hour + temperature + humidity + windspeed)
summary(bike.lm)

# generalized linear model, poisson count statistic
bike.glm <- glm(data = bike.new.train, count ~ hour + temperature + humidity + windspeed, family = "poisson")
summary(bike.glm)
# R^2
1-(915672/1357609)

# generalized linear model
bike.glm2 <- glm(data = bike.new.train, count ~ hour + isweekday + isholiday + weathertype + temperature + humidity + windspeed)
summary(bike.glm2)
bike.glm2.step <- step(bike.glm2)
# R^2
1-(187011168/271430390)

# generalized additive model
# no spline smoothing
bike.gam1 <- gam(data = bike.new.train, count ~ hour + isweekday + isholiday + weathertype + temperature + humidity + windspeed)

# with spline smoothing
bike.gam2 <- gam(data = bike.new.train, count ~ s(hour) + isweekday + isholiday + weathertype + s(temperature) + s(humidity) + s(windspeed))

bike.gam3 <- gam(data = bike.new.train, count ~ s(hour, 28) + isweekday + isholiday + weathertype + s(temperature,28) + s(humidity) + s(windspeed))

bike.gam4 <- gam(data = bike.new.train, count ~ s(hour, 28)+isweekday+isholiday+weathertype+s(temperature,28)+s(humidity,28)+s(windspeed))

bike.gam5 <- gam(data = bike.new.train, count ~ s(hour, 340)+isweekday+isholiday+weathertype+s(temperature,340)+s(humidity,340)+s(windspeed, 340))

# anova table
anova(bike.gam1, bike.gam2, bike.gam3, bike.gam4, bike.gam5, test = "F")

# R^2
1 - (186996335/271430390) #gam1
1 - (183792697/271430390) #gam2
1 - (171871824/271430390) #gam3
1 - (170496890/271430390) #gam4
1 - (154134185/271430390) #gam5

par(mfrow = c(2,4))
plot.gam(bike.gam1, se = TRUE, col = "yellow")
par(mfrow = c(2,4))
plot.gam(bike.gam2, se = TRUE, col = "green")
par(mfrow = c(2,4))
plot.gam(bike.gam3, se = TRUE, col = "red")
par(mfrow = c(2,4))
plot.gam(bike.gam4, se = TRUE, col = "blue")
par(mfrow = c(2,4))
plot.gam(bike.gam5, se = TRUE, col = "cyan")

par(mfrow = c(1,3))
plot(bike.new.test$count, main = "Linear Model", ylab = "Test Set Rental Count")
points(predict(bike.lm, newdata = bike.new.test), col = "yellow")
plot(bike.new.test$count, main = "Count Statistics", ylab = "Test Set Rental Count")
points(predict(bike.glm, newdata = bike.new.test), col = "orange")
plot(bike.new.test$count, main = "Generalized Linear Model", ylab = "Test Set Rental Count")
points(predict(bike.glm2, newdata = bike.new.test), col = "red")

par(mfrow = c(2,2))
plot(bike.new.test$count, main = "GAM1", ylab = "Test Set Rental Count")
points(predict(bike.gam1, newdata = bike.new.test), col = "yellow")
plot(bike.new.test$count, main = "GAM2", ylab = "Test Set Rental Count")
points(predict(bike.gam2, newdata = bike.new.test), col = "green")
plot(bike.new.test$count, main = "GAM3", ylab = "Test Set Rental Count")
points(predict(bike.gam3, newdata = bike.new.test), col = "red")
plot(bike.new.test$count, main = "GAM4", ylab = "Test Set Rental Count")
points(predict(bike.gam4, newdata = bike.new.test), col = "blue")
plot(bike.new.test$count, main = "GAM5", ylab = "Test Set Rental Count")
points(predict(bike.gam5, newdata = bike.new.test), col = "cyan")
```

C Code:

```C
#include <R.h>
#include <Rmath.h>

void nwkre (int *m, int *n, double *x, double *y, double *b, double *g, double *res)
{
	for(int i = 0; i < *m; i++)
    {
		double sum1 = 0.0;
		double sum2 = 0.0;
		for(int j = 0; j < *n; j++)
        {
			double temp = dnorm((x[j] - g[i])/ *b, 0, 1, 0)/ *b;
			sum1 += y[j] * temp;
			sum2 += temp;
		}
		if(sum2 > 0.0) res[i] = sum1 / sum2;
		else res[i] = 0.0;
	}
}
```
