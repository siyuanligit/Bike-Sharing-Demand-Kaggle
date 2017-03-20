setwd("D:/Dropbox/UCLA/STAT404 - Statistical Computing and Programming/Project")
setwd("C:/Users/insuf/Dropbox/UCLA/STAT404 - Statistical Computing and Programming/Project")

### loading library ###
library(MASS)
library(ggplot2)
library(plyr)
library(reshape)
library(gam)

### loading data ###
bike <- read.csv("./alternative_data/train.csv")
month <- as.integer(format(as.POSIXlt(bike.new$datetime), format = "%m"))
weekday <- as.integer(format(as.POSIXlt(bike.new$datetime), format = "%u"))
hour <- as.integer(format(as.POSIXlt(bike.new$datetime), format = "%H"))
bike <- data.frame(bike$season, month, weekday, hour, as.factor(bike$workingday), as.factor(bike$holiday), as.factor(bike$weather), bike$temp, bike$hum, bike$windspeed, bike$count)
names(bike) <- c("season", "month", "weekday", "hour", "isweekday", "isholiday", "weathertype", "temperature", "humidity", "windspeed", "count")

bike <- bike[which(bike$windspeed != 0.0000),]
head(bike, 5)
count.summary <- ddply(bike,.(season, month, weekday, hour, isweekday, isholiday, weathertype), summarise, temperature = mean(temperature), humidity = mean(humidity), windspeed = mean(windspeed), count = mean(count))
head(count.summary)
# as.integer(format(as.POSIXlt(bike.new$datetime), format = "%d")) - as.integer(format(as.POSIXlt(bike.new$datetime[1]), format = "%d")) + 1
# as.integer(format(as.POSIXlt(bike.new$datetime), format = "%H"))


### assesments ###

# correlation plot
bike.select <- data.frame(bike$season, bike$month, bike$weekday, bike$hour, as.integer(bike$isweekday), as.integer(bike$isholiday), as.integer(bike$weather), bike$temperature, bike$humidity, bike$windspeed, bike$count)
names(bike.select) <- c("season", "month", "weekday", "hour", "isweekday", "isholiday", "weathertype", "temperature", "humidity", "windspeed", "count")

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

# boxplot of rental v.s. season 
ggsave("01_bike_rentals_by_season.png", 
ggplot(count.summary, aes(x = season, y = count, fill = factor(season))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values = c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
                    name="Season:",
                    breaks=c(1, 2, 3, 4),
                    labels=c("Winter", "Spring", "Summer", "Fall")))

# line plot of rentals v.s. hour of day
ggsave("02_bike_rentals_by_weekday_and_hour.png", 
ggplot(count.summary, aes(x = hour, y = count, color = as.factor(weekday))) +
  geom_smooth(method = "loess", fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("Hour of the Day") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_color_discrete("") +
  theme(plot.title = element_text(size = 11, face="bold")))


# boxplot of rental v.s. holiday 
ggsave("03_bike_rentals_by_holiday_and_season.png", 
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
  theme(plot.title = element_text(size = 11, face="bold")))

# boxplot of rentals v.s. weather
ggsave("04_bike_rentals_by_weather.png", 
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
  theme(plot.title = element_text(size = 11, face="bold")))

# line plot of rental v.s. temperature
ggsave("05_bike_rentals_by_temperature_and_weather.png", 
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
  theme(plot.title = element_text(size = 11, face="bold")))

# line plot of rental v.s. humidity
ggsave("06_bike_rentals_by_humidity_and_weather.png", 
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
  theme(plot.title = element_text(size = 11, face="bold")))

# line plot of rental v.s. wind speed
ggsave("07_bike_rentals_by_windspeed_and_weather.png", 
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
  theme(plot.title = element_text(size = 11, face="bold")))

### analysis ###

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
  density = .C("nwkre", m, n.sample, as.double(sample[,1]), as.double(sample[,2]), b.sample, g, res = double(m))
  mat[i,] = density$res
}

mat2 <- matrix(0, nrow = 2, ncol = 1000)
for (i in 1:1000){
  mat2[1,i] = quantile(mat[,i], 0.025)
  mat2[2,i] = quantile(mat[,i], 0.975)
}
plot(c(min(x), max(x)), c(min(y), max(y)),type = "n", main = "Hour of the Day v.s. Rental Count, with CI", xlab = "Hour of the Day", ylab = "Rental Count")
points(x,y, pch = ".")
lines(g, hour.density$ans)
lines(g, mat2[1,], lty = 2, col = "blue")
lines(g, mat2[2,], lty = 3, col = "red")
legend("topright", c("97.5% Upper Bound","Kernel Regression","2.5% Lower Bound"), lty=c(3,1,2), cex=.8, col=c("red","black","blue"))

# temperature
x <- as.double(bike$temperature)
y <- as.double(bike$count)
b <- as.double(bw.nrd(x))
g <- as.double(seq(min(x),max(x), length = 1000))
m <- as.integer(length(g))
n <- as.integer(length(x))
temperature.density <- .C("nwkre", m, n, x, y, b, g, ans = double(m))

mat <- matrix(0, nrow = 200, ncol = 1000)
dat <- cbind(x,y)

for (i in 1:200){
  sample = dat[sample(nrow(dat), 1000, replace = TRUE),]
  m.sample = as.integer(1000)
  n.sample = as.integer(1000)
  b.sample = as.double(bw.nrd(sample[,1]))
  density = .C("nwkre", m, n.sample, as.double(sample[,1]), as.double(sample[,2]), b.sample, g, res = double(m))
  mat[i,] = density$res
}

mat2 <- matrix(0, nrow = 2, ncol = 1000)
for (i in 1:1000){
  mat2[1,i] = quantile(mat[,i], 0.025)
  mat2[2,i] = quantile(mat[,i], 0.975)
}
plot(c(min(x), max(x)), c(min(y), max(y)),type = "n", main = "Temperature v.s. Rental Count, with CI", xlab = "Temperature", ylab = "Rental Count")
points(x,y, pch = ".")
lines(g, temperature.density$ans)
lines(g, mat2[1,], lty = 2, col = "blue")
lines(g, mat2[2,], lty = 3, col = "red")
legend("topright", c("97.5% Upper Bound","Kernel Regression","2.5% Lower Bound"), lty=c(3,1,2), cex=.8, col=c("red","black","blue"))

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
  density = .C("nwkre", m, n.sample, as.double(sample[,1]), as.double(sample[,2]), b.sample, g, res = double(m))
  mat[i,] = density$res
}

mat2 <- matrix(0, nrow = 2, ncol = 1000)
for (i in 1:1000){
  mat2[1,i] = quantile(mat[,i], 0.025)
  mat2[2,i] = quantile(mat[,i], 0.975)
}
plot(c(min(x), max(x)), c(min(y), max(y)),type = "n", main = "Humidity v.s. Rental Count, with CI", xlab = "Humidity", ylab = "Rental Count")
points(x,y, pch = ".")
lines(g, humidity.density$ans)
lines(g, mat2[1,], lty = 2, col = "blue")
lines(g, mat2[2,], lty = 3, col = "red")
legend("topright", c("97.5% Upper Bound","Kernel Regression","2.5% Lower Bound"), lty=c(3,1,2), cex=.8, col=c("red","black","blue"))

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
  density = .C("nwkre", m, n.sample, as.double(sample[,1]), as.double(sample[,2]), b.sample, g, res = double(m))
  mat[i,] = density$res
}

mat2 <- matrix(0, nrow = 2, ncol = 1000)
for (i in 1:1000){
  mat2[1,i] = quantile(mat[,i], 0.025)
  mat2[2,i] = quantile(mat[,i], 0.975)
}
plot(c(min(x), max(x)), c(min(y), max(y)),type = "n", main = "Wind Speed v.s. Rental Count, with CI", xlab = "Wind Speed", ylab = "Rental Count")
points(x,y, pch = ".")
lines(g, windspeed.density$ans)
lines(g, mat2[1,], lty = 2, col = "blue")
lines(g, mat2[2,], lty = 3, col = "red")
legend("topright", c("97.5% Upper Bound","Kernel Regression","2.5% Lower Bound"), lty=c(3,1,2), cex=.8, col=c("red","black","blue"))

dyn.unload("nwkre.dll")

### building models ###
# partitioning data
set.seed(1)
sample.index <- sample(nrow(bike), 9573*0.75, replace = FALSE)
bike.train <- bike[sample.index,]
bike.test <- bike[-sample.index,]

# linear regression
bike.lm <- lm(data = bike.train, count ~ season + month + weekday + hour + temperature + humidity + windspeed)
summary(bike.lm)
bike.lm.step <- step(bike.lm)
summary(bike.lm.step)

# generalized linear model
bike.glm2 <- glm(data = bike.train, count ~ as.factor(season) + as.factor(month) + as.factor(weekday) + as.factor(hour) + as.factor(isweekday) + 
                   as.factor(isholiday) + as.factor(weathertype) + temperature + humidity + windspeed)
summary(bike.glm2)
bike.glm2.step <- step(bike.glm2)
summary(bike.glm2.step)
1 - (86480856/241870829)

# gam1
bike.gam1 <- gam(data = bike.train, count ~ as.factor(season) + as.factor(month) + as.factor(weekday) + as.factor(hour) + as.factor(isweekday) + 
                   as.factor(isholiday) + as.factor(weathertype) + temperature + humidity + windspeed)
summary(bike.gam1)
1 - (86367987/241870829)
bike.gam1.step <- gam(data = bike.train, count ~ as.factor(season) + as.factor(month) + as.factor(weekday) + as.factor(hour) + 
                        + as.factor(weathertype) + temperature + humidity + windspeed)
summary(bike.gam1.step)
1 - (86371773/241870829)

# gam2
bike.gam2 <- gam(data = bike.train, count ~ as.factor(season) + as.factor(month) + as.factor(weekday) + as.factor(hour) + as.factor(isweekday) + 
                   as.factor(isholiday) + as.factor(weathertype) + s(temperature) + s(humidity) + s(windspeed))
summary(bike.gam2)
1 - (84860137/241870829)
bike.gam2.step <- gam(data = bike.train, count ~ as.factor(season) + as.factor(month) + as.factor(weekday) + as.factor(hour) + as.factor(weathertype) + 
                        s(temperature) + s(humidity) + s(windspeed))
summary(bike.gam2.step)
1 - (84870598/241870829)

# gam3
bike.gam3 <- gam(data = bike.train, count ~ as.factor(season) + as.factor(month) + as.factor(weekday) + as.factor(hour) + as.factor(isweekday) + 
                   as.factor(isholiday) + as.factor(weathertype) + s(temperature, 4) + s(humidity, 10) + s(windspeed, 6))
summary(bike.gam3)
1 - (84534154/241870829)
bike.gam3.step <- gam(data = bike.train, count ~ as.factor(season) + as.factor(month) + as.factor(weekday) + as.factor(hour) + as.factor(weathertype) + 
                        s(temperature, 4) + s(humidity, 10) + s(windspeed, 6))
summary(bike.gam3.step)
1 - (84544911/241870829)

anova(bike.gam1.step, bike.gam2.step, bike.gam3.step, test = "F")

### predictions ###
# lm
plot(bike.test$count, main = "Linear Model", ylab = "Test Set Rental Count", pch = 20)
points(predict(bike.lm.step, newdata = bike.test), col = "red", pch = 20)

# glm
plot(bike.test$count, main = "Generalized Linear Model", ylab = "Test Set Rental Count", pch = 20)
points(predict(bike.glm2.step, newdata = bike.test), col = "red", pch = 20)

# gam3
plot(bike.test$count, main = "GAM3", ylab = "Test Set Rental Count", pch = 20)
points(predict(bike.gam3.step, newdata = bike.test), col = "red", pch = 20)
