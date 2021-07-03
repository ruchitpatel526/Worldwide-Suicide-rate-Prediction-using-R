#import the data file
master <- read.csv("master.csv") 
# Drop unneccesary columns
master.df <- master[,c(-1,-3,-4,-8,-9,-10,-12)]
View(master.df)
#Dimension of the dataset
dim(master)

#Histograms
hist(master$gdp_per_capita...., col="Blue",breaks = 10, xlab = "GDP", ylab = "Suicide rate", main = "Suicide distribution over GDP")
hist(master$suicides.100k.pop, col="Blue",breaks = 50, xlab = "Age", ylab = "Suicide rate", main = "Suicide distribution over Age")

#Missing Values
sum(is.na(master.df))

#install packages
install.packages("corrplot")
library(corrplot)

#Correlation Matrix
cor(master.df)
corrplot(cor(master.df),method = "number")

#Graph
plot(master.df$population,master.df$suicides_no,col="grey",xlab = "Population", ylab = "No. of Suicides")

#Correlation between Population and Suicide number
cor(master.df$population,master.df$suicides_no)

#Linear Regression
r <- lm(master.df$suicides_no~master.df$population)
abline(r,col="red")
summary(r)
names(r)
r$fitted
plot(master.df$population,r$fitted,col="orange",xlab = "Population", ylab = "No. of Suicides",main = "Linear Regression Model")

#what if we want to find suicide number given population
#1 Manually
#Suicide numbers = -19.544371256 + 0.00014208*Population
# (-19.544371256 + 0.00014208*30000000) = 
# [1] 4242.856

#2 Coefficient
coef(r)
r$coef[1]+r$coef[2]*30000000 
# This will give predicted suicide number for a country having population of 30000000

#Predict Command with 90% confidence interval
predict(r,data.frame(population=12345794264),interval = "confidence",level = 0.9)

#Validation
#Suicides in United States of America in 2017.
r$coef[1]+r$coef[2]*325100000

#Suicides in United States of America in 2018.
r$coef[1]+r$coef[2]*327200000

#Suicides in Russia in 2017.
r$coef[1]+r$coef[2]*144500000

#Suicides in Australia in 2017.
r$coef[1]+r$coef[2]*24600000

#Suicides in New Zealand in 2017.
r$coef[1]+r$coef[2]*4794000

#Suicides in New Zealand in 2018.
r$coef[1]+r$coef[2]*4886000
