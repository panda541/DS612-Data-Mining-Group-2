library(corrplot)
library(MASS)
library(ISLR)
library(ggplot2)
library(GGally)
library(psych)
library(lubridate)
library(Hmisc)
#Load data
#data = read.csv("DS 612 Group Project data set.csv")
data<-read.csv("Desktop/kc_house_data1.csv", header = TRUE, sep = ",")  # this is similiar as row 8. 

#omit na values 
data = na.omit(data)  # since our data does not have NA we don't need this.  
#summarize data 
summary(data)

str(data)  # we have one factor which is date. 
# I believe date is important in our data, we need to find how can we change into numeric. 
#sorddate<-sort(data$date)   # here we need to find how to delete T000000 I saw this data is only for the month of May. 
#date1<-subset(data, date== "20140502T000000") # just an wxample, here show that about 67 houses were sold in one day but we are not looking this. 

#defining categorical variables, ex. zipcode change to factor then to numeric, date into numeric. 
data$zipcode<-as.factor(data$zipcode)
data$zipcode<-as.numeric(data$zipcode)
#''''''''''''''''''''''''''
data1<-factor(data$date)
str(data1)
newdate<-as.Date(data$date,format= "%Y%m%dT000000")
data$new_col<-newdate
data

#removing columns, renaming the new date, and switching newdate
names(data)[22]<-"newDate"
data<-data[-c(1,2)]
colnames(data)
data<-data[, c(20, 1:19)]
#sapply(data, class)  # this is deleting my price
summary(data)

#plot / visualization
plot(data$newDate, data$price)
plot(price ~ sqft_living, data = data) # same as plot(data$newDate, data$price)
plot(data$newDate, data$bedrooms)  # no prediction
plot(price ~ sqft_living, data = data)  # good corrolation
plot(price ~ sqft_lot, data = data)  #no prediction
boxplot(price~sqft_lot,data=data)

#pairs
#for some reason my rstudio is freezin while doing pairs 
pairs(data,panel=panel.smooth, main="Scatter plots")

pairs(~ price + bedrooms + bathrooms + sqft_living + sqft_lot, floors, data = data, panel = panel.smooth)

#regression model
fit1<-lm(zipcode~price+bedrooms+bathrooms, data=data)
summary(fit1)

#Define quant and qual variables
quants <- subset(data, select = -m=newDate)   #omitted date
quals <- data$newDate   

#Outlier detection
multi_func <- function(x){
  c(min = min(x), mean = mean(x), max = max(x), std_dev = sd(x))
}

sapply(quants, multi_func)

#Investigate further: price, bedrooms, yr_renovated
#cor <- round(cor(quants), 2)
#corrplot(cor, method = "number")

#with out rounding
ggcorr(quants, pallette = "RdBu", label = TRUE) # from here I can see that variable price, bedroom, bathroom, sqft_living, sqft_lot,
#waterfront, squarefeet living15, sq_lot15 have good correlation. 

#splitting in part our variable
#newdata<-quants[-c(11:17)]
View(newdata)


#pairs
pairs.panels(quants)    # it is slow because we have multiples variables
#unless if we omit some variables
pairs.panels(quants[,1:4])
pairs.panels(quants[,5:8])
#plots
attach(quants)
plot(quants$price,quants$bedrooms)
plot(bathrooms, price)
plot(price, bathrooms)

