#Load data
data = read.csv("DS 612 Group Project data set.csv")

#omit na values 
data = na.omit(data)
#summarize data 
summary(data)

head(data)
data.frame(colnames(data))
#remove unecessary columns
data <- data[-c(1, 18, 19)]


#drop id, lat, longitude

#price as dep var?
#zip as predictor var




