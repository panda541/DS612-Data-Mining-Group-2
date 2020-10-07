
library(readr)
clean_data <- read_csv("clean_data.csv")


fit1<-lm(log_price~bedrooms+bathrooms, data=clean_data)
summary(fit1)  #highly significant
plot(log_price~bathrooms, clean_data)
plot(log_price~bedrooms, clean_data)
pred<-predict(fit1, clean_data)
head(pred)
anova(fit1) 
fit1$coefficients
plot(fit1, which = 1)
plot(fit1, which = 2)
standardResidual<-rstandard(fit1)
hist(standardResidual)

step(fit1, direction = "backward")
library(MASS)
step.model<-stepAIC(fit1, direction = "both", trace = FALSE)
summary(step.model)

#MSE
sum(step.model$residuals^2)

mean(step.model$residuals^2)

fit<-lm(log_price~bedrooms, data = clean_data) 
fit<-lm(log_price~bathrooms, data = clean_data) 
fit2<-lm(log_price~bedrooms+bathrooms+sqft_living+sqft_lot, data = clean_data)  # these variable are higly significant.