rm(list=ls())
library(ggplot2)
library(scales)
if(!exists("loadAndPrepare", mode="function")) source("loadAndPrepare.R")
if(!exists("predictRevenues", mode="function")) source("simpleRegressionAnalysis.R")
df<-loadAndPrepare("wine21.csv")

###########################
# Simple Regression
###########################

# Running simple regression, gives us the overall trend of revenues.
# Todo: It would be nice to have color regions to indicate holidays and weekends, color?
simple <- qplot(x = df$Day, y = df$Revenues, geom="point", ylab="Revenues") + geom_line()
simple <- simple + scale_y_continuous(name = "Revenues", labels = dollar) 
simple <- simple + scale_x_continuous(name = "Day", breaks = round(seq(min(df$Day), max(df$Day), by = 2),2))
simple <- simple + geom_smooth(method = "lm", se=FALSE, color="blue")

# As is evident from the chart, simple regression misses the effect of 
# seasonality (holidays and weekends) on revenues, there by trendline 
# accentuates the trend. In the example of "wine", fourth week revenues 
# seem negative.

# Now if we run simple linear regression , we get the regression model ddetails
# like fitted values, residuals, Residual std error, R-squared etc.
sr_model <- lm(formula = Revenues ~ Day, data = df)

# Create a table with fitted values and residuals from the regression model created above
df_sr <- data.frame(df , PredictedRevenues = fitted (sr_model), Residual = resid(sr_model))
df$PredictedRevenues<-fitted (sr_model)
  
# Model equation (equation representing the trendline)
sr_eq <- paste("Y = ", round(sr_model$coefficients[1]), " + ", round(sr_model$coefficients[2]), "X", sep = "")

# Based on this equation/model, calculate predicted values
df_psr<-predictRevenues(sr_model, df)

#If residuals are highly auto autocorrelated, it means there is a potential seanoality that is missed in the model
# Typically auto correlation of greater than .30 means it highly correlated, hence not a
# good regression model.
autocorr_sr <- cor(x=sr_model$residuals[1:nrow(df)-1], y=sr_model$residuals[2:nrow(df)])

# Create residual plot - If you notice a visible pattern not a good indication
sr_residual_plot <- qplot(df$Day, sr_model$residuals, ylab="Residuals", xlab="Day") + geom_abline(aes(slope=0,intercept=0))
# Create histogram of residuals, if it doesn't look "normal", not good.
sr_hist <- qplot(sr_model$residuals, geom="histogram",binwidth=1000000)

############################################
# Multiplle Regression with Dummy Variables
############################################
# Take natural logarithm of Revenues. 
df$LNRevenues<-log(df$Revenues)
df<-setDummyVariables(df)
# Now create multiple regression model for LNRevenues over timme series and Dummy variables
# Dummy variables here, indicates whether a day is Friday, Saturday, Sunday or a Holiday.
mr_model <- lm(formula = LNRevenues ~ Day+Friday+Saturday+Sunday+Holiday, data = df)
df$PredictedRevenues<-exp(fitted(mr_model))
# Check autocorrelation of residuals. Less than 0.3 is good
autocorr_mr <- cor(x=mr_model$residuals[1:nrow(df)-1], y=mr_model$residuals[2:nrow(df)])
# Based on this equation/model, calculate predicted values
df_pmr<-predictRevenues(mr_model, df, multipleReg = TRUE)
# Create residual plot - If you notice a visible pattern not a good indication
mr_residual_plot <- qplot(df$Day, mr_model$residuals, ylab="Residuals", xlab="Day") + geom_abline(aes(slope=0,intercept=0))
# Create histogram of residuals, if it doesn't look "normal", not good.
mr_hist <- qplot(mr_model$residuals, geom="histogram", binwidth=0.1)
# Plot the Actual and Predcited Revenues
multiple <- qplot(x = df_pmr$Day, ylab="Revenues") + geom_line(aes(y = df_pmr$Revenues, color = "Actual")) + geom_line(aes(y = df_pmr$PredictedRevenues, color = "Predicted"))+geom_point(aes(y = df_pmr$Revenues, color="Actual")) +geom_point(aes(y = df_pmr$PredictedRevenues, color="Predicted"))
multiple <- multiple + scale_y_continuous(name = "Revenues", labels = dollar) 
multiple <- multiple + scale_x_continuous(name = "Day", breaks = round(seq(min(df_pmr$Day), max(df_pmr$Day), by = 2),2))
