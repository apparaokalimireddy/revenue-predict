rm(list=ls())
if(!exists("loadAndPrepare", mode="function")) source("loadAndPrepare.R")
if(!exists("fourthWeekSimple", mode="function")) source("simpleAnalysis.R")
df<-loadAndPrepare("wine21.csv")

# Simple Analysis
library(ggplot2)
library(scales)

# Running simple regression, gives us the overall trend of revenues.
# Todo: It would be nice to have color regions to indicate holidays and weekends, color?
simple <- qplot(x = df$Day, y = df$Revenues, geom="point", ylab="Revenues") + geom_line()
simple <- simple + scale_y_continuous(name = "Revenues", labels = dollar) 
simple <- simple + scale_x_continuous(name = "Day", 
                                      breaks = round(seq(min(df$Day), max(df$Day), by = 2),2))
simple <- simple + geom_smooth(method = "lm", se=FALSE, color="blue")

# As is evident from the chart, simple regression misses the effect of 
# seasonality (holidays and weekends) on revenues, there by trendline 
# accentuates the trend. In the example of "wine", fourth week revenues 
# seem negative.

# Now if we run simple linear regression , we get the regression model ddetails
# like fitted values, residuals, Residual std error, R-squared etc.
simple_reg_stats <- lm(formula = df$Revenues ~ df$Day, data = df)

# Create a table with fitted values and residuals from the regression model created above
df_simple <- data.frame(df , fitted.value= fitted (simple_reg_stats), residual= resid (simple_reg_stats))

# Model equation (equation representing the trendline)
simple_eq <- paste("Y = ", round(simple_reg_stats$coefficients[1]), " + ", round(simple_reg_stats$coefficients[2]), "X", sep = "")

# Based on this equation/model, our fourth week prediction would be:
df_fourth_simple<-fourthWeekSimple(simple_reg_stats, df$Date[21]+1)

#If residuals are highly auto autocorrelated, it means there is a potential seanoality that is missed in the model
# Typically auto correlation of greater than .30 means it highly correlated, hence not a
# good regression model.
autocorr <- cor(x=simple_reg_stats$residuals[1:20], y=simple_reg_stats$residuals[2:21])

# Create residual plot - If you notice a visible pattern not a good indication
r_plot <- qplot(df$Day, simple_reg_stats$residuals, ylab="Residuals", xlab="Day") + geom_abline(aes(slope=0,intercept=0))
# Create histogram of residuals, if it doesn't look "normal", not good.
hist <- qplot(simple_reg_stats$residuals, geom="histogram",binwidth=1000000)
