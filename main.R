rm(list=ls())
if(!exists("loadAndPrepare", mode="function")) source("loadAndPrepare.R")
df<-loadAndPrepare("wine21.csv")

# Simple Analysis
library(ggplot2)
library(scales)

# Running simple regression, gives us the overall trend of revenues.
# Todo: It would be nice to have color regions to indicate holidays and weekends
simple <- qplot(x = df$Day, y = df$Revenues, geom="point", ylab="Revenues") 
simple <- simple + scale_y_continuous(name = "Revenues", labels = dollar) 
simple <- simple + scale_x_continuous(name = "Day", 
                                      breaks = round(seq(min(df$Day), max(df$Day), by = 1),1))
simple <- simple + geom_smooth(method = "lm", se=FALSE, color="blue", aes(group=1))

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

# Based on this equation/model, our fourth week prediction would be

