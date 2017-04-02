rm(list=ls())
if(!exists("loadAndPrepare", mode="function")) source("loadAndPrepare.R")
df<-loadAndPrepare("wine21.csv")

# Analysis
library(ggplot2)
library(scales)

simple <- qplot(x = df$Day, y = df$Revenues, geom="point", ylab="Revenues") 
simple <- simple + scale_y_continuous(name = "Revenues", labels = dollar) 
simple <- simple + scale_x_continuous(name = "Day", 
                                      breaks = round(seq(min(df$Day), max(df$Day), by = 1),1))


