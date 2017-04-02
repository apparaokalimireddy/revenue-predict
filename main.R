rm(list=ls())
if(!exists("loadAndPrepare", mode="function")) source("loadAndPrepare.R")
df<-loadAndPrepare("wine21.csv")

# Analysis
library(ggplot2)
library(scales)

simple<-qplot(x = df$Day, y = df$Revenues, geom="point", xlab="Day", ylab="Revenues")

