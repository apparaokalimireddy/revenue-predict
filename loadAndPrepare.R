# Load and Prepare the data.
if(!exists("checkForHoliday", mode="function")) source("util.R")
loadAndPrepare <- function(url) {
  df = read.csv(url, stringsAsFactors = FALSE)
  df$Revenues <- as.numeric(gsub("\\$|,","",df$Revenues))
  df$Date <- as.Date(df$Date, "%m/%d/%Y")
  df$Weekday <- factor(substr(weekdays(df$Date), 1, 3))
  return(df)
}
setDummyVariables <- function(df) {
  df$Friday=ifelse(weekdays(df$Date)=="Friday",TRUE,FALSE)
  df$Saturday=ifelse(weekdays(df$Date)=="Saturday",TRUE,FALSE)
  df$Sunday=ifelse(weekdays(df$Date)=="Sunday",TRUE,FALSE)
  df$Holiday<-checkForHoliday(df$Date)
  df
}