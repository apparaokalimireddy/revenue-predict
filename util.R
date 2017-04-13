# Returns TRUE if the Date passed is a defined HOliday. 
# TODO: Need to add more comments
checkForHoliday <- function(x) {
  library(chron)
  library(timeDate)
  hlist <- c("USNewYearsDay","USMemorialDay","USIndependenceDay","USLaborDay","USThanksgivingDay","USChristmasDay") # We can list all possible holidays here.
  holidays <- dates(sapply(sapply(hlist,holiday,year=as.integer(format(x, "%Y"))),as.Date))
  return(is.holiday(x,holidays))
}
checkDayBeforeHoliday<-function(df) {
  for (i in 2:nrow(df)) {
    if (df$Holiday[i] == TRUE)
      df$Holiday[i-1] = TRUE
  }
  df
}