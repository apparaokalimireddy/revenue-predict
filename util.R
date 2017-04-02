# Returns TRUE if the Date passed is a defined HOliday. 
checkForHoliday <- function(x) {
  library(chron)
  library(timeDate)
  hlist <- c("USThanksgivingDay","USChristmasDay")
  holidays <- dates(sapply(sapply(hlist,holiday,year=as.integer(format(x, "%Y"))),as.Date))
  return(is.holiday(x,holidays))
}
