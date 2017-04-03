# Predict fourth week revenues
fourthWeekSimple<-function(reg, startDt) {
  Day<-seq(22, 28, by=1)
  Date<-seq(startDt+1, by=1,length.out = 7)
  Revenue<-simple_reg_stats$coefficients[1]+simple_reg_stats$coefficients[2]*Day
  df_pred<-data.frame(Day, Date, Revenue)
}