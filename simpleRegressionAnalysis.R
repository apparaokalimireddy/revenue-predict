# Predict future revenues
predictRevenues<-function(model, df, noOfDays=7) {
  Day<-seq(from = nrow(df)+1, length.out = noOfDays, by = 1)
  Date<-seq(from = df$Date[nrow(df)]+1, length.out = noOfDays, by = 1)
  df_pred<-data.frame(Day, Date, Revenues=NA, Weekday=NA)
  df_pred$PredictedRevenues<-predict(model, df_pred)
  df_pred$Weekday<-factor(substr(weekdays(df_pred$Date), 1, 3))
  row.names(df_pred)<-df_pred$Day
 # df$PredictedRevenues<-NA
  print(colnames(df))
  print(colnames(df_pred))
  df_pred<-rbind(df, df_pred)
}
predictRevenuesMR<-function(model, df, noOfDays=7) {
  Day<-seq(from = nrow(df)+1, length.out = noOfDays, by = 1)
  Date<-seq(from = df$Date[nrow(df)]+1, length.out = noOfDays, by = 1)
  df_pred<-data.frame(Day, Date, Revenues=NA, Weekday=NA, LNRevenues=NA)
  df_pred = setDummyVariables(df_pred)
  df_pred$PredictedRevenues<-exp(predict(model, df_pred))
  df_pred$Weekday<-factor(substr(weekdays(df_pred$Date), 1, 3))
  row.names(df_pred)<-df_pred$Day
  #df$PredictedRevenues<-NA
  df_pred<-rbind(df, df_pred)
}
