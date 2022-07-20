##Script to drive Diablo Immortal Timer App.##

library(tidyverse)
library(readxl)
library(lubridate)
library(hms)

TimeTable<-read_xlsx("DI_Lookup_Table.xlsx")
TimeTable$Time_Zone_Number<-as.integer(TimeTable$Time_Zone_Number)
TimeTable$Server_Time<-as.POSIXct(TimeTable$Server_Time, tz="UTC")
TimeTable$Ancient_Nightmare_12PM<-as.POSIXct(TimeTable$Ancient_Nightmare_12PM, tz="UTC")
TimeTable$Ancient_Nightmare_830PM<-as.POSIXct(TimeTable$Ancient_Nightmare_830PM, tz="UTC")
TimeTable$Ancient_Nightmare_10PM<-as.POSIXct(TimeTable$Ancient_Nightmare_10PM, tz="UTC")
TimeTable$System_Time<-now()
TimeTable$UTC_Time<-now("UTC")
TimeTable<-as.data.frame(TimeTable)



for(i in 1:nrow(TimeTable)){
  ##Finds server time based on the current UTC time.##
  if(grepl("-", TimeTable[i,"Time_Zone"])){
    TimeTable[i,"Server_Time"]<-force_tz(TimeTable[i,"UTC_Time"]-hours(TimeTable[i,"Time_Zone_Number"]), 'UTC')
  }else{
    TimeTable[i,"Server_Time"]<-force_tz(TimeTable[i,"UTC_Time"]+hours(TimeTable[i,"Time_Zone_Number"]), 'UTC')
    
  }

  ##Finds the weekday for use in certain timers.##
  ##Ancient Nightmare.##
  NightMareDate<-weekdays(TimeTable[i, "Server_Time"])
  NightMareDay<-TimeTable[i,"Server_Time"]
  while(NightMareDate!="Wednesday"&&NightMareDate!="Friday"){
    NightMareDay<-NightMareDay+days(1)
    NightMareDate<-weekdays(NightMareDay)
    
  }
  
  TimeTable[i, "Ancient_Nightmare_12PM"]<-force_tz(NightMareDay,'UTC')
  TimeTable[i, "Ancient_Nightmare_12PM"]<-as.POSIXct(paste0(date(TimeTable[i, "Ancient_Nightmare_12PM"])," ", "12:00:00"), tz='UTC')
  
  TimeTable[i, "Ancient_Nightmare_830PM"]<-force_tz(NightMareDay,'UTC')
  TimeTable[i, "Ancient_Nightmare_830PM"]<-as.POSIXct(paste0(date(TimeTable[i, "Ancient_Nightmare_830PM"])," ", "20:30:00"), tz='UTC')
  
  TimeTable[i, "Ancient_Nightmare_10PM"]<-force_tz(NightMareDay,'UTC')
  TimeTable[i, "Ancient_Nightmare_10PM"]<-as.POSIXct(paste0(date(TimeTable[i, "Ancient_Nightmare_10PM"])," ", "22:00:00"), tz='UTC')

}

round_hms(as_hms(difftime(TimeTable[i, "Ancient_Nightmare_12PM"], TimeTable[i,"UTC_Time"])), digits=0)

