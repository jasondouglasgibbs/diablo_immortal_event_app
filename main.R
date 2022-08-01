##Script to drive Diablo Immortal Timer App.##

library(dplyr)
library(readxl)
library(lubridate)
library(hms)

TimeTable<-read_xlsx("DI_Lookup_Table.xlsx")
TimeTable$Time_Zone_Number<-as.integer(TimeTable$Time_Zone_Number)
TimeTable$Server_Time<-as.POSIXct(TimeTable$Server_Time, tz="UTC")
TimeTable$Server_Time<-as.POSIXct(TimeTable$Server_Time, tz="UTC")
TimeTable$Ancient_Nightmare_12PM<-as.POSIXct(TimeTable$Ancient_Nightmare_12PM, tz="UTC")
TimeTable$Ancient_Nightmare_830PM<-as.POSIXct(TimeTable$Ancient_Nightmare_830PM, tz="UTC")
TimeTable$Ancient_Nightmare_10PM<-as.POSIXct(TimeTable$Ancient_Nightmare_10PM, tz="UTC")
TimeTable$Haunted_Carriage_12PM<-as.POSIXct(TimeTable$Haunted_Carriage_12PM, tz="UTC")
TimeTable$Haunted_Carriage_830PM<-as.POSIXct(TimeTable$Haunted_Carriage_830PM, tz="UTC")
TimeTable$Haunted_Carriage_10PM<-as.POSIXct(TimeTable$Haunted_Carriage_10PM, tz="UTC")
TimeTable$Demon_Gates_12PM<-as.POSIXct(TimeTable$Demon_Gates_12PM, tz="UTC")
TimeTable$Demon_Gates_830PM<-as.POSIXct(TimeTable$Demon_Gates_830PM, tz="UTC")
TimeTable$Demon_Gates_10PM<-as.POSIXct(TimeTable$Demon_Gates_10PM, tz="UTC")
TimeTable$Ancient_Arena_930PM<-as.POSIXct(TimeTable$Ancient_Arena_930PM, tz="UTC")
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
  
  ##Haunted Carriage##
  HCDate<-weekdays(TimeTable[i, "Server_Time"])
  HCDay<-TimeTable[i,"Server_Time"]
  while(HCDate!="Tuesday"&&HCDate!="Saturday"){
    HCDay<-HCDay+days(1)
    HCDate<-weekdays(HCDay)
  }
  
  TimeTable[i, "Haunted_Carriage_12PM"]<-force_tz(HCDay,'UTC')
  TimeTable[i, "Haunted_Carriage_12PM"]<-as.POSIXct(paste0(date(TimeTable[i, "Haunted_Carriage_12PM"])," ", "12:00:00"), tz='UTC')
  
  TimeTable[i, "Haunted_Carriage_830PM"]<-force_tz(HCDay,'UTC')
  TimeTable[i, "Haunted_Carriage_830PM"]<-as.POSIXct(paste0(date(TimeTable[i, "Haunted_Carriage_830PM"])," ", "20:30:00"), tz='UTC')
  
  TimeTable[i, "Haunted_Carriage_10PM"]<-force_tz(HCDay,'UTC')
  TimeTable[i, "Haunted_Carriage_10PM"]<-as.POSIXct(paste0(date(TimeTable[i, "Haunted_Carriage_10PM"])," ", "22:00:00"), tz='UTC')
  
  
  ##DemonGates##
  DGDate<-weekdays(TimeTable[i, "Server_Time"])
  DGDay<-TimeTable[i,"Server_Time"]
  while(DGDate!="Sunday"&&DGDate!="Monday"&&DGDate!="Thursday"){
    DGDay<-DGDay+days(1)
    DGDate<-weekdays(DGDay)
  }
  
  TimeTable[i, "Demon_Gates_12PM"]<-force_tz(DGDay,'UTC')
  TimeTable[i, "Demon_Gates_12PM"]<-as.POSIXct(paste0(date(TimeTable[i, "Demon_Gates_12PM"])," ", "12:00:00"), tz='UTC')
  
  TimeTable[i, "Demon_Gates_830PM"]<-force_tz(DGDay,'UTC')
  TimeTable[i, "Demon_Gates_830PM"]<-as.POSIXct(paste0(date(TimeTable[i, "Demon_Gates_830PM"])," ", "20:30:00"), tz='UTC')
  
  TimeTable[i, "Demon_Gates_10PM"]<-force_tz(DGDay,'UTC')
  TimeTable[i, "Demon_Gates_10PM"]<-as.POSIXct(paste0(date(TimeTable[i, "Demon_Gates_10PM"])," ", "22:00:00"), tz='UTC')
  
  ##Ancient Arena##
  AADate<-weekdays(TimeTable[i, "Server_Time"])
  AADay<-TimeTable[i,"Server_Time"]
  while(AADate!="Sunday"&&AADate!="Tuesday"&&AADate!="Thursday"&AADate!="Saturday"){
    AADay<-AADay+days(1)
    AADate<-weekdays(AADay)
  }
  
  TimeTable[i, "Ancient_Arena_930PM"]<-force_tz(AADay,'UTC')
  TimeTable[i, "Ancient_Arena_930PM"]<-as.POSIXct(paste0(date(TimeTable[i, "Ancient_Arena_930PM"])," ", "21:30:00"), tz='UTC')
  
}



TimerDisplayTable<-read_xlsx("TimerDisplayTable.xlsx")
TimerDisplayTable$Countdown<-as_hms(TimerDisplayTable$Countdown)


if(grepl("-", TimeTable[1,"Time_Zone"])){
  TimeTable[1,"Server_Time"]<-force_tz(TimeTable[1,"UTC_Time"]-hours(TimeTable[1,"Time_Zone_Number"]), 'UTC')
}else{
  TimeTable[1,"Server_Time"]<-force_tz(TimeTable[1,"UTC_Time"]+hours(TimeTable[1,"Time_Zone_Number"]), 'UTC')
  
}


##Ancient Nightmare.##
countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Ancient_Nightmare_12PM"], TimeTable[1,"Server_Time"])), digits=0)
if(grepl("-",countdowntime)){
  countdowntime<-NA
}else{
  countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Ancient_Nightmare_12PM"], TimeTable[1,"Server_Time"])), digits=0)
}

TimerDisplayTable[1,2]<-countdowntime

countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Ancient_Nightmare_830PM"], TimeTable[1,"Server_Time"])), digits=0)
if(grepl("-",countdowntime)){
  countdowntime<-NA
}else{
  countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Ancient_Nightmare_830PM"], TimeTable[1,"Server_Time"])), digits=0)
}

TimerDisplayTable[2,2]<-countdowntime

countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Ancient_Nightmare_10PM"], TimeTable[1,"Server_Time"])), digits=0)
if(grepl("-",countdowntime)){
  countdowntime<-NA
}else{
  countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Ancient_Nightmare_10PM"], TimeTable[1,"Server_Time"])), digits=0)
}

TimerDisplayTable[3,2]<-countdowntime



##Haunted Carriage##
countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Haunted_Carriage_12PM"], TimeTable[1,"Server_Time"])), digits=0)
if(grepl("-",countdowntime)){
  countdowntime<-NA
}else{
  countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Haunted_Carriage_12PM"], TimeTable[1,"Server_Time"])), digits=0)
}

TimerDisplayTable[4,2]<-countdowntime


countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Haunted_Carriage_830PM"], TimeTable[1,"Server_Time"])), digits=0)
if(grepl("-",countdowntime)){
  countdowntime<-NA
}else{
  countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Haunted_Carriage_830PM"], TimeTable[1,"Server_Time"])), digits=0)
}

TimerDisplayTable[5,2]<-countdowntime


countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Haunted_Carriage_10PM"], TimeTable[1,"Server_Time"])), digits=0)
if(grepl("-",countdowntime)){
  countdowntime<-NA
}else{
  countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Haunted_Carriage_10PM"], TimeTable[1,"Server_Time"])), digits=0)
}

TimerDisplayTable[6,2]<-countdowntime

##Demon Gates##
countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Demon_Gates_12PM"], TimeTable[1,"Server_Time"])), digits=0)
if(grepl("-",countdowntime)){
  countdowntime<-NA
}else{
  countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Demon_Gates_12PM"], TimeTable[1,"Server_Time"])), digits=0)
}

TimerDisplayTable[7,2]<-countdowntime


countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Demon_Gates_830PM"], TimeTable[1,"Server_Time"])), digits=0)
if(grepl("-",countdowntime)){
  countdowntime<-NA
}else{
  countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Demon_Gates_830PM"], TimeTable[1,"Server_Time"])), digits=0)
}

TimerDisplayTable[8,2]<-countdowntime


countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Demon_Gates_10PM"], TimeTable[1,"Server_Time"])), digits=0)
if(grepl("-",countdowntime)){
  countdowntime<-NA
}else{
  countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Demon_Gates_10PM"], TimeTable[1,"Server_Time"])), digits=0)
}

TimerDisplayTable[9,2]<-countdowntime

##Ancient Arena.##
countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Ancient_Arena_930PM"], TimeTable[1,"Server_Time"])), digits=0)
if(grepl("-",countdowntime)){
  countdowntime<-NA
}else{
  countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Ancient_Arena_930PM"], TimeTable[1,"Server_Time"])), digits=0)
}

TimerDisplayTable[10,2]<-countdowntime


#Ordering and preping for display.##
TimerDisplayTable<-as.data.frame(filter(TimerDisplayTable, !is.na(TimerDisplayTable$Countdown)))
TimerDisplayTable<-TimerDisplayTable[order(TimerDisplayTable$Countdown, decreasing=FALSE),]





##Shadow War Timers##
ShadowTimeTable<-read_xlsx("Shadow_Lookup_Table.xlsx")
ShadowTimeTable$Start<-as.POSIXct(ShadowTimeTable$Start, tz="UTC")
ShadowTimeTable$Stop<-as.POSIXct(ShadowTimeTable$Stop, tz="UTC")
ShadowTimeTable$Countdown<-as_hms(ShadowTimeTable$Countdown)
ShadowTimeTable$`Active?`<-as.character(ShadowTimeTable$`Active?`)
ShadowTimeTable<-as.data.frame(ShadowTimeTable)

for(i in 1:1){
  ##Raid the Vault##
  ShadowTimeTable[i, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "12:00:00"), tz='UTC')
  ShadowTimeTable[i, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "14:00:00"), tz='UTC')
  ShadowTimeTable[i+1, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "19:00:00"), tz='UTC')
  ShadowTimeTable[i+1, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "21:00:00"), tz='UTC')
  
  #Shadow Assembly##
  
  SADate<-weekdays(TimeTable[i, "Server_Time"])
  SADay<-TimeTable[i,"Server_Time"]
  if(SADate=="Sunday"){
    ShadowTimeTable[i+2, "Active?"]<-NA
  }else{
    ShadowTimeTable[i+2, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "18:00:00"), tz='UTC')
    ShadowTimeTable[i+2, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "20:00:00"), tz='UTC')
  }
  
  

  
  
  ##Countedown Timers##
  
  ##Raid the Vault##
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-NA
  }else{
    countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ShadowTimeTable[i,"Countdown"]<-countdowntime
      ##Active Logic.##
      if(difftime(ShadowTimeTable[i, "Stop"], TimeTable[1,"Server_Time"])<0){
        ShadowTimeTable[i,"Active?"]<-NA
      }else if(difftime(ShadowTimeTable[i, "Start"], TimeTable[1,"Server_Time"])>=0){
        ShadowTimeTable[i,"Active?"]<-"No"
      }else if(difftime(ShadowTimeTable[i, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ShadowTimeTable[i, "Stop"], TimeTable[1,"Server_Time"])>=0){
        ShadowTimeTable[i,"Active?"]<-"Yes"
      }else{
        ShadowTimeTable[i,"Active?"]<-NA
      }
      
  
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+1, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-NA
  }else{
    countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+1, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ShadowTimeTable[i+1,"Countdown"]<-countdowntime
      ##Active Logic.##
      if(difftime(ShadowTimeTable[i+1, "Stop"], TimeTable[1,"Server_Time"])<0){
        ShadowTimeTable[i+1,"Active?"]<-NA
      }else if(difftime(ShadowTimeTable[i+1, "Start"], TimeTable[1,"Server_Time"])>=0){
        ShadowTimeTable[i+1,"Active?"]<-"No"
      }else if(difftime(ShadowTimeTable[i+1, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ShadowTimeTable[i+1, "Stop"], TimeTable[1,"Server_Time"])>=0){
        ShadowTimeTable[i+1,"Active?"]<-"Yes"
      }else{
        ShadowTimeTable[i+1,"Active?"]<-NA
      }
      
  
  
  ##Shadow Assembly##
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+2, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-NA
  }else{
    countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+2, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ShadowTimeTable[i+2,"Countdown"]<-countdowntime
  
      ##Active Logic.##
      if(difftime(ShadowTimeTable[i+2, "Stop"], TimeTable[1,"Server_Time"])<0){
        ShadowTimeTable[i+2,"Active?"]<-NA
      }else if(difftime(ShadowTimeTable[i+2, "Start"], TimeTable[1,"Server_Time"])>=0){
        ShadowTimeTable[i+2,"Active?"]<-"No"
      }else if(difftime(ShadowTimeTable[i+2, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ShadowTimeTable[i+2, "Stop"], TimeTable[1,"Server_Time"])>=0){
        ShadowTimeTable[i+2,"Active?"]<-"Yes"
      }else{
        ShadowTimeTable[i+2,"Active?"]<-NA
      }
      

}

ShadowTimeTable<-as.data.frame(filter(ShadowTimeTable, !is.na(ShadowTimeTable$Active)))
ShadowTimeTable<-ShadowTimeTable[order(ShadowTimeTable$Countdown, decreasing=FALSE),]
