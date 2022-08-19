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
TimeTable$Wrathborne_Invasion_12PM<-as.POSIXct(TimeTable$Wrathborne_Invasion_12PM, tz="UTC")
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
  
  ##Wrathborne Invasion#
  WIDay<-TimeTable[i,"Server_Time"]
  
  TimeTable[i, "Wrathborne_Invasion_12PM"]<-force_tz(WIDay,'UTC')
  TimeTable[i, "Wrathborne_Invasion_12PM"]<-as.POSIXct(paste0(date(TimeTable[i, "Wrathborne_Invasion_12PM"])," ", "12:00:00"), tz='UTC')
  
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


##Wrathborne Invasion.##
countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Wrathborne_Invasion_12PM"], TimeTable[1,"Server_Time"])), digits=0)
if(grepl("-",countdowntime)){
  countdowntime<-NA
}else{
  countdowntime<-round_hms(as_hms(difftime(TimeTable[1, "Wrathborne_Invasion_12PM"], TimeTable[1,"Server_Time"])), digits=0)
}

TimerDisplayTable[11,2]<-countdowntime



#Ordering and preping for display.##
TimerDisplayTable<-as.data.frame(filter(TimerDisplayTable, !is.na(TimerDisplayTable$Countdown)))
TimerDisplayTable<-TimerDisplayTable[order(TimerDisplayTable$Countdown, decreasing=FALSE),]





##Shadow Event Timers##
ShadowTimeTable<-read_xlsx("Shadow_Lookup_Table.xlsx")
ShadowTimeTable$Start<-as.POSIXct(ShadowTimeTable$Start, tz="UTC")
ShadowTimeTable$Stop<-as.POSIXct(ShadowTimeTable$Stop, tz="UTC")
ShadowTimeTable$Countdown<-as_hms(ShadowTimeTable$Countdown)
ShadowTimeTable$`Active?`<-as.character(ShadowTimeTable$`Active?`)
ShadowTimeTable<-as.data.frame(ShadowTimeTable)

for(i in 1:1){
  ##Raid the Vault##
  ShadowTimeTable[i, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "12:00:00"), tz='UTC')
  ShadowTimeTable[i, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "13:00:00"), tz='UTC')
  ShadowTimeTable[i+1, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "19:00:00"), tz='UTC')
  ShadowTimeTable[i+1, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "20:00:00"), tz='UTC')
  
  #Shadow Assembly##
  
  SADate<-weekdays(TimeTable[i, "Server_Time"])
  SADay<-TimeTable[i,"Server_Time"]
  if(SADate=="Sunday"){
    ShadowTimeTable[i+2, "Active?"]<-NA
    ShadowTimeTable[i+2, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "18:00:00"), tz='UTC')
    ShadowTimeTable[i+2, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "20:00:00"), tz='UTC')
  }else{
    ShadowTimeTable[i+2, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "18:00:00"), tz='UTC')
    ShadowTimeTable[i+2, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "20:00:00"), tz='UTC')
  }
  
  
  ##Battlegrounds##
  ShadowTimeTable[i+3, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "08:00:00"), tz='UTC')
  ShadowTimeTable[i+3, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "10:00:00"), tz='UTC')
  ShadowTimeTable[i+4, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "12:00:00"), tz='UTC')
  ShadowTimeTable[i+4, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "14:00:00"), tz='UTC')
  ShadowTimeTable[i+5, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "18:00:00"), tz='UTC')
  ShadowTimeTable[i+5, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "20:00:00"), tz='UTC')
  ShadowTimeTable[i+6, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "22:00:00"), tz='UTC')
  ShadowTimeTable[i+6, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "23:59:59"), tz='UTC')
  
  ##Shadow Lottery##
  ShadowTimeTable[i+7, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "12:00:00"), tz='UTC')
  ShadowTimeTable[i+7, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "13:00:00"), tz='UTC')
  ShadowTimeTable[i+8, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "18:00:00"), tz='UTC')
  ShadowTimeTable[i+8, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "19:00:00"), tz='UTC')
  ShadowTimeTable[i+9, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "21:00:00"), tz='UTC')
  ShadowTimeTable[i+9, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "22:00:00"), tz='UTC')
  
  
  ##Shadow War##
  SWDate<-weekdays(TimeTable[i, "Server_Time"])
  SWDay<-TimeTable[i,"Server_Time"]
  while(SWDate!="Thursday"&&SWDate!="Saturday"){
    SWDay<-SWDay+days(1)
    SWDate<-weekdays(SWDay)
  }
  
  ShadowTimeTable[i+10, "Start"]<-as.POSIXct(paste0(date(SWDay)," ", "18:00:00"), tz='UTC')
  ShadowTimeTable[i+10, "Stop"]<-as.POSIXct(paste0(date(SWDay)," ", "21:00:00"), tz='UTC')
  
  #Rite of Exile##
  
  REDate<-weekdays(TimeTable[i, "Server_Time"])
  REDay<-TimeTable[i,"Server_Time"]
  
  while(REDate!="Sunday"){
    REDay<-REDay+days(1)
    REDate<-weekdays(REDay)
  }
  

  ShadowTimeTable[i+11, "Start"]<-as.POSIXct(paste0(date(REDay)," ", "20:00:00"), tz='UTC')
  ShadowTimeTable[i+11, "Stop"]<-as.POSIXct(paste0(date(REDay)," ", "20:30:00"), tz='UTC')
  
  
  
  
  
  
  ##Countdown Timers##
  
  ##Raid the Vault##
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
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
    countdowntime<-as_hms("00:00:00")
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
    countdowntime<-as_hms("00:00:00")
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
      
      if(weekdays(TimeTable[1,"Server_Time"])=="Sunday"){
        ShadowTimeTable[i+2,"Active?"]<-NA
      }
      
  
  ##Battlegrounds##
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+3, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+3, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ShadowTimeTable[i+3,"Countdown"]<-countdowntime
  
    ##Active Logic.##
    if(difftime(ShadowTimeTable[i+3, "Stop"], TimeTable[1,"Server_Time"])<0){
      ShadowTimeTable[i+3,"Active?"]<-NA
    }else if(difftime(ShadowTimeTable[i+3, "Start"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+3,"Active?"]<-"No"
    }else if(difftime(ShadowTimeTable[i+3, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ShadowTimeTable[i+3, "Stop"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+3,"Active?"]<-"Yes"
    }else{
      ShadowTimeTable[i+3,"Active?"]<-NA
    }
  
  
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+4, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+4, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ShadowTimeTable[i+4,"Countdown"]<-countdowntime
  
    ##Active Logic.##
    if(difftime(ShadowTimeTable[i+4, "Stop"], TimeTable[1,"Server_Time"])<0){
      ShadowTimeTable[i+4,"Active?"]<-NA
    }else if(difftime(ShadowTimeTable[i+4, "Start"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+4,"Active?"]<-"No"
    }else if(difftime(ShadowTimeTable[i+4, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ShadowTimeTable[i+4, "Stop"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+4,"Active?"]<-"Yes"
    }else{
      ShadowTimeTable[i+4,"Active?"]<-NA
    }
  
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+5, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+5, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ShadowTimeTable[i+5,"Countdown"]<-countdowntime
  
    ##Active Logic.##
    if(difftime(ShadowTimeTable[i+5, "Stop"], TimeTable[1,"Server_Time"])<0){
      ShadowTimeTable[i+5,"Active?"]<-NA
    }else if(difftime(ShadowTimeTable[i+5, "Start"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+5,"Active?"]<-"No"
    }else if(difftime(ShadowTimeTable[i+5, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ShadowTimeTable[i+5, "Stop"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+5,"Active?"]<-"Yes"
    }else{
      ShadowTimeTable[i+5,"Active?"]<-NA
    }
  
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+6, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+6, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ShadowTimeTable[i+6,"Countdown"]<-countdowntime
    
    ##Active Logic.##
    if(difftime(ShadowTimeTable[i+6, "Stop"], TimeTable[1,"Server_Time"])<0){
      ShadowTimeTable[i+6,"Active?"]<-NA
    }else if(difftime(ShadowTimeTable[i+6, "Start"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+6,"Active?"]<-"No"
    }else if(difftime(ShadowTimeTable[i+6, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ShadowTimeTable[i+6, "Stop"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+6,"Active?"]<-"Yes"
    }else{
      ShadowTimeTable[i+6,"Active?"]<-NA
    }
    
  
  ##Shadow Lottery##
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+7, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+7, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ShadowTimeTable[i+7,"Countdown"]<-countdowntime
  
    ##Active Logic.##
    if(difftime(ShadowTimeTable[i+7, "Stop"], TimeTable[1,"Server_Time"])<0){
      ShadowTimeTable[i+7,"Active?"]<-NA
    }else if(difftime(ShadowTimeTable[i+7, "Start"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+7,"Active?"]<-"No"
    }else if(difftime(ShadowTimeTable[i+7, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ShadowTimeTable[i+7, "Stop"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+7,"Active?"]<-"Yes"
    }else{
      ShadowTimeTable[i+7,"Active?"]<-NA
    }
  
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+8, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+8, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ShadowTimeTable[i+8,"Countdown"]<-countdowntime
  
    ##Active Logic.##
    if(difftime(ShadowTimeTable[i+8, "Stop"], TimeTable[1,"Server_Time"])<0){
      ShadowTimeTable[i+8,"Active?"]<-NA
    }else if(difftime(ShadowTimeTable[i+8, "Start"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+8,"Active?"]<-"No"
    }else if(difftime(ShadowTimeTable[i+8, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ShadowTimeTable[i+8, "Stop"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+8,"Active?"]<-"Yes"
    }else{
      ShadowTimeTable[i+8,"Active?"]<-NA
    }
  
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+9, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+9, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ShadowTimeTable[i+9,"Countdown"]<-countdowntime
  
    ##Active Logic.##
    if(difftime(ShadowTimeTable[i+9, "Stop"], TimeTable[1,"Server_Time"])<0){
      ShadowTimeTable[i+9,"Active?"]<-NA
    }else if(difftime(ShadowTimeTable[i+9, "Start"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+9,"Active?"]<-"No"
    }else if(difftime(ShadowTimeTable[i+9, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ShadowTimeTable[i+9, "Stop"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+9,"Active?"]<-"Yes"
    }else{
      ShadowTimeTable[i+9,"Active?"]<-NA
    }
  
  ##Shadow War##
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+10, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+10, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ShadowTimeTable[i+10,"Countdown"]<-countdowntime
  
    ##Active Logic.##
    if(difftime(ShadowTimeTable[i+10, "Stop"], TimeTable[1,"Server_Time"])<0){
      ShadowTimeTable[i+10,"Active?"]<-NA
    }else if(difftime(ShadowTimeTable[i+10, "Start"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+10,"Active?"]<-"No"
    }else if(difftime(ShadowTimeTable[i+10, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ShadowTimeTable[i+10, "Stop"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+10,"Active?"]<-"Yes"
    }else{
      ShadowTimeTable[i+10,"Active?"]<-NA
    }
  
  ##Rite of Exile##
  countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+11, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ShadowTimeTable[i+11, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ShadowTimeTable[i+11,"Countdown"]<-countdowntime
    ##Active Logic.##
    if(difftime(ShadowTimeTable[i+11, "Stop"], TimeTable[1,"Server_Time"])<0){
      ShadowTimeTable[i+11,"Active?"]<-NA
    }else if(difftime(ShadowTimeTable[i+11, "Start"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+11,"Active?"]<-"No"
    }else if(difftime(ShadowTimeTable[i+11, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ShadowTimeTable[i+11, "Stop"], TimeTable[1,"Server_Time"])>=0){
      ShadowTimeTable[i+11,"Active?"]<-"Yes"
    }else{
      ShadowTimeTable[i+11,"Active?"]<-NA
    }

}

##Immortal Event Timers##
ImmortalTimeTable<-read_xlsx("Immortal_Lookup_Table.xlsx")
ImmortalTimeTable$Start<-as.POSIXct(ImmortalTimeTable$Start, tz="UTC")
ImmortalTimeTable$Stop<-as.POSIXct(ImmortalTimeTable$Stop, tz="UTC")
ImmortalTimeTable$Countdown<-as_hms(ImmortalTimeTable$Countdown)
ImmortalTimeTable$`Active?`<-as.character(ImmortalTimeTable$`Active?`)
ImmortalTimeTable<-as.data.frame(ImmortalTimeTable)

for(i in 1:1){
  ##Defend the Vault##
  ImmortalTimeTable[i, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "12:00:00"), tz='UTC')
  ImmortalTimeTable[i, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "13:00:00"), tz='UTC')
  ImmortalTimeTable[i+1, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "19:00:00"), tz='UTC')
  ImmortalTimeTable[i+1, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "20:00:00"), tz='UTC')
  
  ##Battlegrounds##
  ImmortalTimeTable[i+2, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "08:00:00"), tz='UTC')
  ImmortalTimeTable[i+2, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "10:00:00"), tz='UTC')
  ImmortalTimeTable[i+3, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "12:00:00"), tz='UTC')
  ImmortalTimeTable[i+3, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "14:00:00"), tz='UTC')
  ImmortalTimeTable[i+4, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "18:00:00"), tz='UTC')
  ImmortalTimeTable[i+4, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "20:00:00"), tz='UTC')
  ImmortalTimeTable[i+5, "Start"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "22:00:00"), tz='UTC')
  ImmortalTimeTable[i+5, "Stop"]<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "23:59:59"), tz='UTC')
  
  
  ##Corvus Expedition##
  CEDate<-weekdays(TimeTable[i, "Server_Time"])
  CEDay<-TimeTable[i,"Server_Time"]
  
  while(CEDate!="Monday"&&CEDate!="Wednesday"&&CEDate!="Friday"){
    CEDay<-CEDay+days(1)
    CEDate<-weekdays(CEDay)
  }
  
  ImmortalTimeTable[i+6, "Start"]<-as.POSIXct(paste0(date(CEDay)," ", "20:00:00"), tz='UTC')
  ImmortalTimeTable[i+6, "Stop"]<-as.POSIXct(paste0(date(CEDay)," ", "20:30:00"), tz='UTC')

  #Rite of Exile##
  
  REDate<-weekdays(TimeTable[i, "Server_Time"])
  REDay<-TimeTable[i,"Server_Time"]
  
  while(REDate!="Sunday"){
    REDay<-REDay+days(1)
    REDate<-weekdays(REDay)
  }
  
  
  ImmortalTimeTable[i+7, "Start"]<-as.POSIXct(paste0(date(REDay)," ", "20:00:00"), tz='UTC')
  ImmortalTimeTable[i+7, "Stop"]<-as.POSIXct(paste0(date(REDay)," ", "20:30:00"), tz='UTC')
  

  
  ##Countdown Timers##
  
  ##Defend the Vault##
  countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ImmortalTimeTable[i,"Countdown"]<-countdowntime
  ##Active Logic.##
  if(difftime(ImmortalTimeTable[i, "Stop"], TimeTable[1,"Server_Time"])<0){
    ImmortalTimeTable[i,"Active?"]<-NA
  }else if(difftime(ImmortalTimeTable[i, "Start"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i,"Active?"]<-"No"
  }else if(difftime(ImmortalTimeTable[i, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ImmortalTimeTable[i, "Stop"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i,"Active?"]<-"Yes"
  }else{
    ImmortalTimeTable[i,"Active?"]<-NA
  }
  
  
  countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+1, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+1, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ImmortalTimeTable[i+1,"Countdown"]<-countdowntime
  ##Active Logic.##
  if(difftime(ImmortalTimeTable[i+1, "Stop"], TimeTable[1,"Server_Time"])<0){
    ImmortalTimeTable[i+1,"Active?"]<-NA
  }else if(difftime(ImmortalTimeTable[i+1, "Start"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+1,"Active?"]<-"No"
  }else if(difftime(ImmortalTimeTable[i+1, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ImmortalTimeTable[i+1, "Stop"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+1,"Active?"]<-"Yes"
  }else{
    ImmortalTimeTable[i+1,"Active?"]<-NA
  }

  
  
  ##Battlegrounds##
  countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+2, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+2, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ImmortalTimeTable[i+2,"Countdown"]<-countdowntime
  
  ##Active Logic.##
  if(difftime(ImmortalTimeTable[i+2, "Stop"], TimeTable[1,"Server_Time"])<0){
    ImmortalTimeTable[i+2,"Active?"]<-NA
  }else if(difftime(ImmortalTimeTable[i+2, "Start"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+2,"Active?"]<-"No"
  }else if(difftime(ImmortalTimeTable[i+2, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ImmortalTimeTable[i+2, "Stop"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+2,"Active?"]<-"Yes"
  }else{
    ImmortalTimeTable[i+2,"Active?"]<-NA
  }
  
  
  countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+3, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+3, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ImmortalTimeTable[i+3,"Countdown"]<-countdowntime
  
  ##Active Logic.##
  if(difftime(ImmortalTimeTable[i+3, "Stop"], TimeTable[1,"Server_Time"])<0){
    ImmortalTimeTable[i+3,"Active?"]<-NA
  }else if(difftime(ImmortalTimeTable[i+4, "Start"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+3,"Active?"]<-"No"
  }else if(difftime(ImmortalTimeTable[i+3, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ImmortalTimeTable[i+3, "Stop"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+3,"Active?"]<-"Yes"
  }else{
    ImmortalTimeTable[i+3,"Active?"]<-NA
  }
  
  countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+4, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+4, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ImmortalTimeTable[i+4,"Countdown"]<-countdowntime
  
  ##Active Logic.##
  if(difftime(ImmortalTimeTable[i+4, "Stop"], TimeTable[1,"Server_Time"])<0){
    ImmortalTimeTable[i+4,"Active?"]<-NA
  }else if(difftime(ImmortalTimeTable[i+4, "Start"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+4,"Active?"]<-"No"
  }else if(difftime(ImmortalTimeTable[i+4, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ImmortalTimeTable[i+4, "Stop"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+4,"Active?"]<-"Yes"
  }else{
    ImmortalTimeTable[i+4,"Active?"]<-NA
  }
  
  countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+5, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+5, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }
  
  ImmortalTimeTable[i+5,"Countdown"]<-countdowntime
  
  ##Active Logic.##
  if(difftime(ImmortalTimeTable[i+5, "Stop"], TimeTable[1,"Server_Time"])<0){
    ImmortalTimeTable[i+5,"Active?"]<-NA
  }else if(difftime(ImmortalTimeTable[i+5, "Start"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+5,"Active?"]<-"No"
  }else if(difftime(ImmortalTimeTable[i+5, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ImmortalTimeTable[i+5, "Stop"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+5,"Active?"]<-"Yes"
  }else{
    ImmortalTimeTable[i+5,"Active?"]<-NA
  }
  
  
  #Corvus Expedition##
  countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+6, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+6, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }

  ImmortalTimeTable[i+6,"Countdown"]<-countdowntime

  ##Active Logic.##
  if(difftime(ImmortalTimeTable[i+6, "Stop"], TimeTable[1,"Server_Time"])<0){
    ImmortalTimeTable[i+6,"Active?"]<-NA
  }else if(difftime(ImmortalTimeTable[i+6, "Start"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+6,"Active?"]<-"No"
  }else if(difftime(ImmortalTimeTable[i+6, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ImmortalTimeTable[i+6, "Stop"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+6,"Active?"]<-"Yes"
  }else{
    ImmortalTimeTable[i+6,"Active?"]<-NA
  }


  #Rite of Exile##
  countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+7, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  if(grepl("-",countdowntime)){
    countdowntime<-as_hms("00:00:00")
  }else{
    countdowntime<-round_hms(as_hms(difftime(ImmortalTimeTable[i+7, "Start"], TimeTable[1,"Server_Time"])), digits=0)
  }

  ImmortalTimeTable[i+7,"Countdown"]<-countdowntime
  ##Active Logic.##
  if(difftime(ImmortalTimeTable[i+7, "Stop"], TimeTable[1,"Server_Time"])<0){
    ImmortalTimeTable[i+7,"Active?"]<-NA
  }else if(difftime(ImmortalTimeTable[i+7, "Start"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+7,"Active?"]<-"No"
  }else if(difftime(ImmortalTimeTable[i+7, "Start"], TimeTable[1,"Server_Time"])<=0&&difftime(ImmortalTimeTable[i+7, "Stop"], TimeTable[1,"Server_Time"])>=0){
    ImmortalTimeTable[i+7,"Active?"]<-"Yes"
  }else{
    ImmortalTimeTable[i+7,"Active?"]<-NA
  }
  
}

ImmortalTimeTable<-as.data.frame(filter(ImmortalTimeTable, !is.na(ImmortalTimeTable$Active)))
ImmortalTimeTable<-ImmortalTimeTable[order(ImmortalTimeTable$Countdown, decreasing=FALSE),]

ShadowTimeTable<-as.data.frame(filter(ShadowTimeTable, !is.na(ShadowTimeTable$Active)))
ShadowTimeTable<-ShadowTimeTable[order(ShadowTimeTable$Countdown, decreasing=FALSE),]


##Reset Timers##
ResetTimeTable<-read_xlsx("ResetTable.xlsx")
ResetTimeTable$Countdown<-as_hms(ResetTimeTable$Countdown)
ResetTimeTable$`Reset`<-as.character(ResetTimeTable$`Reset`)
ResetTimeTable$`Notes`<-as.character(ResetTimeTable$`Notes`)
ResetTimeTable<-as.data.frame(ResetTimeTable)



  ##Daily Reset.##
  DailyDay<-TimeTable[1,"Server_Time"]
  DailyResetTime<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "03:00:00"), tz='UTC')


  while(difftime(DailyResetTime,DailyDay)<0){
    DailyResetTime<-DailyResetTime+days(1)
  }

  DailyCountdown<-round_hms(as_hms(difftime(DailyResetTime,DailyDay)), digits=0)
  ResetTimeTable[1, "Countdown"]<-DailyCountdown
  
  
  ##Hilts Limited Items Reset.##
  ##12PM##
  DailyDay<-TimeTable[1,"Server_Time"]
  DailyResetTime<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "12:00:00"), tz='UTC')
  DailyCountdown<-round_hms(as_hms(difftime(DailyResetTime,DailyDay)), digits=0)
  ResetTimeTable[2, "Countdown"]<-DailyCountdown

  ##Hilts Limited Items Reset.##
  ##8PM##
  DailyDay<-TimeTable[1,"Server_Time"]
  DailyResetTime<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "20:00:00"), tz='UTC')
  DailyCountdown<-round_hms(as_hms(difftime(DailyResetTime,DailyDay)), digits=0)
  ResetTimeTable[3, "Countdown"]<-DailyCountdown
  
  
  ##Weekly Reset.##
  
  WeeklyDate<-weekdays(TimeTable[i, "Server_Time"])
  WeeklyDay<-TimeTable[i,"Server_Time"]
  
  while(WeeklyDate!="Monday"){
    WeeklyDay<-WeeklyDay+days(1)
    WeeklyDate<-weekdays(WeeklyDay)
  }
  WeeklyDay<-as.POSIXct(paste0(date(WeeklyDay)," ", "03:00:00"), tz='UTC')
  #WeeklyResetTime<-as.POSIXct(paste0(date(TimeTable[1, "Server_Time"])," ", "20:00:00"), tz='UTC')
  WeeklyCountdown<-round_hms(as_hms(difftime(WeeklyDay, TimeTable[1, "Server_Time"])), digits=0)
  ResetTimeTable[4, "Countdown"]<-WeeklyCountdown
  
  
ResetTimeTable<-as.data.frame(filter(ResetTimeTable, ResetTimeTable$Countdown>0))
ResetTimeTable<-ResetTimeTable[order(ResetTimeTable$Countdown, decreasing=FALSE),]
