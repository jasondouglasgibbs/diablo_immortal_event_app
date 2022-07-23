library(dplyr)
library(readxl)
library(lubridate)
library(hms)
library(shinydashboard)
library(kableExtra)
TimeTable<-read_xlsx("DI_Lookup_Table.xlsx")

##Defines the UI.##
ui <- dashboardPage(skin="green",
                    
                    ##Application title.##
                    dashboardHeader(titleWidth=350, title = "Diablo Immortal Timers"),
                    
                    ##Sidebar with inputs.##
                    dashboardSidebar(width=350,
                                     sidebarMenu(
                                       selectInput("server", "Server:", TimeTable$Server_Name))
                                     
                    ),
                    
                    ##Body outputs.##
                    dashboardBody(#tableOutput("TimeTable"),
                                  #textOutput("TimerSTR"), 
                                  tableOutput("TimeTable")
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ##Script to drive Diablo Immortal Timer App.##
  observe({
    TimeTable$Time_Zone_Number<-as.integer(TimeTable$Time_Zone_Number)
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
    TimeTable$System_Time<-now()
    TimeTable$UTC_Time<-now("UTC")
    TimeTable<-as.data.frame(TimeTable)
    TimeTable<-filter(TimeTable, TimeTable$Server_Name==isolate(input$server))
    
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
      
      
      
      
      
      
    }
    
    # output$TimeTable<- renderText({
    #   kable(TimeTable, align = "c", caption="<span style='color: black;'><center><strong>Diagnostic Table</strong></center></span>") %>%
    #     kable_styling(
    #       font_size = 15
    #     )
    #   }
    # )
    
    TimerDisplayTable<-read_xlsx("TimerDisplayTable.xlsx")
    TimerDisplayTable$Countdown<-as_hms(TimerDisplayTable$Countdown)
    
    invalidateLater(1000)
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

    #Ordering and preping for display.##
    TimerDisplayTable<-as.data.frame(filter(TimerDisplayTable, !is.na(TimerDisplayTable$Countdown)))
    TimerDisplayTable<-TimerDisplayTable[order(TimerDisplayTable$Countdown, decreasing=FALSE),]
    rownames(TimerDisplayTable)<-NULL
    
    # output$TimerSTR<-renderText({
    #   str(TimerDisplayTable)
    # })
    # isolate({
    
    output$TimeTable<- renderText({
      kable(TimerDisplayTable, align = "c", caption="<span style='color: black;'><center><strong>Event Timers</strong></center></span>") %>%
        kable_styling(
          font_size = 15
        ) 
    }
    )
    
    
    
    # }
    # )
    
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
