library(tidyverse)
library(readxl)
library(lubridate)
library(hms)
library(shinydashboard)
library(kableExtra)
library(shinyjs)
TimeTable<-read_xlsx("DI_Lookup_Table.xlsx")

##Defines the UI.##
ui <- dashboardPage(skin="green",
                    
                    ##Application title.##
                    dashboardHeader(titleWidth=350, title = "Diablo Immortal Timers"),
                    
                    ##Sidebar with inputs.##
                    dashboardSidebar(width=350,
                                     sidebarMenu(
                                       selectInput("server", "Server:", TimeTable$Server_Name),
                                       actionButton("submit", "Submit")
                                     )
                                     
                    ),
                    
                    ##Body outputs.##
                    dashboardBody(shinyjs::useShinyjs(),tableOutput("TimeTable"),
                                  tableOutput("AncientNightmare12PM"),
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
      
      
    }
    
    output$TimeTable<- renderText({
      kable(TimeTable, align = "c", caption="<span style='color: black;'><center><strong>Summary of Dice Rolls</strong></center></span>") %>%
        kable_styling(
          font_size = 15
        ) 
      }
    )
    
    TimerDisplayTable<-read_xlsx("TimerDisplayTable.xlsx")
    
    invalidateLater(1000)
    if(grepl("-", TimeTable[1,"Time_Zone"])){
      TimeTable[1,"Server_Time"]<-force_tz(TimeTable[1,"UTC_Time"]-hours(TimeTable[1,"Time_Zone_Number"]), 'UTC')
    }else{
      TimeTable[1,"Server_Time"]<-force_tz(TimeTable[1,"UTC_Time"]+hours(TimeTable[1,"Time_Zone_Number"]), 'UTC')
      
    }
    
    
    countdowntime<-as.character(round_hms(as_hms(difftime(TimeTable[1, "Ancient_Nightmare_12PM"], TimeTable[1,"Server_Time"])), digits=0))
    if(grepl("-",countdowntime)){
      countdowntime<-"0"
    }else{
      countdowntime<-as.character(round_hms(as_hms(difftime(TimeTable[1, "Ancient_Nightmare_12PM"], TimeTable[1,"Server_Time"])), digits=0))
    }
    
    TimerDisplayTable[1,2]<-countdowntime
    
    countdowntime<-as.character(round_hms(as_hms(difftime(TimeTable[1, "Ancient_Nightmare_830PM"], TimeTable[1,"Server_Time"])), digits=0))
    if(grepl("-",countdowntime)){
      countdowntime<-"0"
    }else{
      countdowntime<-as.character(round_hms(as_hms(difftime(TimeTable[1, "Ancient_Nightmare_830PM"], TimeTable[1,"Server_Time"])), digits=0))
    }
    
    TimerDisplayTable[2,2]<-countdowntime
    
    countdowntime<-as.character(round_hms(as_hms(difftime(TimeTable[1, "Ancient_Nightmare_10PM"], TimeTable[1,"Server_Time"])), digits=0))
    if(grepl("-",countdowntime)){
      countdowntime<-"0"
    }else{
      countdowntime<-as.character(round_hms(as_hms(difftime(TimeTable[1, "Ancient_Nightmare_10PM"], TimeTable[1,"Server_Time"])), digits=0))
    }
    
    TimerDisplayTable[3,2]<-countdowntime
    
    TimerDisplayTable<-as.data.frame(filter(TimerDisplayTable, TimerDisplayTable$Countdown!="0"))
    
    
    # isolate({
    output$AncientNightmare12PM<-renderTable({
      TimerDisplayTable
      }
    )
    
    
    
    # }
    # )
    
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
