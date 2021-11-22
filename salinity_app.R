#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(lubridate)
library(shiny)
library(ggplot2)
library(dataRetrieval)


ui <- fluidPage(
  
  titlePanel("Salinity Max Calculator"),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput("Site","Input Site Number", "02198920") ,
      dateRangeInput("date","Date Range", "2021-01-27", "2021-03-05") ,
      actionButton("button", "Calculate Max Salinity") ,
      downloadButton("downloadData", "Download")
    
    ),
    
    mainPanel(
      plotOutput("SalinityPlot", height = "500")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$button, {
    parameterCd <- "00480"
    
    startDate = input$date[1]
    endDate = input$date[2]
    siteNumber = input$Site
    
    siteINFO <- readNWISsite(siteNumber)
    siteNAME = siteINFO$station_nm
    
    salinity <- readNWISuv(siteNumber, 
                           parameterCd, startDate, endDate)
    
    salinity$dateTime2 =  as.POSIXct(salinity$dateTime, format = "%Y-%m-%d %H:%M:%S") - hours(5)
    salinity$date =  as.Date(format(salinity$dateTime2, "%Y-%m-%d") )
    
    
    salinity2 = aggregate(salinity$X_00480_00000, by = list(salinity$date), max)
    
    
    salinity2$Group.1 = paste0(salinity2$Group.1, " 24:00:00")
    salinity2$Group.2 = as.POSIXct(salinity2$Group.1, format = "%Y-%m-%d %H:%M:%S") - hours(5)
    
    output$SalinityPlot <- renderPlot({
      
      
      ggplot(salinity, aes(dateTime2, X_00480_00000), color = "gray") + geom_line(color = "black") + 
        theme_bw(18) +
        ylab("Salinity (ppt)") + xlab("Date") +
        geom_point(data = salinity2, aes(Group.2,x), color = "red") +
        geom_step(data = salinity2, aes(Group.2,x), color = "red") +
        ggtitle(siteNAME)
    
    
    } )
    data <- salinity2[,1:2]
    colnames(data) = c("Date/Time","Salinity (ppt)")
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(siteNAME, Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(data, file)
      }
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
