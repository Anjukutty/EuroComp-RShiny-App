# Author : Anjukutty Joseph
# Caution: Please launch the application in web browser view for bteer perfomance

# Load libraries

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

##Function to get data from website###############
readDataFile <- function()
{
  temp = tempfile()
  download.file("https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip",temp)
  con <- unz(temp,"eurofxref-hist.csv")
  dataFile <- read.table(con,header = TRUE, sep=",", na.strings = c("N/A"))
  unlink(temp)
  dataFile$Date = as.Date(dataFile$Date, format = "%Y-%m-%d")
  return(dataFile)
}

# Define UI for application that draws a graph
ui <- dashboardPage(
   
   # Application Name
   dashboardHeader (title = "EuroCompare"),
   #################################SIDE BAR###########################
   dashboardSidebar(  
     #Currency Dropdown 10 currencies
     selectInput("currencyId", "Select currency to compare aganist Euro", 
                                   c("USD","CNY","JPY","INR","GBP",
                                     "CAD","AUD","NOK","JPY","SGD"),
                                   "USD"),
     #  Date range selector: Start from 1999 till date
     dateRangeInput("dateId", "Date Range:", start = "1999-01-22",
                     end=Sys.Date(), min = "1999-01-05", Sys.Date(),
                     format = "mm-dd-yy", separator = "to") 
   ),
#################################DASHBOARD BODY###########################
  dashboardBody(
         fluidRow(
             column(
               status = "primary",
               headerPanel(h3("Euro Foregn Exchange Reference Rates")),
               solidHeader = T,
               br(),
               # To display latest rate 
               h4(span(textOutput("latestRate"),style="color:blue")),
               # To display percentage gain or loss
               span(textOutput("gainLoss"), style="color:red"),
               # To display minimum nd maximum rate in the selected date range
               textOutput("currPerf"),
               br(),
               # to display currency graph.
               # Graph display all rates available from 1999 till date at first.
               plotlyOutput("plotCurrency"),
               width = 12,
               height = "300px"
             )
         )
     )
)


# server logic
server <- function(input, output) {
# REACTIVE FUNCTION to filter data based on date range selected
    d= readDataFile()
    ReadFilterData <- reactive({
    d[(d$Date >= input$dateId[1] & d$Date <= input$dateId[2]),]
    })
# Plot  graph
    output$plotCurrency <- renderPlotly({
    # use reactive function to take data
    data = ReadFilterData()
    data = data[,c("Date",input$currencyId)]
    plot_ly(data,x = ~Date)%>%add_trace(y = ~data[[input$currencyId]],
                                           mode = 'lines', color = I('blue'),
                                           type = 'scatter',hoverinfo = 'x+y')%>%
    config(displayModeBar = F)%>% 
      layout(title = paste('Euro Vs',input$currencyId), font = "sans serif", 
             yaxis = list(title = input$currencyId),
             xaxis = list( 
               rangeselector = list( method = "relayout",
                                     buttons = list(
                                       list( 
                                         count = 1,
                                         label = "1M",
                                         step = "month",
                                         stepmode = "backward"),
                                       list(
                                         count = 3,
                                         label = "3M",
                                         step = "month",
                                         stepmode = "backward"
                                       ),
                                       list(
                                         count = 6,
                                         label = "6M",
                                         step = "month",
                                         stepmode = "backward"
                                       ),
                                       list(
                                         count = 1,
                                         label = "1Y",
                                         step = "year",
                                         stepmode = "backward",
                                         active= T
                                       ),
                                       list(
                                         count = 2,
                                         label = "2Y",
                                         step = "year",
                                         stepmode = "backward"
                                       ),
                                       list(
                                         label = "All",
                                         step = "all"))),
               rangeslider=list(),
               type='date'
             ))
      
  })
    ###########################Today's rate##################################
    output$latestRate <- renderText({
      sCurrency = input$currencyId
      rateToday = subset(d, d$Date== max(d$Date),na.rm = T )
      paste("  Latest", rateToday[,"Date"],":EUR 1=", sCurrency, rateToday[,sCurrency])
    })
  #####################Print percentage gain or loss of currency in one year#####
    
    output$gainLoss <- renderText({
      sCurrency = input$currencyId
      rateToday = subset(d, d$Date== max(d$Date),na.rm = T )
      dNew <- as.POSIXlt(max(d$Date));dNew$year <- dNew$year-1;dNew <- as.Date(dNew)
      rateB4OneYear = subset(d, d$Date== dNew,na.rm = T )
      print(rateB4OneYear[,"Date"])
      gL =((rateToday[,sCurrency] - rateB4OneYear[,sCurrency]) / rateB4OneYear[,sCurrency])*100
      paste('The rate of change in EUR /', sCurrency, " over last one year is:" , round(gL,2) )
    })
    
  ####Print the minimum and maximum rate for selected currency in selected date range#############
    output$currPerf <- renderText({
      data = ReadFilterData()
      sCurrency = input$currencyId
      minP = subset(data, data[[sCurrency]]==min(data[[sCurrency]],na.rm = T ))
      maxP = subset(data, data[[sCurrency]]==max(data[[sCurrency]],na.rm = T ))
      paste('Minimum rate was on: ',minP[,1], '-', minP[,sCurrency],", Maximum rate was on : ", maxP[,1] ,"-",maxP[,sCurrency])
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# References
# https://shiny.rstudio.com/reference/shiny/0.14/dateRangeInput.html
# https://towardsdatascience.com/scrape-data-and-build-a-webapp-in-r-using-rvest-and-shiny-f20d84dd1b74
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/
# https://plot.ly/r/shiny-tutorial/
# https://shiny.rstudio.com/tutorial/
