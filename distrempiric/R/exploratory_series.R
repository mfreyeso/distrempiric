# Serie de Tiempo
#
# # Graphic of Time Serie
#
# Hist
# Autocorrelacion
# Autocorrelacion Parcial
#
library(shiny)
library(nycflights13)
library(plotly)
library(dplyr)

## JetBlue departure_delay and origin airport
jetblue<-flights%>%filter(carrier=='B6')%>%
  select(year:day,dep_delay,origin,dest,distance)%>%
  group_by(month)%>%
  summarize(mean_delay_dep=mean(dep_delay,na.rm=TRUE), #Departure Delay on average
            mean_distance=mean(distance,na.rm = TRUE)) #Distance on average
## AmericanAirlines departure_delay and origin airport
americanairlines<-flights%>%filter(carrier=='AA')%>%
  select(year:day,dep_delay,origin,dest,distance)%>%
  group_by(month)%>%
  summarize(mean_delay_dep=mean(dep_delay,na.rm=TRUE), #Deporture Delay on average
            mean_distance=mean(distance,na.rm = TRUE)) #Distance on average

#The main idea here is to figure out if a low cost airline has differences between a high-class airline
#Podemos hacer un modelo con el tiempo y mirar si se ajusta bien

t <- seq(from=1, to=100)
e <- rnorm(100, mean=0, sd=1)
y <- 10 + (0.7 * t) + e


# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  headerPanel(
    h3("Exploratory Analysis of Time Series")
  ),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Horizontal line ----
      tags$hr(),
      h5("Load File Options"),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", FALSE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),

      # Horizontal line ----
      tags$hr(),

      h5("Time Series Parameters"),
      #
      #
      # # Input: Select number of rows to display ----
      # radioButtons("disp", "Display",
      #              choices = c(Head = "head",
      #                          All = "all"),
      #              selected = "head")

      # selectInput(inputId = "column",
      #             label = "Choose a column:",
      #             choices = c("rock", "pressure", "cars"))

      htmlOutput("selectColumn")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      # tableOutput("contents")
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Plot", plotlyOutput("plotTimeSerie")),
                  tabPanel("Histogram", plotlyOutput("histTimeSerie")),
                  tabPanel("Autocorrelation", plotOutput("acfTimeSerie")),
                  tabPanel("Partial Autocorrelation", plotOutput("pacfTimeSerie")),
                  tabPanel("Adjusted Linear",plotlyOutput('model')),
                  tabPanel("Residuals Analysis",plotlyOutput("residuals"))
      )

    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {

  getFileObject <- function(){
    if(!is.null(input$file1)) {
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      return(df)
    }
  }

  getTimeSerie <- function(){
    if(!is.null(getFileObject())) {
      timeseries <- ts(getFileObject(), frequency=12, start=c(1946,1))
      return(timeseries)
    }
  }

  timeSerieObject <- reactive({
    getTimeSerie()
  })

#
#   output$contents <- renderTable({
#
#     # input$file1 will be NULL initially. After the user selects
#     # and uploads a file, head of that data file by default,
#     # or all rows if selected, will be shown.
#
#     # req(input$file1)
#
#     df <- read.csv(timeSerieObject()$datapath,
#                    header = input$header,
#                    sep = input$sep,
#                    quote = input$quote)
#     return(df)
#
#   })

  output$selectColumn <- renderUI({
    selectInput("columnSerie", "Select your choice", names(getFileObject()))
  })

  output$summary <- renderPrint({
      if(!is.null(timeSerieObject())){
        timeserie <- timeSerieObject()
        summary(timeserie)
      }
  })

  output$plotTimeSerie <- renderPlotly({
    if(!is.null(timeSerieObject())){
      timeserie <- timeSerieObject()
      p <- plot_ly(x = ~time(timeserie), y = ~timeserie, mode = 'lines', text = paste(time(timeserie), "days from today"))
    }
  })

  output$histTimeSerie <- renderPlotly({
    timeserie <- timeSerieObject()
    p <- plot_ly(x =~timeserie, type = "histogram", histnorm = "probability")
  })

  output$acfTimeSerie <- renderPlot({
    timeserie <- timeSerieObject()
    p <- acf(timeserie)
  })

  output$pacfTimeSerie <- renderPlot({
    timeserie <- timeSerieObject()
    p <- pacf(timeserie)
  })
}

# Create Shiny app ----
shinyApp(ui, server)
