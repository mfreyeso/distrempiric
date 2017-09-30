library(shiny)
library(plotly)
library(dplyr)
library(forecast)

#' Define UI for data upload app ----
ui <- fluidPage(

  #' App title ----
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
                           ".csv",
                           ".dat")),
      # Horizontal line ----
      tags$hr(),
      h5("Load File Options"),

      # Input: Checkbox if file has header ----
      checkboxInput(inputId = "header", label = "Header", TRUE),

      htmlOutput("selectColumn"),

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
      sliderInput("freq",
                  "Select TimeSeries Frequency:",
                  min = 1,  max = 365, value = 7),

      # Input: Select number of rows to display ----
      radioButtons("tmodel", "Model Type:",
                   choices = c( BoxJenkins='box_jenkins',
                                Linear= "linear"),
                   selected = "linear")
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      # tableOutput("contents")
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Plot", plotlyOutput("plotTimeSerie")),
                  tabPanel("Histogram", plotlyOutput("histTimeSerie")),
                  tabPanel("ACF", plotOutput("acfTimeSerie")),
                  tabPanel("PACF", plotOutput("pacfTimeSerie")),
                  tabPanel("Adjusted Linear",verbatimTextOutput('model')),
                  tabPanel("Residuals Analysis",plotOutput("residuals")),
                  tabPanel("Forecast t+20",plotOutput("forecast"))
      )
    )
  )
)
