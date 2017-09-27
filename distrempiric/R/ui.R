library(shiny)
# library(nycflights13)
library(plotly)
library(dplyr)


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
                           ".csv",
                           ".dat")),
      # Horizontal line ----
      tags$hr(),
      h5("Load File Options"),

      # Input: Checkbox if file has header ----
      checkboxInput(inputId = "header", label = "Header", TRUE),

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

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),

      # Horizontal line ----
      tags$hr(),

      h5("Time Series Parameters"),

      #
      #
      #
      sliderInput("freq",
                  "Select TimeSeries Frequency:",
                  min = 1,  max = 365, value = 7),

      htmlOutput("selectColumn"),


      # sliderInput("startSubset",
      #             "Select TimeSeries Inital Value:",
      #             min = 1,  max = 365, value = 7),

      # sliderInput("startSet",
      #             "Select TimeSeries Start:",
      #             min = 1,  max = 365, value = 7),


      # Input: Select number of rows to display ----
      radioButtons("tmodel", "Model Type:",
                   choices = c( None='none',
                                Linear= "linear",
                                Cuadratic = "cuadratic",
                                Cubic="cubic"),
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
                  tabPanel("Adjusted Linear",plotlyOutput('model')),
                  tabPanel("Residuals Analysis",plotlyOutput("residuals"))
      )
    )
  )
)
