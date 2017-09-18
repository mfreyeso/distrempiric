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




t <- seq(from=1, to=100)
e <- rnorm(100, mean=0, sd=1)
y <- 10 + (0.7 * t) + e


# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Exploratory Analysis of Time Series"),

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

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

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

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      # tableOutput("contents")
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Plot", plotOutput("plotTimeSerie")),
                  tabPanel("Histogram"),
                  tabPanel("Autocorrelation"),
                  tabPanel("Partial Autocorrelation")
      )

    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {

  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)

    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }

  })

  output$summary <- renderPrint({
    req(input$file1)

    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)

    dataset <- df
    summary(dataset)
  })

  output$plotTimeSerie <- renderPlot({
    plot(y, type="l", xlim=c(0,120), ylim=c(0,120))
    # p <- plot_ly(x =t, y =y, mode = 'lines', text = paste(t, "days from today"))
  })

}

# Create Shiny app ----
shinyApp(ui, server)
