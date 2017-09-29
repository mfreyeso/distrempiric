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
      print(input$columnSerie)
      timeseries <- ts(getFileObject()[input$columnSerie], frequency=input$freq, start=c(1,1))
      return(timeseries)
    }
  }

  timeSerieObject <- reactive({
    getTimeSerie()
  })

  output$selectColumn <- renderUI({
    selectInput("columnSerie", "Select a specific column to analyze:", names(getFileObject()))
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
    p <- acf(timeserie,main='ACF Plot')
  })

  output$pacfTimeSerie <- renderPlot({
    timeserie <- timeSerieObject()
    p <- pacf(timeserie,main='PACF Plot')
  })

  output$model <- renderPrint({
    if(!is.null(timeSerieObject())){
      timeserie <- timeSerieObject()
      t<-seq(1:length(timeserie))
      tt<-t*t
      ttt<-t*t*t
      if(input$tmodel=="box_jenkins"){
        #t<-(seq(1,length(timeserie))
        m<-auto.arima(timeserie)
        arimaorder(m)
      }else if(input$tmodel=="linear"){
        m<-lm(timeserie~t)
        m
      }
      else if(input$tmodel=="cuadratic"){
        m<-lm(timeserie~t+tt)
        m
      }
      else if(input$tmodel=="cubic"){
        m<-lm(timeserie~t+tt+ttt)
        m
      }

    }
  })
  output$residuals <- renderPlot({
    timeserie <- timeSerieObject()
    par(mfrow=c(3,1))
    m<-auto.arima(timeserie)
    r<-plot(residuals(m),type="o",col='red')
    p <- acf(residuals(m),main='ACF Plot')
    q<- pacf(residuals(m),main='PACF Plot')

  })

}
