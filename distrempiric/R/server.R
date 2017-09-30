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

  getModelSerie <- function(){
    m <- NULL
    if(!is.null(timeSerieObject())){
      timeserie <- timeSerieObject()
      t<-seq(1:length(timeserie))
      tt<-t*t
      ttt<-t*t*t
      if(input$tmodel=="box_jenkins"){
        m<-auto.arima(timeserie)
        arimaorder(m)
      }else if(input$tmodel=="linear"){
        m<-lm(timeserie~t)
      }
      else if(input$tmodel=="cuadratic"){
        m<-lm(timeserie~t+tt)
      }
      else if(input$tmodel=="cubic"){
        m<-lm(timeserie~t+tt+ttt)
      }
    }
    return(m)

  }

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
    model <- getModelSerie()
    model
  })

  output$residuals <- renderPlot({
    timeserie <- timeSerieObject()
    m <- getModelSerie()
    par(mfrow=c(2,2))
    res<-residuals(m)
    r<-plot(res,type="o",col='red',main = "Residuals")
    hist(res,col='blue',probability = TRUE,breaks = 70,main="Hist of Residuals")
    lines(density(res),col='red',lwd=3)
    p <- acf(res,main='ACF Plot')
    q<- pacf(res,main='PACF Plot')
  })
  output$forecast <- renderPlot({
    timeserie <- timeSerieObject()
    m <- getModelSerie()
    y.for<-rep(0,20)
    if(input$tmodel!='box_jenkins'){

      t.for<-seq(length(timeserie),length(timeserie)+20)

      y.for<-predict(m,h=8)

      plot(y.for,type = 'l')

    } else{
      plot(forecast(m,200))
    }

    #plot(forecast(m,200))



  })

}
