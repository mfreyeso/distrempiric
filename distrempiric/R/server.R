
#' A Server Function
#'
#' This function allows you manage the logic controls and renders or bindings with statistics functions supported.
#' @param input shiny input controls of frontend
#' @param output shiny and plotly outputs of frontend
#' @export
#' server()

server <- function(input, output) {

#' File Object
#'
#' This function support the file object for all statistics functions used on distrempiric.
#' @export
#' getFileObject()

  getFileObject <- function(){
    if(!is.null(input$file1)) {
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      return(df)
    }
  }

#' Time Serie
#'
#' This function allows you manipulate the time serie from file object.
#' @export
#' getTimeSerie()

  getTimeSerie <- function(){
    if(!is.null(getFileObject())) {
      print(input$columnSerie)
      timeseries <- ts(getFileObject()[input$columnSerie], frequency=input$freq, start=c(1,1))
      return(timeseries)
    }
  }

  #reactive
  timeSerieObject <- reactive({
    getTimeSerie()
  })


#' Get Model Serie
#'
#' This function deliver the model generated from the option selected from user.
#' @export
#' getModelSerie()

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
    H<-20
    y.for<-rep(0,H)
    if(input$tmodel=='box_jenkins'){
      plot(forecast(m,H))
      grid()

    } else if(input$tmodel=='linear'){

      y.for<-rep(NA,H+length(timeserie))
      LimInf<-rep(NA,H+length(timeserie))
      LimSup<-rep(NA,H+length(timeserie))
      sigma<-0.2

      for(i in length(timeserie):(length(timeserie)+H)){
        y.for[i]<-coefficients(m)[1]+coefficients(m)[2]*i
        LimInf[i]<-coefficients(m)[1]+coefficients(m)[2]*i-2*sigma*sqrt(i)
        LimSup[i]<-coefficients(m)[1]+coefficients(m)[2]*i+2*sigma*sqrt(i)
      }

      y.for<-ts(y.for,frequency = input$freq,start=c(1,1))
      LimInf<-ts(LimInf,frequency = input$freq,start=c(1,1))
      LimSup<-ts(LimSup,frequency = input$freq,start=c(1,1))
      plot(timeserie,type='l',col='red',ylim=c(min(timeserie,na.rm=TRUE),max(LimSup,na.rm=TRUE)),main='Forecasting 20 steps a-head')
      lines(y.for, col='blue', lwd=2,type='l')
      lines(LimInf,col='darkred')
      lines(LimSup,col='darkred')
      grid()
    }
  })
}
