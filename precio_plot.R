# Library
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)

# Read the data (hosted on the gallery website)
Sys.setenv(TZ='UTC')


precio_chart <- function(fores){

 #Resultados lista
  #
 # dmy_hms(paste(fores$fecha[1], paste0(fores$hora[1]-1,":00:00")), tz="CET")
    data <- data.frame(FECHA = dmy_hms(paste(fores$fecha, paste0(fores$hora-1,":00:00"))),
                       VALOR = fores$proxy )

    
    precios <- xts(x = data$VALOR, order.by = data$FECHA, tzone="UTC")
    
    # Plot a la lista plots
    precios <-
      dygraph(precios, main = "Precios calculados") %>%
      dyOptions( fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#74b879", useDataTimezone = TRUE) %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)
    
  
  
  return(precios)

}

#plots <- inputs_graficos(vars)
