# Library
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)


# fecha1 = "2021-09-27"
# fecha2 = "2021-09-27"

Sys.setenv(TZ='UTC')

inputs_graficos <- function(vars,fecha1,fecha2){

 #Resultados lista
  plots <- list()
  no.plot <- c(15,16,18,26,29,30,31,32)
  #pb <- progress_bar$new(total = (length(vars)-length(no.plot)), width = 60)
  for (k in 1:length(vars)){
    if ( k %in% no.plot){
      next}
    message(print("Cargando gr�ficas..."))
    
    data <- data.frame(FECHA = dmy_hms(paste(vars[[k]]$FECHA, paste0(vars[[k]]$HORA-1,":00:00"))),
                       VALOR = vars[[k]]$VALOR )
    #filtrado
    fecha1 <- dmy_hms(paste(format(as.Date(fecha1),"%d/%m/%Y"),"00:00:00"))
    fechafin <- dmy_hms(paste(format(as.Date(fecha2)+1,"%d/%m/%Y"),"00:00:00"))

    
    data <- data[which(data$FECHA >= fecha1 & data$FECHA <= fechafin),]
    
    don <- xts(x = data$VALOR, order.by = data$FECHA)
    
    # Plot a la lista plots
    plots[[paste0("plot",k)]] <-
      dygraph(don,  main = names(vars)[k]) %>%
      dyOptions( fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#74b879", useDataTimezone = TRUE) %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)
    
  }
  message(print("OK."))
  return(plots)

}

#plots <- inputs_graficos(vars)
