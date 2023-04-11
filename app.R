
library(shiny)
library(markdown)
library(readxl)
library(shinyFiles)
library(rvest)
library(openxlsx)
library(shinyjs)
library(bslib)


library(ggplot2)
library(ggvis)
library(dplyr)
library(hrbrthemes)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
Sys.setenv(TZ='CET')

#Valores default
rutas.fichero <- as.data.frame(read_excel("//filer2usr/dsm/dsm_comun/SINRE simulador/Shiny/fichero_rutas.xlsx", col_names = T))
dir_guardar <- "//filer2usr/dsm/dsm_comun/SINRE simulador/RESULTADOS"
fecha1 <- "2021-01-01"
fecha2 <- "2030-12-31"

reactivos <- reactiveValues(rutas = rutas.fichero,
                            dir = dir_guardar)
#/VAlores Default

source("//filer2usr/dsm/dsm_comun/SINRE simulador/Shiny/simulador_v4.R")
source("//filer2usr/dsm/dsm_comun/SINRE simulador/Shiny/carga_vars.R")
source("//filer2usr/dsm/dsm_comun/SINRE simulador/Shiny/genera_plots.R")
source("//filer2usr/dsm/dsm_comun/SINRE simulador/Shiny/precio_plot.R")
source("//filer2usr/dsm/dsm_comun/SINRE simulador/Shiny/analizador.R")

# Define UI ----
ui <- bootstrapPage( theme = bs_theme(version = 4, bootswatch = "sandstone"),
                     shinyjs::useShinyjs(),
  title = "Simulador SINRE",
  tags$head(tags$link(rel = "shortcut icon", sizes = "180x180", href = "data:image/x-icon;base64,AAABAAEAEBAAAAEAIABoBAAAFgAAACgAAAAQAAAAIAAAAAEAIAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACS1hH0ktYT+JLWE/iS1hP4ktYT8JLWErQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAJLWESyS1hFQktYQBJLWE7CS1hP0ktYT7JLWE/wAAAAAktYQJJLWEWgAAAAAAAAAAAAAAAAAAAAAAAAAAJLWEAQAAAAAktYSzJLWEASS1hAIktYQFJLWEAyS1hAEktYQBJLWEwQAAAAAktYQaAAAAAAAAAAAktYQSJLWEGCS1hAEAAAAAAAAAACS1hMEAAAAAAAAAAAAAAAAktYQBJLWE2QAAAAAAAAAAAAAAACS1hBMAAAAAAAAAACS1hP4ktYT/JLWE/CS1hAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAktYSOJLWE/yS1hP8ktYQ+AAAAAAAAAAAAAAAAJLWE/yS1hP8ktYT/JLWE/QAAAAAAAAAAAAAAAAAAAAAktYT/JLWE/yS1hP8ktYQbAAAAAAAAAAAktYRtJLWEgAAAAAAAAAAAJLWE/yS1hP8ktYT/AAAAAAAAAAAktYT/JLWE/yS1hP0ktYQBAAAAAAAAAAAAAAAAJLWEYSS1hMAktYQCAAAAAAAAAAAktYQCJLWEqSS1hP8ktYT+JLWEdCS1hAIAAAAAAAAAACS1hAIktYQgJLWEASS1hN0ktYT9AAAAAAAAAAAAAAAAAAAAAAAAAAAktYT6AAAAAAAAAAAAAAAAAAAAAAAAAAAktYQBJLWEmiS1hD0ktYT7JLWE/wAAAAAAAAAAAAAAAAAAAAAAAAAAJLWE/yS1hEoAAAAAAAAAAAAAAAAAAAAAJLWEASS1hPwktYT7JLWE6QAAAAAktYQBJLWE+wAAAAAAAAAAAAAAACS1hP8ktYT+AAAAAAAAAAAAAAAAJLWE/wAAAAAktYQkJLWE3CS1hAEktYT9JLWE/gAAAAAktYS2AAAAACS1hAEktYT/JLWE/gAAAAAAAAAAJLWEkyS1hAEktYT9JLWEmgAAAAAAAAAAJLWE9iS1hAMktYT9AAAAACS1hP8ktYQCJLWE/yS1hP4ktYQBJLWE9AAAAAAktYTQJLWEASS1hN0AAAAAAAAAAAAAAAAktYT8AAAAACS1hP8AAAAAAAAAACS1hP8ktYTbAAAAAAAAAAAktYT9JLWEAyS1hP8AAAAAAAAAAAAAAAAAAAAAAAAAACS1hF4ktYQDJLWEAQAAAAAktYT+JLWEDCS1hDIAAAAAJLWEOSS1hJYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACS1hAUAAAAAJLWErAAAAAAktYQSJLWEVQAAAAAAAAAAAAAAAAAAAAAAAAAA/B8AAPw/AAD37wAA+98AAI/jAADDxwAAsY8AALx/AAA+/QAAPvwAAG52AACWaQAAqlUAANZrAAD+9wAA/v8AAA==")),
  navbarPage (div(img(src="https://www.omie.es/sites/default/files/styles/logo/public/logo.png", 
                  style="float:left; padding-left:25px"), 
                  p("DSM", style = {'vertical-align: bottom;text-align: right; font-family:verdana; color:#f2f2f2'}))),

             # p("DSM", style = {'color:white'})),
  h1("Lanzador de simulaciones SINRE",style={'font-family: Garamond, serif;padding-left:25px'}),
  strong("Direccion de seguimiento del mercado (DSM)", style={'padding-left:25px'}),
              br(),
                  
                          sidebarLayout(
                            
                                sidebarPanel(
                                  
                                  width = 4,
                                  textInput("nombre", "Nombre del fichero de resultados:", value = "Nueva_SINRE"),
                                  dateRangeInput("fechas", "Intervalo de fechas:", 
                                                 language = "es", weekstart = 1,   separator = "hasta",
                                                 start = fecha1, end = fecha2,format = "dd/mm/yyyy"),
                                  actionButton("goButton","Elegir directorio para guardar"),
                                  textOutput("dir"),
                                  br(),
                                  actionButton("submit", "Start"),
                                  actionButton("submit2", "Ver inputs"),
                                  verbatimTextOutput("summary"),
                                  tableOutput("test"),
                                  verbatimTextOutput( "console")
                                  ),                      
                                    
                                    
                                    mainPanel(
                                      
                                        tabsetPanel(
                                          tabPanel("1 - Definir las rutas",
                                          
                                            fluidPage(
                                                br(),
                                                downloadButton("rutas", label = "Descargar fichero de rutas por defecto"),
                                                br(),br(),br(),
                                                fileInput("frutas", "Cargar fichero de rutas de los inputs:",accept = c(".xlsx"))
                                              
                                            )
                                          ),
                                          tabPanel("2 - Definir otras variables constantes",
                                                   fluidPage(
                                                     fluidRow(
                                                       column(3,
                                                         verticalLayout(
                                                           h4("Párametros España:"),
                                                           numericInput("pmin.sol","Precio min. FV",2),
                                                           numericInput("pmin.ts","Precio min. termosolar",2),
                                                           numericInput("pmin.eol","Precio min. eólica",3)
                                                           
                                                         )
                                                       ),
                                                       column(3,
                                                         verticalLayout(
                                                           h4("Párametros Portugal:"),
                                                           numericInput("PT_pmin.sol","Precio min. FV",2),
                                                           numericInput("PT_pmin.eol","Precio min. eólica",3) 
                                                         )
                                                       ),
                                                       column(3,
                                                              verticalLayout(
                                                                h4("Párametros Mibel:"),
                                                                numericInput("Pmax.mercado","Precio max. de oferta",3000),
                                                                numericInput("Pmin.mercado","Precio min. de oferta",-500),
                                                                numericInput("r.bat","Rendimiento baterias tanto por uno:", 1,min = 0,max = 1, step = 0.01)
                                                              )
                                                       )                                                       
                                                     )
                                                     
                                                   )  
                                          )  
                                       )
                                    )
                            ),
                          br(),
                           fluidPage(
                             #mainPanel(
                               tabsetPanel(
                                 tabPanel("Dashboard de resultados",
                                          fluidPage(
                                            splitLayout(
                                              dygraphOutput("precios"),
                                              plotlyOutput("mix")
                                            )
                                            
                                          )
                                 ),
                                 tabPanel("Dashboard de inputs",
                                   fluidPage(
                                     verticalLayout(
                                       splitLayout(
                                         dygraphOutput("plot1"),
                                         dygraphOutput("plot2"),
                                         dygraphOutput("plot3"),
                                         dygraphOutput("plot4")
                                       ),
                                       splitLayout(
                                         dygraphOutput("plot5"),
                                         dygraphOutput("plot6"),
                                         dygraphOutput("plot7"),
                                         dygraphOutput("plot8")
                                       ),
                                       splitLayout(
                                         dygraphOutput("plot9"),
                                         dygraphOutput("plot10"),
                                         dygraphOutput("plot11"),
                                         dygraphOutput("plot12")
                                       ),
                                       splitLayout(
                                         dygraphOutput("plot13"),
                                         dygraphOutput("plot14"),
                                         dygraphOutput("plot17"),
                                         dygraphOutput("plot19")
                                       ),
                                       splitLayout(
                                         dygraphOutput("plot20"),
                                         dygraphOutput("plot21"),
                                         dygraphOutput("plot22"),
                                         dygraphOutput("plot23")
                                       ),
                                       splitLayout(
                                         dygraphOutput("plot24"),
                                         dygraphOutput("plot25"),
                                         dygraphOutput("plot27"),
                                         dygraphOutput("plot28")
                                       )                               
                                     )
                                   )
                                 ),
                                 tabPanel("Analizador curva horaria",
                                          sidebarLayout(
                                            
                                            sidebarPanel(
                                            dateInput("fecha.analiza", "Fecha:", value =  fecha1,
                                                      language = "es", weekstart = 1 ,format = "dd/mm/yyyy"),
                                            numericInput("hora.analiza","Hora: ", 1,min = 1,max = 25, step = 1),
                                            splitLayout(
                                            actionButton("submit3", "OK")
                                            #textOutput("flag")
                                            ),
                                            width = 2
                                            
                                          ),
                                          mainPanel(tags$head(tags$style(type="text/css", "
                                                                                    #loadmessage {
                                                                                    position: fixed;
                                                                                    top: 0px;
                                                                                    left: 0px;
                                                                                    width: 100%;
                                                                                    padding: 5px 0px 5px 0px;
                                                                                    text-align: center;
                                                                                    font-weight: bold;
                                                                                    font-size: 100%;
                                                                                    color: #000000;
                                                                                    background-color: #CCFF66;
                                                                                    z-index: 105;
                                                                                    }
                                                                                    ")),
                                            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                             tags$div("Cargando...",id="loadmessage")),
                                            plotlyOutput("curva.agregada"),
                                            width = 7
                                          )
                                          
                                          )
                                 )                                 

                             )

                           )
                          
                  )
                    





  server <- function(input, output, session) {
    

    
    observeEvent(input$frutas,
                 reactivos$rutas <- as.data.frame(read_excel(input$frutas$datapath,  col_names = T))
                 
    )
    

 

    
    observeEvent(input$submit, {
      
      ##Ejecucion algoritmo casacion con captura de mensajes
      withCallingHandlers(
                         fores   <-    algoritmo(reactivos$rutas,
                                                input$fechas,
                                                input$fechas[1],
                                                input$fechas[2],
                                                input$nombre,
                                                reactivos$dir,
                                                input$pmin.sol,
                                                input$pmin.ts,
                                                input$pmin.eol,
                                                input$PT_pmin.sol,
                                                input$PT_pmin.eol,
                                                input$Pmax.mercado,
                                                input$Pmin.mercado,
                                                input$r.bat
                                                 ),
        # can use "warning" instead/on top of "message" to catch warnings too 
        message = function(m) {
          shinyjs::html("console", m$message, F)
        }
      )
      #mix <- pintar_mix(fores)  
      precios <- precio_chart(fores)
      output$mix <- renderPlotly({pintar_mix(fores)})   
      output$precios <- renderDygraph({precios})
  
    })
    
    observeEvent(input$submit2, {
      
      withCallingHandlers(
      #Funcion de carga de vars
      vars <- carga_vars(reactivos$rutas),
      message = function(m) {
        shinyjs::html("console", m$message, F)
      })

      
      withCallingHandlers(            
      plots <- inputs_graficos(vars, input$fechas[1], input$fechas[2]),
      message = function(m) {
        shinyjs::html("console", m$message, F)
      })      
 
      

      
      
      ##Plots
      

      output$plot1 <- renderDygraph({plots$plot1})
      output$plot2 <-renderDygraph({plots$plot2})
      output$plot3 <-renderDygraph({plots$plot3})
      output$plot4 <-renderDygraph({plots$plot4})
      output$plot5 <-renderDygraph({plots$plot5})
      output$plot6 <-renderDygraph({plots$plot6})
      output$plot7 <-renderDygraph({plots$plot7})
      output$plot8 <-renderDygraph({plots$plot8})
      output$plot9 <-renderDygraph({plots$plot9})
      output$plot10 <-renderDygraph({plots$plot10})
      output$plot11 <-renderDygraph({plots$plot11})
      output$plot12 <-renderDygraph({plots$plot12})
      output$plot13 <-renderDygraph({plots$plot13})
      output$plot14 <-renderDygraph({plots$plot14})
      output$plot15 <-renderDygraph({plots$plot15})
      output$plot17 <-renderDygraph({plots$plot17})
      output$plot19 <-renderDygraph({plots$plot19})
      output$plot20 <-renderDygraph({plots$plot20})
      output$plot21 <-renderDygraph({plots$plot21})
      output$plot22 <-renderDygraph({plots$plot22})
      output$plot23 <-renderDygraph({plots$plot23})
      output$plot24 <-renderDygraph({plots$plot24})
      output$plot25 <-renderDygraph({plots$plot25})
      output$plot27 <-renderDygraph({plots$plot27})
      output$plot28 <-renderDygraph({plots$plot28})
      
      
    })
    
    observeEvent(input$submit3, {
      
      
      aa = reactivos$rutas
      bb = input$fecha.analiza
      cc = input$hora.analiza
      dd = input$pmin.sol
      ee = input$pmin.ts
      ff = input$pmin.eol
      gg = input$PT_pmin.sol
      hh = input$PT_pmin.eol
      ii = input$Pmax.mercado
      jj = input$Pmin.mercado
      kk = input$r.bat     
      
      
      output$curva.agregada <- renderPlotly({analizador( aa, bb, cc, dd, ee, ff, gg, hh , ii, jj, kk
        
                                                              # reactivos$rutas,
                                                              # input$fecha.analiza,
                                                              # input$hora.analiza,
                                                              # input$pmin.sol,
                                                              # input$pmin.ts,
                                                              # input$pmin.eol,
                                                              # input$PT_pmin.sol,
                                                              # input$PT_pmin.eol,
                                                              # input$Pmax.mercado,
                                                              # input$Pmin.mercado,
                                                              # input$r.bat
                                                              )})
   
    })
    
    
    observe({
      if(input$goButton > 0){
        
        reactivos$dir <- gsub("\\\\", "/",choose.dir("//filer2usr/dsm/dsm_comun/SINRE simulador/RESULTADOS"))
        output$dir <- renderText(reactivos$dir)
       
        
      
      }
    })   
    
    

    output$rutas<-downloadHandler(
      
      
         filename = function(){
           
           
           "fichero_rutas.xlsx"
           
         },
         content = function(con) {
           write.xlsx(rutas.fichero, con)
         }
    )
    
    
 
    
    
    
    
    
    
    
    
  }




# Run the app ----
shinyApp(ui = ui, server = server)

