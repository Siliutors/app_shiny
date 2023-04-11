
library(readxl)
library(RODBC)
library(progress)

#rutas.fichero <- as.data.frame(read_excel("//filer2usr/dsm/dsm_comun/SINRE simulador/Shiny/fichero_rutas.xlsx", col_names = T))



carga_vars <-function(rutas.fichero){




  rutas.fichero$Ruta <- gsub("\\\\", "/", rutas.fichero$Ruta)
  
  print("Cargando variables...")
 message(print("Cargando variables..."))
  
  pb <- progress_bar$new(total = nrow(rutas.fichero), width = 60)
  
  
  vars <- list()
  for (k in 1:nrow(rutas.fichero)){
    
    print(pb$tick())
    
    # assign(
    #         rutas.fichero$Variable[k],
    #         as.data.frame(read_excel(rutas.fichero$Ruta[k], col_names = T)) #Leer excel,
    #         #as.Date(df1$Date, "%d/%m/%y")
    #         #format(df1$Date, format="%d/%m/%Y"),"%Y")
    #         )
    vars[[rutas.fichero$Variable[k]]] <- as.data.frame(read_excel(rutas.fichero$Ruta[k], col_names = T))
    
  }
  


  return (vars)

 }

# 
#vars <- carga_vars(rutas.fichero)

