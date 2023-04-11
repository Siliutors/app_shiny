library(xlsx)
library(readxl)
library(RODBC)
library(progress)
library(plotly)



 # rutas.fichero <- as.data.frame(read_excel("//filer2usr/dsm/dsm_comun/SINRE simulador/Shiny/fichero_rutas.xlsx", col_names = T))
 # fecha.analiza = "2021-01-14"
 # hora.analiza = 1
 # pmin.sol <- 2
 # pmin.ts <- 2
 # pmin.eol <- 2
 # PT_pmin.sol <- 2
 # PT_pmin.eol <- 3
 # Pmax.mercado <- 3000
 # Pmin.mercado <- -500

analizador <-function( rutas.fichero,
                       fecha.analiza,
                       hora.analiza,
                       pmin.sol,
                       pmin.ts,
                       pmin.eol,
                       PT_pmin.sol,
                       PT_pmin.eol,
                       Pmax.mercado,
                       Pmin.mercado,
                       r.bat){
#

  


  
  
  
  
  
  rutas.fichero$Ruta <- gsub("\\\\", "/", rutas.fichero$Ruta)
  

  
  for (k in 1:nrow(rutas.fichero)){
    
    
    
    assign(
      rutas.fichero$Variable[k],
      as.data.frame(read_excel(rutas.fichero$Ruta[k], col_names = T)) #Leer excel,
      #as.Date(df1$Date, "%d/%m/%y")
      #format(df1$Date, format="%d/%m/%Y"),"%Y")
    )
    
  }
  
  
  # check <- lapply(ls(), function(x){head(get(x))}) #check vars
  #print(check)
  
  
  ##Adapt vars
  cCCmodel <- data.frame(FECHA = co2$FECHA, HORA = co2$HORA, VALOR = 2*gas$VALOR+0.35*co2$VALOR)
  cCAmodel <- data.frame(FECHA = co2$FECHA, HORA = co2$HORA, VALOR = 0.4*api2$VALOR+co2$VALOR)
  
  
  
  ##Mecanismo max / min
  maxcchist <- max(na.omit(costeshist$CC))
  mincchist <- min(na.omit(costeshist$CC))
  maxcahist <- max(na.omit(costeshist$CA))
  mincahist <- min(na.omit(costeshist$CA))
  multiprecio_cc <- 1
  multiprecio_ca <- 1
  ##//Mecanismo max/min
  
  
  #formato de resultados
  fores = data.frame(
    f1 = "Resultado curvas",
    fecha = 0, hora = 0, proxy = 0, x = 0, uof.marginal = 0, separador1 = "",
    f2 = "Portfolio ES",
    cc = 0, ca = 0, nu = 0, hi= 0, eo = 0, sf = 0, st=0, cog = 0, mh = 0, re= 0, g.bom = 0, g.bat = 0,
    demES = 0, c.bom = 0,  c.bat = 0,
    separador2 = "", f3 = "Portoflio PT",
    PTcc = 0, PTca = 0, PTnu = "", PThi = 0, PTeo = 0, PTsf = 0, PTst = "", PTcog = 0, PTmh = "", PTg.bom = 0, PTg.bat ="", PTre ="",
    demPT = 0, PTc.bom = 0, PTc.bat = "",
    separador3 = "", f4 = "Intercon.",
    imp.expES = 0, imp.expPT = 0, imp.expFR = 0,
    separador4 = "", f5 = "Vertidos",
    vertido = 0,nocasmin.sf = 0, nocasmin.st = 0, nocasmin.eo = 0, nocasmin.PTeo = 0, nocasmin.PTsf = 0,
    ofemin.sf = 0, ofemin.st = 0, ofemin.eo = 0, ofemin.PTeo = 0, ofemin.PTsf = 0,
    separador5 = "", f6 = "Inputs",
    api2 = 0, gas = 0, co2 = 0, futurosFR = 0, tvca = 0, tvcc = 0,
    demanda.ES = 0, previ.FV.ES = 0, previ.TS.ES = 0, previ.EO.ES = 0, previ.NK.ES = 0, previ.HidroFluye.ES = 0, previ.mh = 0, previ.HuecoTer.ES = 0,
    camax.ES = 0, ccmax.ES = 0, cogmax.ES = 0, hiUGH.ES = 0,ofebomES = 0, capmax.bat = 0,
    demanda.PT = 0, previ.FV.PT = 0, previ.EO.PT = 0, previ.HidroFluye.PT = 0, previ.HuecoTer.PT = 0,
    camax.PT = 0, ccmax.PT = 0, cogmax.PT = 0, hiUGH.PT = 0, ofebomPT = 0,
    capinterPT = 0, capinterFR = 0,
    ofeb = 0, P.B.compra = 0, P.B.venta = 0, ofe.bat = 0,
    pminsf = 0, pmints = 0, pmineol = 0, pminsfPT = 0, pmineolPT = 0,  tfijo = 0, pmax.mercado = 0, pmin.mercado = 0, R.bat = 0
    
  )
  ##Dif bombeo - embalse
  difb <- 0
  ratiob <- 0
  ratiob_PT <- 0
  acum.c.bom <- 0
  acum.c.bom_PT <- 0
  acum.g.bom <- 0
  acum.g.bom_PT <- 0
  ##Contador bombeo x h
  contador_bom_ES <- 0
  contador_bom_PT <- 0
  carga <- 0
  descargo <- 0
  carga_ES <- 0
  descargo_ES <- 0
  carga_PT <- 0
  descargo_PT <- 0
  horasbom = 0.7
  referencia.precio.b <- 40
  vector.proxy <- c(40)
  ##//Contador bombeo x h
  

  


    
    

    
    wd1 = as.numeric(format(as.Date(fecha.analiza),"%u"))
    mes1 = as.numeric(format(as.Date(fecha.analiza),"%m"))
    
    fecha.estimacion = format(as.Date(fecha.analiza),"%d/%m/%Y")
    
    
    ccinput <-   cCCmodel[which(as.Date(cCCmodel$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)
                                & cCCmodel$HORA == 1 ),]$VALOR
    
    cainput <-   cCAmodel[which(as.Date(cCAmodel$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)
                                & cCAmodel$HORA == 1 ),]$VALOR
    
    demandainput<- demanda.ES[which(as.Date(demanda.ES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    
    co2input<- co2[which(as.Date(co2$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    gasinput<- gas[which(as.Date(gas$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    api2input<- api2[which(as.Date(api2$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    
    camaxinput<- camax.ES[which(as.Date(camax.ES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    ccmaxinput<- ccmax.ES[which(as.Date(ccmax.ES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    remaxinput<- cogmax.ES[which(as.Date(cogmax.ES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    
    hidrofluyeinput<- previ.HidroFluye.ES[which(as.Date(previ.HidroFluye.ES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    numaxinput<- previ.NK.ES[which(as.Date(previ.NK.ES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    fvinput <-  previ.FV.ES[which(as.Date(previ.FV.ES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    tsolarinput <-  previ.TS.ES[which(as.Date(previ.TS.ES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    eolinput <-  previ.EO.ES[which(as.Date(previ.EO.ES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]	
    mhinput <-  previ.minihidro.ES[which(as.Date(previ.minihidro.ES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    eolinput <-  previ.EO.ES[which(as.Date(previ.EO.ES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]	
    ptinput <- capinterPT[which(as.Date(capinterPT$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    frinput <- capinterFR[which(as.Date(capinterFR$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]	
    futurosfrinput <- futurosFR[which(as.Date(futurosFR$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    tfijoinput <- tfijo[which(as.Date(tfijo$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    hidroUGH.ESinput <- previ.HidroUGH.ES[which(as.Date(previ.HidroUGH.ES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    ofebombeoESinput <- ofebombeoES[which(as.Date(ofebombeoES$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    capbateriasinput <- capmax.bat[which(as.Date(capmax.bat$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    ofebateriasinput <- ofebaterias[which(as.Date(ofebaterias$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    #PT
    PTdemandinput = demanda.PT[which(as.Date(demanda.PT$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    PTfvinput = previ.FV.PT[which(as.Date(previ.FV.PT$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    PTeolinput = previ.EO.PT[which(as.Date(previ.EO.PT$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    PTfluyehidroinput = previ.HidroFluye.PT[which(as.Date(previ.HidroFluye.PT$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    PTcamaxinput = camax.PT[which(as.Date(camax.PT$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    PTccmaxinput = ccmax.PT[which(as.Date(ccmax.PT$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    hidroUGH.PTinput <- previ.HidroUGH.PT[which(as.Date(previ.HidroUGH.PT$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    ofebombeoPTinput <- ofebombeoPT[which(as.Date(ofebombeoPT$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    cogmax.PTinput <- cogmax.PT[which(as.Date(cogmax.PT$FECHA,"%d/%m/%Y") == as.Date(fecha.analiza)),]
    #/PT      

      j = hora.analiza
      hora1 = j  
      
      api2j = api2input[which(api2input$HORA == j),3]
      co2j = co2input[which(co2input$HORA == j),3]
      gasj = gasinput[which(gasinput$HORA == j),3]
      demandaj = demandainput[which(demandainput$HORA == j),3]
      hidrofluyej = hidrofluyeinput[which(hidrofluyeinput$HORA == j),3] 
      
      numaxj = numaxinput[which(numaxinput$HORA == j),3]
      camaxj = camaxinput[which(camaxinput$HORA == j),3] 
      ccmaxj = ccmaxinput[which(ccmaxinput$HORA == j),3] 
      remaxj = remaxinput[which(remaxinput$HORA == j),3] 
      fvj <-fvinput[which(fvinput$HORA == j),3] 
      tsolarj <-tsolarinput[which(tsolarinput$HORA == j),3] 
      eolj <-eolinput[which(eolinput$HORA == j),3] 
      mhj <-mhinput[which(mhinput$HORA == j),3] 
      ptj<-ptinput[which(ptinput$HORA == j),3] 
      frj<-frinput[which(frinput$HORA == j),3]
      futurosfrj<-futurosfrinput[which(futurosfrinput$HORA == j),3]
      tfijoj<-tfijoinput[which(tfijoinput$HORA == j),3]
      hidroUGH.ESj <- hidroUGH.ESinput[which(hidroUGH.ESinput$HORA == j),3]
      ofebombeoESj <- ofebombeoESinput[which(ofebombeoESinput$HORA == j),3]
      capbateriasj = capbateriasinput[which(capbateriasinput$HORA == j),3]  
      ofebateriasj = ofebateriasinput[which(ofebateriasinput$HORA == j),3] 
      
      # #PT
      PTdemandj = PTdemandinput[which(PTdemandinput$HORA == j),3] 
      PTfvj = PTfvinput[which(PTfvinput$HORA == j),3] 
      PTeolj = PTeolinput[which(PTeolinput$HORA == j),3] 
      PTfluyehidroj = PTfluyehidroinput[which(PTfluyehidroinput$HORA == j),3] 
      PTcamaxj = PTcamaxinput[which(PTcamaxinput$HORA == j),3] 
      PTccmaxj = PTccmaxinput[which(PTccmaxinput$HORA == j),3] 
      hidroUGH.PTj <- hidroUGH.PTinput[which(hidroUGH.PTinput$HORA == j),3]
      ofebombeoPTj <- ofebombeoPTinput[which(ofebombeoPTinput$HORA == j),3]
      cogmax.PTj <- cogmax.PTinput[which(cogmax.PTinput$HORA == j),3]
      
      # #/PT
      # 
      
      
      
      ccca_histj <- costeshist[which(wd1 == costeshist$WD 
                                     & mes1 == as.numeric(format(as.Date(costeshist$FECHA, "%Y-%m-%d"),"%m"))),]
      
      fechasccj = paste("to_date('",format(as.Date(ccca_histj[which.min(abs(ccca_histj[,3]-ccinput)),][,2]), "%d/%m/%Y"),"')", sep= "")
      fechascaj = paste("to_date('",format(as.Date(ccca_histj[which.min(abs(ccca_histj[,4]-cainput)),][,2]), "%d/%m/%Y"),"')", sep= "")    
      
      ##Mecanismo max / min
      
      cchist_ref <- ccca_histj[which.min(abs(ccca_histj[,3]-ccinput)),][,3]
      multiprecio_cc <- abs(ccinput)/cchist_ref
      
      cahist_ref <- ccca_histj[which.min(abs(ccca_histj[,4]-cainput)),][,4]
      multiprecio_ca <- abs(cainput)/cahist_ref
      
      
      
      
      
      
      ##CONSULTA DE VOLUMEN OFERTADO ESE DIA para Coeficientes de ajute de volumen ofertado por termicas 
      channel <- odbcConnect("OMCONSUL", uid="DSM_R",  pwd="DSM_R")
      rex <- sqlQuery(channel, paste("select * from
                                     (
                                     
                                     select * from
                                     (
                                     select  tecnologia, sum(ofertado) ofertado
                                     from
                                     (
                                     
                                     select a.fecha, a.hora, A.CDUNIOFE, ofertado, case when a.tipo = 'O' then a.PRECIO_OFERTA_EUR else 0 end PRECIO_OFERTA_EUR, 
                                     case when a.cdnupais = 2 then 'CC_PT' else a.idtec1 end TECNOLOGIA
                                     from
                                     detalle_ofertas_md a, det_tecno_uof_dia c where --A.TIPO = 'O'
                                     A.FECHA = C.FECHA and A.CDUNIOFE = C.CDUNIOFE
                                     and 
                                     A.FECHA = ",fechasccj,"
                                     and a.idtec1 = 'CC'
                                     and A.CDNUPAIS in (1,3,5,2)
                                     and A.HORA = ", hora1 ,"
                                     
                                     ) 
                                     group by tecnologia
                                     order by tecnologia
                                     )    
                                     union all 
                                     
                                     select * from
                                     (
                                     select  tecnologia, sum(ofertado) ofertado
                                     from
                                     (
                                     
                                     select a.fecha, a.hora, A.CDUNIOFE, ofertado, case when a.tipo = 'O' then a.PRECIO_OFERTA_EUR else 0 end PRECIO_OFERTA_EUR, 
                                     case when a.cdnupais = 2 then 'CA_PT' else a.idtec1 end TECNOLOGIA
                                     from
                                     detalle_ofertas_md a, det_tecno_uof_dia c where --A.TIPO = 'O'
                                     A.FECHA = C.FECHA and A.CDUNIOFE = C.CDUNIOFE
                                     and 
                                     A.FECHA = ",fechascaj,"
                                     and a.idtec1 = 'CA'
                                     and A.CDNUPAIS in (1,3,5,2)
                                     and A.HORA = ", hora1 ,"
                                     
                                     ) 
                                     group by tecnologia
                                     order by tecnologia
                                     )
                                     
                                     union all 
                                     
                                     select * from
                                     (
                                     select case when A.CDNUPAIS = 1 then 'HI MAX' else 'PT HI MAX' end tecnologia,
                                     sum(B.PTMAXIMA) ofertado
                                     from unidad_ofertante_tabla a, potencia_uof b
                                     where a.cduniofe = b.cduniofe 
                                     and ",fechasccj," between a.fdesde and a.fhasta
                                     and ",fechasccj," between b.fdesde and b.fhasta
                                     and A.IDTEC1 = 'HI'
                                     group by a.cdnupais
                                     
                                     ) 
                                     
                                     
                                     )
                                     "))
      
      odbcClose(channel)
      ##//Consulta RE para hora, fecha  
      
      xca = rex[which(rex[,1] == "CA"),2]
      if (length(xca) == 0){xca = 0}  	
      if (is.na(xca) == TRUE){xca = NULL }
      
      xcc = rex[which(rex[,1] == "CC"),2]
      if (length(xcc) == 0){xcc = 0}
      if (is.na(xcc) == TRUE){xcc = NULL }
      
      
      
      #PT
      PT_xca = rex[which(rex[,1] == "CA_PT"),2]
      if (length(PT_xca) == 0){PT_xca = 0}    	
      if (is.na(PT_xca) == TRUE){PT_xca = NULL }
      
      PT_xcc = rex[which(rex[,1] == "CC_PT"),2]
      if (length(PT_xcc) == 0){PT_xcc = 0} 
      if (is.na(PT_xcc) == TRUE){PT_xcc = NULL }
      
      
      
      multi.ca = 1 + ((camaxj - xca)/xca)
      if (is.nan(multi.ca)){multi.ca <-0} 
      if (is.infinite(multi.ca)){multi.ca <-0} 
      multi.cc = 1 + ((ccmaxj - xcc)/xcc)
      if (is.nan(multi.cc)){multi.cc <-0} 
      if (is.infinite(multi.cc)){multi.cc <-0}         
      
      PT_multi.ca = 1 + ((PTcamaxj - PT_xca)/PT_xca)
      if (is.nan(PT_multi.ca)){PT_multi.ca <-0} 
      if (is.infinite(PT_multi.ca)){PT_multi.ca <-0} 
      PT_multi.cc = 1 + ((PTccmaxj - PT_xcc)/PT_xcc)
      if (is.nan(PT_multi.cc)){PT_multi.cc <-0} 
      if (is.infinite(PT_multi.cc)){PT_multi.cc <-0}           
      
      
      
      
      
      difbmaxj = 4000000 #MWh = +-4 TWh dif bombeo = 8 TWh almacenamiento bombeo.
      referencia.precio.b <- mean(vector.proxy[max(1,(length(vector.proxy)-(30*24))):length(vector.proxy)])
      #Tramos bombeo
      bombeov = ""
      bombeoc = ""
      bombeov_PT = ""
      bombeoc_PT = "" 
      
      
      p.b.compra <- Pmax.mercado #abs(ofebombeoj)
      p.b.venta <-  (pmin.eol+0.1)#6.1 #ofebombeoj
      
      if (ofebombeoESj < 0){
        
        
        bombeoc <- data.frame(OFERTADO = c(abs(ofebombeoESj)),
                              PRECIO = c(abs(p.b.compra)),
                              IDTEC1 = c('Consumo Bombeo'))
        
        
        
      }   
      
      if (ofebombeoESj > 0){
        
        bombeov <-paste("
                        union all
                        
                        select * from (                                      
                        --bombeo venta--------------------------------
                        
                        select", max((ofebombeoESj),0) ,"ofertado, ", abs(p.b.venta) ," precio, 'Generacion Bombeo' idtec1
                        from dual 
                        
                        --//////bombeo venta-----------------
                        )
                        
                        
                        ")
        
      }  
      
      if (ofebombeoPTj < 0){
        
        
        bombeoc_PT <- data.frame(OFERTADO = c(abs(ofebombeoPTj)),
                                 PRECIO = c( abs(p.b.compra)),
                                 IDTEC1 = c('Consumo Bombeo PT'))
        
        
        
      }   
      if (!is.data.frame(bombeoc)){
        bombeoc <- bombeoc_PT
      }else if (!is.data.frame(bombeoc_PT)){
        bombeoc <- bombeoc 
      }else{
        bombeoc <- rbind(bombeoc, bombeoc_PT)
      }
      
      if (ofebombeoPTj > 0){
        
        bombeov_PT <-paste("
                           
                           union all
                           
                           select * from (                                      
                           --bombeo venta--------------------------------
                           
                           select", ofebombeoPTj ,"ofertado, ", (abs(p.b.venta)+0.01) ," precio, 'Generacion Bombeo PT' idtec1
                           from dual 
                           
                           --//////bombeo venta------------------
                           )                     ")
          
      }  
      
      bombeov = paste0(bombeov,bombeov_PT)
      #//TRamos bombeo 	  
      
      
      
      #Tramos baterias
      bateriav = ""
      bateriac = ""
      
      
      if (ofebateriasj < 0){
        
        
        bateriac <- data.frame(OFERTADO = capbateriasj,
                               PRECIO = Pmax.mercado,
                               IDTEC1 = "Consumo Bateria"
        )
        
        
        
      }   
      
      if (ofebateriasj > 0){
        
        bateriav <-paste("
                         union all
                         
                         select * from (                                      
                         --bateria venta--------------------------------
                         
                         select", r.bat*capbateriasj,"ofertado, ", Pmin.mercado ," precio, 'Generacion Bateria' idtec1
                         from dual 
                         
                         --//////bateria venta-----------------
                         )
                         ")
        
      }  
      
      
      #INT FR venta
      frSQLv <-paste("
                     union all
                     
                     select * from (                                      
                     --INT a 0--------------------------------
                     
                     select", abs(frj) ,"ofertado, ", (futurosfrj+0.1) ," precio, 'Int.FR' idtec1
                     from dual 
                     
                     --//////INT a 0------------------
                     )")	
        
      
      #INT FR venta
      
      
      
      
      #Consulta de DEMANDA
      
      OFERTADO <- c(demandaj,  PTdemandj,  frj, 100000)
      PRECIO <- c(Pmax.mercado, Pmax.mercado, (futurosfrj-0.1),  Pmin.mercado)
      IDTEC1<- c("Comercializadora",  "PT Comercializadora",
                 "Int.FR",  "corte")
      
      d1 <- data.frame(OFERTADO,PRECIO,IDTEC1)
      #Bombeo
      if (length(bombeoc) > 1){
        d1 <- rbind(d1,bombeoc)
      }
      #/Bombeo
      #Bateria
      if (length(bateriac) > 1){
        d1 <- rbind(d1,bateriac)
      }
      #/Bateria
      
      d1 <- d1[order(d1$PRECIO, decreasing = T),]
      x <- cumsum(d1$OFERTADO)
      x2 <- c(0,x[-length(x)])
      
      d11 <- cbind(d1,x)
      d12 <- cbind(d1,x = x2)
      demanda <- rbind(d11,d12)
      demanda <- demanda[order(demanda$x),]
      
      
      
      #////Consulta de DEMANDA
      
      #Consulta oferta
      channel <- odbcConnect("OMCONSUL", uid="DSM_R",  pwd="DSM_R")
      oferta <- sqlQuery(channel, paste("select ofertado, precio, idtec1, x from
                                        (
                                        select * from
                                        (
                                        --SIN LAGEAR
                                        select ofertado, precio, idtec1, sum(ofertado) over (order by precio, 
                                        (case when IDTEC1 = 'NU' then 1
                                        when IDTEC1 = 'TNR' then 2
                                        when IDTEC1 = 'TR' then 3
                                        when IDTEC1 = 'PT COG' then 3.1
                                        when IDTEC1 = 'MHI' then 4
                                        when IDTEC1 = 'Fluyente' then 5
                                        when IDTEC1 = 'PT Fluyente' then 5.1
                                        when IDTEC1 = 'SF' then 6
                                        when IDTEC1 = 'PT SF' then 6.1
                                        when IDTEC1 = 'EO' then 7
                                        when IDTEC1 = 'PT EO' then 7.1
                                        when IDTEC1 = 'RRE' then 8
                                        when IDTEC1 = 'ST' then 9
                                        when IDTEC1 = 'HHG' then 10
                                        when IDTEC1 = 'PT HI' then 10.1
                                        when IDTEC1 = 'CA' then 11
                                        when IDTEC1 = 'PT CA' then 11.1
                                        when IDTEC1 = 'CC' then 12
                                        when IDTEC1 = 'PT CC' then 12.1
                                        when IDTEC1 = 'Int.PT' then 13
                                        when IDTEC1 = 'Int.FR' then 14
                                        else 15 end )asc             
                                        , ofertado asc) x
                                        from
                                        (
                                        SELECT sum(OFERTADO) ofertado, PRECIO, IDTEC1
                                        FROM
                                        (
                                        select * from
                                        (
                                        --CC--------------------------------
                                        select A.OFERTADO*", multi.cc, " ofertado,
                                        (((case when A.PRECIO_OFERTA_EUR >= a.tervaeur and a.tipo = 'O' then a.precio_oferta_eur 
                                        when A.PRECIO_OFERTA_EUR < a.tervaeur and a.tipo = 'O' then a.tervaeur 
                                        else 0 end)*", multiprecio_cc ,")+", tfijoj,") precio,
                                        idtec1
                                        from detalle_ofertas_md a where
                                        --A.TIPO = 'O'
                                        --and 
                                        A.FECHA =
                                        (
                                        -- filtros fechas CC
                                        ", fechasccj,"
                                        --// filtros fechas CC
                                        )
                                        
                                        and A.HORA = ", hora1, " and A.IDTEC1 in ('CC')
                                        and a.cdnupais in (1,3,5)
                                        --//////CC------------------
                                        )
                                        
                                        union all
                                        
                                        select * from
                                        (
                                        --PT CC--------------------------------
                                        select A.OFERTADO*", PT_multi.cc, " ofertado,
                                        (((case when A.PRECIO_OFERTA_EUR >= a.tervaeur and a.tipo = 'O' then a.precio_oferta_eur 
                                        when A.PRECIO_OFERTA_EUR < a.tervaeur and a.tipo = 'O' then a.tervaeur 
                                        else 0 end)*", multiprecio_cc ,")+", tfijoj,") precio,
                                        'PT CC' idtec1
                                        from detalle_ofertas_md a where
                                        --A.TIPO = 'O'
                                        --and 
                                        A.FECHA =
                                        (
                                        -- filtros fechas CC
                                        ", fechasccj,"
                                        --// filtros fechas CC
                                        )
                                        
                                        and A.HORA = ", hora1, " and A.IDTEC1 in ('CC')
                                        and a.cdnupais in (2)
                                        --//////PT CC------------------
                                        )
                                        
                                        union all    
                                        select * from (                                      
                                        --Hidro fluyente a 0--------------------------------
                                        
                                        select", hidrofluyej ,"ofertado, 0 precio, 'Fluyente' idtec1
                                        from dual 
                                        
                                        --//////Hidro fluyente a0------------------
                                        ) 
                                        union all    
                                        select * from (                                      
                                        --PT Hidro fluyente a 0--------------------------------
                                        
                                        select", PTfluyehidroj ,"ofertado, 0 precio, 'PT Fluyente' idtec1
                                        from dual 
                                        
                                        --//////PT Hidro fluyente a0------------------
                                        )   
                                        union all
                                        
                                        select * from (                                      
                                        --NU a 0--------------------------------
                                        
                                        select", numaxj ,"ofertado, 0 precio, 'NU' idtec1
                                        from dual 
                                        
                                        --//////NU a 0------------------
                                        )",
    
                                        bombeov, frSQLv, bateriav,
                                        #ptSQLv,
                                        
                                        "union all
                                        
                                        select * from
                                        (
                                        --CA-------------------
                                        
                                        select A.OFERTADO*", multi.ca, " ofertado,
                                        (((case when A.PRECIO_OFERTA_EUR >= a.tervaeur and a.tipo = 'O' then a.precio_oferta_eur 
                                        when A.PRECIO_OFERTA_EUR < a.tervaeur and a.tipo = 'O' then a.tervaeur 
                                        else 0 end)*", multiprecio_ca ,")+", tfijoj,") precio,
                                        idtec1
                                        from detalle_ofertas_md a where
                                        --A.TIPO = 'O'
                                        --and 
                                        A.FECHA =
                                        (
                                        -- filtros fechas CA
                                        ", fechascaj ,"
                                        
                                        --// filtros fechas CA
                                        )
                                        
                                        and A.HORA = ", hora1, " and A.IDTEC1 = 'CA'
                                        and a.cdnupais in (1,3,5)
                                        --//CA----------------
                                        )
                                        
                                        union all
                                        
                                        select * from
                                        (
                                        --PT CA-------------------
                                        
                                        select A.OFERTADO*", PT_multi.ca, " ofertado,
                                        (((case when A.PRECIO_OFERTA_EUR >= a.tervaeur and a.tipo = 'O' then a.precio_oferta_eur 
                                        when A.PRECIO_OFERTA_EUR < a.tervaeur and a.tipo = 'O' then a.tervaeur 
                                        else 0 end)*", multiprecio_ca ,")+", tfijoj,") precio,
                                        'PT CA' idtec1
                                        from detalle_ofertas_md a where
                                        --A.TIPO = 'O'
                                        --and 
                                        A.FECHA =
                                        (
                                        -- filtros fechas CA
                                        ", fechascaj ,"
                                        
                                        --// filtros fechas CA
                                        )
                                        
                                        and A.HORA = ", hora1, " and A.IDTEC1 = 'CA'
                                        and a.cdnupais in (2)
                                        --//PT CA----------------
                                        )                                         
                                        
                                        union all
                                        
                                        select * from
                                        (
                                        --HI gestionable-------------
                                        select", hidroUGH.ESj ,"ofertado, ",0, "precio, 'HHG' idtec1
                                        from dual 
                                        --// HI gestionable--------------------------
                                        )        
                                        
                                        union all
                                        
                                        select * from
                                        (
                                        --PT HI gestionable-------------
                                        select", hidroUGH.PTj ,"ofertado, ",0, "precio, 'PT HI' idtec1
                                        from dual 
                                        --//PT HI gestionable--------------------------
                                        )
                                        
                                        
                                        union all
                                        
                                        select * from (                                      
                                        --SF a pmin--------------------------------
                                        
                                        select", fvj ,"ofertado, ",pmin.sol, "precio, 'SF' idtec1
                                        from dual 
                                        
                                        --//////SF pmin------------------
                                        )
                                        
                                        union all
                                        
                                        select * from
                                        (
                                        --PT SF-------------
                                        select", PTfvj ,"ofertado,", PT_pmin.sol  ," precio, 'PT SF' idtec1
                                        from dual 
                                        --//PT SF--------------------------
                                        )
                                        union all
                                        
                                        select * from (                                      
                                        --ST a pmin--------------------------------
                                        
                                        select", tsolarj ,"ofertado, ",pmin.ts, "precio, 'ST' idtec1
                                        from dual 
                                        
                                        --//////ST pmin------------------
                                        )
                                        
                                        union all
                                        
                                        select * from (                                      
                                        --EO a pmin--------------------------------
                                        
                                        select", eolj ,"ofertado, ",pmin.eol, "precio, 'EO' idtec1
                                        from dual 
                                        
                                        --//////EO pmin------------------
                                        )
                                        
                                        union all
                                        
                                        
                                        
                                        select * from
                                        (
                                        --RRE (tr y tnr) -------------
                                        select", remaxj ,"ofertado, ",0, "precio, 'TR' idtec1
                                        from dual 
                                        --// RRE (tr y tnr)--------------------------
                                        )
                                        
                                        union all 
                                        
                                        select * from
                                        (
                                        --RRE (MHI) -------------
                                        --RRE (MHI) -------------
                                        select", mhj ,"ofertado, ",0, "precio, 'MHI' idtec1
                                        from dual 
                                        --// RRE (MHI)--------------------------
                                        --// RRE (MHI)--------------------------
                                        )
                                        union all
                                        select * from
                                        (
                                        --PT EO +RRE-------------
                                        select", PTeolj ,"ofertado,", PT_pmin.eol  ," precio, 'PT EO' idtec1
                                        from dual 
                                        --//PT EO +RRE--------------------------
                                        )
                                        
                                        
                                        
                                        union all
                                        
                                        select * from
                                        (
                                        --PT COGyMIh-------------
                                        select", cogmax.PTj ,"ofertado, 0 precio, 'PT COG' idtec1
                                        from dual 
                                        --//PT COGyMIh--------------------------
                                        )                                         
                                        )
                                        group by  PRECIO, IDTEC1
                                        ORDER BY  PRECIO, 
                                        (case when IDTEC1 = 'NU' then 1
                                        when IDTEC1 = 'TNR' then 2
                                        when IDTEC1 = 'TR' then 3
                                        when IDTEC1 = 'PT COG' then 3.1
                                        when IDTEC1 = 'MHI' then 4
                                        when IDTEC1 = 'Fluyente' then 5
                                        when IDTEC1 = 'PT Fluyente' then 5.1
                                        when IDTEC1 = 'SF' then 6
                                        when IDTEC1 = 'PT SF' then 6.1
                                        when IDTEC1 = 'EO' then 7
                                        when IDTEC1 = 'PT EO' then 7.1
                                        when IDTEC1 = 'RRE' then 8
                                        when IDTEC1 = 'ST' then 9
                                        when IDTEC1 = 'HHG' then 10
                                        when IDTEC1 = 'PT HI' then 10.1
                                        when IDTEC1 = 'CA' then 11
                                        when IDTEC1 = 'PT CA' then 11.1
                                        when IDTEC1 = 'CC' then 12
                                        when IDTEC1 = 'PT CC' then 12.1
                                        when IDTEC1 = 'Int.PT' then 13
                                        when IDTEC1 = 'Int.FR' then 14
                                        else 15 end )asc  
                                        , ofertado
                                        )
                                        ORDER BY  PRECIO, 
                                        (case when IDTEC1 = 'NU' then 1
                                        when IDTEC1 = 'TNR' then 2
                                        when IDTEC1 = 'TR' then 3
                                        when IDTEC1 = 'PT COG' then 3.1
                                        when IDTEC1 = 'MHI' then 4
                                        when IDTEC1 = 'Fluyente' then 5
                                        when IDTEC1 = 'PT Fluyente' then 5.1
                                        when IDTEC1 = 'SF' then 6
                                        when IDTEC1 = 'PT SF' then 6.1
                                        when IDTEC1 = 'EO' then 7
                                        when IDTEC1 = 'PT EO' then 7.1
                                        when IDTEC1 = 'RRE' then 8
                                        when IDTEC1 = 'ST' then 9
                                        when IDTEC1 = 'HHG' then 10
                                        when IDTEC1 = 'PT HI' then 10.1
                                        when IDTEC1 = 'CA' then 11
                                        when IDTEC1 = 'PT CA' then 11.1
                                        when IDTEC1 = 'CC' then 12
                                        when IDTEC1 = 'PT CC' then 12.1
                                        when IDTEC1 = 'Int.PT' then 13
                                        when IDTEC1 = 'Int.FR' then 14
                                        else 15 end )asc  
                                        , ofertado
                                        --//SIN LAGEAR
                                        )
                                        
                                        union all
                                        
                                        select * from
                                        (
                                        select ofertado, precio, idtec1, lag(x,1,0) over (order by x, precio, 
                                        (case when IDTEC1 = 'NU' then 1
                                        when IDTEC1 = 'TNR' then 2
                                        when IDTEC1 = 'TR' then 3
                                        when IDTEC1 = 'PT COG' then 3.1
                                        when IDTEC1 = 'MHI' then 4
                                        when IDTEC1 = 'Fluyente' then 5
                                        when IDTEC1 = 'PT Fluyente' then 5.1
                                        when IDTEC1 = 'SF' then 6
                                        when IDTEC1 = 'PT SF' then 6.1
                                        when IDTEC1 = 'EO' then 7
                                        when IDTEC1 = 'PT EO' then 7.1
                                        when IDTEC1 = 'RRE' then 8
                                        when IDTEC1 = 'ST' then 9
                                        when IDTEC1 = 'HHG' then 10
                                        when IDTEC1 = 'PT HI' then 10.1
                                        when IDTEC1 = 'CA' then 11
                                        when IDTEC1 = 'PT CA' then 11.1
                                        when IDTEC1 = 'CC' then 12
                                        when IDTEC1 = 'PT CC' then 12.1
                                        when IDTEC1 = 'Int.PT' then 13
                                        when IDTEC1 = 'Int.FR' then 14
                                        else 15 end )
                                        asc) x from
                                        (
                                        --CON LAGEAR
                                        select ofertado, precio, idtec1, sum(ofertado) over (order by precio,
                                        (case when IDTEC1 = 'NU' then 1
                                        when IDTEC1 = 'TNR' then 2
                                        when IDTEC1 = 'TR' then 3
                                        when IDTEC1 = 'PT COG' then 3.1
                                        when IDTEC1 = 'MHI' then 4
                                        when IDTEC1 = 'Fluyente' then 5
                                        when IDTEC1 = 'PT Fluyente' then 5.1
                                        when IDTEC1 = 'SF' then 6
                                        when IDTEC1 = 'PT SF' then 6.1
                                        when IDTEC1 = 'EO' then 7
                                        when IDTEC1 = 'PT EO' then 7.1
                                        when IDTEC1 = 'RRE' then 8
                                        when IDTEC1 = 'ST' then 9
                                        when IDTEC1 = 'HHG' then 10
                                        when IDTEC1 = 'PT HI' then 10.1
                                        when IDTEC1 = 'CA' then 11
                                        when IDTEC1 = 'PT CA' then 11.1
                                        when IDTEC1 = 'CC' then 12
                                        when IDTEC1 = 'PT CC' then 12.1
                                        when IDTEC1 = 'Int.PT' then 13
                                        when IDTEC1 = 'Int.FR' then 14
                                        else 15 end )
                                        , ofertado asc) x
                                        from
                                        (
                                        SELECT sum(OFERTADO) ofertado, PRECIO, IDTEC1
                                        FROM
                                        (
                                        select * from
                                        (
                                        --CC--------------------------------
                                        select A.OFERTADO*", multi.cc, " ofertado,
                                        (((case when A.PRECIO_OFERTA_EUR >= a.tervaeur and a.tipo = 'O' then a.precio_oferta_eur 
                                        when A.PRECIO_OFERTA_EUR < a.tervaeur and a.tipo = 'O' then a.tervaeur 
                                        else 0 end)*", multiprecio_cc ,")+", tfijoj,") precio,
                                        idtec1
                                        from detalle_ofertas_md a where
                                        --A.TIPO = 'O'
                                        --and 
                                        A.FECHA =
                                        (
                                        -- filtros fechas CC
                                        ", fechasccj,"
                                        --// filtros fechas CC
                                        )
                                        
                                        and A.HORA = ", hora1, " and A.IDTEC1 in ('CC')
                                        and a.cdnupais in (1,3,5)
                                        --//////CC------------------
                                        )
                                        
                                        union all
                                        
                                        select * from
                                        (
                                        --PT CC--------------------------------
                                        select A.OFERTADO*", PT_multi.cc, " ofertado,
                                        (((case when A.PRECIO_OFERTA_EUR >= a.tervaeur and a.tipo = 'O' then a.precio_oferta_eur 
                                        when A.PRECIO_OFERTA_EUR < a.tervaeur and a.tipo = 'O' then a.tervaeur 
                                        else 0 end)*", multiprecio_cc ,")+", tfijoj,") precio,
                                        'PT CC' idtec1
                                        from detalle_ofertas_md a where
                                        --A.TIPO = 'O'
                                        --and 
                                        A.FECHA =
                                        (
                                        -- filtros fechas CC
                                        ", fechasccj,"
                                        --// filtros fechas CC
                                        )
                                        
                                        and A.HORA = ", hora1, " and A.IDTEC1 in ('CC')
                                        and a.cdnupais in (2)
                                        --//////PT CC------------------
                                        )
                                        
                                        union all    
                                        select * from (                                      
                                        --Hidro fluyente a 0--------------------------------
                                        
                                        select", hidrofluyej ,"ofertado, 0 precio, 'Fluyente' idtec1
                                        from dual 
                                        
                                        --//////Hidro fluyente a0------------------
                                        ) 
                                        union all    
                                        select * from (                                      
                                        --PT Hidro fluyente a 0--------------------------------
                                        
                                        select", PTfluyehidroj ,"ofertado, 0 precio, 'PT Fluyente' idtec1
                                        from dual 
                                        
                                        --//////PT Hidro fluyente a0------------------
                                        )   
                                        union all
                                        
                                        select * from (                                      
                                        --NU a 0--------------------------------
                                        
                                        select", numaxj ,"ofertado, 0 precio, 'NU' idtec1
                                        from dual 
                                        
                                        --//////NU a 0------------------
                                        )",
    
                                        bombeov, frSQLv, bateriav,
                                        #ptSQLv,
                                        
                                        "union all
                                        
                                        select * from
                                        (
                                        --CA-------------------
                                        
                                        select A.OFERTADO*", multi.ca, " ofertado,
                                        (((case when A.PRECIO_OFERTA_EUR >= a.tervaeur and a.tipo = 'O' then a.precio_oferta_eur 
                                        when A.PRECIO_OFERTA_EUR < a.tervaeur and a.tipo = 'O' then a.tervaeur 
                                        else 0 end)*", multiprecio_ca ,")+", tfijoj,") precio,
                                        idtec1
                                        from detalle_ofertas_md a where
                                        --A.TIPO = 'O'
                                        --and 
                                        A.FECHA =
                                        (
                                        -- filtros fechas CA
                                        ", fechascaj ,"
                                        
                                        --// filtros fechas CA
                                        )
                                        
                                        and A.HORA = ", hora1, " and A.IDTEC1 = 'CA'
                                        and a.cdnupais in (1,3,5)
                                        --//CA----------------
                                        )
                                        
                                        union all
                                        
                                        select * from
                                        (
                                        --PT CA-------------------
                                        
                                        select A.OFERTADO*", PT_multi.ca, " ofertado,
                                        (((case when A.PRECIO_OFERTA_EUR >= a.tervaeur and a.tipo = 'O' then a.precio_oferta_eur 
                                        when A.PRECIO_OFERTA_EUR < a.tervaeur and a.tipo = 'O' then a.tervaeur 
                                        else 0 end)*", multiprecio_ca ,")+", tfijoj,") precio,
                                        'PT CA' idtec1
                                        from detalle_ofertas_md a where
                                        --A.TIPO = 'O'
                                        --and 
                                        A.FECHA =
                                        (
                                        -- filtros fechas CA
                                        ", fechascaj ,"
                                        
                                        --// filtros fechas CA
                                        )
                                        
                                        and A.HORA = ", hora1, " and A.IDTEC1 = 'CA'
                                        and a.cdnupais in (2)
                                        --//PT CA----------------
                                        )                                         
                                        
                                        union all
                                        
                                        select * from
                                        (
                                        --HI gestionable-------------
                                        select", hidroUGH.ESj ,"ofertado, ",0, "precio, 'HHG' idtec1
                                        from dual 
                                        --// HI gestionable--------------------------
                                        )        
                                        
                                        union all
                                        
                                        select * from
                                        (
                                        --PT HI gestionable-------------
                                        select", hidroUGH.PTj ,"ofertado, ",0, "precio, 'PT HI' idtec1
                                        from dual 
                                        --//PT HI gestionable--------------------------
                                        )
                                        
                                        union all
                                        
                                        select * from (                                      
                                        --SF a pmin--------------------------------
                                        
                                        select", fvj ,"ofertado, ",pmin.sol, "precio, 'SF' idtec1
                                        from dual 
                                        
                                        --//////SF pmin------------------
                                        )
                                        
                                        union all
                                        
                                        select * from
                                        (
                                        --PT SF-------------
                                        select", PTfvj ,"ofertado,", PT_pmin.sol  ," precio, 'PT SF' idtec1
                                        from dual 
                                        --//PT SF--------------------------
                                        )
                                        union all
                                        
                                        select * from (                                      
                                        --ST a pmin--------------------------------
                                        
                                        select", tsolarj ,"ofertado, ",pmin.ts, "precio, 'ST' idtec1
                                        from dual 
                                        
                                        --//////ST pmin------------------
                                        )
                                        
                                        union all
                                        
                                        select * from (                                      
                                        --EO a pmin--------------------------------
                                        
                                        select", eolj ,"ofertado, ",pmin.eol, "precio, 'EO' idtec1
                                        from dual 
                                        
                                        --//////EO pmin------------------
                                        )
                                        
                                        union all
                                        
                                        
                                        select * from
                                        (
                                        --RRE (tr y tnr) -------------
                                        select", remaxj ,"ofertado, ",0, "precio, 'TR' idtec1
                                        from dual 
                                        --// RRE (tr y tnr)--------------------------
                                        )
                                        
                                        union all 
                                        
                                        select * from
                                        (
                                        --RRE (MHI) -------------
                                        select", mhj ,"ofertado, ",0, "precio, 'MHI' idtec1
                                        from dual 
                                        --// RRE (MHI)--------------------------
                                        )
                                        union all
                                        
                                        select * from
                                        (
                                        --PT EO +RRE-------------
                                        select", PTeolj ,"ofertado,", PT_pmin.eol  ," precio, 'PT EO' idtec1
                                        from dual 
                                        --//PT EO +RRE--------------------------
                                        )
                                        union all
                                        
                                        
                                        
                                        select * from
                                        (
                                        --PT COGyMIh-------------
                                        select", cogmax.PTj ,"ofertado, 0 precio, 'PT COG' idtec1
                                        from dual 
                                        --//PT COGyMIh--------------------------
                                        )
                                        )
                                        group by  PRECIO, IDTEC1
                                        ORDER BY  PRECIO, 
                                        (case when IDTEC1 = 'NU' then 1
                                        when IDTEC1 = 'TNR' then 2
                                        when IDTEC1 = 'TR' then 3
                                        when IDTEC1 = 'PT COG' then 3.1
                                        when IDTEC1 = 'MHI' then 4
                                        when IDTEC1 = 'Fluyente' then 5
                                        when IDTEC1 = 'PT Fluyente' then 5.1
                                        when IDTEC1 = 'SF' then 6
                                        when IDTEC1 = 'PT SF' then 6.1
                                        when IDTEC1 = 'EO' then 7
                                        when IDTEC1 = 'PT EO' then 7.1
                                        when IDTEC1 = 'RRE' then 8
                                        when IDTEC1 = 'ST' then 9
                                        when IDTEC1 = 'HHG' then 10
                                        when IDTEC1 = 'PT HI' then 10.1
                                        when IDTEC1 = 'CA' then 11
                                        when IDTEC1 = 'PT CA' then 11.1
                                        when IDTEC1 = 'CC' then 12
                                        when IDTEC1 = 'PT CC' then 12.1
                                        when IDTEC1 = 'Int.PT' then 13
                                        when IDTEC1 = 'Int.FR' then 14
                                        else 15 end )asc  
                                        , ofertado
                                        )
                                        ORDER BY  PRECIO, 
                                        (case when IDTEC1 = 'NU' then 1
                                        when IDTEC1 = 'TNR' then 2
                                        when IDTEC1 = 'TR' then 3
                                        when IDTEC1 = 'PT COG' then 3.1
                                        when IDTEC1 = 'MHI' then 4
                                        when IDTEC1 = 'Fluyente' then 5
                                        when IDTEC1 = 'PT Fluyente' then 5.1
                                        when IDTEC1 = 'SF' then 6
                                        when IDTEC1 = 'PT SF' then 6.1
                                        when IDTEC1 = 'EO' then 7
                                        when IDTEC1 = 'PT EO' then 7.1
                                        when IDTEC1 = 'RRE' then 8
                                        when IDTEC1 = 'ST' then 9
                                        when IDTEC1 = 'HHG' then 10
                                        when IDTEC1 = 'PT HI' then 10.1
                                        when IDTEC1 = 'CA' then 11
                                        when IDTEC1 = 'PT CA' then 11.1
                                        when IDTEC1 = 'CC' then 12
                                        when IDTEC1 = 'PT CC' then 12.1
                                        when IDTEC1 = 'Int.PT' then 13
                                        when IDTEC1 = 'Int.FR' then 14
                                        else 15 end )asc  
                                        , ofertado
                                        
                                        --//CON LAGEAR
                                        )
                                        
      )
                                        
      )
                                        order by x, precio,
                                        (case when IDTEC1 = 'NU' then 1
                                        when IDTEC1 = 'TNR' then 2
                                        when IDTEC1 = 'TR' then 3
                                        when IDTEC1 = 'PT COG' then 3.1
                                        when IDTEC1 = 'MHI' then 4
                                        when IDTEC1 = 'Fluyente' then 5
                                        when IDTEC1 = 'PT Fluyente' then 5.1
                                        when IDTEC1 = 'SF' then 6
                                        when IDTEC1 = 'PT SF' then 6.1
                                        when IDTEC1 = 'EO' then 7
                                        when IDTEC1 = 'PT EO' then 7.1
                                        when IDTEC1 = 'RRE' then 8
                                        when IDTEC1 = 'ST' then 9
                                        when IDTEC1 = 'HHG' then 10
                                        when IDTEC1 = 'PT HI' then 10.1
                                        when IDTEC1 = 'CA' then 11
                                        when IDTEC1 = 'PT CA' then 11.1
                                        when IDTEC1 = 'CC' then 12
                                        when IDTEC1 = 'PT CC' then 12.1
                                        when IDTEC1 = 'Int.PT' then 13
                                        when IDTEC1 = 'Int.FR' then 14
                                        else 15 end )asc  
                                        , ofertado
                                        "))
      
      odbcClose(channel)
      #///Consulta oferta
      
      #########CASADOR DE CURVAS
      
      
      
      #########CASADOR DE CURVAS
      
      
      
      inic = 2
      iniv = 2
      
      
      
      precioc = demanda[inic,2]
      preciov = oferta[iniv,2]
      xc = demanda[inic,4]
      xv = oferta[iniv,4]
      
      while(preciov < precioc){
        
        precioc = demanda[inic,2]
        preciov = oferta[iniv,2]
        
        xc = demanda[inic,4]
        xv = oferta[iniv,4]
        
        
        
        if (xc > xv){
          iniv = iniv + 1
        } else if (xc < xv){
          inic = inic + 1
        } else {
          iniv = iniv + 1
          inic = inic + 1
        }
        
        # print(inic)
        # print(iniv)
        
      }
      
      
      if (xc > xv){
        precio = precioc
        xcas = xv
      } else if (xc < xv){
        precio = preciov
        xcas = xc
      } else {
        precio = (preciov + preciov) / 2
        xcas = xc
      }
      #
      #
      #
      
      
      
      
      ##########//CASADOR DE CURVAS
      
      
      
      demandacas <- demanda[0:(length(which(demanda$x<= xcas))+1),] 
      ofertacas <- oferta[0:(length(which(oferta$X<= xcas))+1),]
      
      # #Ultima linea
      xn <- xcas-oferta[length(which(oferta$X<= xcas)),]$X
      ofertacas[(length(which(oferta$X<= xcas))):(length(which(oferta$X<= xcas))+1),]$OFERTADO =  xn
      
      demxn <- xcas-demanda[length(which(demanda$x<= xcas)),]$x
      demandacas[(length(which(demanda$x<= xcas))):(length(which(demanda$x<= xcas))+1),]$OFERTADO =  demxn   
      # #//Ultima linea   
      
      
      demmix <- aggregate(demandacas$OFERTADO, list(demandacas$IDTEC1), sum)
      colnames(demmix)<-c("Tecnologia","Energia")
      demmix$Energia <- demmix$Energia/2    
      
      esDem10 = demmix[which(demmix$Tecnologia == 'Comercializadora'),]$Energia
      ptDem10 = demmix[which(demmix$Tecnologia == 'PT Comercializadora'),]$Energia
      expFR10 = demmix[which(demmix$Tecnologia == 'Int.FR'),]$Energia
      
      if (length(esDem10) == 0){esDem10 <- 0}
      if (length(ptDem10) == 0){ptDem10 <- 0}
      if (length(expFR10) == 0){expFR10 <- 0}
      
      
      mix <- aggregate(ofertacas$OFERTADO, list(ofertacas$IDTEC1), sum)
      colnames(mix)<-c("Tecnologia","Energia")
      mix$Energia <- mix$Energia/2
      
      cc10 = sum(mix[which(mix$Tecnologia == 'CC' |mix$Tecnologia == 'GIC' ),]$Energia)
      ca10 = mix[which(mix$Tecnologia == 'CA'),]$Energia
      nu10 = mix[which(mix$Tecnologia == 'NU'),]$Energia
      hi10 = sum(mix[which(mix$Tecnologia == 'HHG' |mix$Tecnologia == 'HBP'|mix$Tecnologia == 'Fluyente' ),]$Energia)
      eo10 = mix[which(mix$Tecnologia == 'EO'),]$Energia
      sf10 = mix[which(mix$Tecnologia == 'SF'),]$Energia
      st10 = mix[which(mix$Tecnologia == 'ST'),]$Energia
      co10 = sum(mix[which(mix$Tecnologia == 'TNR' |mix$Tecnologia == 'TR'),]$Energia)
      mh10 = mix[which(mix$Tecnologia == 'MHI'),]$Energia
      re10 = mix[which(mix$Tecnologia == 'RRE'),]$Energia
      im10 = sum(mix[which(mix$Tecnologia == 'Int.FR' |mix$Tecnologia == 'Int.PT'),]$Energia)
      
      #PT
      PTcc10 = mix[which(mix$Tecnologia == 'PT CC'),]$Energia
      PTca10 = mix[which(mix$Tecnologia == 'PT CA'),]$Energia
      PThi10 = sum(mix[which(mix$Tecnologia == 'PT HI' |mix$Tecnologia == 'PT Fluyente' ),]$Energia)
      PTeo10 = mix[which(mix$Tecnologia == 'PT EO'),]$Energia
      PTsf10 = mix[which(mix$Tecnologia == 'PT SF'),]$Energia
      PTco10 = mix[which(mix$Tecnologia == 'PT COG'),]$Energia
      #/PT
      
      #Bombeo
      c.bom10 <- sum(demandacas[demandacas$IDTEC1 == "Consumo Bombeo",]$OFERTADO)/2
      PTc.bom10 <- sum(demandacas[demandacas$IDTEC1 == "Consumo Bombeo PT",]$OFERTADO)/2
      g.bom10 <- sum(ofertacas[ofertacas$IDTEC1 == "Generacion Bombeo",]$OFERTADO)/2
      PTg.bom10 <- sum(ofertacas[ofertacas$IDTEC1 == "Generacion Bombeo PT",]$OFERTADO)/2
      #/Bombeo
      
      #Baterias
      c.bat10 <- sum(demandacas[demandacas$IDTEC1 == "Consumo Bateria",]$OFERTADO)/2
      g.bat10 <- sum(ofertacas[ofertacas$IDTEC1 == "Generacion Bateria",]$OFERTADO)/2
      #/Baterias
      
      if (length(cc10) == 0){cc10 <- 0}
      if (length(ca10) == 0){ca10 <- 0}
      if (length(re10) == 0){re10 <- 0}
      if (length(nu10) == 0){nu10 <- 0}
      if (length(hi10) == 0){hi10 <- 0}
      if (length(eo10) == 0){eo10 <- 0}
      if (length(sf10) == 0){sf10 <- 0}
      if (length(st10) == 0){st10 <- 0}
      if (length(co10) == 0){co10 <- 0}
      if (length(mh10) == 0){mh10 <- 0}
      if (length(im10) == 0){im10 <- 0}
      
      #PT
      if (length(PTcc10) == 0){PTcc10 <- 0}
      if (length(PTca10) == 0){PTca10 <- 0}
      if (length(PThi10) == 0){PThi10 <- 0}
      if (length(PTeo10) == 0){PTeo10 <- 0}
      if (length(PTsf10) == 0){PTsf10 <- 0}
      if (length(PTco10) == 0){PTco10 <- 0}
      #/PT        
      
      #Bombeo
      
      if (length(c.bom10) == 0){c.bom10 <- 0}
      if (length(PTc.bom10) == 0){PTc.bom10 <- 0}
      if (length(g.bom10) == 0){g.bom10 <- 0}
      if (length(PTg.bom10) == 0){PTg.bom10 <- 0}
      
      #/Bombeo
      
      #Baterias 
      if (length(c.bat10) == 0){c.bat10 <- 0}
      if (length(g.bat10) == 0){g.bat10 <- 0}    
      #Baterias
      
      ###95%
      ofertacas95 <- ofertacas[which(ofertacas$PRECIO >= 0.95*precio),]
      
      mix95 <- aggregate(ofertacas95$OFERTADO, list(ofertacas95$IDTEC1), sum)
      colnames(mix95)<-c("Tecnologia","Energia")
      mix95$Energia <- mix95$Energia/2
      
      cc10.95 = sum(mix95[which(mix95$Tecnologia == 'CC' |mix95$Tecnologia == 'GIC' ),]$Energia)
      ca10.95 = mix95[which(mix95$Tecnologia == 'CA'),]$Energia
      nu10.95 = mix95[which(mix95$Tecnologia == 'NU'),]$Energia
      hi10.95 = sum(mix95[which(mix95$Tecnologia == 'HHG' |mix95$Tecnologia == 'HBP'|mix95$Tecnologia == 'Fluyente' ),]$Energia)
      eo10.95 = mix95[which(mix95$Tecnologia == 'EO'),]$Energia
      sf10.95 = mix95[which(mix95$Tecnologia == 'SF'),]$Energia
      st10.95 = mix95[which(mix95$Tecnologia == 'ST'),]$Energia
      co10.95 = sum(mix95[which(mix95$Tecnologia == 'TNR' |mix95$Tecnologia == 'TR'),]$Energia)
      mh10.95 = mix95[which(mix95$Tecnologia == 'MHI'),]$Energia
      re10.95 = mix95[which(mix95$Tecnologia == 'RRE'),]$Energia
      im10.95 = sum(mix95[which(mix95$Tecnologia == 'Int.FR' |mix95$Tecnologia == 'Int.PT'),]$Energia)
      
      #PT
      PTcc10.95 = mix95[which(mix95$Tecnologia == 'PT CC'),]$Energia
      PTca10.95 = mix95[which(mix95$Tecnologia == 'PT CA'),]$Energia
      PThi10.95 = sum(mix95[which(mix95$Tecnologia == 'PT HI' |mix95$Tecnologia == 'PT Fluyente' ),]$Energia)
      PTeo10.95 = mix95[which(mix95$Tecnologia == 'PT EO'),]$Energia
      PTsf10.95 = mix95[which(mix95$Tecnologia == 'PT SF'),]$Energia
      PTco10.95 = mix95[which(mix95$Tecnologia == 'PT COG'),]$Energia
      #/PT    
      
      #Bombeo
      g.bom10.95 = mix95[which(mix95$Tecnologia == 'Generacion Bombeo'),]$Energia
      PTg.bom10.95 = mix95[which(mix95$Tecnologia == 'Generacion Bombeo PT'),]$Energia
      
      #/Bombeo
      
      #???Baterias
      g.bat10.95 <- mix95[which(mix95$Tecnologia == "Generacion Bateria"),]$Energia
      #/Baterias
      
      if (length(cc10.95) == 0){cc10.95 <- 0}
      if (length(ca10.95) == 0){ca10.95 <- 0}
      if (length(re10.95) == 0){re10.95 <- 0}
      if (length(nu10.95) == 0){nu10.95 <- 0}
      if (length(hi10.95) == 0){hi10.95 <- 0}
      if (length(eo10.95) == 0){eo10.95 <- 0}
      if (length(sf10.95) == 0){sf10.95 <- 0}
      if (length(st10.95) == 0){st10.95 <- 0}
      if (length(co10.95) == 0){co10.95 <- 0}
      if (length(mh10.95) == 0){mh10.95 <- 0}
      if (length(im10.95) == 0){im10.95 <- 0}
      
      #PT
      if (length(PTcc10.95) == 0){PTcc10.95 <- 0}
      if (length(PTca10.95) == 0){PTca10.95 <- 0}
      if (length(PThi10.95) == 0){PThi10.95 <- 0}
      if (length(PTeo10.95) == 0){PTeo10.95 <- 0}
      if (length(PTsf10.95) == 0){PTsf10.95 <- 0}
      if (length(PTco10.95) == 0){PTco10.95 <- 0}
      #/PT
      
      #Bombeo
      if (length(g.bom10.95) == 0){g.bom10.95 <- 0}
      if (length(PTg.bom10.95) == 0){PTg.bom10.95 <- 0}
      
      #/Bobmbeo
      
      #Bateria
      if (length(g.bat10.95) == 0){g.bat10.95 <- 0}
      #Bateria
      
      
      ##//95%
      
      
      
      
      #Vertidos old
      
      vertidoj <- 0  
      
      #/Vertidos old
      
      #Datos de energia en oferta a p min
      
      
      min.sf10 = sum(oferta[oferta$PRECIO == pmin.sol & oferta$IDTEC1 == "SF",1])/2
      min.st10 = sum(oferta[oferta$PRECIO == pmin.ts & oferta$IDTEC1 == "ST",1])/2
      min.eo10 = sum(oferta[oferta$PRECIO == pmin.eol & oferta$IDTEC1 == "EO",1])/2
      min.PTeo10 = sum(oferta[oferta$PRECIO == PT_pmin.eol & oferta$IDTEC1 == "PT EO",1])/2
      min.PTsf10 = sum(oferta[oferta$PRECIO == PT_pmin.sol & oferta$IDTEC1 == "PT SF",1])/2
      
      
      if (length(min.sf10) == 0){min.sf10 <- 0}
      if (length(min.st10) == 0){min.st10 <- 0}
      if (length(min.eo10) == 0){min.eo10 <- 0}
      if (length(min.PTeo10) == 0){min.PTeo10 <- 0}
      if (length(min.PTsf10) == 0){min.PTsf10 <- 0}
      
      
      
      casmin.sf10 = sum(ofertacas[ofertacas$PRECIO == pmin.sol & ofertacas$IDTEC1 == "SF",1])/2
      casmin.st10 = sum(ofertacas[ofertacas$PRECIO == pmin.ts & ofertacas$IDTEC1 == "ST",1])/2
      casmin.eo10 = sum(ofertacas[ofertacas$PRECIO == pmin.eol & ofertacas$IDTEC1 == "EO",1])/2
      casmin.PTeo10 = sum(ofertacas[ofertacas$PRECIO == PT_pmin.eol & ofertacas$IDTEC1 == "PT EO",1])/2
      casmin.PTsf10 = sum(ofertacas[ofertacas$PRECIO == PT_pmin.sol & ofertacas$IDTEC1 == "PT SF",1])/2
      
      
      if (length(casmin.sf10) == 0){casmin.sf10 <- 0}
      if (length(casmin.st10) == 0){casmin.st10 <- 0}
      if (length(casmin.eo10) == 0){casmin.eo10 <- 0}
      if (length(casmin.PTeo10) == 0){casmin.PTeo10 <- 0}
      if (length(casmin.PTsf10) == 0){casmin.PTsf10 <- 0}
      
      
      nocasmin.sf10 = min.sf10 - casmin.sf10
      nocasmin.st10 = min.st10 - casmin.st10
      nocasmin.eo10 = min.eo10 - casmin.eo10
      nocasmin.PTeo10 = min.PTeo10 - casmin.PTeo10  
      nocasmin.PTsf10 = min.PTsf10 - casmin.PTsf10
      
      #/Datos de energia en oferta a p min
      
      
      
      
      
      imp.expES10 = (cc10+ca10+nu10+hi10+eo10+sf10+st10+co10+mh10+re10) - demandaj - c.bom10 + g.bom10
      imp.expPT10 = (PTcc10+PTca10+PThi10+PTsf10+PTeo10+PTco10) - PTdemandj - PTc.bom10 + PTg.bom10
      imp.expFR10 = (cc10+ca10+nu10+hi10+eo10+sf10+st10+co10+mh10+re10+PTcc10+PTca10+PThi10+PTsf10+PTeo10+PTco10)-(demandaj+PTdemandj) - c.bom10 - PTc.bom10 + g.bom10 + PTg.bom10
      
      
      #Unidad en marginal
      ofertacasmar <- ofertacas[which(ofertacas$OFERTADO != 0),]
      if(ofertacasmar[length(ofertacasmar$IDTEC1),3] == "Int.FR"){
        uof.marginal10 <- as.character(ofertacasmar[(length(ofertacasmar$IDTEC1)-2),3])
      }else{
        uof.marginal10 <- as.character(ofertacasmar[length(ofertacasmar$IDTEC1),3])
      }
      
      #//Unidad en marginal
      
      ##Actualizacion de difb embalse diferencial de bombeo
      difb <- difb + c.bom10 - g.bom10   
      acum.g.bom <- acum.g.bom + g.bom10
      acum.c.bom <- acum.c.bom + c.bom10
      acum.g.bom_PT <- acum.g.bom_PT + PTg.bom10
      acum.c.bom_PT <- acum.c.bom_PT + PTc.bom10
      
      if (acum.c.bom != 0){  
        ratiob <- acum.g.bom/acum.c.bom
      }else {
        ratiob <-  0.35
      }
      if (acum.c.bom_PT != 0){  
        ratiob_PT <- acum.g.bom_PT/acum.c.bom_PT
      }else {
        ratiob_PT <-  0.35
      }    
      
      
      #Act. contadores bombeo
      if (PTc.bom10 >0){
        contador_bom_PT = contador_bom_PT - 1
      }
      if (PTg.bom10 > 0){
        contador_bom_PT = contador_bom_PT + 1
      }
      if (c.bom10 > 0){
        contador_bom_ES = contador_bom_ES - 1
      }
      if (g.bom10 > 0){
        contador_bom_ES = contador_bom_ES + 1
      }	
      
      # if (contador_bom_ES == horasbom){ carga_ES = 1}
      # if (contador_bom_ES == -horasbom){ descargo_ES = 1}
      # if (contador_bom_ES == 0){ 
      #   carga_ES = 0
      #   descargo_ES = 0
      # }    	
      # if (contador_bom_PT == horasbom){ carga_PT = 1}
      # if (contador_bom_PT == -horasbom){ descargo_PT = 1}
      # if (contador_bom_PT == 0){ 
      #   carga_PT = 0
      #   descargo_PT = 0
      # } 
      
      if (ratiob < horasbom/2){ carga_ES = 1}
      if (ratiob > horasbom){ descargo_ES = 1}
      if (ratiob < (horasbom/2+0.05) & ratiob > (horasbom/2-0.05)){ 
        carga_ES = 0
        descargo_ES = 0
      }    	
      if (ratiob_PT < horasbom/2){ carga_PT = 1}
      if (ratiob_PT  > horasbom){ descargo_PT = 1}
      if (ratiob_PT < (horasbom/2+0.05) & ratiob_PT > (horasbom/2-0.05)){ 
        carga_PT = 0
        descargo_PT = 0
      }     
      
      
      
      #/Act. contadores bombeo
      
      #Escritura de resultado
      df1 <- data.frame(
        f1 = "Resultado curvas",
        fecha = fecha.estimacion, hora = hora1, proxy = precio, x = xcas, uof.marginal = uof.marginal10,
        separador1 = "", f2 = "Portfolio ES",
        cc = cc10, ca = ca10, nu = nu10, hi= hi10, eo = eo10, sf = sf10, st=st10, cog = co10, mh = mh10, re= re10, g.bom = g.bom10, g.bat = g.bat10,
        demES = demandaj, c.bom = c.bom10,  c.bat = c.bat10,
        separador2 = "", f3 = "Portoflio PT",
        PTcc =  PTcc10, PTca = PTca10, PTnu = "", PThi = PThi10,  PTeo = PTeo10, PTsf = PTsf10, PTst = "", PTcog = PTco10, PTmh = "", PTg.bom = PTg.bom10, PTg.bat ="", PTre ="",
        demPT = PTdemandj, PTc.bom = PTc.bom10, PTc.bat = "",
        separador3 = "", f4 = "Intercon.",
        imp.expES = imp.expES10, imp.expPT = imp.expPT10, imp.expFR = imp.expFR10,
        separador4 = "", f5 = "Vertidos",
        vertido = vertidoj, nocasmin.sf = nocasmin.sf10, nocasmin.st = nocasmin.st10, nocasmin.eo = nocasmin.eo10, nocasmin.PTeo = nocasmin.PTeo10, nocasmin.PTsf = nocasmin.PTsf10,
        ofemin.sf = min.sf10, ofemin.st = min.st10, ofemin.eo = min.eo10, ofemin.PTeo = min.PTeo10, ofemin.PTsf = min.PTsf10, 
        separador5 = "",f6 = "Inputs",
        api2 = api2j, gas = gasj, co2 = co2j, futurosFR = futurosfrj, tvca = cainput, tvcc = ccinput,
        demanda.ES = demandaj, previ.FV.ES = fvj, previ.TS.ES = tsolarj, previ.EO.ES = eolj, previ.NK.ES = numaxj, previ.HidroFluye.ES = hidrofluyej, previ.mh = mh10, previ.HuecoTer.ES = (demandaj-(fvj+tsolarj+eolj)),
        camax.ES =  camaxj, ccmax.ES = ccmaxj, cogmax.ES = remaxj, hiUGH.ES = hidroUGH.ESj, ofebomES = ofebombeoESj, capmax.bat = capbateriasj,
        demanda.PT = PTdemandj, previ.FV.PT = PTfvj, previ.EO.PT = PTeolj, previ.HidroFluye.PT = PTfluyehidroj, previ.HuecoTer.PT = (PTdemandj-(PTfvj+PTeolj)),
        camax.PT = PTcamaxj, ccmax.PT = PTccmaxj, cogmax.PT = cogmax.PTj, hiUGH.PT = hidroUGH.PTj, ofebomPT = ofebombeoPTj,
        capinterPT = ptj, capinterFR = frj,
        ofeb = ofebombeoESj,  P.B.compra = p.b.compra, P.B.venta = p.b.venta, ofe.bat = ofebateriasj,
        pminsf = pmin.sol, pmints = pmin.ts, pmineol = pmin.eol, pminsfPT = PT_pmin.sol, pmineolPT = PT_pmin.eol, tfijo = tfijoj,
        pmax.mercado = Pmax.mercado, pmin.mercado = Pmin.mercado, R.bat = r.bat
        
      )    
      
      
      vector.proxy <- append(vector.proxy,precio)
      
      fores <- rbind(fores,df1)        
      
      
    
      
  
  fores <- fores[-1,]


 # plot(demanda$x, demanda$PRECIO, type = 'l')
 # lines(oferta$X, oferta$PRECIO, col =2 )
  
  data <- data.frame(demanda$x, demanda$PRECIO)
  data2 <- data.frame(oferta$X, oferta$PRECIO)
  x = seq(0,10000,0.01)
  
  fig <- plot_ly(data, x = ~demanda$x, y = ~demanda$PRECIO, name = 'Demanda', type = 'scatter', mode = 'lines')
  fig <- fig %>% add_trace(y = ~data2 , name = 'Oferta', mode = 'lines')
  
  
  p <- ggplot() + 
    geom_line(data = data, aes(x = demanda$x, y = demanda$PRECIO, label = demanda$IDTEC1), color = "grey")+
    geom_point(size = 2)+
    geom_line(data = data2, aes(x = oferta$X, y = oferta$PRECIO, label = oferta$IDTEC1), color = "darkgreen") +
    geom_point(size = 2)+
    xlab('Energia (MWh)') +
    ylab('Precio Euros/MWh)')
  

  ggplotly(p)
  
 
  
 
}
# 


# 
 
