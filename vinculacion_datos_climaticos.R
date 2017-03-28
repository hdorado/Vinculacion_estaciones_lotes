
#Vinculacion estaciones climáticas a unidades de manejo
#Se pretende asociar series de datos climáticos a periodos 
#de cosecha de cultivos que van desde siembra a recolección.
#Hugo Andrés Dorado B.
#28 - 03 - 2017

rm(list=ls())

library(plyr)

setwd(".")

#Lectura de datos y ubcación de archivos

dir_station <- "Datos_climaticos/"

assginament0 <- read.csv("Assignment.csv")

baseEventos <- read.csv("base_cosecha.csv")

#Ajuste de fechas

baseEventos$FECHA_SIEMBRA <- as.Date(baseEventos$FECHA_SIEMBRA,"%m/%d/%Y")

baseEventos$FECHA_EMERGENCIA <- as.Date(baseEventos$FECHA_EMERGENCIA,"%m/%d/%Y")

baseEventos$FECHA_FLORACION <- as.Date(baseEventos$FECHA_FLORACION,"%m/%d/%Y")

baseEventos$FECHA_COSECHA <- as.Date(baseEventos$FECHA_COSECHA,"%m/%d/%Y")

summary(baseEventos)

baseEventos$FECHA_SIEMBRA[is.na(baseEventos$FECHA_SIEMBRA)] <- baseEventos$FECHA_COSECHA[is.na(baseEventos$FECHA_SIEMBRA)]-134 #De acuerdo al promedio

summary(baseEventos)


daysFlor_Emerg <- baseEventos$FECHA_FLORACION-baseEventos$FECHA_EMERGENCIA #40-80

daysCosh_Florc <- baseEventos$FECHA_COSECHA-baseEventos$FECHA_FLORACION #20- 

assginament <- merge(assginament0,baseEventos[c("ID_EVENTO","PROD_COSECHADO")],by.x="ID_EVENTO",by.y="ID_EVENTO",all.x = T, all.y=F,sort=F)

assginament <- assginament[assginament$PROD_COSECHADO=="Grano seco",]

assginament <- merge(assginament,baseEventos,by.x="ID_EVENTO",by.y="ID_EVENTO",all.x=T,all.y = F,sort=F)

sort(assginament$FECHA_COSECHA - assginament$FECHA_SIEMBRA)


assginament <- mutate(assginament, File = paste(StationUse,VAR,"FUS.txt",sep = "_"))


listEvent <- split(assginament,assginament$ID_EVENTO)

#Ajuste de nombres

nomFecSim <- "FECHA_SIEMBRA"
nomFecCos <- "FECHA_COSECHA" 
nomFile   <- "File"
nomDateStat <- "Dates"
nomVarClim <- "VAR"



climDiar <- lapply(listEvent, 
                   function(x){
                       print("1")
                       u <- x[1,]
                       
                       date <- as.Date(u[,nomFecSim]:u[,nomFecCos], origin="1970-01-01")
                       
                       climaDiario <- data.frame(date)
                       
                       ListStatInfo <- lapply( paste0(dir_station,x[,nomFile]),function(h){
                           myDate <- read.table(h,header=T)[,1:2]
                           myDate[,nomDateStat] <- as.Date(myDate[,nomDateStat])
                           merge(climaDiario,myDate,by.x="date",by.y = "Dates",all.x = T,all.y = F)["Value"]
                       }
                       )
                       
                       climaDiarioEvent <- data.frame(date,do.call(cbind,ListStatInfo))
                       
                       names(climaDiarioEvent ) <- c("DATE",as.character(x[,nomVarClim])) 
                       climaDiarioEvent
                       
                   }
)

baseEventos <- baseEventos[baseEventos$ID_EVENTO %in% names(climDiar),]

#Ordenamos

climDiar              <- climDiar[order(as.numeric(names(climDiar)))]

baseEventos<- baseEventos[order(baseEventos$ID_EVENTO),]

baseEventos$ID_EVENTO == as.numeric(names(climDiar))

names(climDiar) <- names(listEvent)

save(climDiar, file = "Lista_Clima_Estacion.RData")  

