#################################################################################
#                          CROSS WAVELET                                        #  

library(WaveletComp)
library(openxlsx)
library(readxl)
library(tidyverse)
library(ape)
library(caret)

load('./Data/datos_totales.RData')

cantones <- datos_totales %>%
  select(CCanton) %>% distinct()


Period <- NULL
Power  <- NULL
Names  <- NULL
################################################################################
# PLOTS                                                                        #
################################################################################
# Se debe cambiar:
# 1. nameVarclima     = indica la variable climática que se quiere analizar
# 2. folderName       = nombre del folder donde se guardarán las imágenes
# 3. casesVarclima    = carga los datos correspondientes a la variable climática
# 4. FileNameVarClima = abreviación con la que se guardarán los archivos

xlabel<- seq(2001,2019,1)

for (i in 1:32){
  base1 <- datos_totales %>% dplyr::filter(CCanton == cantones$CCanton[i])
  nameRegion     <- base1$Canton[1]
  sqrt_cases     <- sqrt(base1$Cases)
  process_cases  <- preProcess(as.data.frame(sqrt_cases),method=c("range")) 
  casesRegion    <- predict(process_cases,as.data.frame(sqrt_cases))
  
  #normaliza datos vegetacion
  nameVarclima   <- "NDWI"
  process        <- preProcess(as.data.frame(base1$NDWI), method=c("range")) 
  casesVarclima  <- predict(process , as.data.frame(base1$NDWI))
  colnames(casesVarclima)<-"Index"
  

  
  #Wavelet Coherence ------------------------------------------------------------
  my.data <- data.frame(Canton = casesRegion$sqrt_cases, N3= casesVarclima$Index)
  my.wc   <- analyze.coherency(my.data, my.pair = c('Canton', "N3"),
                               dt = 1/12, dj = 1/200,
                               method = "AR",
                               params = list( AR = list(p=1)),
                               make.pval = TRUE, n.sim = 1000)

  Period<-cbind( Period,my.wc$Period)
  Power <-cbind( Power ,my.wc$Power.xy.avg)
  Names <-rbind( Names, nameRegion)
                                              
}

Period = as.data.frame(Period)
Power  = as.data.frame(Power)
colnames(Period)<-Names[,1]
colnames(Power) <-Names[,1]

File_Result = list(
  "Period" = Period,
  "Power"  = Power
)

write.xlsx(File_Result, paste("Power_",nameVarclima,".xlsx",sep=""))