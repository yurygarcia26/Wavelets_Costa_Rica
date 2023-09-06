#################################################################################
#                           WAVELET COHERENCE                                      #  
------------------------------------------------------------------------------#

library(WaveletComp)
library(openxlsx)
library(readxl)
library(tidyverse)
library(ape)
library(caret)

load('./Data/datos_totales.RData')

cantones <- datos_totales %>%
  select(CCanton) %>% distinct()

MAT <- matrix(nrow= 0, ncol=1)
colnames(MAT)<-"Canton"
DataFrame1 <-as.data.frame(MAT,stringsAsFactors = FALSE) 

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
  
  
  ##CAMBIAR VARIABLES
  #normaliza datos vegetacion
  nameVarclima   <- "EVI"
  process        <- preProcess(as.data.frame(base1$EVI), method=c("range")) 
  casesVarclima  <- predict(process , as.data.frame(base1$EVI))
  colnames(casesVarclima)<-"Index"
  
  folderName       = "Plots/EVI"   #folder donde se guardara toda la informacion
  fileName         = nameRegion     #nombre para los archivos relacionados con la region
  fileNameVarClima = "EVI"         #abreviacion para la variable climatica (N3, N4, N12, N34, TNA)
  fileNameRegion   = nameRegion     #nombre del cantón
  
  
#Wavelet Coherence ------------------------------------------------------------
my.data <- data.frame(Canton = casesRegion$sqrt_cases, N3= casesVarclima$Index)
my.wc   <- analyze.coherency(my.data, my.pair = c('Canton', "N3"),
                               dt = 1/12, dj = 1/200,
                               method = "AR",
                               params = list( AR = list(p=1)),
                               make.pval = TRUE, n.sim = 1000)
  
#Guarda la grafica de coherencia entre la serie 1 y 2 en un folder
#con nombre folderName asignado previamente

mypath <- file.path(".",folderName,"/Coherence_Canton/",paste("Coherence_",fileNameRegion,"_",fileNameVarClima,".pdf",sep=""))
pdf(mypath, width = 7, height = 5) 
wc.image(my.wc, n.levels = 250, 
           color.key   = "interval",
           plot.legend = FALSE,
           plot.arrow  = TRUE,
           siglvl.contour = 0.01, siglvl.arrow = 0.05, which.arrow.sig = "wt",
           legend.params = list(lab = "cross-wavelet power levels"),
           timelab = "", spec.time.axis = list(at = seq(1, 228, by = 12),
                                               labels  = xlabel), periodlab = "Period (years)", 
           main    = paste(nameRegion,nameVarclima, sep = " - "))
  dev.off()
  
#Guarda la grafica: Avarega Wavelet Power
#entre la serie 1 y 2 en un folder con nombre folderName asignado previamente
#mypath <- file.path(".",folderName,"/PowerAverage/",paste("Power_",fileNameRegion,"_",fileNameVarClima,".pdf",sep=""))
#pdf(mypath, width = 7, height = 5) 
#wc.avg(my.wc, sigcol = "red", sigpch = 20, 
#         show.siglvl = FALSE,
#         periodlab = "Period (years)",col="red", 
#         main =paste(c(nameRegion,nameVarclima),collapse= " - "))
#dev.off()
  
  
#----------------------------------------------------------------------------
bandPeriod = 1
#To extract the time series of angles at period x
#locate the period in my.wc closest to x
row.closest <- which.min((my.wc$Period -bandPeriod)^2)
# determine angles:
angle.series <- my.wc$Angle[row.closest,]
lead.time.in.months <- 12*(bandPeriod*(angle.series / (2*pi)))

#Guardar la diferencia de fase -------------
  if (i==1){
    DataFrame1 <- data.frame(Canton=rep(nameRegion,length(lead.time.in.months)))
    DataFrame1 <- DataFrame1%>% mutate(column1=lead.time.in.months)
  }else{
    newDataFrame <- data.frame(Canton=rep(nameRegion,length(lead.time.in.months)),
                               column1 = lead.time.in.months) 
    DataFrame1   <- rbind(DataFrame1,newDataFrame)}
}

write.table(DataFrame1,file=paste("./",folderName,"/PhaseDiffCanton/",nameVarclima,".csv",sep=""))

