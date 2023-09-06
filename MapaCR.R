library(tidyverse)
library(ggplot2)
library(sf)
library(ggpubr)
library(zoo)
library(openxlsx)
library(readxl)


load('./Data/datos_totales.RData')
CantonsPop <- as.data.frame(read.xlsx("./Data/CantonsPopulation.xlsx"))
DataCanton <- as.data.frame(read.xlsx("./Data/31Cantones.xlsx"))
Datazoom   <- as.data.frame(read.xlsx("./Data/31Cantones.xlsx", sheet="Hoja1"))

#Archivos para el mapa
distritos_st <-st_read('Data/distritos_shape/Distritos_de_Costa_Rica.shp') %>%
  filter(CODIGO!=60110)

cantones_st    <- distritos_st%>%
  mutate(DTA_C = str_sub(CODIGO,start = 1,end = 3))%>%
  group_by(DTA_C)%>%dplyr::summarise()%>%
  ungroup()%>% mutate(CCanton=as.numeric(DTA_C))




#suma los casos por year per canton
TotalCases_overtime<- datos_totales%>%group_by(Canton,CCanton)%>%
  dplyr::summarise(TotalCases=sum(Cases),Pop=max(Poblacion))%>%
  mutate(cases_x_10 = TotalCases*10000/Pop)

TotalCases_overtime<-merge(TotalCases_overtime,DataCanton,by="CCanton")

DataPlot= merge(cantones_st,TotalCases_overtime,by="CCanton",all=TRUE)

Mapa <- ggplot(data   = DataPlot) +
  geom_sf(mapping     = aes(fill=as.numeric(cases_x_10),geometry=geometry),colour="black",
          show.legend = TRUE,size=0.1,alpha=0.4)+
  theme_void()+
  geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "transparent")


mypath  <- file.path("Plots2/MapaCR.pdf")
#mypath <- file.path("Plots/EffectEnvir.pdf")
pdf(mypath,width = 10, height = 6) 
print(Mapa)
dev.off()



#########################################################
#suma los casos por year per canton
DataPlot1        = merge(cantones_st,Datazoom,by="CCanton")

Mapa1 <- ggplot(data   = DataPlot1) +
  geom_sf(mapping     = aes(fill=as.factor(valor),geometry=geometry),colour="black",
          show.legend = FALSE,size=0.1,alpha=0.4)+
  theme_light()+
  geom_sf_text(aes(label = label),size=6)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank(),
        panel.grid.major = element_blank())+
  scale_fill_manual(values="lightblue",na.value = 'transparent')

mypath <- file.path("Plots/MapaZoom.pdf")

pdf(mypath,width = 10, height = 6) 
print(Mapa1)
dev.off()


#%%%-----------------------------------------
#Aannual average or mean value

annual_avg_EVI      <- aggregate(EVI ~ CCanton, data = datos_totales, FUN = mean)
annual_avg_NDWI     <- aggregate(NDWI ~ CCanton, data = datos_totales, FUN = mean)
annual_avg_ET       <- aggregate(ET ~ CCanton, data = datos_totales, FUN = mean)
annual_avg_Precip   <- aggregate(Precip_t ~ CCanton, data = datos_totales, FUN = mean)

annual_avg1 <- merge(annual_avg_EVI,annual_avg_NDWI, by="CCanton")
annual_avg2 <- merge(annual_avg_ET,annual_avg_Precip, by="CCanton")

annual_avg <- merge(annual_avg1,annual_avg2, by="CCanton")

Plot_annual_avg = merge(cantones_st,annual_avg,by="CCanton",all=TRUE)

##EVI map-------------------------------------
Mapa_evi <- ggplot(data   = Plot_annual_avg) +
  geom_sf(mapping     = aes(fill=as.numeric(EVI),geometry=geometry),colour="black",
          show.legend = TRUE,size=0.1)+
  theme_void()+
  #geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_gradient(low = "yellow", high = "darkgreen", na.value = "transparent")


mypath  <- file.path("Plots2/MapaEVI.pdf")
#mypath <- file.path("Plots/EffectEnvir.pdf")
pdf(mypath,width = 10, height = 6) 
print(Mapa_evi)
dev.off()

##NDWI map-------------------------------------
Mapa_ndwi <- ggplot(data   = Plot_annual_avg) +
  geom_sf(mapping     = aes(fill=as.numeric(NDWI),geometry=geometry),colour="black",
          show.legend = TRUE,size=0.1)+
  theme_void()+
  #geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_gradient(low = "yellow", high = "darkgreen", na.value = "transparent")


mypath  <- file.path("Plots2/MapaNDWI.pdf")
#mypath <- file.path("Plots/EffectEnvir.pdf")
pdf(mypath,width = 10, height = 6) 
print(Mapa_ndwi)
dev.off()

##ET map-------------------------------------
Mapa_ET <- ggplot(data   = Plot_annual_avg) +
  geom_sf(mapping     = aes(fill=as.numeric(ET),geometry=geometry),
          colour="black",
          show.legend = TRUE,size=0.1,alpha=0.4)+
  theme_void()+
  #geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_gradient(low = "yellow", high = "red", na.value = "transparent")


mypath  <- file.path("Plots2/MapaET.pdf")
#mypath <- file.path("Plots/EffectEnvir.pdf")
pdf(mypath,width = 10, height = 6) 
print(Mapa_ET)
dev.off()

##ET Precipitation-------------------------------------
Mapa_ET <- ggplot(data   = Plot_annual_avg) +
  geom_sf(mapping     = aes(fill=as.numeric(Precip_t),geometry=geometry),colour="black",
          show.legend = TRUE,size=0.1)+
  theme_void()+
  #geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "transparent")


mypath  <- file.path("Plots2/MapaPrec.pdf")
#mypath <- file.path("Plots/EffectEnvir.pdf")
pdf(mypath,width = 10, height = 6) 
print(Mapa_ET)
dev.off()

