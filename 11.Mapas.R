library(tidyverse)
library(ggplot2)
library(sf)
library(ggpubr)
library(zoo)
library(openxlsx)
library(readxl)


load('./Data/datos_totales.RData')
Cantonsdata  <- as.data.frame(read.xlsx("./Data/31Cantones.xlsx"))

data_to_color<- Cantonsdata%>%dplyr::select(CCanton,NoNino3YR,TNA3YR,TNA1YR,EVI,NDWI,ET,Precip,VEG)

#Archivos para el mapa
distritos_st <-st_read('Data/distritos_shape/Distritos_de_Costa_Rica.shp') %>%
  filter(CODIGO!=60110)

cantones_st    <- distritos_st%>%
  mutate(DTA_C = str_sub(CODIGO,start = 1,end = 3))%>%
  group_by(DTA_C)%>%dplyr::summarise()%>%
  ungroup()%>% mutate(CCanton=as.numeric(DTA_C))

codCantones <- cantones_st%>%dplyr::select(CCanton)%>%distinct()




DataPlot= merge(codCantones,data_to_color,by="CCanton",all=TRUE)

Mapa1 <- ggplot(data   = DataPlot) +
  geom_sf(mapping     = aes(fill=as.factor(TNA1YR),geometry=geometry),colour="black",
          show.legend = FALSE, size=0.2,alpha=0.4)+
  theme_void()+
  #geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_manual(values=c("darkblue","red"),na.value = 'transparent')

mypath <- file.path("Plots2/Mapa_TNA1YR.pdf")
pdf(mypath,width = 15, height = 12) 
print(Mapa1)
dev.off()


##TNA 3YR
Mapa2 <- ggplot(data   = DataPlot) +
  geom_sf(mapping     = aes(fill=as.factor(TNA3YR),geometry=geometry),colour="black",
          show.legend = FALSE,size=0.2,alpha=0.4)+
  theme_void()+
  #geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_manual(values=c("darkblue","red"),na.value = 'transparent')

mypath <- file.path("Plots2/Mapa_TNA3YR.pdf")
pdf(mypath,width = 15, height = 12) 
print(Mapa2)
dev.off()


##EVI-------------------------------------------------------
Mapa3 <- ggplot(data   = DataPlot) +
  geom_sf(mapping     = aes(fill=as.factor(EVI),geometry=geometry),colour="black",
          show.legend = FALSE,size=0.2,alpha=0.4)+
  theme_void()+
  #geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_manual(values=c("darkblue","red"),na.value = 'transparent')

mypath <- file.path("Plots2/Mapa_EVI.pdf")
pdf(mypath,width = 15, height = 12) 
print(Mapa3)
dev.off()

##NDWI-------------------------------------------------------
Mapa4 <- ggplot(data   = DataPlot) +
  geom_sf(mapping     = aes(fill=as.factor(NDWI),geometry=geometry),colour="black",
          show.legend = FALSE,size=0.2,alpha=0.4)+
  theme_void()+
  #geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_manual(values=c("darkblue","red"),na.value = 'transparent')

mypath <- file.path("Plots2/Mapa_NDWI.pdf")
pdf(mypath,width = 15, height = 12) 
print(Mapa4)
dev.off()

##ET-------------------------------------------------------
Mapa4 <- ggplot(data   = DataPlot) +
  geom_sf(mapping     = aes(fill=as.factor(ET),geometry=geometry),colour="black",
          show.legend = FALSE,size=0.2,alpha=0.4)+
  theme_void()+
  #geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_manual(values=c("darkblue","red"),na.value = 'transparent')

mypath <- file.path("Plots2/Mapa_ET.pdf")
pdf(mypath,width = 15, height = 12) 
print(Mapa4)
dev.off()


##Precip-------------------------------------------------------
Mapa5 <- ggplot(data   = DataPlot) +
  geom_sf(mapping     = aes(fill=as.factor(Precip),geometry=geometry),colour="black",
          show.legend = FALSE,size=0.2,alpha=0.4)+
  theme_void()+
  #geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_manual(values=c("darkblue","red"),na.value = 'transparent')

mypath <- file.path("Plots2/Mapa_Precip.pdf")
pdf(mypath,width = 15, height = 12) 
print(Mapa5)
dev.off()


##cantons with high correltaion with all vegetation variables-------------------------------------------------------
Mapa6 <- ggplot(data   = DataPlot) +
  geom_sf(mapping     = aes(fill=as.factor(VEG),geometry=geometry),colour="black",
          show.legend = FALSE,size=0.2,alpha=0.4)+
  theme_void()+
  #geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_manual(values=c("darkblue","red"),na.value = 'transparent')

mypath <- file.path("Plots2/Mapa_highcorr_veg.pdf")
pdf(mypath,width = 15, height = 12) 
print(Mapa6)
dev.off()



##cantons with high correltaion with all vegetation variables-------------------------------------------------------
TotalCases_overtime<- datos_totales%>%group_by(Canton,CCanton)%>%
  dplyr::summarise(TotalCases=sum(Cases),Pop=max(Poblacion))%>%
  mutate(cases_x_10 = TotalCases*10000/Pop)

DataPlot= merge(codCantones,TotalCases_overtime,by="CCanton",all=TRUE)

##cantons with high correltaion with all vegetation variables-------------------------------------------------------
Mapa7 <- ggplot(data   = DataPlot) +
  geom_sf(mapping     = aes(fill=as.numeric(cases_x_10),geometry=geometry),colour="black",
          show.legend = FALSE,size=0.2,alpha=0.4)+
  theme_void()+
  geom_sf_text(aes(label = label),size=2)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "transparent")

mypath <- file.path("Plots2/Mapa_highcorr_veg.pdf")
pdf(mypath,width = 15, height = 12) 
print(Mapa7)
dev.off()


###---------------------------------------

Population <- datos_totales %>%dplyr::select("CCanton", "Poblacion") %>%
  distinct(CCanton, .keep_all = TRUE)

Data_Pop = merge(codCantones,Population,by="CCanton",all=TRUE)

Data_Pop$label = Data_Pop$CCanton
Data_Pop$label[is.na(Data_Pop$Poblacion)] = NaN
Data_Pop$label[Data_Pop$label%in%c("109","101","110","103")] = NaN

##cantons with high correltaion with all vegetation variables-------------------------------------------------------
Mapa8 <- ggplot(data  = Data_Pop) +
  geom_sf(mapping     = aes(fill=as.numeric(Poblacion),geometry=geometry),colour="black",
          show.legend = TRUE,size=0.2,alpha=0.4)+
  theme_void()+
  geom_sf_text(aes(label = label),size=6)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "transparent")

mypath <- file.path("Plots2/Population.pdf")
pdf(mypath,width = 15, height = 12) 
print(Mapa8)
dev.off()


##-----------------------------------------------
##plot zoom
Data_Pop_zoom <- Data_Pop%>%filter(CCanton%in%c("101","103","109","110"))

Mapa9 <- ggplot(data  = Data_Pop_zoom) +
  geom_sf(mapping     = aes(fill=as.numeric(Poblacion),geometry=geometry),colour="black",
          show.legend = TRUE,size=0.2,alpha=0.4)+
  theme_void()+
  #geom_sf_text(aes(label = label),size=6)+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "transparent")

mypath <- file.path("Plots2/Population_zoom.pdf")
pdf(mypath,width = 15, height = 12) 
print(Mapa9)
dev.off()
