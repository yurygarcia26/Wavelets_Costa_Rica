#GRAFICAS DE LA DIFERENCIA DE FASE Canton - Clima
#--------------------------------------------------------------------------------
  
library(openxlsx)
library(readxl)
library(tidyverse)
library(ranger)
library(Hmisc)
library(dtw)
library(ggplot2)
library(sf)
library(reshape2)
library(ggpubr)

color <- 1:32
semanas <-19*12
load('./Data/datos_totales.RData')
cantones <- datos_totales %>%
  select(CCanton) %>% distinct()

###graficar clusters
PhaseDiffPlot<-function(df,i){
  #melt data frame into long format
  #df1 <- melt(df,  id.vars = 'Period1', variable.name = 'Weeks')
  xlabels <- seq(2001,2019,3)
  
  Plot1<-ggplot(df , aes(Weeks, Period)) +
    geom_line(aes(colour = Cantons))+
    geom_hline(yintercept = recta1,linetype = "dashed")+
    geom_hline(yintercept = recta2,linetype = "dashed")+
    geom_hline(yintercept = 0)+
    theme_bw()+
    scale_y_continuous(lim=c(ymin,ymax))+
    scale_x_continuous(breaks = seq(1,semanas,semanas/7),labels =  xlabels)+
    labs(x="Period (Years)",y="")
  
  return(Plot1)}

##-------------------------------------------------

Cantons31 <-as.data.frame(read.xlsx("./Data/31Cantones.xlsx"))
SelectCant <- Cantons31%>%filter(VEG==1)%>%select(Cantones,VEG)

##--------------------------------------------------
Cluster_Clim   <- as.data.frame(read.xlsx("./Data/cluster_Veg.xlsx"))
DataPhaseDiff  <- read.table('./Data/Precip_t.csv')
DataPhaseDiff1 <- as.data.frame(DataPhaseDiff)
colnames(DataPhaseDiff1)<-c("Cantons","Period")


###grafico especial----------
DataPhaseDiff2 <- DataPhaseDiff1%>%filter(Cantons%in%SelectCant$Cantones)%>%group_by(Cantons)%>%mutate(Weeks=1:semanas)%>%ungroup()
figure<-PhaseDiffPlot(DataPhaseDiff2,1)
namePlot <- paste("Figures/Precip.pdf",sep="")
ggsave(namePlot, width = 15, height = 5, units = "in")
plot(figure)
dev.off() 
##-----------------------------------


bandPeriod    = 1

nameVarclima  <- "PREC"
recta1 <-  3*bandPeriod
recta2 <- -3*bandPeriod
ymax   <-  6*bandPeriod
ymin   <- -6*bandPeriod

plot_list = list()
for(i in 1:6){
  i=1
  cluster       = Cluster_Clim%>%filter(groups==i)
  df            = DataPhaseDiff1%>%filter(Cantons%in%cluster$Canton)%>%dplyr::select(Cantons,Period=Period)
  df            = df%>%group_by(Cantons)%>%mutate(Weeks=1:semanas)%>%ungroup()
  plot_list[[i]]= PhaseDiffPlot(df,i)
}

figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                    plot_list[[4]], plot_list[[5]], plot_list[[6]],
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3",
                               "Cluster 4", "Cluster 5", "Cluster 6"),
                    ncol = 2, nrow =3,font.label = list(size = 12, color ="black"),vjust = 0.3)

annotate_figure(figure, top = text_grob(""))

namePlot <- paste("Figures/PhaseDiff_Clima_",nameVarclima,".pdf",sep="")
ggsave(namePlot, width = 15, height = 10, units = "in")
plot(figure)
dev.off() 

#%%--------------------------------------------------

DataPhaseDiff  <- read.table('./Data/EVI.csv')
DataPhaseDiff1 <- as.data.frame(DataPhaseDiff)
colnames(DataPhaseDiff1)<-c("Cantons","Period")

###grafico especial----------
DataPhaseDiff2 <- DataPhaseDiff1%>%filter(Cantons%in%SelectCant$Cantones)%>%group_by(Cantons)%>%mutate(Weeks=1:semanas)%>%ungroup()
figure<-PhaseDiffPlot(DataPhaseDiff2,1)
namePlot <- paste("Figures/EVI.pdf",sep="")
ggsave(namePlot, width = 15, height = 5, units = "in")
plot(figure)
dev.off() 
##-----------------------------------



bandPeriod    = 1

nameVarclima  <- "EVI"
recta1 <-  3*bandPeriod
recta2 <- -3*bandPeriod
ymax   <-  6*bandPeriod
ymin   <- -6*bandPeriod

plot_list = list()
for(i in 1:6){
  cluster       = Cluster_Clim%>%filter(groups==i)
  df            = DataPhaseDiff1%>%filter(Cantons%in%cluster$Canton)%>%dplyr::select(Cantons,Period=Period)
  df            = df%>%group_by(Cantons)%>%mutate(Weeks=1:semanas)%>%ungroup()
  plot_list[[i]]= PhaseDiffPlot(df,i)
}

figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                    plot_list[[4]], plot_list[[5]], plot_list[[6]],
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3",
                               "Cluster 4", "Cluster 5", "Cluster 6"),
                    ncol = 2, nrow =3,font.label = list(size = 12, color ="black"),vjust = 0.3)

annotate_figure(figure, top = text_grob(""))

namePlot <- paste("Figures/PhaseDiff_Clima_",nameVarclima,".pdf",sep="")
ggsave(namePlot, width = 15, height = 10, units = "in")
plot(figure)
dev.off() 

#%%--------------------------------------------------

DataPhaseDiff  <- read.table('./Data/NDWI.csv')
DataPhaseDiff1 <- as.data.frame(DataPhaseDiff)
colnames(DataPhaseDiff1)<-c("Cantons","Period")

###grafico especial----------
DataPhaseDiff2 <- DataPhaseDiff1%>%filter(Cantons%in%SelectCant$Cantones)%>%group_by(Cantons)%>%mutate(Weeks=1:semanas)%>%ungroup()
figure<-PhaseDiffPlot(DataPhaseDiff2,1)
namePlot <- paste("Figures/NDWI.pdf",sep="")
ggsave(namePlot, width = 15, height = 5, units = "in")
plot(figure)
dev.off() 
##------



bandPeriod    = 1

nameVarclima  <- "NDWI"
recta1 <-  3*bandPeriod
recta2 <- -3*bandPeriod
ymax   <-  6*bandPeriod
ymin   <- -6*bandPeriod

plot_list = list()
for(i in 1:6){
  cluster       = Cluster_Clim%>%filter(groups==i)
  df            = DataPhaseDiff1%>%filter(Cantons%in%cluster$Canton)%>%dplyr::select(Cantons,Period=Period)
  df            = df%>%group_by(Cantons)%>%mutate(Weeks=1:semanas)%>%ungroup()
  plot_list[[i]]= PhaseDiffPlot(df,i)
}

figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                    plot_list[[4]], plot_list[[5]], plot_list[[6]],
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3",
                               "Cluster 4", "Cluster 5", "Cluster 6"),
                    ncol = 2, nrow =3,font.label = list(size = 12, color ="black"),vjust = 0.3)

annotate_figure(figure, top = text_grob(""))

namePlot <- paste("Figures/PhaseDiff_Clima_",nameVarclima,".pdf",sep="")
ggsave(namePlot, width = 15, height = 10, units = "in")
plot(figure)
dev.off() 


#%%--------------------------------------------------

DataPhaseDiff  <- read.table('./Data/ET.csv')
DataPhaseDiff1 <- as.data.frame(DataPhaseDiff)
colnames(DataPhaseDiff1)<-c("Cantons","Period")


###grafico especial----------
DataPhaseDiff2 <- DataPhaseDiff1%>%filter(Cantons%in%SelectCant$Cantones)%>%group_by(Cantons)%>%mutate(Weeks=1:semanas)%>%ungroup()
figure<-PhaseDiffPlot(DataPhaseDiff2,1)
namePlot <- paste("Figures/ET.pdf",sep="")
ggsave(namePlot, width = 15, height = 5, units = "in")
plot(figure)
dev.off() 
##------


bandPeriod    = 1

nameVarclima  <- "ET"
recta1 <-  3*bandPeriod
recta2 <- -3*bandPeriod
ymax   <-  6*bandPeriod
ymin   <- -6*bandPeriod

plot_list = list()
for(i in 1:6){
  cluster       = Cluster_Clim%>%filter(groups==i)
  df            = DataPhaseDiff1%>%filter(Cantons%in%cluster$Canton)%>%dplyr::select(Cantons,Period=Period)
  df            = df%>%group_by(Cantons)%>%mutate(Weeks=1:semanas)%>%ungroup()
  plot_list[[i]]= PhaseDiffPlot(df,i)
}

figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                    plot_list[[4]], plot_list[[5]], plot_list[[6]],
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3",
                               "Cluster 4", "Cluster 5", "Cluster 6"),
                    ncol = 2, nrow =3,font.label = list(size = 12, color ="black"),vjust = 0.3)

annotate_figure(figure, top = text_grob(""))

namePlot <- paste("Figures/PhaseDiff_Clima_",nameVarclima,".pdf",sep="")
ggsave(namePlot, width = 15, height = 10, units = "in")
plot(figure)
dev.off() 

