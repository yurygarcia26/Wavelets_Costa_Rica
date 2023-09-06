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
##TNA

Cluster_Clim   <- as.data.frame(read.xlsx("./Data/Clima_clusters.xlsx"))
DataPhaseDiff  <- read.table('./Data/TNA_3.csv')
DataPhaseDiff1 <- as.data.frame(DataPhaseDiff)
colnames(DataPhaseDiff1)<-c("Cantons","Period")

bandPeriod    = 3

nameVarclima  <- "TNA_3"
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
                               "Cluster 4", "Cluster 5", "Cluster 6","Cluster 7",""),
                    ncol = 2, nrow =3,font.label = list(size = 12, color ="black"),vjust = 0.3)

annotate_figure(figure)

namePlot <- paste("Figures/PhaseDiff_Clima_",nameVarclima,".pdf",sep="")
ggsave(namePlot, width = 15, height = 10, units = "in")
plot(figure)
dev.off() 

##-------------------------------------------------
##NINO34
Cluster_Clim   <- as.data.frame(read.xlsx("./Data/Clima_clusters.xlsx"))
DataPhaseDiff  <- read.table('./Data/NINO34_2.csv')
DataPhaseDiff1 <- as.data.frame(DataPhaseDiff)
colnames(DataPhaseDiff1)<-c("Cantons","Period")

bandPeriod    = 3

nameVarclima  <- "NINO34"
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
                               "Cluster 4", "Cluster 5", "Cluster 6","Cluster 7",""),
                    ncol = 2, nrow =3,font.label = list(size = 12, color ="black"),vjust = 0.3)



annotate_figure(figure)
namePlot <- paste("Figures/PhaseDiff_Clima_",nameVarclima,"_2.pdf",sep="")
ggsave(namePlot, width = 15, height = 10, units = "in")
plot(figure)
dev.off() 



