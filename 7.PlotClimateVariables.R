library(tidyverse)
library(ggplot2)
library(sf)
library(ggpubr)
library(zoo)
library(openxlsx)
library(readxl)
library(reshape2)


load('./Data/datos_totales.RData')
CantonsPop    <- as.data.frame(read.xlsx("./Data/CantonsPopulation.xlsx"))
#Cluster_Data <- as.data.frame(read.xlsx("./Data/Clima_clusters.xlsx"))
Cluster_Data  <- as.data.frame(read.xlsx("./Data/Veg_clusters.xlsx"))
#Dengue_cases <- datos_totales%>%dplyr::select(CCanton,Year,Canton,Month, Cases)

Dengue_cases  <- datos_totales%>%dplyr::select(CCanton,Year,Canton,Month, Cases=EVI)


#suma los casos por year per canton
DataPlot        = Dengue_cases%>%mutate(avr=rollapply(Dengue_cases$Cases,7,mean,fill=NA))

  i = 3
  Cluster1      = Cluster_Data%>%filter(groups==i)
  Data_Cluster1 = DataPlot%>%filter(CCanton%in%Cluster1$CCanton)%>%dplyr::select(Canton,Year,Month,avr)
  Data_Cluster1 = as.data.frame(Data_Cluster1%>%pivot_wider(names_from = Canton,values_from = avr))
  Data_Cluster1$Month <- 1:nrow(Data_Cluster1)
  xlabes = unique(Data_Cluster1$Year)
  Data_Cluster1$Year  <- NULL
  
  #melt data frame into long format
  df <- melt(Data_Cluster1,  id.vars = 'Month', variable.name = 'Canton')
  
  #create line plot for each column in data frame
 Plot1<-ggplot(df, aes(Month, value)) +
    geom_line(aes(colour = Canton))+
    theme_bw()+
    #scale_y_continuous(expand=c(0,0),limits = c(0,max(df$value,na.rm=0)+10))+
    scale_x_continuous(breaks = seq(1,nrow(Data_Cluster1),nrow(Data_Cluster1)/length(xlabes)),labels = xlabes)+
    labs(x="Years",y="Cases x 10 mil inhabitants")
  
  print(Plot1)
  #namePlot <- paste("FiguresCases/VegVariables_",i,".pdf",sep="")
  #ggsave(namePlot, width = 13, height = 6, units = "in")
  #plot(Plot1)
  #dev.off() 



