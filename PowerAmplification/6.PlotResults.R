library(tidyverse)
library(ggplot2)
library(sf)
library(ggpubr)
library(zoo)
library(openxlsx)
library(readxl)
library(reshape2)
library(ggpubr)


COLNames <- c("Alajuela",	  "Alajuelita",	  "Atenas",	"Cañas",	"Carrillo",	
              "Corredores",	"Desamparados",	"Esparza",	"Garabito",
              "Golfito",   	"Guacimo", 	    "La Cruz", "Liberia",	"Limon",	
              "Matina",	    "Montes de Oro","Nicoya",	"Orotina",	"Osa",
              "Parrita",    "Perez Zeledón","Quepos","Pococí",	"Puntarenas",		
              "San Jose",   "Santa Ana",	  "SantaCruz",	"Sarapiquí",	"Siquirres",
              "Talamanca",	"Turrialba",    "Upala")

Data_ET   <- as.data.frame(read.xlsx("./FilesResult/Power_ET.xlsx",   sheet="Power"))
Data_EVI  <- as.data.frame(read.xlsx("./FilesResult/Power_EVI.xlsx",  sheet="Power"))
Data_NDWI <- as.data.frame(read.xlsx("./FilesResult/Power_NDWI.xlsx", sheet="Power"))
Data_Prec <- as.data.frame(read.xlsx("./FilesResult/Power_Prec.xlsx", sheet="Power"))

colnames(Data_ET)  <-COLNames
colnames(Data_EVI) <-COLNames
colnames(Data_NDWI)<-COLNames
colnames(Data_Prec)<-COLNames



Data_TNA    <- as.data.frame(read.xlsx("./FilesResult/Power_TNA.xlsx",sheet = "Power"))
Data_NINO34 <- as.data.frame(read.xlsx("./FilesResult/Power_NINO34.xlsx",sheet="Power"))

colnames(Data_TNA)   <-COLNames
colnames(Data_NINO34)<-COLNames

#-Period-------------------------------------------
Period  <- as.data.frame(read.xlsx("./FilesResult/Power_ET.xlsx",   sheet="Period"))
colnames(Period)  <-COLNames

##-----------------------------------------------------
Cluster_Clim<- as.data.frame(read.xlsx("./FilesResult/Clima_clusters.xlsx"))
Cluster_Veg <- as.data.frame(read.xlsx("./FilesResult/Veg_clusters.xlsx"))




###graficar clusters
PowerPlot<-function(df,i){
df$Period  = Period[,1]
indx       = which(df[,1]==max(df[,1],na.rm = T))
vline      = df$Period[indx]
#melt data frame into long format
df1 <- melt(df,  id.vars = 'Period', variable.name = 'Canton')

Plot1<-ggplot(df1, aes(Period, value)) +
  geom_line(aes(colour = Canton))+
  geom_vline(xintercept = vline, linetype="dashed",color = "black", size=0.5)+
  #geom_vline(xintercept = 3, linetype="dashed",color = "black", size=0.5)+
  theme_bw()+
  scale_y_continuous(expand=c(0,0),limits = c(0,max(df1$value,na.rm=T)))+
  #scale_x_continuous(breaks = seq(1,nrow(Data_ET),nrow(Data_Cluster1)/length(xlabes)),labels = xlabes)+
  labs(x="Period (Years)",y="")+
  coord_flip()

return(Plot1)}

plot_list = list()
for(i in 1:6){
cluster    = Cluster_Veg%>%filter(groups==i)
df         = Data_EVI%>%dplyr::select(cluster$Canton)
plot_list[[i]]= PowerPlot(df,i)

}



figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                    plot_list[[4]], plot_list[[5]], plot_list[[6]],
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3",
                               "Cluster 4", "Cluster 5", "Cluster 6"),
                    ncol = 2, nrow =3, font.label = list(size = 12, color ="black"),vjust=0.3)

annotate_figure(figure, top = text_grob(""))

#annotate_figure(figure, top = text_grob("Time−Average Wavelet Power", 
#                                      face = "bold", size = 14))

namePlot <- paste("Figures/Power_EVI.pdf",sep="")
ggsave(namePlot, width = 15, height = 8, units = "in")
plot(figure)
dev.off() 



###plot NDWI---------------------------------------------
plot_list = list()
for(i in 1:6){
  cluster    = Cluster_Veg%>%filter(groups==i)
  df         = Data_NDWI%>%dplyr::select(cluster$Canton)
  plot_list[[i]]= PowerPlot(df,i)
  
}


figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                    plot_list[[4]], plot_list[[5]], plot_list[[6]],
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3",
                               "Cluster 4", "Cluster 5", "Cluster 6"),
                    ncol = 2, nrow =3,font.label = list(size = 12, color ="black"),vjust=0.3)

annotate_figure(figure, top = text_grob(""))

namePlot <- paste("Figures/Power_NDWI.pdf",sep="")
ggsave(namePlot, width = 15, height = 10, units = "in")
plot(figure)
dev.off() 

###plot ET---------------------------------------------
plot_list = list()
for(i in 1:6){
  cluster    = Cluster_Veg%>%filter(groups==i)
  df         = Data_ET%>%dplyr::select(cluster$Canton)
  plot_list[[i]]= PowerPlot(df,i)
  
}


figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                    plot_list[[4]], plot_list[[5]], plot_list[[6]],
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3",
                               "Cluster 4", "Cluster 5", "Cluster 6"),
                    ncol = 2, nrow =3,font.label = list(size = 12, color ="black"),vjust=0.3)

annotate_figure(figure, top = text_grob(""))

namePlot <- paste("Figures/Power_ET.pdf",sep="")
ggsave(namePlot, width = 15, height = 10, units = "in")
plot(figure)
dev.off() 

###plot Prec---------------------------------------------
plot_list = list()
for(i in 1:6){
  cluster    = Cluster_Veg%>%filter(groups==i)
  df         = Data_Prec%>%dplyr::select(cluster$Canton)
  plot_list[[i]]= PowerPlot(df,i)
  
}



figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                    plot_list[[4]], plot_list[[5]], plot_list[[6]],
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3",
                               "Cluster 4", "Cluster 5", "Cluster 6"),
                    ncol = 2, nrow =3,font.label = list(size = 12, color ="black"),vjust=0.3)

annotate_figure(figure, top = text_grob(""))

namePlot <- paste("Figures/Power_Prec.pdf",sep="")
ggsave(namePlot, width = 15, height = 10, units = "in")
plot(figure)
dev.off() 


###plot TNA---------------------------------------------
plot_list = list()
for(i in 1:6){
  cluster    = Cluster_Clim%>%filter(groups==i)
  df         = Data_TNA%>%dplyr::select(cluster$Canton)
  plot_list[[i]]= PowerPlot(df,i)
  
}

figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                    plot_list[[4]], plot_list[[5]], plot_list[[6]],
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3",
                               "Cluster 4", "Cluster 5", "Cluster 6"),
                    ncol = 2, nrow =3,font.label = list(size = 12, color ="black"),vjust=0.3)

#annotate_figure(figure, top = text_grob("Time−Average Wavelet Power", 
#                                        face = "bold", size = 14))

namePlot <- paste("Figures/Power_TNA.pdf",sep="")
ggsave(namePlot, width = 15, height = 10, units = "in")
plot(figure)
dev.off() 



###plot NINO34---------------------------------------------
plot_list = list()
for(i in 1:6){
  cluster    = Cluster_Clim%>%filter(groups==i)
  df         = Data_NINO34%>%dplyr::select(cluster$Canton)
  plot_list[[i]]= PowerPlot(df,i)
  
}

figure <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                    plot_list[[4]], plot_list[[5]], plot_list[[6]],
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3",
                               "Cluster 4", "Cluster 5", "Cluster 6"),
                    ncol = 2, nrow =3,font.label = list(size = 12, color ="black"),vjust=0.3)

#annotate_figure(figure, top = text_grob("Time−Average Wavelet Power", 
#                                        face = "bold", size = 14))

namePlot <- paste("Figures/Power_NINO34.pdf",sep="")
ggsave(namePlot, width = 15, height = 10, units = "in")
plot(figure)
dev.off() 




