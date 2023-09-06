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
#Dengue_cases  <- datos_totales%>%dplyr::select(CCanton,Year,Canton,Month, Cases)

Dengue_cases  <- datos_totales%>%dplyr::select(CCanton,Year,Canton,Month, Cases=EVI)


#suma los casos por year per canton
DataPlot        = merge(Dengue_cases ,CantonsPop,by=c("CCanton","Canton"),all=T)
DataPlot        = DataPlot%>%mutate(popx10=round(10000*Cases/Population))

for(i in 1:9){
  i=1
Cluster1      = Cluster_Data%>%filter(groups==i)
Data_Cluster1 = DataPlot%>%filter(CCanton%in%Cluster1$CCanton)%>%dplyr::select(Canton,Year,Month,popx10)
Data_Cluster1 = as.data.frame(Data_Cluster1%>%pivot_wider(names_from = Canton,values_from = popx10))
Data_Cluster1$Month <- 1:nrow(Data_Cluster1)
xlabes = unique(Data_Cluster1$Year)
Data_Cluster1$Year  <- NULL

#melt data frame into long format
df <- melt(Data_Cluster1,  id.vars = 'Month', variable.name = 'Canton')

#create line plot for each column in data frame
Plot1<-ggplot(df, aes(Month, value)) +
  geom_line(aes(colour = Canton))+
  theme_bw()+
  scale_y_continuous(expand=c(0,0),limits = c(0,max(df$value,na.rm=0)+10))+
  scale_x_continuous(breaks = seq(1,nrow(Data_Cluster1),nrow(Data_Cluster1)/length(xlabes)),labels = xlabes)+
  labs(x="Years",y="Cases x 10 mil inhabitants")

print(Plot1)
namePlot <- paste("FiguresCases/VegCluster_",i,".pdf",sep="")
ggsave(namePlot, width = 13, height = 6, units = "in")
plot(Plot1)
dev.off() 
}




##--------Noisy Cantons--------------------------------
#Noisy_Cantons <-c("Turrialba","Sarapiquí","Parrita","Quepos","Talamanca","Corredores",
#                  "Garabito","Osa","Golfito","Limon","Matina","Upala")

#clima
Noisy_Cantons <-c("Puntarenas", "Talamanca", "Parrita", "Limon","Corredores","Quepos","Matina","Turrialba")
Cases <- Dengue_cases%>%filter(Canton%in%Noisy_Cantons)
Cases$CCanton<-NULL

Cases1<-Cases%>%pivot_wider(names_from = Canton, values_from = Cases)
Cases1$Year<-NULL
Cases1$Month <- 1:nrow(Cases1)


Plot2 <-Cases1%>% tidyr::gather("id", "value", 2:ncol(Cases1)) %>% 
  ggplot(., aes(Month, value))+
  geom_line()+
  theme_bw()+
facet_wrap(~id)

namePlot <- paste("FiguresCases/Noisy_canton_Clima.pdf",sep="")
ggsave(namePlot, width = 13, height = 6, units = "in")
plot(Plot2)
dev.off() 
