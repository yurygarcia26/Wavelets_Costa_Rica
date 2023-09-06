library(openxlsx)
library(readxl)
library(tidyverse)
library(dendextend)
library(ape)
library(Hmisc)
library(dtw)
library(sf)
library(NbClust)
library(dendextend)
library(gridExtra)

load('./Data/datos_totales.RData')

best.cluster<-function(diss=diss,method = "ward.D",min=2,max=15){
  res1<-NbClust(diss=diss, distance = NULL, min.nc=min, max.nc=max, 
                method = "ward.D", index = "silhouette")
  res2<-NbClust(diss=diss, distance = NULL, min.nc=min, max.nc=max, 
                method = "ward.D", index = "frey")
  res3<-NbClust(diss=diss, distance = NULL, min.nc=min, max.nc=max, 
                method = "ward.D", index = "mcclain")
  res4<-NbClust(diss=diss, distance = NULL, min.nc=min, max.nc=max, 
                method = "ward.D", index = "cindex")
  res5<-NbClust(diss=diss, distance = NULL, min.nc=min, max.nc=max, 
                method = "ward.D", index = "dunn")
  result<-rbind(res1$Best.nc,res2$Best.nc,res3$Best.nc,res4$Best.nc,res5$Best.nc)
  result<-data.frame(c("silhouette","frey","mcclain","cindex","dunn"),result)
  colnames(result)<-c("index","num_cluster","value")
  return(result)
}


cantones <- datos_totales%>%
  select(Canton,CCanton)%>%distinct()

# 1. conjuntamente todas las variables climáticas ----------------------------
load('./FileResults/VC_Dissim_Clima.RData')
Matrix.dis.clima<-Matrix.dis
load('./FileResults/VC_Dissim_all_veg.RData')
Matrix.dis.vegetation<-Matrix.dis
load('./FileResults/VC_Dissim_evi_ndwi.RData')
Matrix.dis.evi.ndwi<-Matrix.dis
load('./FileResults/VC_Dissim_evi_ndwi_et.RData')
Matrix.dis.evi.ndwi.et<-Matrix.dis
load('./FileResults/VC_Dissim_evi_ndwi_prec.RData')
Matrix.dis.evi.ndwi.prec<-Matrix.dis


(res.clima        <-best.cluster(diss=Matrix.dis.clima$dist.mat,method = "ward.D2",min=2,max=15))
(res.vegetation   <-best.cluster(diss=Matrix.dis.vegetation$dist.mat,method = "ward.D2",min=2,max=15))
(res.evi.ndwi     <-best.cluster(diss=Matrix.dis.evi.ndwi$dist.mat,method = "ward.D2",min=2,max=15))
(res.evi.ndwi.et  <-best.cluster(diss=Matrix.dis.evi.ndwi.et$dist.mat,method = "ward.D2",min=2,max=15))
(res.evi.ndwi.prec<-best.cluster(diss=Matrix.dis.evi.ndwi.prec$dist.mat,method = "ward.D2",min=2,max=15))

cbind(res.clima[,-3],res.vegetation$num_cluster,res.evi.ndwi$num_cluster,
      res.evi.ndwi.et$num_cluster,res.evi.ndwi.prec$num_cluster)


number_of_cluster <- 8
number_of_cluster_vegetation <- 7

rosa      = "#d875ad"
cafe      = "#c38d3f"
verde1    = "#989f3f"
verde     = "#2aa454"
verdeazul = "#a6e9ee"
azul      = "#259fc1"
morado    = "#998ce1"
purpura   = "#cc4ec0"
breaks1 <- c(1,2,3,4,5,6,7,8)
mycolor <- c(rosa,verde,cafe,verdeazul,verde1,purpura,morado,azul)

par(mfrow=c(3,2))

cluster.clima         <- hclust(Matrix.dis.clima$dist.mat,method="ward.D2")
cluster.clima         <- color_labels(cluster.clima,k=number_of_cluster,col=mycolor)
labels(cluster.clima) <- cantones$Canton[order.dendrogram(cluster.clima)]
plot(cluster.clima,cex=0.2, main = "clima", ylab="", xlab="",     sub="") 

cluster.vegetation         <- hclust(Matrix.dis.vegetation$dist.mat,method="ward.D2")
cluster.vegetation         <- color_labels(cluster.vegetation,k=number_of_cluster_vegetation,col=mycolor)
labels(cluster.vegetation) <- cantones$Canton[order.dendrogram(cluster.vegetation)]
plot(cluster.vegetation,cex=0.2, main = "vegetation", ylab="", xlab="",     sub="") 

cluster.evi.ndwi         <- hclust(Matrix.dis.evi.ndwi$dist.mat,method="ward.D2")
cluster.evi.ndwi         <- color_labels(cluster.evi.ndwi,k=number_of_cluster_vegetation,col=mycolor)
labels(cluster.evi.ndwi) <- cantones$Canton[order.dendrogram(cluster.evi.ndwi)]
plot(cluster.evi.ndwi,cex=0.2, main = "evi.ndwi", ylab="", xlab="",     sub="") 

cluster.evi.ndwi.et         <- hclust(Matrix.dis.evi.ndwi.et$dist.mat,method="ward.D2")
cluster.evi.ndwi.et         <- color_labels(cluster.evi.ndwi.et,k=number_of_cluster_vegetation,col=mycolor)
labels(cluster.evi.ndwi.et) <- cantones$Canton[order.dendrogram(cluster.evi.ndwi.et)]
plot(cluster.evi.ndwi.et,cex=0.2, main = "evi.ndwi.et", ylab="", xlab="",     sub="") 

cluster.evi.ndwi.prec         <- hclust(Matrix.dis.evi.ndwi.prec$dist.mat,method="ward.D2")
cluster.evi.ndwi.prec         <- color_labels(cluster.evi.ndwi.prec,k=number_of_cluster_vegetation,col=mycolor)
labels(cluster.evi.ndwi.prec) <- cantones$Canton[order.dendrogram(cluster.evi.ndwi.prec)]
plot(cluster.evi.ndwi.prec,cex=0.2, main = "evi.ndwi.prec", ylab="", xlab="",     sub="") 


##Crear Mapa

#extraer los grupos

groups.clima            <- cutree(cluster.clima, k=number_of_cluster)
groups.clima        <- as.data.frame(groups.clima)
groups.clima$Canton <- rownames(groups.clima)

groups.vegetation            <- cutree(cluster.vegetation, k=number_of_cluster_vegetation)
groups.vegetation        <- as.data.frame(groups.vegetation)
groups.vegetation$Canton <- rownames(groups.vegetation)

groups.evi.ndwi           <- cutree(cluster.evi.ndwi, k=number_of_cluster_vegetation)
groups.evi.ndwi        <- as.data.frame(groups.evi.ndwi)
groups.evi.ndwi$Canton <- rownames(groups.evi.ndwi)

groups.evi.ndwi.et           <- cutree(cluster.evi.ndwi.et, k=number_of_cluster_vegetation)
groups.evi.ndwi.et        <- as.data.frame(groups.evi.ndwi.et)
groups.evi.ndwi.et$Canton <- rownames(groups.evi.ndwi.et)

groups.evi.ndwi.prec           <- cutree(cluster.evi.ndwi.prec, k=number_of_cluster_vegetation)
groups.evi.ndwi.prec        <- as.data.frame(groups.evi.ndwi.prec)
groups.evi.ndwi.prec$Canton <- rownames(groups.evi.ndwi.prec)

groups.all <- groups.clima %>%
  left_join(groups.vegetation, by="Canton") %>%
  left_join(groups.evi.ndwi, by="Canton") %>%
  left_join(groups.evi.ndwi.et, by="Canton") %>%
  left_join(groups.evi.ndwi.prec, by="Canton") 


groups_data   <- merge(groups.all,cantones,by="Canton") 


#Archivos para el mapa
distritos_st <-st_read('Data/distritos_shape/Distritos_de_Costa_Rica.shp') %>%
  filter(CODIGO!=60110)

cantones_st <- distritos_st %>%
  mutate(DTA_C = str_sub(CODIGO,start = 1,end = 3))%>%
  group_by(DTA_C)%>% summarise() %>%
  ungroup() %>% mutate(CCanton=as.numeric(DTA_C))


data_for_plot<- merge(groups_data,cantones_st,by="CCanton",all=T)


Mapa.clima <- ggplot(data = data_for_plot) +
  geom_sf(mapping   = aes(fill=as.factor(groups.clima),geometry=geometry),colour="gray",show.legend = FALSE)+
  theme_void()+labs(title="clima")+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_manual(breaks=breaks1,values=mycolor,na.value = 'transparent')

Mapa.vegetation <- ggplot(data = data_for_plot) +
  geom_sf(mapping   = aes(fill=as.factor(groups.vegetation),geometry=geometry),colour="gray",show.legend = FALSE)+
  theme_void()+labs(title="vegetation")+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_manual(breaks=breaks1,values=mycolor,na.value = 'transparent')

Mapa.evi.ndwi <- ggplot(data = data_for_plot) +
  geom_sf(mapping   = aes(fill=as.factor(groups.evi.ndwi),geometry=geometry),colour="gray",show.legend = FALSE)+
  theme_void()+labs(title="evi,ndwi")+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_manual(breaks=breaks1,values=mycolor,na.value = 'transparent')

Mapa.evi.ndwi.et <- ggplot(data = data_for_plot) +
  geom_sf(mapping   = aes(fill=as.factor(groups.evi.ndwi.et),geometry=geometry),colour="gray",show.legend = FALSE)+
  theme_void()+labs(title="evi,ndwi.et")+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_manual(breaks=breaks1,values=mycolor,na.value = 'transparent')

Mapa.evi.ndwi.prec <- ggplot(data = data_for_plot) +
  geom_sf(mapping   = aes(fill=as.factor(groups.evi.ndwi.prec),geometry=geometry),colour="gray",show.legend = FALSE)+
  theme_void()+labs(title="evi,ndwi.prec")+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  scale_fill_manual(breaks=breaks1,values=mycolor,na.value = 'transparent')


grid.arrange(Mapa.clima,Mapa.vegetation,Mapa.evi.ndwi,Mapa.evi.ndwi.et,Mapa.evi.ndwi.prec, 
             nrow = 3)

