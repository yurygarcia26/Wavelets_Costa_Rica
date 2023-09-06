#################################################################################
#                                 CLUSTER                                       #
#  Info para  crear arboles                                                     #
# https://yulab-smu.github.io/treedata-book/chapter9.html                       #
#-------------------------------------------------------------------------------#
library(openxlsx)
library(readxl)
library(tidyverse)
library(dendextend)
library(ape)
library(Hmisc)
library(dtw)
library(sf)
library(ggpubr)


#load('./FileResults/Vector_Coherence_Dissimilarities_no_ndvi_cases.RData')
#Estos datos incluyen TNA, SS34, EVI, NDWI
#load('./FileResults/Vector_Coherence_Dissimilarities_VarClimaticas_no_ndvi_cases.RData')

#Estos datos incluyen TNA, cases, nino34
load('./FileResults/VC_Dissim_Clima.RData')


load('./Data/datos_totales.RData')

#Elegir el numero de grupos que se quiere crear
number_of_cluster<-6
rosa          = "#db7590"
cafe          = "#e5b25f"
verdecafe     = "#c6c679"
verdelimon    = "#7ca43f"
verde         = "#2c8e22"
verdeazul     = "#5cc4d5"
azulTurquesa  = "#3fabc9"
azul          = "#3584da"
morado        = "#998ce1"
purpura       = "#cc4ec0"



#tna -ss34
#breaks1 <- c(1,2,3,4,5,6,7,8)
#mycolor <- c(verde,rosa,verdelimon,cafe,azul,verdeazul,purpura,morado)

#evi, ndwi -tna - ssta34
breaks1 <- c(1,2,3,4,5,6)
mycolor <- c(verde,rosa,cafe,azul,verdeazul,purpura)



#-------------------------------------------------
cantones <- datos_totales%>%
  select(Canton,CCanton)%>%distinct()

#Tree 1
aaa         <- as.dendrogram(hclust(Matrix.dis$dist.mat,method="ward.D2"))
aaa         <- color_labels(aaa,k=number_of_cluster)
labels(aaa) <- cantones$Canton[order.dendrogram(aaa)]
labels_cex(aaa) <- 0.6
plot(aaa, cex=0.5,main = "", ylab= "", xlab="",sub="", type = "triangle",cex.axis = 1)



#Tree2 unroot
ccc         <- as.dendrogram(hclust(Matrix.dis$dist.mat,method="ward.D2"))
ccc         <- color_labels(ccc,k=number_of_cluster)
labels(ccc) <- cantones$Canton[order.dendrogram(aaa)]
labels_cex(ccc) <- 0.4
clus4           <- cutree(ccc, number_of_cluster)
plot(as.phylo(ccc),type = "unrooted",
     no.margin = TRUE,tip.color = mycolor[clus4], cex = 0.5)


##Crear Mapa

#extraer los grupos
groups        <- cutree(aaa, k=number_of_cluster)
groups        <- as.data.frame(groups)
groups$Canton <- rownames(groups)
groups_data   <- merge(groups,cantones,by="Canton") 
#write.xlsx(groups_data,"./PhaseDiff/Data/Clima_clusters.xlsx")


#Archivos para el mapa
distritos_st <-st_read('Data/distritos_shape/Distritos_de_Costa_Rica.shp') %>%
  filter(CODIGO!=60110)

cantones_st <- distritos_st%>%
  mutate(DTA_C = str_sub(CODIGO,start = 1,end = 3))%>%
  group_by(DTA_C)%>%dplyr::summarise()%>%
  ungroup()%>% mutate(CCanton=as.numeric(DTA_C))


data_for_plot1<- merge(cantones_st,groups_data,by="CCanton",all=T)
data_for_plot <- merge(cantones_st,groups_data,by="CCanton",all=T)

Mapas<-function(i){
data_for_plot$groups[data_for_plot1$groups==i]=i
data_for_plot$groups[data_for_plot1$groups!=i]=0
  
Mapa <- ggplot(data   = data_for_plot) +
  geom_sf(mapping     = aes(fill=as.factor(groups),geometry=geometry),colour="black",
          show.legend = FALSE,size=0.5)+
  theme_void()+
  theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  #scale_fill_manual(breaks=breaks1,values=mycolor,na.value = 'transparent')
  scale_fill_manual(values=c("lightgray",mycolor[i]),na.value = 'transparent')


#mypath <- file.path("Plots/",paste("Clima",i,".pdf",sep=""))
#pdf(mypath,width = 7, height = 5) 
#print(Mapa)
#dev.off()

mypath <- file.path("Plots2/",paste("Clima",i,".png",sep=""))
png(mypath, width = 15, height = 12, units = "in", res = 300)
print(Mapa)
dev.off()

return(Mapa)
}


#----------------------------------------------
Map_cluster1<-Mapas(1)
Map_cluster2<-Mapas(2)
Map_cluster3<-Mapas(3)
Map_cluster4<-Mapas(4)
Map_cluster5<-Mapas(5)
Map_cluster6<-Mapas(6)


figure <- ggarrange(Map_cluster1, Map_cluster2, Map_cluster3,Map_cluster4,
                    Map_cluster5, Map_cluster6, 
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3","Cluster 4",
                               "Cluster 5", "Cluster 6"),
                    ncol = 3, nrow = 2,font.label = list(size = 12, color ="black"))

mypath <- file.path("Plots/All_clima_maps.pdf")

pdf(mypath,width = 10, height = 6) 
print(figure)
dev.off()
