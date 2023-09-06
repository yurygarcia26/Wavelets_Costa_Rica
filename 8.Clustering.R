library(dplyr)
library(pscl)
library(VGAM)
library(ggplot2)
library(tidyverse)
library(GGally)
library(gamlss)
library(astsa)
library(readxl)

library(tidyverse)
library(reshape2)
library(dtw)
library(gridExtra)
library(sf)
library(openxlsx)
library(dendextend)
library(ape)

# 0. reading data ------------------------------------------------------------

load(file = './Data/datos_totales.RData')
str(datos_totales)
dim(datos_totales)
table(datos_totales$Year,datos_totales$Month)

# creating time index
datos_totales$time <- paste0(datos_totales$Year,"-",datos_totales$Month,"-",15)
datos_totales$time <- as.Date(datos_totales$time, "%Y-%m-%d")

#datos_totales1<-na.omit(datos_totales)

dim(datos_totales)

datos_totales2 <- datos_totales %>% 
  dplyr::mutate(logCases=log(Cases+0.5),logRR=log(RR+0.5),sqrtCases=sqrt(Cases))%>% 
  dplyr::select(Canton,CCanton,time,Cases,logCases,RR,logRR,sqrtCases,Nino12SSTA,Nino3SSTA,Nino4SSTA,Nino34SSTA,TNA,
                Poblacion, PoblacionCR,
                EVI,NDVI,NDWI,LSD,LSN,OFF) 


cantones <- datos_totales%>%
  dplyr::select(Canton,CCanton)%>%distinct()


# Clustering con logCases, logRR o sqrtCases ------------------------------

# sqrt(cases) -------------------------------------------------------------

#logCases
sqrtCases_wide <- dcast(datos_totales2, Canton ~ time, value.var="logCases")

#RR
# sqrtCases_wide <- dcast(datos_totales2, Canton ~ time, value.var="RR")

#sqrtCases
# sqrtCases_wide <- dcast(datos_totales2, Canton ~ time, value.var="sqrtCases")
sqrtCases_wide1 <- sqrtCases_wide %>% dplyr::select(-Canton)
dim(sqrtCases_wide1)
distMatrix_sqrtCases <- dist(sqrtCases_wide1, method="DTW")

sqrtCases_hc <- hclust(distMatrix_sqrtCases, method="ward.D2")

plot(sqrtCases_hc,  main="")
rect.hclust(sqrtCases_hc, k = 2, border = 2)
rect.hclust(sqrtCases_hc, k = 3, border = 3)
rect.hclust(sqrtCases_hc, k = 4, border = 4) # 4 clusters
rect.hclust(sqrtCases_hc, k = 5, border = 5)
rect.hclust(sqrtCases_hc, k = 6, border = 6)

cluster2_sqrtCases <- cutree(sqrtCases_hc, k = 2)
cluster3_sqrtCases <- cutree(sqrtCases_hc, k = 3)
cluster4_sqrtCases <- cutree(sqrtCases_hc, k = 4)
cluster5_sqrtCases <- cutree(sqrtCases_hc, k = 5)
cluster6_sqrtCases <- cutree(sqrtCases_hc, k = 6)
sqrtCases_wide$cluster2_sqrtCases <- as.factor(cluster2_sqrtCases)
sqrtCases_wide$cluster3_sqrtCases <- as.factor(cluster3_sqrtCases)
sqrtCases_wide$cluster4_sqrtCases <- as.factor(cluster4_sqrtCases)
sqrtCases_wide$cluster5_sqrtCases <- as.factor(cluster5_sqrtCases)
sqrtCases_wide$cluster6_sqrtCases <- as.factor(cluster6_sqrtCases)

results_sqrtCases <- sqrtCases_wide %>% dplyr::select(Canton,cluster2_sqrtCases,cluster3_sqrtCases,cluster4_sqrtCases, cluster5_sqrtCases, cluster6_sqrtCases)

list_canton <- unique(datos_totales[c("Canton","CCanton")])

results_sqrtCases <- results_sqrtCases %>% left_join(list_canton,by=c("Canton"="Canton"))


#Archivos para el mapa
distritos_st <-st_read('Data/distritos_shape/Distritos_de_Costa_Rica.shp') %>%
  filter(CODIGO!=60110)

cantones_st    <- distritos_st%>%
  mutate(DTA_C = str_sub(CODIGO,start = 1,end = 3))%>%
  group_by(DTA_C)%>%dplyr::summarise()%>%
  ungroup()%>% mutate(CCanton=as.numeric(DTA_C))

codCantones <- cantones_st %>% left_join(results_sqrtCases,by=c("CCanton"="CCanton"))

data_frame <- codCantones %>% st_drop_geometry()


write.xlsx(data_frame, './clusters_casos.xlsx')



rosa          = "#db7590"
cafe          = "#e5b25f"
verdecafe     = "#c6c679"
verdelimon    = "#7ca43f"
verde         = "#2c8e22"
azulTurquesa  = "#3fabc9"
azul          = "#3584da"
morado        = "#998ce1"
purpura       = "#cc4ec0"
gris          = "#636363"

mycolor <- c(rosa,cafe,verdecafe,purpura,verde,morado,azulTurquesa,gris)

mapa.2 <- ggplot() + 
  geom_sf(data = codCantones, aes(fill = as.factor(cluster2_sqrtCases),
                                  geometry=geometry), color = "black",
          alpha=0.4) +
  labs(fill = "3 Clusters") +
  theme_void()

mapa.3 <- ggplot() + 
  geom_sf(data = codCantones, aes(fill = as.factor(cluster3_sqrtCases),
                                  geometry=geometry), color = "black",
          alpha=0.4) +
  labs(fill = "3 Clusters") +
    theme_void()#+
  #   theme(axis.ticks  = element_blank(),axis.text = element_blank())+
  # theme( legend.key.size = unit(1, 'cm'),
  #        legend.title = element_text(size=30),
  #        legend.text = element_text(size=30),
  #        strip.text = element_text(size = 30))

verde         = "#2c8e22"
azul          = "#3584da"
morado        = "#998ce1"
rosa          = "#db7590"
cafe          = "#e5b25f"

mapa.4 <- ggplot() + 
    geom_sf(data = codCantones, aes(fill = as.factor(cluster4_sqrtCases),geometry=geometry), color = "black",
            alpha=0.6) +
    labs(fill = "4 Clusters") +
    theme_void()+
    scale_fill_manual(values=c(verde, azul,cafe,rosa),na.value = 'transparent')

    # theme(axis.ticks  = element_blank(),axis.text = element_blank())+
    # theme( legend.key.size = unit(1, 'cm'),
    #        legend.title = element_text(size=30),
    #        legend.text = element_text(size=30),
    #        strip.text = element_text(size = 30)))

mapa.5 <- ggplot() + 
    geom_sf(data = codCantones, aes(fill = as.factor(cluster5_sqrtCases),geometry=geometry), color = "black",
            alpha=0.4) +
    labs(fill = "5 Clusters")+
    theme_void()+
    scale_fill_manual(values=c(verde, azul,morado,rosa,cafe),na.value = 'transparent')

    # theme(axis.ticks  = element_blank(),axis.text = element_blank())+
    # theme( legend.key.size = unit(1, 'cm'),
    #        legend.title = element_text(size=30),
    #        legend.text = element_text(size=30),
    #        strip.text = element_text(size = 30)))


mapa.6 <- ggplot() + 
  geom_sf(data = codCantones, aes(fill = as.factor(cluster6_sqrtCases),
                                  geometry=geometry), color = "black",
          alpha=0.4) +
  labs(fill = "6 Clusters") +
  theme_void()#+
# theme(axis.ticks  = element_blank(),axis.text = element_blank())+
# theme( legend.key.size = unit(1, 'cm'),
#        legend.title = element_text(size=30),
#        legend.text = element_text(size=30),
#        strip.text = element_text(size = 30)))

  mapa.3
  mapa.4
  mapa.5
  mapa.6
  
  

# archivos para el paper --------------------------------------------------

  pdf("descriptive_clustering2.pdf",width = 5, height = 10) 
  # dendrograma
  number_of_cluster <-4
  ccc         <- as.dendrogram(hclust(distMatrix_sqrtCases,method="ward.D2"))
  ccc         <- color_labels(ccc,k=number_of_cluster)
  labels(ccc) <- cantones$Canton[order.dendrogram(ccc)]
  labels_cex(ccc) <- 0.5
  clus4           <- cutree(ccc, number_of_cluster)
  plot(as.phylo(ccc),type = "phylogram",
       no.margin = TRUE,tip.color = mycolor[clus4], cex = 0.8)
  
  
  #el mapa
  
  mapa.4
  
  dev.off()
  