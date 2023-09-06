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
library(ggdendro)
library(ggstance)
library(cowplot)



#load('./FileResults/Vector_Coherence_Dissimilarities_no_ndvi_cases.RData')
#Estos datos incluyen TNA, SS34, EVI, NDWI
#load('./FileResults/Vector_Coherence_Dissimilarities_VarClimaticas_no_ndvi_cases.RData')

#Estos datos incluyen TNA, cases, nino34
load('./FileResults/VC_Dissim_all_veg.RData')
load('./Data/datos_totales.RData')

##----------------------------------------------
#Figure 1
datos_totales <- datos_totales%>%mutate(cases_100k=Cases*10000/Poblacion)
dengue_data   <- datos_totales%>%select("Year","Month","cases_100k","Canton")

dengue_data <- dengue_data%>%
  group_by(Canton)%>%
  mutate(month_number = row_number())

datos_totales <-datos_totales%>%
  group_by(Canton) %>%
  mutate(month_number = row_number())



####################################
#Figure 2
my_cols  <- c("#6A00A8FF", "#B12A90FF","#E16462FF", "#FCA636FF", "#F0F921FF","yellow")

###baloon plot 
CantonsPop <- datos_totales%>%select("CCanton","Canton","Poblacion")%>%filter(!duplicated(CCanton))

#suma los casos por year per canton
CantonCases = datos_totales%>%group_by(Year,CCanton,Canton)%>%dplyr::summarise(Cases=sum(Cases,na.rm = T))

CantonCases1 <- merge(CantonCases,CantonsPop,by=c("CCanton","Canton"))
CantonCases1 <- CantonCases1%>%mutate(cases_10K=10000*Cases/Poblacion)
CantonPlot   <- CantonCases1%>%select("Year","Canton","cases_10K")


##----------------------------------------------
#Elegir el numero de grupos que se quiere crear
#Elegir el numero de grupos que se quiere crear
number_of_cluster<-7
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

breaks1 <- c(1,2,3,4,5,6,7)
mycolor <- c(azul,purpura,verde,cafe,verdeazul,rosa,verdecafe,morado,verdelimon)


#create dendogram
cantones1 <- datos_totales%>%
  select(Canton,CCanton)%>%distinct()

indices=which(cantones1$Canton%in%c("Upala","Quepos"))
cantones = cantones1[-indices,]

#create dendogram
dend <- as.dendrogram(hclust(Matrix.dis$dist.mat,method="ward.D2"))
dend <- color_branches(dend, k = number_of_cluster)
dend             <- color_labels(dend,k=number_of_cluster)
labels(dend)     <- cantones$Canton[order.dendrogram(dend)]
labels_cex(dend) <- 1.5     # increase the size of the labels

png(file = "Plots2/dendo_veg.png", width = 3*300, height = 12*300, res = 300)
par(mar = c(1, 1, 1, 10))
plot(dend, horiz = TRUE, yaxt = "n")
dev.off()



#Ballon-------------------------------------------------------
# reorder CantonPlot to match the order of the dendrogram
new_labels<-c(labels(dend),"Upala","Quepos")
CantonPlot$Canton <- factor(CantonPlot$Canton, levels = new_labels)


CantonPlot[CantonPlot == 0] <- NA
ballonPlot<-ggballoonplot(CantonPlot, x = "Year", y = "Canton",
                          size = "cases_10K", fill = "#6A00A8FF") +
  #scale_fill_gradientn(colors = my_cols, name = "cases per 10K") +
  scale_x_continuous(breaks = unique(CantonPlot$Year)) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16, angle = 0),
        legend.title = element_text(size =16,margin = margin(b = 10), 
        hjust = 0.5, vjust = 1),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title.align = 0.5)

png(file = "Plots2/Ballon.png", width = 12*300, height = 20*300, res = 300)
#par(mar = c(1, 1, 1, 10))
plot(ballonPlot, horiz = TRUE, yaxt = "n")
dev.off()

###-----------------------------------------------
#Option 1
datos_totales1 <- datos_totales%>%
  group_by(Canton) %>%mutate(month_number = row_number())%>%
  select(Canton,month_number,EVI,NDWI)

datos_totales1$Canton <- factor(datos_totales1$Canton, levels = rev(new_labels))

vegPlot<-ggplot(datos_totales1) +
  geom_line(aes(x = month_number, y = EVI ),color="green") +
  geom_line(aes(x = month_number, y = NDWI ), color="purple") +
  #geom_line(aes(x = month_number, y = Precip_t ), color="orange") +
  #geom_line(aes(x = month_number, y = ET ),color="magenta") +
  facet_wrap(~Canton, scales = "free_y", nrow = 32, ncol = 1,strip.position = "right")+
  scale_x_continuous(breaks = seq(1, 228, 12), labels = seq(2001, 2019, 1))+
  theme_bw()+
  labs(x="", y="")+
  theme(axis.text.x = element_text(size = 16,angle=90),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white"))
        #strip.text  = element_text(size  = 10), # added to reduce size of title
        #strip.background = element_rect(fill="white",size=0.3,color="black"),
        #panel.background = element_rect(fill = "white"),
        #strip.switch.pad.grid = unit(0, "mm")) # added to change background color

png(file = "Plots2/veg_over_time.png", width = 6*300, height = 20*300, res = 300)
#par(mar = c(1, 1, 1, 10))
plot(vegPlot, horiz = TRUE, yaxt = "n")
dev.off()


#Option 2
datos_totales2 <- datos_totales%>%
  group_by(Canton) %>%mutate(month_number = row_number())%>%
  select(Canton,month_number,ET,Precip_t)

datos_totales2$Canton <- factor(datos_totales2$Canton, levels = rev(new_labels))

vegPlot2<-ggplot(datos_totales2) +
  geom_line(aes(x = month_number, y = Precip_t ), color="blue") +
  geom_line(aes(x = month_number, y = ET ),color="orange") +
  facet_wrap(~Canton, scales = "free_y", nrow = 32, ncol = 1)+
  scale_x_continuous(breaks = seq(1, 228, 12), labels = seq(2001, 2019, 1))+
  theme_bw()+
  labs(x="", y="")+
  theme(axis.text.x = element_text(size = 16,angle=90),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white"))
#strip.text  = element_text(size  = 10), # added to reduce size of title
#strip.background = element_rect(fill="white",size=0.3,color="black"),
#panel.background = element_rect(fill = "white"),
#strip.switch.pad.grid = unit(0, "mm")) # added to change background color

png(file = "Plots2/veg_over_time2.png", width = 6*300, height = 20*300, res = 300)
#par(mar = c(1, 1, 1, 10))
plot(vegPlot2, horiz = TRUE, yaxt = "n")
dev.off()


#Time Series
datos_totales <- datos_totales%>%mutate(cases_100k=Cases*10000/Poblacion)
dengue_data   <- datos_totales%>%select("Year","Month","cases_100k","Canton")

dengue_data <- dengue_data%>%
  group_by(Canton) %>%
  mutate(month_number = row_number())

dengue_data$Canton <- factor(dengue_data$Canton, levels = rev(new_labels))

cases<-ggplot(dengue_data, aes(x = month_number, y = cases_100k)) +
  geom_line() +
  facet_wrap(~Canton, scales = "free_y", nrow = 32, ncol = 1)+
  scale_x_continuous(breaks = seq(1, 228, 12), labels = seq(2001, 2019, 1))+
  theme_bw()+
  labs(x="", y="")+
  theme(axis.text.x = element_text(size = 22,angle=90),
        axis.text.y = element_text(size = 12),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white"))

png(file ="Plots2/Cases_over_time_veg.png", width = 10*300, height = 25*300, res = 300)
plot(cases, horiz = TRUE, yaxt = "n")
dev.off()


