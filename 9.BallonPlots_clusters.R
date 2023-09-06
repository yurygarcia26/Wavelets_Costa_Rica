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
library(ggplot2)
library(zoo)
library(patchwork)




#load('./FileResults/Vector_Coherence_Dissimilarities_no_ndvi_cases.RData')
#Estos datos incluyen TNA, SS34, EVI, NDWI
#load('./FileResults/Vector_Coherence_Dissimilarities_VarClimaticas_no_ndvi_cases.RData')

#Estos datos incluyen TNA, cases, nino34
load('./FileResults/VC_Dissim_Clima.RData')
load('./Data/datos_totales.RData')

##----------------------------------------------
#Figure 1
datos_totales <- datos_totales%>%mutate(cases_100k=Cases*10000/Poblacion)
dengue_data   <- datos_totales%>%select("Year","Month","cases_100k","Canton")

dengue_data <- dengue_data%>%
  group_by(Canton) %>%
  mutate(month_number = row_number())

datos_totales <- datos_totales%>%
  group_by(Canton) %>%
  mutate(month_number = row_number())

Climate_Data <-datos_totales%>%filter(Canton=="Alajuela")
Climate_Data <-Climate_Data%>%mutate(month_number=1:nrow(Climate_Data))

##datos ballon

####################################
#Figure 2
my_cols  <- c("#6A00A8FF", "#B12A90FF","#E16462FF", "#FCA636FF", "#F0F921FF","yellow")
#my_cols <- c("#6A00A8FF", "#B12A90FF","#E16462FF")

###baloon plot 
CantonsPop <- datos_totales%>%select("CCanton","Canton","Poblacion")%>%filter(!duplicated(CCanton))

#suma los casos por year per canton
CantonCases = datos_totales%>%group_by(Year,CCanton,Canton)%>%dplyr::summarise(Cases=sum(Cases,na.rm = T))

CantonCases1 <- merge(CantonCases,CantonsPop,by=c("CCanton","Canton"))
CantonCases1 <- CantonCases1%>%mutate(cases_10K=10000*Cases/Poblacion)
CantonPlot   <- CantonCases1%>%select("Year","Canton","cases_10K")


##----------------------------------------------

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


#tna - ssta34
breaks1 <- c(1,2,3,4,5,6)
mycolor <- c(verde,rosa,cafe,azul,verdeazul,purpura)


#-------------------------------------------------
cantones <- datos_totales%>%
  select(Canton,CCanton)%>%distinct()

#create dendogram
dend <- as.dendrogram(hclust(Matrix.dis$dist.mat,method="ward.D2"))
dend <- color_branches(dend, k = number_of_cluster)
dend             <- color_labels(dend,k=number_of_cluster)
labels(dend)     <- cantones$Canton[order.dendrogram(dend)]
labels_cex(dend) <- 1.5     # increase the size of the labels

dend_tree <- dendro_data(dend)
plot(dend,horiz=TRUE, yaxt="n")

#ggdendrogram(dend,rotate = TRUE)


#pdf(file = "Plots2/dendo.pdf", width = 3, height = 12)
#par(mar = c(1, 1, 1, 10))  # set right margin to 10
#plot(dend, horiz = TRUE, yaxt = "n")
#dev.off()

png(file = "Plots2/dendo.png", width = 3*300, height = 12*300, res = 300)
par(mar = c(1, 1, 1, 10))
plot(dend, horiz = TRUE, yaxt = "n")
dev.off()

#Ballon-------------------------------------------------------
# reorder CantonPlot to match the order of the dendrogram
CantonPlot$Canton <- factor(CantonPlot$Canton, levels = labels(dend))


CantonPlot[CantonPlot == 0] <- NA
ballonPlot<-ggballoonplot(CantonPlot, x = "Year", y = "Canton",
                          size = "cases_10K", fill = "#6A00A8FF") +
  #scale_fill_gradientn(colors = my_cols, name = "cases per 10K") +
  #scale_color_discrete("darkblue")+
  scale_x_continuous(breaks = unique(CantonPlot$Year)) +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 14, angle = 0),
        legend.title = element_text(size =14,margin = margin(b = 10), 
        hjust = 0.5, vjust = 1),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title.align = 0.5)




###TNA
TNA_plot<-ggplot(Climate_Data)+
  #geom_line(aes(x=month_number,y=Nino34SSTA),color="black")+
  geom_line(aes(x=month_number,y=TNA),color="blue")+
  theme_bw()+
  theme(axis.text.x= element_blank(),
        axis.text.y=element_text(angle=0,size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=20))+
  labs(x="",y="")+
  scale_x_continuous(breaks = seq(min(Climate_Data$month_number), max(Climate_Data$month_number), by = 12)) +
  theme(panel.grid.major.x = element_line(linetype = "dashed"))



###-----------------------------------------------
#Option 1

NINO34_plot <- ggplot(Climate_Data)+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=2,ymax=Inf),    fill="#EF5350",alpha=0.5)+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=1.5,ymax=2),    fill="#EF9A9A",alpha=1)+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=1,ymax=1.5),    fill="#FFCDD2",alpha=0.1)+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=0,ymax=1),      fill="#FFEBEE",alpha=0.1)+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=0,ymax=-1),     fill="#e8f5ff",alpha=0.1)+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-1,ymax=-1.5),  fill="#c3e3fd",alpha=0.1)+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-1.5,ymax=-2),  fill="#9acef8",alpha=0.1)+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-2,ymax=-Inf),  fill="#2898ee",alpha=0.5)+
  geom_line(aes(x=1:length(Nino34SSTA),y=Nino34SSTA))+
  annotate("text", x = 220, y =  2.4,  label = "El Nino", size=3)+
  annotate("text", x = 220, y = -2.4,  label = "La Nina", size=3)+
  annotate("text", x = 0.5, y = 0.5,   label = "Weak",       color="red",     size=3)+
  annotate("text", x = 3,   y = 1.25,  label = "Moderate",   color="red",     size=3)+
  annotate("text", x = 1,   y = 1.75,  label = "Strong",     color="red",     size=3)+
  annotate("text", x = 5,   y = 2.5,   label = "Very Strong",color="white",   size=3)+
  annotate("text", x = 0.5, y = -0.5,  label = "Weak",       color="darkblue",size=3)+
  annotate("text", x = 3,   y = -1.25, label = "Moderate",   color="darkblue",size=3)+
  annotate("text", x = 1,   y = -1.75, label = "Strong",     color="darkblue",size=3)+
  annotate("text", x = 5,   y = -2.5,  label = "Very Strong",color="white",   size=3)+
  theme_bw()+
  theme(axis.text.x= element_blank(),
        axis.text.y=element_text(angle=0,size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=20))+
  labs(x="",y="Nino 3.4 SSTA")+
  scale_x_continuous(breaks = seq(min(Climate_Data$month_number), max(Climate_Data$month_number), by = 12)) +
  theme(panel.grid.major.x = element_line(linetype = "dashed"))



#combined_plot1 <-TNA_plot + NINO34_plot+ ballonPlot +
#  plot_layout(ncol = 1, heights = c(0.5,0.5,3),
#              shareX = TRUE)

#combined_plot1 <- plot_grid(TNA_plot, NINO34_plot, ballonPlot, ncol = 1, align = "v", axis = "tb")

#mypath <- file.path("Plots2/combined_plot1.png")
#png(mypath,width = 15, height = 12) 
#print(combined_plot1)
#dev.off()

mypath <- file.path("Plots2/combined_plot1.png")
png(mypath, width = 15, height = 12, units = "in", res = 300)
print(combined_plot1)
dev.off()




##Option 2
##--------
NINO2<-ggplot(Climate_Data)+
  geom_line(aes(x=month_number,y=Nino34SSTA),color="black")+
  #geom_line(aes(x=month_number,y=TNA),color="blue")+
  geom_ribbon(aes(x=month_number,ymin = 1, ymax = ifelse(Nino34SSTA>1, Nino34SSTA, 1),
                  fill = "Moderate"),fill="red") +
  geom_ribbon(aes(x=month_number,ymax =-1, ymin = ifelse(Nino34SSTA>-1,-1,Nino34SSTA),
                  fill = "Moderate"),fill="blue") +
  geom_hline(yintercept = -2,linetype = "dashed")+
  geom_hline(yintercept = -1,linetype = "dashed")+
  geom_hline(yintercept = 1,linetype = "dashed")+
  geom_hline(yintercept = 2,linetype = "dashed")+
  geom_hline(yintercept = 0)+
  annotate("text", x = 220, y =  2.5,  label = "El Nino", size=4)+
  annotate("text", x = 220, y = -2.5,  label = "La Nina", size=4)+
  annotate("text", x = 0, y = 0.5,   label = "Weak",       color="red",     size=3.5)+
  annotate("text", x = 0, y = 1.5,  label = "Moderate",   color="red",     size=3.5)+
  #annotate("text", x = 1,   y = 1.75,  label = "Strong",     color="red",     size=3.5)+
  annotate("text", x = 0,   y = 2.3,   label = "Very Strong",color="white",   size=3.5)+
  annotate("text", x = 0   , y = -0.5,  label = "Weak",       color="darkblue",size=3.5)+
  annotate("text", x = 0,   y = -1.5, label = "Moderate",   color="darkblue",size=3.5)+
  #annotate("text", x = 1,   y = -1.75, label = "Strong",     color="darkblue",size=3.5)+
  annotate("text", x = 0,   y = -2.3,  label = "Very Strong",color="white",   size=3.5)+
  theme_bw()+
  #labs(y="Temperature Anomaly (°C)",x="")+
  labs(y="",x="")+
  ylim(c(-3,3))+
  theme(axis.text.x=element_blank(),
        axis.text.y = element_text(size=14))+
  scale_x_continuous(breaks = seq(min(Climate_Data$month_number), max(Climate_Data$month_number), by = 12)) +
  theme(panel.grid.major.x = element_line(linetype = "dashed"))



combined_plot2 <- TNA_plot + NINO2+ ballonPlot +
  plot_layout(ncol = 1, heights = c(0.5,0.5, 3))

mypath <- file.path("Plots2/combined_plot2.png")
png(mypath, width = 15, height = 12, units = "in", res = 300)
print(combined_plot2)
dev.off()



#Time Series
datos_totales <- datos_totales%>%mutate(cases_100k=Cases*10000/Poblacion)
dengue_data   <- datos_totales%>%select("Year","Month","cases_100k","Canton")

dengue_data <- dengue_data%>%
  group_by(Canton) %>%
  mutate(month_number = row_number())

dengue_data$Canton <- factor(dengue_data$Canton, levels = rev(labels(dend)))

cases<-ggplot(dengue_data, aes(x = month_number, y = cases_100k)) +
  geom_line() +
  facet_wrap(~Canton, scales = "free_y", nrow = 32, ncol = 1)+
  scale_x_continuous(breaks = seq(1, 228, 12), labels = seq(2001, 2019, 1))+
  theme_bw()+
  labs(x="", y="")+
  theme(axis.text.x = element_text(size = 22,angle=90),
        axis.text.y = element_text(size = 14),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white"))

png(file ="Plots2/Cases_over_time.png", width = 12*300, height = 25*300, res = 300)
plot(cases, horiz = TRUE, yaxt = "n")
dev.off()


#guardar figure
combined_plot3 <-TNA_plot + NINO2+ cases +
plot_layout(ncol = 1, heights = c(2,2, 32))

mypath <- file.path("Plots2/combined_plot3.png")
png(mypath, width = 4000, height = 8000, res = 300)
print(combined_plot3)
dev.off()



















