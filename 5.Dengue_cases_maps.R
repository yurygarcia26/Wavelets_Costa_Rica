library(tidyverse)
library(ggplot2)
library(sf)
library(ggpubr)
library(zoo)
library(openxlsx)
library(readxl)


load('./Data/datos_totales.RData')
CantonsPop <- as.data.frame(read.xlsx("./Data/CantonsPopulation.xlsx"))


#Archivos para el mapa
distritos_st <-st_read('Data/distritos_shape/Distritos_de_Costa_Rica.shp') %>%
  filter(CODIGO!=60110)

cantones_st    <- distritos_st%>%
  mutate(DTA_C = str_sub(CODIGO,start = 1,end = 3))%>%
  group_by(DTA_C)%>%dplyr::summarise()%>%
  ungroup()%>% mutate(CCanton=as.numeric(DTA_C))


codCantones <- cantones_st%>%
  select(CCanton)%>%distinct()%>% 
  slice(rep(1:n(), 19))%>%mutate(Year=c(rep(2001,81),rep(2002,81),rep(2003,81),
                                         rep(2004,81),rep(2005,81),rep(2006,81),
                                         rep(2007,81),rep(2008,81),rep(2009,81),
                                         rep(2010,81),rep(2011,81),rep(2012,81),
                                         rep(2013,81),rep(2014,81),rep(2015,81),
                                         rep(2016,81),rep(2017,81),rep(2018,81),
                                         rep(2019,81)))

#suma los casos por year per canton
CantonCases     = datos_totales%>%group_by(Year,CCanton,Canton)%>%dplyr::summarise(Cases=sum(Cases,na.rm = T))


DataPlot        = merge(codCantones,CantonCases,by=c("CCanton","Year"),all=TRUE)
DataPlot        = merge(DataPlot,CantonsPop,by=c("CCanton","Canton"),all=T)
DataPlot        = DataPlot%>%mutate(popx10=round(10000*Cases/Population))
max_value_cases = max(DataPlot$popx10,na.rm=T)
Year<- unique(DataPlot$Year)

###Mapas
my_plots <- map(
  .x = Year,
  .f = ~ggplot(data     = DataPlot%>%filter(Year==.x)) +
    geom_sf(mapping     = aes(fill=popx10,geometry=geometry),colour="black",
            show.legend = FALSE,size=0.1,alpha=0.8)+
    theme_void()+
    theme(axis.ticks  = element_blank(),
          axis.text   = element_blank())+
    scale_fill_gradient(low = "lightcyan", high = "darkblue", na.value = 'transparent', limits=c(0,max_value_cases))+
    ggtitle(.x)+
    theme(plot.title = element_text(vjust = -50, hjust=0.3))   
)

Figure<-ggarrange(plotlist = my_plots, ncol = 5, nrow = 4)

mypath <- file.path("Plots/Cases_2001_2019.pdf")
pdf(mypath,width = 15, height = 12) 
print(Figure)
dev.off()

###-----------------------------------------------
##Climate variables


Climate_Data <- datos_totales%>%filter(CCanton==101)

TNA_plot <- ggplot(Climate_Data)+
    geom_rect(aes(xmin=98,xmax=135,ymin=-Inf,ymax=Inf),fill="lightgray",alpha=0.1)+
    geom_line(aes(x=1:length(TNA),y=TNA))+
    theme_bw()+
    scale_x_continuous(name="Years",breaks=seq(1,length(Climate_Data$TNA),24), labels=seq(2001,2019,2))+
    theme(axis.text.x=element_text(angle=0,size=8))+
    labs(x="Time (Year)",y="TNA")


mypath <- file.path("Plots/TNA_trend.pdf")
pdf(mypath,width = 10, height = 6) 
print(TNA_plot)
dev.off()



###-----------------------------------------------
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
  annotate("text", x = 220, y =  2.3,  label = "El Nino", size=5)+
  annotate("text", x = 220, y = -2.3,  label = "La Nina", size=5)+
  annotate("text", x = 0.5, y = 0.5,   label = "Weak",       color="red",     size=5)+
  annotate("text", x = 4,   y = 1.25,  label = "Moderate",   color="red",     size=5)+
  annotate("text", x = 1,   y = 1.75,  label = "Strong",     color="red",     size=5)+
  annotate("text", x = 7,   y = 2.5,   label = "Very Strong",color="white",   size=5)+
  annotate("text", x = 0.5, y = -0.5,  label = "Weak",       color="darkblue",size=5)+
  annotate("text", x = 4,   y = -1.25, label = "Moderate",   color="darkblue",size=5)+
  annotate("text", x = 1,   y = -1.75, label = "Strong",     color="darkblue",size=5)+
  annotate("text", x = 7,   y = -2.5,  label = "Very Strong",color="white",   size=5)+
  theme_bw()+
  scale_x_continuous(name="Years",breaks=seq(1,length(Climate_Data$TNA),24), labels=seq(2001,2019,2))+
  theme(axis.text.x=element_text(angle=0,size=12),
        axis.text.y=element_text(angle=0,size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14))+
  labs(x="Time (Year)",y="Nino Region 3.4")
plot(NINO34_plot)

mypath <- file.path("Plots/Nino34_trend.pdf")
pdf(mypath,width = 10, height = 6) 
print(NINO34_plot)
dev.off()


###Aggregated cases --------------------------------------

Aggregated       <- datos_totales%>%group_by(Year,Month)%>%dplyr::summarise(Total=sum(Cases),Pop =max(PoblacionCR))
Aggregated$Month <- 1:nrow(Aggregated)
Aggregated       <- Aggregated%>%mutate(cases_x_10=10000*Total/Pop) 

Breaks = seq(1,228,12)
Years  = seq(2001,2019,1)


cases_over_time <- ggplot(Aggregated)+
                   geom_line(aes(x=Month, y=Total))+
                   theme_bw()+
                   scale_x_continuous(breaks = Breaks, labels = Years)+
                   labs(x="", y="Total cases")+
                   theme(axis.text = element_text(size=16),
                         axis.title = element_text(size=16))


mypath <- file.path("Plots2/dengue cases.pdf")
pdf(mypath,width = 15, height = 6) 
print(cases_over_time )
dev.off()


#