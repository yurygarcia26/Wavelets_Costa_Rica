library(tidyverse)
library(WaveletComp)
library(biwavelet)
library(vectorwavelet)
library(ape)
library(caret)

load('./Data/datos_totales.RData')

correlation_matrix <- cor(datos_totales[ ,c("Cases","Nino34SSTA","TNA","NDVI","NDWI","ET","Precip_t","LSD","LSN")], use = "pairwise.complete.obs")

# Create a data frame for the heatmap
heatmap_data <- as.data.frame(as.table(correlation_matrix))
colnames(heatmap_data) <- c("Row", "Column", "Correlation")

# Create the heatmap using ggplot2
ggplot(heatmap_data, aes(x = Column, y = Row, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0, na.value = "grey50") +
    labs(title = "Correlation Heatmap") +
    theme_minimal() +
    theme(axis.text.x   = element_text(angle = 90, hjust = 1)) +
    geom_text(aes(label = round(Correlation, 1)), color = "black", size = 2)+
    labs(x="", y="")
  
