rm(list=ls())
library(DT)
library(rmarkdown)
library(rbokeh)
library(plotly)
library(lubridate)
library(tidyverse)

## Flujo de caja
system("rm /tmp/rSuperficies.csv")
system(" ledger -f ~/buildsNoBkup/covid19_mde/data_superficies_covid19.ldg  csv  not covid19  >> /tmp/rSuperficies.csv" )
superficies <- read.csv("/tmp/rSuperficies.csv", header = FALSE)


##=======CHARTS
## DURATION
superficies <- superficies[order(superficies$V6, decreasing = FALSE),]
superficies$V6 <- (superficies$V6/60)/60
duration <- plot_ly( x = superficies$V6, y = superficies$V4,  type ='bar', orientation = "h", color = I("darkOliveGreen") )%>%
    layout(yaxis = list(title = 'Superficies'),  plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"), xaxis = list(title="Duración en Horas" ))
Graph.Duration <- ggplotly(duration)

## ========= TABLES
Unique.Sources <- unique(superficies$V3)
