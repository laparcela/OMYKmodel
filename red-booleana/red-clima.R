### PAQUETES ####
library(BoolNet)
library(ggplot2)
library(ggpubr)

## cargar datos y función graficadora
redClima <- loadNetwork('otoch-red-clima.txt')
source('funcion-imagen-atractor.R')

## graficar
attr <- getAttractors(redClima)
attr.sec.1 <- getAttractorSequence(attr, 1)
attr.sec.2 <- getAttractorSequence(attr, 2)

names(attr.sec.1)[names(attr.sec.1) == "presion"] <- "presión"
names(attr.sec.1)[names(attr.sec.1) == "precipitacion"] <- "precipitación"
names(attr.sec.1)[names(attr.sec.1) == "produccionCarbon"] <- "producciónCarbón"
names(attr.sec.2)[names(attr.sec.2) == "presion"] <- "presión"
names(attr.sec.2)[names(attr.sec.2) == "precipitacion"] <- "precipitación"
names(attr.sec.2)[names(attr.sec.2) == "produccionCarbon"] <- "producciónCarbón"

figura <- ggarrange(
  imagenAtractor(attr.sec.1, 0, 16),
  imagenAtractor(attr.sec.2, 0, 16),
  common.legend = TRUE,
  nrow = 2,
  legend = "right",
  labels = c("A","B")
)

ggsave("./imgs/img-atractores-clima.png",
  device = "png",
  figura,
  width = 12,
  height = 9,
  units = "cm"
)
