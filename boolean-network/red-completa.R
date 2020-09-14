### PAQUETES ####
library(BoolNet)
library(ggplot2)

## cargar datos y función graficadora
redOMYK <- loadNetwork('otoch-red-completa.txt')
source('funcion-imagen-atractor.R')

## graficar
attr <- getAttractors(redOMYK)
attr.toda.1 <- getAttractorSequence(attr, 2)

figura <- imagenAtractor(attr.toda.1, 0, 9)

ggsave(
  "./imgs/img-atractores-total.eps",
  #"./imgs/img-atractores-total.png",
  device = "eps",
  #device = "png",
  figura,
  width = 85,
  height = 90,
  units = "mm",
  dpi = 300
)

ggsave(
  #"./imgs/img-atractores-total.eps",
  "./imgs/img-atractores-total.jpg",
  #device = "eps",
  device = "jpg",
  figura,
  width = 85,
  height = 90,
  units = "mm",
  dpi = 300
)
