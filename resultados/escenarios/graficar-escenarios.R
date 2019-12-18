### PAQUETES ###
library(ggpubr)
library(ggplot2)

### DATOS ###
datosNormal <- read.csv("./datos-resultados/OMYK-modelo-experiment-casos-table.csv", skip = 6)
datosAumentarTormetas <- read.csv("./datos-resultados/OMYK-modelo-experiment-aumentoOcurrHuracanes-table.csv", skip = 6)
datosAumentarIncendios <- read.csv("./datos-resultados/OMYK-modelo-experiment-aumentoProbOcurIncendios-table.csv", skip = 6)
datosDisminuirTurismo <- read.csv("./datos-resultados/OMYK-modelo-experiment-modifProbFlujoAltoTuristas-table.csv", skip = 6)

datosAumentarTormetas <- datosAumentarTormetas[datosAumentarTormetas$multiplicarTormentas == 3, ]
datosAumentarIncendios <- datosAumentarIncendios[datosAumentarIncendios$multiplicarProbOcurIncendios == 3, ]
datosDisminuirTurismo <- datosDisminuirTurismo[datosDisminuirTurismo$probTuristasAltoBim == 0.3, ]

datosNormal$disturbio <- "normal"
datosAumentarTormetas$disturbio <- "increase storms" #"incremento de tormentas"
datosAumentarIncendios$disturbio <- "increase forest fires" #"incremento de incendios"
datosDisminuirTurismo$disturbio <-  "decrease tourism" #"disminución de turismo"

datosTotal <- rbind(datosNormal, datosAumentarTormetas, datosAumentarIncendios,datosDisminuirTurismo)

levels(datosTotal$milpaT.) <- c("-", "M")
levels(datosTotal$apiculturaT.) <- c("-", "A")
levels(datosTotal$carbonT.) <- c("-", "C")
levels(datosTotal$turismoT.) <- c("-", "T")

# Se genera una columna que describe las actividades que realiza el hogar
datosTotal$actividades <-paste(datosTotal$milpaT.,datosTotal$apiculturaT.,datosTotal$carbonT.,datosTotal$turismoT.)

## Se cambia el nombre de la columna de interés
colnames(datosTotal)[colnames(datosTotal)=="count.patches.with...edadSucesional...50.."] <- "selvaMayor50"
colnames(datosTotal)[colnames(datosTotal)=="count.monos"] <- "totalMonos"
colnames(datosTotal)[colnames(datosTotal)=="mean...mediaAnoSustento...of.hogares"] <- "mediaAnoSustento"

datosTotal$estrategia <-factor( ifelse(datosTotal$milpaT. == "M" & datosTotal$turismoT. == "-", "Tradicional",
                                ifelse(datosTotal$milpaT. == "M" & datosTotal$turismoT. == "T", "Mixta",
                                       ifelse(datosTotal$milpaT. == "-" & datosTotal$turismoT. == "T", "Orientada a Servicios",
                                              ifelse(datosTotal$milpaT. == "-" & datosTotal$turismoT. == "-", "Otra"," ")))),
                         levels = c("Otra", "Tradicional", "Mixta", "Orientada a Servicios"))

datosTotal$disturbio <- as.factor(datosTotal$disturbio)

#datosTotal$disturbio <- factor(datosTotal$disturbio, levels = c("normal","incremento de tormentas","incremento de incendios","disminución de turismo"))
datosTotal$disturbio <- factor(datosTotal$disturbio, levels = c("normal","increase storms","increase forest fires","decrease tourism"))

levels(datosTotal$estrategia) <- c("Other", "Traditional", "Mixed", "Service Oriented")

############################################
### Gráfica de actividades y estrategias ###

dat <- datosTotal[datosTotal$disturbio == "normal",]

ylabSelva <- "Area of\nmature forest (ha)" #"Área de selva madura (ha)"
ylabMonos <- "Number of monkeys" #"Número de monos"
ylabValMo <- "Average monetary value\n(daily minimum wages)" #"Valor monetario promedio\n(salarios mínimos / día)"

source("funcion-graficar-casos.R")

tamano.letra <- 9

# nombre_parametro <- "actividades"
# leyenda_parametro <-"Actividades"
# #colores <- c("#999999","#F0E442","#999999","#F0E442","#999999","#F0E442","#999999","#F0E442","#0072B2","#D55E00","#0072B2","#D55E00","#0072B2","#D55E00","#0072B2","#D55E00")
#
# ga <- ggarrange(
#   graficar(dat,"selvaMayor50",nombre_parametro,ylabSelva,leyenda_parametro,multiplicar = 3, tamano.texto = tamano.letra) +
#     guides(col = guide_legend(ncol = 2), fill = guide_legend(ncol = 2)) #+
#     #scale_color_manual(values = colores) +
#     #scale_fill_manual(values = colores)
#   ,
#   graficar(dat,"totalMonos",nombre_parametro,ylabMonos,leyenda_parametro, tamano.texto = tamano.letra ) +
#     guides(col = guide_legend(ncol = 2), fill = guide_legend(ncol = 2)) #+
#     #scale_color_manual(values = colores) +
#     #scale_fill_manual(values = colores)
#   ,
#   graficar(dat,"mediaAnoSustento",nombre_parametro,ylabValMo,leyenda_parametro,tamano.texto = tamano.letra ) +
#     guides(col = guide_legend(ncol = 2), fill = guide_legend(ncol = 2)) #+
#     #scale_color_manual(values = colores) +
#     #scale_fill_manual(values = colores)
#   ,
#   common.legend = TRUE,
#   legend = "right",
#   nrow = 3,
#   ncol = 1,
#   align = "v",
#   labels = c("A","B","C")
# )
#
# ggsave(
#   "./imgs/escenarios-actividades.png",
#   device = "png",
#   ga,
#   width = 23, height = 27, units = "cm", dpi = 300
# )

# nombre_parametro <- "estrategia"
# leyenda_parametro <- "Strategy" #"Estrategia"
#
e1 <- theme(plot.margin = margin(0.1,0.1,0.1,0.1,"cm"))
e5 <- guides(colour = guide_legend(title.position = "top"))
e6 <- list(scale_color_manual(values = c("#999999","#F0E442","#0072B2","#D55E00")), scale_fill_manual(values = c("#999999","#F0E442","#0072B2","#D55E00")) )
#
# ge <- ggarrange(
#   graficar(dat,"selvaMayor50",nombre_parametro,ylabSelva,leyenda_parametro,multiplicar = 3, tamano.texto = tamano.letra) + e1 + e5 + e6,
#   graficar(dat,"totalMonos",nombre_parametro,ylabMonos,leyenda_parametro, tamano.texto = tamano.letra ) + e1 + e5 + e6,
#   graficar(dat,"mediaAnoSustento",nombre_parametro,ylabValMo,leyenda_parametro,tamano.texto = tamano.letra )+ e1 + e5 + e6,
#   common.legend = TRUE,
#   legend = "bottom",
#   nrow = 3,
#   ncol = 1,
#   align = "v",
#   #labels = c("A","B","C")
#   #labels = c("(A)","(B)","(C)"),
#   font.label = list(size = tamano.letra)
# )
#
# ggsave(
#   #"./imgs/escenarios-estrategias.png",
#   "./imgs/escenarios-estrategias.eps",
#   #device = "png",
#   device = cairo_ps,
#   ge,
#   #width = 23, height = 27, units = "cm",
#   width = 85, height = 150, units = "mm",
#   dpi = 300
# )

##############################
### Gráfica de distrubrios ###

#dat <- datosTotal[datosTotal$disturbio != "normal",]
dat <- datosTotal

source("funcion-graficar-disturbios.R")

separado_por <- "disturbio"
nombre_parametro <- "estrategia"
leyenda_parametro <- "Strategy" #"Estrategia"

e2 <- theme(strip.background =  element_blank(), strip.text = element_blank() )
e3 <- theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
e4 <- labs(x = element_blank())

gd <- ggarrange(
  graficarD(dat,"selvaMayor50",nombre_parametro,ylabSelva,leyenda_parametro,multiplicar = 3, separar_por = separado_por, tamano.texto = tamano.letra ) + e1 + e3 + e4 + e6,
  graficarD(dat,"totalMonos",nombre_parametro,ylabMonos,leyenda_parametro,multiplicar = 1, separar_por = separado_por,tamano.texto = tamano.letra ) + e1 + e2 + e3 + e4 + e6,
  graficarD(dat,"mediaAnoSustento",nombre_parametro,ylabValMo,leyenda_parametro,multiplicar = 1, separar_por = separado_por, tamano.texto = tamano.letra ) + e1 + e2 + e6,
  common.legend = TRUE,
  legend = "bottom",
  nrow = 3,
  ncol = 1,
  align = "v",
  heights = c(1,0.8,1),
  #labels = c("A","B","C")
  #labels = c("(A)","(B)","(C)"),
  font.label = list(size = tamano.letra)
)

ggsave(
  "./imgs/disturbios.png",
  #"./imgs/disturbios.eps",
  device = "png",
  #device = cairo_ps,
  gd,
  #width = 23, height = 27, units = "cm",
  width = 180, height = 130, units = "mm",
  dpi = 300
)
