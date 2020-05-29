### PAQUETES ####
library(ggplot2)
library(plyr)
library(tidyr)
library(ggpubr)

### DATOS ###
### datos de https://smn.cna.gob.mx/es/informacion-climatologica-por-estado?estado=qroo (Cobá>Normales 1951-2010 o https://smn.conagua.gob.mx/tools/RESOURCES/Normales5110/NORMAL23012.TXT)
precipitacion <- data.frame(
  mesn = c(1:12),
  mes = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic"),
  precip.normal= c(52.7,38.2,40.4,59.9,97.6,138.2,106.5,153.2,194.4,143.3,66.4,36.6)
)

## dato de https://www.datatur.sectur.gob.mx/ITxEF/ITxEF_QROO.aspx (Estadísticas>Información Turística por entidad Federativa>Quintana Roo>Desgargar Excel 6.2 o https://www.datatur.sectur.gob.mx/ITxEF_Docs/QROO_COMPENDIO_6_2.zip)
ocupacion.hotelera <- data.frame(
  a2017 = c(82.36,84.54,83.13,83.72,80.66,81.81,86.76,77.95,62.96,62.99,75.50,76.35),
  a2016 = c(79.84,82.04,82.73,80.76,77.44,79.12,85.87,77.93,64.30,67.33,78.02,79.73),
  a2015 = c(82.33,85.35,82.31,82.49,78.42,77.59,84.75,78.03,61.55,64.91,76.00,76.52),
  a2014 = c(79.90,84.15,80.45,77.92,74.81,72.49,81.52,75.05,57.56,63.68,76.43,77.66),
  a2013 = c(75.73,79.41,80.99,74.65,67.49,69.99,81.22,71.27,54.44,55.20,68.03,75.69),
  a2012 = c(74.46,79.31,79.47,77.09,66.00,66.24,77.25,68.86,53.06,54.03,67.73,73.92),
  a2011 = c(66.23,73.30,75.28,69.27,58.42,59.48,72.67,63.11,46.24,47.17,58.82,66.73),
  a2010 = c(63.84,73.49,74.64,67.91,57.15,59.41,67.91,59.23,41.62,42.80,56.18,65.10),
  a2009 = c(72.88,79.11,74.88,70.86,26.45,44.33,59.78,54.67,36.35,39.31,52.61,60.10),
  a2008 = c(74.97,82.50,83.51,75.90,68.65,71.54,77.76,68.87,47.55,50.95,63.21,69.64),
  a2007 = c(77.05,82.20,80.49,79.17,67.97,71.81,80.15,63.39,46.97,51.38,65.09,68.12),
  a2006 = c(77.36,82.26,82.74,76.62,71.84,75.00,78.43,72.21,49.53,52.88,67.37,71.09),
  a2005 = c(84.47,91.54,90.85,83.32,78.47,84.75,81.32,80.17,59.30,52.18,32.74,56.93),
  a2004 = c(75.88,87.56,84.49,82.82,73.78,81.80,90.48,83.94,60.90,62.77,72.96,73.66),
  a2003 = c(64.91,74.63,76.15,71.99,68.16,77.44,86.86,82.12,59.73,57.25,69.11,69.17),
  a2002 = c(60.89,67.92,81.27,69.55,64.48,74.75,81.96,74.01,48.32,46.37,55.74,60.10),
  a2001 = c(72.96,80.87,85.02,78.24,75.93,80.97,86.93,78.83,53.74,51.44,55.82,55.09),
  a2000 = c(61.48,80.00,86.53,75.62,73.52,83.05,86.85,78.80,56.50,56.80,63.24,60.17),
  a1999 = c(63.43,71.06,74.28,67.72,62.55,67.96,70.85,79.82,53.66,49.86,53.38,59.80),
  a1998 = c(68.77,77.27,74.35,74.61,70.01,71.15,73.05,73.19,58.19,55.41,61.17,60.12),
  a1997 = c(78.97,87.15,85.03,78.39,75.74,78.90,82.73,80.45,61.85,64.29,71.21,70.33),
  a1996 = c(82.42,87.70,85.96,79.14,71.55,74.28,79.30,76.86,55.74,58.60,69.66,70.68),
  a1995 = c(81.67,86.33,84.68,73.31,64.35,69.52,78.12,78.22,61.32,59.12,68.09,66.62),
  a1994 = c(73.81,77.72,79.99,68.68,59.69,58.61,74.69,74.40,55.31,56.80,60.55,64.09),
  a1993 = c(78.89,81.70,78.30,72.26,61.75,61.15,76.69,78.66,53.46,56.48,61.09,60.61),
  a1992 = c(74.38,78.47,77.98,67.74,60.95,60.02,75.82,82.32,54.28,60.53,67.13,67.32),
  mesn = c(1:12),
  mes = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")
)

ocupacion.hotelera.ano <- ocupacion.hotelera %>% gather(ano, ocupacion, a1992:a2017)
resumen.ocupacion.hotelera <- ddply(
  ocupacion.hotelera.ano,
  .(mesn),
  summarise,
  media = mean(ocupacion),
  sd = sd(ocupacion),
  se = sd(ocupacion) / sqrt(length(ocupacion)),
  maxi = max(ocupacion),
  mini = min(ocupacion)
)


### GRAFICAS ###
figura.ocupacion.hotelera <- ggplot(resumen.ocupacion.hotelera, aes(x = mesn,y = media)) +
  geom_line(size = 0.7) +
  geom_errorbar(aes(ymin = (media - se), ymax = (media + se)), width = .1, size = 0.7) +
  geom_ribbon(aes(ymin = mini, ymax = maxi), alpha = 0.15, colour = 0) +
  scale_x_continuous(breaks = c(1:12), labels = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")) +
  ylab("Ocupación hotelera (%)") +
  xlab("Mes") +
  geom_hline(yintercept=c(75), linetype="dotted", color="#999999") +
  geom_vline(xintercept=c(2.5,4.5,6.5,8.5,10.5), linetype="dotted", color="#999999") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_rect(xmin=2.5, xmax=4.5, ymin=75, ymax=202, fill="#00BFC4", alpha=0.05) +
  geom_rect(xmin=0.4, xmax=2.5, ymin=75, ymax=202, fill="#F8766D", alpha=0.05) +
  geom_rect(xmin=4.4, xmax=6.5, ymin=75, ymax=202, fill="#F8766D", alpha=0.05) +
  geom_rect(xmin=6.5, xmax=8.5, ymin=75, ymax=202, fill="#00BFC4", alpha=0.05) +
  geom_rect(xmin=8.5, xmax=12.6, ymin=75, ymax=202, fill="#F8766D", alpha=0.05) +
  geom_rect(xmin=0.4, xmax=12.6, ymin=0, ymax=75, fill="#00BFC4", alpha=0.05) +
  theme(text = element_text(size=16))

ggsave("./imgs/img-ocupacion-hotelera.png",
  device = "png",
  figura.ocupacion.hotelera,
  width = 14,
  height = 12,
  units = "cm"
)

###

figura.precipitacion <- ggplot(precipitacion, aes(x = factor(mesn))) +
  geom_line(aes(y = precip.normal, group = 1), size = 0.7, stat="identity") +
  scale_x_discrete(breaks = factor(1:12),  drop = FALSE, labels = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")) +
  ylab("Precipitación promedio mensual (mm)") +
  xlab("Mes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_hline(yintercept = c(75), linetype="dotted", color="#999999") +
  geom_vline(xintercept=c(2.5,4.5,6.5,8.5,10.5), linetype="dotted", color="#999999") +
  geom_rect(xmin=4.5, xmax=10.5, ymin=0, ymax=202, fill="#00BFC4", alpha=0.05) +
  geom_rect(xmin=0.4, xmax=4.5, ymin=0, ymax=202, fill="#F8766D", alpha=0.05) +
  geom_rect(xmin=10.5, xmax=12.6, ymin=0, ymax=202, fill="#F8766D", alpha=0.05) +
  theme(text = element_text(size = 16))

ggsave("./imgs/img-precipitacion.png",
  device = "png",
  figura.precipitacion,
  width = 14,
  height = 12,
  units = "cm"
)

###

cul <- "#555555"
e1 <- theme(plot.margin = margin(0.05,0.05,0.05,0.8,"cm"))
e2 <- theme(strip.background =  element_blank(), strip.text = element_blank() )
e3 <- theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
e4 <- labs(x = element_blank())

## datos de García-Frapolli (2006) (Chapter 5)
figura.actividades <- ggarrange(
  ggplot(precipitacion, aes(x = factor(mesn))) +
  geom_rect(xmin = 0.4, xmax = 2.5, ymin = 0, ymax = 2, fill = "#F8766D", alpha = 0.05) +
  geom_rect(xmin = 2.5, xmax = 12.6, ymin = 0, ymax = 2, fill = "#00BFC4", alpha = 0.05) +
  annotate(geom = "text", x = 4, y = 1, label = "Quema", color = cul, angle = 90) +
  annotate(geom = "text", x = 6, y = 1, label = "Siembra", color = cul, angle = 90) +
  annotate(geom = "text", x = 11, y = 1, label = "Cosecha Maíz", color = cul, angle = 90) +
  annotate(geom = "text", x = 1, y = 1, label = "Cosecha Frijol", color = cul, angle = 90) +
  annotate(geom = "text", x = 2, y = 1, label = "Cosecha Frijol", color = cul, angle = 90) +
  annotate(geom = "text", x = 3, y = 1, label = "Cosecha Calabaza", color = cul, angle = 90) +
  geom_vline(xintercept=c(2.5,4.5,6.5,8.5,10.5), linetype="dotted", color="#999999") +
  labs(x = "Mes", y = "Agricultura") +
  scale_x_discrete(breaks = factor(1:12),  drop=FALSE, labels = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=16)) +
  e1 + e2 + e3 + e4,

  ggplot(precipitacion, aes(x = factor(mesn))) +
  geom_rect(xmin = 0.4, xmax = 6.5, ymin = 0, ymax = 2, fill = "#00BFC4", alpha = 0.05) +
  geom_rect(xmin = 6.5, xmax = 12.6, ymin = 0, ymax = 2, fill = "#F8766D", alpha = 0.05) +
  annotate(geom = "text", x = 1, y = 1, label = "Cosecha Apicultura", color = cul, angle = 90) +
  annotate(geom = "text", x = 2, y = 1, label = "Cosecha Apicultura", color = cul, angle = 90) +
  annotate(geom = "text", x = 3, y = 1, label = "Cosecha Apicultura", color = cul, angle = 90) +
  annotate(geom = "text", x = 4, y = 1, label = "Cosecha Apicultura", color = cul, angle = 90) +
  annotate(geom = "text", x = 5, y = 1, label = "Cosecha Apicultura", color = cul, angle = 90) +
  annotate(geom = "text", x = 10, y = 1, label = "Cosecha Apicultura", color = cul, angle = 90) +
  annotate(geom = "text", x = 11, y = 1, label = "Cosecha Apicultura", color = cul, angle = 90) +
  annotate(geom = "text", x = 12, y = 1, label = "Cosecha Apicultura", color = cul, angle = 90) +
  geom_vline(xintercept=c(2.5,4.5,6.5,8.5,10.5), linetype="dotted", color="#999999") +
  labs(x = "Mes", y = "Apicultura") +
  scale_x_discrete(breaks = factor(1:12),  drop=FALSE, labels = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=16)) +
  e1 + e2 + e3 + e4,

  ggplot(precipitacion, aes(x = factor(mesn))) +
  geom_rect(xmin = 0.4, xmax = 2.5, ymin = 0, ymax = 2, fill = "#F8766D", alpha = 0.05) +
  geom_rect(xmin = 2.5, xmax = 12.6, ymin = 0, ymax = 2, fill = "#00BFC4", alpha = 0.05) +
  annotate(geom = "text", x = 12, y = 1, label = "Producción Carbón", color = cul, angle = 90) +
  annotate(geom = "text", x = 11, y = 1, label = "Producción Carbón", color = cul, angle = 90) +
  annotate(geom = "text", x = 10, y = 1, label = "Producción Carbón", color = cul, angle = 90) +
  annotate(geom = "text", x = 9, y = 1, label = "Producción Carbón", color = cul, angle = 90) +
  annotate(geom = "text", x = 8, y = 1, label = "Producción Carbón", color = cul, angle = 90) +
  annotate(geom = "text", x = 7, y = 1, label = "Producción Carbón", color = cul, angle = 90) +
  annotate(geom = "text", x = 6, y = 1, label = "Producción Carbón", color = cul, angle = 90) +
  annotate(geom = "text", x = 5, y = 1, label = "Producción Carbón", color = cul, angle = 90) +
  annotate(geom = "text", x = 4, y = 1, label = "Producción Carbón", color = cul, angle = 90) +
  annotate(geom = "text", x = 1, y = 1, label = "Producción Carbón", color = cul, angle = 90) +
  geom_vline(xintercept=c(2.5,4.5,6.5,8.5,10.5), linetype="dotted", color="#999999") +
  labs(x = "Mes", y = "Producción\nCarbón") +
  scale_x_discrete(breaks = factor(1:12),  drop=FALSE, labels = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=16)) +
  e1,

  heights = c(1,1,1.3),
  nrow = 3,
  align = "v",
  labels = c("A","B","C")
)

ggsave("./imgs/img-actividades.png",
  device = "png",
  figura.actividades,
  width = 15,
  height = 13,
  units = "cm"
  )
