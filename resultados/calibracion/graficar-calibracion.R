### PAQUETES ###
library(ggplot2)
library(plyr)
library(xtable)

### DATOS ###
# Datos de selva quemada observados en 2015 (Rangel-Rivera, 2017)
quemado.observado.2003.a.2015 <- 667.2

datos <- read.csv('datos-resultados/OMYK-modelo-experiment-calibracion-table.csv', skip = 6)
datos$probQuemarse <- as.factor(datos$probQuemarse)

# % de cuadros afectados cuando no ocurre ningún evento
texto <- data.frame(parametroSigmoidea1 = rep(as.numeric(levels(as.factor(datos$parametroSigmoidea1))), length(levels(as.factor(datos$parametroSigmoidea2)))),
                    parametroSigmoidea2 = rep(as.numeric(levels(as.factor(datos$parametroSigmoidea2))), each = length(levels(as.factor(datos$parametroSigmoidea1)))))
texto$valor <-  round (100 * (1 /( 1 + exp(texto$parametroSigmoidea1 * (- 10 + texto$parametroSigmoidea2 )))) , digits = 3)

# Poner un asterisco en la combinación elegida
ann_texto <- data.frame(parametroSigmoidea1 = 0.025,
                        parametroSigmoidea2 = 74,
                        mediaDuracionBiomasComb = 12,
                        probQuemarse = as.factor(0.5),
                        totalAreaQuemada = 10000)
# Poner una flecha en la combinación elegida
ann_flecha <- data.frame(parametroSigmoidea1 = 0.025,
                        parametroSigmoidea2 = 74,
                        mediaDuracionBiomasComb = 12,
                        probQuemarse = as.factor(0.5),
                        x = 2,
                        ymax = 12500,
                        ymin = 7500)

### SE GRAFICA ###
grafica <- ggplot(datos, aes(x=probQuemarse, y=totalAreaQuemada)) +
  facet_grid(mediaDuracionBiomasComb ~ parametroSigmoidea1 + parametroSigmoidea2 ) +
  geom_boxplot() +
  geom_hline((aes(yintercept = quemado.observado.2003.a.2015 )), colour="#CC6666", size = 0.7) +
  #ylab("Área quemada (ha)") +
  ylab("Burned area (ha)") +
  #xlab("Probabilidad de cuadro de quemarse") +
  xlab("Patch burning probability") +
  theme(plot.subtitle = element_text(size = 9), axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5)) +
  theme(text = element_text(size = 12)) +
  theme(panel.grid.minor = element_blank()) +
  #geom_text(data = texto, mapping = aes(x=0.5+length(levels(as.factor(datos$probQuemarse)))/2, y=Inf, label = valor), size = 3, vjust = 1.5, colour = "gray50") +
  #geom_text(data = ann_texto,label="*", size = 15, colour="#CC6666") +
  geom_segment( data=ann_flecha, aes(x=x,xend=x,y=ymax,yend=ymin),arrow = arrow(length=unit(0.2,"cm")), colour="#CC6666", size=1.2)

ggsave("./imgs/img-calibracion.png",
  device = "png",
  grafica,
  width = 360,
  height = 150,
  units = "mm"
  )

ggsave("./imgs/img-calibracion.eps",
  device = cairo_ps,
  grafica,
  width = 360,
  height = 150,
  units = "mm",
  dpi = 300
  )

### SE GENERA Y EXPORTA TABLA CON MEJORES COMBINACIONES DE PARÁMETROS ###
resumen <- ddply(datos, .(parametroSigmoidea1, parametroSigmoidea2, probQuemarse,mediaDuracionBiomasComb), summarise,
                                               mediana = median(totalAreaQuemada),
                                               q1 = quantile(totalAreaQuemada, c(0.25), type = 7),
                                               q3 = quantile(totalAreaQuemada, c(0.75), type = 7),
                                               rangoInter = q3 -q1)
resumen$probQuemarse <- as.numeric(levels(resumen$probQuemarse))[resumen$probQuemarse]
resumen$porcentajeAfectadosNoTormenta <- round (100 * (1 /( 1 + exp(resumen$parametroSigmoidea1 * (- 10 + resumen$parametroSigmoidea2 )))) , digits = 3)
dentros <- resumen[resumen$q1 <= quemado.observado.2003.a.2015 & resumen$q3 >= quemado.observado.2003.a.2015,]
dentros <- dentros[order(dentros$rangoInter),]

print(xtable(dentros, digits = c(1,3,0,2,0,1,2,2,2,2), type = "latex"), file = "./imgs/dentros.tex")
write.table(dentros, file = "./imgs/dentros.txt", sep = ",", quote = FALSE, row.names = T)
