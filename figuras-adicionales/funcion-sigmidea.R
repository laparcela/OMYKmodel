### PAQUETES ###
library(ggplot2)

## Función
sigfun = function(x,y,z) {
  100*(1 /(1 + exp(y*(-x + z))))
}

fin <- 200

z <- as.data.frame( seq(0,fin,by=1))

y1 <- 0.05
y2 <- 0.045
y3 <- 0.04
y4 <- 0.035
y5 <- 0.03
y6 <- 0.025
y7 <- 0.02
z1 <- 74
z2 <- 96
z3 <- 111
z4 <- 130
z5 <- 157
nt <- 10
tt <- 20
fin <- 200

s <- 0.8

funcionEjemplos <- ggplot(z) +
  stat_function(aes(0:fin), fun = sigfun, args = c(y=y1,z=z2), color = "#E0E0E0", size = s) +
  stat_function(aes(0:fin), fun = sigfun, args = c(y=y2,z=z2), color = "#E0E0E0", size = s) +
  stat_function(aes(0:fin), fun = sigfun, args = c(y=y3,z=z2), color = "#E0E0E0", size = s) +
  stat_function(aes(0:fin), fun = sigfun, args = c(y=y5,z=z2), color = "#E0E0E0", size = s) +
  stat_function(aes(0:fin), fun = sigfun, args = c(y=y6,z=z2), color = "#E0E0E0", size = s) +
  stat_function(aes(0:fin), fun = sigfun, args = c(y=y4,z=z1), color = "pink", size = s) +
  stat_function(aes(0:fin), fun = sigfun, args = c(y=y7,z=z2), color = "#E0E0E0", size = s) +
  stat_function(aes(0:fin), fun = sigfun, args = c(y=y4,z=z3), color = "pink", size = s) +
  stat_function(aes(0:fin), fun = sigfun, args = c(y=y4,z=z2), color = "red", size = 1) +
  labs(x="Velocidad máxima del viento (mph)", y="% de cuadros afectados") +
  theme_bw() +
  theme(text = element_text(size=16))

  ggsave("./imgs/img-funcion-sigmoidea.png",
    device = "png",
    funcionEjemplos,
    width = 14,
    height = 10,
    units = "cm"
    )
