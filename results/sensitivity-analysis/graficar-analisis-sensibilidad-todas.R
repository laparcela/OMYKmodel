### PAQUETES ###
suppressMessages(library(ggpubr))
suppressMessages(library(ggplot2))

source('funcion-graficar.R')

lista_parametros <- list(
  #c("mediaDuracionBiomasComb","Media duración de\nbiomasa combustible\n(bimestres)"),
  c("mediaDuracionBiomasComb","Mean duration\nof fuel biomass\n(bimesters)"),
  #c("probQuemarse","Probabilidad de\ncuadro de quemarse"),
  c("probQuemarse","Burning\nprobability\nof patches"),
  #c("desviacionEstandarDuracionBiomasComb","Desviación estandar\nduración biomasa\ncombustible"),
  c("desviacionEstandarDuracionBiomasComb","Standard\ndeviation of\nduration of\nfuel biomass\n(bimesters)"),
  #c("numeroMinDeMonosParaTurismo","Numero mínimo\nde monos para\nturismo"),
  c("numeroMinDeMonosParaTurismo","Minimum\nnumber of\nmonkeys for\ntourism"),
  #c("parametroSigmoidea1","Parámetro función\nsigmoidea 1"),
  c("parametroSigmoidea1","Sigmoid\nfunction\nparameter 1"),
  #c("parametroSigmoidea2","Parámetro función\nsigmoidea 2"),
  c("parametroSigmoidea2","Sigmoid\nfunction\nparameter 2"),
  #c("probTuristasAltoBim","Probabilidad de flujo\nalto de turistas"),
  c("probTuristasAltoBim","Probability of\nhigh flow of\ntourists"),
  #c("tamanoCuadro","Tamaño de\ncuadro (ha)")
  c("tamanoCuadro","Patch size (ha)")
)

for (parametro in lista_parametros) {
  print(paste('Generando gráfica para: ',parametro[1],'...'))
  if (parametro[1] == "tamanoCuadro"){
    ## Cargar datos
    datos.1 <- read.csv("./datos-resultados/OMYK-modelo-experiment-sens-tamanoCuadro-1ha-table.csv", skip = 6)
    datos.3 <- read.csv("./datos-resultados/OMYK-modelo-experiment-sens-tamanoCuadro-3ha-table.csv", skip = 6)

    datos.1$tamanoCuadro <- 1
    datos.3$tamanoCuadro <- 3

    datos.1$count.patches.with...edadSucesional...50.. <- datos.1$count.patches.with...edadSucesional...50.. * 0.998 / 3  # se multiplica por 0.998 para convertirlo a ha
    datos.3$count.patches.with...edadSucesional...50.. <- datos.3$count.patches.with...edadSucesional...50.. * 3 / 3 # se multiplica por 3 para convertirlo a ha

    datos <- rbind(datos.1, datos.3)
  }
  if (parametro[1] != "tamanoCuadro"){
    datos <- read.csv(paste('./datos-resultados/OMYK-modelo-experiment-sens-',parametro[1],'-table.csv', sep = ''), skip = 6)
    if (parametro[1] == "probQuemarse") {
      datos <- datos[datos$probQuemarse != 0.56,]
    }
  }
  ## Se cambia el nombre de las columnas de interés
  colnames(datos)[colnames(datos)=="count.patches.with...edadSucesional...50.."] <- "selva.mayor.50"
  colnames(datos)[colnames(datos)=="count.monos"] <- "total.monos"
  colnames(datos)[colnames(datos)=="mean...mediaAnoSustento...of.hogares"] <- "media.ano.sustento"

  nombre_parametro <- parametro[1]
  leyenda_parametro <-parametro[2]

  tamano.letra <- 8.5

  e1 <- theme(plot.margin = margin(0.1,0.25,0.1,0.4,"cm"),axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))
  e2 <- guides(col = guide_legend(nrow = 2,title.vjust = 1), fill = guide_legend(nrow = 2,title.vjust = 1))

  #grafica.selva <- graficar(datos, "selva.mayor.50" , nombre_parametro, "Área de \nselva > 50 años (ha)", multiplicar = 3, leyenda_parametro, tamano.texto = tamano.letra) + e1
  grafica.selva <- graficar(datos, "selva.mayor.50" , nombre_parametro, "Mature forest area (ha)", multiplicar = 3, leyenda_parametro, tamano.texto = tamano.letra) + e1 #+ e2
  #grafica.monos <- graficar(datos, "total.monos" , nombre_parametro, "Número de monos", leyenda_parametro, tamano.texto = tamano.letra) + e1
  grafica.monos <- graficar(datos, "total.monos" , nombre_parametro, "Number of monkeys", leyenda_parametro, tamano.texto = tamano.letra) + e1 #+ e2
  #grafica.sustento <-graficar(datos, "media.ano.sustento" , nombre_parametro, "Valor monetario promedio\n (salarios mínimos / día)", leyenda_parametro, tamano.texto = tamano.letra) + e1
  grafica.sustento <-graficar(datos, "media.ano.sustento" , nombre_parametro, "Average monetary value\n(daily minimum wages)", leyenda_parametro, tamano.texto = tamano.letra) + e1 #+ e2

  graficas <-ggarrange(
    grafica.selva,
    grafica.monos,
    grafica.sustento,
    common.legend = TRUE,
    legend = "none",
    nrow = 3,
    align = "v" ,
    labels = c("A", "B", "C"),
    font.label = list(size = tamano.letra - 2)
  )
  leyenda <- cowplot::get_legend(grafica.selva)
  g <- ggarrange(
    graficas,
    leyenda,
    ncol = 2,
    widths = c(4,1)
  )
  ggsave(
    paste("imgs/sensibilidad-",nombre_parametro,".png", sep = ""),
    device = "png",
    g,
    width = 85,
    height = 120,
    units = "mm",
    dpi = 300)
  ggsave(
    paste("imgs/sensibilidad-",nombre_parametro,".eps", sep = ""),
    device = cairo_ps,
    g,
    width = 85,
    height = 120,
    units = "mm",
    dpi = 300)

    ggsave(
      paste("imgs/sensibilidad-",nombre_parametro,".jpg", sep = ""),
      device = 'jpeg',
      g,
      width = 85,
      height = 120,
      units = "mm",
      dpi = 300)

  print(paste('Gráfica para: ',nombre_parametro,'generada.'))
}


print('Fin :)')
