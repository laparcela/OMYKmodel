suppressMessages(library(ggplot2))
suppressMessages(library(ggpubr))
suppressMessages(library(plyr))
suppressMessages(library(plotrix))
suppressMessages(library(xtable))

# Datos de selva observados en 2015 (Rangel-Rivera, 2017)
observado.selva.50.2015 <- 149.4
observado.selva.30.50.2015 <- 3050.4
observado.selva.16.29.2015 <- 977.8
observado.selva.2.15.2015 <- 983.6

estimado.monos.2015.con.2015 <- 132.38

# Se leen los datos
datos <- read.csv("./datos-resultados/OMYK-modelo-experiment-validacion-table.csv", skip = 6)

######################################################
############# SE PREPARAN LOS DATOS ##################
colnames(datos)[colnames(datos)=="count.patches.with...edadSucesional...50.."] <- "sMayor50"
datos$'sMayor50' <- datos$'sMayor50' * 3

colnames(datos)[colnames(datos)=="count.patches.with...edadSucesional....30.and.edadSucesional....50.."] <- "s30a50"
datos$'s30a50' <- datos$'s30a50' * 3

colnames(datos)[colnames(datos)=="count.patches.with...edadSucesional....16.and.edadSucesional....29.."] <- "s16a29"
datos$'s16a29' <- datos$'s16a29' * 3

colnames(datos)[colnames(datos)=="count.patches.with...edadSucesional....2..and.edadSucesional....15.."] <- "s2a15"
datos$'s2a15' <- datos$'s2a15' * 3

colnames(datos)[colnames(datos)=="count.monos"] <- "monos"

######################################################
######### FUNCIÓN GRAFICADORA Y EXPORTADORA ##########
graficarCalibracionUnaCombParam <- function(datos, probOcurInc, probQuemarse, paramS1, paramS2, columnasAGraficar, datosObservados, textoColunmas){
  # Se seleccionan solo los datos con los parámetros deceados
  datos.filtrados <- datos[datos$probOcurrenciaIncendioBimSecas == probOcurInc &
                             datos$probQuemarse == probQuemarse &
                             datos$parametroSigmoidea1 == paramS1 &
                             datos$parametroSigmoidea2 == paramS2,]

  # las gráficas se guardarán en estas dos listas
  graficas.individuales <- list()
  contador <- 1
  # Se recorren las columnas a graficar
  for (elementoColumnasAGraficar in columnasAGraficar) {
    grafica <- ggplot(datos.filtrados, aes_string(x = NULL, y = elementoColumnasAGraficar )) +
      geom_boxplot() +
      xlab(textoColunmas[contador]) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank()) +
      theme(text = element_text(size = 16))
    if (elementoColumnasAGraficar != "monos") {
      grafica <- grafica +
        #geom_hline((aes_string(yintercept = datosObservados[contador] )), colour="#00BA38", size = 1.2) +
        geom_point(shape = 23, size = 3, fill = "white", (aes_string(x = 0, y = datosObservados[contador] )) ) +
        #ylab("Área (ha)")
        ylab("Area (ha)")
    }
    if (elementoColumnasAGraficar == "monos") {
      grafica <- grafica +
        #geom_hline((aes_string(yintercept = datosObservados[contador] )), colour="#F8766D", size = 1.2) +
        #geom_hline((aes_string(yintercept = datosObservados[contador + 1] )), colour="#00BFC4", size = 1.2) +
        geom_point(shape = 23, size = 3, fill = "white", (aes_string(x = 0, y = datosObservados[contador] )) ) +
        #ylab("Número de monos")
        ylab("Number of monkeys")
    }
    grafica <- grafica + theme(text = element_text(size=10)) + theme(plot.margin = margin(0.2,0.2,0.2,0.5,"cm"))
    graficas.individuales[[contador]]<- grafica
    contador <- contador + 1
    graficas.individuales[contador]
  }
  ggarrange(plotlist = graficas.individuales ,
            legend = "left",
            ncol = 5,
            nrow = 1,
            labels = c("A","B","C","D","E"),
            align = "h",
            font.label = list(size = 8)
          )
}

###########################################
# Se grafica

columnasParaGraficar <- c("s2a15", "s16a29", "s30a50", "sMayor50", "monos")
observados.2015 <- c(observado.selva.2.15.2015,observado.selva.16.29.2015,observado.selva.30.50.2015,observado.selva.50.2015, estimado.monos.2015.con.2015)
#texto.columnas <- c("Selva\n2-7 años","Selva\n16-29 años","Selva\n30-50 años","Selva\n>50 años","Número\nde monos")
texto.columnas <- c("Forest\n2-15 years","Forest\n16-29 years","Forest\n30-50 years","Forest\n>50 years","Monkeys")

g <- graficarCalibracionUnaCombParam(datos, 0.093, 0.5, 0.025, 74, columnasParaGraficar, observados.2015, texto.columnas)

graficaAExportar <- g
nombreGrafica <- "prueba-validacion"
ggsave(paste("./imgs/",nombreGrafica,".png", sep = ""), device = "png", graficaAExportar,  width = 180, height = 60, units = "mm", dpi = 300)
ggsave(paste("./imgs/",nombreGrafica,".jpg", sep = ""), device = "jpg", graficaAExportar,  width = 180, height = 60, units = "mm", dpi = 300)
ggsave(paste("./imgs/",nombreGrafica,".eps", sep = ""), device = cairo_ps, graficaAExportar,  width = 180, height = 60, units = "mm", dpi = 300)

###########################################

## TABLA RESUMEN ##
resumen <- data.frame(
  observacion = c("s2a15","s16a29","s30a50","sMayor50","monos"),
  observados2003 = round(c(686.6,1007.8,3088.8,212.3,152),digits = 2),
  observados2015 = round(observados.2015,digits = 2),
  media = round(c(mean(datos$s2a15),mean(datos$s16a29),mean(datos$s30a50),mean(datos$sMayor50),mean(datos$monos)), digits = 2),
  se = round(c(std.error(datos$s2a15),std.error(datos$s16a29),std.error(datos$s30a50),std.error(datos$sMayor50),std.error(datos$monos)), digits = 2),
  mediana = round(c(median(datos$s2a15),median(datos$s16a29),median(datos$s30a50),median(datos$sMayor50),median(datos$monos)), digits = 2)
)

print(xtable(resumen, type = "latex"), file = "./imgs/resumen.tex")
write.table(resumen, file = "./imgs/resumen.txt", sep = ",", quote = FALSE, row.names = T)
