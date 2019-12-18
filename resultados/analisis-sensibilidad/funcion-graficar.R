## esta función grafica una variable de salida a lo largo del tiempo evaluada en múltiples parámetros

graficar <- function(tabla_datos, variable_salida, parametro, texto_y, texto_cuadro, multiplicar = 1, tamano.texto=12) {
  ############# PAQUETES #############
  suppressMessages(library(plyr)) ## paquete para usar ddply que genera una tabla resumida
  suppressMessages(library(ggplot2))
  suppressMessages(library(dplyr)) ## paquete para usar función select() y seleccionar columnas de un data.frame
  suppressMessages(library(extrafont))

  ## se crea una nuevo data.frame con las columnas de interés
  tabla_datos.variable_salida <- select(tabla_datos, X.step., X.run.number., parametro, variable_salida)

  ## se extraen solo los datos divisibles entre 6
  tabla_datos.variable_salida.6 <- tabla_datos.variable_salida[tabla_datos.variable_salida$X.step.%%6==0,]
  tabla_datos.variable_salida.6$ano <- tabla_datos.variable_salida.6$X.step. / 6

  ## se renombran las columnas para poder trabajar con ellas (si no se renombran no funciona)
  colnames(tabla_datos.variable_salida.6)[colnames(tabla_datos.variable_salida.6)==variable_salida] <- "variable_salida_n"
  colnames(tabla_datos.variable_salida.6)[colnames(tabla_datos.variable_salida.6)==parametro] <- "parametro_n"

  ## se genera una tabla que resume los datos agrupando los datos del mismo tiempo y del mismo tratamiento
  resumen.tabla_datos.variable_salida.6 <- ddply(tabla_datos.variable_salida.6, .(parametro_n, ano ), summarise,
                                                 mediana = median(variable_salida_n),
                                                 media = mean(variable_salida_n),
                                                 se = sd(variable_salida_n) / sqrt(length(variable_salida_n)),
                                                 lq =quantile(variable_salida_n , probs = (0.05) ),
                                                 uq =quantile(variable_salida_n , probs = (0.95) ))

  ## se convierte el tratamiento en un factor
  resumen.tabla_datos.variable_salida.6$parametro_n <- as.factor(resumen.tabla_datos.variable_salida.6$parametro_n)

  ################ VISUALIZACIÓN ########################
  ggplot(resumen.tabla_datos.variable_salida.6, aes(x=ano, y= multiplicar * media, colour = parametro_n, fill = parametro_n  )) +
    geom_ribbon(aes(ymin = multiplicar * lq, ymax = multiplicar * uq), alpha = 0.1, colour = 0) +
    geom_line(size = 0.7) +
    #xlab("Tiempo (años)") +
    xlab("years") +
    ylab(texto_y) +
    #guides(col = guide_legend(ncol = 2), fill = guide_legend(ncol = 2)) +
    labs(fill = texto_cuadro, colour = texto_cuadro) +
    scale_x_continuous(expand = c(0,0)) +
    #theme_classic2() +
    theme(text = element_text(size=tamano.texto))#, family = "ArchivoNarrow")) # La presentación usa esta fuente
  #######################################################
}
