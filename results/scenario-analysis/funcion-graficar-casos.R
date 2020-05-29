## esta función grafica una variable de salida a lo largo del tiempo evaluada en múltiples parámetros

graficar <- function(tabla_datos, variable_salida, parametro, texto_y, texto_cuadro, multiplicar = 1, tamano.texto=12) {
  ############# PAQUETES #############
  library(plyr) ## paquete para usar ddply que genera una tabla resumida
  library(ggplot2)
  library(dplyr) ## paquete para usar función select() y seleccionar columnas de un data.frame
  library(scales)
  library(xtable)

  ## se crea una nuevo data.frame con las columnas de interés

  tabla_datos.variable_salida <- select(tabla_datos, X.step., X.run.number., parametro, variable_salida)

  ## se extraen solo los datos divisibles entre 6
  tabla_datos.variable_salida.6 <- tabla_datos.variable_salida[tabla_datos.variable_salida$X.step.%%6==0,]
  tabla_datos.variable_salida.6$ano <- tabla_datos.variable_salida.6$X.step. / 6

  ## se renombran las columnas para poder trabajar con ellas, no sé por qué pero si no se renombran no funciona
  colnames(tabla_datos.variable_salida.6)[colnames(tabla_datos.variable_salida.6)==variable_salida] <- "variable_salida_n"
  colnames(tabla_datos.variable_salida.6)[colnames(tabla_datos.variable_salida.6)==parametro] <- "parametro_n"

  ## se genera una tabla que resume los datos agrupando los datos del mismo tiempo y del mismo tratamiento
  resumen.tabla_datos.variable_salida.6 <- ddply(tabla_datos.variable_salida.6, .(parametro_n, ano), summarise,
                                                 mediana = median(variable_salida_n),
                                                 media = mean(variable_salida_n),
                                                 sd = sd(variable_salida_n),
                                                 vari = var(variable_salida_n),
                                                 cv = (sd / media) * 100,
                                                 se = sd(variable_salida_n) / sqrt(length(variable_salida_n)),
                                                 lq = quantile(variable_salida_n , probs = (0.05) ),
                                                 uq = quantile(variable_salida_n , probs = (0.95) ),
                                                 maxi = max(variable_salida_n),
                                                 mini = min(variable_salida_n),
                                                 rango = maxi - mini)

  resumen.medidas <- ddply(resumen.tabla_datos.variable_salida.6, .(parametro_n), summarise,
                           media = round(mean(media),2),
                           #media.se = mean(se),
                           sd.promedio = round(mean(sd),2),
                           #vari.promedio = mean(vari),
                           cv.promedio = round(mean(na.omit(cv)),2)
                           #rango.promedio = mean(rango),
                           #media.maxi = mean(maxi),
                           #media.mini = mean(mini),
                           #mediana = mean(mediana)
                         )
  print(variable_salida)
  print(resumen.medidas)
  #print(xtable(resumen.medidas, type = "latex"), file = paste("./imgs/",parametro,"-",variable_salida,".tex",sep=""))
  #write.table(resumen.medidas, file = paste("./imgs/",parametro,"-",variable_salida,".txt",sep=""), sep = ",", quote = FALSE, row.names = T)

  ## se convierte el tratamiento en un factor
  resumen.tabla_datos.variable_salida.6$parametro_n <- as.factor(resumen.tabla_datos.variable_salida.6$parametro_n)

  ################ VISUALIZACIÓN ########################
  ggplot(resumen.tabla_datos.variable_salida.6, aes(x=ano, y= multiplicar * media, colour = parametro_n, fill = parametro_n  )) +
    geom_ribbon(aes(ymin = multiplicar * lq, ymax = multiplicar * uq), alpha = 0.1, colour = 0) +
    #geom_ribbon(aes(ymin = multiplicar *(media - se), ymax = multiplicar * (media + se)), alpha = 0.1, colour = 0) +
    geom_line(size = 0.7) +
    #xlab("años") +
    xlab("years") +
    ylab(texto_y) +
    labs(fill = texto_cuadro, colour = texto_cuadro) +
    #scale_x_continuous(expand = c(0,0)) +
    #theme_classic2() +
    theme(text = element_text(size=tamano.texto))
  #######################################################
}
