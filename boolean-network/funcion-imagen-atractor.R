## Esta función genera una imagen similar a la de plottAtractor() de Boolnet

##### PAQUETES #####
library(reshape2) # para la función melt()
library(ggplot2) # para plotear
library(forcats) # para la función fct_rev() que pone los datos en reversa

imagenAtractor <- function(atractor, pasos.a.mover = 0, tamano.texto){
  # se convierte el atractor en matriz
  atractor.como.matriz <- data.matrix(atractor)

  # se convierte la matriz en un data frame con 3 columnas: iteración (Var1), nodo (Var2), estado (value)
  atractor.como.matriz.melteado <- melt(atractor.como.matriz)

  # se ajustan las iteraciones para facilitar su visualización
  atractor.como.matriz.melteado$Var1 <-(atractor.como.matriz.melteado$Var1 + pasos.a.mover)%%6
  atractor.como.matriz.melteado$Var1[atractor.como.matriz.melteado$Var1==0]<-6

  # visualización
  ggplot(atractor.como.matriz.melteado, aes(x=as.factor(Var1),  forcats::fct_rev(Var2) )) +
    geom_tile(aes(fill=as.factor(value)), color="black", size=0.2) +
    labs(x="Attractor state", y="Node") +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    #labs(fill = "Estado\ndel nodo") +
    labs(fill = "Node\nstate") +
    #annotate("rect", xmin=3.5, xmax=6.5, ymin=0.5, ymax=17.5, alpha=0.4, fill="white") +
    #annotate("text", x = 5, y = 17.65, label="Temporada de lluvias", size=2) +
    theme_bw() +
    #guides(col = guide_legend(title.position = "left", nrow = 1,title.vjust = 1), fill = guide_legend(title.position = "left",nrow = 1,title.vjust = 1)) +
    #theme(legend.position ="bottom") +
    geom_text(x=1,y=13.5,label="Jan-Feb",colour="gray",angle=90,size=2.5)+
    geom_text(x=2,y=13.5,label="Mar-Apr",colour="gray",angle=90,size=2.5)+
    geom_text(x=3,y=13.5,label="May-Jun",colour="gray",angle=90,size=2.5)+
    geom_text(x=4,y=13.5,label="Jul-Aug",colour="gray",angle=90,size=2.5)+
    geom_text(x=5,y=13.5,label="Sep-Oct",colour="gray",angle=90,size=2.5)+
    geom_text(x=6,y=13.5,label="Nov-Dec",colour="gray",angle=90,size=2.5)+
    theme(text = element_text(size=tamano.texto),plot.margin = unit(c(2.2,0,0,0), "lines")) + 
    coord_cartesian(clip = "off")
    #theme(axis.text.y = element_text(size = 10))
}
