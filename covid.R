library(ggplot2)
library(scales)

calcular_derivada <- function(datos) {
    diferencia <- rep(0, nrow(datos))    
    
    for (i in 2:(nrow(datos)))
    {
        diferencia[i] <- (datos$ia14d[i] - datos$ia14d[i-1])/(as.numeric(datos$fecha[i]) - as.numeric(datos$fecha[i-1]))
    }
    
    result <- diferencia
}

datos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_ia14d.csv")
datos_calculados <- subset(datos, cod_ine == 8)
datos_calculados$fecha <- as.Date(datos_calculados$fecha)
datos_calculados$DIF_CLM = calcular_derivada(datos_calculados)

datos_sim <- datos[c(1,4)]
datos_es <- aggregate(datos_sim$ia14d, by = list(datos_sim$fecha), FUN = sum, drop=TRUE)
datos_es$fecha <- datos_es$Group.1
datos_es$ia14d <- datos_es$x/17
datos_calculados$fecha <- as.Date(datos_calculados$fecha)
datos_calculados$IA14_ES = datos_es$ia14d
datos_calculados$DIF_ES = calcular_derivada(datos_es)



leyenda <- c("Castilla-La Mancha", "España")

png(file = "ia14.png")
ggplot(data=datos_calculados, aes(x=fecha)) +
    geom_line(aes(y = ia14d, group = 1), color = "red") +
    geom_point(data = datos_calculados[c(nrow(datos_calculados)),], aes(y = ia14d, group = 1), color = "red") +
    geom_text(data = datos_calculados[c(nrow(datos_calculados)),], aes(y = ia14d, label = sprintf("%0.2f", round(ia14d, digits = 2))), position = position_dodge(width = 1), color = "red") +
    geom_line(aes(y = IA14_ES, group = 2), color = "blue") + 
    geom_point(data = datos_calculados[c(nrow(datos_calculados)),], aes(y = IA14_ES, group = 1), color = "blue") +
    geom_text(data = datos_calculados[c(nrow(datos_calculados)),], aes(y = IA14_ES, label = sprintf("%0.2f", round(IA14_ES, digits = 2))), color = "red") 
    #geom_line(aes(y = IA7_ES, group = 3), color = "black") +
    #geom_point(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA7_ES, group = 1), color = "black") +
    #geom_text(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA7_ES, label = sprintf("%0.2f", round(IA7_ES, digits = 2))), color = "red")    
    
dev.off()

png(file = "ia14-log.png")
ggplot(data=datos_calculados, aes(x=fecha)) +
    geom_line(aes(y = ia14d, group = 1), color = "red") +
    geom_point(data = datos_calculados[c(nrow(datos_calculados)),], aes(y = ia14d, group = 1), color = "red") +
    geom_text(data = datos_calculados[c(nrow(datos_calculados)),], aes(y = ia14d, label = sprintf("%0.2f", round(ia14d, digits = 2))), position = position_dodge(width = 1), color = "red") +
    geom_line(aes(y = IA14_ES, group = 2), color = "blue") + 
    geom_point(data = datos_calculados[c(nrow(datos_calculados)),], aes(y = IA14_ES, group = 1), color = "blue") +
    geom_text(data = datos_calculados[c(nrow(datos_calculados)),], aes(y = IA14_ES, label = sprintf("%0.2f", round(IA14_ES, digits = 2))), color = "red") +
    scale_y_continuous(trans='log10')
    #geom_line(aes(y = IA7_ES, group = 3), color = "black") +
    #geom_point(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA7_ES, group = 1), color = "black") +
    #geom_text(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA7_ES, label = sprintf("%0.2f", round(IA7_ES, digits = 2))), color = "red")    
    
dev.off()

png(file = "derivada.png")
ggplot(data=datos_calculados, aes(x=fecha)) +
    geom_line(aes(y = DIF_CLM, group = 1), color = "red") +
    geom_point(data = datos_calculados[c(nrow(datos_calculados)),], aes(y = DIF_CLM, group = 1), color = "red") +
    geom_text(data = datos_calculados[c(nrow(datos_calculados)),], aes(y = DIF_CLM, label = sprintf("%0.2f", round(DIF_CLM, digits = 2))), position = position_dodge(width = 1), color = "red") + 
    geom_line(aes(y = DIF_ES, group = 2), color = "blue") + 
    geom_point(data = datos_calculados[c(nrow(datos_calculados)),], aes(y = DIF_ES, group = 1), color = "blue") +
    geom_text(data = datos_calculados[c(nrow(datos_calculados)),], aes(y = DIF_ES, label = sprintf("%0.2f", round(DIF_ES, digits = 2))), color = "red") #+
    #geom_line(aes(y = IA7_ES, group = 3), color = "black") +
    #geom_point(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA7_ES, group = 1), color = "black") +
    #geom_text(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA7_ES, label = sprintf("%0.2f", round(IA7_ES, digits = 2))), color = "red")    
    
dev.off()