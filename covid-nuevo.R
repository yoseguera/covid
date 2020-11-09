library(ggplot2)
library(scales)

calcular_incidencia <- function(datos, dias, poblacion) {
    incidencia_acumulada <- rep(0, nrow(datos))
    for (i in dias:(nrow(datos)))
    {
        incidencia <- 0
        for (j in 0:(dias-1))
        {            
            incidencia <- incidencia + datos[i-j, "num_casos"]
        }
        incidencia_acumulada[i] <- incidencia / poblacion * 100000
    }

    result <- incidencia_acumulada
}

datos <- read.csv("https://cnecovid.isciii.es/covid19/resources/datos_provincias.csv")
#datos <- read.csv("datos_provincias.csv")
datos_ab <- subset(datos, provincia_iso == "AB")
datos_ab$IA7_AB = calcular_incidencia(datos_ab, 7, 390032)
datos_ab$IA14_AB = calcular_incidencia(datos_ab, 14, 390032)

datos <- read.csv("https://cnecovid.isciii.es/covid19/resources/datos_ccaas.csv")
#datos <- read.csv("datos_ccaas.csv")
datos_clm <- subset(datos, ccaa_iso == "CM")
datos_ab$IA7_CM = calcular_incidencia(datos_clm, 7, 2115334)
datos_ab$IA14_CM = calcular_incidencia(datos_clm, 14, 2115334)

datos <- datos[c(2,3)]
datos_es <- aggregate(datos[c(2)], by = list(datos$fecha), FUN = sum, drop=TRUE)
datos_ab$IA7_ES = calcular_incidencia(datos_es, 7,  47329000)
datos_ab$IA14_ES = calcular_incidencia(datos_es, 14, 47329000)

datos_ab <- datos_ab[-c(nrow(datos_ab)),]
datos_ab$fecha <- as.Date(datos_ab$fecha)

leyenda <- c("Albacete", "Castilla-La Mancha", "España")

png(file = "incidencia7.png")
#limit_y = max(c(max(datos_ab$incidencia_7), max(datos_clm$incidencia_7)))
#plot(0, type="l", xlim=c(0,nrow(datos_ab)), ylim=c(1,limit_y))
#g1 <- ggplot(data=datos_ab[c("fecha", "incidencia_7")], aes(x=fecha, y=incidencia_7, group=1)) +
#    geom_line()
#g1 ggplot(data=datos_clm[c("fecha", "incidencia_7")], aes(x=fecha, y=incidencia_7, group=1)) +
#    geom_line()
ggplot(data=datos_ab, aes(x=fecha)) +
    geom_line(aes(y = IA7_AB, group = 1), color = "red") +
    geom_point(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA7_AB, group = 1), color = "red") +
    geom_text(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA7_AB, label = sprintf("%0.2f", round(IA7_AB, digits = 2))), position = position_dodge(width = 1), color = "red") +
    geom_line(aes(y = IA7_CM, group = 2), color = "blue") + 
    geom_point(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA7_CM, group = 1), color = "blue") +
    geom_text(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA7_CM, label = sprintf("%0.2f", round(IA7_CM, digits = 2))), color = "red") +
    geom_line(aes(y = IA7_ES, group = 3), color = "black") +
    geom_point(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA7_ES, group = 1), color = "black") +
    geom_text(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA7_ES, label = sprintf("%0.2f", round(IA7_ES, digits = 2))), color = "red")    
    
#lines(datos_ab$incidencia_7, col=1, lty=1, type="l")
#lines(datos_clm$incidencia_7, col=2, lty=2, type="l")
#lines(datos_es$incidencia_7, col=3, lty=3, type="l")
#legend("bottomleft", legend=leyenda, col=1:length(leyenda), lty=1:length(leyenda))
dev.off()

png(file = "incidencia14.png")
ggplot(data=datos_ab, aes(x=fecha)) +
    geom_line(aes(y = IA14_AB, group = 1), color = "red") +
    geom_point(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA14_AB, group = 1), color = "red") +
    geom_text(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA14_AB, label = sprintf("%0.2f", round(IA14_AB, digits = 2))), color = "red") +
    geom_line(aes(y = IA14_CM, group = 2), color = "blue") +     
    geom_point(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA14_CM, group = 1), color = "blue") +
    geom_text(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA14_CM, label = sprintf("%0.2f", round(IA14_CM, digits = 2))), color = "blue") +
    geom_line(aes(y = IA14_ES, group = 3), color = "black") +
    geom_point(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA14_ES, group = 1), color = "black") +
    geom_text(data = datos_ab[c(nrow(datos_ab)),], aes(y = IA14_ES, label = sprintf("%0.2f", round(IA14_ES, digits = 2))), color = "black")

#limit_y = max(c(max(datos_ab$incidencia_14), max(datos_clm$incidencia_14)))
#plot(0, type="l", xlim=c(0,nrow(datos_ab)), ylim=c(1,limit_y))
#lines(datos_ab$IA14_ES, col=1, lty=1, type="l")
#lines(datos_clm$incidencia_14, col=2, lty=2, type="l")
#lines(datos_es$incidencia_14, col=3, lty=3, type="l")
#legend("bottomleft", legend=leyenda, col=1:length(leyenda), lty=1:length(leyenda))
dev.off()

#datos_ab$IA14_ES