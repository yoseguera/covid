datos <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
paises <- c("Spain", "Germany", "France", "Belgium", "United_Kingdom", "Italy", "Netherlands", "Singapore", "Japan", "South_Korea")

png(file = "muertes-mundo.png")
plot(0, type="l", xlim=c(0,60), ylim=c(1,500))
for (i in 1:length(paises))
{    
    pais <- paises[i]
    condicion <- datos[,"countriesAndTerritories"] == pais
    datos_pais <- rev(datos[condicion, "deaths"]/datos[condicion, "popData2018"]*1000000)
    datos_pais <- cumsum(datos_pais)
    datos_pais <- datos_pais[datos_pais>1]
    lines(datos_pais, col=i, lty=i, type="l")     
}
legend("topleft", legend=paises, col=1:length(paises), lty=1:length(paises))
dev.off()

png(file = "casos-mundo.png")
plot(0, type="l", xlim=c(0,100), ylim=c(1,5000))
for (i in 1:length(paises))
{    
    pais <- paises[i]
    condicion <- datos[,"countriesAndTerritories"] == pais
    datos_pais <- rev(datos[condicion, "cases"]/datos[condicion, "popData2018"]*1000000)
    datos_pais <- cumsum(datos_pais)
    datos_pais <- datos_pais[datos_pais>1]
    lines(datos_pais, col=i, lty=i, type="l")     
}
legend("topleft", legend=paises, col=1:length(paises), lty=1:length(paises))
dev.off()

datos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos_long.csv")
ccaas <- read.csv("ccaa.csv", stringsAsFactors=FALSE)
png(file = "casos-ccaa.png")
plot(0, type="l", xlim=c(0,60), ylim=c(500,200000))
for (i in 1:nrow(ccaas))
{    
    ccaa <- ccaas[i,"nombre"]
    poblacion <- ccaas[i,"poblacion"]
    condicion <- datos[,"CCAA"] == ccaa
    datos_ccaa <- rev(datos[condicion, "total"]/poblacion*1000000)    
    datos_ccaa <- cumsum(datos_ccaa)
    datos_ccaa <- datos_ccaa[datos_ccaa>1]
    lines(datos_ccaa, col=i, lty=i, type="l")     
}
legend("topleft", legend=ccaas[,"nombre"], col=1:nrow(ccaas), lty=1:nrow(ccaas))
dev.off()

datos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos_long.csv")
ccaas <- read.csv("ccaa.csv", stringsAsFactors=FALSE)
png(file = "muertos-ccaa.png")
plot(0, type="l", xlim=c(0,60), ylim=c(100,30000))
for (i in 1:nrow(ccaas))
{    
    ccaa <- ccaas[i,"nombre"]
    poblacion <- ccaas[i,"poblacion"]
    condicion <- datos[,"CCAA"] == ccaa
    datos_ccaa <- rev(datos[condicion, "total"]/poblacion*1000000)    
    datos_ccaa <- cumsum(datos_ccaa)
    datos_ccaa <- datos_ccaa[datos_ccaa>1]
    lines(datos_ccaa, col=i, lty=i, type="l")     
}
legend("topleft", legend=ccaas[,"nombre"], col=1:nrow(ccaas), lty=1:nrow(ccaas))
dev.off()