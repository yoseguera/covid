datos <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
paises <- c("Spain", "Germany", "France", "Belgium", "United_Kingdom", "Italy", "Netherlands", "Singapore", "Japan", "South_Korea", "United_States_of_America")

maxX <- 0
maxY <- 0
for (i in 1:length(paises))
{    
    pais <- paises[i]
    condicion <- datos[,"countriesAndTerritories"] == pais
    datos_pais <- rev(datos[condicion, "deaths"]/datos[condicion, "popData2018"]*1000000)
    datos_pais <- cumsum(datos_pais)
    datos_pais <- datos_pais[datos_pais>1]
    if (maxX < length(datos_pais))
    {
        maxX <- length(datos_pais)
    }        
    if (maxY < max(datos_pais)) 
    {
        maxY <- max(datos_pais)  
    }
}

png(file = "muertes-mundo.png")
plot(0, type="l", xlim=c(0,maxX), ylim=c(1,maxY))
for (i in 1:length(paises))
{    
    pais <- paises[i]
    condicion <- datos[,"countriesAndTerritories"] == pais
    datos_pais <- rev(datos[condicion, "deaths"]/datos[condicion, "popData2018"]*1000000)
    datos_pais <- cumsum(datos_pais)
    datos_pais <- datos_pais[datos_pais>1]
    lines(datos_pais, col=i, lty=i, type="l")     
}
legend("bottomleft", legend=paises, col=1:length(paises), lty=1:length(paises))
dev.off()

maxX <- 0
maxY <- 0
for (i in 1:length(paises))
{    
    pais <- paises[i]
    condicion <- datos[,"countriesAndTerritories"] == pais
    datos_pais <- rev(datos[condicion, "cases"]/datos[condicion, "popData2018"]*1000000)
    datos_pais <- cumsum(datos_pais)
    datos_pais <- datos_pais[datos_pais>1]
    if (maxX < length(datos_pais)) 
    {
        maxX <- length(datos_pais)
    }
    if (maxY < max(datos_pais))
    {
        maxY <- max(datos_pais)  
    }
}

png(file = "casos-mundo.png")
plot(0, type="l", xlim=c(0,maxX), ylim=c(1,maxY))
for (i in 1:length(paises))
{    
    pais <- paises[i]
    condicion <- datos[,"countriesAndTerritories"] == pais
    datos_pais <- rev(datos[condicion, "cases"]/datos[condicion, "popData2018"]*1000000)
    datos_pais <- cumsum(datos_pais)
    datos_pais <- datos_pais[datos_pais>1]
    lines(datos_pais, col=i, lty=i, type="l")     
}
legend("bottomleft", legend=paises, col=1:length(paises), lty=1:length(paises))
dev.off()

datos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos_long.csv")
ccaas <- read.csv("ccaa.csv", stringsAsFactors=FALSE)
maxX <- 0
maxY <- 0
for (i in 1:nrow(ccaas))
{    
    ccaa <- ccaas[i,"nombre"]
    poblacion <- ccaas[i,"poblacion"]
    condicion <- datos[,"CCAA"] == ccaa
    datos_ccaa <- rev(datos[condicion, "total"]/poblacion*1000000)    
    datos_ccaa <- cumsum(datos_ccaa)
    datos_ccaa <- datos_ccaa[datos_ccaa>1]
    if (maxX < length(datos_ccaa))
    {
        maxX <- length(datos_ccaa)
    }
    if (maxY < max(datos_ccaa))
    {
        maxY <- max(datos_ccaa)  
    }
}

png(file = "casos-ccaa.png")
plot(0, type="l", xlim=c(0,maxX), ylim=c(500,maxY))
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
legend("bottomleft", legend=ccaas[,"nombre"], col=1:nrow(ccaas), lty=1:nrow(ccaas))
dev.off()

datos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos_long.csv")
ccaas <- read.csv("ccaa.csv", stringsAsFactors=FALSE)
maxX <- 0
maxY <- 0
for (i in 1:nrow(ccaas))
{    
    ccaa <- ccaas[i,"nombre"]
    poblacion <- ccaas[i,"poblacion"]
    condicion <- datos[,"CCAA"] == ccaa
    datos_ccaa <- rev(datos[condicion, "total"]/poblacion*1000000)    
    datos_ccaa <- cumsum(datos_ccaa)
    datos_ccaa <- datos_ccaa[datos_ccaa>1]
    if (maxX < length(datos_ccaa))
    {
        maxX <- length(datos_ccaa)
    }
    if (maxY < max(datos_ccaa))
    {
        maxY <- max(datos_ccaa) 
    }
}

png(file = "muertos-ccaa.png")
plot(0, type="l", xlim=c(0,maxX), ylim=c(100,maxY))
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
legend("bottomleft", legend=ccaas[,"nombre"], col=1:nrow(ccaas), lty=1:nrow(ccaas))
dev.off()