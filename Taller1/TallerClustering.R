library(readxl)
library(dplyr)
library(fpc)
library(cluster)
library(psych)


setwd("/home/david/Documentos/Analitica/Taller1")
bd <- read_xlsx("infoclientebanca.xlsx")


describe(bd[,3:7])

describe(bd[,8:15])

describe(bd[,16:25])

par(mfrow=c(2,2))
hist(bd$Numero_de_transacciones, xlim = c(0,100))
hist(bd$promedio_por_transaccion, xlim = c(0,5000000))
hist(bd$transaccion_minima, xlim = c(0,5000000))
hist(bd$transaccion_maxima, xlim = c(0,5000000))

par(mfrow=c(2,3))
hist(bd$porcentaje_visa_nacional,main = "Visa_Nal")
hist(bd$porcentaje_visa_internacional,main = "Visa_Int")
hist(bd$porcentaje_mastercard_nacional,main = "Master_Nal")
hist(bd$porcentaje_mastercard_internacional,main = "Master_Int")
hist(bd$porcentaje_nacional_total,main = "Porc_Nal")
hist(bd$porcentaje_internacional_total,main = "Porc_Int")

par(mfrow=c(2,3))
hist(bd$porcentaje_manana, main = "Porc_Mañana")
hist(bd$porcentaje_tarde, main = "Porc_Tarde")
hist(bd$porcentaje_noche ,main = "Porc_Noche")

par(mfrow=c(3,3))
hist(bd$porcLUNES)
hist(bd$porcMARTES)
hist(bd$porcMIERCOLES)
hist(bd$porcJUEVES)
hist(bd$porcVIERNES)
hist(bd$porcSABADO)
hist(bd$porcDOMINGO)

bdcpy <- bd %>% select(c(-grupo_de_cliente,-CLIENTE,-Sitio_consumo_masfrecuente,-porcDOMINGO,-porcSABADO,-porcLUNES,-porcMARTES,-porcMIERCOLES,-porcJUEVES,-porcVIERNES,-porcentaje_nacional_total,-porcentaje_internacional_total))

Perce <- function(var1,var2){floor((var1*(var2*100))/100)}

bdcpy$porcentaje_visa_nacional                <- mapply(Perce,bdcpy$Numero_de_transacciones,bdcpy$porcentaje_visa_nacional)
bdcpy$porcentaje_visa_internacional           <- mapply(Perce,bdcpy$Numero_de_transacciones,bdcpy$porcentaje_visa_internacional)
bdcpy$porcentaje_mastercard_nacional          <- mapply(Perce,bdcpy$Numero_de_transacciones,bdcpy$porcentaje_mastercard_nacional)
bdcpy$porcentaje_mastercard_internacional     <- mapply(Perce,bdcpy$Numero_de_transacciones,bdcpy$porcentaje_mastercard_internacional)
bdcpy$porcentaje_manana                       <- mapply(Perce,bdcpy$Numero_de_transacciones,bdcpy$porcentaje_manana)
bdcpy$porcentaje_tarde                        <- mapply(Perce,bdcpy$Numero_de_transacciones,bdcpy$porcentaje_tarde)
bdcpy$porcentaje_noche                        <- mapply(Perce,bdcpy$Numero_de_transacciones,bdcpy$porcentaje_noche)

bdcpy <- bdcpy %>% select(c(-Numero_de_transacciones))
bdcpy <- bdcpy %>% select(c(-Porcentaje_otrafranquicia_nacional,-porcentaje_otrafranquicia_internacional))

bdcpy <- as.data.frame(scale(bdcpy))

muestra1 <- sample_n(bdcpy,4500)
estGAP1 <- clusGap(muestra1, FUNcluster = kmeans, K.max = 7, B = 10)
clusasw1 <- kmeansruns(muestra1,krange=2:7,criterion="asw",iter.max=100, runs= 100,critout=TRUE)
resGAP1 <- as.data.frame(estGAP1$Tab)

muestra2 <- sample_n(bdcpy,4500)
estGAP2 <- clusGap(muestra2, FUNcluster = kmeans, K.max = 7, B = 10)
clusasw2 <- kmeansruns(muestra2,krange=2:7,criterion="asw",iter.max=100, runs= 100,critout=TRUE)
resGAP2 <- as.data.frame(estGAP2$Tab)

muestra3 <- sample_n(bdcpy,4500)
estGAP3 <- clusGap(muestra3, FUNcluster = kmeans, K.max = 7, B = 10)
clusasw3 <- kmeansruns(muestra3,krange=2:7,criterion="asw",iter.max=100, runs= 100,critout=TRUE)
resGAP3 <- as.data.frame(estGAP3$Tab)

muestra4 <- sample_n(bdcpy,4500)
estGAP4 <- clusGap(muestra4, FUNcluster = kmeans, K.max = 7, B = 10)
clusasw4 <- kmeansruns(muestra4,krange=2:7,criterion="asw",iter.max=100, runs= 100,critout=TRUE)
resGAP4 <- as.data.frame(estGAP4$Tab)

muestra5 <- sample_n(bdcpy,4500)
estGAP5 <- clusGap(muestra5, FUNcluster = kmeans, K.max = 7, B = 10)
clusasw5 <- kmeansruns(muestra5,krange=2:7,criterion="asw",iter.max=100, runs= 100,critout=TRUE)
resGAP5 <- as.data.frame(estGAP1$Tab)

muestra6 <- sample_n(bdcpy,4500)
estGAP6 <- clusGap(muestra6, FUNcluster = kmeans, K.max = 7, B = 10)
clusasw6 <- kmeansruns(muestra6,krange=2:7,criterion="asw",iter.max=100, runs= 100,critout=TRUE)
resGAP6 <- as.data.frame(estGAP6$Tab)

remove(muestra1)
remove(muestra2)
remove(muestra3)
remove(muestra4)
remove(muestra5)
remove(muestra6)

resultadosASW <- data.frame(clusasw1$crit,clusasw2$crit,clusasw3$crit,clusasw4$crit,clusasw5$crit,clusasw6$crit)
plot(resultadosASW)

resultadosGAP <- data.frame(resGAP1$gap,resGAP2$gap,resGAP3$gap,resGAP4$gap,resGAP5$gap,resGAP6$gap)
resultadosGAP <- data.frame(c(1,2,3,4,5,6,7),resGAP1$gap,resGAP2$gap,resGAP3$gap,resGAP4$gap,resGAP5$gap,resGAP6$gap)


library(ggplot2)
par(mfrow=c(2,3))
resGAP1 <- resGAP1 %>% mutate(K=1:7)
resGAP2 <- resGAP2 %>% mutate(K=1:7)
resGAP3 <- resGAP3 %>% mutate(K=1:7)
resGAP4 <- resGAP4 %>% mutate(K=1:7)
resGAP5 <- resGAP5 %>% mutate(K=1:7)
resGAP6 <- resGAP6 %>% mutate(K=1:7)

p1 <- ggplot(resGAP1, aes(x=1:7, y=gap, max=gap+SE.sim, min=gap-SE.sim))+geom_point()+geom_errorbar()+geom_line()
p2 <- ggplot(resGAP2, aes(x=1:7, y=gap, max=gap+SE.sim, min=gap-SE.sim))+geom_point()+geom_errorbar()+geom_line()
p3 <- ggplot(resGAP3, aes(x=1:7, y=gap, max=gap+SE.sim, min=gap-SE.sim))+geom_point()+geom_errorbar()+geom_line()
p4 <- ggplot(resGAP4, aes(x=1:7, y=gap, max=gap+SE.sim, min=gap-SE.sim))+geom_point()+geom_errorbar()+geom_line()
p5 <- ggplot(resGAP5, aes(x=1:7, y=gap, max=gap+SE.sim, min=gap-SE.sim))+geom_point()+geom_errorbar()+geom_line()
p6 <- ggplot(resGAP6, aes(x=1:7, y=gap, max=gap+SE.sim, min=gap-SE.sim))+geom_point()+geom_errorbar()+geom_line()

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
