library(readxl)
library(dplyr)
library(fpc)
library(cluster)
library(psych)

setwd("/home/david/Documentos/Analitica/Codigo/Taller1")
bd <- read_xlsx("infoclientebanca.xlsx")


#Exploracion de los datos

"describe(bd[,3:7])

describe(bd[,8:15])

describe(bd[,16:25])

par(mfrow=c(2,2))"

#boxplot(bd$Numero_de_transacciones)
bd <- bd[-which(bd$Numero_de_transacciones > 65),] #Excluir a quines hacen mas de 65 transacciones

#boxplot(bd$promedio_por_transaccion)

#boxplot(bd$transaccion_minima)
bd <- bd[-which(bd$transaccion_minima < 2500),] #Excluir transacciones de menos de 2500

#boxplot(bd$transaccion_maxima)

'#par(mfrow=c(2,3))
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
hist(bd$porcDOMINGO)'#

#Transformacion y preparacion
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

#Modelado, seleccion de k
'#muestra1 <- sample_n(bdcpy,4500)
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
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)'#

#Modelado, prueba k = 7 
'#clusterClientes <- kmeans(bdcpy,centers = 7,iter.max = 10,nstart = 20)
kclusters <- clusterboot(bdcpy,B=10,clustermethod=kmeansCBI,k=7)
clusterClientes$size
kclusters$bootmean

#Modelado, prueba k = 6 
clusterClientes2 <- kmeans(bdcpy,centers = 6,iter.max = 10,nstart = 20)
kclusters2 <- clusterboot(bdcpy,B=10,clustermethod=kmeansCBI,k=6)
clusterClientes2$size
kclusters2$bootmean
clusterClientes2$centers

#Modelado, prueba k = 5
clusterClientes3 <- kmeans(bdcpy,centers = 5,iter.max = 20,nstart = 20)
kclusters3 <- clusterboot(bdcpy,B=10,clustermethod=kmeansCBI,k=5)
clusterClientes3$size
kclusters3$bootmean
clusterClientes3$centers'#

#Se quitan variables para intenter mejorar
bdcpy <- bdcpy %>% select(c(-porcentaje_manana,-porcentaje_noche,-porcentaje_tarde))

#Modelo definitivo
clusterClientes <- kmeans(bdcpy,centers = 7,iter.max = 20,nstart = 20)
kclusters <- clusterboot(bdcpy,B=10,clustermethod=kmeansCBI,k=7)
clusterClientes$size
kclusters$bootmean

centros <- as.data.frame(clusterClientes$centers) #Tomar los centros del modelo definitivo k=7
par(mfrow=c(2,4))
plot(centros$promedio_por_transaccion)
plot(centros$transaccion_minima)
plot(centros$transaccion_maxima)
plot(centros$desviacion_estandar_por_transaccion)
plot(centros$porcentaje_visa_nacional)
plot(centros$porcentaje_visa_internacional)
plot(centros$porcentaje_mastercard_nacional)
plot(centros$porcentaje_mastercard_internacional)

#Transformar para perfilamiento
bd$grupos <- clusterClientes$cluster
bd$Sitio_consumo_masfrecuente <- as.factor(bd$Sitio_consumo_masfrecuente)#Cambiar a categorica
for (i in 8:25){
  bd[i] <- mapply(Perce,bd[3],bd[i])
}


par(mfrow=c(1,2))
boxplot(porcentaje_nacional_total~grupos, data=bd, main="Compras Nacionales",xlab="Cluster", ylab="Numero de compras")
boxplot(porcentaje_internacional_total~grupos, data=bd, main="Compras Internacionales",xlab="Cluster", ylab="Numero de compras")

par(mfrow=c(1,3))
boxplot(porcentaje_manana~grupos, data=bd,main="Compras Franja Mañana",xlab="Cluster", ylab="Numero de compras")
boxplot(porcentaje_tarde~grupos, data=bd,main="Compras Franja Tarde",xlab="Cluster", ylab="Numero de compras")
boxplot(porcentaje_noche~grupos, data=bd,main="Compras Franja Noche",xlab="Cluster", ylab="Numero de compras")


par(mfrow=c(2,4))
boxplot(porcLUNES~grupos, data=bd,main="Compras Lunes",xlab="Cluster", ylab="Numero de compras")
boxplot(porcMARTES~grupos, data=bd,main="Compras Martes",xlab="Cluster", ylab="Numero de compras")
boxplot(porcMIERCOLES~grupos, data=bd,main="Compras Miercoles",xlab="Cluster", ylab="Numero de compras")
boxplot(porcJUEVES~grupos, data=bd,main="Compras Jueves",xlab="Cluster", ylab="Numero de compras")
boxplot(porcVIERNES~grupos, data=bd,main="Compras Viernes",xlab="Cluster", ylab="Numero de compras")
boxplot(porcSABADO~grupos, data=bd,main="Compras Sabado",xlab="Cluster", ylab="Numero de compras")
boxplot(porcDOMINGO~grupos, data=bd,main="Compras Domigo",xlab="Cluster", ylab="Numero de compras")


par(mfrow=c(2,3))
boxplot(porcentaje_mastercard_nacional~grupos, data=bd,main="Compras MasterNacional",xlab="Cluster", ylab="Numero de compras")
boxplot(porcentaje_visa_nacional~grupos, data=bd,main="Compras VisaNacional",xlab="Cluster", ylab="Numero de compras")
boxplot(Porcentaje_otrafranquicia_nacional~grupos, data=bd,main="Compras OtraNacional",xlab="Cluster", ylab="Numero de compras")
boxplot(porcentaje_mastercard_internacional~grupos, data=bd,main="Compras MasterInternacional",xlab="Cluster", ylab="Numero de compras")
boxplot(porcentaje_visa_internacional~grupos, data=bd,main="Compras VisaInternacional",xlab="Cluster", ylab="Numero de compras")
boxplot(porcentaje_otrafranquicia_internacional~grupos, data=bd,main="Compras OtraInternacional",xlab="Cluster", ylab="Numero de compras")

boxplot(Numero_de_transacciones~grupos, data=bd, main="Numero de Transacciones",xlab="Cluster", ylab="Numero de compras")


