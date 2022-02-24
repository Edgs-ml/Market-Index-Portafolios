#Librerias----------------------------------------------------------------------
library(readxl)
library(timeDate) # Requerido para cargar la libreria de fBasics
library(timeSeries) # Requerido para cargar la libreria de fBasics
library(fBasics) # Analisis estadistico
library(aTSA)# Raiz Unitaria
library(zoo) # Requerido para cargar la libreria de tseries
library(tseries) # Raiz Unitaria
library(naniar) # remueve Nas
library(tidyverse) # Ya incluye read excel
library(xts)
library(PerformanceAnalytics)
library(textshape)
library(boot) # Requuerida para cargar la libreria QuantPsyc
library(MASS) # Requuerida para cargar la libreria QuantPsyc
library(QuantPsyc) # Pruba multivariada de normlaidad
library(statmod)
library(numDeriv) # Requuerida para cargar la libreria ghyp
library(ghyp) # multivariado NIG
library(cramer) # Pruba cramer NIG multivariada
library(textshape)

#Ordenar la base----------------------------------------------------------------
g1<- read.csv("1.2.3.g1 5 menos correlacionados.csv")
View(g1)
colnames(g1)<-c("Fecha","SPX","OMXC","FTSE","KOSPI","MSCI")

View(g1)
g1<-column_to_rownames(g1,loc=1)

g1<-drop_na(g1)
g1 <- as.xts(g1)
glimpse(g1)
#vis_miss(g1) hacerlo antes de pasarlo a xxts
#View(g1)

#g1<-Return.calculate(g1,method = "log")[-1,]
#g1<-drop_na(g1)
#View(g1)

#Pruebas de estacionaridad------------------------------------------------------
Estg1<-basicStats(g1)
Estg1

adf.test(g1$SPX)
adf.test(g1$OMXC)
adf.test(g1$FTSE)
adf.test(g1$KOSPI)
adf.test(g1$MSCI)

#Todas las series son estacionarias

#Pruebas de normalidad----------------------------------------------------------
#Shapiro Wilk
shapiro.test(g1$SPX)
shapiro.test(g1$OMXC)
shapiro.test(g1$FTSE)
shapiro.test(g1$KOSPI)
shapiro.test(g1$MSCI)

#Shapiro Francia
sfTest(g1$SPX)
sfTest(g1$OMXC)
sfTest(g1$FTSE)
sfTest(g1$KOSPI)
sfTest(g1$MSCI)

#Lo que qeuremos es comparar gráficamente si los datos se parecen a una DN teorica
#Construccion de normal con parametros multivariados de la series


#Kormogorov--------------------------------------------Construir normal teorica
#Prubas de Kormogorov contra distribucion normal, 
#creada con paramaetros de nuestra serie 
ks.test(g1$SPX,basenormal)#*
ks.test(g1$OMXC,basenormal)#*
ks.test(g1$FTSE,basenormal)#*
ks.test(g1$KOSPI,basenormal)#*
ks.test(g1$MSCI,basenormal)#*
#Ninguna de las series es normal#*

#Prueba multivariada de normlaidad----------------------------------------------
mult.norm(g1)$mult.test
#De forma multivariada no hay normalidad



#-------------------------
#Teorica Normal ajustada con las series de tiempo de los activos del portafolio
muN_OMXC <- mean(g1$OMXC)
sdN_OMXC <- sd(g1$OMXC)
gridN_OMXC <- seq(-.20, .20, by = .001)
dN_OMXC <- dnorm(gridN_OMXC, muN_OMXC, sdN_OMXC)

muN_SPX <- mean(g1$SPX)
sdN_SPX <- sd(g1$SPX)
gridN_SPX <- seq(-.20, .20, by = .001)
dN_SPX <- dnorm(gridN_SPX, muN_SPX, sdN_SPX)

muN_FTSE <- mean(g1$FTSE)
sdN_FTSE <- sd(g1$FTSE)
gridN_FTSE <- seq(-.20, .20, by = .001)
dN_FTSE <- dnorm(gridN_FTSE, muN_FTSE, sdN_FTSE)

muN_KOSPI <- mean(g1$KOSPI)
sdN_KOSPI <- sd(g1$KOSPI)
gridN_KOSPI <- seq(-.20, .20, by = .001)
dN_KOSPI <- dnorm(gridN_KOSPI, muN_KOSPI, sdN_KOSPI)

muN_MSCI <- mean(g1$MSCI)
sdN_MSCI <- sd(g1$MSCI)
gridN_MSCI <- seq(-.20, .20, by = .001)
dN_MSCI <- dnorm(gridN_MSCI, muN_MSCI, sdN_MSCI)

#Teorica NIG ajustada--------------
NIG_OMXC <- nigFit(g1$OMXC)
NIG_SPX <- nigFit(g1$SPX)
NIG_FTSE <- nigFit(g1$FTSE)
NIG_KOSPI <- nigFit(g1$KOSPI)
NIG_MSCI <- nigFit(g1$MSCI)
#agrupar parametros en un objeto
ParametroNIG_OMXC <- NIG_OMXC@fit[["par"]]
ParametroNIG_OMXC <- data.frame(t(ParametroNIG_OMXC))
r_OMXC=dnig(gridN_OMXC,
            alpha = ParametroNIG_OMXC$alpha, 
            beta = ParametroNIG_OMXC$beta, 
            delta = ParametroNIG_OMXC$delta,
            mu = ParametroNIG_OMXC$mu)

ParametroNIG_SPX <- NIG_SPX@fit[["par"]]
ParametroNIG_SPX <- data.frame(t(ParametroNIG_SPX))
r_SPX=dnig(gridN_SPX,
           alpha = ParametroNIG_SPX$alpha,
           beta = ParametroNIG_SPX$beta,
           delta = ParametroNIG_SPX$delta,
           mu = ParametroNIG_SPX$mu)

ParametroNIG_FTSE <- NIG_FTSE@fit[["par"]]
ParametroNIG_FTSE <- data.frame(t(ParametroNIG_FTSE))
r_FTSE=dnig(gridN_FTSE,
            alpha = ParametroNIG_FTSE$alpha,
            beta = ParametroNIG_FTSE$beta,
            delta = ParametroNIG_FTSE$delta,
            mu = ParametroNIG_FTSE$mu)

ParametroNIG_KOSPI <- NIG_KOSPI@fit[["par"]]
ParametroNIG_KOSPI <- data.frame(t(ParametroNIG_KOSPI))
r_KOSPI=dnig(gridN_KOSPI,
             alpha = ParametroNIG_KOSPI$alpha,
             beta = ParametroNIG_KOSPI$beta,
             delta = ParametroNIG_KOSPI$delta,
             mu = ParametroNIG_KOSPI$mu)

ParametroNIG_MSCI <- NIG_MSCI@fit[["par"]]
ParametroNIG_MSCI <- data.frame(t(ParametroNIG_MSCI))
r_MSCI=dnig(gridN_MSCI,
            alpha = ParametroNIG_MSCI$alpha,
            beta = ParametroNIG_MSCI$beta,
            delta = ParametroNIG_MSCI$delta,
            mu = ParametroNIG_MSCI$mu)

#Graficas de normalidad y NIG Y Empiricas---------------------------------------------------------
#Queremos comparar una normal teorica ajustada con los datos de las series 
plot(x = gridN_OMXC, y = r_OMXC,
     type = "l", label = "Gráfica 1 OMXC")
lines(x=gridN_OMXC, y=dN_OMXC, col="red")
lines(density(g1$OMXC), col = "green")

plot(x = gridN_SPX, y = r_SPX,
     type = "l")
lines(x=gridN_SPX, y=dN_SPX, col="red")
lines(density(g1$SPX), col="green")

plot(x = gridN_FTSE, y = r_FTSE,
     type = "l")
lines(x=gridN_FTSE, y=dN_FTSE, col="red")
lines(density(g1$FTSE), col = "green") #Grafica normal, nig y empirica de FTSE

plot(x = gridN_KOSPI, y = r_KOSPI,
     type = "l")
lines(x=gridN_KOSPI, y=dN_KOSPI, col="red")
lines(density(g1$KOSPI), col = "green") #Grafica normal, nig y empirica de KOSPI

plot(x = gridN_MSCI, y = r_MSCI,
     type = "l")
lines(x=gridN_MSCI, y=dN_MSCI, col="red")
lines(density(g1$MSCI), col = "green") #Grafica normal, nig y empirica de MCSI

#Nig Multivariada---------------------------------------------------------------
#Parametros para NIG Multivariada
multNIG<-fit.NIGmv(data=g1,silent=FALSE)

#Localizar parametros dentor de un obejto
Mom1NIGm<-multNIG@expected.value
Mom2NIGm<-multNIG@variance

#Construccion de la funcion NIG con nuestros  parametros de la funcion multivariada
Mnig <- rghyp(len,multNIG)
g11<-as.matrix(g1)

#Prueba cramer de comprobacion--------------------------------------------------
#Se buscan similitudes estadisticas
cramer.test(Mnig,g11,conf.level = .95)

#Graficas de NIG Multivariada---------------------------------------------------
plot(density(Mnig),
     col="red",ylim=c(0,60),main="Distribuciones contra NIG multivariada")+
  lines(density(g1$SPX),
        col="green")+
  lines(density(g1$FTSE),
        col="purple")+
  lines(density(g1$KOSPI),
        col="black")+
  lines(density(g1$MSCI),
        col="dark blue")+
  lines(density(g1$OMXC),
        col="pink")

# De aqui obtenemos los momentos estadisticos de las series para poder hacer la 
# optimización de portafolio ALPHA 1.2. en el excel de la carpeta 1.2.4. Optimización
