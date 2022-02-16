
library(readxl)
library(fBasics) # Analisis estadistico
library(aTSA)# Raiz Unitaria
library(zoo) # Requerido para cargar la libreria de tseries
library(tseries) # Raiz Unitaria
library(naniar) # remueve Nas
library(tidyverse) # Ya incluye read excel
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

g1<- read.csv("1.2.3.g1 5 menos correlacionados.csv")
View(g1)
colnames(g1)<-c("Fecha","SPX","OMXC","FTSE","KOSPI","MSCI")

View(g1)
g1<-column_to_rownames(g1,loc=1)

g1<-drop_na(g1)
glimpse(g1)
#vis_miss(g1)
#View(g1)

#g1<-Return.calculate(g1,method = "log")[-1,]
#g1<-drop_na(g1)
#View(g1)

Estg1<-basicStats(g1)
Estg1

adf.test(g1$SPX)
adf.test(g1$OMXC)
adf.test(g1$FTSE)
adf.test(g1$KOSPI)
adf.test(g1$MSCI)

#Todas las series son estacionarias

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

#Construccion de normal con parametros multivariados de la series
m<-mean(g1$SPX)
sd<-sd(g1$SPX)
len<-length(g1$SPX)
basenormal<-rnorm(len,m,sd)#normal con los parametros de nuestras series

#Prubas de Kormogorov contra distribucion normal, creada con paramaetros de nuestra serie 
ks.test(g1$SPX,basenormal)
ks.test(g1$OMXC,basenormal)
ks.test(g1$FTSE,basenormal)
ks.test(g1$KOSPI,basenormal)
ks.test(g1$MSCI,basenormal)
#Ninguna de las series es normal

mult.norm(g1)$mult.test
#De forma multivariada no hay normalidad


plot(density(g1$SPX),col="blue",ylim=c(0,60), main="Distribuciones contra normal")+
  lines(density(g1$OMXC),
        col="green")+
  lines(density(g1$FTSE),
        col="orange")+
  lines(density(g1$KOSPI),
        col="black")+
  lines(density(g1$MSCI),
        col="grey")+
  lines(density(basenormal),
        col="red")

#Parametros de la NIG
NIG<-nigFit(g1$OMXC)

#Agrupar parametros en un objeto
a<-NIG@fit[["par"]]
a<-data.frame(t(a))

#NIG aleatoria con parametors univariados de nuestra serie
r = rnig(len,
         alpha = a$alpha , 
         beta = a$beta, 
         delta = a$delta ,
         mu= a$mu)
plot(density(r),
     col="red",
     main="NIG Univariada",
     sub="STI index")

#Pruba de Kormogorov univariada para NIG
ks.test(g1$OMXC,r)

#Parametros para NIG Multivariada
multNIG<-fit.NIGmv(data=g1,silent=FALSE)

#Localizar parametros dentor de un obejto
Mom1NIGm<-multNIG@expected.value
Mom2NIGm<-multNIG@variance

#Construccion de la funcion NIG con nuestros  parametros de la funcion multivariada
Mnig <- rghyp(len,multNIG)
g11<-as.matrix(g1)

#Prueba cramer de comprobacion
#Se buscan similitudes estadisticas
cramer.test(Mnig,g11,conf.level = .95)

#Graficas de NIG Multivariada
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
