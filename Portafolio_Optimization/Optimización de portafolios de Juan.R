{r}
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(readxl)
library(tidyverse)
library(textshape)
library(naniar)


{r base de datos}
g1<- read_excel("C:/Users/juanl/Desktop/paper/Bases de datos/Series de grupos/g1.xls")

colnames(g1)<-c("Fecha","OMX","STI","FTSE","HSI","DJI")

g1<-drop_na(g1)



{r matriz xts y orden de fechas}

FTSE<-xts(g1$FTSE,as.Date(g1$Fecha))
OMX<-xts(g1$OMX,as.Date(g1$Fecha))
STI<-xts(g1$STI,as.Date(g1$Fecha))
HSI<-xts(g1$HSI,as.Date(g1$Fecha))
DJI<-xts(g1$DJI,as.Date(g1$Fecha))

Portafolio<-merge.xts(OMX,STI,FTSE,HSI,DJI)
data.frame(Portafolio)


{r}
retornos<-Return.calculate(Portafolio)[-1,]


{r inicio de obejto y restricciones}
Specs_Port1 <- portfolio.spec(c("FTSE",
                                "NIKEI",
                                "SENSEX",#Iniciar objeto portafolio
                                "DAX",
                                "DJ",
                                "CAC",
                                "HSI"))

Specs_Port1<-add.constraint(Specs_Port1,#Suma 1 y no apalancado
                            type="full_investment")

Specs_Port1 <- add.constraint(Specs_Port1,
                              type="long_only")#sin cortos
#View(Specs_Port1)


{r obejtivos, minimizar o maximizar}
Specs_Port1 <- add.objective(Specs_Port1,
                             type="risk",#minimiza
                             name="StdDev")

Specs_Port1 <- add.objective(Specs_Port1,
                             type='return',#maximiza
                             name='mean')

Specs_Port1#Plantilla para especificaciones del portafolio.


{r Optimizacion del portafolio}

Optimized_Port1<-optimize.portfolio(retornos,Specs_Port1)
Optimized_Port1

chart.Weights(Optimized_Port1)
W <- extractWeights(Optimized_Port1)
W
sum(W)


{r Evaluacion del portafolio}
Return_opt <- Return.portfolio(retornos,W)

table.AnnualizedReturns(Return_opt,
                        scale = 252,
                        geometric = FALSE)
Return.cumulative(Return_opt,
                  geometric = FALSE)