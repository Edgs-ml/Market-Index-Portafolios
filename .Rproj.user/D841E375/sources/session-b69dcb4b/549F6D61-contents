library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(readxl)
library(fBasics)
library(ghyp)

g1<- read_excel("D:/JL/Market-Index-Portafolios/APLHA/ALPHA 1/ALPHA 1.1/1.1.3Portafolio_Optimization/Optimizacion en excel/NIG/Optimizacion (el bueno1).xlsx", 
                sheet = "Datos")

#Esta base de datos ya tiene los retornos de los indices, no sacar retornos otra vez

colnames(g1)<-c("Fecha","DJI",	"HSI",	"OMX20",	"STI",	"FTSE")

DJI<-xts(g1$DJI,as.Date(g1$Fecha))
HSI<-xts(g1$HSI,as.Date(g1$Fecha))
OMX20<-xts(g1$OMX20,as.Date(g1$Fecha))
STI<-xts(g1$STI,as.Date(g1$Fecha))
FTSE<-xts(g1$FTSE,as.Date(g1$Fecha))

Portafolio_NORM<-merge.xts(DJI,HSI,OMX20,STI,FTSE)

#Los datos ya son los retornos, no es necesario volverlos a sacar. 
#Returns<- Return.calculate(PreciosAd, method = "log")[-1]

Specs_Port_NORM <- portfolio.spec(c("DJI",	"HSI",	"OMX20",	"STI",	"FTSE"))

##### Add Constraints #####
Specs_Port_NORM <- add.constraint(Specs_Port_NORM,type="full_investment")
Specs_Port_NORM <- add.constraint(Specs_Port_NORM,type="long_only")

##### Add Objective #####
Specs_Port_NORM <- add.objective(Specs_Port_NORM,type="risk",name="StdDev")
Specs_Port_NORM <- add.objective(Specs_Port_NORM,type='return',name='mean')
Specs_Port_NORM

Optimized_Port_NORM <- optimize.portfolio(Portafolio_NORM,
                                      Specs_Port_NORM,
                                      optimize_method = "random",
                                      trace = TRUE)

chart.Weights(Optimized_Port_NORM,plot.type="barplot")
W_R_NORM <- extractWeights(Optimized_Port_NORM)
sum(W_R_NORM)

Return_opt_NORM <- Return.portfolio(Portafolio_NORM,W_R_NORM)

table.AnnualizedReturns(Return_opt_NORM,
                        scale = 252,
                        geometric = FALSE)

Return.cumulative(Return_opt_NORM,
                  geometric = FALSE)

chart.RiskReward(Optimized_Port_NORM,
                 risk.col = 'StdDev',
                 return.col = 'mean',
                 chart.assets = T)
