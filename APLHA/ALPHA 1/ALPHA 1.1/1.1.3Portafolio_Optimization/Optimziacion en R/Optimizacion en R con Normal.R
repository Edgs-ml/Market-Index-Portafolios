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

Portafolio<-merge.xts(DJI,HSI,OMX20,STI,FTSE)

#Los datos ya son los retornos, no es necesario volverlos a sacar. 
#Returns<- Return.calculate(PreciosAd, method = "log")[-1]

Specs_Port1 <- portfolio.spec(c("DJI",	"HSI",	"OMX20",	"STI",	"FTSE"))

##### Add Constraints #####
Specs_Port1 <- add.constraint(Specs_Port1,type="full_investment")
Specs_Port1 <- add.constraint(Specs_Port1,type="long_only")

##### Add Objective #####
Specs_Port1 <- add.objective(Specs_Port1,type="risk",name="StdDev")
Specs_Port1 <- add.objective(Specs_Port1,type='return',name='mean')
Specs_Port1

Optimized_Port1 <- optimize.portfolio(Portafolio,
                                      Specs_Port1,
                                      optimize_method = "random",
                                      trace = TRUE)

chart.Weights(Optimized_Port1)
W_R <- extractWeights(Optimized_Port1)
sum(W_R)

Return_opt2 <- Return.portfolio(Portafolio,W_R)

table.AnnualizedReturns(Return_opt2,
                        scale = 252,
                        geometric = FALSE)
Return.cumulative(Return_opt1,
                  geometric = FALSE)

chart.RiskReward(Optimized_Port1,
                 risk.col = 'StdDev',
                 return.col = 'mean',
                 chart.assets = T)
