
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(readxl)
library(fBasics)
library(ghyp)


g1<- read_excel("D:/JL/Market-Index-Portafolios/APLHA/ALPHA 1/ALPHA 1.2/1.2.4 Optimizaci?n/Optimizacion NIG alpha 1.2.xlsx", 
                sheet = "Datos")

View(g1)
#Esta base de datos ya tiene los retornos de los indices, no sacar retornos otra vez

colnames(g1)<-c("Fecha","SPX",	"OMXC",	"FTSE","KOSPI","MSCI")
g1 <- g1[,-7]
g1 <- na.omit(g1)

SPX<-xts(g1$SPX,as.Date(g1$Fecha))
OMXC<-xts(g1$OMXC,as.Date(g1$Fecha))
FTSE<-xts(g1$FTSE,as.Date(g1$Fecha))
KOSPI<-xts(g1$KOSPI,as.Date(g1$Fecha))
MSCI<-xts(g1$MSCI,as.Date(g1$Fecha))

Portafolio_NIG_A1.2<-merge.xts(SPX,OMXC,FTSE,KOSPI,MSCI)

#Los datos ya son los retornos, no es necesario volverlos a sacar. 
#Returns<- Return.calculate(PreciosAd, method = "log")[-1]

Specs_Port_NIG_A1.2 <- portfolio.spec(c("SPX",	"OMXC",	"FTSE","KOSPI","MSCI"))

##### Add Constraints #####
Specs_Port_NIG_A1.2 <- add.constraint(Specs_Port_NIG_A1.2,
                                      type="full_investment")
Specs_Port_NIG_A1.2 <- add.constraint(Specs_Port_NIG_A1.2,
                                      type="long_only")

##### Add Objective #####
Specs_Port_NIG_A1.2 <- add.objective(Specs_Port_NIG_A1.2,
                                     type="risk",
                                     name="StdDev")
Specs_Port_NIG_A1.2 <- add.objective(Specs_Port_NIG_A1.2,
                                     type='return',
                                     name='mean')
Specs_Port_NIG_A1.2

covnig<-function(R,portfolio){
  a<-fit.NIGmv(R,silent=TRUE)
  COV<-a@variance
  mu<-a@expected.value
  mu<-matrix(mu,ncol = 1)
  resultado<-list(mu=mu,sigma=COV)
  return(resultado)
}

Optimized_Port_NIG_A1.2 <- optimize.portfolio(Portafolio_NIG_A1.2,
                                      Specs_Port_NIG_A1.2,
                                      momentFUN = covnig,
                                      optimize_method = "random",
                                      trace = TRUE)

chart.Weights(Optimized_Port_NIG_A1.2,plot.type = "barplot")
W_R <- extractWeights(Optimized_Port_NIG_A1.2)
sum(W_R)

Return_opt_NIG_A1.2 <- Return.portfolio(Portafolio_NIG_A1.2,W_R)

table.AnnualizedReturns(Return_opt_NIG_A1.2,
                        scale = 252,
                        geometric = FALSE)
Return.cumulative(Return_opt_NIG_A1.2,
                  geometric = FALSE)

chart.RiskReward(Optimized_Port_NIG_A1.2,
                 risk.col = 'StdDev',
                 return.col = 'mean',
                 chart.assets = T)
