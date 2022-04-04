library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(readxl)
library(fBasics)
library(ghyp)
library(naniar)
library(tidyverse)

g1<- read_excel("D:/JL/Market-Index-Portafolios/APLHA/ALPHA 1/ALPHA 1.2/1.2.4 Optimizaci?n/Optimizacion NIG alpha 1.2.xlsx", 
                sheet = "Datos")
View(g1)
#Esta base de datos ya tiene los retornos de los indices, no sacar retornos otra vez

colnames(g1)<-c("Fecha","DJI","HSI","OMX20","STI","FTSE")
g1 <- g1[,-7]

g1 %>%
  vis_miss()

g1 <- g1 %>%
  drop_na()

g2 <- 

DJI<-xts(g1$DJI,as.Date(g1$Fecha))
HSI<-xts(g1$HSI,as.Date(g1$Fecha))
OMX20<-xts(g1$OMX20,as.Date(g1$Fecha))
STI<-xts(g1$STI,as.Date(g1$Fecha))
FTSE<-xts(g1$FTSE,as.Date(g1$Fecha))

Portafolio <-merge.xts(DJI,HSI,OMX20,STI,FTSE)

rendimientos <- xts(g1[,2:6], order.by = as.Date(g1$...1))


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

covnig<-function(R,portfolio){
  a<-fit.NIGmv(R,silent=TRUE)
  COV<-a@variance
  mu<-a@expected.value
  mu<-matrix(mu,ncol = 1)
  resultado<-list(mu=mu,sigma=COV)
  return(resultado)
}

Optimized_Port1 <- optimize.portfolio(rendimientos,
                                      Specs_Port1,
                                      momentFUN = covnig,
                                      optimize_method = "random",
                                      trace = TRUE)

chart.Weights(Optimized_Port1,plot.type = "barplot")
W_R <- extractWeights(Optimized_Port1)
sum(W_R)

Return_opt1 <- Return.portfolio(rendimientos,W_R)

table.AnnualizedReturns(Return_opt1,
                        scale = 252,
                        geometric = FALSE)
Return.cumulative(Return_opt1,
                  geometric = FALSE)

chart.RiskReward(Optimized_Port1,
                 risk.col = 'StdDev',
                 return.col = 'mean',
                 chart.assets = T)
points(y=0.0015, x=0.010, col="red")

rand <- Optimized_Port1$random_portfolios

stdv <-Optimized_Port1$random_portfolio_objective_results[[1]]$objective_measures$StdDev

medias <- Optimized_Port1$random_portfolio_objective_results[[1]]$objective_measures$mean

medias <- NULL
standDevi <- NULL
for (i in 1:1839) {
  medias[i] <- Optimized_Port1$random_portfolio_objective_results[[i]]$objective_measures$mean
  standDevi[i] <- Optimized_Port1$random_portfolio_objective_results[[i]]$objective_measures$StdDev
}


Optimized_Port2 <- optimize.portfolio(rendimientos,
                                      Specs_Port1,
                                      optimize_method = "random",
                                      trace = TRUE)

frontera <- tibble(volatilidad = standDevi, ExpectedRet = medias)

frontera %>%
  ggplot(aes(x=volatilidad, y=ExpectedRet))+
  geom_point(alpha=0.3, col="blue")+
  geom_point(data=tibble(volatilidad = Optimized_Port1$objective_measures$StdDev, ExpectedRet = Optimized_Port1$objective_measures$mean), 
             col="red", size = 3)+
  geom_point(data=tibble(volatilidad = Optimized_Port2$objective_measures$StdDev, ExpectedRet = Optimized_Port2$objective_measures$mean), 
             col="green", size = 3)
  



