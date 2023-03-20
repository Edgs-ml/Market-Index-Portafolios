library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(readxl)
library(fBasics)
library(ghyp)
library(naniar)
library(tidyverse)

#g1<- read_excel("D:/JL/Market-Index-Portafolios/APLHA/ALPHA 1/ALPHA 1.2/1.2.4 Optimizaci?n/Optimizacion NIG alpha 1.2.xlsx", 
#                sheet = "Datos")

g1 <- Optimizacion_NIG_alpha_1_2
View(g1)
#Esta base de datos ya tiene los retornos de los indices, no sacar retornos otra vez

colnames(g1)<-c("Fecha","DJI","HSI","OMX20","STI","FTSE")
g1 <- g1[,-7]

g1 %>%
  vis_miss()

g1 <- g1 %>%
  drop_na()

rendimientos <- xts(g1[,2:6], order.by = as.Date(g1$Fecha))
View(rendimientos)

#Los datos ya son los retornos, no es necesario volverlos a sacar. 
#Returns<- Return.calculate(PreciosAd, method = "log")[-1]

#------------- Creación de portafolio
Specs_Port <- portfolio.spec(c("DJI",	"HSI",	"OMX20",	"STI",	"FTSE"))

##### Add Constraints #####
Specs_Port <- add.constraint(Specs_Port,type="full_investment")
Specs_Port <- add.constraint(Specs_Port,type="long_only")

##### Add Objective #####
Specs_Port <- add.objective(Specs_Port,type="risk",name="StdDev")
Specs_Port <- add.objective(Specs_Port,type='return',name='mean')
Specs_Port

Optimized_Port_Normal <- optimize.portfolio(rendimientos,
                                      Specs_Port,
                                      optimize_method = "random",
                                      trace = TRUE)

covnig<-function(R,portfolio){
  a<-fit.NIGmv(R,silent=TRUE)
  COV<-a@variance
  mu<-a@expected.value
  mu<-matrix(mu,ncol = 1)
  resultado<-list(mu=mu,sigma=COV)
  return(resultado)
}

Optimized_Port_NIG <- optimize.portfolio(rendimientos,
                                         Specs_Port,
                                         momentFUN = covnig,
                                         optimize_method = "random",
                                         trace = TRUE)
#--------------
#-------------- Análisis de portafolios

chart.Weights(Optimized_Port_Normal, plot.type = "barplot")
W_R_Normal <- extractWeights(Optimized_Port_Normal)
W_R_Normal
sum(W_R_Normal)

chart.Weights(Optimized_Port_NIG, plot.type = "barplot")
W_R_NIG <- extractWeights(Optimized_Port_NIG)
W_R_NIG
sum(W_R_NIG)

Return_Port_Normal <- Return.portfolio(rendimientos, W_R_Normal)

table.AnnualizedReturns(Return_Port_Normal,
                        scale = 252,
                        geometric = FALSE)

Return.cumulative(Return_Port_Normal,
                  geometric = FALSE)

chart.RiskReward(Optimized_Port_Normal,
                 risk.col = 'StdDev',
                 return.col = 'mean',
                 chart.assets = TRUE)

Return_Port_NIG <- Return.portfolio(rendimientos, W_R_NIG)

table.AnnualizedReturns(Return_Port_NIG,
                        scale = 252,
                        geometric = FALSE)

Return.cumulative(Return_Port_NIG,
                  geometric = FALSE)

chart.RiskReward(Optimized_Port_NIG,
                 risk.col = 'StdDev',
                 return.col = 'mean',
                 chart.assets = TRUE)

#--------------Graficas comparativas

rand <- Optimized_Port1$random_portfolios
# 1794 portafolios de diferentes pesos al asar 

stdv <- Optimized_Port1$random_portfolio_objective_results[[1]]$objective_measures$StdDev

medias <- Optimized_Port1$random_portfolio_objective_results[[1]]$objective_measures$mean


#--------
mediasNORM <- NULL
standDevNORM <- NULL
for (i in 1:1682) {
  mediasNORM[i] <- Optimized_Port_Normal$random_portfolio_objective_results[[i]]$objective_measures$mean
  standDevNORM[i] <- Optimized_Port_Normal$random_portfolio_objective_results[[i]]$objective_measures$StdDev
}

mediasNIG <- NULL
standDeviNIG <- NULL
for (i in 1:1768) {
  mediasNIG[i] <- Optimized_Port_NIG$random_portfolio_objective_results[[i]]$objective_measures$mean
  standDeviNIG[i] <- Optimized_Port_NIG$random_portfolio_objective_results[[i]]$objective_measures$StdDev
}
#-------- Fronteras

fronteraNorm <- tibble(Volatility = standDevNORM, Expected_Return = mediasNORM)
fronteraNIG <- tibble(Volatility = standDeviNIG, Expected_Return = mediasNIG)

fronteraNorm %>%
  ggplot(aes(x=Volatility, y=Expected_Return))+
  geom_point(alpha=0.5, col="pink")+
  geom_point(data=tibble(Volatility = Optimized_Port_Normal$objective_measures$StdDev,
                         Expected_Return = Optimized_Port_Normal$objective_measures$mean),
             col="red", size=3)+
  geom_point(data=fronteraNIG, aes(x=Volatility, y=Expected_Return), alpha=0.2, col="blue")+
  geom_point(data=tibble(Volatility = Optimized_Port_NIG$objective_measures$StdDev, 
                         Expected_Return = Optimized_Port_NIG$objective_measures$mean), 
             alpha=1, col="darkgreen", size=3)
