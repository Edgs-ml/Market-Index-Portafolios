#Valores generados en la optimizacion

NIG_1.1<-c(" 0.0397", " 0.1481"," 0.2678"," 0.8268")

Normal_1.1<-c(" 0.0434","0.1482"," 0.2929"," 0.90544")

#Resultados con nIG

Alpha1.1<-data.frame(NIG_1.1,Normal_1.1)

row.names(Alpha1.1)<-c("Return", "STD","Sharpe","CumRet")

View(Alpha1.1)
