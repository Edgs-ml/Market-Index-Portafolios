

NIG_1.2<-c(" 0.0455","0.1570"," 0.2902"," 0.9315835")

Normal_1.2<-c(" 0.0536","0.1603"," 0.3346"," 1.097142")

#Resultados con nIG

Alpha1.2<-data.frame(NIG_1.2,Normal_1.2)

row.names(Alpha1.2)<-c("Return", "STD","Sharpe","CumRet")

View(Alpha1.2)

