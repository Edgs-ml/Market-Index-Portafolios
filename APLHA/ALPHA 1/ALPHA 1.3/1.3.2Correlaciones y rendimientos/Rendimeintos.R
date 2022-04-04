

library(readxl)
df<-read_excel("D:/JL/Market-Index-Portafolios/APLHA/ALPHA 1/ALPHA 1.2/1.2.2.Correlaciones/10 indices del PCA 1.xls",
               sheet="Hoja2")

library(naniar)
df<-drop_na(df)
View(df)
library(PortfolioAnalytics)

colnames(df)<-c("Fecha","SPX","OMXC","HSI","STI","FTSE","KOSPI","OMXS","MSCI","DAX")

#Ajuste de formato 
SPX<-xts(df$SPX,as.Date(df$Fecha))
FTSE<-xts(df$FTSE,as.Date(df$Fecha))
OMXC<-xts(df$OMXC,as.Date(df$Fecha))
STI<-xts(df$STI,as.Date(df$Fecha))
HSI<-xts(df$HSI,as.Date(df$Fecha))
KOSPI<-xts(df$KOSPI,as.Date(df$Fecha))
OMXS<-xts(df$OMXS,as.Date(df$Fecha))
MSCI<-xts(df$MSCI,as.Date(df$Fecha))
DAX<-xts(df$DAX,as.Date(df$Fecha))

#Formato de matriz
Portafolio<-merge.xts(SPX,FTSE,OMXC,STI,HSI,KOSPI,OMXS,MSCI,DAX)
#Retornos
Retornos<-Return.calculate(Portafolio,method = "log")[-1,]

#Formato
library(tidyr)
Retornos<-data.frame(na.omit(Retornos))


#library(PerformanceAnalytics)

Return.cumulative(Retornos)


