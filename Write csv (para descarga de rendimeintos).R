
library(readxl)
  df<-read_excel("C:/Users/juanl/Desktop/paper/Bases de datos/Series de grupos/Charting Excel Export - Jan 2nd 2022 11_56_10 pm.xls")

library(naniar)
  df<-drop_na(df)
  
library(PortfolioAnalytics)
  
  colnames(df)<-c("Fecha","OMX","STI","FTSE","HSI","S&P500")
  
  FTSE<-xts(df$FTSE,as.Date(df$Fecha))
  OMX<-xts(df$OMX,as.Date(df$Fecha))
  STI<-xts(df$STI,as.Date(df$Fecha))
  HSI<-xts(df$HSI,as.Date(df$Fecha))
  Sp<-xts(df$`S&P500`,as.Date(df$Fecha))
  
  Portafolio<-merge.xts(OMX,STI,FTSE,HSI,Sp)
  
  Retornos<-Return.calculate(Portafolio,method = "log")[-1,]
  Retornos<-data.frame(Retornos)
  
  View(Retornos)
  
  write.csv(Retornos,"DIRECCION DEL ARCHIVO EN TU PC O GITHUB\\NOMBREX.csv")
  
  
  
  
  
  
  
  
  
  