## km1
k3c1 <- c("Canada", "China", "France", "Germany", 
          "Japan", "Korea, Rep", "Netherlands", "Switzerland",
          "United Kingdom", "United States")
k3c1_matrix <- indices[,c("Canada", "China", "France", "Germany", 
                          "Japan", "Korea, Rep", "Netherlands", "Switzerland",
                          "United Kingdom", "United States")]
k3c1_matrix_scaled <- scale(k3c1_matrix)
## km 2
k3c2 <- c("Australia", "India", "Italy", "Mexico", "Russian Federation", 
          "Saudi Arabia", "Spain")
k3c2_matrix <- indices[,c("Australia", "India", "Italy", "Mexico", 
                          "Russian Federation", "Saudi Arabia", "Spain")]
k3c2_matrix_scale <- scale(k3c2_matrix)
## BRICS
brics <- c("Brazil", "Russian Federation", "India", "China", "South Africa")
brics_matrix <- indices[,c("Brazil", "Russian Federation", "India", 
                           "China", "South Africa")]
brics_matrix_scale <- scale(brics_matrix)


# ADF
adftable.apply <- apply(indices,
                        MARGIN = 2,
                        FUN = adf.test) # de matrix a lista

adftable <- lapply(indices, adf.test) # de matrix a lista


adftable.dtf <- as.data.frame(adftable$`United States`$p.value)

america.adf <- as.data.frame(adftable[["United States"]][["statistic"]][["Dickey-Fuller"]])


adf.table.function <- function(serie, ){
  
  adf.apply <- apply(indices,
                     MARGIN = 2,
                     FUN = adf.test)
  
  for(i in adf.apply){
    # crear un nuevo objeto de cada sublista 
    print(i)
    # imprimir [[x]] de cada objeto
    print([["statistic"]])
  }
   
}

for(i in adf.apply){ #que aqui, en lugar de adf.apply, sean los objetos creados
  objeto <- i
}


# Ciclos for para crear objetos y pegar los nombre con base en la varriable i 
# en la que vaya el proceso

for(i in ){
  assign(paste0("Ad_",i), Ad(get(i)))
  Equity2_ad <- c(Equity2_ad, paste0("Ad_",i))
}

Equity2_Portfolio <- merge.xts(get(Equity2_ad[1]),
                               get(Equity2_ad[2]),
                               join = "inner")

for(i in 3:length(Equity2_ad)){
  Equity2_Portfolio <- merge.xts(Equity2_Portfolio,
                                 get(Equity_ad[i]),
                                 join = "inner")
}




adf_sti_table <- as.data.frame(c(basic.stat.indices$p.value))

adf_sti_table <- adf_sti_table %>%
  mutate(method=basic.stat.indices[["method"]])

adf_sti_table <- adf_sti_table %>%
  mutate(DF=adf_sti[["statistic"]][["Dickey-Fuller"]])

# KS Test automatizado


m <- mean(indices$`United States`)
sd <- sd(indices$`United States`)
len <- length(indices$`United States`)
basenormal <- dnorm(len,m,sd) #normal con los parametros de nuestras series


ks.function <- function(serie, base = basenormal){
  vector.series <- as.vector(serie)
  ks.test(vector.series, base)
}

lapply(brics_matrix, ks.function)




base.normal.fun <- function(serie){
  for(col in 1:ncol(serie)){
    assign(paste0("mean_", col), 
           mean(col))
  }
  
  
}
s <- base.normal.fun(brics_matrix)







ks.function <- function(serie, base){
  vector.series <- as.vector(serie)
  ks.test(vector.series, base)
}

lapply(brics_matrix, FUN = ks.function, base =)

### K3C1
lapply(k3c1_matrix$Canada, ks.function, base)

### K3C1
lapply(k3c2_matrix, ks.function)





ks.test(retornos$SP, )


