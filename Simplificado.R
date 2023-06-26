library(tidyverse)
library(readxl)
library(textshape) #"Tools for Reshaping Text". Usado en columns_to_rownames
library(broom)
library(plotly)
library(scales)
library(caTools)
library(caret)
library(cluster)
library(factoextra) #Para graficar K-Means y PCA
library(psych) #Usado por su funsión de crar matices de correlaciones de colores
library(stats) #Para hacer el PCA
library(naniar) #Para limpiar las bases de datos
library(fBasics) #Analisis estadistico
library(PerformanceAnalytics)
library(statmod)
library(knitr)
library(stargazer)
library(kableExtra)
library(ggpubr)
library(ggdist)
library(ggExtra)
library(ggbeeswarm)
library(aTSA) #Raiz Unitaria
library(tseries) #Raiz Unitaria
library(QuantPsyc) #Pruba multivariada
library(ghyp) #Para hacer momentos estadisticos de la NIG
library(quantmod) #Para descargar datos
library(cramer) #Para la prueba de cramer 
library(PortfolioAnalytics)
library(DEoptim)
library(tidyquant)
library(NbClust)
library(dendextend)
library(xts)
library(zoo)

# Creación de las Data Frames
df <- read_excel("APLHA/ALPHA_1/ALPHA_1.1/1.1.1PCA_Codes/Criterios-Unificado (Datos para PCA).xlsx")
df <- df[,-6]
df_plot <- df #Data Frame para hacer gráficas descriptivas
df <- column_to_rownames(df, loc = 1)

df_plot <- cbind(df_plot[,-5], log10(df$GDP))
colnames(df_plot)[5] = "GDP"

df <- column_to_rownames(df_plot, 
                         loc = 1)

# Graficas descriptivas de las variables
df_plot %>%
  ggplot(aes(x=GDP))+
  geom_histogram(bins=100)+
  geom_label(data = df_plot %>%
               filter(GDP>13), aes(x=GDP,y=Country, 
                                        label=Country))

# PCA con df_gdplog
pca_df <- prcomp(df, 
                 center = TRUE, 
                 scale. = TRUE)
summary(pca_df)

#biplot(pca_df)

fviz_pca_biplot(pca_df)
fviz_contrib(pca_df, 
             choice = "var")
fviz_contrib(pca_df, 
             choice = "ind")
fviz_screeplot(pca_df)

df_PC1234 <- cbind(df, 
                   as.data.frame(pca_df$x)) %>% 
  arrange(desc(PC1))
View(df_PC1234)

df_PC12 <- as.data.frame(pca_df$x)
df_PC12 <- df_PC12[,1:2]
View(df_PC12)

# Kmeans sobre el PCA
NbClust(df_PC12,
        distance = "euclidean",
        method = "kmeans")

kmean1_df_PC12 <- kmeans(df_PC12, 
                           centers = 3,
                           iter.max = 50) #creamos objeto de kmeans con la df principal

fviz_cluster(kmean1_df_PC12, 
             data = df_PC12)

## Segundo kmeans sobre PC12
df_K1C1 <- cbind(df_PC12, 
                 as.data.frame(kmean1_df_PC12$cluster))
colnames(df_K1C1)[3] = "K1C1"
df_K1C1 <- df_K1C1 %>%
  filter(K1C1==3)
df_K1C1 <- df_K1C1[,-3]
View(df_K1C1)
#----
NbClust(df_K1C1,
        distance = "euclidean",
        method = "kmeans")

kmean2_df_PC12 <- kmeans(df_K1C1, 
                         centers = 2,
                         iter.max = 50)

fviz_cluster(kmean2_df_PC12, 
             data = df_K1C1)

## Tercer kmeans sobre PC12
df_K2C2 <- cbind(df_K1C1,
                 as.data.frame(kmean2_df_PC12$cluster))
colnames(df_K2C2)[3] = "K2C2"
df_K2C2 <- df_K2C2 %>%
  filter(K2C2==2)
df_K2C2 <- df_K2C2[,-3]
View(df_K2C2)
#----
kmean3_df_PC12 <- kmeans(df_K2C2, 
                         centers = 2,
                         iter.max = 50)

fviz_cluster(kmean3_df_PC12, 
             data = df_K2C2)

df_K3C2 <- cbind(df_K2C2,
                 as.data.frame(kmean3_df_PC12$cluster))
colnames(df_K3C2)[3] = "K3C2"
df_K3C2 <- df_K3C2 %>%
  filter(K3C2==2)
df_K3C2 <- df_K3C2[,-3]
View(df_K3C2)

df_K3C1 <- cbind(df_K2C2,
                 as.data.frame(kmean3_df_PC12$cluster))
colnames(df_K3C1)[3] = "K3C1"
df_K3C1 <- df_K3C1 %>%
  filter(K3C1==1)
df_K3C1 <- df_K3C1[,-3]
View(df_K3C1)

# Hirarchical Clustering sobre el PC12

hclust.out <- hclust(dist(df_PC12))
summary(hclust.out)

fviz_dend(hclust.out,
          repel = TRUE)

hc.out.dend <- as.dendrogram(hclust.out) # Solo para hacer otra forma del dendrograma

plot(hc.out.dend)+
  abline(h=1.4, col = "red")

branch.height <- get_branches_heights(hc.out.dend,
                                      sort = FALSE,
                                      decreasing = FALSE,
                                      include_leaves = FALSE)
branch.height <- as.data.frame(branch.height)

HC_clusters <- cutree(hclust.out, h = 1.4)
HC_clusters <- as.data.frame(HC_clusters)

# Final Data Frame
df_PC1234_HC_KM <- cbind(df_PC1234, 
                         HC_clusters)
colnames(df_PC1234_HC_KM)[9] <- "HC_h1.4"

df_PC1234_HC_KM <- cbind(df_PC1234_HC_KM,
                         as.data.frame(kmean1_df_PC12[["cluster"]]))
colnames(df_PC1234_HC_KM)[10] <- "KM_1"

#Portfolio1 <- df_GDPlog_PC1234_hk_km %>%
#  filter()
#View(ch16)



# Indices
indices <- read_excel("Indices_km.xlsx", 
                      sheet = "Hoja1",
                      col_types = c("date", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric"))
names(indices)[names(indices) == '...1'] <- 'Date'
indices <- indices[-1,]
indices <- na.omit(indices)
indices <- column_to_rownames(indices, loc = 1)
indices <- round(indices, digits = 2)
indices <- as.xts(indices)
new_names <- c("United States", "Australia", "India", "Italy", "Mexico", 
               "Russian Federation", "Saudi Arabia", "Spain", "Canada", "China",
               "France", "Germany","Japan", "Korea, Rep", "Netherlands", 
               "Switzerland", "United Kingdom", "Brazil", "South Africa")
colnames(indices) <- new_names
View(indices)
class(indices)

#-----Portafolios
Indices_Returns <- Return.calculate(indices)[-1,]
View(Indices_Returns)


Eq1_Specs_Port <- portfolio.spec(c())
##### Add Constraints #####
Eq1_Specs_Port <- add.constraint(Eq1_Specs_Port,
                                 type="full_investment")
Eq1_Specs_Port <- add.constraint(Eq1_Specs_Port,
                                 type="long_only")

##### Add Objective #####
Eq1_Specs_Port <- add.objective(Eq1_Specs_Port,
                                type="risk",
                                name="StdDev")
Eq1_Specs_Port <- add.objective(Eq1_Specs_Port,
                                type='return',
                                name='mean')
Eq1_Specs_Port

#------- Pruebas estadisticas
Estg1 <- basicStats(retornos)
Estg1

adf_sti <- adf.test(retornos$SP)

adf_sti_table <- as.data.frame(c(adf_sti$p.value))
adf_sti_table <- adf_sti_table %>%
  mutate(method=adf_sti[["method"]])
adf_sti_table <- adf_sti_table %>%
  mutate(DF=adf_sti[["statistic"]][["Dickey-Fuller"]])



adf.test(retornos$SP)
adf.test(retornos$OMX30)
adf.test(retornos$HSI)
adf.test(retornos$STI)
adf.test(retornos$DJI)
adf.test(retornos$KOSPI)
adf.test(retornos$DAX)

#### 3.1.2.2 Pruba de Kormogorov-Smirnov 
*contra distribucion normal, creada con paramaetros de nuestra serie**
  *Construccion de normal con parametros multivariados de la series*
  ```{r}
m <- mean(retornos$SP)
sd <- sd(retornos$SP)
len <- length(retornos$SP)
basenormal <- dnorm(len,m,sd)
#normal con los parametros de nuestras series

ks.test(retornos$SP, basenormal)
ks.test(retornos$OMX30, basenormal)
ks.test(retornos$HSI, basenormal)
ks.test(retornos$STI, basenormal)
ks.test(retornos$DJI, basenormal)
ks.test(retornos$KOSPI, basenormal)
ks.test(retornos$DAX, basenormal)
```


### 3.1.4 Portafolio Optimizado con Distribución Normal
### 3.1.5 Portafolio Optimizado con Distribución NIG
*Construcción de la NIG*
```{r}
#Parametros de la NIG
  NIG<-nigFit(retornos$SP)

#Agrupar parametros en un objeto
   a<-NIG@fit[["par"]]
   a<-data.frame(t(a))
   
#NIG aleatoria con parametors univariados de nuestra serie
   r = rnig(len,
            alpha = a$alpha, 
            beta = a$beta, 
            delta = a$delta,
            mu= a$mu)
   plot(density(r),
        col="black",
        main="NIG Univariada",
        sub="SP index")
   
#Pruba de Kormogorov univariada para NIG
ks.test(retornos$SP,r)
```

--------
```{r}
#Parametros para NIG Multivariada
multNIG <- fit.NIGmv(data=retornos,
                     silent=FALSE)

#Localizar parametros dentor de un obejto
Mom1NIGm <- multNIG@expected.value
Mom2NIGm <- multNIG@variance

#Construccion de la funcion NIG con nuestros  parametros de la funcion multivariada
Mnig <- rghyp(len, multNIG)
retornos1 <- as.matrix(retornos)

#Prueba cramer de comprobacion
  #Se buscan similitudes estadisticas
cramer.test(Mnig,
            retornos1,
            conf.level = .95)


```


