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

df <- read_excel("APLHA/ALPHA_1/ALPHA_1.1/1.1.1PCA_Codes/Criterios-Unificado (Datos para PCA).xlsx")
df <- df[,-6]
df_plot <- df #Data Frame para hacer gráficas descriptivas
df <- column_to_rownames(df, loc = 1)

df_gdplog_plot <- cbind(df_plot[,-5], log10(df$GDP))
colnames(df_gdplog_plot)[5] = "GDP"

df_gdplog <- column_to_rownames(df_gdplog_plot, loc = 1)




df_gdplog_plot %>%
  ggplot(aes(x=GDP))+
  geom_histogram(bins=100)+
  geom_label(data = df_gdplog_plot %>%
               filter(GDP>13), aes(x=GDP,y=Country, 
                                        label=Country))

# PCA con df_gdplog
pca_df_gdplog <- prcomp(df_gdplog, 
                 center = TRUE, 
                 scale. = TRUE)
summary(pca_df_gdplog)

biplot(pca_df_gdplog)

fviz_pca_ind(pca_df_gdplog)
fviz_pca_biplot(pca_df_gdplog)
fviz_contrib(pca_df_gdplog, 
             choice = "var")
fviz_contrib(pca_df_gdplog, 
             choice = "ind")
fviz_screeplot(pca_df_gdplog)

df_gdplog_PC1234 <- cbind(df_gdplog, pca_df_gdplog$x) %>% 
  arrange(desc(PC1))

# Kmean
NB_df_gdplog <- NbClust(df_gdplog,
                        distance = "euclidean",
                        method = "kmeans")

kmean1_df_gdplog <- kmeans(df_gdplog, 
                           centers = 8,
                           iter.max = 20) #creamos objeto de kmeans con la df principal

fviz_cluster(kmean1_df_gdplog, 
             data = df_gdplog)

kmean1_df_gdplogdf <- cbind(df_gdplog, 
                            as.data.frame(kmean1_df_gdplog$cluster))
colnames(kmean1_df_gdplogdf)[5] = "k1"

df_gdplog_k1C1 <- kmean1_df_gdplogdf %>%
  filter(k1==1)
df_gdplog_k1C1 <- df_gdplog_k1C1[,-5]

#--- Segundo kmeans

kmean2_df_gdplog <- kmeans(df_gdplog_k1C1,
                           centers = 2,
                           iter.max = 10)
fviz_cluster(kmean2_df_gdplog,
             data = df_gdplog_k1C1)



kmean2_df_gdplogdf <- cbind(df_gdplog_k1C1, 
                            as.data.frame(kmean2_df_gdplog$cluster))
colnames(kmean2_df_gdplogdf)[5] = "k2"

df_gdplog_k1C1 <- kmean1_df_gdplogdf %>%
  filter(k1==1)
df_gdplog_k1C1 <- df_gdplog_k1C1[,-5]




# Hirarchical Clustering

hclust.out <- hclust(dist(pca_df_gdplog))
summary(hclust.out)
agn_pc1 <- agnes(pc1_df1, method = "ward")
pltree(agn_pc1)
cutree(hclust.out, h=2)






