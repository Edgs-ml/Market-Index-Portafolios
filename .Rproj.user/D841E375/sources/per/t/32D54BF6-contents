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

# Creación de las Data Frames
df <- read_excel("APLHA/ALPHA_1/ALPHA_1.1/1.1.1PCA_Codes/Criterios-Unificado (Datos para PCA).xlsx")
df <- df[,-6]
df_plot <- df #Data Frame para hacer gráficas descriptivas
df <- column_to_rownames(df, loc = 1)

df_gdplog_plot <- cbind(df_plot[,-5], log10(df$GDP))
colnames(df_gdplog_plot)[5] = "GDP"

df_gdplog <- column_to_rownames(df_gdplog_plot, loc = 1)

# Graficas descriptivas de las variables
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

fviz_pca_biplot(pca_df_gdplog)
fviz_contrib(pca_df_gdplog, 
             choice = "var")
fviz_contrib(pca_df_gdplog, 
             choice = "ind")
fviz_screeplot(pca_df_gdplog)

df_gdplog_PC1234 <- cbind(df_gdplog, pca_df_gdplog$x) %>% 
  arrange(desc(PC1))
View(df_gdplog_PC1234)

df_PC12 <- as.data.frame(pca_df_gdplog$x)
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
#----
NbClust(df_K1C1,
        distance = "euclidean",
        method = "kmeans")

kmean2_df_PC12 <- kmeans(df_K1C1, 
                         centers = 5,
                         iter.max = 50)

fviz_cluster(kmean2_df_PC12, 
             data = df_K1C1)

# Kmean sin PCA
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


# Hirarchical Clustering sobre el PC12

hclust.out <- hclust(dist(df_PC12))
summary(hclust.out)

hc.out.dend <- as.dendrogram(hclust.out)

fviz_dend(hclust.out,
          repel = TRUE,
          rect = TRUE,
          color_labels_by_k = TRUE)

plot(hc.out.dend)+
  abline(h=1.4, col = "red")

branch.height <- get_branches_heights(hc.out.dend,
                                      sort = FALSE,
                                      decreasing = FALSE,
                                      include_leaves = FALSE)
branch.height <- as.data.frame(branch.height)

clusters <- cutree(hc.out, h = 1.4)
ch <- as.data.frame(clusters)

ch16 <- ch %>%
  filter(clusters == 16)
View(ch16)


#chatgpt
clusters <- cutree(hc.out.dend, h = 1.5)

# Determine the number of clusters
num_clusters <- length(unique(clusters))

# Print the number of clusters
print(num_clusters)





