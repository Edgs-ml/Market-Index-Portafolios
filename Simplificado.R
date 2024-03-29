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

#hclust.out <- hclust(dist(df_PC12))
#summary(hclust.out)

#fviz_dend(hclust.out,
#          repel = TRUE)

#hc.out.dend <- as.dendrogram(hclust.out) # Solo para hacer otra forma del dendrograma

#plot(hc.out.dend)+
#  abline(h=1.4, col = "red")

#branch.height <- get_branches_heights(hc.out.dend,
#                                      sort = FALSE,
#                                      decreasing = FALSE,
#                                      include_leaves = FALSE)
#branch.height <- as.data.frame(branch.height)

#HC_clusters <- cutree(hclust.out, h = 1.4)
#HC_clusters <- as.data.frame(HC_clusters)

# Final Data Frame
#df_PC1234_HC_KM <- cbind(df_PC1234, 
#                         HC_clusters)
#colnames(df_PC1234_HC_KM)[9] <- "HC_h1.4"

#df_PC1234_HC_KM <- cbind(df_PC1234_HC_KM,
#                         as.data.frame(kmean1_df_PC12[["cluster"]]))
#colnames(df_PC1234_HC_KM)[10] <- "KM_1"

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

indices_scale <- scale(indices)

plot(indices_scale)

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

#------- Pruebas estadisticas
## Basic Stats
basic.stat.indices <- basicStats(indices)
basic.stats.k3c1 <- basicStats(k3c1_matrix)
basic.stats.k3c2 <- basicStats(k3c2_matrix)
basic.stats.brics <- basicStats(brics_matrix)
## adf.test
adf.apply <- apply(indices,
                   MARGIN = 2,
                   FUN = adf.test)
adf.table <- read_excel("adf.table.xlsx", sheet = "Sheet2")
names(adf.table)[names(adf.table) == '...1'] <- 'Country'

adf.table <- adf.table %>%
  arrange(desc(`p-value`))

adf.table %>%
  ggplot(aes(x=Country, y=`p-value`))+
  geom_col()

## Pruba de Kormogorov-Smirnov 


### contra distribucion normal, creada con paramaetros de nuestra serie
m_United.States <- mean(indices$`United States`)
sd_United.States <- sd(indices$`United States`)
len_United.States <- length(indices$`United States`)
base.normal_United.States <- dnorm(len_United.States,
                                   m_United.States,
                                   sd_United.States)

m_Australia <- mean(indices$Australia)
sd_Australia <- sd(indices$Australia)
len_Australia <- length(indices$Australia)
base.normal_Australia <- dnorm(len_Australia,
                               m_Australia,
                               sd_Australia)

m_India <- mean(indices$India)
sd_India <- sd(indices$India)
len_India <- length(indices$India)
base.normal_India <- dnorm(len_India,
                               m_India,
                               sd_India)

m_Italy <- mean(indices$Italy)
sd_Italy <- sd(indices$Italy)
len_Italy <- length(indices$Italy)
base.normal_Italy <- dnorm(len_Italy,
                           m_Italy,
                           sd_Italy)

m_Mexico <- mean(indices$Mexico)
sd_Mexico <- sd(indices$Mexico)
len_Mexico <- length(indices$Mexico)
base.normal_Mexico <- dnorm(len_Mexico,
                           m_Mexico,
                           sd_Mexico)

m_Russian.Federation <- mean(indices$`Russian Federation`)
sd_Russian.Federation <- sd(indices$`Russian Federation`)
len_Russian.Federation <- length(indices$`Russian Federation`)
base.normal_Russian.Federation <- dnorm(len_Russian.Federation,
                            m_Russian.Federation,
                            sd_Russian.Federation)

m_Saudi.Arabia <- mean(indices$`Saudi Arabia`)
sd_Saudi.Arabia <- sd(indices$`Saudi Arabia`)
len_Saudi.Arabia <- length(indices$`Saudi Arabia`)
base.normal_Saudi.Arabia <- dnorm(len_Saudi.Arabia,
                                        m_Saudi.Arabia,
                                        sd_Saudi.Arabia)

m_Spain <- mean(indices$Spain)
sd_Spain <- sd(indices$Spain)
len_Spain <- length(indices$Spain)
base.normal_Spain <- dnorm(len_Spain,
                           m_Spain,
                           sd_Spain)

m_Canada <- mean(indices$Canada)
sd_Canada <- sd(indices$Canada)
len_Canada <- length(indices$Canada)
base.normal_Canada <- dnorm(len_Canada,
                           m_Canada,
                           sd_Canada)

m_China <- mean(indices$China)
sd_China <- sd(indices$China)
len_China <- length(indices$China)
base.normal_China <- dnorm(len_China,
                            m_China,
                            sd_China)

m_France <- mean(indices$France)
sd_France <- sd(indices$France)
len_France <- length(indices$France)
base.normal_France <- dnorm(len_France,
                           m_France,
                           sd_France)

m_Germany <- mean(indices$Germany)
sd_Germany <- sd(indices$Germany)
len_Germany <- length(indices$Germany)
base.normal_Germany <- dnorm(len_Germany,
                            m_Germany,
                            sd_Germany)

m_Japan <- mean(indices$Japan)
sd_Japan <- sd(indices$Japan)
len_Japan <- length(indices$Japan)
base.normal_Japan <- dnorm(len_Japan,
                             m_Japan,
                             sd_Japan)

m_Korea.Rep <- mean(indices$`Korea, Rep`)
sd_Korea.Rep <- sd(indices$`Korea, Rep`)
len_Korea.Rep <- length(indices$`Korea, Rep`)
base.normal_Korea.Rep <- dnorm(len_Korea.Rep,
                           m_Korea.Rep,
                           sd_Korea.Rep)

m_Netherlands <- mean(indices$Netherlands)
sd_Netherlands <- sd(indices$Netherlands)
len_Netherlands <- length(indices$Netherlands)
base.normal_Netherlands <- dnorm(len_Netherlands,
                               m_Netherlands,
                               sd_Netherlands)

m_Switzerland <- mean(indices$Switzerland)
sd_Switzerland <- sd(indices$Switzerland)
len_Switzerland <- length(indices$Switzerland)
base.normal_Switzerland <- dnorm(len_Switzerland,
                                 m_Switzerland,
                                 sd_Switzerland)

m_United.Kingdom <- mean(indices$`United Kingdom`)
sd_United.Kingdom <- sd(indices$`United Kingdom`)
len_United.Kingdom <- length(indices$`United Kingdom`)
base.normal_United.Kingdom <- dnorm(len_United.Kingdom,
                                 m_United.Kingdom,
                                 sd_United.Kingdom)

m_Brazil <- mean(indices$Brazil)
sd_Brazil <- sd(indices$Brazil)
len_Brazil <- length(indices$Brazil)
base.normal_Brazil <- dnorm(len_Brazil,
                            m_Brazil,
                            sd_Brazil)

m_South.Africa <- mean(indices$`South Africa`)
sd_South.Africa <- sd(indices$`South Africa`)
len_South.Africa <- length(indices$`South Africa`)
base.normal_South.Africa <- dnorm(len_South.Africa,
                            m_South.Africa,
                            sd_South.Africa)

#### ks tests
ks_United.States <- ks.test(as.vector(indices$`United States`),
                            base.normal_United.States)
ks_United.States <- as.data.frame(ks_United.States[["p.value"]])

ks_Australia <- ks.test(as.vector(indices$Australia),
                        base.normal_Australia)
ks_Australia <- as.data.frame(ks_Australia[["p.value"]])

ks_India <- ks.test(as.vector(indices$India),
                    base.normal_India)
ks_India <- as.data.frame(ks_India[["p.value"]])

ks_Italy <- ks.test(as.vector(indices$Italy),
                    base.normal_Italy)
ks_Italy <- as.data.frame(ks_Italy[["p.value"]])

ks_Mexico <- ks.test(as.vector(indices$Mexico),
                     base.normal_Mexico)
ks_Mexico <- as.data.frame(ks_Mexico[["p.value"]])

ks_Russian.Federation <- ks.test(as.vector(indices$`Russian Federation`),
                                 base.normal_Russian.Federation)
ks_Russian.Federation <- as.data.frame(ks_Russian.Federation[["p.value"]])

ks_Saudi.Arabia <- ks.test(as.vector(indices$`Saudi Arabia`),
                           base.normal_Saudi.Arabia)
ks_Saudi.Arabia <- as.data.frame(ks_Saudi.Arabia[["p.value"]])

ks_Spain <- ks.test(as.vector(indices$Spain),
                    base.normal_Spain)
ks_Spain <- as.data.frame(ks_Spain[["p.value"]])

ks_Canada <- ks.test(as.vector(indices$Canada),
                     base.normal_Canada)
ks_Canada <- as.data.frame(ks_Canada[["p.value"]])

ks_China <- ks.test(as.vector(indices$China),
                    base.normal_China)
ks_China <- as.data.frame(ks_China[["p.value"]])

ks_France <- ks.test(as.vector(indices$France),
                     base.normal_France)
ks_France <- as.data.frame(ks_France[["p.value"]])

ks_Germany <- ks.test(as.vector(indices$Germany),
                      base.normal_Germany)
ks_Germany <- as.data.frame(ks_Germany[["p.value"]])

ks_Japan <- ks.test(as.vector(indices$Japan),
                    base.normal_Japan)
ks_Japan <- as.data.frame(ks_Japan[["p.value"]])

ks_Korea.Rep <- ks.test(as.vector(indices$`Korea, Rep`),
                    base.normal_Korea.Rep)
ks_Korea.Rep <- as.data.frame(ks_Korea.Rep[["p.value"]])

ks_Netherlands <- ks.test(as.vector(indices$Netherlands),
                          base.normal_Netherlands)
ks_Netherlands <- as.data.frame(ks_Netherlands[["p.value"]])

ks_Switzerland <- ks.test(as.vector(indices$Switzerland),
                          base.normal_Switzerland)
ks_Switzerland <- as.data.frame(ks_Switzerland[["p.value"]])

ks_United.Kingdom <- ks.test(as.vector(indices$`United Kingdom`),
                             base.normal_United.Kingdom)
ks_United.Kingdom <- as.data.frame(ks_Brazil[["p.value"]])

ks_Brazil <- ks.test(as.vector(indices$Brazil),
                     base.normal_Brazil)
ks_Brazil <- as.data.frame(ks_Brazil[["p.value"]])

ks_South.Africa <- ks.test(as.vector(indices$`South Africa`),
                     base.normal_South.Africa)
ks_South.Africa <- as.data.frame(ks_South.Africa[["p.value"]])


ks.brics <- cbind(ks_Brazil,
                  ks_Russian.Federation,
                  ks_India,
                  ks_China,
                  ks_South.Africa)
colnames(ks.brics) <- brics
rownames(ks.brics) <- "p.value"

ks_k3c1 <- cbind(ks_Canada,
                 ks_China,
                 ks_France,
                 ks_Germany,
                 ks_Japan,
                 ks_Korea.Rep,
                 ks_Netherlands,
                 ks_Switzerland,
                 ks_United.Kingdom,
                 ks_United.States)
colnames(ks.k3c1) <- k3c1
rownames(ks.k3c1) <- "p.value"

ks_k3c2 <- cbind(ks_Australia,
                 ks_India,
                 ks_Italy,
                 ks_Mexico,
                 ks_Russian.Federation,
                 ks_Saudi.Arabia,
                 ks_Spain)
colnames(ks_k3c2) <- k3c2
rownames(ks_k3c2) <- "p.value"

#Construccion de normal con parametros multivariados de la series
base.normal_United.States


## Construcción de la NIG
### Parametros de la NIG

NIG <- nigFit(indices$`United States`)

#Agrupar parametros en un objeto
a <- NIG@fit[["par"]]
a <- data.frame(t(a))
   
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
ks.test(as.vector(indices$Australia), r)


#--------

#Parametros para NIG Multivariada
multNIG <- fit.NIGmv(data = indices,
                     silent = FALSE)

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













# Portafolios ---------------------------
brics_Specs_Portfolio <- portfolio.spec(brics)
k3c1_Specs_Portfolio <- portfolio.spec(k3c1)
k3c2_Specs_Portfolio <- portfolio.spec(k3c2)
##### Add Constraints #####
brics_Specs_Portfolio <- add.constraint(brics_Specs_Portfolio,
                                        type="full_investment")
brics_Specs_Portfolio <- add.constraint(brics_Specs_Portfolio,
                                        type="long_only")


##### Add Objective #####
brics_Specs_Portfolio <- add.objective(brics_Specs_Portfolio,
                                       type="risk",
                                       name="StdDev")
brics_Specs_Portfolio <- add.objective(brics_Specs_Portfolio,
                                       type='return',
                                       name='mean')
brics_Specs_Portfolio




brics_Optimum_Portfolio <- optimize.portfolio(brics_matrix, 
                                              brics_Specs_Portfolio) 
k3c1_Optimum_Portfolio <- optimize.portfolio(k3c1_matrix,
                                           k3c1_Specs_Portfolio)
k3c2_Optimum_Portfolio <- optimize.portfolio(k3c2_matrix,
                                           k3c2_Specs_Portfolio)
#en caso de que no converja, usar ", optimize_method = random"

chart.Weights(brics_Optimum_Portfolio)


# nuevo objeto para los rendimientos del port
bricRendOptPort <- Return.portfolio(brics_matrix,
                                    extractWeights(brics_Optimum_Portfolio))
plot(density(bricRendOptPort))

VaR(RendOptPort)*sqrt(1)*1000000

charts.PerformanceSummary(RendOptPort)



table.AnnualizedReturns(RendOptPort) #por default esta anualizado
table.AnnualizedReturns(Rend)

