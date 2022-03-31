data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data
View(df)

# View the firt 3 rows of the data
head(df, n = 3)
library(factoextra)
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)
print(km.res)
km <- kmeans(df, centers = 4, nstart = 25)
fviz_cluster(km, data = df)
