



# cor = TRUE indicates that PCA is performed on 
# standardized data (mean = 0, variance = 1)
inputData=NewData[complete.cases(NewData), ]
inputData <- as.data.frame(inputData)

pcaCars <- princomp(inputData, cor = TRUE)
pcaCars

res.pca <- prcomp(inputData)
res.pca
# view objects stored in pcaCars
names(pcaCars)

# proportion of variance explained
summary(pcaCars)

# scree plot
plot(pcaCars)
plot(pcaCars , type = "lines")
# cluster cars
carsHC <- hclust(dist(pcaCars$scores), method = "ward.D2")

# dendrogram
plot(carsHC)

# cut the dendrogram into 3 clusters
carsClusters <- cutree(carsHC, k = 3)

# add cluster to data frame of scores
carsDf <- data.frame(pcaCars$scores, "cluster" = factor(carsClusters))
cluster_name = paste("Cluster",carsClusters)
carsDf <- transform(carsDf, cluster_name)


library(plotly)
p <- plot_ly(carsDf, x = carsDf$Comp.1 , y = carsDf$Comp.2, text = rownames(carsDf),
             mode = "markers", color = cluster_name, marker = list(size = 11)) 

p <- layout(p, title = "PCA Clusters from Hierarchical Clustering of Cars Data", 
            xaxis = list(title = "PC 1"),
            yaxis = list(title = "PC 2"))

p


library(factoextra)
fviz_eig(res.pca)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

