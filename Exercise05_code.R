# Exercise 05
# Quizzes
# 1
# Option 3

# 2
# a -> number of pairs of elements that are in the same set in X and in Y
# b -> number of pairs of elements of S that are in different sets in X and Y
# (n 2) -> number of pairs of S
# R -> a + b / (n 2) 
# R = 0 + 4 / (6 2) = 0.4

# 3
# Option 1 or 3

# 4
# C and D

# Tutorial
# Section 00
library(ggplot2)
library(data.table)
library(magrittr) # Needed for %>% operator
library(tidyr)
install.packages("GGally")
install.packages("pheatmap")
install.packages("mclust")


library(GGally)
library(pheatmap)
library(mclust)

# Section 01 - Visualizing multiple variables
expr <- readRDS("C:/Users/usertest/Documents/Data Analysis/Lectures/extdata/extdata/cancer_data.rds") %>% as.data.table(keep.rownames="tumor_type")
head(expr[, 1:6])
mat <- as.matrix(expr[,-"tumor_type"])
rownames(mat) <- expr$tumor_type
colnames(mat) <-colnames(expr[,-"tumor_type"])

# 1
d <- dist(expr)
hc <- hclust(d)
plot(hc, hang = 1)

# 2
pheatmap(mat, cluster_rows = FALSE, cluster_cols = FALSE)

# 3
pheatmap(mat, cluster_rows = FALSE, cluster_cols = FALSE, scale = "column")

mat[mat == 100.00000000] <- NA

# Section 02 - Heatmaps and Hierarchical clustering
iris
iris_dt <- as.data.table(iris)
iris_mat <- as.matrix(iris_dt[,-"Species"])
rownames(iris_mat) <- iris_dt$Species
colnames(iris_mat) <- colnames(iris_dt[,-"Species"])

pheatmap(iris_mat, cluster_cols = F, cluster_rows = F)

# 2
pheatmap(iris_mat, cluster_cols = F, cluster_rows = T)

# 3
d_iris <- dist(iris_mat)
hc_iris <-hclust(d_iris)
plot(hc_iris, hang = 1)

clust_hc <- cutree(hc_iris, k= 3)
clust_hc

# Section 03 - Cluster comparison
# a = 1
# b = bd,be,ba,fd,fe,fa,cd,ce,ca,da = 10
# (6 2) = 5 * 6 / 2 = 15
# R = 11/15 = 0.73

# Homework
# Section 04 - Clustering and Heatmaps
# 1
# ???

# 2
clust_km <- kmeans(scale(iris_mat), 2, nstart= 20)
clust_km$cluster
