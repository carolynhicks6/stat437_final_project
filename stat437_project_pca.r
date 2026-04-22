##### LOAD DATA:
#install.packages("OncoDataSets")
library(OncoDataSets)
data("BreastCancerWI_df")


##### PERFORM PCA:
pca <- prcomp(BreastCancerWI_df[, -1], center=TRUE, scale.=TRUE) #remove diagnosis column
print(pca)
summary(pca)

##### VISUALIZE PCA:
plot(pca,type="l") # lbow plot
library(ggfortify)
autoplot(pca, data = BreastCancerWI_df, loadings = TRUE, loadings.label=TRUE, colour="diagnosis")

##### PC SELECTION & TRAINING DATASET:
top_pcs <- pca$x[,1:10]

library(dplyr)
df <- BreastCancerWI_df |> select(diagnosis)

top_pcs <- as.data.frame(top_pcs)
df <- as.data.frame(df)
training_df <-cbind(top_pcs, df)
training_df

##### MAX LOADINGS:
loadings <- pca$rotation 
abs_loadings <- abs(loadings)
max_loadings <- apply(abs_loadings[, 1:10], 1, max) # first 10 PCs explain 95% of variance
sorted <- sort(max_loadings, decreasing = TRUE)

loadings_df <- as.data.frame(sorted)
loadings_df

##### CORRELATION MATRIX:
corr_df <- select(BreastCancerWI_df, c(row.names(loadings_df)[1:10]))

library(corrplot)
cor_matrix <- cor(corr_df)
corrplot(cor_matrix, method='color')
