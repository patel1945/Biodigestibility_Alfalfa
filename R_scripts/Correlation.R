##PCA
library(ggplot2)  
library(dplyr)     # For data manipulation
library(FactoMineR) # For PCA
library(ggfortify)

pheno_Bs=read.csv("/home/hawkins/Desktop/Smit/Biodigestibilty/BLUEs_ST/pheno_Bs.csv")
str(pheno_Bs)
pheno_Bs$Env=as.factor(pheno_Bs$Env)
phenoforPCA=pheno_Bs%>%select(-Treatment)

phenoforPCA=phenoforPCA%>%remove_missing(na.rm = TRUE)
pca_pheno=prcomp(select(phenoforPCA, -Env), scale. = TRUE)

autoplot(pca_pheno, data=phenoforPCA, color = 'Env')
summary(pca_pheno)


##Heat map and dedrogram for hierarchical clustering 

Pheno=pheno_Bs%>%remove_missing(na.rm=T)
Pheno_NO_T=Pheno%>%select(-Env, -Treatment)
Pheno_matrix=Pheno_NO_T%>%as.matrix()
rownames(Pheno_matrix)=Pheno$Treatment
str(Pheno_matrix)
h=heatmap(x=Pheno_matrix, scale = "column")
ggsave(h, file="/home/hawkins/Desktop/Smit/Biodigestibilty/heatmap.png")

##K means clustering
#install.packages('factoextra')
library(factoextra)
fviz_nbclust(select(phenoforPCA, -Env), kmeans, method = "silhouette")
kmeans_pheno=kmeans(select(phenoforPCA, -Env), center=3)
aggregate(select(phenoforPCA, -Env), by=list(cluster=kmeans_pheno$cluster), mean)
fviz_cluster(kmeans_pheno, data=select(phenoforPCA, -Env))


##Correlation matrix
library(corrplot)  # For correlation matrix visualization
phenoforCor=pheno_Bs%>%select(-Treatment, -Env)%>%remove_missing(na.rm = TRUE)
cor_matrix=cor(phenoforCor)
# Plot the correlation matrix using corrplot
corrplot(cor_matrix, method = "color")

