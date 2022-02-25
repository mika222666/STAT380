rm(list =ls())
library(data.table)
library(ggplot2)
library(caret)
library(ClusterR)
library(Metrics)

set.seed(3000)
# load in data 
data<-fread("./project/volume/data/raw/data.csv")

#- store and remove the id
id <- data$id
data$id <- NULL

#- first lets do pca
pca <- prcomp(data, center = TRUE, scale. = TRUE)

# look at the variance and cummulative variance
summary(pca)
#- looks like 1 or 2 components is enough

# extract the components so i can use them in PCA
pca_dt <- data.table(unclass(pca)$x)



# lets consider Gaussian mixture models GMM
max_clus <- 10
# will do GMM for k 1 to max_clus
k_aic <- Optimal_Clusters_GMM(pca_dt[,1:3], 
                              max_clusters = max_clus,
                              criterion = "AIC")

# how to determine the number of clusters
delta_k <- c(NA,k_aic[-1] - k_aic[-length(k_aic)])
del_k_tab <- data.table(delta_k=delta_k, k=1:length(delta_k))
# lets look at the change in slope
ggplot(del_k_tab,aes(x=k,y=-delta_k))+geom_point()+geom_line()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_text(aes(label=k),hjust=0, vjust=-1)


# For our assignment we "know" the correct number of clusters
opt_num_clus <- 4
gmm_data <- GMM(pca_dt[,1:3],opt_num_clus)

clusterInfo <- predict_GMM(pca_dt[,1:3],
                           gmm_data$centroids,
                           gmm_data$covariance_matrices,
                           gmm_data$weights)
# clusterInfo contains a lot of information which we can extract
clusterInfo$log_likelihood
clusterInfo$cluster_proba
clusterInfo$cluster_labels


tmp <- data.table(clusterInfo$cluster_proba)
setnames(tmp,c("V1","V2","V4","V3"),c("Breed1","Breed2","Breed3","Breed4"))
tmp$id<-id

fwrite(tmp,'./project/volume/data/processed/submit_tsne.csv')
