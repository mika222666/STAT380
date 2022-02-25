rm(list =ls())
library(data.table)
library(caret)
library(ClusterR)
library(Rtsne) 

set.seed(1000)
data <- fread("./project/volume/data/raw/data.csv")

#- store and remove the wine types
id <- data$id
data$id <- NULL

#- first lets do pca
# data has to be numeric, make sure to remove ids
pca <- prcomp(data)

# lets look at the variance explained by these components
screeplot(pca)

# look at the variance and cummulative variance
summary(pca)

#- looks like 1 or 2 components is enough
biplot(pca)

# extract the components so i can use them in PCA
pca_dt <- data.table(unclass(pca)$x)

# add back the party to prove to ourselves that this works
pca_dt$id<-id

# see a plot with the party data 
ggplot(pca_dt[1:1000,],aes(x=PC1,y=PC2))+geom_point()

tsne_dat_1 <- Rtsne(pca_dt,  #PCA_DT
                    pca=F,   #F
                    perplexity = 30,   #5-50
                    max_iter = 1000,
                    check_duplicates = F)

tsne_dat_2 <- Rtsne(pca_dt,  #PCA_DT
                    pca=F,   #F
                    perplexity = 15,   #5-50
                    max_iter = 1000,
                    check_duplicates = F)

#extract the coordinates
tsne_data_table_1 <- data.table(tsne_dat_1$Y)
tsne_data_table_2 <- data.table(tsne_dat_2$Y)

tsne_dt <- cbind(tsne_data_table_1, tsne_data_table_2)


# add back in id and locus_1 so we can see what the analysis did with them
tsne_dt$id<-id

ggplot(tsne_dt,aes(x=V1,y=V2))+geom_point()

# this fits a gmm to the data for all k=1 to k= max_clusters, we then look for a major change in likelihood between k values
k_bic<-Optimal_Clusters_GMM(tsne_dt[,.(V1,V2)],max_clusters = 10,criterion = "BIC")

# now we will look at the change in model fit between successive k values
delta_k<-c(NA,k_bic[-1] - k_bic[-length(k_bic)])

# make a plot so you can see the values, this part isnt necessary
del_k_tab<-data.table(delta_k=delta_k,k=1:length(delta_k))

# plot 
ggplot(del_k_tab,aes(x=k,y=-delta_k))+geom_point()+geom_line()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_text(aes(label=k),hjust=0, vjust=-1)

# I think 3 clusters are more appropriate?

opt_num_clus <- 4

# now we run the model with our chosen k value
gmm_data<-GMM(tsne_dt[,.(V1,V2)],opt_num_clus)

clusterInfo <- predict_GMM(tsne_dt[,.(V1,V2)],
                           gmm_data$centroids,
                           gmm_data$covariance_matrices,
                           gmm_data$weights)

# clusterInfo contains a lot of information which we can extract
clusterInfo$log_likelihood
clusterInfo$cluster_proba
clusterInfo$cluster_labels
tmp <- data.table(clusterInfo$cluster_proba)

setnames(tmp,c("V1","V2","V3","V4"),c("Breed1","Breed2","Breed4","Breed3"))
tmp$id<-id

fwrite(tmp,'./project/volume/data/processed/submit_tsne.csv')
