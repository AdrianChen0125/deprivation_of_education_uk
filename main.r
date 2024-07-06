# Install and load the foreign package
library(factoextra)
library(RColorBrewer)
library(GGally)
library(cowplot)
library(cluster)
library(corrplot)
library(tidyverse)
library(foreign)
library(ggplot2)
library(dplyr)

##[1]extract data  (Read the DBF file into a data)##
#[1-1]Read data of England 2019
dbf_file <- "IMD_2019.dbf"
Imd_2019_data <- read.dbf(dbf_file)
##[2]Data preprocess##
#[2-1-1]Select required variables
IMD_2019_LSOA<-Imd_2019_data[grepl("IncScore|EmpScore|EduScore|HDDScore|CriScore|BHSScore|EnvScore|LADnm|lsoa11nmw|TotPop|DepChi", colnames(Imd_2019_data))]
IMD_2019_LSOA$S_Depchi<-IMD_2019_LSOA$DepChi/IMD_2019_LSOA$TotPop
#[2-1-2]View missing value (No missing V)
summary (IMD_2019_LSOA)
#[2-1-3]Transforming group by mean and sum for clustering 
IMD_2019_LA<-IMD_2019_LSOA %>%
group_by(LADnm)%>%
summarise(
  IncScore = mean(IncScore),
  EmpScore = mean(EmpScore),
  EduScore = mean(EduScore),
  HDDScore = mean(HDDScore),
  CriScore = mean(CriScore),
  BHSScore = mean(BHSScore),
  EnvScore = mean(EnvScore),
  S_Depchi = sum(DepChi)/sum(TotPop),
  )
##[3]Data exploratory analysis##
#[3-1]Descriptive analysis
#box chart for independent variables
#Population
boxplot(IMD_2019_LSOA$S_Depchi, col = "lightblue", main = "Share of dependent chidren in total population",horizontal = TRUE)
#score
boxplot(IMD_2019_LSOA[c("IncScore","EmpScore")], col = "lightblue", main = "Income and employment",horizontal = TRUE)
boxplot(IMD_2019_LSOA[c("HDDScore","CriScore")], col = "lightblue", main = "health care and crime",horizontal = TRUE)
boxplot(IMD_2019_LSOA[c("BHSScore","EnvScore")], col = "lightblue", main = "Barrier to house and environment",horizontal = TRUE)


#Distribution of Education Score in LSOA

ggplot(IMD_2019_LSOA, aes(x = EduScore)) +
geom_histogram(binwidth = 5, aes(y = ..density..), fill = "lightblue", color = "black") +
geom_boxplot(width = 0.01, 
             position = position_dodge(100),
             colour = "#3366FF",
             outlier.colour = "black", outlier.shape = 1 ,outlier.alpha = 0.1)+
geom_density(color = "RED")+
labs(title = "Distribution of education deprivation score in LSOA", x = "Education deprivation score", y = "density")

#[3-2]Correlation matrix
#compare with different Score 
IMD_2019_score_pop<-IMD_2019_LSOA[grepl("Score|S_Depchi", colnames(IMD_2019_LSOA))] 
#continuous value ustilising pearson
#p-value
cor_matrix <- cor(IMD_2019_score_pop,method = "spearman")
res1 <- cor.mtest(IMD_2019_score_pop, conf.level = .95)
corrplot(cor_matrix, p.mat = res1$p, sig.level = .05,method = "number")
#[3-3]clustering analysis
cluster_ds_1 <- IMD_2019_LA[grep("IncScore|EmpScore|EduScore|HDDScore|CriScore|BHSScore|EnvScore|S_Depchi", colnames(IMD_2019_LA))]
scaled_cluster_ds_1<-scale(cluster_ds_1)

silhouette_scores <- sapply(2:10, function(k) {
  kmeans_result <- kmeans(cluster_ds_1, centers = k)
  silhouette_avg <- silhouette(kmeans_result$cluster, dist(scaled_cluster_ds_1))
  return(mean(silhouette_avg[, 3]))
})

# silhouette_scores plot
plot(2:10, 
     silhouette_scores, 
     type = "b", pch = 19, col = "blue", 
     xlab = "Number of Clusters (k)", ylab = "Silhouette Score", main = "Silhouette Method")

#clustering 
kmeans_result <- kmeans(scaled_cluster_ds_1, centers = 3)
cluster_assignments <- kmeans_result$cluster 
cluster_centers <- kmeans_result$centers
cluster_ds_1$cluster<-as.factor(cluster_assignments)

#Scater plot matrix 1
ggpairs(cluster_ds_1,               
        columns = 1:4,        
        aes(color = cluster,  
            alpha = 0.5))
#Scater plot matrix 2
ggpairs(cluster_ds_1,                 
        columns = c(5,6,7,8),        
        aes(color = cluster,  
            alpha = 0.5))     
#PCA
pca<-prcomp(IMD_2019_LA[,2:9],scale=TRUE)

IMD_2019_LA.pca<-data.frame(
LA=IMD_2019_LA$LADnm,
PC1=pca$x[,1],
PC2=pca$x[,2]
)
# sho
pca$rotation[, 1:2]
#Graph of the variables

fviz_pca_var(pca, col.var = "blue",
             repel=TRUE,
             alpha=0.5,
             title="The loading plot with PCA")

IMD_2019_LA$cluster<-kmeans_result$cluster
IMD_2019_LA$pca1<-pca$x[,1]
IMD_2019_LA$pca2<-pca$x[,2]

ggplot(IMD_2019_LA, aes(x = pca1, y = pca2, color =as.factor(cluster),shape = as.factor(cluster))) +
  geom_point(size = 4) +
  labs(title = "PCA with Cluster Assignments", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal() +
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#e31a1c"))+ # Custom color palette
  theme(panel.background = element_rect(fill = "lightgray"))


#////////[4-1]Predictive analysis\\\\\\\\\\\\ 
##Model A Variables selected by correlation matrix 
ML_dataset<-IMD_2019_LSOA %>% select(c(3:7))
#scale data 
ML_dataset[c("IncScore","EmpScore","HDDScore","CriScore")] <- scale(ML_dataset[c("IncScore","EmpScore","HDDScore","CriScore")])
#split data set into train data and test data 
sample <- sample(c(TRUE, FALSE), nrow(ML_dataset), replace=TRUE, prob=c(0.7,0.3))
IMD_Score_train  <- ML_dataset[sample, ]
IMD_Score_test   <- ML_dataset[!sample, ]
#train the model
mod_IMDScore <- lm(
formula=EduScore ~ ., 
data=ML_dataset)
#Check model detail
summary(mod_IMDScore)
#predict with test data 
predictions <- predict(mod_IMDScore, newdata = IMD_Score_test)
#model evaluation
mse <- mean((predictions - IMD_Score_test$EduScore)^2)
cat("Mean Squared Error (MSE):", mse, "\n")
rmse <- sqrt(mse)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
mae <- mean(abs(predictions - IMD_Score_test$EduScore))
cat("Mean Absolute Error (MAE):", mae, "\n")

#residual plot
plot(fitted(mod_IMDScore), residuals(mod_IMDScore), pch = 16, col = rgb(0, 0, 1, 0.3),
     xlab = "Predicted education scores ", ylab = "Residuals",
     main = "Residuals vs Fitted Values in Model A (correlation matrix)")
abline(h = 0, col = "red", lty = 2) 
lines(lowess(fitted(mod_IMDScore), residuals(mod_IMDScore)), col = "green", lwd = 2)

## Model b Use PCA
ML_dataset_2<-IMD_2019_LSOA %>% select(c(3:9,12))
ML_dataset_2_without_index<-ML_dataset_2 %>% select(-c(EduScore))
# Conducting PCA 
ML_pca<-prcomp(ML_dataset_2_without_index,scale=TRUE)
ML_dataset_pca<-data.frame(
  EduScore=ML_dataset_2$EduScore,
  PC1=ML_pca$x[,1],
  PC2=ML_pca$x[,2]
)
# Spilt data set 
sample <- sample(c(TRUE, FALSE), nrow(ML_dataset_pca), replace=TRUE, prob=c(0.7,0.3))
Score_train  <- ML_dataset_pca[sample, ]
Score_test   <- ML_dataset_pca[!sample, ]
# train model 
mod_IMDScore_pca <- lm(
  formula=EduScore ~ ., 
  data=Score_train)

# View model's detail
summary(mod_IMDScore_pca)
# predict by test data 
predictions <- predict(mod_IMDScore_pca, newdata = Score_test)
# evaluate model
mse <- mean((predictions - Score_test$EduScore)^2)
cat("Mean Squared Error (MSE):", mse, "\n")
rmse <- sqrt(mse)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
mae <- mean(abs(predictions - Score_test$EduScore))
cat("Mean Absolute Error (MAE):", mae, "\n")
# Residual plot 
plot(fitted(mod_IMDScore_pca), residuals(mod_IMDScore_pca), pch = 16, col =rgb(0, 0, 1, 0.3),
     xlab = "Predicted education scores", ylab = "Residuals",
     main = "Residuals vs Fitted Values in Model B(PCA)")
abline(h = 0, col = "red", lty = 2) 
lines(lowess(fitted(mod_IMDScore_pca), residuals(mod_IMDScore_pca)), col = "green", lwd = 2)



