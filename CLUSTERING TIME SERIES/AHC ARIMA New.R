# Load Data
library(readxl)
set.seed(123)
data <- read_excel('dm1.xlsx')

# Step 1: Preprocess the time series data (if necessary)
data <- na.omit(data)

# Calculate mean and standard deviation
mean_val <- mean(as.matrix(data[,-1]))
mean_val
sd_val <- sd(as.matrix(data[,-1]))
sd_val
head(data)

# check missing value
anyNA(data)
data <- data[,-1]

# z-score standardization
data_z <- scale(data) 
head(data_z)

# menghitung distance
data_dist <- dist(x = data_z, method = "euclidean")

# Implementasi AHC
data_hc_complete <- hclust(d = data_dist, method = "complete")
data_hc_single <- hclust(d = data_dist, method = "single")
data_hc_avg <- hclust(d = data_dist, method = "average")
set.seed(100)
data_hc_ward <- hclust(d = data_dist, method = "ward.D2")

# Dendogram untuk Complete
library(factoextra)
fviz_dend(data_hc_complete, cex = 0.5, 
          main = "Cluster Dendrogram Complete Linkage")

# cutree untuk menghasilkan cluster data
complete_clust <- cutree(data_hc_complete,  k = 4)
head(complete_clust)
table(complete_clust)
fviz_dend(data_hc_complete, k = 4, k_colors = "jco", rect = T, 
          main = "Complete Linkage Cluster")
complete_coph <- cophenetic(data_hc_complete)
cor(complete_coph, data_dist)

# Dendogram untuk Single
fviz_dend(data_hc_single, cex = 0.5, 
          main = "Cluster Dendrogram Single Linkage")
single_clust <- cutree(data_hc_single, k = 4)
head(single_clust)
table(single_clust)
fviz_dend(data_hc_single, k = 4, k_colors = "jco", rect = T, 
          main = "Single Linkage Cluster")
single_coph <- cophenetic(data_hc_single)
cor(single_coph, data_dist)

# Dendogram untuk Average
fviz_dend(data_hc_avg, cex = 0.5, main = "Cluster Dendrogram Average Linkage")
avg_coph <- cophenetic(data_hc_avg)
cor(avg_coph, data_dist)
avg_clust <- cutree(data_hc_avg, k = 4)
table(avg_clust)
fviz_dend(data_hc_avg, k = 4, k_colors = "jco", rect = T, main = "Average Linkage Cluster")

# Dendogram untuk Ward
fviz_dend(data_hc_ward, cex = 0.5, 
          main = "Cluster Dendrogram Ward's Minimum Variance")
ward_coph <- cophenetic(data_hc_ward)
cor(ward_coph, data_dist)
ward_clust <- cutree(data_hc_ward, k = 4)
table(ward_clust)
fviz_dend(data_hc_ward, k = 4, k_colors = "jco", rect = T, 
          main = "Ward's Minimum Variance Cluster")

#Mengumpulkan Hasil Clustering
library(magrittr)
df<- data.frame(complete = cor(complete_coph, data_dist),
           single = cor(single_coph, data_dist),
           average = cor(avg_coph, data_dist),
           ward = cor(ward_coph, data_dist)) %>% 
  tidyr::pivot_longer(cols = colnames(.),names_to = "method", values_to = "correlation")

clusts <- data.frame(cluster_single = single_clust, cluster_complete = complete_clust,
                 cluster_average = avg_clust, cluster_ward = ward_clust)
library(clValid)
# internal measures
internal <- clValid(data_z, nClust = 3:5, 
                    clMethods = "agnes", 
                    validation = "internal", 
                    metric = "euclidean",
                    method = "complete")
summary(internal)
# stabilitas measures
stability <- clValid(data_z, nClust = 3:4, 
                     clMethods = "agnes", 
                     validation = "stability", 
                     metric = "euclidean",
                     method = "complete")
# hanya menampilkan skor optimal
optimalScores(stability)

library(forecast)
predictions <- list()  # Create an empty list to store the predictions

for (col in names(clusts)) {
  ts_data <- ts(clusts[col], frequency = 12)  # Convert each column to a time series
  arima_model <- auto.arima(ts_data)  # Fit the ARIMA model
  forecast_values <- forecast(arima_model)  # Generate predictions for 10 future periods
  
  # Store the predictions in the list with the column name as the list element name
  predictions[[col]] <- forecast_values
}
# Melihat Model dari masing-masing cluster
predictions$cluster_single$model
predictions$cluster_complete$model
predictions$cluster_average$model
predictions$cluster_ward$model

# Melihat akurasi dari masing-masing prediksi
accuracy(predictions$cluster_single)
accuracy(predictions$cluster_complete)
accuracy(predictions$cluster_average)
accuracy(predictions$cluster_ward)


# Clustering and Forecasting for certain variable, as example Aceh
# Step 1: Install and load required packages
library(forecast)
library(fpc)

# Step 2: Prepare your data
# Assuming you have a time series dataset called 'my_data' with a single variable 'my_variable'
Aceh <- ts(data$Aceh, frequency = 12)

# Step 3: Perform AHC using Ward's method
ward_hc_result <- agnes(Aceh, method = "ward")

# Step 4: Determine the number of clusters
# Use a clustering validation index or inspect the dendrogram visually

# Step 5: Create cluster memberships
num_clusters <- 3  # Example: Assuming 3 clusters
cluster_memberships <- cutree(ward_hc_result, k = num_clusters)

# Step 6: Perform ARIMA forecasting for the selected variable
selected_variable <- Aceh  # Modify if necessary

for (i in 1:num_clusters) {
  cluster_data <- selected_variable[cluster_memberships == i]
  arima_model <- auto.arima(cluster_data)
  cluster_forecast <- forecast(arima_model)
  print(paste("Cluster", i))
  print(cluster_forecast)
}
