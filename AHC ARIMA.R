# Install and load the necessary packages
library(dplyr)
library(TSclust)
library(forecast)
library(readxl)

# Step 0: Prepare your time series data
# Assuming your time series data is stored in a matrix called 'data'

# Generate example data
set.seed(123)
data <- read_excel('dm1.xlsx')

# Step 1: Preprocess the time series data (if necessary)
data <- na.omit(data)
data <- data[,-1]
data <- as.matrix(data)
# Calculate mean and standard deviation
mean_val <- mean(data)
sd_val <- sd(data)

# Normalize data using Z-score
normalize_data <- as.data.frame(scale(data))

# Print the original and normalized data
print("Original Data:")
print(data)

print("Normalized Data:")
print(normalize_data)


# Step 2: Perform hierarchical clustering
dist_matrix <- as.matrix(normalize_data)
hc_single <- agnes(dist_matrix, method = "single")
hc_complete <- agnes(dist_matrix, method = "complete")
hc_average <- agnes(dist_matrix, method = "average")
hc_ward <- agnes(dist_matrix, method = "ward")

# Step 3: Visualize the dendrograms
par(mfrow = c(2, 2))
plot(hc_single, main = "Single Linkage")
plot(hc_complete, main = "Complete Linkage")
plot(hc_average, main = "Average Linkage")
plot(hc_ward, main = "Ward's Linkage")

# Melihat cophenetic coefficient
hc_single_coph <- cophenetic(hc_single)

# Step 4: Choose the desired number of clusters
cluster_labels_single <- cutree(hc_single, k = 3)
cluster_labels_complete <- cutree(hc_complete, k = 3)
cluster_labels_average <- cutree(hc_average, k = 3)
cluster_labels_ward <- cutree(hc_ward, k = 3)

# Step 5: Perform ARIMA forecasting within each cluster
df <- data.frame(value = normalize_data, cluster_single = cluster_labels_single, cluster_complete = cluster_labels_complete,
                 cluster_average = cluster_labels_average, cluster_ward = cluster_labels_ward)

unique_clusters_single <- unique(cluster_labels_single)
unique_clusters_complete <- unique(cluster_labels_complete)
unique_clusters_average <- unique(cluster_labels_average)
unique_clusters_ward <- unique(cluster_labels_ward)

# Iterate single linkage over each column
Single <- for (col_name in colnames(df)) {
  for (cluster in unique_clusters_single) {
    cluster_data <- df[df$cluster_single == cluster, col_name]
    arima_model <- auto.arima(cluster_data)
    forecast_result <- forecast(arima_model, h = 10)  # Forecasting 10 steps ahead
    print(forecast_result)
  }
}# Iterate complete linkage over each column
Complete <- for (col_name in colnames(df)) {
  for (cluster in unique_clusters_complete) {
    cluster_data <- df[df$cluster_complete == cluster, col_name]
    arima_model <- auto.arima(cluster_data)
    forecast_result <- forecast(arima_model, h = 10)  # Forecasting 10 steps ahead
    print(forecast_result)
  }
}# Iterate average linkage over each column
Average <- for (col_name in colnames(df)) {
  for (cluster in unique_clusters_average) {
    cluster_data <- df[df$cluster_average == cluster, col_name]
    arima_model <- auto.arima(cluster_data)
    forecast_result <- forecast(arima_model, h = 10)  # Forecasting 10 steps ahead
    print(forecast_result)
  }
}
# Iterate ward linkage over each column

Ward <- for (col_name in colnames(df)) {
  for (cluster in unique_clusters_ward) {
    cluster_data <- df[df$cluster_ward == cluster, col_name]
    arima_model <- auto.arima(cluster_data)
    forecast_result <- forecast(arima_model, h = 10)  # Forecasting 10 steps ahead
    print(forecast_result)
  }
}

cluster_data <- df$cluster_ward
arima_model <- auto.arima(cluster_data)
forecast_result <- forecast(arima_model, h = 10)  # Forecasting 10 steps ahead
print(forecast_result)
