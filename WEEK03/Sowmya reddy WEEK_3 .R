# Load required libraries
library(readxl)
library(dplyr)

# Read the Excel file
clinic_data <- read_excel("clinics.xls")

# Convert latitude and longitude to numeric
clinic_data$locLat <- as.numeric(clinic_data$locLat)
clinic_data$locLong <- as.numeric(clinic_data$locLong)

# Define the Haversine function
haversine <- function(lat1, lon1, lat2, lon2) {
  MILES <- 3959
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  
  return(MILES * c)
}

# Method 1: For loop approach
for_loop_distance <- function(data) {
  distances <- numeric(nrow(data))
  for(i in 1:nrow(data)) {
    distances[i] <- haversine(40.671, -73.985, 
                              data$locLat[i], 
                              data$locLong[i])
  }
  return(distances)
}

# Method 2: Apply approach
apply_distance <- function(data) {
  apply(data[, c("locLat", "locLong")], 1, 
        function(x) haversine(40.671, -73.985, x[1], x[2]))
}

# Method 3: Vectorized approach
vectorized_distance <- function(data) {
  lat1 <- rep(40.671 * pi / 180, nrow(data))
  lon1 <- rep(-73.985 * pi / 180, nrow(data))
  lat2 <- data$locLat * pi / 180
  lon2 <- data$locLong * pi / 180
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  
  return(3959 * c)
}

# Time the for loop approach
start_time <- Sys.time()
result1 <- for_loop_distance(clinic_data)
end_time <- Sys.time()
for_loop_time <- difftime(end_time, start_time, units = "secs")
cat("For loop time:", for_loop_time, "seconds\n")

# Time the apply approach
start_time <- Sys.time()
result2 <- apply_distance(clinic_data)
end_time <- Sys.time()
apply_time <- difftime(end_time, start_time, units = "secs")
cat("Apply time:", apply_time, "seconds\n")

# Time the vectorized approach
start_time <- Sys.time()
result3 <- vectorized_distance(clinic_data)
end_time <- Sys.time()
vectorized_time <- difftime(end_time, start_time, units = "secs")
cat("Vectorized time:", vectorized_time, "seconds\n")

# Create a data frame with the timing results
timing_results <- data.frame(
  Method = c("For Loop", "Apply", "Vectorized"),
  Time_Seconds = c(for_loop_time, apply_time, vectorized_time)
)

# Print the timing results
print(timing_results)