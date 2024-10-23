# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Step 1: Create Dummy Data
set.seed(123)  # For reproducibility
dates <- seq.Date(from = as.Date("1992-01-01"), to = as.Date("2023-12-31"), by = "day")
n <- length(dates)

# Generate random data for climatic variables
data <- data.frame(
  date = dates,
  chl_a = rnorm(n, mean = 10, sd = 5),          # Chl-a concentrations
  air_temp = rnorm(n, mean = 20, sd = 5),      # Air temperature
  lswt = rnorm(n, mean = 15, sd = 5),          # Lake surface water temperature
  wind_speed = rnorm(n, mean = 5, sd = 2),     # Wind speed
  sw_radiation = rnorm(n, mean = 300, sd = 50), # Shortwave radiation
  precipitation = rnorm(n, mean = 3, sd = 1),  # Precipitation
  cloud_cover = rnorm(n, mean = 50, sd = 10),  # Cloud cover
  humidity = rnorm(n, mean = 70, sd = 15)       # Humidity
)

# Step 2: Perform PCA
# Standardize the data by removing the date column
data_numeric <- data %>% select(-date)

# Run PCA
pca_result <- prcomp(data_numeric, center = TRUE, scale. = TRUE)

# Print PCA summary
summary(pca_result)

# Create a data frame for PCA results
pca_df <- as.data.frame(pca_result$x)

# Step 3: Create a Scree Plot
# Calculate the variance explained by each principal component
scree_data <- data.frame(
  PC = paste0("PC", 1:length(pca_result$sdev)),
  Variance = (pca_result$sdev^2) / sum(pca_result$sdev^2)  # Proportion of variance explained
)

# Scree Plot
scree_plot <- ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(group = 1), color = "red", size = 1) +
  geom_point(size = 2, color = "red") +
  labs(title = "Scree Plot", x = "Principal Components", y = "Proportion of Variance Explained") +
  theme_minimal()

print(scree_plot)

# Step 4: Rename the PCs
# Get variable names for PCs based on their contributions
loadings <- abs(pca_result$rotation)  # Get absolute values of loadings
variable_names <- colnames(data_numeric)

# Initialize a vector to store PC names
pc_names <- vector("character", ncol(loadings))
used_variables <- c()  # Track which variables have already been used

# Loop to create unique PC names
for (i in 1:ncol(loadings)) {
  # Find the index of the variable with the maximum loading for the current PC
  var_contrib <- which.max(loadings[, i])  # Get index of the variable contributing most to the PC
  var_name <- variable_names[var_contrib]
  
  # Ensure uniqueness: if the variable has already been used, find the next highest loading variable
  while (var_name %in% used_variables) {
    loadings[var_contrib, i] <- -Inf  # Exclude the current variable by setting its loading to negative infinity
    var_contrib <- which.max(loadings[, i])  # Re-calculate the variable with max loading
    var_name <- variable_names[var_contrib]
  }
  
  # Record the variable name and mark it as used
  used_variables <- c(used_variables, var_name)
  pc_names[i] <- paste0("PC", i, " (", var_name, ")")  # Create name
}

# Rename the PCs in the PCA results
colnames(pca_df) <- pc_names

# Plot PCA results using aes() for tidy evaluation
pca_plot <- ggplot(pca_df, aes(x = .data[[pc_names[1]]], y = .data[[pc_names[2]]])) +  # Use the new PC names
  geom_point(alpha = 0.5) +
  labs(title = "PCA of Climatic Variables", x = pc_names[1], y = pc_names[2]) +
  theme_minimal()

print(pca_plot)



