# Load necessary libraries
library(ggplot2)
library(reshape2)
library(corrplot)

# Set seed for reproducibility
set.seed(123)

# Create a sequence of daily dates from 01.01.1992 to 31.12.2023
dates <- seq(as.Date("1992-01-01"), as.Date("2023-12-31"), by = "day")

# Number of days
n_days <- length(dates)

# Generate dummy data
# Chlorophyll-a (random but with some seasonality)
chl_a <- 5 + 10 * sin(2 * pi * seq(1, n_days) / 365) + rnorm(n_days, 0, 2)

# Air temperature (seasonal variation with random noise)
air_temp <- 10 + 15 * sin(2 * pi * seq(1, n_days) / 365) + rnorm(n_days, 0, 5)

# Lake Surface Water Temperature (seasonally correlated with air temp)
lswt <- 8 + 12 * sin(2 * pi * seq(1, n_days) / 365) + rnorm(n_days, 0, 3)

# Wind speed (random with small daily variation)
wind_speed <- abs(rnorm(n_days, mean = 5, sd = 2))

# Short-wave radiation (seasonal pattern)
sw_radiation <- abs(200 + 400 * sin(2 * pi * seq(1, n_days) / 365) + rnorm(n_days, 0, 50))

# Precipitation (random with occasional high peaks)
precipitation <- abs(rnorm(n_days, mean = 2, sd = 5))

# Cloud cover (random daily variation, but higher in winter)
cloud_cover <- abs(50 + 30 * sin(2 * pi * seq(1, n_days) / 365) + rnorm(n_days, 0, 15))

# Humidity (higher in winter, lower in summer)
humidity <- abs(70 + 20 * cos(2 * pi * seq(1, n_days) / 365) + rnorm(n_days, 0, 10))

# Combine all variables into a data frame
data <- data.frame(Date = dates, chl_a, air_temp, lswt, wind_speed, sw_radiation, precipitation, cloud_cover, humidity)

# Drop the date column for correlation
data_numeric <- data[, -1]

# Create the correlation matrix
cor_matrix <- cor(data_numeric, use = "complete.obs")

# Set the upper triangle and the diagonal of the matrix to NA
cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA

# Melt the correlation matrix (including only the lower triangle without diagonal)
cor_melt <- melt(cor_matrix, na.rm = TRUE)

# Create the correlation heatmap for the lower triangle without diagonal
ggplot(data = cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "black", size = 0.5) +  # Add black borders to the tiles to simulate gridlines
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 3, vjust = 0.5, hjust = 0.5) +  # Ensure labels are centered
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(vjust = 1, hjust = 1),
    panel.grid = element_blank(),  # Remove the default background grid
    axis.ticks = element_blank()   # Remove axis ticks for a cleaner look
  ) +
  coord_fixed() +  # Ensure tiles are perfectly square
  labs(title = "Correlation Heatmap (Lower Triangle, No Diagonal)", x = "", y = "")
