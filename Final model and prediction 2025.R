# Load necessary libraries
library(dplyr)
library(readr)
library(tidyr)

# Load dataset
file_path <- "5cities_Weather_Summary_2025_final.csv"
df <- read_csv(file_path)

# Define Ensemble Model for Predicting DOY
predict_doy_ensemble <- function(data) {
  return(125.973 + 
           (-0.068 * data$GDD5_cum) + 
           (-2.948 * data$TMAX) + 
           (-0.468 * data$CODE0) + 
           (-0.37 * data$CODE2) + 
           (0.543 * data$CODE63) + 
           (-0.758 * data$WIND) + 
           (9.223 * data$EVPR) + 
           (0.213 * data$CODE51))
}

# List of cities for ensemble prediction (with updated names)
ensemble_cities <- c("Kyoto", "Liestal", "Washington", "Vancouver", "NewYorkCity")

# Filter dataset for the selected cities
df_filtered <- df %>% filter(City %in% ensemble_cities)

# Apply Ensemble Model to Predict DOY
df_filtered$Predicted_DOY <- predict_doy_ensemble(df_filtered)

# Reshape data to required format
df_reshaped <- df_filtered %>%
  select(Year, City, Predicted_DOY) %>%
  pivot_wider(names_from = City, values_from = Predicted_DOY) %>%
  rename(WashingtonDC = Washington, NewYorkCity = NewYorkCity)  # Adjust city names

# Round values to 2 decimal places
df_reshaped <- df_reshaped %>% mutate(across(where(is.numeric), ~ round(.x, 2)))

# Print results
print(df_reshaped)

# Save results as CSV file with 2 decimal places
output_file <- "Predicted_DOY_Formatted_2025.csv"
write.csv(df_reshaped, output_file, row.names = FALSE)

# Completion message
cat("Results saved to:", output_file, "\n")
