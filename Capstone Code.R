# Analysis and Visualizations Code
# Hannah Chiou

#----------- LIBRARIES -----------#
library(dplyr)
library(tidyverse)
library(readxl)
library(jpeg)
library(ggmap)
library(geosphere)
library(usdm)
library(car)
library(MASS)
library(leaps)
library(caret)
library(Metrics)
library(rplot)
library(rplot.plot)
library(car)

#----------- MERGING AND CLEANING -----------#
tripdata <- read_csv("Downloads/202301-bluebikes-tripdata.csv")
tripdata <- tripdata %>%
  mutate(across(c(`start station id`, `end station id`), as.character))
stationdata <- read_excel("Downloads/Bluebikes_Station_List.xlsx", skip = 1)
boston_stations <- stationdata %>% 
  filter(Municipality=='Boston')

# Merge on start and end station ID 
# Include new columns:
# -> Municipality of start and end location 
# -> Total docks of start and end location

# Create a version of stationdata for start stations
start_stationdata <- stationdata %>%
  rename(`start station id` = `Station ID (to match to historic system data)`,
         start_total_docks = `Total Docks`) %>%
  select(`start station id`, start_total_docks) %>%
  mutate(`start station id` = as.character(`start station id`))  # Convert to character

# Create a version of stationdata for end stations
end_stationdata <- stationdata %>%
  rename(`end station id` = `Station ID (to match to historic system data)`,
         end_total_docks = `Total Docks`) %>%
  select(`end station id`, end_total_docks) %>%
  mutate(`end station id` = as.character(`end station id`))

# Merge with tripdata to get start station details
merged_data <- left_join(tripdata, start_stationdata, by = "start station id",relationship='many-to-many')
print(merged_data,n=10,width=Inf)

# Merge data again to get end station details
merged_data <- left_join(merged_data, end_stationdata, by = "end station id", relationship='many-to-many')
print(merged_data,n=10,width=Inf)

# Merge to zipcodes, removing zipcodes in trip data that are missing / not included in our zipcode dataset
merged_data <- merged_data %>%
  filter(!is.na(`postal code`) & `postal code` != "")

zipcodes <- read_csv('Downloads/Boston_Neighborhoods_Zipcodes.csv')

merged_data <- merged_data %>%
  left_join(zipcodes, by = c("postal code" = "Zip Code"),relationship='many-to-many')

# Getting distinct rides
merged_data_cleaned <- merged_data %>%
  distinct(`start station id`, `end station id`, Neighborhood, `tripduration`, .keep_all = TRUE)

#----------- REVERSE GEOCODING AND AGGREGATING -----------#
# Zipcodes of beginning and ending stations
merged_data <- merged_data %>%
  reverse_geocode(lat = `start station latitude`, 
                  long = `start station longitude`, 
                  method = "osm") %>%
  mutate(`start station zip code` = str_extract(address, "\\d{5}")) %>%
  select(-address) 

merged_data <- merged_data %>%
  reverse_geocode(lat = `end station latitude`, 
                  long = `end station longitude`, 
                  method = "osm") %>%
  mutate(`end station zip code` = str_extract(address, "\\d{5}")) %>%
  select(-address) 

# Joining to zipcodes to get neighborhood
merged_data <- merged_data %>%
  select( -`end station neighborhood`)

zipcodes <- zipcodes %>%
  rename(`zip_code` = `Zip Code`)

zipcodes <- zipcodes %>%
  distinct(zip_code, .keep_all = TRUE)


merged_data <- merged_data %>%
  left_join(zipcodes, by = c("start station zip code" = "zip_code")) %>%
  rename(`start station neighborhood` = Neighborhood)

merged_data <- merged_data %>%
  left_join(zipcodes, by = c("end station zip code" = "zip_code")) %>%
  rename(`end station neighborhood` = Neighborhood)

merged_data <- merged_data %>%
  distinct(`start station id`, `end station id`, starttime, stoptime, .keep_all = TRUE)

# Adding same neighborhood variable
merged_data <- merged_data %>%
  mutate(same_neighborhood = ifelse(`start station neighborhood` == `end station neighborhood`, "Yes", "No"))

merged_data_clean <- merged_data %>%
  filter(`start station zip code` %in% zipcodes$zip_code &
           `end station zip code` %in% zipcodes$zip_code)

# Calculating number of docks and stations in each neighborhood
# 1: Use reverse_geocode to get the zip code for each station
boston_stations <- boston_stations %>%
  reverse_geocode(lat = Lat, long = Long, method = "osm") %>%
  mutate(station_zip_code = str_extract(address, "\\d{5}")) %>%  # Extract the zip code from the address
  select(-address)  # Remove the full address column

# 2: Rename the zip code column to match the zipcodes dataset
boston_stations <- boston_stations %>%
  rename(zip_code = station_zip_code)

# 3: Merge with the zipcodes dataset to get the neighborhood
boston_stations <- boston_stations %>%
  left_join(zipcodes, by = "zip_code") %>%
  rename(station_neighborhood = Neighborhood)

# View the resulting boston_stations data (optional)
head(boston_stations)

# Summing up the total docks by neighborhood
total_docks_by_neighborhood <- boston_stations %>%
  group_by(station_neighborhood) %>%
  summarise(total_docks = sum(`Total Docks`, na.rm = TRUE)) %>%
  arrange(desc(total_docks)) 

# Merge total_docks_by_neighborhood with merged_data for start neighborhood
merged_data_clean <- merged_data_clean %>%
  left_join(total_docks_by_neighborhood, by = c(`start station neighborhood` = "station_neighborhood")) %>%
  rename(start_neighborhood_docks = total_docks)

# Merge total_docks_by_neighborhood with merged_data for end neighborhood
merged_data_clean <- merged_data_clean %>%
  left_join(total_docks_by_neighborhood, by = c(`end station neighborhood` = "station_neighborhood")) %>%
  rename(end_neighborhood_docks = total_docks)

merged_data_no_duplicates <- merged_data_clean %>%
  distinct()

# Count the total number of stations in each neighborhood using unique 'Number' or 'NAME'
station_count_by_neighborhood <- boston_stations %>%
  group_by(station_neighborhood) %>%
  summarise(total_stations = n_distinct(Number)) 

# Merge station counts for the start neighborhood
merged_data_no_duplicates <- merged_data_no_duplicates %>%
  left_join(station_count_by_neighborhood, by = c("start station neighborhood" = "station_neighborhood")) %>%
  rename(start_neighborhood_stations = total_stations)

# Merge station counts for the end neighborhood
merged_data_no_duplicates <- merged_data_no_duplicates %>%
  left_join(station_count_by_neighborhood, by = c("end station neighborhood" = "station_neighborhood")) %>%
  rename(end_neighborhood_stations = total_stations)

# Calculating trip distance
merged_data_no_duplicates <- merged_data_no_duplicates %>%
  mutate(
    distance_meters = distHaversine(
      cbind(`start station longitude`, `start station latitude`),
      cbind(`end station longitude`, `end station latitude`)
    ),
    distance_km = distance_meters / 1000
  )

#----------- VIFSTEP -----------#
vifstep(merged_data_no_duplicates[,c("start_neighborhood_docks","end_neighborhood_docks","start_neighborhood_stations","end_neighborhood_stations", "distance_meters")], th=10)

numeric_vars <- c("start_neighborhood_docks", "end_neighborhood_docks",
                  "start_neighborhood_stations", "end_neighborhood_stations", 
                  "distance_meters")

clean_data <- as.data.frame(merged_data_no_duplicates[, numeric_vars])

vifstep(clean_data, th = 10)

model_data <- merged_data_no_duplicates %>%
  mutate(
    start_neighborhood = as.factor(`start station neighborhood`),
    end_neighborhood = as.factor(`end station neighborhood`),
    same_neighborhood = as.factor(same_neighborhood)
  )

lm_model <- lm(tripduration ~ 
                 start_neighborhood_stations +
                 end_neighborhood_stations +
                 distance_meters +
                 same_neighborhood,               
               data = model_data)

vif(lm_model)

#----------- ANALYSIS AND VISUALIZATIONS -----------#

# Stepwise Regression
library(MASS)
fullmodel <- lm(log_tripduration~start_neighborhood_stations+end_neighborhood_stations+distance_km+same_neighborhood,data=model_data)
stepwise_aic <- step(fullmodel, direction = "both", k = 2)
stepwise_bic <- step(fullmodel, direction = "both", k = log(nrow(model_data)))

# Subset Selection (AIC and BIC)
library(leaps)
subset_selection <- regsubsets(log_tripduration ~ 
                                 start_neighborhood_stations + 
                                 end_neighborhood_stations + 
                                 distance_km + 
                                 same_neighborhood,
                               data = model_data)

# Get summary of the regsubsets object
subset_summary <- summary(subset_selection)

# Extract the best model
aic_model <- which.min(subset_summary$aic)
bic_model <- which.min(subset_summary$bic)

# Retrieve the selected variables
selected_aic_vars <- names(subset_summary$which[aic_model, ][subset_summary$which[aic_model, ] == TRUE])
selected_bic_vars <- names(subset_summary$which[bic_model, ][subset_summary$which[bic_model, ] == TRUE])

# Create formulas for the AIC and BIC models
aic_formula <- as.formula(paste("log_tripduration ~", paste(selected_aic_vars, collapse = " + ")))
bic_formula <- as.formula(paste("log_tripduration ~", paste(selected_bic_vars, collapse = " + ")))

# Print out the formulas for verification
print(aic_formula)
print(bic_formula)
# exclude AIC variable

# Cross validation
library(caret)
library(Metrics)

# Cross-validation function
cross_validate <- function(model_formula, data, k_folds = 10) {
  # Set up the cross-validation method
  train_control <- trainControl(method = "cv", number = k_folds, returnResamp = "all")
  model_cv <- train(model_formula, data = data, method = "lm", trControl = train_control)
  # Return RMSE from the cross-validation results
  cv_rmse <- mean(model_cv$resample$RMSE)
  return(cv_rmse)
}

stepwise_aic_formula <- as.formula("log_tripduration ~ end_neighborhood_stations + distance_km + same_neighborhood")
stepwise_bic_formula <- as.formula("log_tripduration ~ distance_km + same_neighborhood")
subset_bic <- as.formula("log_tripduration ~ distance_km + same_neighborhood")
tree <- as.formula("log_tripduration ~ distance_km")

cv_rmse_aic <- cross_validate(stepwise_aic_formula, model_data)
cv_rmse_bic <- cross_validate(stepwise_bic_formula, model_data)
cv_rmse_bic_subset <- cross_validate(subset_bic, model_data)
cv_tree <- cross_validate(tree, model_data)

cat("CV RMSE for Stepwise AIC Model: ", cv_rmse_aic, "\n")
cat("CV RMSE for Stepwise BIC Model: ", cv_rmse_bic, "\n")
cat("CV RMSE for BIC Subset Model: ", cv_rmse_bic_subset, "\n")
cat("CV RMSE for Tree Model: ", cv_tree, "\n")

cv_results <- data.frame(
  Model = c("Stepwise AIC", "Stepwise BIC", "BIC Subset", "Tree Model"),
  CV_RMSE = c(cv_rmse_aic, cv_rmse_bic, cv_rmse_bic_subset, cv_tree)
)

# Print the table
print(cv_results)

# Regression tree model
library(rpart)
tree_model <- rpart(log_tripduration ~ start_neighborhood_stations + 
                      end_neighborhood_stations + 
                      distance_km + 
                      same_neighborhood +
                      start_neighborhood +
                      end_neighborhood, 
                    data = model_data, 
                    method = "anova")
summary(tree_model)


library(rpart.plot)
rpart.plot(tree_model)

# Visualizations
library(car)
# For the stepwise AIC model
stepwise_aic_model <- lm(stepwise_aic_formula, data = model_data)

# For the stepwise BIC model
stepwise_bic_model <- lm(stepwise_bic_formula, data = model_data)

# For the subset BIC model
subset_bic_model <- lm(subset_bic, data = model_data)

tree_model <- lm(tree, data=model_data)

avPlots(stepwise_aic_model, ask = FALSE, main = "Fig 02: Stepwise AIC Model")
avPlots(stepwise_bic_model, ask = FALSE, main = "Stepwise BIC Model / Subset BIC Model")

# Tree plot
tree_plot <- ggplot(model_data, aes(x = distance_km, y = log_tripduration)) +
  geom_point(color = "darkgrey", size = 2) +  
  geom_line(aes(x = distance_km, y = pred_tree), color = "blue", linewidth = 1) +  # Line for predictions
  labs(title = "Fig 03: Decision Tree Model (Logged Trip Duration vs. Distance)", x = "Distance (km)", y = "log_tripduration") +
  theme_minimal() +
  guides(color = "none")  # Remove the legend

# Display the plot
print(tree_plot)


