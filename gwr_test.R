# Load necessary libraries
library(spgwr) # For gwr.sel function
library(GWmodel) # For gwr function
library(terra) # For handling raster data.
library(sf) # For handling spatial vector data.
library(gstat) # For Kriging
library(tmap) # For map plotting
library(Metrics) # For models evaluation
library(ggplot2) # For visualization
library(automap) # For automatic variogram fitting and kriging
library(dplyr) # For data manipulation
library(ggspatial) # For map annotation

## 1. Data preparation ##
# Read the data file
soil_data <- read.csv("data/soil_data_gp.csv")

# Converts the data to a spatial data frame with coordinates and specific CRS.
sf_soil <- st_as_sf(soil_data, coords = c("x", "y"), crs = 5070)

# Loads a polygon shapefile that will be used for masking the prediction results.
soil_polygon <- st_read("data/polygon.gpkg")

# Verify the Struct of the data
print(str(sf_soil))

## 2. Selecting optimal bandwidth ##
# Extract coordinates for gwr.sel
coords <- st_coordinates(sf_soil)

# Names of the predictors from own data without geometry and response variable
raster_vars <- names(sf_soil)[!names(sf_soil) %in% c("SOC", "geometry")]

# Generate formula for GWR
predictors <- paste(raster_vars, collapse = " + ")
formula <- as.formula(paste("SOC ~", predictors))

# Select the optimal bandwidth for the GWR model using spgwr package
optimal_bandwidth <- gwr.sel(
    formula, data = sf_soil,
    gweight = gwr.Gauss,
    coords = coords,
    method = "aic",
    adapt = FALSE,
    verbose = FALSE
)

# Select the optimal bandwidth for the GWR model using GWmodel package
bw_gwr <- bw.gwr(formula,
                 data = sf_soil,
                 approach = "AICc",
                 kernel = "gaussian",
                 adaptive = FALSE
)

## 3. Fit GWR model ##
# Fit the GWR model using the optimal bandwidth
gwr_model <- gwr(
    formula,
    data = sf_soil,
    coords = coords,
    bandwidth = bw_gwr,
    gweight = gwr.Gauss,
    hatmatrix = TRUE
)

print(gwr_model)

# Extract the fitted, residuals and local R-squared values
gwr_results <- data.frame(
    x = coords[,1],
    y = coords[,2],
    observed = sf_soil$SOC,
    fitted = gwr_model$SDF$pred,
    residuals = gwr_model$SDF$gwr.e,
    localR2 = gwr_model$SDF$localR2
)

# Save the GWR results to a CSV file
write.csv(gwr_results, "outputs/gwr_results.csv", row.names = FALSE)

# Plot the GWR results using tmap
# Convert the data frame to an sf object for tmap
gwr_sf <- st_as_sf(gwr_results, coords = c("x", "y"), crs = st_crs(soil_polygon))

tmap_mode("plot")

# Create an map with contrast colors
r2_map <- tm_shape(soil_polygon) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_shape(gwr_sf) +
  tm_symbols(
    col = "localR2",
    palette = "RdYlBu",
    contrast = c(0.1, 1.0),    # Maximum contrast
    n = 7,
    size = "localR2",
    sizes.legend = c(0.3, 0.5, 0.7, 0.9),
    title.col = "Local R²",
    title.size = "Local R²",
    border.col = "black",      # Black borders for definition
    border.lwd = 0.7,          # Thicker borders
    alpha = 1.0                # Full opacity
  ) +
  tm_layout(
    main.title = "Spatial Distribution of Local R² Values",
    main.title.position = "center",
    main.title.size = 1.2,
    main.title.fontface = "bold",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = TRUE,
    bg.color = "white"
  ) +
  tm_compass(position = c("left", "top")) +
  tm_scale_bar(position = c("right", "bottom"))

# Save the alternative maps
tmap_save(r2_map,
          filename = "plots/r2_tmap_contrast.png",
          width = 10,
          height = 8,
          dpi = 300)

## 4. Model evaluation ##
# Create a scatter plot of observed vs predicted values
# Scatter plot with 1:1 line and sized points
residuals_plot <- ggplot(gwr_results, aes(x = observed, y = fitted)) +
  geom_point(aes(size = abs(residuals)), alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_size_continuous(name = "Absolute Residual", range = c(2, 10)) +
  labs(
    title = "Observed vs Predicted Values",
    x = "Observed SOC",
    y = "Predicted SOC"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12),
    legend.position = "right"
  )

# Save the plot
ggsave("plots/residuals_plot_sizes.png", residuals_plot, width = 8, height = 6, dpi = 300)

# Create a histogram of residuals with enhanced visualization
residuals_hist <- ggplot(gwr_results, aes(x = residuals)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(residuals)), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = 0), color = "darkgreen", linetype = "solid", size = 0.8) +
  annotate("text", x = mean(gwr_results$residuals) + 1, y = max(table(cut(gwr_results$residuals, breaks = 30))) * 0.9,
           label = paste("Mean =", round(mean(gwr_results$residuals), 2)), color = "red") +
  scale_fill_gradient2(low = "blue", mid = "lightblue", high = "red", midpoint = 0) +
  labs(
    title = "Distribution of Residuals",
    subtitle = paste("Mean:", round(mean(gwr_results$residuals), 2), "| SD:", round(sd(gwr_results$residuals), 2)),
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "darkgrey"),
    text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )
# Save the histogram
ggsave("plots/residuals_histogram.png", residuals_hist, width = 8, height = 6, dpi = 300)

# Calculate RMSE
rmse_value <- rmse(sf_soil$SOC, gwr_results$fitted)

# Calculate MAE
mae_value <- mae(sf_soil$SOC, gwr_results$fitted)

# Calculate R2 manually
ss_res <- sum((sf_soil$SOC - gwr_results$fitted)^2)
ss_tot <- sum((sf_soil$SOC - mean(sf_soil$SOC))^2)
r2_value <- 1 - (ss_res / ss_tot)

# Create a dataframe with metrics
metrics_df <- data.frame(
  Metric = c("RMSE", "MAE", "R2"),
  Value = c(rmse_value, mae_value, r2_value)
)

# Print metrics
print("Model Performance Metrics:")
print(metrics_df)

# Export metrics to CSV
write.csv(metrics_df, "outputs/model_metrics.csv", row.names = FALSE)

## 5. Predictions grid values ##
# Load raster covariates
raster_files <- list.files("data/", pattern = "\\.tif$", full.names = TRUE)
raster_names <- tools::file_path_sans_ext(basename(raster_files))
raster_list <- lapply(raster_files, rast)
names(raster_list) <- raster_names

# Extract coordinates from the raster grid from raster_list
raster_coords <- as.data.frame(xyFromCell(raster_list[[1]], 1:ncell(raster_list[[1]])))

# Fit the GWR model to the raster grid points
g_soc <- gwr(
    formula,
    data = sf_soil,
    coords = coords,
    bandwidth = bw_gwr,
    gweight = gwr.Gauss,
    fit.points = as.matrix(raster_coords)
)

print(g_soc)

# Export coefficients to raster files
# Create lists to store the rasters of coefficients
coef_rasters <- list()
masked_coef_rasters <- list()  # Add this list to store masked rasters

# Iterate over the raster variables - combined loop for efficiency
for (i in seq_along(raster_vars)) {
    # Create an empty raster with the same properties as the input rasters
    coef_raster <- rast(raster_list[[1]])
    # Get the coefficient values for this variable
    coef_values <- g_soc$SDF[[paste0(raster_vars[i])]]
    # Assign the coefficient values to the raster
    values(coef_raster) <- coef_values
    # Store the coefficient raster in the list with the variable name
    coef_rasters[[raster_vars[i]]] <- coef_raster
    # Export the coefficient raster as TIFF
    writeRaster(coef_raster,
                filename = file.path("outputs", paste0("coef_", raster_vars[i], ".tif")),
                overwrite = TRUE)

    # Immediately crop and mask the raster in the same iteration
    masked_raster <- mask(crop(coef_raster, soil_polygon), soil_polygon)
    # Store the masked raster in the list with the variable name
    masked_coef_rasters[[raster_vars[i]]] <- masked_raster
    # Export the masked raster
    writeRaster(masked_raster,
                filename = file.path("outputs", paste0("masked_coef_", raster_vars[i], ".tif")),
                overwrite = TRUE)
}

# Sort the names of coef_rasters alphabetically
coef_rasters <- coef_rasters[sort(names(coef_rasters))]
# Sort the names of masked_coef_rasters alphabetically
masked_coef_rasters <- masked_coef_rasters[sort(names(masked_coef_rasters))]

# Print the names of coefficient rasters to verify
print("Coefficient raster names:")
print(names(coef_rasters))

# Plot all masked coeffients in one step
par(mfrow = c(3, 3))  # 3x3 grid for 9 variables
for (i in 1:length(masked_coef_rasters)) {
    plot(masked_coef_rasters[[i]], main = paste("Masked Coefficients for", names(masked_coef_rasters)[i]))
}

# Extract the intercept values
intercept_values <- g_soc$SDF$'(Intercept)'

# Create a raster object with the same dimensions and extent as the original raster
intercept_raster <- rast(raster_list[[1]])
values(intercept_raster) <- intercept_values

# Export the intercept raster as TIFF
writeRaster(intercept_raster,
            filename = file.path("outputs", "intercept.tif"),
            overwrite = TRUE)

# Immediately crop and mask the intercept raster
masked_intercept <- mask(crop(intercept_raster, soil_polygon), soil_polygon)

# Export the masked intercept raster
writeRaster(masked_intercept,
            filename = file.path("outputs", "masked_intercept.tif"),
            overwrite = TRUE)

# Plot the intercept raster
plot(masked_intercept, main="Intercept")

# Create and calculate the prediction raster in one step
# Initialize prediction raster with intercept values
prediction_raster <- masked_intercept
prediction_values <- values(masked_intercept)

# Add the contribution of each variable in one loop
for (var in raster_vars) {
    prediction_values <- prediction_values + values(masked_coef_rasters[[var]]) * values(raster_list[[var]])
}

# Assign the calculated values to the prediction raster
values(prediction_raster) <- prediction_values
names(prediction_raster) <- "SOC"

# Export the prediction raster as TIFF
writeRaster(prediction_raster,
            filename = file.path("outputs", "prediction_gwr.tif"),
            overwrite = TRUE)

## 6. Residual kriging interpolation ##
# Create a new data frame for residuals
residuals_df <- data.frame(
    x = coords[,1],
    y = coords[,2],
    residuals = gwr_model$SDF$gwr.e
)

# Convert the data frame to an sf object
residuals_sf <- st_as_sf(residuals_df, coords = c("x", "y"), crs = st_crs(soil_polygon))

# Calculate experimental variogram and create visualization
variogram_model <- variogram(residuals ~ 1, data = residuals_sf)

# Create ggplot visualization of the experimental variogram
variogram_plot <- ggplot(data = as.data.frame(variogram_model),
                        aes(x = dist, y = gamma)) +
  geom_point(size = 3, shape = 21, fill = "lightblue", color = "black") +
  geom_line(linetype = "dashed", color = "grey50") +
  labs(x = "Distance (m)",
       y = "Semivariance",
       title = "Experimental Variogram of GWR Residuals",
       subtitle = "Spatial dependence structure of model residuals") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# Save the plot
ggsave(file.path("plots", "experimental_variogram.png"),
       variogram_plot,
       width = 8,
       height = 6,
       dpi = 300)


# Automatic variogram fitting using automap
auto_variogram <- autofitVariogram(residuals ~ 1, residuals_sf)

# Plot the automatically fitted variogram
plot(auto_variogram)

# Save the fitted variogram plot
png(file.path("plots", "automatic_variogram.png"),
    width = 8, height = 6, units = "in", res = 300)
plot(auto_variogram)
dev.off()

# Perform automatic kriging
raster_coords_sf <- st_as_sf(raster_coords, coords = c("x", "y"), crs = st_crs(residuals_sf))
kriging_auto <- autoKrige(residuals ~ 1,
                         input_data = residuals_sf,
                         new_data = raster_coords_sf)

# Create kriging results raster
# Create a template raster with the same properties as the prediction raster
template_rast <- rast(raster_list[[1]])

# Create kriging results raster
kriging_raster <- template_rast
values(kriging_raster) <- kriging_auto$krige_output$var1.pred
names(kriging_raster) <- "SOC_residuals"

# Mask the kriging raster with the soil polygon
masked_kriging_raster <- mask(crop(kriging_raster, soil_polygon), soil_polygon)

# Export the kriging raster as TIFF
writeRaster(masked_kriging_raster,
            filename = file.path("outputs", "kriging_residuals.tif"),
            overwrite = TRUE)

# Combine GWR predictions with kriging residuals for adjusted prediction
adjusted_prediction <- prediction_raster + masked_kriging_raster
names(adjusted_prediction) <- "SOC"

# Export the adjusted prediction raster
writeRaster(adjusted_prediction,
            filename = file.path("outputs", "prediction_gwr_adjusted.tif"),
            overwrite = TRUE)


# Plot the adjusted prediction raster using tmap
# Set tmap mode to plot
tmap_mode("plot")

# Now create the map
map_adjusted_prediction <- tm_shape(adjusted_prediction) +
  tm_raster(style = "quantile", n = 6, palette = "viridis", title="Adjusted SOC (Mg/ha)" ) +
  tm_shape(soil_polygon) +
  tm_compass(type="arrow", position=c(.05, 1.05))+
  tm_borders(alpha = 0.2) +
  tm_layout(title="Adjusted GWR Model", title.position = c("center", "top"), legend.position = c("right", "top"))

map_adjusted_prediction

tmap_save(map_adjusted_prediction, filename = "plots/adjusted_gwr_prediction_masked.png", dpi = 300)


# 6. Model validation after adjustment
# Validation at Sampling Points

# Extract values from rasters at soil sampling points
soil_coords <- st_coordinates(sf_soil)
adjusted_values <- terra::extract(adjusted_prediction, soil_coords)
original_values <- terra::extract(prediction_raster, soil_coords)

# Create a data frame with the extracted values
validation_df <- data.frame(
    observed = sf_soil$SOC,
    adjusted_pred = adjusted_values$SOC,
    original_pred = original_values$SOC
)

# Calculate RMSE
rmse_value <- rmse(validation_df$observed, validation_df$adjusted_pred)

# Calculate MAE
mae_value <- mae(validation_df$observed, validation_df$adjusted_pred)

# Calculate R2
ss_res <- sum((validation_df$observed - validation_df$adjusted_pred)^2)
ss_tot <- sum((validation_df$observed - mean(validation_df$observed))^2)
r2_value <- 1 - (ss_res / ss_tot)

# Calculate improvement metrics
rmse_original <- rmse(validation_df$observed, validation_df$original_pred)
mae_original <- mae(validation_df$observed, validation_df$original_pred)
ss_res_original <- sum((validation_df$observed - validation_df$original_pred)^2)
r2_original <- 1 - (ss_res_original / ss_tot)

# Create results data frame
results_df <- data.frame(
    Metric = c("RMSE", "MAE", "R2"),
    Original = c(rmse_original, mae_original, r2_original),
    Adjusted = c(rmse_value, mae_value, r2_value),
    Improvement = c(
        ((rmse_original - rmse_value) / rmse_original) * 100,
        ((mae_original - mae_value) / mae_original) * 100,
        ((r2_value - r2_original) / abs(r2_original)) * 100
    )
)

# Save validation metrics
write.csv(results_df, 
          file = normalizePath("outputs/validation_metrics_at_points.csv", mustWork = FALSE),
          row.names = FALSE)

# Save validation data
write.csv(validation_df, 
          file = normalizePath("outputs/validation_comparative.csv", mustWork = FALSE),
          row.names = FALSE)
