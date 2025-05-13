# Script to install all required packages for the GWR Shiny app

# List of required packages
required_packages <- c(
  # Shiny packages
  "shiny", 
  "shinydashboard", 
  "shinyjs", 
  "shinycssloaders", 
  "shinythemes",
  "DT",
  
  # GWR modeling packages
  "spgwr", 
  "GWmodel", 
  
  # Spatial data handling packages
  "terra", 
  "sf", 
  "gstat",
  "automap",
  
  # Visualization packages
  "tmap", 
  "leaflet",
  "ggplot2", 
  "plotly",
  "ggspatial",
  
  # Other utility packages
  "Metrics", 
  "dplyr"
)

# Function to install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("Installing package: ", pkg, "\n"))
    install.packages(pkg, dependencies = TRUE)
  } else {
    cat(paste0("Package already installed: ", pkg, "\n"))
  }
}

# Install missing packages
cat("Checking and installing required packages...\n")
sapply(required_packages, install_if_missing)
cat("\nAll required packages are now installed.\n")
cat("You can now run the GWR Shiny app using 'shiny::runApp(\"app.R\")'\n")
