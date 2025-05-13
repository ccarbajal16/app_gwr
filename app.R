library(shiny)
library(spgwr)
library(GWmodel)
library(terra)
library(sf)
library(gstat)
library(tmap)
library(Metrics)
library(ggplot2)
library(automap)
library(dplyr)
library(ggspatial)
library(DT)
library(leaflet)
library(shinyjs)
library(shinycssloaders)
library(plotly)
library(shinydashboard)

# Custom directory input function
directoryInput <- function(id, label, value = NULL) {
  div(
    class = "form-group shiny-input-container",
    label = label,
    div(
      style = "display: flex;",
      textInput(id, NULL, value = value, placeholder = "Select directory"),
      actionButton(paste0(id, "Button"), "Browse", class = "btn-secondary")
    )
  )
}

# UI Definition
ui <- dashboardPage(
  skin = "blue",

  # Dashboard Header
  dashboardHeader(
    title = "GWR Modeling Tool",
    titleWidth = 300
  ),

  # Dashboard Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Input", tabName = "data_input", icon = icon("upload"),
               menuSubItem("Soil Data", tabName = "soil_data"),
               menuSubItem("Spatial Files", tabName = "spatial_files")
      ),
      menuItem("Analysis", tabName = "analysis", icon = icon("cogs"),
               menuSubItem("Model Settings", tabName = "model_settings"),
               menuSubItem("Run Model", tabName = "run_model")
      ),
      menuItem("Results", tabName = "results", icon = icon("chart-bar"),
               menuSubItem("Model Summary", tabName = "model_summary"),
               menuSubItem("Coefficient Maps", tabName = "coef_maps"),
               menuSubItem("Prediction Maps", tabName = "pred_maps")
      ),
      menuItem("Download", tabName = "download", icon = icon("download")),
      menuItem("Documentation", tabName = "documentation", icon = icon("book"))
    )
  ),

  # Dashboard Body
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .box-header {
          background-color: #f8f9fa;
          color: #444;
        }
        .progress-bar {
          background-color: #3c8dbc;
        }
        .shiny-notification {
          position: fixed;
          top: 70px;
          right: 15px;
        }
      "))
    ),

    tabItems(
      # Home Tab
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12,
            title = "Welcome to GWR Modeling Tool",
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(width = 8,
                h4("This application allows you to perform Geographically Weighted Regression (GWR)
                   and optional Kriging analysis on spatial data."),
                p("Follow these steps to complete your analysis:"),
                tags$ol(
                  tags$li("Upload your soil data with coordinates and response variable"),
                  tags$li("Upload your boundary polygon (optional) and predictor raster files"),
                  tags$li("Configure GWR and Kriging parameters"),
                  tags$li("Run the analysis and explore the results"),
                  tags$li("Download the outputs for further use")
                ),
                p("Use the sidebar menu to navigate through the application.")
              ),
              column(width = 4,
                div(style = "text-align: center;",
                  tags$img(src = "gwr_logo.svg", alt = "GWR Logo", width = "100%", style = "max-width: 300px;"),
                  p(style = "font-style: italic; margin-top: 10px;", "GWR captures spatial variation using weighted local regression")
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "About GWR",
            status = "info",
            solidHeader = TRUE,
            p("Geographically Weighted Regression (GWR) is a spatial statistical technique that takes
              into account spatial non-stationarity in regression modeling. Unlike traditional regression
              methods, GWR allows regression coefficients to vary across space, providing a local model
              of the variable you are trying to predict.")
          ),
          box(
            width = 6,
            title = "About Kriging",
            status = "info",
            solidHeader = TRUE,
            p("Kriging is a geostatistical interpolation technique that uses a variogram to express
              spatial variation and minimizes the error of predicted values. In this application,
              Kriging is used to interpolate the residuals from the GWR model, improving the overall
              prediction accuracy.")
          )
        )
      ),

      # Soil Data Tab
      tabItem(
        tabName = "soil_data",
        fluidRow(
          box(
            width = 12,
            title = "Upload Soil Data",
            status = "primary",
            solidHeader = TRUE,
            p("Upload a CSV file containing soil data with coordinates and response variable."),
            p("The file should include columns for X and Y coordinates and the response variable."),
            fileInput("soilDataFile", "Choose CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv"
                      )
            ),
            checkboxInput("headerPresent", "File has header", TRUE),
            textInput("responseVar", "Response Variable Name", "SOC")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Soil Data Preview",
            status = "info",
            solidHeader = TRUE,
            DTOutput("soilDataTable") %>% withSpinner(color = "#3c8dbc")
          )
        )
      ),

      # Spatial Files Tab
      tabItem(
        tabName = "spatial_files",
        fluidRow(
          box(
            width = 12,
            title = "Upload Spatial Files",
            status = "primary",
            solidHeader = TRUE,

            h4("Boundary Polygon (Optional)"),
            p("Upload a polygon file to define the boundary of your study area."),
            fileInput("polygonFile", "Choose Polygon File",
                      accept = c(".gpkg", ".geojson", ".shp")
            ),

            hr(),

            h4("Predictor Rasters"),
            p("Upload GeoTIFF files containing predictor variables."),
            fileInput("rasterFiles", "Choose GeoTIFF Files",
                      multiple = TRUE,
                      accept = c(".tif", ".tiff")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Spatial Data Preview",
            status = "info",
            solidHeader = TRUE,
            leafletOutput("mapPreview", height = "500px") %>% withSpinner(color = "#3c8dbc")
          )
        )
      ),

      # Model Settings Tab
      tabItem(
        tabName = "model_settings",
        fluidRow(
          box(
            width = 12,
            title = "GWR Model Settings",
            status = "primary",
            solidHeader = TRUE,
            p("Configure the settings for Geographically Weighted Regression (GWR) model."),

            h4("Bandwidth Selection"),
            selectInput("bandwidthMethod", "Bandwidth Selection Method",
                       choices = c("AIC" = "aic", "CV" = "cv", "AICc" = "AICc"),
                       selected = "AICc"),

            h4("Kernel Function"),
            selectInput("kernelType", "Kernel Type",
                       choices = c("Gaussian" = "gaussian",
                                   "Exponential" = "exponential",
                                   "Bisquare" = "bisquare"),
                       selected = "gaussian"),

            h4("Additional Options"),
            checkboxInput("adaptiveBandwidth", "Use Adaptive Bandwidth", FALSE),
            checkboxInput("performKriging", "Perform Residual Kriging", TRUE)
          )
        )
      ),

      # Run Model Tab
      tabItem(
        tabName = "run_model",
        fluidRow(
          box(
            width = 12,
            title = "Run GWR Model",
            status = "primary",
            solidHeader = TRUE,
            p("Review your settings and run the GWR model."),

            h4("Output Options"),
            textInput("outputPrefix", "Output File Prefix", "gwr_model"),
            directoryInput("outputDir", "Select Output Directory", value = "results"),

            hr(),

            div(style = "text-align: center;",
                actionButton("runModel", "Run GWR Model",
                            class = "btn-primary btn-lg",
                            icon = icon("play"))
            )
          )
        )
      ),

      # Model Summary Tab
      tabItem(
        tabName = "model_summary",
        fluidRow(
          box(
            width = 12,
            title = "GWR Model Summary",
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("modelSummary") %>% withSpinner(color = "#3c8dbc")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Performance Metrics",
            status = "info",
            solidHeader = TRUE,
            tableOutput("metricsTable") %>% withSpinner(color = "#3c8dbc"),
            downloadButton("downloadMetricsSummary", "Download Metrics", class = "btn-sm btn-info")
          ),
          box(
            width = 6,
            title = "Residual Analysis",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("residualPlot") %>% withSpinner(color = "#3c8dbc"),
            downloadButton("downloadResidualPlot", "Download Plot", class = "btn-sm btn-info")
          )
        )
      ),

      # Coefficient Maps Tab
      tabItem(
        tabName = "coef_maps",
        fluidRow(
          box(
            width = 3,
            title = "Coefficient Options",
            status = "primary",
            solidHeader = TRUE,
            selectInput("coefficientSelect", "Select Coefficient", choices = NULL),
            downloadButton("downloadCoeffMap", "Download Map", class = "btn-sm btn-info")
          ),
          box(
            width = 9,
            title = "Coefficient Map",
            status = "info",
            solidHeader = TRUE,
            plotOutput("coefficientMap", height = "600px") %>% withSpinner(color = "#3c8dbc")
          )
        )
      ),

      # Prediction Maps Tab
      tabItem(
        tabName = "pred_maps",
        fluidRow(
          box(
            width = 3,
            title = "Prediction Options",
            status = "primary",
            solidHeader = TRUE,
            checkboxInput("showAdjusted", "Show Adjusted Prediction (with Kriging)", TRUE),
            downloadButton("downloadPredMap", "Download Map", class = "btn-sm btn-info")
          ),
          box(
            width = 9,
            title = "Prediction Map",
            status = "info",
            solidHeader = TRUE,
            plotOutput("predictionMap", height = "600px") %>% withSpinner(color = "#3c8dbc")
          )
        )
      ),

      # Download Tab
      tabItem(
        tabName = "download",
        fluidRow(
          box(
            width = 12,
            title = "Download Results",
            status = "primary",
            solidHeader = TRUE,
            p("Download the results of your analysis in various formats."),

            h4("Raster Files"),
            fluidRow(
              column(width = 4, downloadButton("downloadPredRaster", "GWR Prediction (GeoTIFF)", class = "btn-info")),
              column(width = 4, downloadButton("downloadAdjustedRaster", "Adjusted Prediction (GeoTIFF)", class = "btn-info"))
            ),

            hr(),

            h4("Data Files"),
            fluidRow(
              column(width = 4, downloadButton("downloadMetrics", "Model Metrics (CSV)", class = "btn-info")),
              column(width = 4, downloadButton("downloadCoefficients", "Coefficients (CSV)", class = "btn-info"))
            )
          )
        )
      ),

      # Documentation Tab
      tabItem(
        tabName = "documentation",
        fluidRow(
          tabBox(
            id = "docs_tabs",
            width = 12,
            tabPanel(
              title = "GWR Analysis",
              h3("Geographically Weighted Regression"),
              p("Geographically Weighted Regression (GWR) is a spatial version of linear regression that takes into account the spatial non-stationarity in the data."),
              p("Unlike traditional regression methods, GWR allows the relationship between the response variable and predictors to vary spatially."),
              h4("Key Parameters:"),
              tags$ul(
                tags$li(tags$strong("Bandwidth:"), " Controls the extent of the spatial kernel. Either fixed (distance-based) or adaptive (based on number of neighbors)."),
                tags$li(tags$strong("Kernel Function:"), " Defines how observations are weighted based on distance. Options include Gaussian, bisquare, and exponential."),
                tags$li(tags$strong("Predictors:"), " The environmental covariates that are used to explain the response variable.")
              ),
              h4("Outputs:"),
              tags$ul(
                tags$li(tags$strong("Local Coefficients:"), " Regression coefficients calculated for each location"),
                tags$li(tags$strong("Local R²:"), " Goodness-of-fit measure at each location"),
                tags$li(tags$strong("Residuals:"), " Difference between observed and predicted values")
              )
            ),
            tabPanel(
              title = "Kriging",
              h3("Kriging Interpolation"),
              p("Kriging is a geostatistical interpolation technique that uses the spatial correlation structure in the data to make predictions at unsampled locations."),
              h4("In GWR-Kriging:"),
              p("Kriging is applied to interpolate the residuals from the GWR model, capturing remaining spatial patterns not explained by the covariates.")
            ),
            tabPanel(
              title = "How to Use",
              h3("Using the GWR Modeling Tool"),
              p("This application guides you through the process of performing Geographically Weighted Regression analysis on spatial data."),
              h4("Step 1: Data Input"),
              p("Upload your soil data CSV file with coordinates (x, y) and the response variable you want to model. Optionally, upload a boundary polygon to mask predictions and predictor raster files."),
              h4("Step 2: Configure Model Parameters"),
              tags$ul(
                tags$li(tags$strong("Bandwidth Selection Method:"), " Choose between AIC, CV, or AICc for optimal bandwidth selection"),
                tags$li(tags$strong("Kernel Type:"), " Select the kernel function (Gaussian, Exponential, or Bisquare)"),
                tags$li(tags$strong("Adaptive Bandwidth:"), " Enable to use a variable bandwidth based on number of neighbors"),
                tags$li(tags$strong("Residual Kriging:"), " Enable to improve predictions by kriging the residuals")
              ),
              h4("Step 3: Run the Model"),
              p("Click the 'Run GWR Model' button to start the analysis."),
              h4("Step 4: Explore Results"),
              p("Navigate through the Results tabs to view model summary, coefficient maps, and prediction maps.")
            )
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Create a reactive value to store the data
  values <- reactiveValues(
    soilData = NULL,
    sfSoil = NULL,
    polygon = NULL,
    rasterList = list(),
    rasterVars = NULL,
    gwrModel = NULL,
    gwrResults = NULL,
    predictionRaster = NULL,
    adjustedPrediction = NULL,
    coefficientRasters = list(),
    maskedCoeffRasters = list()
  )

  # Handle directory selection
  observeEvent(input$outputDirButton, {
    shinyjs::alert("Please note the directory path. In a full deployment, this would open a directory selection dialog.")
  })

  # Load soil data
  observeEvent(input$soilDataFile, {
    req(input$soilDataFile)

    # Read the uploaded CSV file
    soilData <- read.csv(input$soilDataFile$datapath,
                         header = input$headerPresent)

    # Store the data
    values$soilData <- soilData

    # Create spatial object if x and y columns exist
    if(all(c("x", "y") %in% colnames(soilData))) {
      values$sfSoil <- st_as_sf(soilData, coords = c("x", "y"), crs = 5070)

      # Update response variable choices
      updateSelectInput(session, "responseVar",
                       choices = names(soilData)[!names(soilData) %in% c("x", "y")],
                       selected = input$responseVar)
    } else {
      showNotification("CSV must contain 'x' and 'y' columns for coordinates",
                      type = "error")
    }

    # Update the data preview
    output$soilDataTable <- renderDT({
      datatable(soilData, options = list(pageLength = 10, scrollX = TRUE))
    })

    # Update map preview
    updateMapPreview()
  })

  # Load polygon data
  observeEvent(input$polygonFile, {
    req(input$polygonFile)

    # Read the uploaded polygon file
    ext <- tools::file_ext(input$polygonFile$name)

    tryCatch({
      if(ext %in% c("gpkg", "geojson", "shp")) {
        values$polygon <- st_read(input$polygonFile$datapath)

        # Update map preview
        updateMapPreview()
      } else {
        showNotification("Unsupported polygon file format", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error loading polygon:", e$message), type = "error")
    })
  })

  # Load raster files
  observeEvent(input$rasterFiles, {
    req(input$rasterFiles)

    # Clear existing raster list
    values$rasterList <- list()

    # Read each uploaded raster file
    for(i in 1:nrow(input$rasterFiles)) {
      fileName <- input$rasterFiles$name[i]
      filePath <- input$rasterFiles$datapath[i]

      tryCatch({
        # Load raster
        r <- rast(filePath)

        # Get variable name from file name (without extension)
        varName <- tools::file_path_sans_ext(fileName)

        # Add to raster list
        values$rasterList[[varName]] <- r
      }, error = function(e) {
        showNotification(paste("Error loading raster", fileName, ":", e$message),
                        type = "error")
      })
    }

    # Update raster variable names
    values$rasterVars <- names(values$rasterList)

    # Update map preview
    updateMapPreview()

    # Show notification
    showNotification(paste("Loaded", length(values$rasterList), "raster files"),
                    type = "message")
  })

  # Update map preview
  updateMapPreview <- function() {
    output$mapPreview <- renderLeaflet({
      # Create base leaflet map
      map <- leaflet() %>%
        addTiles() %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")

      # Add soil points if available
      if(!is.null(values$sfSoil)) {
        # Transform to WGS84 for leaflet
        sfSoilWgs <- st_transform(values$sfSoil, 4326)

        map <- map %>%
          addCircleMarkers(
            data = sfSoilWgs,
            radius = 5,
            color = "blue",
            fillOpacity = 0.8,
            popup = ~paste("ID:", row.names(sfSoilWgs))
          )
      }

      # Add polygon if available
      if(!is.null(values$polygon)) {
        # Transform to WGS84 for leaflet
        polygonWgs <- st_transform(values$polygon, 4326)

        map <- map %>%
          addPolygons(
            data = polygonWgs,
            fillColor = "red",
            weight = 2,
            opacity = 1,
            color = "red",
            fillOpacity = 0.2
          )
      }

      # Return the map
      return(map)
    })
  }

  # Run GWR model
  observeEvent(input$runModel, {
    req(values$sfSoil, input$responseVar)

    # Validate response variable
    if(!input$responseVar %in% names(values$sfSoil)) {
      showNotification("Response variable not found in data", type = "error")
      return()
    }

    # Start progress tracking
    withProgress(message = 'Running GWR model...', value = 0, {

      # Step 1: Prepare data
      incProgress(0.1, detail = "Preparing data")

      # Get predictor variables (all columns except response and geometry)
      predictorVars <- names(values$sfSoil)[!names(values$sfSoil) %in%
                                           c(input$responseVar, "geometry")]

      # If no predictors are available, show error
      if(length(predictorVars) == 0) {
        showNotification("No predictor variables found in data", type = "error")
        return()
      }

      # Generate formula for GWR
      predictors <- paste(predictorVars, collapse = " + ")
      formula <- as.formula(paste(input$responseVar, "~", predictors))

      # Extract coordinates for GWR
      coords <- st_coordinates(values$sfSoil)

      # Step 2: Select optimal bandwidth
      incProgress(0.2, detail = "Selecting optimal bandwidth")

      # Choose bandwidth selection method based on input
      if(input$bandwidthMethod == "AICc") {
        # Use GWmodel package for AICc
        bw <- tryCatch({
          bw.gwr(formula,
                data = values$sfSoil,
                approach = "AICc",
                kernel = input$kernelType,
                adaptive = input$adaptiveBandwidth)
        }, error = function(e) {
          showNotification(paste("Bandwidth selection error:", e$message),
                          type = "error")
          return(NULL)
        })
      } else {
        # Use spgwr package for AIC or CV
        bw <- tryCatch({
          gwr.sel(formula,
                 data = values$sfSoil,
                 coords = coords,
                 method = input$bandwidthMethod,
                 adapt = input$adaptiveBandwidth,
                 gweight = if(input$kernelType == "gaussian") gwr.Gauss else gwr.bisquare)
        }, error = function(e) {
          showNotification(paste("Bandwidth selection error:", e$message),
                          type = "error")
          return(NULL)
        })
      }

      # Check if bandwidth selection failed
      if(is.null(bw)) {
        return()
      }

      # Step 3: Fit GWR model
      incProgress(0.4, detail = "Fitting GWR model")

      # Fit the GWR model using the optimal bandwidth
      values$gwrModel <- tryCatch({
        gwr(formula,
           data = values$sfSoil,
           coords = coords,
           bandwidth = bw,
           gweight = if(input$kernelType == "gaussian") gwr.Gauss else gwr.bisquare,
           hatmatrix = TRUE)
      }, error = function(e) {
        showNotification(paste("GWR model fitting error:", e$message),
                        type = "error")
        return(NULL)
      })

      # Check if model fitting failed
      if(is.null(values$gwrModel)) {
        return()
      }

      # Step 4: Extract results
      incProgress(0.6, detail = "Processing results")

      # Extract the fitted, residuals and local R-squared values
      values$gwrResults <- data.frame(
        x = coords[,1],
        y = coords[,2],
        observed = values$sfSoil[[input$responseVar]],
        fitted = values$gwrModel$SDF$pred,
        residuals = values$gwrModel$SDF$gwr.e,
        localR2 = values$gwrModel$SDF$localR2
      )

      # Create spatial object from results
      values$gwrSf <- st_as_sf(values$gwrResults,
                              coords = c("x", "y"),
                              crs = st_crs(values$sfSoil))

      # Step 5: Generate coefficient rasters if raster data is available
      if(length(values$rasterList) > 0) {
        incProgress(0.7, detail = "Creating coefficient rasters")

        # Create lists to store the rasters of coefficients
        values$coefficientRasters <- list()
        values$maskedCoeffRasters <- list()

        # Get the first raster to use as template
        template_rast <- values$rasterList[[1]]

        # Create intercept raster
        intercept_raster <- template_rast
        values(intercept_raster) <- values$gwrModel$SDF$"(Intercept)"
        names(intercept_raster) <- "Intercept"
        values$coefficientRasters[["Intercept"]] <- intercept_raster

        # If polygon is available, mask the intercept raster
        if(!is.null(values$polygon)) {
          values$maskedCoeffRasters[["Intercept"]] <- mask(
            crop(intercept_raster, values$polygon),
            values$polygon
          )
        } else {
          values$maskedCoeffRasters[["Intercept"]] <- intercept_raster
        }

        # Iterate over the predictor variables
        for(var in predictorVars) {
          # Create coefficient raster
          coef_raster <- template_rast
          values(coef_raster) <- values$gwrModel$SDF[[var]]
          names(coef_raster) <- var
          values$coefficientRasters[[var]] <- coef_raster

          # If polygon is available, mask the coefficient raster
          if(!is.null(values$polygon)) {
            values$maskedCoeffRasters[[var]] <- mask(
              crop(coef_raster, values$polygon),
              values$polygon
            )
          } else {
            values$maskedCoeffRasters[[var]] <- coef_raster
          }
        }

        # Update coefficient selection dropdown
        updateSelectInput(session, "coefficientSelect",
                         choices = c("Intercept", predictorVars),
                         selected = "Intercept")

        # Step 6: Create prediction raster
        incProgress(0.8, detail = "Creating prediction raster")

        # Create prediction raster
        values$predictionRaster <- template_rast

        # Initialize prediction values with intercept
        prediction_values <- values(values$maskedCoeffRasters[["Intercept"]])

        # Add the contribution of each predictor variable
        for(var in predictorVars) {
          if(var %in% names(values$rasterList)) {
            prediction_values <- prediction_values +
              values(values$maskedCoeffRasters[[var]]) *
              values(values$rasterList[[var]])
          }
        }

        # Assign values to prediction raster
        values(values$predictionRaster) <- prediction_values
        names(values$predictionRaster) <- input$responseVar

        # Step 7: Perform residual kriging if selected
        if(input$performKriging) {
          incProgress(0.9, detail = "Performing residual kriging")

          # Create spatial object from residuals
          residuals_sf <- st_as_sf(
            data.frame(
              x = coords[,1],
              y = coords[,2],
              residuals = values$gwrResults$residuals
            ),
            coords = c("x", "y"),
            crs = st_crs(values$sfSoil)
          )

          # Create raster coordinates for prediction
          raster_coords <- as.data.frame(xyFromCell(values$predictionRaster,
                                                   1:ncell(values$predictionRaster)))

          # Convert to sf for autoKrige
          raster_coords_sf <- st_as_sf(raster_coords,
                                      coords = c("x", "y"),
                                      crs = st_crs(residuals_sf))

          # Perform automatic variogram fitting and kriging
          tryCatch({
            # Automatic variogram fitting
            auto_variogram <- autofitVariogram(residuals ~ 1, residuals_sf)

            # Perform kriging
            kriging_auto <- autoKrige(residuals ~ 1,
                                     input_data = residuals_sf,
                                     new_data = raster_coords_sf)

            # Create kriging raster
            kriging_raster <- template_rast
            values(kriging_raster) <- kriging_auto$krige_output$var1.pred
            names(kriging_raster) <- paste0(input$responseVar, "_residuals")

            # Mask the kriging raster if polygon is available
            if(!is.null(values$polygon)) {
              masked_kriging_raster <- mask(
                crop(kriging_raster, values$polygon),
                values$polygon
              )
            } else {
              masked_kriging_raster <- kriging_raster
            }

            # Combine GWR predictions with kriging residuals
            values$adjustedPrediction <- values$predictionRaster + masked_kriging_raster
            names(values$adjustedPrediction) <- input$responseVar
          }, error = function(e) {
            showNotification(paste("Kriging error:", e$message), type = "error")
            # If kriging fails, use the original prediction
            values$adjustedPrediction <- values$predictionRaster
          })
        } else {
          # If kriging is not selected, use the original prediction
          values$adjustedPrediction <- values$predictionRaster
        }
      }

      # Step 8: Calculate performance metrics
      incProgress(1.0, detail = "Finalizing")

      # Calculate RMSE
      rmse_value <- rmse(values$gwrResults$observed, values$gwrResults$fitted)

      # Calculate MAE
      mae_value <- mae(values$gwrResults$observed, values$gwrResults$fitted)

      # Calculate R2
      ss_res <- sum((values$gwrResults$observed - values$gwrResults$fitted)^2)
      ss_tot <- sum((values$gwrResults$observed - mean(values$gwrResults$observed))^2)
      r2_value <- 1 - (ss_res / ss_tot)

      # Create metrics dataframe with basic metrics
      metrics_data <- data.frame(
        Metric = c("RMSE", "MAE", "R²"),
        Value = c(
          rmse_value,
          mae_value,
          r2_value
        )
      )

      # Add AIC and AICc if they exist
      if (!is.null(values$gwrModel$results$AIC)) {
        metrics_data <- rbind(metrics_data,
                             data.frame(Metric = "AIC",
                                       Value = values$gwrModel$results$AIC))
      }

      if (!is.null(values$gwrModel$results$AICc)) {
        metrics_data <- rbind(metrics_data,
                             data.frame(Metric = "AICc",
                                       Value = values$gwrModel$results$AICc))
      }

      # Store the metrics
      values$metrics <- metrics_data

      # Update UI with results
      updateModelOutputs()

      # Show success notification
      showNotification("GWR model completed successfully", type = "message")

      # Switch to results tab
      updateTabItems(session, "sidebar", "model_summary")
    })
  })

  # Update model outputs
  updateModelOutputs <- function() {
    # Model summary
    output$modelSummary <- renderPrint({
      req(values$gwrModel)
      print(values$gwrModel)
    })

    # Metrics table
    output$metricsTable <- renderTable({
      req(values$metrics)
      values$metrics
    })

    # Residual plot
    output$residualPlot <- renderPlotly({
      req(values$gwrResults)

      p <- ggplot(values$gwrResults, aes(x = observed, y = fitted)) +
        geom_point(aes(size = abs(residuals), color = residuals), alpha = 0.7) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        scale_color_gradient2(low = "blue", mid = "white", high = "red",
                             midpoint = 0, name = "Residuals") +
        labs(
          title = "Observed vs. Fitted Values",
          subtitle = "Point size indicates absolute residual magnitude",
          x = paste("Observed", input$responseVar),
          y = paste("Fitted", input$responseVar),
          size = "|Residuals|"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.position = "right"
        )

      ggplotly(p)
    })

    # Coefficient map
    output$coefficientMap <- renderPlot({
      req(values$maskedCoeffRasters, input$coefficientSelect)

      # Set tmap mode to plot
      tmap_mode("plot")

      # Create map
      tm_shape(values$maskedCoeffRasters[[input$coefficientSelect]]) +
        tm_raster(
          style = "cont",
          palette = "RdBu",
          midpoint = 0,
          title = paste("Coefficient:", input$coefficientSelect)
        ) +
        tm_layout(
          main.title = paste("Spatial Distribution of", input$coefficientSelect, "Coefficient"),
          main.title.position = "center",
          legend.outside = TRUE,
          legend.outside.position = "right",
          frame = TRUE
        ) +
        tm_compass(position = c("left", "top")) +
        tm_scale_bar(position = c("right", "bottom"))
    })

    # Prediction map
    output$predictionMap <- renderPlot({
      # Choose which prediction to display
      if(input$showAdjusted && !is.null(values$adjustedPrediction)) {
        pred_raster <- values$adjustedPrediction
        title_prefix <- "Adjusted"
      } else {
        pred_raster <- values$predictionRaster
        title_prefix <- "Original"
      }

      req(pred_raster)

      # Set tmap mode to plot
      tmap_mode("plot")

      # Create map
      tm_shape(pred_raster) +
        tm_raster(
          style = "cont",
          palette = "viridis",
          title = paste(input$responseVar)
        ) +
        tm_layout(
          main.title = paste(title_prefix, "GWR Prediction of", input$responseVar),
          main.title.position = "center",
          legend.outside = TRUE,
          legend.outside.position = "right",
          frame = TRUE
        ) +
        tm_compass(position = c("left", "top")) +
        tm_scale_bar(position = c("right", "bottom"))
    })
  }

  # Download handlers
  # Download handlers for metrics in model summary tab
  output$downloadMetricsSummary <- downloadHandler(
    filename = function() {
      paste0(input$outputPrefix, "_metrics.csv")
    },
    content = function(file) {
      write.csv(values$metrics, file, row.names = FALSE)
    }
  )

  output$downloadResidualPlot <- downloadHandler(
    filename = function() {
      paste0(input$outputPrefix, "_residuals.png")
    },
    content = function(file) {
      p <- ggplot(values$gwrResults, aes(x = observed, y = fitted)) +
        geom_point(aes(size = abs(residuals), color = residuals), alpha = 0.7) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        scale_color_gradient2(low = "blue", mid = "white", high = "red",
                             midpoint = 0, name = "Residuals") +
        labs(
          title = "Observed vs. Fitted Values",
          subtitle = "Point size indicates absolute residual magnitude",
          x = paste("Observed", input$responseVar),
          y = paste("Fitted", input$responseVar),
          size = "|Residuals|"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.position = "right"
        )

      ggsave(file, p, width = 10, height = 8, dpi = 300)
    }
  )

  output$downloadCoeffMap <- downloadHandler(
    filename = function() {
      paste0(input$outputPrefix, "_coeff_", input$coefficientSelect, ".png")
    },
    content = function(file) {
      # Get selected coefficient raster
      coef_raster <- values$maskedCoeffRasters[[input$coefficientSelect]]

      # Set tmap mode to plot
      tmap_mode("plot")

      # Create map
      coeff_map <- tm_shape(coef_raster) +
        tm_raster(
          style = "cont",
          palette = "RdBu",
          midpoint = 0,
          title = paste("Coefficient:", input$coefficientSelect)
        ) +
        tm_layout(
          main.title = paste("Spatial Distribution of", input$coefficientSelect, "Coefficient"),
          main.title.position = "center",
          legend.outside = TRUE,
          legend.outside.position = "right",
          frame = TRUE
        ) +
        tm_compass(position = c("left", "top")) +
        tm_scale_bar(position = c("right", "bottom"))

      # Save map
      tmap_save(coeff_map, filename = file, width = 10, height = 8, dpi = 300)
    }
  )

  output$downloadPredMap <- downloadHandler(
    filename = function() {
      if(input$showAdjusted && !is.null(values$adjustedPrediction)) {
        paste0(input$outputPrefix, "_prediction_adjusted.png")
      } else {
        paste0(input$outputPrefix, "_prediction.png")
      }
    },
    content = function(file) {
      # Choose which prediction to display
      if(input$showAdjusted && !is.null(values$adjustedPrediction)) {
        pred_raster <- values$adjustedPrediction
        title_prefix <- "Adjusted"
      } else {
        pred_raster <- values$predictionRaster
        title_prefix <- "Original"
      }

      # Set tmap mode to plot
      tmap_mode("plot")

      # Create map
      pred_map <- tm_shape(pred_raster) +
        tm_raster(
          style = "cont",
          palette = "viridis",
          title = paste(input$responseVar)
        ) +
        tm_layout(
          main.title = paste(title_prefix, "GWR Prediction of", input$responseVar),
          main.title.position = "center",
          legend.outside = TRUE,
          legend.outside.position = "right",
          frame = TRUE
        ) +
        tm_compass(position = c("left", "top")) +
        tm_scale_bar(position = c("right", "bottom"))

      # Save map
      tmap_save(pred_map, filename = file, width = 10, height = 8, dpi = 300)
    }
  )

  # Download handler for prediction raster in download tab
  output$downloadPredRaster <- downloadHandler(
    filename = function() {
      paste0(input$outputPrefix, "_prediction.tif")
    },
    content = function(file) {
      req(values$predictionRaster)
      writeRaster(values$predictionRaster, filename = file, overwrite = TRUE)
    }
  )

  # Add handler for downloadAdjustedRaster
  output$downloadAdjustedRaster <- downloadHandler(
    filename = function() {
      paste0(input$outputPrefix, "_prediction_adjusted.tif")
    },
    content = function(file) {
      req(values$adjustedPrediction)
      writeRaster(values$adjustedPrediction, filename = file, overwrite = TRUE)
    }
  )

  # Download handler for metrics in download tab
  output$downloadMetrics <- downloadHandler(
    filename = function() {
      paste0(input$outputPrefix, "_metrics.csv")
    },
    content = function(file) {
      req(values$metrics)
      write.csv(values$metrics, file, row.names = FALSE)
    }
  )

  # Add handler for downloadCoefficients
  output$downloadCoefficients <- downloadHandler(
    filename = function() {
      paste0(input$outputPrefix, "_coefficients.csv")
    },
    content = function(file) {
      req(values$gwrModel)
      # Extract coefficients from the model
      coef_data <- as.data.frame(values$gwrModel$SDF)
      write.csv(coef_data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)