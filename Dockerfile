# Dockerfile for GWR Shiny App
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        libgdal-dev \
        libgeos-dev \
        libproj-dev \
        libudunits2-dev \
        libfontconfig1-dev \
        libfreetype6-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev \
        gdal-bin \
        pandoc \
        && rm -rf /var/lib/apt/lists/*

# Copy app files
COPY . /srv/shiny-server/app_gwr
WORKDIR /srv/shiny-server/app_gwr

# Install R packages
COPY install_packages.R ./
RUN Rscript install_packages.R

# Expose Shiny port
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=3838)"]
