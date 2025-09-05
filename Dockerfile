# Use the official Shiny image from Rocker
FROM rocker/shiny:latest

# Install system dependencies needed for some R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy your app into the container
COPY . /srv/shiny-server/

# Copy and run the package installer
COPY install.R /tmp/install.R
RUN Rscript /tmp/install.R

# Expose the Shiny server port
EXPOSE 3838

# Start the Shiny server
CMD ["/usr/bin/shiny-server"]
