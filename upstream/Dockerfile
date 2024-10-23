# Base image
FROM rocker/shiny:4.4.0

LABEL maintainer="USER <jardine@uw.edu>"

# Install Linux packages
RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends --fix-broken \
    libfreetype6 \
    libfreetype6-dev \
    libfontconfig1 \
    libfontconfig1-dev \
    sudo \
    libicu-dev \
    pandoc \
    make \
    git \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libgit2-dev \
    zlib1g-dev \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    libpng-dev \
    libudunits2-dev \
    libglpk-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    pkg-config && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /upstream

COPY app.R app.R
COPY data data
COPY data-raw data-raw
COPY dev dev
COPY inst inst
COPY man man 
COPY NAMESPACE NAMESPACE
COPY R R
COPY LICENSE LICENSE
COPY DESCRIPTION DESCRIPTION
COPY NAMESPACE NAMESPACE
COPY renv.lock renv.lock

# Install initial packages and renv
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Set environment variables and restore from renv
RUN R -e "Sys.setenv(RENV_CONFIG_REPOS_OVERRIDE = 'https://cloud.r-project.org'); renv::restore(repos = c(CRAN = 'https://cloud.r-project.org'))"

# Install leafgl
RUN R -e "remotes::install_github('r-spatial/leafgl')"

# Create the missing directory for Rprofile.site
RUN mkdir -p /usr/lib/R/etc

# Set shiny app options
RUN echo "local(options(shiny.port = 8080, shiny.host = '0.0.0.0'))" >> /usr/local/lib/R/etc/Rprofile.site

EXPOSE 8080

CMD Rscript app.R

