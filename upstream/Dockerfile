FROM rocker/r-base:4.1.0
LABEL maintainer="USER <jardine@uw.edu>"

ENV RENV_VERSION 0.15.5

# Update package list
RUN apt-get update --fix-missing

# Install packages one by one to isolate the problematic one
RUN apt-get install -y --no-install-recommends sudo || { echo 'ERROR: Failed to install sudo'; exit 1; }
RUN apt-get install -y --no-install-recommends libfreetype-dev || { echo 'ERROR: Failed to install libfreetype-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libicu-dev || { echo 'ERROR: Failed to install libicu-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends pandoc || { echo 'ERROR: Failed to install pandoc'; exit 1; }
RUN apt-get install -y --no-install-recommends make || { echo 'ERROR: Failed to install make'; exit 1; }
RUN apt-get install -y --no-install-recommends git || { echo 'ERROR: Failed to install git'; exit 1; }
RUN apt-get install -y --no-install-recommends libssl-dev || { echo 'ERROR: Failed to install libssl-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libcurl4-openssl-dev || { echo 'ERROR: Failed to install libcurl4-openssl-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libxml2-dev || { echo 'ERROR: Failed to install libxml2-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libgit2-dev || { echo 'ERROR: Failed to install libgit2-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends zlib1g-dev || { echo 'ERROR: Failed to install zlib1g-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libgdal-dev || { echo 'ERROR: Failed to install libgdal-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends gdal-bin || { echo 'ERROR: Failed to install gdal-bin'; exit 1; }
RUN apt-get install -y --no-install-recommends libgeos-dev || { echo 'ERROR: Failed to install libgeos-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libproj-dev || { echo 'ERROR: Failed to install libproj-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libsqlite3-dev || { echo 'ERROR: Failed to install libsqlite3-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libpng-dev || { echo 'ERROR: Failed to install libpng-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libudunits2-dev || { echo 'ERROR: Failed to install libudunits2-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libglpk-dev || { echo 'ERROR: Failed to install libglpk-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libfontconfig1-dev || { echo 'ERROR: Failed to install libfontconfig1-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libfribidi-dev || { echo 'ERROR: Failed to install libfribidi-dev'; exit 1; }
RUN apt-get install -y --no-install-recommends libharfbuzz-dev || { echo 'ERROR: Failed to install libharfbuzz-dev'; exit 1; }

# Clean up
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

RUN install.r ROI.plugin.glpk

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

# install initial packages
# install renv
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN R -e "remotes::install_github('r-spatial/leafgl')"

# install packages from renv 
ENV RENV_PATHS_LIBRARY renv/library

RUN R -e "renv::restore()"

RUN echo "local(options(shiny.port = 8080, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

EXPOSE 8080
CMD Rscript app.R


