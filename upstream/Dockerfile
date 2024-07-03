FROM rocker/r-base:4.1.0
LABEL maintainer="USER <jardine@uw.edu>"

ENV RENV_VERSION 0.15.5

# Step 1: Install usrmerge to fix the merged-/usr layout issue
RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends usrmerge

# Step 2: Install the remaining dependencies
RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends \
    sudo \
    libfreetype-dev \
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
    libfontconfig1-dev \
    libfribidi-dev \
    libharfbuzz-dev

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

# Install initial packages
# Install renv
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN R -e "remotes::install_github('r-spatial/leafgl')"

# Install packages from renv 
ENV RENV_PATHS_LIBRARY renv/library

RUN R -e "renv::restore()"

RUN echo "local(options(shiny.port = 8080, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

EXPOSE 8080
CMD Rscript app.R

