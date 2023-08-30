FROM rocker/r-base:latest
LABEL maintainer="USER <user@example.com>"

# set renv version
ENV RENV_VERSION 0.15.5

# install linux packages
RUN apt-get update && apt-get install -y --no-install-recommends \
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
    libfontconfig1-dev \
    libfreetype-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev

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

# install packages from renv 
ENV RENV_PATHS_LIBRARY renv/library

RUN R -e "renv::restore()"

RUN echo "local(options(shiny.port = 80, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

EXPOSE 80
CMD Rscript app.R

