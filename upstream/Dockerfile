FROM rocker/r-base:4.4.0

LABEL maintainer="USER <jardine@uw.edu>"

# Install Linux packages
RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends --fix-broken \
    libfreetype-dev \
    libfreetype6-dev \ 
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
    libfribidi-dev \
    libharfbuzz-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

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
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('r-spatial/leafgl')"

# Set environment variables and explicitly set the repositories in R
RUN R -e "Sys.setenv(RENV_CONFIG_REPOS_OVERRIDE = 'https://cloud.r-project.org'); renv::restore(repos = c(CRAN = 'https://cloud.r-project.org'))"

RUN echo "local(options(shiny.port = 8080, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

EXPOSE 8080

CMD Rscript app.R 

