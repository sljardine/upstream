FROM rocker/r-base:4.1.0
LABEL maintainer="USER <jardine@uw.edu>"

ENV RENV_VERSION 0.15.5

# Step 1: Install usrmerge to fix the merged-/usr layout issue
RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends usrmerge && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Step 2: Install the remaining dependencies separately to isolate issues
RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends sudo

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libfreetype-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libicu-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends pandoc

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends make

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends git

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libssl-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libcurl4-openssl-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libxml2-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libgit2-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends zlib1g-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libgdal-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends gdal-bin

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libgeos-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libproj-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libsqlite3-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libpng-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libudunits2-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libglpk-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libfontconfig1-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libfribidi-dev

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends libharfbuzz-dev

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

