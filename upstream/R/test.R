library(here)
library(leaflet)
library(leafgl)
library(sf)
library(tidyverse)
library(ROI)
library(ROI.plugin.glpk)

load(here("data", "D.rda"))
load(here("data", "culverts_cmb.rda"))
load(here("data", "wrias.rda"))
load(here("data", "lines_simp.rda"))
load(here("data", "marginal_line_ids.rda"))

source(here("R", "fct_suggest.R"))

soln <- solve_opt(
  points = culverts_cmb, #points with variables: hmarg_net, cost, and wria_number
  budget = 1e6, #budget constraint
  D = D, #connectivity matrix
  wria_sel = 7 #wria to run the optimization problem on
)


points = culverts_cmb #culverts
lines = lines_simp #lines with linestring geometries if glify = TRUE
soln = soln #output from solve_opt()
marginal_line_ids = marginal_line_ids #comids for all lines marginally upstream of each point

leaflet_proxy <- wrias %>%
  leaflet::leaflet() %>%
  leaflet::addProviderTiles("CartoDB.Positron", group = "Grayscale", options = leaflet::providerTileOptions(minZoom = 7))  %>%
  leaflet::addScaleBar("bottomleft") %>%
  leaflet::addPolygons(
    popup =  ~ paste0(
      "<b>WRIA Name:</b> ",
      WRIA_NM,
      "<br>",
      "<b>WRIA Number:</b> ",
      WRIA_NR
    ),
    weight = 1.5,
    opacity = 1,
    color = "#1c1cff",
    fillColor = "transparent"
  )

map_leaflet_opt(
  leaflet_proxy,
  culverts_cmb, #culverts
  lines_simp, #lines with linestring geometries
  soln, #output from solve_opt()
  marginal_line_ids #comids for all lines marginally upstream of each point
)
