library(here)
library(tidyverse)

load(here("data-raw", "upstream_inputs_gm.Rdata"))
load(here("data-raw", "upstream_inputs.Rdata"))
source(here("R", "fct_Suggest.R"))
all_wrias <- wrias %>% dplyr::arrange(WRIA_NM) %>% dplyr::pull(WRIA_NR)

#for(i in 1 : length(all_wrias)){
#  wria_sel_i = all_wrias[i]

wria_sel_i = 10
soln <- solve_opt(
    #Inputs
  points = culverts_cmb_gm, #points with variables: hmarg_length, hmarg_area, hmarg_volume, cost, and wria_number
  budget = 3e5, #budget constraint
  D = D_gm, #connectivity matrix
  wria_sel = as.integer(wria_sel_i), #wria(s) to run the optimization problem on
  huc_sel = pw_hucs, #huc(s) to run the optimization problem on
  owner_sel = 0, #owner(s) to run the optimization problem on
  obj = 1, #indicator for whether objective function is to max quant (1) or max weighted sum of attributes (2)
  w_urb = NULL, #weight on urban habitat quantity
  w_ag = NULL, #weight on agricultural habitat quantity
  w_nat = NULL, #weight on natural habitat quantity
  w_temp = NULL, #weight on temperature
  hq = 1, #habitat quantity definition
  species_sel = "all", #species to run the optimization problem on
  barrier_idp = 0, #planned barrier IDs
  cost = 1, #cost definition
  mean_design_cost = NULL, #user-defined mean design cost
  mean_construction_cost = NULL  #user-defined mean construction cost
)
 
leaf_proxy <- wrias %>%
  leaflet::leaflet() %>%
  leaflet::addProviderTiles("CartoDB.Positron", group = "Grayscale", options = leaflet::providerTileOptions(minZoom = 6.5))  %>%
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

map <- map_leaflet_opt(
    leaf_proxy = leaf_proxy,
    points = culverts_cmb_gm, #culverts
    lines = lines_simp_gm, #lines with linestring geometries upstream of culvs
    dslines = lines_ds_gm, #lines with linestring geometries downstream of culvs
    soln = soln, #output from solve_opt()
    marginal_line_ids = marginal_line_ids_gm, #comids for all lines marginally upstream of each point
    downstream_line_ids = downstream_line_ids_gm, #comids for all lines downstream of each point on main stem
    wria_sel = as.integer(wria_sel_i), #wria(s) to run the optimization problem on
    huc_sel = 0, #huc(s) to run the optimization problem on
    barrier_idp = 0 #planned barrier IDs
)

#print(paste("done with", wria_sel_i))
#}


