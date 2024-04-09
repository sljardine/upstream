library(ROI)
library(ROI.plugin.glpk)
library(sf)
library(tidyverse)
load(here("data-raw", "upstream_inputs_gm.Rdata"))
source(here("R", "fct_Suggest.R"))
source(here("R", "fct_Tables.R"))

test <- solve_opt(
  points = culverts_cmb_gm,
  budget = 60e6,
  D = D_gm,
  wria_sel = 0,
  huc_sel = 0,
  owner_sel = 0,
  obj = 1,
  w_urb = 0.33,
  w_ag = 0.33,
  w_nat = 0.34,
  w_temp = c(8, 23),
  hq = 1,
  species_sel = "all",
  barrier_idp = 0,
  cost = 2,
  mean_design_cost = 5e5,
  mean_construction_cost = 5e5
)

get_summary_table(
  points = culverts_cmb_gm,
  points_sel = test,
  barrier_idp = 0,
  hq = 1
)


