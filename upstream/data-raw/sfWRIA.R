## code to prepare `sfWRIA` dataset goes here

#library(sf)

sfWRIA <- read_sf('../../culverts_opt/data/processed/wrias_casearea/wrias_casearea.shp')
usethis::use_data(sfWRIA, overwrite = TRUE)
