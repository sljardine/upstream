library(sf)

culvert_lines_sf <- read_sf('../../culverts_opt/data/processed/culvert_lines.gpkg')
usethis::use_data(culvert_lines, overwrite = TRUE)
