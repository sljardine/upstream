## code to prepare `sfCulverts` dataset goes here

library(dplyr)
library(purrr)
library(ggplot2)
library(sf)

sfCulverts <- read_sf('../../culverts_opt/data/processed/culverts_cmb.gpkg')

# dub missing cost with linear model: cost ~ barrier_count
lmCost <- lm(cost ~ barrier_count, data = sfCulverts)
sfCulverts <- sfCulverts %>%
  mutate(isEstimatedCost = case_when(is.na(cost) ~ TRUE, TRUE ~ FALSE)) %>%
  mutate(
    cost = case_when(is.na(cost) ~ predict(lmCost, list(barrier_count = barrier_count)), TRUE ~ cost),
  )

# plot actual and predicted cost vs barrier_count
# sfCulverts %>%
#  ggplot(aes(x = barrier_count, y = cost, col = isEstimatedCost)) +
#  geom_point() +
#  theme_bw()

# potential_species also has NA values...

# assume hfull values greater than 20000 is in meters (convert to km)
#sfCulverts <- sfCulverts %>%
#  mutate(hfull = case_when(hfull > 20000 ~ hfull / 1000, TRUE ~ hfull))

# add wria_name
sfCulverts <- sfCulverts %>%
  merge(
    sf::read_sf('../../culverts_opt/data/processed/wrias_casearea/wrias_casearea.shp') %>%
      sf::st_drop_geometry() %>%
      dplyr::select(WRIA_NR, WRIA_NM) %>%
      dplyr::rename(wria_number = WRIA_NR, wria_name = WRIA_NM),
    left = TRUE
  )

# add owner class names
dfO <- sfCulverts %>%
  sf::st_drop_geometry() %>%
  dplyr::select(site_id, unique_owner_type_code) %>%
  purrr::pmap_dfr(function(site_id, unique_owner_type_code){
   cOwnerCodes <- strsplit(unique_owner_type_code, '/', fixed = TRUE)[[1]]
   cOwnerNames <- dplyr::case_when(
     cOwnerCodes == 1 ~ 'City',
     cOwnerCodes == 2 ~ 'County',
     cOwnerCodes == 3 ~ 'Federal',
     cOwnerCodes == 4 ~ 'Private',
     cOwnerCodes == 5 ~ 'State',
     cOwnerCodes == 6 ~ 'Tribal',
     cOwnerCodes == 7 ~ 'Other',
     cOwnerCodes == 8 ~ 'Port',
     cOwnerCodes == 9 ~ 'Drainage District',
     cOwnerCodes == 11 ~ 'Irrigation District',
     cOwnerCodes == 12 ~ 'Unknown'
  )
  data.frame(
    site_id = site_id,
    unique_owner_type_code = unique_owner_type_code,
    unique_owner_type_name = cOwnerNames %>% paste(collapse = '/'),
    owner_type_name_short = ifelse(length(cOwnerCodes) > 1, 'Multiple', cOwnerNames[1]),
    is_city = ifelse(1 %in% cOwnerCodes, TRUE, FALSE),
    is_county = ifelse(2 %in% cOwnerCodes, TRUE, FALSE),
    is_federal = ifelse(3 %in% cOwnerCodes, TRUE, FALSE),
    is_private = ifelse(4 %in% cOwnerCodes, TRUE, FALSE),
    is_state = ifelse(5 %in% cOwnerCodes, TRUE, FALSE),
    is_tribal = ifelse(6 %in% cOwnerCodes, TRUE, FALSE),
    is_other = ifelse(7 %in% cOwnerCodes, TRUE, FALSE),
    is_port = ifelse(8 %in% cOwnerCodes, TRUE, FALSE),
    is_drainage_district = ifelse(9 %in% cOwnerCodes, TRUE, FALSE),
    is_irrigation_district = ifelse(11 %in% cOwnerCodes, TRUE, FALSE),
    is_unknown = ifelse(12 %in% cOwnerCodes, TRUE, FALSE),
    is_multiple = ifelse(length(cOwnerCodes) > 1, TRUE, FALSE)
  )})
sfCulverts <- sfCulverts %>% merge(dfO)

# create leaflet popup html string
sfCulverts$popup <- sfCulverts %>%
  st_drop_geometry() %>%
  select(site_id, percent_fish_passable_code, potential_species, hfull_net, hmarg_net, cost, unique_owner_type_name, survey_date) %>%
  pmap_chr(function(site_id, percent_fish_passable_code, potential_species, hfull_net, hmarg_net, cost, unique_owner_type_name, survey_date){
    paste0(
      "<b>WDFW Site ID:</b> ",
      paste(
        paste0(
          "<a href='http://apps.wdfw.wa.gov/fishpassagephotos/Reports/",
          strsplit(site_id, '/', fixed = TRUE)[[1]],
          "_Report.pdf' target = '_blank'>",
          strsplit(site_id, '/', fixed = TRUE)[[1]],
          "</a>"
        ),
        collapse = '/'
      ),
      "<br>",
      "<b>Passability:</b> ",
      dplyr::case_when(
        percent_fish_passable_code == 99 ~ "Unknown",
        percent_fish_passable_code == 10 ~ "0%",
        percent_fish_passable_code == 20 ~ "33%",
        percent_fish_passable_code == 30 ~ "66%",
      ),
      "<br>",
      "<b>Potential Species:</b>",
      dplyr::case_when(is.na(potential_species) ~ " ", TRUE ~ ""),
      potential_species,
      "<br>",
      "<b>Total Upstream Habitat:</b> ",
      paste(round(hfull_net, 1), "km"),
      "<br>",
      "<b>Habitat to Next Barrier:</b> ",
      paste(round(hmarg_net, 1), "km"),
      "<br>",
      "<b>Estimated Construction Cost:</b> ",
      paste0("$", format(round(cost, 0), big.mark = ",", scientific = FALSE)),
      "<br>",
      "<b>Ownership:</b> ",
      unique_owner_type_name,
      "<br>",
      "<b>Survey Date:</b> ",
      survey_date,
      ""
    )
  })

# remove temporary variables
sfCulverts <- sfCulverts %>% select(-isEstimatedCost)

# write data to rda file in data folder
usethis::use_data(sfCulverts, overwrite = TRUE)
