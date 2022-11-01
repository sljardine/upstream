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

# create leaflet popup html string
sfCulverts$popup <- sfCulverts %>%
  st_drop_geometry() %>%
  select(site_id, percent_fish_passable_code, potential_species, hfull, hmarg, cost, owner_type_code, survey_date) %>%
  pmap_chr(function(site_id, percent_fish_passable_code, potential_species, hfull, hmarg, cost, owner_type_code, survey_date){
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
      paste(round(hfull, 1), "km"),
      "<br>",
      "<b>Habitat to Next Barrier:</b> ",
      paste(round(hmarg, 1), "km"),
      "<br>",
      "<b>Estimated Construction Cost:</b> ",
      paste0("$", format(round(cost, 0), big.mark = ",", scientific = FALSE)),
      "<br>",
      "<b>Ownership:</b> ",
      owner_type_code,
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
