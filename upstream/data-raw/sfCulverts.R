## code to prepare `sfCulverts` dataset goes here

#library(dplyr)
#library(ggplot2)
#library(sf)

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

# remove temporary variables
sfCulverts <- sfCulverts %>% select(-isEstimatedCost)

# write data to rda file in data folder
usethis::use_data(sfCulverts, overwrite = TRUE)
