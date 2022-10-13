## code to prepare `dfCulverts` dataset goes here

#library(dplyr)
#library(ggplot2)

dfCulverts <- read.csv('../../culverts_opt/data/processed/culverts_cmb.csv')

# dub missing cost with linear model: cost ~ barrier_count
lmCost <- lm(cost ~ barrier_count, data = dfCulverts)
dfCulverts <- dfCulverts %>% 
  mutate(isEstimatedCost = case_when(is.na(cost) ~ TRUE, TRUE ~ FALSE)) %>%
  mutate(
    cost = case_when(is.na(cost) ~ predict(lmCost, list(barrier_count = barrier_count)), TRUE ~ cost),
  )

# plot actual and predicted cost vs barrier_count
#dfCulverts %>%
#  ggplot(aes(x = barrier_count, y = cost, col = isEstimatedCost)) + 
#  geom_point() +
#  theme_bw()

# potential_species also has NA values...

# remove temporary variables
dfCulverts <- dfCulverts %>% select(-isEstimatedCost)

# write data to rda file in data folder
usethis::use_data(dfCulverts, overwrite = TRUE)

