#' @title Summarize variables for select culverts
#' @param points A simple features point data frame containing culvert locations and attributes.
#' @param points_sel A simple features point data frame containing selected culvert locations and attributes.
#' @return A summary table
#' @export
get_summary_table <- function(
    points,
    points_sel
    ){
  
  sum_tab <- points %>% 
    sf::st_drop_geometry() %>% 
    dplyr::filter(points_sel) %>% 
    dplyr::summarize(
      `Bull trout` = round(sum(hmarg_net * grepl("Bull", potential_species)), 2),
      `Chinook` = round(sum(hmarg_net * grepl("Chinook", potential_species)), 2),
      `Chum` = round(sum(hmarg_net * grepl("Chum", potential_species)), 2),
      `Coho` = round(sum(hmarg_net * grepl("Coho", potential_species)), 2),
      `Pink` = round(sum(hmarg_net * grepl("Pink", potential_species)), 2),
      `Resident` = round(sum(hmarg_net * grepl("Resident", potential_species)), 2),
      `Sockeye` = round(sum(hmarg_net * grepl("Sockeye", potential_species)), 2),
      `Steelhead` = round(sum(hmarg_net * grepl("Steelhead", potential_species)), 2),
      `SR Cutthroat` = round(sum(hmarg_net * grepl("Cutthroat", potential_species)), 2),
      `Total` = round(sum(hmarg_net), 2)
    ) %>% 
    t() %>% 
    as.data.frame() %>% 
    dplyr::rename(`Habitat (km)` = V1)
  
  return(sum_tab)
}