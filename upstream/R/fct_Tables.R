#' @title Summarize variables for select culverts
#' @param points A simple features point data frame containing culvert locations and attributes.
#' @param points_sel A simple features point data frame containing selected culvert locations and attributes.
#' @param barrier_idp A vector of planned culvert IDs
#' @param hq Habitat quantity definition
#' @return A summary table
#' @export
get_summary_table <- function(
    points,
    points_sel,
    barrier_idp,
    hq = 1
    ){
  
  #habitat quantity definition
  ##length
  if(hq == 1){
    points <- points %>%
      dplyr::mutate(
        hmarg = hmarg_length,
        hmarg_urb_percent = hmarg_length_urb_nlcd_percent,
        hmarg_ag_percent = hmarg_length_agri_nlcd_percent,
        hmarg_nat_percent = hmarg_length_natural_percent
        )
    ##area
  } else if(hq == 2){
    points <- points %>%
      dplyr::mutate(
        hmarg = hmarg_area,
        hmarg_urb_percent = hmarg_area_urb_nlcd_percent,
        hmarg_ag_percent = hmarg_area_agri_nlcd_percent,
        hmarg_nat_percent = hmarg_area_natural_percent
      )
    ##volume
  } else {
    points <- points %>%
      dplyr::mutate(
        hmarg = hmarg_volume,
        hmarg_urb_percent = hmarg_volume_urb_nlcd_percent,
        hmarg_ag_percent = hmarg_volume_agri_nlcd_percent,
        hmarg_nat_percent = hmarg_volume_natural_percent
      )
  }
  # planned barrier selection: set benefit to zero if barrier is already planned by user
  if(! 0 %in% barrier_idp){
    points <- points %>%
      dplyr::mutate(hmarg = ifelse(site_id %in% barrier_idp, 0, hmarg)
      )
  }

  sum_tab <- points %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(points_sel) %>%
    dplyr::summarize(
      `Chinook` = round(sum(hmarg * grepl("Chinook", potential_species)), 2),
      `Chum` = round(sum(hmarg * grepl("Chum", potential_species)), 2),
      `Coho` = round(sum(hmarg * grepl("Coho", potential_species)), 2),
      `Pink` = round(sum(hmarg * grepl("Pink", potential_species)), 2),
      `Sockeye` = round(sum(hmarg * grepl("Sockeye", potential_species)), 2),
      `Steelhead` = round(sum(hmarg * grepl("Steelhead", potential_species)), 2),
      `Total Habitat` = round(sum(hmarg), 2),
      `Natural` = round(sum(hmarg * hmarg_nat_percent, na.rm = TRUE), 2),
      `Agricultural` = round(sum(hmarg * hmarg_ag_percent, na.rm = TRUE), 2),
      `Urban` = round(sum(hmarg * hmarg_urb_percent, na.rm = TRUE), 2)
    ) %>%
    t() %>%
    as.data.frame()

  #habitat quantity label
  ##length
  if(hq == 1){
    sum_tab <- sum_tab %>% dplyr::rename(`Habitat (km)` = V1)
    ##area
  } else if(hq == 2){
    sum_tab <- sum_tab %>% dplyr::rename(`Habitat (km^2)` = V1)
    ##volume
  } else {
    sum_tab <- sum_tab %>% dplyr::rename(`Habitat (km^3)` = V1)
  }
  return(sum_tab)
}


#' @title Get the list of culverts selected for the plan
#' @param points A simple features point data frame containing culvert locations and attributes.
#' @param points_sel A simple features point data frame containing selected culvert locations and attributes.
#' @param barrier_idp A vector of planned culvert IDs
#' @param cost A numeric value defining cost option (1 = default, 2 = user adjusted)
#' @param mean_design_cost A numeric value.
#' @param mean_construction_cost A numeric value.
#' @return A summary table
#' @export
get_plan_list <- function(
  points,
  points_sel,
  barrier_idp,
  cost = 1, #cost definition
  mean_design_cost = NULL, #user-defined mean design cost
  mean_construction_cost = NULL  #user-defined mean construction cost
){

  # cost adjustment
  if(cost == 2){
    points <- points %>%
      dplyr::mutate(
        cost = cost * mean_construction_cost / mean(cost) + mean_design_cost,
        cost_text = paste0("$", format(round(cost, 0), nsmall = 0, big.mark = ","))
      )
  } else {
    points <- points %>%
      dplyr::mutate(
        cost_text = paste0("$", format(round(cost, 0), nsmall = 0, big.mark = ","))
      )
  }
  
  # planned barrier selection if barrier is already planned by user
  points <- points %>%
    dplyr::mutate(proj_plan = ifelse(
      site_id %in% barrier_idp, paste("Yes"), paste("No"))
      )

  plan_list <- points %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(points_sel) %>%
    dplyr::filter(proj_plan == "No") %>%
    dplyr::select(site_id, huc_name, cost_text) %>%
    as.data.frame() %>%
    dplyr::rename(
      `Site ID` = site_id,
      `HUC 12 Name` = huc_name,
      `Estimated Site Cost` = cost_text
      )

  return(plan_list)
}
