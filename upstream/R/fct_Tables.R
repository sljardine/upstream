#' @title Summarize variables for select culverts
#' @param points A simple features point data frame containing culvert locations and attributes.
#' @param points_sel A simple features point data frame containing selected culvert locations and attributes.
#' @param barrier_idp A vector of planned culvert IDs
#' @return A summary table
#' @export
get_summary_table <- function(
    points,
    points_sel,
    barrier_idp,
    remove_bad_culvert_matches,
    # TODO remove this!!!
    bad_match
    ){
  
  # remove bad culvert matches
  if(remove_bad_culvert_matches){
    points <- points %>% dplyr::filter(!bad_match)
    points_sel <- points_sel[!bad_match]
  }

  # planned barrier selection: set benefit to zero if barrier is already planned by user
  if(! 0 %in% barrier_idp){
    points <- points %>%
      dplyr::mutate(hmarg_length = ifelse(site_id %in% barrier_idp, 0, hmarg_length)
      )
  }

  sum_tab <- points %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(points_sel) %>%
    dplyr::summarize(
      `Chinook` = round(sum(hmarg_length * grepl("Chinook", potential_species)), 2),
      `Chum` = round(sum(hmarg_length * grepl("Chum", potential_species)), 2),
      `Coho` = round(sum(hmarg_length * grepl("Coho", potential_species)), 2),
      `Pink` = round(sum(hmarg_length * grepl("Pink", potential_species)), 2),
      `Sockeye` = round(sum(hmarg_length * grepl("Sockeye", potential_species)), 2),
      `Steelhead` = round(sum(hmarg_length * grepl("Steelhead", potential_species)), 2),
      `Total Habitat` = round(sum(hmarg_length), 2)
    ) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::rename(`Habitat (km)` = V1)

  return(sum_tab)
}


#' @title Get the list of culverts selected for the plan
#' @param points A simple features point data frame containing culvert locations and attributes.
#' @param points_sel A simple features point data frame containing selected culvert locations and attributes.
#' @param barrier_idp A vector of planned culvert IDs
#' @return A summary table
#' @export
get_plan_list <- function(
  points,
  points_sel,
  barrier_idp,
  remove_bad_culvert_matches,
  # TODO remove this!!!
  bad_match
){
  
  # remove bad culvert matches
  if(remove_bad_culvert_matches){
    points <- points %>% dplyr::filter(!bad_match)
    points_sel <- points_sel[!bad_match]
  }

  # planned barrier selection if barrier is already planned by user
  points <- points %>%
    dplyr::mutate(proj_plan = ifelse(site_id %in% barrier_idp, paste("Yes"), paste("No")))

  # #sum costs for suggested barriers (do not include already planned/complete)
  # cost_less_plan <-points %>%
  #   sf::st_drop_geometry() %>%
  #   dplyr::filter(points_sel) %>%
  #   dplyr::group_by(proj_plan) %>%
  #   dplyr::filter(proj_plan == "No") %>%
  #   dplyr::summarize(cost_sum = sum(cost))

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
