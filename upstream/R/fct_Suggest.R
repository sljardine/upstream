#' @title Solve the optimization problem
#' @param points A simple features point data frame containing culvert locations and attributes.
#' @param budget A number specifying the budget constraint.
#' @param D A connectivity matrix.
#' @param area_sel A vector of WRIA ID numbers of interest.
#' @param subarea_sel A vector of WRIA ID numbers of interest.
#' @param owner_sel A vector of owner ID numbers of interest.
#' @param obj An indicator for whether objective function is to max quant (obj = 1) or max weighted sum of attributes (obj = 2).
#' @param w_urb A weight on urban habitat quantity.
#' @param w_ag A weight on agricultural habitat quantity.
#' @param w_nat A weight on natural habitat quantity.
#' @param w_temp A weight on ideal temperature.
#' @param hq A value for habitat quality definition (1 = length, 2 = area, 3 = volume).
#' @param species_sel A vector of species ID numbers of interest.
#' @param barrier_idp A vector of planned culvert IDs.
#' @return A logical vector of TRUE/FALSE values.
#' @export
solve_opt <- function(
    #Inputs
  points, #points with variables: hmarg_length, hmarg_area, hmarg_volume, cost, and wria_number
  budget, #budget constraint
  D, #connectivity matrix
  wria_sel, #wria(s) to run the optimization problem on
  huc_sel, #huc(s) to run the optimization problem on
  owner_sel, #owner(s) to run the optimization problem on
  obj, #indicator for whether objective function is to max quant (1) or max weighted sum of attributes (2)
  w_urb, #weight on urban habitat quantity
  w_ag, #weight on agricultural habitat quantity
  w_nat, #weight on natural habitat quantity
  w_temp, #weight on temperature
  hq, #habitat quantity definition
  species_sel, #species to run the optimization problem on
  barrier_idp, #planned barrier IDs
  cost, #cost definition
  mean_design_cost, #user-defined mean design cost
  mean_construction_cost  #user-defined mean construction cost
){
  
  #habitat quantity definition
  ##length
  if(hq == 1){
  points <- points %>%
    dplyr::mutate(
      hmarg = hmarg_length,
      hmarg_urb_nlcd_percent = hmarg_length_urb_nlcd_percent,
      hmarg_agri_nlcd_percent = hmarg_length_agri_nlcd_percent,
      hmarg_natural_percent = hmarg_length_natural_percent,
      hmarg_TempVMM08 = hmarg_length_TempVMM08)
  ##area
  } else if(hq == 2){
    points <- points %>%
      dplyr::mutate(
        hmarg = hmarg_area,
        hmarg_urb_nlcd_percent = hmarg_area_urb_nlcd_percent,
        hmarg_agri_nlcd_percent = hmarg_area_agri_nlcd_percent,
        hmarg_natural_percent = hmarg_area_natural_percent,
        hmarg_TempVMM08 = hmarg_area_TempVMM08)
  ##volume
  } else {
    points <- points %>%
      dplyr::mutate(
        hmarg = hmarg_volume,
        hmarg_urb_nlcd_percent = hmarg_volume_urb_nlcd_percent,
        hmarg_agri_nlcd_percent = hmarg_volume_agri_nlcd_percent,
        hmarg_natural_percent = hmarg_volume_natural_percent,
        hmarg_TempVMM08 = hmarg_volume_TempVMM08)
  }


  # wria selection: set benefit to zero if not in the wria of interest (wria_sel)
  if(! 0 %in% wria_sel){
    points <- points %>%
      dplyr::mutate(
        hmarg = ifelse(wria_number %in% wria_sel, hmarg, 0)
        )
  }

  # huc selection: set benefit to zero if not in the hucof interest (huc_sel)
  if(! 0 %in% huc_sel){
    points <- points %>%
      dplyr::mutate(
        hmarg = ifelse(huc_number %in% huc_sel, hmarg, 0)
      )
  }

  # owner selection: set benefit to zero if not owned by the owner of interest (owner_sel)
  if(! 0 %in% owner_sel){
    points <- points %>%

      dplyr::mutate(hmarg = ifelse(
        grepl(paste(owner_sel, collapse = "|"), unique_owner_type_code),
        hmarg, 0))
  }


  # species selection: set benefit to zero if not providing habitat for a species of interest (species_sel)
  if(! "all" %in% species_sel){
  points <- points %>%
    dplyr::mutate(
      species_of_interest = grepl(
        paste(species_sel, collapse = "|"), potential_species, ignore.case = TRUE
        ),
      hmarg = hmarg * species_of_interest
    )
  }

  # adjust cost with user inputs
  if(cost == 2){
    points <- points %>%
      dplyr::mutate(
        cost = cost * mean_construction_cost / mean(cost) + mean_design_cost
      )
  }
  
  # planned barrier selection: set cost to zero if barrier is already planned by user
  if(! 0 %in% barrier_idp){
    points <- points %>%
      dplyr::mutate(cost = ifelse(site_id %in% barrier_idp, 0, cost)
      )
  }

  # objective function inputs
  h <- points %>% dplyr::pull(hmarg)
  urb_per <- points %>% dplyr::pull(hmarg_urb_nlcd_percent)
  ag_per <- points %>% dplyr::pull(hmarg_agri_nlcd_percent)
  nat_per <- points %>% dplyr::pull(hmarg_natural_percent)
  temp <- points %>% dplyr::pull(hmarg_TempVMM08)

  if(obj == 1){
  v <- h
  } else {
  v <- w_urb * urb_per * h + w_ag *  ag_per * h + w_nat * nat_per * h# + w_temp * temp
  v[temp < w_temp[1]] <- 0
  v[temp > w_temp[2]] <- 0
  v[is.na(v)] <- 0 #HOT FIX
  }

  brc <- points %>% dplyr::pull(cost)
  nb <- length(v)
  di <- colSums(D)

  # objective function
  obj <- ROI::L_objective(v)

  # constraints
  bc <- ROI::L_constraint(L = brc, dir = "<=", rhs = budget) #budget
  hc <- ROI::L_constraint(L = diag(nb) - t(D), dir = rep("<=", nb), rhs = 1 - di) #hydrology

  # problem & solution
  # bc and hc
  prob <- ROI::OP(objective = obj,
                  constraints = c(bc, hc),
                  bounds = ROI::V_bound(li = 1 : nb, lb = rep.int(0, nb), ui = 1 : nb, ub = rep.int(1, nb)),
                  types = rep.int("B", nb),
                  maximum = TRUE)

  sol <- ROI::ROI_solve(prob, "glpk",  control = list("verbose" = TRUE, "presolve" = TRUE))

  soln <- as.logical(ROI::solution(sol))

  return(soln)
}

#' @title Map the optimization solution
#' @param leaf_proxy A basemap.
#' @param points A simple features point data frame containing culvert locations and attributes.
#' @param lines A simple features data frame with linestring geometries upstream of culvs
#' @param dslines A simple features data frame with linestring geometries downstream of culvs
#' @param soln A logical vector of TRUE/FALSE values that is the output from solve_opt(.
#' @param marginal_line_ids A vector of line IDs for all lines marginally upstream of each point.
#' @param downstream_line_ids A vector of line IDs for all lines marginally upstream of each point.
#' @param area_sel A vector of WRIA ID numbers of interest.
#' @param subarea_sel A vector of WRIA ID numbers of interest.
#' @param barrier_idp A vector of planned culvert IDs.
#' @return A leaflet map.
#' @export
map_leaflet_opt <- function(
    leaf_proxy,
    points, #culverts
    lines, #lines with linestring geometries upstream of culvs
    dslines, #lines with linestring geometries downstream of culvs
    soln, #output from solve_opt()
    marginal_line_ids, #comids for all lines marginally upstream of each point
    downstream_line_ids, #comids for all lines downstream of each point on main stem
    wria_sel, #wria(s) to run the optimization problem on
    huc_sel, #huc(s) to run the optimization problem on
    barrier_idp #planned barrier IDs
  ){
  
  #Lines to display depend on whether the solution is a null set
  if(sum(soln) == 0){

    #If so, display lines in selected wrias
    if(! 0 %in% wria_sel && ! 0 %in% huc_sel){
    in_sel_area <- points$wria_number %in% wria_sel & points$huc_number %in% huc_sel
    } else if(! 0 %in% wria_sel && 0 %in% huc_sel) {
    in_sel_area <- points$wria_number %in% wria_sel
    } else if(0 %in% wria_sel && ! 0 %in% huc_sel) {
    in_sel_area <- points$huc_number %in% huc_sel
    } else {
    in_sel_area <- base::rep(TRUE, base::nrow(points))
    }
    blocked_lines <- marginal_line_ids[in_sel_area] %>% base::unlist()
    ds_blocked_lines <- downstream_line_ids[in_sel_area] %>% base::unlist()
    leaflet_lines <- lines %>% dplyr::filter(COMID %in% blocked_lines)
    ds_leaflet_lines <- dslines %>% dplyr::filter(COMID %in% ds_blocked_lines)

    if (inherits(sf::st_geometry(leaflet_lines), c("sfc_LINESTRING", "sfc_MULTILINESTRING")) == TRUE){
      
    leaf_proxy <- leaf_proxy %>%
      leafgl::addGlPolylines(data = leaflet_lines,
        color = "#cf6e7d",
        opacity = 0.5,
        group = "blocked_lines"
      )
    } else {
      
      NULL
      
    }

  } else {

  #If not, display lines in wrias that appear int he solution
  soln_wrias <- unique(points[soln, ]$wria_number)
  in_soln_wrias <- points$wria_number %in% soln_wrias

  #First get blocked/unblocked lines
  blocked_lines <- marginal_line_ids[in_soln_wrias] %>% base::unlist()
  ds_blocked_lines <- downstream_line_ids[in_soln_wrias] %>% base::unlist()

  leaflet_lines <- lines %>% dplyr::filter(COMID %in% blocked_lines)
  ds_leaflet_lines <- dslines %>% dplyr::filter(COMID %in% ds_blocked_lines)

  #Define blocked/unblocked
  milp_stream_ids <- marginal_line_ids[soln] %>% base::unlist()
  ds_stream_ids <- downstream_line_ids[soln] %>% base::unlist()

  #Define idp lines
  if(! 0 %in% barrier_idp){
    names(marginal_line_ids) <- points$site_id
    idp_stream_ids <- marginal_line_ids[barrier_idp] %>% base::unlist()
  } else {
    idp_stream_ids <- NULL
  }
  
  soln_lines <- leaflet_lines %>% 
    dplyr::filter(COMID %in% milp_stream_ids) %>% 
    dplyr::mutate(soln_type = ifelse(COMID %in% idp_stream_ids, 1, 0))
  
  # define unblocked lines color pallet
  lines_pal <- leaflet::colorNumeric(c("#2739c7", "#f1e2bA"), 0 : 1)
  
  leaf_proxy <- leaf_proxy %>%
    leafgl::addGlPolylines(
      data = leaflet_lines %>%
        dplyr::filter(!COMID %in% milp_stream_ids),
      color = "#cf6e7d",
      opacity = 0.5,
      group = "blocked_lines"
    ) %>%
    leafgl::addGlPolylines(
      data = soln_lines,
      color = ~lines_pal(soln_type),
      opacity = 0.5,
      group = "unblocked_lines"
    )

  #test for null sets in ds lines. If FALSE draw lines
  testfilter <- ds_leaflet_lines %>% dplyr::filter(COMID %in% ds_stream_ids & !COMID %in% milp_stream_ids)
  if (inherits(sf::st_geometry(testfilter), c("sfc_LINESTRING", "sfc_MULTILINESTRING")) == TRUE){

    leaf_proxy <- leaf_proxy %>%
      leafgl::addGlPolylines(
        data = ds_leaflet_lines %>%
          dplyr::filter(COMID %in% ds_stream_ids & !COMID %in% milp_stream_ids),
        color = "#b0b0b0",
        opacity = 0.25,
        group = "unblocked_lines"
      )
    
  } else {
    NULL
  }
  
  
  }

  # define project color pallet
  pal <- leaflet::colorNumeric(c("#d9a1a0", "#91afeb", "#f1e2b9"), 0 : 2)

  # define match border pallet 
  match_pal <- leaflet::colorFactor(palette = c("black", "transparent"), domain = c(TRUE, FALSE), ordered = TRUE)
  
  # define idp points
  if(! 0 %in% barrier_idp){
    idp_point <- points$site_id %in% barrier_idp
    soln <- soln + as.numeric(idp_point)
  } 
  
  #Add culverts
  leaf_proxy <- leaf_proxy %>%
    leaflet::addCircleMarkers(
      data = points,
      lng = ~ site_longitude,
      lat = ~ site_latitude,
      group = "selected_culverts",
      radius = 5,
      weight = 1.5,
      color = ~match_pal(bad_match),
      fillColor = ~pal(soln),
      fillOpacity = 1,
      opacity = 1,
      clusterOptions = leaflet::markerClusterOptions(
        iconCreateFunction = htmlwidgets::JS("function (cluster) {
          var childCount = cluster.getChildCount();
          var c = ' marker-cluster-';
          if (childCount < 100) {
          c = 'rgba(241, 226, 185, 255);'
          } else if (childCount < 1000) {
          c = 'rgba(197, 247, 244, 255);'
          } else {
          c = 'rgba(232, 169, 157, 255);'
          }
         return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>',
           className: 'marker-cluster', iconSize: new L.Point(40, 40) });}"),

        spiderfyOnMaxZoom = FALSE,
        disableClusteringAtZoom = 10
      ),
      popup = ~popup
    )

  return(leaf_proxy)
}
