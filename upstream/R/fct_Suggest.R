# Solve the optimization problem
solve_opt <- function(
    #Inputs
  points, #points with variables: hmarg_net, cost, and wria_number
  budget, #budget constraint
  D, #connectivity matrix
  wria_sel = NULL #wria to run the optimization problem on
){

  # set benefit to zero if not in the wria of interest (wria_sel)
  if(!is.null(wria_sel)){
    points <- points %>%
      dplyr::mutate(hmarg_net = ifelse(wria_number == wria_sel, hmarg_net, 0))
  }

  # inputs
  v <- points %>% dplyr::pull(hmarg_net)
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
  
  sol <- ROI::ROI_solve(prob, "glpk",
                         control = list("verbose" = TRUE, "presolve" = TRUE))

  soln <- as.logical(ROI::solution(sol))
  
  return(soln)
}

# Map the optimization solution
map_leaflet_opt <- function(
    leaf_proxy,
    points, #culverts
    lines, #lines with linestring geometries 
    soln, #output from solve_opt()
    marginal_line_ids #comids for all lines marginally upstream of each point
  ){
  

  #Crop lines to wrias in solution
  soln_wrias <- unique(points[soln, ]$wria_number)
  in_soln_wrias <- points$wria_number %in% soln_wrias
  #First get blocked/unblocked lines
  blocked_lines <- marginal_line_ids[in_soln_wrias] %>% unlist()
  leaflet_lines <- lines %>% filter(COMID %in% blocked_lines)
  #Defined blocked/unblocked
  milp_stream_ids <- marginal_line_ids[soln] %>% unlist()
  
  #Barrier color
  pal <- leaflet::colorNumeric(c("#b80000", "#179848"), 0 : 1)
  
  #Add lines  
  leaf_proxy <- leaf_proxy %>% 
    leafgl::addGlPolylines(data = leaflet_lines %>%  
      filter(FCODE != 55800, !COMID %in% milp_stream_ids),
      color = "#d46666", 
      opacity = 0.5
      ) %>% 
      leafgl::addGlPolylines(
        data = leaflet_lines %>% 
          filter(COMID %in% milp_stream_ids),
        color = "#3cdd78", 
        opacity = 0.5
      ) 
    
 
  return(leaf_proxy)
}