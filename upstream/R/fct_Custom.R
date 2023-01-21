#Convert custom portfolio to TRUE/FALSE vector
get_points_sel_custom <- function(
    points, 
    prtf_cust){
  
  points_sel <- points$site_id %in% prtf_cust
  
  return(points_sel)
}

# Map the custom solution
map_leaflet_custom <- function(
    leaf_proxy,
    points, #culverts
    lines, #lines with linestring geometries 
    prtf_cust, #inputs from mod_Custom
    E, #full connectivity matrix
    marginal_line_ids #comids for all lines marginally upstream of each point
  ){
  
  #Convert custom portfolio to TRUE/FALSE vector
  cust <- points$site_id %in% prtf_cust
  
  #Identify points in custom plan that unlock habitat
  h_inc <- lapply(1 : length(prtf_cust), 
    FUN = function(x) ifelse(sum(E[, prtf_cust[x]]) == 0, TRUE,
      ifelse(sum(which(E[, prtf_cust[x]] == 1) %in% prtf_cust) == 
      length(which(E[, prtf_cust[x]] == 1)), TRUE, FALSE))) %>% 
    do.call("rbind", .) %>% 
    as.logical()
  
  #Crop lines to wrias in plan regardless of whether they unlock habitat
  cust_wrias <- unique(points[points$site_id %in% prtf_cust, ]$wria_number)
  in_cust_wrias <- points$wria_number %in% cust_wrias
  
  #First get blocked/unblocked lines
  blocked_lines <- marginal_line_ids[in_cust_wrias] %>% unlist()
  leaflet_lines <- lines %>% dplyr::filter(COMID %in% blocked_lines)
  names(marginal_line_ids) <- points$site_id
  milp_stream_ids <- marginal_line_ids[prtf_cust[h_inc]] %>% unlist()
  
  #Barrier color
  pal <- leaflet::colorNumeric(c("#b80000", "#179848"), 0 : 1)
  
  #Add lines 
  leaf_proxy <- leaf_proxy %>%
    leafgl::addGlPolylines(data = leaflet_lines %>%
      dplyr::filter(FCODE != 55800, !COMID %in% milp_stream_ids),
      color = "#d46666",
      opacity = 0.5,
      group = "blocked_lines"
      )  %>%
      leafgl::addGlPolylines(
        data = leaflet_lines %>%
          dplyr::filter(COMID %in% milp_stream_ids),
        color = "#3cdd78",
        opacity = 0.5,
        group = "unblocked_lines"
      ) 

  #Add culverts
  leaf_proxy <- leaf_proxy %>%  
    leaflet::addCircleMarkers(
      data = points,
      lng = ~ site_longitude,
      lat = ~ site_latitude,
      group = "selected_culverts",
      radius = 5,
      weight = 1.5,
      color = ~pal(cust),
      fillOpacity = 1,
      opacity = 1,
      clusterOptions = leaflet::markerClusterOptions(
        iconCreateFunction = htmlwidgets::JS("function (cluster) {    
          var childCount = cluster.getChildCount();  
          if (childCount < 100) {  
          c = 'rgba(204, 252, 255, 1.0);'
          } else if (childCount < 1000) {  
          c = 'rgba(237, 192, 181, 1);'  
          } else { 
          c = 'rgba(164, 164, 243, 1);'  
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