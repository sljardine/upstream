#' @title Convert projects in custom portfolio to TRUE/FALSE vector
#' @param points A simple features point data frame containing culvert locations and attributes.
#' @param prtf_cust A vector of point IDs for selected points.
get_points_sel_custom <- function(
    points, #projects
    prtf_cust #inputs from mod_Custom (projects in plan)
    ){

  points_sel <- points$site_id %in% prtf_cust

}

#' @title Convert projects in custom portfolio adding habitat to TRUE/FALSE vector
#' @param points A simple features point data frame containing culvert locations and attributes.
#' @param prtf_cust A vector of point IDs for selected points.
#' @param barrier_idp A vector of planned culvert IDs.
#' @param E A full connectivity matrix.
#' @return A simple features point data frame containing culvert location and attributes for selected points.
#' @export
get_points_sel_hab_custom <- function(
    points, #projects
    prtf_cust, #inputs from mod_Custom (projects in plan)
    barrier_idp, #inputs from mod_Custom (projects to ignore)
    E #connectivity
    ){

  #Convert custom portfolio to TRUE/FALSE vector
  if(! 0 %in% barrier_idp){
    prtf_cust_idp <- c(prtf_cust, barrier_idp)
  } else {
    prtf_cust_idp <- prtf_cust
  }
  cust_sel <- prtf_cust_idp %in% prtf_cust

  #Identify points in custom plan that unlock habitat
  h_inc <- lapply(
    1 : length(prtf_cust_idp),
    FUN = function(x) ifelse(
      sum(E[, prtf_cust_idp[x]]) == 0, TRUE,
      ifelse(
        sum(points$site_id[which(E[, prtf_cust_idp[x]] == 1)] %in% prtf_cust_idp) ==
          length(which(E[, prtf_cust_idp[x]] == 1)), TRUE, FALSE)
    )
  ) %>%
    do.call("rbind", .) %>%
    as.logical()

  points_prtf_cust <- points$site_id %in% prtf_cust
  points_sel <- points$site_id %in% prtf_cust_idp[which(as.logical(h_inc * cust_sel))]

  return(points_sel)
}

#' @title Map the custom solution
#' @param leaf_proxy A basemap.
#' @param points A simple features point data frame containing culvert locations and attributes.
#' @param lines A simple features data frame with linestring geometries.
#' @param dslines A simple features data frame with linestring geometries downstream of culvs
#' @param prtf_cust A vector of point IDs for selected points.
#' @param E A full connectivity matrix.
#' @param marginal_line_ids A vector of line IDs for all lines marginally upstream of each point.
#' @param downstream_line_ids A vector of line IDs for all lines downstream of each point.
#' @param barrier_idp A vector of planned culvert IDs.
#' @return A leaflet map.
#' @export
map_leaflet_custom <- function(
    leaf_proxy,
    points, #culverts
    lines, #lines with linestring geometries
    dslines, #lines with linestring geometries downstream of culvs
    prtf_cust, #inputs from mod_Custom
    E, #full connectivity matrix
    marginal_line_ids, #comids for all lines marginally upstream of each point
    downstream_line_ids, #comids for all lines downstream of each point
    barrier_idp #planned barrier IDs
){

  #Convert custom portfolio to TRUE/FALSE vector
  cust <- points$site_id %in% prtf_cust
  if(! 0 %in% barrier_idp){
    idp <- (points$site_id %in% barrier_idp) * 2
    prtf_cust_idp <- c(prtf_cust, barrier_idp)
  } else {
    idp <- rep(0, base::nrow(points))
    prtf_cust_idp <- prtf_cust
  }
   cust_idp <- cust + idp

   cust_sel <- prtf_cust_idp %in% prtf_cust
   idp_sel <- prtf_cust_idp %in% barrier_idp


  #Identify points in custom plan that unlock habitat
  h_inc <- lapply(
    1 : length(prtf_cust_idp),
    FUN = function(x) ifelse(
      sum(E[, prtf_cust_idp[x]]) == 0, TRUE,
      ifelse(
        sum(points$site_id[which(E[, prtf_cust_idp[x]] == 1)] %in% prtf_cust_idp) ==
          length(which(E[, prtf_cust_idp[x]] == 1)), TRUE, FALSE)
      )
    ) %>%
    do.call("rbind", .) %>%
    as.logical()

  #Crop lines to wrias in plan regardless of whether they unlock habitat
  cust_wrias <- unique(points[points$site_id %in% prtf_cust_idp, ]$wria_number)
  in_cust_wrias <- points$wria_number %in% cust_wrias

  #First get blocked/unblocked lines
  blocked_lines <- marginal_line_ids[in_cust_wrias] %>% base::unlist()
  ds_blocked_lines <- downstream_line_ids[in_cust_wrias] %>% base::unlist()

  leaflet_lines <- lines %>% dplyr::filter(COMID %in% blocked_lines)
  ds_leaflet_lines <- dslines %>% dplyr::filter(COMID %in% ds_blocked_lines)

  #Defined blocked/unblocked
  names(marginal_line_ids) <- points$site_id
  names(downstream_line_ids) <- points$site_id
  cust_stream_ids <- marginal_line_ids[prtf_cust_idp[which(as.logical( h_inc * cust_sel))]] %>% base::unlist()
  ds_stream_ids <- downstream_line_ids[prtf_cust_idp[h_inc]] %>% base::unlist()

  if(! 0 %in% barrier_idp){
    idp_stream_ids <- marginal_line_ids[prtf_cust_idp[which(as.logical( h_inc * idp_sel))]] %>% base::unlist()
  } else {
    idp_stream_ids <- NULL
  }

  leaf_proxy <- leaf_proxy %>%
    leafgl::addGlPolylines(
      data = leaflet_lines %>%
        dplyr::filter(!COMID %in% cust_stream_ids & !COMID %in% idp_stream_ids),
      color = "#cf6e7d",
      opacity = 0.5,
      group = "blocked_lines"
    )

  #draw unblocked lines if there are any
  draw_ub_lines <- leaflet_lines %>% dplyr::filter(COMID %in% cust_stream_ids)
  if (inherits(sf::st_geometry(draw_ub_lines), c("sfc_LINESTRING", "sfc_MULTILINESTRING")) == TRUE){

    leaf_proxy <- leaf_proxy %>%
      leafgl::addGlPolylines(
        data = leaflet_lines %>%
          dplyr::filter(COMID %in% cust_stream_ids),
        color = "#2739c7",
        opacity = 0.5,
        group = "unblocked_lines"
      ) } else {
        NULL
      }

    #draw idp lines if there are any
    draw_idp_lines <- leaflet_lines %>% dplyr::filter(COMID %in% idp_stream_ids)
    if (inherits(sf::st_geometry(draw_idp_lines ), c("sfc_LINESTRING", "sfc_MULTILINESTRING")) == TRUE){

    leaf_proxy <- leaf_proxy %>%
      leafgl::addGlPolylines(
        data = leaflet_lines %>%
          dplyr::filter(COMID %in% idp_stream_ids),
        color = "#f1e2bA",
        opacity = 0.5,
        group = "unblocked_lines"
      )
    } else {
      NULL
    }


  #draw downstream lines if there are any
  draw_ds_lines <- ds_leaflet_lines %>% dplyr::filter(COMID %in% ds_stream_ids & !COMID %in% cust_stream_ids & !COMID %in% idp_stream_ids)
  if (inherits(sf::st_geometry(draw_ds_lines), c("sfc_LINESTRING", "sfc_MULTILINESTRING")) == TRUE){

    leaf_proxy <- leaf_proxy %>%
      leafgl::addGlPolylines(
        data = ds_leaflet_lines %>%
          dplyr::filter(COMID %in% ds_stream_ids & !COMID %in% cust_stream_ids & !COMID %in% idp_stream_ids),
        color = "#b0b0b0",
        opacity = 0.25,
        group = "unblocked_lines"
      ) } else {
        NULL
      }


  # define project color pallet
  pal <- leaflet::colorNumeric(c("#d9a1a0", "#91afeb", "#f1e2b9"), 0 : 2)

  # define match border pallet
  match_pal <- leaflet::colorFactor(palette = c("black", "transparent"), domain = c(TRUE, FALSE), ordered = TRUE)

  #Add culverts
  leaf_proxy <- leaf_proxy %>%
    leaflet::clearControls() %>%
    leaflet::addCircleMarkers(
      data = points,
      lng = ~ site_longitude,
      lat = ~ site_latitude,
      group = "selected_culverts",
      radius = 5,
      weight = 1.5,
      color = ~match_pal(bad_match),
      fillColor = ~pal(cust_idp),
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
