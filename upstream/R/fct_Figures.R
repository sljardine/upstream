#' get bounding box for selected wrias
#'
#' @param cWRIAs
#'
#' @return A numeric vector of the bounding box for the provided WRIA simple features
#' @export
get_wria_bounding_box <- function(area_sel){
  sfW <- wrias %>%
    dplyr::filter(WRIA_NR %in% area_sel)

  bbox <- sfW %>%
    sf::st_bbox() %>%
    as.vector()

  return(bbox)
}

#' @title get leaflet map
#'
#' @return none
#' @export
get_leaflet_map <- function(){
  # init the map
  m <- wrias %>%
    leaflet::leaflet() %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Grayscale", options = leaflet::providerTileOptions(minZoom = 7))  %>%
    leaflet::addScaleBar("bottomleft")
  
  # add wria polygons
  m <- m %>%
    leaflet::addPolygons(
      popup =  ~ paste0(
        "<b>WRIA Name:</b> ",
        WRIA_NM,
        "<br>",
        "<b>WRIA Number:</b> ",
        WRIA_NR
      ),
      weight = 1.5,
      opacity = 1,
      color = "#1c1cff",
      fillColor = "transparent"
    )
  
  # add circle markers (could use addGlPoints maybe...)
  m <- m %>%
    leaflet::addCircleMarkers(
      data = culverts_cmb,
      group = 'culverts',
      radius = 5,
      weight = 1.5,
      color = 'black',
      opacity = 1,
      fillColor = 'grey',
      fillOpacity = 1,
      clusterOptions = leaflet::markerClusterOptions(
        spiderfyOnMaxZoom = FALSE,
        disableClusteringAtZoom = 9
      ),
      popup = ~popup
    )
}

reset_map <- function(leaf_proxy){
  leaf_proxy %>%
    leaflet::clearGroup('culverts') %>%
    leaflet::clearGroup('selected_wria') %>%
    leaflet::addCircleMarkers(
      data = culverts_cmb,
      group = 'culverts',
      radius = 5,
      weight = 1.5,
      color = 'black',
      opacity = 1,
      fillColor = 'grey',
      fillOpacity = 1,
      clusterOptions = leaflet::markerClusterOptions(
        spiderfyOnMaxZoom = FALSE,
        disableClusteringAtZoom = 9
      ),
      popup = ~popup
    )
  
  # selected wria bounding box
  bbox <- get_wria_bounding_box(wrias$WRIA_NR)
  
  # zoom map to selected wrias
  leaf_proxy %>%
    leaflet::flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4])
}

#' @title update map WRIA labels
#'
#' @param leaf_proxy leaflet proxy reference
#' @param zoomLevel zoom level for leaflet map
#' @param r reactive values object with app input values
#'
#' @return none
#' @export
update_map_WRIA_labels <- function(leaf_proxy, zoom_level, area_sel, owner_sel){
  # get WRIA centroids
  sfWC <- wrias %>%
    sf::st_drop_geometry() %>%
    dplyr::select(WRIA_NR, WRIA_NM) %>%
    dplyr::bind_cols(
      wrias %>%
        dplyr::select() %>%
        sf::st_centroid() %>%
        sf::st_coordinates()
    )

  # summarize culvert count by WRIA
  dfC <- culverts_cmb %>% sf::st_drop_geometry()
  if(!is.null(area_sel)){
    dfC <- dfC %>% dplyr::filter(wria_number %in% area_sel)
  }
  if(!is.null(owner_sel)){
    dfC <- dfC %>% dplyr::filter(owner_type_code %in% owner_sel)
  }

  sfWC <- sfWC %>%
    dplyr::inner_join(
      dfC %>%
        dplyr::group_by(wria_number) %>%
        dplyr::summarize(Count = dplyr::n(), .groups = 'drop'),
      by = c('WRIA_NR' = 'wria_number')
    )

  # create label based on zoom
  if(is.null(zoom_level)){
    sfWC <- sfWC %>% dplyr::mutate(Label = Count)
  } else {
    if(zoom_level < 9){
      sfWC <- sfWC %>% dplyr::mutate(Label = Count)
    } else {
      sfWC <- sfWC %>% dplyr::mutate(Label = paste0(WRIA_NM, ' (', Count, ')'))
    }
  }

  # update map
  leaf_proxy %>%
    leaflet::clearGroup('wria') %>%
    leaflet::addLabelOnlyMarkers(
      data = sfWC, lat = ~Y, lng = ~X,
      group = 'wria',
      label = ~Label,
      labelOptions = leaflet::labelOptions(textsize = '16px', noHide = TRUE, direction = 'center', textOnly = TRUE)
    )
}

#' @title update map selected wria polygons
#'
#' @param leaf_proxy leaflet proxy reference
#' @param r reactive values object with app input values
#'
#' @return none
#' @export
update_map_selected_WRIA_polygons <- function(leaf_proxy, area_sel){
  sfW <- wrias %>% dplyr::filter(WRIA_NR %in% area_sel)

  leaf_proxy %>%
    leaflet::clearGroup('selected_wria') %>%
    leaflet::addPolygons(
      data = sfW,
      group = 'selected_wria',
      weight = 5,
      opacity = 1,
      color = "#1c1cff",
      fillColor = "transparent"
    )
}

#' @title update map culvert markers
#'
#' @param leaf_proxy leaflet proxy reference
#' @param r reactive values object with app input values
#'
#' @return none
#' @export
update_map_culvert_markers <- function(leaf_proxy, area_sel, owner_sel, color_variable, highlight, barrier_ids){
  # set null variables for initial map draw
  if(is.null(color_variable)){color_variable <- 'none'} else {color_variable <- color_variable}
  if(is.null(area_sel)){area_sel <- wrias$WRIA_NR} else {area_sel <- area_sel}
  if(is.null(owner_sel)){owner_sel <- c(1:9, 11:12)} else {owner_sel <- owner_sel}
  if(is.null(highlight)){highlight <- 0} else {highlight <- highlight}

  # filter culverts to selected wrias
  sfC <- culverts_cmb %>%
    dplyr::filter(wria_number %in% area_sel)

  # filter by owner class
  cSiteIds <- c()
  if('0' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::pull(site_id))}
  if('1' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_city) %>% dplyr::pull(site_id))}
  if('2' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_county) %>% dplyr::pull(site_id))}
  if('3' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_federal) %>% dplyr::pull(site_id))}
  if('4' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_private) %>% dplyr::pull(site_id))}
  if('5' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_state) %>% dplyr::pull(site_id))}
  if('6' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_tribal) %>% dplyr::pull(site_id))}
  if('7' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_other) %>% dplyr::pull(site_id))}
  if('8' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_port) %>% dplyr::pull(site_id))}
  if('9' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_drainage_district) %>% dplyr::pull(site_id))}
  if('11' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_irrigation_district) %>% dplyr::pull(site_id))}
  if('12' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_unknown) %>% dplyr::pull(site_id))}
  sfC <- sfC %>% dplyr::filter(site_id %in% cSiteIds)

  # replace owner_type_code with name
  if(color_variable == 'owner_type_code'){
    #sfC <- replaceOwnerCodeWithName(sfC)
    sfC <- sfC %>%
      dplyr::select(-owner_type_code) %>%
      dplyr::rename(owner_type_code = owner_type_name_short)
  }

  # replace wria_number with name
  if(color_variable == 'wria_number'){
    sfC <- replace_WRIA_number_with_name(sfC, wrias)
  }

  # assign color variable to C
  if(color_variable %in% c('none', '')){
    sfC$C <- 'none'
  } else {
    sfC$C <- sfC %>% dplyr::pull(color_variable)
  }

  # palette
  # colorRampPalette(brewer.pal(10, 'Spectral'))(11)
  if(color_variable %in% c('none', '')){
    pal <- function(x){return('grey')}
  } else if(color_variable == 'owner_type_code'){
    pal <- leaflet::colorFactor(
      palette = c(rev(c('#5E4FA2','#3682BA','#5BB6A9','#96D4A4','#CEEB9C','#F2EA91','#FDCC7A','#FA9A58','#ED6345','#CF374D', '#9E0142')), '#B8B8B8'),
      domain = c('City', 'County', 'Drainage District', 'Federal', 'Irrigation District', 'Other', 'Port', 'Private', 'State', 'Tribal', 'Unknown', 'Multiple'),
      ordered = TRUE
    )
  } else if(color_variable == 'wria_number'){
    pal <- leaflet::colorFactor(
      palette = c('#9E0142', '#B71C47', '#D0384D', '#E04F4A', '#EE6445', '#F67E4B', '#FA9C58', '#FDB768', '#FDCD7B', '#FEE28F', '#FEF0A7', '#FFFFBF', '#F3FAAD', '#E8F59B', '#D0EC9C', '#B5E1A1', '#98D5A4', '#78C9A4', '#5CB7A9', '#449DB4', '#3682BA', '#4A68AE', '#5E4FA2'),
      domain = c('Cedar - Sammamish', 'Chambers - Clover', 'Deschutes', 'Duwamish - Green', 'Elwha - Dungeness', 'Island', 'Kennedy - Goldsborough', 'Kitsap', 'Lower Chehalis', 'Lower Skagit - Samish', 'Lyre - Hoko', 'Nisqually', 'Nooksack', 'Puyallup - White', 'Queets - Quinault', 'Quilcene - Snow', 'San Juan', 'Skokomish - Dosewallips', 'Snohomish', 'Soleduc', 'Stillaguamish', 'Upper Chehalis', 'Upper Skagit'),
      ordered = TRUE
    )
  } else {
    pal <- leaflet::colorNumeric(
      palette = 'Spectral',
      domain = sfC$C %>% range(),
      reverse = TRUE
    )
  }

  # set barrier ids
  if(is.null(barrier_ids) | highlight == 0){
    cBarrierIds <- ''
  } else {
    cBarrierIds <- barrier_ids
  }

  # calculate highlighted variable in culverts data frame
  sfC <- sfC %>%
    dplyr::mutate(IsHighlighted = dplyr::case_when(site_id %in% cBarrierIds ~ 'Highlighted', TRUE ~ 'Not Highlighted')) %>%
    dplyr::arrange(IsHighlighted)

  # define stroke palette function
  strokePal <- leaflet::colorFactor(palette = c('#00ffff', 'black'), domain = c('Highlighted', 'Not Highlighted'), ordered = TRUE)

  # remove the culverts from the map
  leaf_proxy %>% leaflet::clearGroup('culverts')

  # return if no culverts to draw
  if(nrow(sfC) == 0){return()}

  # add culverts to map if zoomed in enough
  leaf_proxy %>%
    leaflet::addCircleMarkers(
      data = sfC,
      group = 'culverts',
      radius = 5,
      weight = 1.5,
      color = ~strokePal(IsHighlighted),
      opacity = 1,
      fillColor = ~pal(C),
      fillOpacity = 1,
      clusterOptions = leaflet::markerClusterOptions(
        spiderfyOnMaxZoom = FALSE,
        disableClusteringAtZoom = 9
      ),
      popup = ~popup
    )
}

#' @title filter and format culverts for histogram
#'
#' @param sfC simple features data frame of culvert data
#' @param r reactive values object with app input values
#'
#' @return data frame of culvert data formatted to make histogram in ggplot
#' @export
filter_and_format_culverts_for_histogram <- function(sfC, area_sel, owner_sel, color_variable, histogram_variable){
  # filter by area
  sfC <- sfC %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(wria_number %in% as.integer(area_sel))

  # filter by owner class
  cSiteIds <- c()
  if('0' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::pull(site_id))}
  if('1' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_city) %>% dplyr::pull(site_id))}
  if('2' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_county) %>% dplyr::pull(site_id))}
  if('3' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_federal) %>% dplyr::pull(site_id))}
  if('4' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_private) %>% dplyr::pull(site_id))}
  if('5' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_state) %>% dplyr::pull(site_id))}
  if('6' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_tribal) %>% dplyr::pull(site_id))}
  if('7' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_other) %>% dplyr::pull(site_id))}
  if('8' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_port) %>% dplyr::pull(site_id))}
  if('9' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_drainage_district) %>% dplyr::pull(site_id))}
  if('11' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_irrigation_district) %>% dplyr::pull(site_id))}
  if('12' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_unknown) %>% dplyr::pull(site_id))}
  sfC <- sfC %>% dplyr::filter(site_id %in% cSiteIds)

  # this swaps wria_name if X = wria_number
  if(histogram_variable == 'wria_number' | color_variable == 'wria_number'){
    sfC <- replace_WRIA_number_with_name(sfC, wrias)
  }

  # this swaps owner names with code
  if(histogram_variable == 'owner_type_code' | color_variable == 'owner_type_code'){
    sfC <- sfC %>%
      dplyr::select(-owner_type_code) %>%
      dplyr::rename(owner_type_code = owner_type_name_short)
  }

  # create dummy variable if color variable is 'none'
  if(color_variable == 'none'){
    sfC[color_variable] <- 'none'
  }

  # calculate new variables
  sfC$X <- sfC %>% dplyr::pull(histogram_variable)
  sfC$C <- sfC %>% dplyr::pull(color_variable)

  # select variables
  sfC <- sfC %>% dplyr::select(site_id, X, C)

  # this splits the X variable at commas into rows when X = potential_species
  if(is.character(sfC$X)){
    sfC <- sfC %>% purrr::pmap_dfr(function(site_id, X, C){
      cX <- strsplit(X, ',', fixed = TRUE)[[1]]
      data.frame(
        site_id = site_id,
        X = cX,
        C = C
      )
    })
  }

  if(color_variable == 'barrier_count'){
    sfC <- sfC %>% dplyr::mutate(C = as.factor(C))
  }

  return(sfC)
}

#' @title filter and format culverts for explore tab scatterplot
#'
#' @param sfC simple features data frame of culvert data
#' @param r reactive values object with app input values
#'
#' @return data frame of culvert data formatted to make scatterplot in ggplot
#' @export
filter_and_format_culverts_for_scatterplot <- function(sfC, area_sel, owner_sel, x_axis_variable, y_axis_variable, color_variable){
  # filter by area
  sfC <- sfC %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(wria_number %in% as.integer(area_sel))

  # filter by owner class
  cSiteIds <- c()
  if('0' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::pull(site_id))}
  if('1' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_city) %>% dplyr::pull(site_id))}
  if('2' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_county) %>% dplyr::pull(site_id))}
  if('3' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_federal) %>% dplyr::pull(site_id))}
  if('4' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_private) %>% dplyr::pull(site_id))}
  if('5' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_state) %>% dplyr::pull(site_id))}
  if('6' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_tribal) %>% dplyr::pull(site_id))}
  if('7' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_other) %>% dplyr::pull(site_id))}
  if('8' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_port) %>% dplyr::pull(site_id))}
  if('9' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_drainage_district) %>% dplyr::pull(site_id))}
  if('11' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_irrigation_district) %>% dplyr::pull(site_id))}
  if('12' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_unknown) %>% dplyr::pull(site_id))}
  sfC <- sfC %>% dplyr::filter(site_id %in% cSiteIds)

  # this swaps wria_name if X = wria_number
  if(x_axis_variable == 'wria_number' | y_axis_variable == 'wria_number' | color_variable == 'wria_number'){
    sfC <- replace_WRIA_number_with_name(sfC, wrias)
  }

  # this swaps owner names with code
  if(x_axis_variable == 'owner_type_code' | y_axis_variable == 'owner_type_code' | color_variable == 'owner_type_code'){
    sfC <- sfC %>%
      dplyr::select(-owner_type_code) %>%
      dplyr::rename(owner_type_code = owner_type_name_short)
  }

  # add dummy color variable if color_variable = 'none'
  if(color_variable == 'none'){sfC[color_variable] <- 'none'}

  # calculate new variables
  sfC$X <- sfC %>% dplyr::pull(x_axis_variable)
  sfC$Y <- sfC %>% dplyr::pull(y_axis_variable)
  sfC$C <- sfC %>% dplyr::pull(color_variable)

  # select the variables
  sfC <- sfC %>% dplyr::select(site_id, X, Y, C)

  # this splits the X variable at commas into rows when X = potential_species
  if(is.character(sfC$X)){
    sfC <- sfC %>% purrr::pmap_dfr(function(site_id, X, Y, C){
      cX <- strsplit(X, ',', fixed = TRUE)[[1]]
      data.frame(
        site_id = site_id,
        X = cX,
        Y = Y,
        C = C
      )
    })
  }

  # this splits the Y variable at commas into rows when Y = potential_species
  if(is.character(sfC$Y)){
    sfC <- sfC %>% purrr::pmap_dfr(function(site_id, X, Y, C){
      cY <- strsplit(Y, ',', fixed = TRUE)[[1]]
      data.frame(
        site_id = site_id,
        X = X,
        Y = cY,
        C = C
      )
    })
  }

  return(sfC)
}

#' @title figure explore tab scatterplot
#'
#' @param sfC simple features data frame of culvert data
#' @param r reactive values object with app input values
#'
#' @return ggplot object of culvert data scatterplot
#' @export
figure_scatterplot <- function(sfC, x_axis_variable, y_axis_variable, color_variable, x_jitter, y_jitter, highlight, barrier_ids, plot_xmin, plot_xmax, plot_ymin, plot_ymax){
  # set the barrier ids to '' if null
  if(is.null(barrier_ids)){
    cBarrierIds <- ''
  } else {
    cBarrierIds <- barrier_ids
  }

  # calculate highlight variable
  sfC$IsHighlighted <- 'N'
  if(highlight == 2){
    sfC <- sfC %>%
      dplyr::mutate(IsHighlighted = dplyr::case_when(site_id %in% cBarrierIds ~ 'Y', TRUE ~ 'N')) %>%
      dplyr::arrange(IsHighlighted)
  }

  # init the ggplot
  ggP <- sfC %>%
    ggplot2::ggplot(ggplot2::aes(x = X, y = Y, fill = C, color = IsHighlighted)) +
    ggplot2::geom_jitter(width = x_jitter, height = y_jitter, alpha = .9, stroke = 1.3, size = 3.5, pch = 21) +
    #ggplot2::scale_y_continuous(labels = function(x) formatC(x, width = 10)) +
    ggplot2::scale_color_manual(values = c('N' = 'black', 'Y' = '#00ffff')) +
    ggplot2::guides(color = 'none') +
    ggplot2::xlab(get_pretty_variable_name(x_axis_variable)) +
    ggplot2::ylab(get_pretty_variable_name(y_axis_variable)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16, family = 'mono', face = 'bold'),
      legend.title = ggplot2::element_blank(),
      legend.key.width = ggplot2::unit(55, 'native')
    )

  # y axis variable
  if(y_axis_variable %in% c('owner_type_code', 'wria_number', 'potential_species')){
    ggP <- ggP + ggplot2::scale_y_discrete(labels = function(x) abbreviate(x, 10) %>% sprintf(fmt = '%10s'))
  } else {
    ggP <- ggP + ggplot2::scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ',', scientific = FALSE) %>% sprintf(fmt = '%10s'))
  }

  # x axis variable
  if(x_axis_variable %in% c('owner_type_code', 'wria_number', 'potential_species')){
    ggP <- ggP + ggplot2::scale_x_discrete(labels = function(x) abbreviate(x, 10) %>% sprintf(fmt = '%10s'))
  } else if(!x_axis_variable %in% c('owner_type_code', 'wria_number', 'potential_species')){
    ggP <- ggP + ggplot2::scale_x_continuous(labels = function(x) prettyNum(x, big.mark = ',', scientific = FALSE) %>% sprintf(fmt = '%10s'))
  }

  # color (fill) scale and legend
  if(color_variable == 'none'){
    ggP <- ggP +
      ggplot2::scale_fill_manual(values = c('none' = 'grey')) +
      ggplot2::theme(legend.position = 'none')
  } else if (color_variable %in% c('owner_type_code', 'wria_number')) {
    # discrete variables
    if(color_variable == 'owner_type_code'){
      # colorRampPalette(brewer.pal(10, 'Spectral'))(11)
      scaleFill <- ggplot2::scale_fill_manual(
        values = c(
          'City' = '#9E0142',
          'County' = '#CF374D',
          'Drainage District' = '#ED6345',
          'Federal' = '#FA9A58',
          'Irrigation District' = '#FDCC7A',
          'Other' = '#F2EA91',
          'Port' = '#CEEB9C',
          'Private' = '#96D4A4',
          'State' = '#5BB6A9',
          'Tribal' = '#3682BA',
          'Unknown' = '#5E4FA2',
          'Multiple' = '#B8B8B8'
        ),
        drop = TRUE, limits = force
      )
    } else if(color_variable == 'wria_number') {
      scaleFill <- ggplot2::scale_fill_manual(
        values = c(
          'Cedar - Sammamish' = '#9E0142',
          'Chambers - Clover' = '#B71C47',
          'Deschutes' = '#D0384D',
          'Duwamish - Green' = '#E04F4A',
          'Elwha - Dungeness' = '#EE6445',
          'Island' = '#F67E4B',
          'Kennedy - Goldsborough' = '#FA9C58',
          'Kitsap' = '#FDB768',
          'Lower Chehalis' = '#FDCD7B',
          'Lower Skagit - Samish' = '#FEE28F',
          'Lyre - Hoko' = '#FEF0A7',
          'Nisqually' = '#FFFFBF',
          'Nooksack' = '#F3FAAD',
          'Puyallup - White' = '#E8F59B',
          'Queets - Quinault' = '#D0EC9C',
          'Quilcene - Snow' = '#B5E1A1',
          'San Juan' = '#98D5A4',
          'Skokomish - Dosewallips' = '#78C9A4',
          'Snohomish' = '#5CB7A9',
          'Soleduc' = '#449DB4',
          'Stillaguamish' = '#3682BA',
          'Upper Chehalis' = '#4A68AE',
          'Upper Skagit' =  '#5E4FA2'
        ),
        drop = TRUE, limits = force
      )
    }
    ggP <- ggP +
      scaleFill +
      ggplot2::theme(
        legend.position = c(.99, .95),
        legend.direction = 'vertical',
        legend.justification = c(1, 1),
        legend.box.background = ggplot2::element_rect(color = 'darkgrey')
      )
  } else {
    # continuous variables
    ggP <- ggP +
      ggplot2::scale_fill_gradientn(
        colors = c('#5E4FA2','#3288BD','#66C2A5','#ABDDA4','#E6F598','#FFFFBF','#FEE08B','#FDAE61','#F46D43','#D53E4F','#9E0142'),
        labels = function(x) prettyNum(x, big.mark = ',', scientific = FALSE)
      ) +
      ggplot2::theme(
        legend.position = c(.99, .95),
        legend.direction = 'vertical',
        legend.justification = c(1, 1),
        legend.key.height = ggplot2::unit(1, 'cm'),
        legend.box.background = ggplot2::element_rect(color = 'grey')
      )
  }

  # x axis tick label orientation
  if(x_axis_variable %in% c('wria_number', 'owner_type_code', 'potential_species')){
    ggP <- ggP +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }

  # ranges from brush plot
  if(!is.null(plot_xmin)){
    if(!(is.na(plot_xmin) & is.na(plot_xmax) & is.na(plot_ymin) & is.na(plot_ymax))){
      ggP <- ggP + ggplot2::coord_cartesian(
        xlim = c(plot_xmin, plot_xmax),
        ylim = c(plot_ymin, plot_ymax),
        expand = FALSE
      )
    }
  }

  # return ggplot object
  return(ggP)
}


#' @title figure explore tab histogram
#'
#' @param sfC simple features data frame of culvert data
#' @param r reactive values object with app input values
#'
#' @return ggplot object of culvert data histogram
#' @export
figure_histogram <- function(sfC, x_axis_variable, y_axis_variable, color_variable, histogram_variable, histogram_nbins, highlight, barrier_ids, plot_xmin, plot_xmax, plot_ymin, plot_ymax){
  # init the ggplot
  ggP <- sfC %>%
    ggplot2::ggplot(ggplot2::aes(x = X, fill = C)) +
    ggplot2::xlab(get_pretty_variable_name(histogram_variable)) +
    ggplot2::guides(color = 'none') +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16, family = 'mono', face = 'bold'),
      legend.title = ggplot2::element_blank()
    )

  # use bins if numeric
  if(is.numeric(sfC$X)){
    ggP <- ggP + ggplot2::geom_histogram(bins = histogram_nbins)
  } else {
    ggP <- ggP + ggplot2::geom_bar()
  }

  # y axis variable
  ggP <- ggP + ggplot2::scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ',', scientific = FALSE) %>% sprintf(fmt = '%10s'))

  # x axis variable
  if(histogram_variable %in% c('owner_type_code', 'wria_number', 'potential_species')){
    ggP <- ggP + ggplot2::scale_x_discrete(labels = function(x) abbreviate(x, 10) %>% sprintf(fmt = '%10s'))
  } else if(!histogram_variable %in% c('owner_type_code', 'wria_number', 'potential_species')){
    ggP <- ggP + ggplot2::scale_x_continuous(labels = function(x) prettyNum(x, big.mark = ',', scientific = FALSE) %>% sprintf(fmt = '%10s'))
  }

  # color variable
  # color (fill) scale and legend
  if(color_variable == 'none'){
    ggP <- ggP +
      ggplot2::scale_fill_manual(values = c('none' = '#5b5b5b')) +
      ggplot2::theme(legend.position = 'none')
  } else if (color_variable %in% c('owner_type_code', 'wria_number', 'barrier_count')) {
    # discrete variables
    if(color_variable == 'owner_type_code'){
      # colorRampPalette(brewer.pal(10, 'Spectral'))(11)
      scaleFill <- ggplot2::scale_fill_manual(
        values = c(
          'City' = '#9E0142',
          'County' = '#CF374D',
          'Drainage District' = '#ED6345',
          'Federal' = '#FA9A58',
          'Irrigation District' = '#FDCC7A',
          'Other' = '#F2EA91',
          'Port' = '#CEEB9C',
          'Private' = '#96D4A4',
          'State' = '#5BB6A9',
          'Tribal' = '#3682BA',
          'Unknown' = '#5E4FA2',
          'Multiple' = '#B8B8B8'
        ),
        drop = TRUE, limits = force
      )
    } else if(color_variable == 'wria_number') {
      scaleFill <- ggplot2::scale_fill_manual(
        values = c(
          'Cedar - Sammamish' = '#9E0142',
          'Chambers - Clover' = '#B71C47',
          'Deschutes' = '#D0384D',
          'Duwamish - Green' = '#E04F4A',
          'Elwha - Dungeness' = '#EE6445',
          'Island' = '#F67E4B',
          'Kennedy - Goldsborough' = '#FA9C58',
          'Kitsap' = '#FDB768',
          'Lower Chehalis' = '#FDCD7B',
          'Lower Skagit - Samish' = '#FEE28F',
          'Lyre - Hoko' = '#FEF0A7',
          'Nisqually' = '#FFFFBF',
          'Nooksack' = '#F3FAAD',
          'Puyallup - White' = '#E8F59B',
          'Queets - Quinault' = '#D0EC9C',
          'Quilcene - Snow' = '#B5E1A1',
          'San Juan' = '#98D5A4',
          'Skokomish - Dosewallips' = '#78C9A4',
          'Snohomish' = '#5CB7A9',
          'Soleduc' = '#449DB4',
          'Stillaguamish' = '#3682BA',
          'Upper Chehalis' = '#4A68AE',
          'Upper Skagit' =  '#5E4FA2'
        ),
        drop = TRUE, limits = force
      )
    } else if(color_variable == 'barrier_count'){
      scaleFill <- ggplot2::scale_fill_manual(
        values = c(
          '1' = '#5E4FA2',
          '2' = '#3D79B6',
          '3' = '#4CA4B1',
          '4' = '#77C8A4',
          '5' = '#ABDDA4',
          '6' = '#D7EF9B',
          '7' = '#F2EA91',
          '8' = '#FDD380',
          '9' = '#FDAE61',
          '10' = '#F67D4A',
          '11' = '#E45549',
          '12' = '#C72E4B',
          '13' = '#9E0142'
        ),
        drop = TRUE, limits = force
      )
    }
    ggP <- ggP +
      scaleFill +
      ggplot2::theme(
        legend.position = c(.99, .99),
        legend.direction = 'vertical',
        legend.justification = c(1, 1),
        legend.box.background = ggplot2::element_rect(color = 'darkgrey')
      )
  } else {
    # continuous variables
    ggP <- ggP +
      ggplot2::scale_fill_gradientn(colors = c('#5E4FA2','#3288BD','#66C2A5','#ABDDA4','#E6F598','#FFFFBF','#FEE08B','#FDAE61','#F46D43','#D53E4F','#9E0142')) +
      ggplot2::theme(
        legend.position = c(.99, .99),
        legend.direction = 'vertical',
        legend.justification = c(1, 1),
        legend.key.height = ggplot2::unit(1, 'cm'),
        legend.box.background = ggplot2::element_rect(color = 'grey')
      )
  }

  # highlighted barriers
  if(highlight == 2){
    # set the barrier ids to '' if null
    if(is.null(barrier_ids)){
      cBarrierIds <- ''
    } else {
      cBarrierIds <- barrier_ids
    }

    # filter out highlighted barriers
    sfH <- sfC %>% dplyr::filter(site_id %in% cBarrierIds)

    # add to histogram as vertical lines
    ggP <- ggP +
      ggplot2::geom_vline(data = sfH, ggplot2::aes(xintercept = X), col = '#00ffff')
  }

  # x axis tick label orientation
  if(histogram_variable %in% c('wria_number', 'owner_type_code', 'potential_species')){
    ggP <- ggP +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }

  # ranges from brush plot
  if(!is.null(plot_xmin)){
    if(!(is.na(plot_xmin) & is.na(plot_xmax) & is.na(plot_ymin) & is.na(plot_ymax))){
      ggP <- ggP + ggplot2::coord_cartesian(
        xlim = c(plot_xmin, plot_xmax),
        ylim = c(plot_ymin, plot_ymax),
        expand = FALSE
      )
    }
  }

  # return ggplot object
  return(ggP)
}

#' @title figure explore tab culvert count by WRIA histogram
#'
#' @param sfC simple features data frame of culvert data
#' @param sfW simple features data frame of wria data
#'
#' @return ggplot object of culvert count by WRIA
#' @export
figure_culvert_count_by_WRIA_histogram <- function(sfC, sfW){
  sfC %>%
    replace_WRIA_number_with_name(sfW) %>%
    ggplot2::ggplot(ggplot2::aes(x = wria_number)) +
    ggplot2::geom_bar() +
    ggplot2::scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ',', scientific = FALSE) %>% sprintf(fmt = '%10s')) +
    ggplot2::xlab(NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16, family = 'mono', face = 'bold'),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5)
    )
}

#' @title replace WRIA number with name
#'
#' @param sfC simple features data frame of culvert data
#' @param sfW simple features data frame of wria data
#'
#' @return data frame of culverts with wria name instead of wria number
#' @export
replace_WRIA_number_with_name <- function(sfC, sfW){
  sfC <- sfC %>%
    dplyr::inner_join(
      sfW %>% sf::st_drop_geometry() %>% dplyr::select(WRIA_NR, WRIA_NM),
      c('wria_number' = 'WRIA_NR')
    ) %>%
    dplyr::select(-wria_number) %>%
    dplyr::rename(wria_number = WRIA_NM)

  return(sfC)
}

#' @title replace owner code with name
#'
#' @param sfC simple features data frame of culvert data
#'
#' @return data frame of culverts with owner name instead of owner type code
#' @export
replace_owner_code_with_name <- function(sfC){
  sfC <- sfC %>%
    dplyr::mutate(owner_name = dplyr::case_when(
      owner_type_code == 1 ~ "City",
      owner_type_code == 2 ~ "County",
      owner_type_code == 3 ~ "Federal",
      owner_type_code == 4 ~ "Private",
      owner_type_code == 5 ~ "State",
      owner_type_code == 6 ~ "Tribal",
      owner_type_code == 7 ~ "Other",
      owner_type_code == 8 ~ "Port",
      owner_type_code == 9 ~ "Drainage District",
      owner_type_code == 11 ~ "Irrigation District",
      owner_type_code == 12 ~ "Unknown"
    )) %>%
    dplyr::select(-owner_type_code) %>%
    dplyr::rename(owner_type_code = owner_name)

  return(sfC)
}

#' @title get pretty variable name
#'
#' @param varName string of variable name
#'
#' @return string value of variable name to use in chart axis and legend labels
#' @export
get_pretty_variable_name <- function(varName){
  if(varName == 'cost'){
    prettyName <- 'Cost ($)'
  } else if(varName ==  'barrier_count'){
    prettyName <-  'Downstream Barrier (count)'
  } else if(varName ==  'potential_species'){
    prettyName <-  'Potential Species'
  } else if(varName ==  'hmarg_net'){
    prettyName <-  'Marginal Habitat Length (km)'
  } else if(varName ==  'hfull_net'){
    prettyName <-  'Full Habitat Length (km)'
  } else if(varName == 'wria_number'){
    prettyName <- 'WRIA'
  } else if(varName == 'owner_type_code'){
    prettyName <- 'Owner Type'
  }

  return(prettyName)
}

#' @title get plot click site id
#'
#' @param r reactive values object with app input values
#' @param plotClickX x coordinate from plot click event
#' @param plotClickY y coordinate from plot click event
#'
#' @return string value of culvert site id closest to plot click coordinates or empty string if beyond maximum distance
#' @export
get_plot_click_site_id <- function(owner_sel, area_sel, x_axis_variable, y_axis_variable, plotClickX, plotClickY){
  sfC <- culverts_cmb

  # get sites for selected owners
  cSiteIds <- c()
  if('1' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_city) %>% dplyr::pull(site_id))}
  if('2' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_county) %>% dplyr::pull(site_id))}
  if('3' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_federal) %>% dplyr::pull(site_id))}
  if('4' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_private) %>% dplyr::pull(site_id))}
  if('5' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_state) %>% dplyr::pull(site_id))}
  if('6' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_tribal) %>% dplyr::pull(site_id))}
  if('7' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_other) %>% dplyr::pull(site_id))}
  if('8' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_port) %>% dplyr::pull(site_id))}
  if('9' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_drainage_district) %>% dplyr::pull(site_id))}
  if('11' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_irrigation_district) %>% dplyr::pull(site_id))}
  if('12' %in% owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_unknown) %>% dplyr::pull(site_id))}

  # filter by area and owner
  sfC <- sfC %>%
    dplyr::filter(wria_number %in% area_sel & site_id %in% cSiteIds) %>%
    dplyr::rename(X = x_axis_variable, Y = y_axis_variable)

  sfC1 <- sfC %>%
    dplyr::mutate(RelX = X / max(sfC$X), RelY = Y / max(sfC$Y)) %>%
    dplyr::mutate(Diff = sqrt((RelX - plotClickX / max(sfC$X))^2 + (RelY - plotClickY / max(sfC$Y))^2)) %>%
    dplyr::arrange(Diff) %>%
    dplyr::slice(1) %>%
    dplyr::select(site_id, X, Y, Diff)

  if(sfC1$Diff < .03){
    siteId <- paste0('Site Id: ', sfC1$site_id)
  } else {
    siteId <- ''
  }

  return(siteId)
}
