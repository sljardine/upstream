#' Figures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Figures_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #shinydashboard::box(
      #width = 11,
      #solidHeader = TRUE,
      #htmlOutput(ns("base_map")),
      tags$div(
        style = 'float: right; width: calc(100% - 15.4ch); height: calc(50vh - 100px); min-height: 500px; border: thin solid grey; margin-right: 5px;',
        leaflet::leafletOutput(ns('base_map'), height = '100%'),
        align = "center"
      )
      #)
    ),
    fluidRow(
      #shinydashboard::box(
      #width = 11,
      #solidHeader = TRUE,
      uiOutput(ns("uiPlot")),
      align = "center"
      #)
    )
  )
}



#' Figures Server Functions
#'
#' @noRd
mod_Figures_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # incrementing variable to trigger plot redraw on brush event
    r$plot_brush <- 0

    # plot click barrier id
    r$plot_click_text_output <- ''

    # render leaflet output
    output$base_map <- leaflet::renderLeaflet({
      getLeafletMap()
    })

    # map zoom event
    observeEvent(input$base_map_zoom, {
      updateMapWRIALabels(leaflet::leafletProxy(ns('base_map')), input$base_map_zoom, r)
    })

    # explore submit button event for base_map
    observeEvent(c(r$submit_explore), {
      updateMapWRIASelectedPolygons(leaflet::leafletProxy(ns('base_map')), r)
      updateMapWRIALabels(leaflet::leafletProxy(ns('base_map')), input$base_map_zoom, r)
      updateMapCulvertMarkers(leaflet::leafletProxy(ns('base_map')), r)
      updateMapCulvertMarkers(leaflet::leafletProxy(ns('base_map')), r, TRUE)
    })

    # fly to selected wrias on Explore submit
    observeEvent(r$submit_explore, {
      if(r$rezoom_on_submit){
        # selected wria bounding box
        bbox <- getBoundingBoxForSelectedWRIAs(r$area_sel)

        # zoom map to selected wrias
        leaflet::leafletProxy(mapId = ns("base_map")) %>%
          leaflet::flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      }
    })

    # reset plot click text output
    observeEvent(r$submit_explore, {r$plot_click_text_output <- ''})

    # initiates plot
    user_plot <- reactiveVal(FALSE)
    observeEvent(c(r$submit_explore, r$submit_suggest, r$submit_custom), user_plot(TRUE))

    # render plot on submit button events or brush event
    store_plot <- eventReactive(c(r$submit_explore, r$submit_suggest, r$submit_custom, r$plot_brush), {
      # default plot for app is culvert count by WRIA
      if(!user_plot()){
        getInitialExploreTabHistogram(sfCulverts, sfWRIA)
      }
      # explore tab plots (scatter plot and histogram)
      else if(r$tab_sel == "Explore"){
        if(r$plot_type == 'Scatterplot'){
          sfCulverts %>%
            filterAndFormatCulvertsForExploreTabScatterplot(r) %>%
            figureExploreTabScatterPlot(r)
        } else if(r$plot_type == 'Histogram'){
          sfCulverts %>%
            filterAndFormatCulvertsForExploreTabHistogram(r) %>%
            figureExploreTabHistogram(r)
        }
      }
    })

    output$logo <- renderImage({
      list(src = "inst/app/www/placeholder.png",
           width = "75%",
           align = "center")
    }, deleteFile = FALSE)

    # render plot ui
    output$uiPlot <- renderUI({
      tags$div(
        style = 'width: 100%; height: calc(50vh - 100px); min-height: 500px; overflow: hidden;',
        plotOutput(
          ns('plot'),
          brush = brushOpts(ns('plot_brush'), resetOnNew = TRUE, clip = FALSE),
          click = ns('plot_click'), dblclick = ns('plot_dblclick'),
          height = "100%"
        ),
        tagAppendAttributes(
          textOutput(ns('plot_click_text_output')),
          style = 'position: relative; bottom: 99%; right: 10px; text-align: right; font-size: 16px; font-weight: bold;'
        )

      )
    })

    # render plot
    output$plot <- renderPlot({store_plot()})

    # makes figures tab plot zoomable
    observeEvent(input$plot_brush, {
      r$plot_xmin <- input$plot_brush$xmin
      r$plot_xmax <- input$plot_brush$xmax
      r$plot_ymin <- input$plot_brush$ymin
      r$plot_ymax <- input$plot_brush$ymax
      r$plot_brush <- r$plot_brush + 1
    })

    # plot click event to identify culvert
    observeEvent(input$plot_click, {
      if(r$plot_type == 'histogram'){
        r$plot_click_text_output <- ''
      } else if(r$x_axis_variable %in% c('potential_species', 'owner_type_code') |
         r$y_axis_variable %in% c('potential_species', 'owner_type_code')){
        r$plot_click_text_output <- ''
      } else {
        sfC <- sfCulverts %>%
          dplyr::filter(wria_number %in% r$area_sel & owner_type_code %in% r$owner_sel) %>%
          dplyr::rename(X = r$x_axis_variable, Y = r$y_axis_variable)

        sfC1 <- sfC %>%
          dplyr::mutate(Diff = sqrt((X - input$plot_click$x)^2 + (Y - input$plot_click$y)^2)) %>%
          dplyr::arrange(Diff) %>%
          dplyr::slice(1) %>%
          dplyr::select(site_id, X, Y, Diff)

        if(
          abs(sfC1$X - input$plot_click$x) / max(sfC$X) < .025 &
          abs(sfC1$Y - input$plot_click$y) / max(sfC$Y) < .025
        ){
          r$plot_click_text_output <- sfC1$site_id
        } else {
          r$plot_click_text_output <- ''
        }
      }
    })

    # render plot click text output
    output$plot_click_text_output <- renderText(r$plot_click_text_output)
  })

  # Map functions ----

  # get bounding box for selected wrias
  getBoundingBoxForSelectedWRIAs <- function(cWRIAs){
    sfW <- sfWRIA %>%
      dplyr::filter(WRIA_NR %in% cWRIAs)

    bbox <- sfW %>%
      sf::st_bbox() %>%
      as.vector()

    return(bbox)
  }

  # data frame for map labels
  getMapLabelDataframe <- function(zoomLevel, r, asSF = FALSE){
    # get WRIA centroids
    sfWC <- sfWRIA %>%
      sf::st_drop_geometry() %>%
      dplyr::select(WRIA_NR, WRIA_NM) %>%
      dplyr::bind_cols(
        sfWRIA %>%
          dplyr::select() %>%
          sf::st_centroid() %>%
          sf::st_coordinates()
      )

    # if asSF = FALSE return with lat/lng fields, else with geometry field
    if(!asSF){
      sfWC <- sfWC %>% dplyr::rename(lng = X, lat = Y)
    }

    # summarize culvert count by WRIA
    dfC <- sfCulverts %>% sf::st_drop_geometry()
    if(!is.null(r$area_sel)){
      dfC <- dfC %>% dplyr::filter(wria_number %in% r$area_sel)
    }
    if(!is.null(r$owner_sel)){
      dfC <- dfC %>% dplyr::filter(owner_type_code %in% r$owner_sel)
    }

    sfWC <- sfWC %>%
      dplyr::inner_join(
        dfC %>%
          dplyr::group_by(wria_number) %>%
          dplyr::summarize(Count = dplyr::n(), .groups = 'drop'),
        by = c('WRIA_NR' = 'wria_number')
      )

    # create label based on zoom
    if(is.null(zoomLevel)){
      sfWC <- sfWC %>% dplyr::mutate(Label = Count)
    } else {
      if(zoomLevel < 9){
        sfWC <- sfWC %>% dplyr::mutate(Label = Count)
      } else {
        sfWC <- sfWC %>% dplyr::mutate(Label = paste0(WRIA_NM, ' (', Count, ')'))
      }
    }

    return(sfWC)
  }

  # get leaflet map
  getLeafletMap <- function(){
    sfWRIA %>%
      leaflet::leaflet() %>%
      leaflet::addProviderTiles("CartoDB.Positron", group = "Grayscale")  %>%
      leaflet::addScaleBar("bottomleft") %>%
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
  }

  # update map wria labels
  updateMapWRIALabels <- function(leafProxy, zoomLevel, r){
    sfWC <- getMapLabelDataframe(zoomLevel, r)

    leafProxy %>%
      leaflet::clearGroup('wria') %>%
      leaflet::addLabelOnlyMarkers(
        data = sfWC, lat = ~lat, lng = ~lng,
        group = 'wria',
        label = ~Label,
        labelOptions = leaflet::labelOptions(textsize = '16px', noHide = TRUE, direction = 'center', textOnly = TRUE)
      )
  }

  # update map selected wria polygons
  updateMapWRIASelectedPolygons <- function(leafProxy, r){
    sfW <- sfWRIA %>% dplyr::filter(WRIA_NR %in% r$area_sel)

    leafProxy %>%
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

  # update map culverts markers
  updateMapCulvertMarkers <- function(leafProxy, r, isHighlighted = FALSE){
    # filter culverts to selected wrias
    sfC <- sfCulverts %>%
      dplyr::filter(wria_number %in% r$area_sel & owner_type_code %in% r$owner_sel)

    # replace owner_type_code with name
    if(r$color_variable == 'owner_type_code'){
      sfC <- replaceOwnerCodeWithName(sfC)
    }

    # replace wria_number with name
    if(r$color_variable == 'wria_number'){
      sfC <- replaceWRIANumberWithName(sfC, sfWRIA)
    }

    # assign color variable to C
    if(r$color_variable == 'none'){
      sfC$C <- 'none'
    } else {
      sfC$C <- sfC %>% dplyr::pull(r$color_variable)
    }

    # palette
    if(r$color_variable == 'none'){
      pal <- function(x){return('grey')}
    } else if(r$color_variable == 'owner_type_code'){
      pal <- leaflet::colorFactor(
        palette = rev(c('#5E4FA2','#3288BD','#66C2A5','#ABDDA4','#E6F598','#FEE08B','#FDAE61','#F46D43','#D53E4F','#9E0142')),
        domain = c('City', 'County', 'Drainage District', 'Federal', 'Other', 'Port', 'Private', 'State', 'Tribal', 'Unknown'),
        ordered = TRUE
      )
    } else if(r$color_variable == 'wria_number'){
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
    if(is.null(r$barrier_ids) | r$highlight == 1){
      cBarrierIds <- ''
    } else {
      cBarrierIds <- r$barrier_ids
    }

    # filter culverts for highlighted set or not
    # set group id and stroke parameters
    if(isHighlighted){
      sfC <- sfC %>% dplyr::filter(site_id %in% cBarrierIds)
      groupId <- 'culverts_highlighted'
      strokeColor <- '#00ffff'
    } else {
      sfC <- sfC %>% dplyr::filter(!site_id %in% cBarrierIds)
      groupId <- 'culverts'
      strokeColor <- 'black'
    }

    # remove the culverts from the map
    leafProxy %>% leaflet::clearGroup(groupId)

    # return if no culverts to draw
    if(nrow(sfC) == 0){return()}

    # add culverts to map if zoomed in enough
    leafProxy %>%
      leaflet::addCircleMarkers(
        data = sfC,
        group = groupId,
        radius = 5,
        weight = 1.5,
        color = strokeColor,
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

  # Figure functions ----
  filterAndFormatCulvertsForExploreTabHistogram <- function(sfC, r){
    # filter by area and owner class, rename, and select variables for chart
    sfC <- sfC %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(wria_number %in% as.integer(r$area_sel) & owner_type_code %in% as.integer(r$owner_sel))

    # this swaps wria_name if X = wria_number
    if(r$histogram_variable == 'wria_number' | r$color_variable == 'wria_number'){
      sfC <- replaceWRIANumberWithName(sfC, sfWRIA)
    }

    # this swaps owner names with code
    if(r$histogram_variable == 'owner_type_code' | r$color_variable == 'owner_type_code'){
      sfC <- replaceOwnerCodeWithName(sfC)
    }

    # create dummy variable if color variable is 'none'
    if(r$color_variable == 'none'){
      sfC[r$color_variable] <- 'none'
    }

    # calculate new variables
    sfC$X <- sfC %>% dplyr::pull(r$histogram_variable)
    sfC$C <- sfC %>% dplyr::pull(r$color_variable)

    # select variables
    sfC <- sfC %>% dplyr::select(site_id, X, C)

    # get pretty units for some variables (eg. divide big numbers by 1000)
    if(r$histogram_variable %in% c('cost', 'hmarg', 'hfull')){
      sfC <- sfC %>% dplyr::mutate(X = X)
    }

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

    return(sfC)
  }

  filterAndFormatCulvertsForExploreTabScatterplot <- function(sfC, r){
    # filter by area and owner class, rename, and select variables for chart
    sfC <- sfC %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(wria_number %in% as.integer(r$area_sel) & owner_type_code %in% as.integer(r$owner_sel))

    # this swaps wria_name if X = wria_number
    if(r$x_axis_variable == 'wria_number' | r$y_axis_variable == 'wria_number' | r$color_variable == 'wria_number'){
      sfC <- replaceWRIANumberWithName(sfC, sfWRIA)
    }

    # this swaps owner names with code
    if(r$x_axis_variable == 'owner_type_code' | r$y_axis_variable == 'owner_type_code' | r$color_variable == 'owner_type_code'){
      sfC <- replaceOwnerCodeWithName(sfC)
    }

    # add dummy color variable if color_variable = 'none'
    if(r$color_variable == 'none'){sfC[r$color_variable] <- 'none'}

    # calculate new variables
    sfC$X <- sfC %>% dplyr::pull(r$x_axis_variable)
    sfC$Y <- sfC %>% dplyr::pull(r$y_axis_variable)
    sfC$C <- sfC %>% dplyr::pull(r$color_variable)

    # select the variables
    sfC <- sfC %>% dplyr::select(site_id, X, Y, C)

    # get pretty units for some variables (eg. divide big numbers by 1000)
    if(r$x_axis_variable %in% c('cost', 'hmarg', 'hfull')){
      sfC <- sfC %>% dplyr::mutate(X = X / 1)
    }
    if(r$y_axis_variable %in% c('cost', 'hmarg', 'hfull')){
      sfC <- sfC %>% dplyr::mutate(Y = Y / 1)
    }

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

  # initial chart for app (Explore Tab)
  getInitialExploreTabHistogram <- function(sfC, sfW){
    sfC %>%
      replaceWRIANumberWithName(sfW) %>%
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

  # Explore tab scatterplot
  figureExploreTabScatterPlot <- function(sfC, r){
    # set the barrier ids to '' if null
    if(is.null(r$barrier_ids)){
      cBarrierIds <- ''
    } else {
      cBarrierIds <- r$barrier_ids
    }

    # calculate highlight variable
    sfC$IsHighlighted <- 'N'
    if(r$highlight == 2){
      sfC <- sfC %>%
        dplyr::mutate(IsHighlighted = dplyr::case_when(site_id %in% cBarrierIds ~ 'Y', TRUE ~ 'N')) %>%
        dplyr::arrange(IsHighlighted)
    }

    # init the ggplot
    ggP <- sfC %>%
      ggplot2::ggplot(ggplot2::aes(x = X, y = Y, fill = C, color = IsHighlighted)) +
      ggplot2::geom_jitter(width = r$x_jitter, height = r$y_jitter, alpha = .9, stroke = 1.3, size = 3.5, pch = 21) +
      #ggplot2::scale_y_continuous(labels = function(x) formatC(x, width = 10)) +
      ggplot2::scale_color_manual(values = c('N' = 'black', 'Y' = '#00ffff')) +
      ggplot2::guides(color = 'none') +
      ggplot2::xlab(getPrettyVariableName(r$x_axis_variable)) +
      ggplot2::ylab(getPrettyVariableName(r$y_axis_variable)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 16, family = 'mono', face = 'bold'),
        legend.title = ggplot2::element_blank(),
        legend.key.width = ggplot2::unit(55, 'native')
      )

    # y axis variable
    if(r$y_axis_variable %in% c('owner_type_code', 'wria_number', 'potential_species')){
      ggP <- ggP + ggplot2::scale_y_discrete(labels = function(x) abbreviate(x, 10) %>% sprintf(fmt = '%10s'))
    } else {
      ggP <- ggP + ggplot2::scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ',', scientific = FALSE) %>% sprintf(fmt = '%10s'))
    }

    # x axis variable
    if(r$x_axis_variable %in% c('owner_type_code', 'wria_number', 'potential_species')){
      ggP <- ggP + ggplot2::scale_x_discrete(labels = function(x) abbreviate(x, 10) %>% sprintf(fmt = '%10s'))
    } else if(!r$x_axis_variable %in% c('owner_type_code', 'wria_number', 'potential_species')){
      ggP <- ggP + ggplot2::scale_x_continuous(labels = function(x) prettyNum(x, big.mark = ',', scientific = FALSE) %>% sprintf(fmt = '%10s'))
    }

    # color (fill) scale and legend
    if(r$color_variable == 'none'){
      ggP <- ggP +
        ggplot2::scale_fill_manual(values = c('none' = 'grey')) +
        ggplot2::theme(legend.position = 'none')
    } else if (r$color_variable %in% c('owner_type_code', 'wria_number')) {
      # discrete variables
      if(r$color_variable == 'owner_type_code'){
        scaleFill <- ggplot2::scale_fill_manual(
          values = c(
            'City' = '#9E0142',
            'County' = '#D53E4F',
            'Drainage District' = '#F46D43',
            'Federal' = '#FDAE61',
            'Other' = '#FEE08B',
            'Port' = '#E6F598',
            'Private' = '#ABDDA4',
            'State' = '#66C2A5',
            'Tribal' = '#3288BD',
            'Unknown' = '#5E4FA2'
          ),
          drop = TRUE, limits = force
        )
      } else if(r$color_variable == 'wria_number') {
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
        ggplot2::scale_fill_gradientn(colors = c('#5E4FA2','#3288BD','#66C2A5','#ABDDA4','#E6F598','#FFFFBF','#FEE08B','#FDAE61','#F46D43','#D53E4F','#9E0142')) +
        ggplot2::theme(
          legend.position = c(.99, .95),
          legend.direction = 'vertical',
          legend.justification = c(1, 1),
          legend.key.height = ggplot2::unit(1, 'cm'),
          legend.box.background = ggplot2::element_rect(color = 'grey')
        )
    }

    # ranges from brush plot
    if(!is.null(r$plot_xmin)){
      if(!(is.na(r$plot_xmin) & is.na(r$plot_xmax) & is.na(r$plot_ymin) & is.na(r$plot_ymax))){
        ggP <- ggP + ggplot2::coord_cartesian(
          xlim = c(r$plot_xmin, r$plot_xmax),
          ylim = c(r$plot_ymin, r$plot_ymax),
          expand = FALSE
        )
      }
    }

    # return ggplot object
    return(ggP)
  }

  # explore tab histogram
  figureExploreTabHistogram <- function(sfC, r){
    # init the ggplot
    ggP <- sfC %>%
      ggplot2::ggplot(ggplot2::aes(x = X, fill = C)) +
      ggplot2::xlab(getPrettyVariableName(r$histogram_variable)) +
      ggplot2::guides(color = 'none') +
      ggplot2::theme_bw() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 16, family = 'mono', face = 'bold'),
        legend.title = ggplot2::element_blank()
      )

    # use bins if numeric
    if(is.numeric(sfC$X)){
      ggP <- ggP + ggplot2::geom_histogram(bins = r$histogram_nbins)
    } else {
      ggP <- ggP + ggplot2::geom_bar()
    }

    # y axis variable
    ggP <- ggP + ggplot2::scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ',', scientific = FALSE) %>% sprintf(fmt = '%10s'))

    # x axis variable
    if(r$x_axis_variable %in% c('owner_type_code', 'wria_number', 'potential_species')){
      ggP <- ggP + ggplot2::scale_x_discrete(labels = function(x) abbreviate(x, 10) %>% sprintf(fmt = '%10s'))
    } else if(!r$x_axis_variable %in% c('owner_type_code', 'wria_number', 'potential_species')){
      ggP <- ggP + ggplot2::scale_x_continuous(labels = function(x) prettyNum(x, big.mark = ',', scientific = FALSE) %>% sprintf(fmt = '%10s'))
    }

    # color variable
    # color (fill) scale and legend
    if(r$color_variable == 'none'){
      ggP <- ggP +
        ggplot2::scale_fill_manual(values = c('none' = '#5b5b5b')) +
        ggplot2::theme(legend.position = 'none')
    } else if (r$color_variable %in% c('owner_type_code', 'wria_number')) {
      # discrete variables
      if(r$color_variable == 'owner_type_code'){
        scaleFill <- ggplot2::scale_fill_manual(
          values = c(
            'City' = '#9E0142',
            'County' = '#D53E4F',
            'Drainage District' = '#F46D43',
            'Federal' = '#FDAE61',
            'Other' = '#FEE08B',
            'Port' = '#E6F598',
            'Private' = '#ABDDA4',
            'State' = '#66C2A5',
            'Tribal' = '#3288BD',
            'Unknown' = '#5E4FA2'
          ),
          drop = TRUE, limits = force
        )
      } else if(r$color_variable == 'wria_number') {
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
    if(r$highlight == 2){
      # set the barrier ids to '' if null
      if(is.null(r$barrier_ids)){
        cBarrierIds <- ''
      } else {
        cBarrierIds <- r$barrier_ids
      }

      # filter out highlighted barriers
      sfH <- sfC %>% dplyr::filter(site_id %in% cBarrierIds)

      # add to histogram as vertical lines
      ggP <- ggP +
        ggplot2::geom_vline(data = sfH, ggplot2::aes(xintercept = X), col = '#00ffff', position = ggplot2::position_jitter())
    }

    # ranges from brush plot
    if(!is.null(r$plot_xmin)){
      if(!(is.na(r$plot_xmin) & is.na(r$plot_xmax) & is.na(r$plot_ymin) & is.na(r$plot_ymax))){
        ggP <- ggP + ggplot2::coord_cartesian(
          xlim = c(r$plot_xmin, r$plot_xmax),
          ylim = c(r$plot_ymin, r$plot_ymax),
          expand = FALSE
        )
      }
    }

    # return ggplot object
    return(ggP)
  }

  # replace wria_number with wria name value for charts
  replaceWRIANumberWithName <- function(sfC, sfW){
    sfC <- sfC %>%
      dplyr::inner_join(
        sfW %>% sf::st_drop_geometry() %>% dplyr::select(WRIA_NR, WRIA_NM),
        c('wria_number' = 'WRIA_NR')
      ) %>%
      dplyr::select(-wria_number) %>%
      dplyr::rename(wria_number = WRIA_NM)

    return(sfC)
  }

  # replace owner_type_code with owner class name
  replaceOwnerCodeWithName <- function(sfC){
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

  # get pretty variable names for chart axis labels
  getPrettyVariableName <- function(varName){
    if(varName == 'cost'){
      prettyName <- 'Cost ($)'
    } else if(varName ==  'barrier_count'){
      prettyName <-  'Downstream Barrier (count)'
    } else if(varName ==  'potential_species'){
      prettyName <-  'Potential Species'
    } else if(varName ==  'hmarg'){
      prettyName <-  'Marginal Habitat Length (km)'
    } else if(varName ==  'hfull'){
      prettyName <-  'Full Habitat Length (km)'
    } else if(varName == 'wria_number'){
      prettyName <- 'WRIA'
    } else if(varName == 'owner_type_code'){
      prettyName <- 'Ownership Group'
    }

    return(prettyName)
  }
}
