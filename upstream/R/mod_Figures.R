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
      leaflet::leafletOutput(ns('base_map')),
      align = "center"
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

    r$plot_brush <- 0

    user_plot <- reactiveVal(FALSE)

    observeEvent(c(r$submit_explore,
                   r$submit_suggest,
                   r$submit_custom),
                 user_plot(TRUE))

    store_plot <- eventReactive(c(r$submit_explore, r$submit_suggest, r$submit_custom, r$plot_brush), {
      if(r$tab_sel == "Explore"){
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

    # output$base_map <- renderUI({
    #   tags$iframe(src= "www/base_map.html",
    #     width = 450, height = 450,
    #     frameBorder = "0")
    # })

    output$base_map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldGrayCanvas,
          options = leaflet::providerTileOptions(noWrap = TRUE)
        ) %>%
        leaflet::addPolygons(data = sfWRIA, fill = FALSE, stroke = 'blue', weight = 1, opacity = 1)
    })

    output$uiPlot <- renderUI({
      if(!user_plot()){
        imageOutput(ns("logo"))
      } else {
        tags$div(
          style = 'width: 100%; height: calc(50vh - 75px); overflow: hidden;',
          plotOutput(
            ns('plot'),
            brush = brushOpts(ns('plot_brush'), resetOnNew = TRUE, clip = FALSE),
            click = ns('plot_click'), dblclick = ns('plot_dblclick'),
            height = "100%"
          )
        )
      }
    })

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
    # observeEvent(input$plot_click, {
    #   r$x_axis_variable %>% print()
    #   r$y_axis_variable %>% print()
    #   r$area_sel %>% print()
    #   r$owner_sel %>% print()
    #   input$plot_click$x %>% print()
    #   input$plot_click$y %>% print()
    #
    #   sfCulverts %>%
    #     dplyr::filter(wria_number == r$area_sel & owner_type_code == r$owner_sel) %>%
    #     dplyr::rename(X = r$x_axis_variable, Y = r$y_axis_variable) %>%
    #     dplyr::mutate(
    #       X = dplyr::case_when(r$x_axis_variable %in% c('cost', 'hmarg', 'hfull') ~ X / 1000, TRUE ~ X),
    #       Y = dplyr::case_when(r$y_axis_variable %in% c('cost', 'hmarg', 'hfull') ~ Y / 1000, TRUE ~ Y)
    #     ) %>%
    #     dplyr::mutate(Diff = sqrt((X - input$plot_click$x)^2 + (Y - input$plot_click$y)^2)) %>%
    #     dplyr::arrange(Diff) %>%
    #     dplyr::slice(1) %>%
    #     dplyr::select(site_id, X, Y, Diff) %>%
    #     print()
    # })
  })

  # Figure functions ----
  filterAndFormatCulvertsForExploreTabHistogram <- function(sfC, r){
    # filter by area and owner class, rename, and select variables for chart
    sfC <- sfC %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(wria_number %in% as.integer(r$area_sel) & owner_type_code %in% as.integer(r$owner_sel)) %>%
      dplyr::rename(X = r$histogram_variable) %>%
      dplyr::select(site_id, X)

    # get pretty units for some variables (eg. divide big numbers by 1000)
    if(r$histogram_variable %in% c('cost', 'hmarg', 'hfull')){
      sfC <- sfC %>% dplyr::mutate(X = X / 1000)
    }

    # this splits the X variable at commas into rows when X = potential_species
    if(is.character(sfC$X)){
      sfC <- sfC %>% purrr::pmap_dfr(function(site_id, X){
        cX <- strsplit(X, ',', fixed = TRUE)[[1]]
        data.frame(
          site_id = site_id,
          X = cX
        )
      })
    }

    return(sfC)
  }

  filterAndFormatCulvertsForExploreTabScatterplot <- function(sfC, r){
    # filter by area and owner class, rename, and select variables for chart
    sfC <- sfC %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(wria_number %in% as.integer(r$area_sel) & owner_type_code %in% as.integer(r$owner_sel)) %>%
      dplyr::rename(X = r$x_axis_variable, Y = r$y_axis_variable) %>%
      dplyr::select(site_id, X, Y)

    # get pretty units for some variables (eg. divide big numbers by 1000)
    if(r$x_axis_variable %in% c('cost', 'hmarg', 'hfull')){
      sfC <- sfC %>% dplyr::mutate(X = X / 1)
    }
    if(r$y_axis_variable %in% c('cost', 'hmarg', 'hfull')){
      sfC <- sfC %>% dplyr::mutate(Y = Y / 1)
    }

    # this splits the X variable at commas into rows when X = potential_species
    if(is.character(sfC$X)){
      sfC <- sfC %>% purrr::pmap_dfr(function(site_id, X, Y){
        cX <- strsplit(X, ',', fixed = TRUE)[[1]]
        data.frame(
          site_id = site_id,
          X = cX,
          Y = Y
        )
      })
    }

    # this splits the Y variable at commas into rows when Y = potential_species
    if(is.character(sfC$Y)){
      sfC <- sfC %>% purrr::pmap_dfr(function(site_id, X, Y){
        cY <- strsplit(Y, ',', fixed = TRUE)[[1]]
        data.frame(
          site_id = site_id,
          X = X,
          Y = cY
        )
      })
    }

    sfC %>% head() %>% print()

    return(sfC)
  }

  figureExploreTabHistogram <- function(sfC, r){
    # init the ggplot
    ggP <- sfC %>%
      ggplot2::ggplot(ggplot2::aes(x = X)) +
      ggplot2::xlab(getPrettyVariableName(r$histogram_variable)) +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size = 16))

    # use bins if numeric
    if(is.numeric(sfC$X)){
      ggP <- ggP + ggplot2::geom_histogram(bins = r$histogram_nbins)
    } else {
      ggP <- ggP + ggplot2::geom_bar()
    }

    # ranges from brush plot
    if(!is.null(r$plot_xmin)){
      if(!(is.na(r$plot_xmin) & is.na(r$plot_xmax))){
        ggP <- ggP + ggplot2::coord_cartesian(xlim = c(r$plot_xmin, r$plot_xmax))
      }
    }

    # return ggplot object
    return(ggP)
  }

  figureExploreTabScatterPlot <- function(sfC, r){
    # set the barrier ids to '' if null
    if(is.null(r$barrier_ids)){
      cBarrierIds <- ''
    } else {
      cBarrierIds <- r$barrier_ids
    }

    # if barriers to highlight then filter them out and add them in second geom_point layer
    if(r$highlight == 2){
      dfH <- sfC %>% dplyr::filter(site_id %in% cBarrierIds)
      sfC <- sfC %>% dplyr::filter(!site_id %in% cBarrierIds)
    }

    # init the ggplot
    ggP <- sfC %>%
      ggplot2::ggplot(ggplot2::aes(x = X, y = Y)) +
      ggplot2::geom_jitter(width = r$x_jitter, height = r$y_jitter) +
      ggplot2::xlab(getPrettyVariableName(r$x_axis_variable)) +
      ggplot2::ylab(getPrettyVariableName(r$y_axis_variable)) +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size = 16))

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

    # add the highlight layer
    if(r$highlight == 2){
      ggP <- ggP + ggplot2::geom_jitter(data = dfH, width = r$x_jitter, height = r$y_jitter, col = 'orange', size = 3)
    }

    # return ggplot object
    return(ggP)
  }

  # get pretty variable names for chart axis labels
  getPrettyVariableName <- function(varName){
    if(varName == 'cost'){
      prettyName <- 'Cost (x $1,000)'
    } else if(varName ==  'barrier_count'){
      prettyName <-  'Downstream Barrier (count)'
    } else if(varName ==  'potential_species'){
      prettyName <-  'Potential Species'
    } else if(varName ==  'hmarg'){
      prettyName <-  'Marginal Habitat Length (km)'
    } else if(varName ==  'hfull'){
      prettyName <-  'Full Habitat Length (km)'
    }

    return(prettyName)
  }
}
