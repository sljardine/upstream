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
      htmlOutput(ns("base_map")),
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
    
    user_plot <- reactiveVal(FALSE)
    
    observeEvent(c(r$submit_explore,
      r$submit_suggest,
      r$submit_custom),
      user_plot(TRUE))
      
    store_plot <- eventReactive(c(r$submit_explore,
      r$submit_suggest,
      r$submit_custom,
      r$plot_brush),
      {
        if(r$tab_sel == "Explore"){
          if(r$plot_type == 'Scatterplot'){
            dfCulverts %>%
              filterAndFormatCulvertsForExploreTabScatterplot(r) %>%
              figureExploreTabScatterPlot(r)
          } else if(r$plot_type == 'Histogram'){
            dfCulverts %>%
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
    
    output$base_map <- renderUI({
      tags$iframe(src= "www/base_map.html",
        width = 450, height = 450,
        frameBorder = "0")
    })
    
    output$uiPlot <- renderUI({
      if(!user_plot()){
        imageOutput(ns("logo"))
      } else {
        tags$div(
          style = 'width: 100%; height: calc(50vh - 75px); overflow: hidden;',
          plotOutput(
            ns('plot'), 
            brush = brushOpts('plot_brush', resetOnNew = TRUE, clip = FALSE), 
            click = 'plot_click', dblclick = 'plot_dblclick',
            height = "100%"
          )
        )
      }
    })
    
    output$plot <- renderPlot({store_plot()})
  })
  
  # Figure functions ----
  filterAndFormatCulvertsForExploreTabHistogram <- function(dfC, r){
    # filter by area and owner class, rename, and select variables for chart
    dfC <- dfC %>% 
      dplyr::filter(wria_number %in% as.integer(r$area_sel) & owner_type_code %in% as.integer(r$owner_sel)) %>%
      dplyr::rename(X = r$histogram_variable) %>%
      dplyr::select(site_id, X)
    
    # get pretty units for some variables (eg. divide big numbers by 1000)
    if(r$histogram_variable %in% c('cost', 'hmarg', 'hfull')){
      dfC <- dfC %>% dplyr::mutate(X = X / 1000)
    }
    
    # this splits the X variable at commas into rows when X = potential_species
    if(is.character(dfC$X)){
      dfC <- dfC %>% purrr::pmap_dfr(function(site_id, X){
        cX <- strsplit(X, ',', fixed = TRUE)[[1]]
        data.frame(
          site_id = site_id,
          X = cX
        )
      })
    }
    
    return(dfC)
  }
    
  filterAndFormatCulvertsForExploreTabScatterplot <- function(dfC, r){
    # filter by area and owner class, rename, and select variables for chart
    dfC <- dfC %>% 
      dplyr::filter(wria_number %in% as.integer(r$area_sel) & owner_type_code %in% as.integer(r$owner_sel)) %>%
      dplyr::rename(X = r$x_axis_variable, Y = r$y_axis_variable) %>%
      dplyr::select(site_id, X, Y)
    
    # get pretty units for some variables (eg. divide big numbers by 1000)
    if(r$x_axis_variable %in% c('cost', 'hmarg', 'hfull')){
      dfC <- dfC %>% dplyr::mutate(X = X / 1000)
    }
    if(r$y_axis_variable %in% c('cost', 'hmarg', 'hfull')){
      dfC <- dfC %>% dplyr::mutate(Y = Y / 1000)
    }
    
    # this splits the X variable at commas into rows when X = potential_species
    if(is.character(dfC$X)){
      dfC <- dfC %>% purrr::pmap_dfr(function(site_id, X, Y){
        cX <- strsplit(X, ',', fixed = TRUE)[[1]]
        data.frame(
          site_id = site_id,
          X = cX,
          Y = Y
        )
      })
    }
    
    # this splits the Y variable at commas into rows when Y = potential_species
    if(is.character(dfC$Y)){
      dfC <- dfC %>% purrr::pmap_dfr(function(site_id, X, Y){
        cY <- strsplit(Y, ',', fixed = TRUE)[[1]]
        data.frame(
          site_id = site_id,
          X = X,
          Y = cY
        )
      })
    }
    
    return(dfC)
  }
  
  figureExploreTabHistogram <- function(dfC, r){
    # init the ggplot
    ggP <- dfC %>% 
      ggplot2::ggplot(ggplot2::aes(x = X)) +
      ggplot2::xlab(getPrettyVariableName(r$histogram_variable)) +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size = 16))
    
    # use bins if numeric
    if(is.numeric(dfC$X)){
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
  
  figureExploreTabScatterPlot <- function(dfC, r){
    # set the barrier ids to '' if null
    if(is.null(r$barrier_ids)){
      cBarrierIds <- ''
    } else {
      cBarrierIds <- r$barrier_ids
    }
    
    # if barriers to highlight then filter them out and add them in second geom_point layer
    if(r$highlight == 2){
      dfH <- dfC %>% dplyr::filter(site_id %in% cBarrierIds)
      dfC <- dfC %>% dplyr::filter(!site_id %in% cBarrierIds)
    }
    
    # init the ggplot
    ggP <- dfC %>% 
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
      prettyName <-  'Marginal Habitat Length (ft x 1,000)'
    } else if(varName ==  'hfull'){
      prettyName <-  'Full Habitat Length (ft x 1,000)'
    }
    
    return(prettyName)
  }
}
