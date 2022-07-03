
<!-- README.md is generated from README.Rmd. Please edit that file -->

# golemyamlapps

This is an example of rendering the same app in three different ways
based on a Golem YAML configuration.

## inst/golem-config.yaml

``` yaml
one:
  h1: Mtcars Plot
  dataset: mtcars
  column: mpg
  ggtheme: theme_minimal
two:
  h1: Airquality Plot
  dataset: airquality
  column: Day
  ggtheme: theme_bw
three:
  h1: ChickWeight Plot
  dataset: ChickWeight
  column: weight
  ggtheme: theme_dark
```

``` r
Sys.setenv("GOLEM_CONFIG_ACTIVE" = "one")
source("dev/run_dev.R", echo = TRUE)

Sys.setenv("GOLEM_CONFIG_ACTIVE" = "two")
source("dev/run_dev.R", echo = TRUE)

Sys.setenv("GOLEM_CONFIG_ACTIVE" = "three")
source("dev/run_dev.R", echo = TRUE)
```

## How it works

`get_golem_config()` will look for the value based on the
`GOLEM_CONFIG_ACTIVE` parameter (or also `RCONFIG_ACTIVE` in some old
versions of `{golem}`, check `R/app_config.R` for supported env var).

Remember that `get_golem_config()` always returns a character string.

``` r
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      h1(
        get_golem_config("h1")
      ),
      plotOutput("plot")
    )
  )
}
```

``` r
app_server <- function( input, output, session ) {
  # Getting the theme function from {ggplot2}
  gg_theme <- getFromNamespace(
    get_golem_config("ggtheme"),
    ns = "ggplot2"
  )

  output$plot <- renderPlot({
    # Getting the dataset
    get(
      get_golem_config("dataset")
    ) %>%
      ggplot2::ggplot(
        # Using .data for non standard selection in `ggplot()`
        ggplot2::aes(x = .data[[
          get_golem_config("column")
        ]])
        ) +
      ggplot2::geom_bar() +
      gg_theme()

  })
}
```
