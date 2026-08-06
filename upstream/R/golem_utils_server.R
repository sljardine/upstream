#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)

#' Drop the "None"/"All" sentinel from a selection
#'
#' Guards of the form `if (! 0 %in% barrier_idp)` disable themselves whenever
#' the sentinel is present, which it always was. Strip it instead, then test
#' whether anything real is left.
#'
#' @param x a selection vector
#' @param sentinel the sentinel value
#'
#' @noRd
drop_sentinel <- function(x, sentinel = "0") {
  x <- as.character(x)
  x[!is.na(x) & x != sentinel]
}

#' Stop an "All"/"None" sentinel sticking in a multi-select
#'
#' A `selectizeInput` with `multiple = TRUE` and a pre-selected sentinel
#' ("All WRIAs", "All Ownership Types", "None") keeps that sentinel in the box
#' when the user picks a real value, leaving the selection as
#' `c(sentinel, real)`. Downstream that either widens the filter back to
#' everything or, for `barrier_idp`, makes `solve_opt()` skip its cost-zeroing
#' entirely, so the control silently does nothing.
#'
#' This drops whichever side the user did not just act on: adding a real value
#' clears the sentinel, and clicking the sentinel clears the real values.
#' Clearing the box entirely falls back to the sentinel, so downstream code
#' never receives an empty selection.
#'
#' @param session,input the module session and input objects
#' @param id un-namespaced input id
#' @param sentinel the sentinel value, as a character scalar
#'
#' @noRd
sync_sentinel_selection <- function(session, input, id, sentinel = "0") {
  # observeEvent evaluates input[[id]] lazily at flush time, so `id` must be
  # forced here or a loop over ids would leave every observer watching the last
  force(id)
  force(sentinel)

  previous <- shiny::reactiveVal(sentinel)

  shiny::observeEvent(input[[id]], ignoreNULL = FALSE, {
    current <- as.character(input[[id]])
    prev <- previous()

    # empty box: restore the sentinel rather than pass nothing downstream
    if (length(current) == 0) {
      shiny::updateSelectizeInput(session, id, selected = sentinel)
      previous(sentinel)
      return(invisible(NULL))
    }

    if (sentinel %in% current && length(current) > 1) {
      # sentinel already present means the user just added a real value;
      # otherwise they just clicked the sentinel and want everything reset
      keep <- if (sentinel %in% prev) setdiff(current, sentinel) else sentinel
      shiny::updateSelectizeInput(session, id, selected = keep)
      previous(keep)
      return(invisible(NULL))
    }

    previous(current)
  })
}
