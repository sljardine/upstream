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

#' Count culverts in each HUC 12 under the current bad-match setting
#'
#' Used to keep empty subareas out of the "Select Subarea" dropdown and to
#' label the rest with how many culverts they hold.
#'
#' @param huc_numbers HUC 12 numbers to count
#' @param remove_bad_match the radio input value; "2" means drop bad matches
#'
#' @return an integer vector the same length as `huc_numbers`
#'
#' @noRd
count_culverts_by_huc <- function(huc_numbers, remove_bad_match) {
  pts <- if (identical(as.character(remove_bad_match), "2")) {
    culverts_cmb_gm
  } else {
    culverts_cmb
  }
  tally <- table(sf::st_drop_geometry(pts)$huc_number)
  out <- as.integer(tally[as.character(huc_numbers)])
  out[is.na(out)] <- 0L
  out
}

#' The Explore ID-lookup selection, or nothing when browsing by area
#'
#' Returns a non-empty vector only when Explore is in ID lookup mode and at
#' least one id is entered. Callers treat an empty result as "browse by area",
#' which keeps the two paths from needing separate branches everywhere.
#'
#' @param r the shared reactiveValues object
#'
#' @return a character vector of site ids, possibly empty
#'
#' @noRd
explore_lookup_ids <- function(r) {
  if (!identical(r$explore_mode, "id")) return(character(0))
  ids <- r$lookup_ids_explore
  if (is.null(ids)) character(0) else as.character(ids)
}

#' Site ids of WSDOT barriers already scheduled for correction
#'
#' Washington is under a federal injunction to correct fish-passage barriers on
#' state highways, so these will be fixed regardless of how a cost-effectiveness
#' ranking scores them. Treating them as already funded is what lets Suggest
#' reach the culverts upstream of them.
#'
#' @param remove_bad_match the radio input value; "2" means drop bad matches
#'
#' @return a character vector of site ids, possibly empty
#'
#' @noRd
planned_wsdot_ids <- function(remove_bad_match) {
  pts <- if (identical(as.character(remove_bad_match), "2")) {
    culverts_cmb_gm
  } else {
    culverts_cmb
  }
  d <- sf::st_drop_geometry(pts)
  if (!"planned_wsdot" %in% names(d)) return(character(0))
  as.character(d$site_id[!is.na(d$planned_wsdot) & d$planned_wsdot == "Yes"])
}

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
#' @param choices for a server-side selectize, a function returning its current
#'   named choices. `updateSelectizeInput()` cannot reliably set `selected` on a
#'   server-side input without also re-sending `choices`; omitting them can empty
#'   the box, which downstream reads as "nothing selected" rather than "all".
#'
#' @noRd
sync_sentinel_selection <- function(session, input, id, sentinel = "0",
                                    choices = NULL) {
  # observeEvent evaluates input[[id]] lazily at flush time, so `id` must be
  # forced here or a loop over ids would leave every observer watching the last
  force(id)
  force(sentinel)
  force(choices)

  update <- function(selected) {
    if (is.null(choices)) {
      shiny::updateSelectizeInput(session, id, selected = selected)
    } else {
      shiny::updateSelectizeInput(
        session, id,
        choices = choices(), selected = selected, server = TRUE
      )
    }
  }

  previous <- shiny::reactiveVal(sentinel)

  shiny::observeEvent(input[[id]], ignoreNULL = FALSE, {
    current <- as.character(input[[id]])
    prev <- previous()

    # empty box: restore the sentinel rather than pass nothing downstream
    if (length(current) == 0) {
      update(sentinel)
      previous(sentinel)
      return(invisible(NULL))
    }

    if (sentinel %in% current && length(current) > 1) {
      # sentinel already present means the user just added a real value;
      # otherwise they just clicked the sentinel and want everything reset
      keep <- if (sentinel %in% prev) setdiff(current, sentinel) else sentinel
      update(keep)
      previous(keep)
      return(invisible(NULL))
    }

    previous(current)
  })
}
