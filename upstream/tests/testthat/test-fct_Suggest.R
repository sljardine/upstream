test_that("solve_opt zeroes the cost of ignored barriers even with the sentinel present", {
  # No skip_on_cran(): this is a deployed app, never a CRAN package, and the
  # guard silently hid this test whenever NOT_CRAN was unset. It solves three
  # small ILPs and takes well under a minute.

  # Regression test. "Select Projects to Ignore" is a multi-select whose "None"
  # option (value 0) stayed selected alongside any real choice, so solve_opt
  # received c("0", id). The old guard `if(! 0 %in% barrier_idp)` then skipped
  # the cost-zeroing entirely and the control silently did nothing.
  #
  # Constructed so the outcome cannot be a coincidence: the target barrier has
  # no downstream prerequisite (so the hydrology constraint cannot block it)
  # and the budget is half its cost, so it is affordable only if zeroed.

  d <- sf::st_drop_geometry(culverts_cmb)
  di <- colSums(D)
  wria <- 17

  candidates <- which(
    di == 0 & d$hmarg_length > 0 & d$wria_number == wria & !d$bad_match
  )
  skip_if(length(candidates) == 0, "no unconstrained candidate barrier in this WRIA")

  i <- candidates[which.max(d$cost[candidates])]
  target <- d$site_id[i]
  budget <- d$cost[i] * 0.5

  run <- function(barrier_idp) {
    out <- utils::capture.output(
      soln <- solve_opt(
        points = culverts_cmb, budget = budget, D = D, wria_sel = wria,
        huc_sel = 0, owner_sel = 0, obj = 1, w_urb = 0, w_ag = 0, w_nat = 0,
        w_temp = c(0, 100), hq = 1, species_sel = "all",
        barrier_idp = barrier_idp, cost = 1,
        mean_design_cost = NULL, mean_construction_cost = NULL
      )
    )
    soln
  }

  # too expensive when nothing is ignored
  expect_false(run("0")[i])

  # free once ignored, both with and without the sentinel tagging along
  expect_true(run(c("0", target))[i])
  expect_true(run(target)[i])
})
