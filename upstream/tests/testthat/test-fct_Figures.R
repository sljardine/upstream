# Build the parts of a Shiny click object that nearPoints actually reads:
# coords_img and img_css_ratio for the click position, domain and range for the
# panel, and discrete_limits when an axis is categorical.
mock_click <- function(img_x, img_y, xlim, ylim, discrete_x = NULL,
                       range = list(left = 50, right = 650, bottom = 450, top = 50)) {
  list(
    coords_img = list(x = img_x, y = img_y),
    img_css_ratio = list(x = 1, y = 1),
    domain = c(
      list(left = xlim[1], right = xlim[2], bottom = ylim[1], top = ylim[2]),
      if (!is.null(discrete_x)) list(discrete_limits = list(x = discrete_x)) else NULL
    ),
    range = range,
    log = list(x = NULL, y = NULL),
    mapping = list(x = "X", y = "Y"),
    x = NA, y = NA
  )
}

test_that("a click lands on the nearest point and misses when too far", {
  df <- data.frame(X = c(1, 5, 9), Y = c(1, 5, 9), site_id = c("a", "b", "c"))
  # X=5,Y=5 sits at image (350, 250) given this domain and range
  expect_equal(get_plot_click_site_id(df, mock_click(352, 250, c(0, 10), c(0, 10))),
               "Site Id: b")
  expect_equal(get_plot_click_site_id(df, mock_click(450, 250, c(0, 10), c(0, 10))),
               "")
})

test_that("the hit radius is in pixels, so a heavy-tailed axis still works", {
  # Regression test. The old implementation normalised by the maximum of the
  # data and required a distance under 0.03 in that space, so on a cost axis
  # reaching $229M the radius was roughly $6.9M wide and the clicker was
  # effectively unusable. A pixel radius does not care about the axis scale.
  df <- data.frame(
    X = c(1e3, 5e6, 229e6),
    Y = c(1, 5, 9),
    site_id = c("cheap", "mid", "expensive")
  )
  expect_equal(get_plot_click_site_id(df, mock_click(352, 250, c(0, 1e7), c(0, 10))),
               "Site Id: mid")

  # two points a few dollars apart are still distinguishable when zoomed in
  near <- data.frame(X = c(1000, 1001), Y = c(5, 5), site_id = c("lo", "hi"))
  expect_equal(get_plot_click_site_id(near, mock_click(650, 250, c(1000, 1001), c(0, 10))),
               "Site Id: hi")
})

test_that("categorical axes need no special casing", {
  # The previous click handler returned "" whenever either axis was
  # potential_species, owner_type_code or percent_fish_passable_code.
  df <- data.frame(
    X = c("City", "County", "State"),
    Y = c(2, 5, 8),
    site_id = c("a", "b", "c")
  )
  cats <- c("City", "County", "State")
  # "County" is position 2, which maps to image x 350 for this domain
  expect_equal(
    get_plot_click_site_id(df, mock_click(350, 250, c(0.4, 3.6), c(0, 10), discrete_x = cats)),
    "Site Id: b"
  )
  # clicking the gap between categories matches nothing
  expect_equal(
    get_plot_click_site_id(df, mock_click(275, 250, c(0.4, 3.6), c(0, 10), discrete_x = cats)),
    ""
  )
})

test_that("jitter widens the hit radius", {
  df <- data.frame(X = c(1, 5, 9), Y = c(1, 5, 9), site_id = c("a", "b", "c"))
  # 60px per data unit here. The point sits at image x 350, so a click at 380
  # is 30px away: outside the 10px base radius.
  expect_equal(get_plot_click_site_id(df, mock_click(380, 250, c(0, 10), c(0, 10))), "")
  # a jitter of 0.4 data units adds 0.4 * 60 = 24px, giving a 34px radius
  expect_equal(
    get_plot_click_site_id(df, mock_click(380, 250, c(0, 10), c(0, 10)), x_jitter = 0.4),
    "Site Id: b"
  )
  # and the allowance is proportional: a small jitter is not enough
  expect_equal(
    get_plot_click_site_id(df, mock_click(380, 250, c(0, 10), c(0, 10)), x_jitter = 0.05),
    ""
  )
})

test_that("missing or unusable input yields an empty string rather than an error", {
  df <- data.frame(X = 1, Y = 1, site_id = "a")
  expect_equal(get_plot_click_site_id(NULL, mock_click(350, 250, c(0, 10), c(0, 10))), "")
  expect_equal(get_plot_click_site_id(df, NULL), "")
  expect_equal(get_plot_click_site_id(df[0, ], mock_click(350, 250, c(0, 10), c(0, 10))), "")
  # a frame that was never formatted for plotting has no X/Y
  expect_equal(
    get_plot_click_site_id(data.frame(a = 1), mock_click(350, 250, c(0, 10), c(0, 10))),
    ""
  )
})
