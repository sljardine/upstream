load("upstream_inputs.Rdata")
load("upstream_inputs_gm.Rdata")

# Habitat volume: km^3 -> m^3 --------------------------------------------------
# The pipeline builds volume as length_km * width_m * depth_m with everything
# carried in kilometres, so a typical reach is ~3e-8 km^3 and the statewide
# total is 0.01 km^3. Every figure and table then reads 0.00. The number is
# correct, the unit is unusable. 1 km^3 = 1e9 m^3, so the statewide total
# becomes ~1e7 m^3.
#
# Applied here rather than in the pipeline so the stored data keeps its native
# units, and here rather than at each display site because there are four of
# them (two figure entry points, the tables, and the Suggest objective).
# Idempotent: this script always reloads from the .RData above, so re-running
# it cannot scale twice.
#
# ONLY the true volume columns. The *_percent columns are percentages (0-100)
# and the *_temp columns are degrees C; both merely carry "volume" in the name
# because they are volume-weighted. Scaling those would be wrong.
KM3_TO_M3 <- 1e9
volume_cols <- c("hmarg_volume", "hfull_volume",
                 "hmarg_volume_agri", "hmarg_volume_urb", "hmarg_volume_natural",
                 "hfull_volume_agri", "hfull_volume_urb", "hfull_volume_natural")

scale_volume <- function(x) {
  present <- intersect(volume_cols, names(x))
  for (v in present) x[[v]] <- x[[v]] * KM3_TO_M3
  message("  scaled ", length(present), " volume columns to m^3")
  x
}

culverts_cmb       <- scale_volume(culverts_cmb)
culverts_cmb_gm    <- scale_volume(culverts_cmb_gm)
welcome_map_points <- scale_volume(welcome_map_points)

#temporary until next data update
culverts_cmb$popup <- str_replace_all(culverts_cmb$popup, "http:", "https:")
culverts_cmb_gm$popup <- str_replace_all(culverts_cmb_gm$popup, "http:", "https:")
welcome_map_points$popup <- str_replace_all(welcome_map_points$popup, "http:", "https:")

usethis::use_data(culverts_cmb, overwrite = TRUE)
usethis::use_data(D, overwrite = TRUE)
usethis::use_data(E, overwrite = TRUE)
usethis::use_data(lines_simp, overwrite = TRUE)
usethis::use_data(lines_ds, overwrite = TRUE)
usethis::use_data(marginal_line_ids, overwrite = TRUE)
usethis::use_data(downstream_line_ids, overwrite = TRUE)
usethis::use_data(wrias, overwrite = TRUE)
usethis::use_data(huc12, overwrite = TRUE)
usethis::use_data(huc12_wrias, overwrite = TRUE)

usethis::use_data(culverts_cmb_gm, overwrite = TRUE)
usethis::use_data(welcome_map_points, overwrite = TRUE)
usethis::use_data(D_gm, overwrite = TRUE)
usethis::use_data(E_gm, overwrite = TRUE)
usethis::use_data(lines_simp_gm, overwrite = TRUE)
usethis::use_data(lines_ds_gm, overwrite = TRUE)
usethis::use_data(marginal_line_ids_gm, overwrite = TRUE)
usethis::use_data(downstream_line_ids_gm, overwrite = TRUE)
usethis::use_data(huc12_wrias_gm, overwrite = TRUE)
