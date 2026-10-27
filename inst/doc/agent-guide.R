## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(radiatR)

## ----tracks-------------------------------------------------------------------
sim <- as.data.frame(simulate_tracks(seed = 1, n_points = 50))
path <- tempfile(fileext = ".csv")
write.csv(sim, path, row.names = FALSE)

ts <- read_tracks(
  path,
  mapping   = list(id = "trial_id", time = "frame", x = "abs_x", y = "abs_y"),
  time_type = "frames",
  fps       = 30,
  keep      = "condition"
)
hd <- derive_headings(ts, rule = "distal", carry = "condition")
head(hd, 3)

## ----angles-------------------------------------------------------------------
bearings <- data.frame(bearing = c(10, 20, 350, 5), site = c("a", "a", "b", "b"))
hf <- headings_frame(bearings, col = "bearing", units = "degrees",
                     angle_convention = "clock")
hf

## ----summary------------------------------------------------------------------
circ_summarise(hd, col = "heading", units = "radians", .by = "condition")

## ----attrition----------------------------------------------------------------
attr(hd, "n_total")
attr(hd, "n_missing")

## ----tests--------------------------------------------------------------------
set.seed(1)
test_uniformity(hd, group_col = "condition", angle_col = "heading")
test_mean_directions(hd, group_col = "condition", angle_col = "heading")

