## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 6,
  fig.height = 5
)

## ----load---------------------------------------------------------------------
library(radiatR)

## ----data---------------------------------------------------------------------
data(cpunctatus)
cpunctatus

## ----length-------------------------------------------------------------------
head(track_length(cpunctatus))                         # unit-circle radii
head(track_length(set_distance_scale(cpunctatus, 50, unit = "mm")))   # mm

## ----restrict-----------------------------------------------------------------
within <- restrict_to_circumference(cpunctatus, mode = "truncate")
head(track_length(within))

## ----straightness-------------------------------------------------------------
si <- straightness_index(cpunctatus)
tr <- tortuosity_ratio(cpunctatus)
head(si)
# single-path versions on bare coordinates
path_straightness(x = c(0, 1, 2), y = c(0, 0, 0))      # straight -> 1
path_tortuosity(x = c(0, 0, 1), y = c(0, 1, 1))        # L-shaped  -> sqrt(2)

## ----sinuosity----------------------------------------------------------------
sn <- sinuosity(cpunctatus)
head(sn)

## ----compare------------------------------------------------------------------
metrics <- Reduce(
  function(a, b) merge(a, b, by = "trial_id"),
  list(straightness_index(cpunctatus), tortuosity_ratio(cpunctatus),
       sinuosity(cpunctatus))
)
arc <- unique(as.data.frame(cpunctatus)[, c("trial_id", "arc")])
metrics <- merge(metrics, arc, by = "trial_id")

aggregate(cbind(straightness, tortuosity, sinuosity) ~ arc,
          data = metrics, FUN = function(v) round(mean(v, na.rm = TRUE), 2))

## ----goal-entries-------------------------------------------------------------
head(count_goal_entries(cpunctatus, target_angle = 0, coords = "relative"))

## ----zone-dwell---------------------------------------------------------------
dwell <- zone_dwell(cpunctatus, target_angle = 0, coords = "relative")
head(dwell)
# mean occupancy of the target quadrant (Q1) across tracks
mean(tapply(dwell$proportion[dwell$quadrant == 1],
            dwell$id[dwell$quadrant == 1], sum), na.rm = TRUE)

