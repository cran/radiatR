## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width = 6,
  fig.height = 6,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(radiatR)
library(ggplot2)

## ----headings-----------------------------------------------------------------
data(cpunctatus)

hd <- derive_headings(cpunctatus, rule = "crossing",
                      circ0 = 0.2, circ1 = 0.4,
                      coords = "relative")
names(hd)[names(hd) == "id"] <- "trial_id"

# attach target half-width (arc) from the dataset, by trial
arc_map  <- unique(cpunctatus@data[, c("trial_id", "arc")])
hd       <- merge(hd, arc_map, by = "trial_id")
hd$arc   <- factor(hd$arc)

# keep trials with a defined crossing heading
hd <- hd[is.finite(hd$heading), , drop = FALSE]
head(hd[, c("trial_id", "arc", "heading")])

## ----circ-summary-headings----------------------------------------------------
circ_summary_headings(cpunctatus, rule = "distal", group_by = NULL)

## ----dispersion---------------------------------------------------------------
circ_dispersion(hd, group_col = "arc")

## ----sectors------------------------------------------------------------------
sector_summary(hd, sectors = 8L)

## ----vonmises-----------------------------------------------------------------
vonmises_fit(hd, group_col = "arc")[, c("arc", "mu_deg", "kappa", "n")]

## ----wrappedcauchy------------------------------------------------------------
wrappedcauchy_fit(hd, group_col = "arc")[, c("arc", "mu_deg", "rho", "n")]

## ----uniformity---------------------------------------------------------------
test_uniformity(hd, group_col = "arc", test = "rayleigh", p_adjust = "BH")

## ----ww-omnibus---------------------------------------------------------------
test_mean_directions(hd, group_col = "arc")

## ----ww-pairwise--------------------------------------------------------------
pw <- test_mean_directions(hd, group_col = "arc",
                           pairwise = TRUE, p_adjust = "holm")
head(pw[order(pw$p_value_adj), ])

## ----concentration------------------------------------------------------------
test_concentration(hd, group_col = "arc")

## ----model-select-------------------------------------------------------------
hd_ms <- derive_headings(cpunctatus, rule = "distal")
arc_map <- unique(cpunctatus@data[, c("trial_id", "arc")])
names(arc_map)[1] <- "id"
hd_ms <- merge(as.data.frame(hd_ms), arc_map, by = "id")
circ_model_select(hd_ms, group_col = "arc")

## ----cor-linear---------------------------------------------------------------
hd$arc_num <- as.numeric(as.character(hd$arc))
circ_cor(hd, x_col = "arc_num", angle_col = "heading", x_type = "linear")

## ----reg-simulate-------------------------------------------------------------
cond <- data.frame(condition = "demo", n_trials = 120, ref_mean = 0,
                   concentration_base = 12, mean_slope = 0.6,
                   predictor_mean = 0, predictor_sd = 1)
s   <- simulate_tracks(conditions = cond, n_points = 8, seed = 1)
reg <- s[!duplicated(s$trial_id), c("predictor", "final_heading")]
names(reg)[2] <- "heading"

fit <- circ_regression(reg, heading ~ predictor)
summary(fit)

## ----reg-predict--------------------------------------------------------------
predict(fit, data.frame(predictor = c(-1, 0, 1)))

## ----reg-fitted-arrows--------------------------------------------------------
radiate(headings_frame(hd, heading, units = "radians")) +
  add_circ_mean(fitted_directions(fit, at = seq(-2, 2, length.out = 7)),
                colour_col = "predictor")

## ----canvas-------------------------------------------------------------------
canvas <- ggplot() + coord_fixed() +
  add_circ(radius = 1) +
  theme_void()

## ----overlays, fig.width=6.5, fig.height=6.5----------------------------------
vm <- vonmises_fit(hd)        # pooled fit (group_col = NULL)
wc <- wrappedcauchy_fit(hd)

canvas +
  add_angle_rose(hd, bins = 12, scale = 0.8, fill = "grey80") +
  add_circular_kde(hd, scale = 0.8, colour = "tomato") +
  add_vonmises_density(vm, scale = 0.8, colour = "steelblue") +
  add_wrappedcauchy_density(wc, scale = 0.8, colour = "darkorange")

## ----stacked, fig.width=6.5, fig.height=6.5-----------------------------------
hd_frame <- derive_headings(cpunctatus, rule = "distal")
radiate(hd_frame, show_markers = FALSE) + add_stacked_headings(hd_frame)

## ----grouped-overlays, fig.width=6.5, fig.height=6.5--------------------------
set.seed(1)  # the interval is bootstrapped
canvas +
  add_heading_arrow(hd,    colour_col = "arc") +
  add_heading_interval(hd, group_col  = "arc", stat = "bootstrap_ci")

## ----radiate-arrow-colour, eval=FALSE-----------------------------------------
# radiate(cpunctatus, group_col = "trial_id",
#         show_arrow = TRUE, arrow_colour_col = "arc")

## ----critical-r, fig.width=6.5, fig.height=6.5--------------------------------
disp <- circ_dispersion(hd)
mean_vec <- data.frame(x = disp$resultant_R * cos(disp$mean_dir),
                       y = disp$resultant_R * sin(disp$mean_dir))

canvas +
  add_critical_r(hd, alpha = 0.05, test = "rayleigh") +
  geom_segment(data = mean_vec,
               aes(x = 0, y = 0, xend = x, yend = y),
               arrow = arrow(length = unit(0.15, "inches")),
               linewidth = 1)

## ----rayleigh-pval------------------------------------------------------------
test_uniformity(hd, test = "rayleigh")

## ----critical-v, fig.width=6.5, fig.height=6.5--------------------------------
canvas +
  add_critical_v_line(hd, mu0 = 0, alpha = 0.05, show_region = TRUE) +
  geom_segment(data = mean_vec,
               aes(x = 0, y = 0, xend = x, yend = y),
               arrow = arrow(length = unit(0.15, "inches")),
               linewidth = 1)

## ----periodic, fig.width=6, fig.height=6--------------------------------------
set.seed(1)
# 200 events: a strong morning cluster (~09:00) and a weaker evening one (~21:00)
times <- as.POSIXct("2020-06-01", tz = "UTC") +
  c(rnorm(150, 9 * 3600, 1.2 * 3600), rnorm(50, 21 * 3600, 3600))

events <- headings_frame(data.frame(heading = as_angle(times, "day")),
                         col = heading, units = "radians")

# is the timing non-uniform over the day?
test_uniformity(events, test = "rayleigh")

radiate(events, angle_labels = "hours")

