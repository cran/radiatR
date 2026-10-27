## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(radiatR)

## ----import-tracks------------------------------------------------------------
untar(system.file("extdata", "tracks.tar.gz", package = "radiatR"),
      exdir = tempdir())
track_dir <- file.path(tempdir(), "tracks")
file_tbl  <- import_tracks(track_dir)
head(file_tbl)

## ----import-info--------------------------------------------------------------
manifest_path <- system.file("extdata", "millipede_trials.csv", package = "radiatR")
manifest <- import_info(manifest_path, cond_cols = c("type", "arc"))
head(manifest)

## ----get-all------------------------------------------------------------------
ts <- suppressWarnings(get_all_object_pos(file_tbl = file_tbl, track_dir = track_dir,
                                          manifest = manifest))
ts
head(ts@meta$trial_limits[, c("video", "arc", "type", "obstacle")])

## ----dlc-example--------------------------------------------------------------
dlc_path <- system.file("extdata", "dlc_single_wasp_predictions.csv",
                        package = "radiatR")
ts_dlc <- read_tracks(dlc_path, dialect = "deeplabcut", fps = 40)
ts_dlc

## ----dlc-threshold------------------------------------------------------------
ts_dlc_sure <- read_tracks(dlc_path, dialect = "deeplabcut", fps = 40,
                           dialect_args = list(likelihood_min = 0.9),
                           on_invalid = "drop")
ts_dlc_sure@meta$attrition

## ----sleap-example------------------------------------------------------------
sleap_path <- system.file("extdata", "sleap_three_mice.analysis.csv",
                          package = "radiatR")
ts_sleap <- read_tracks(sleap_path, dialect = "sleap")
ts_sleap

## ----basic-load---------------------------------------------------------------
example_df <- data.frame(
  id   = rep(c("A", "B"), each = 4),
  time = rep(seq(0, 3), times = 2),
  x    = c(1, 1.2, 1.4, 1.5, -0.2, -0.1, 0, 0.15),
  y    = c(0, 0.1, 0.3, 0.4, 1.0, 1.1, 1.3, 1.5)
)

ts_df <- read_tracks(example_df, mapping = list(id = "id", time = "time", x = "x", y = "y"))
ts_df

## ----dir-load-----------------------------------------------------------------
csv_dir <- tempfile()
dir.create(csv_dir)

write.csv(example_df, file.path(csv_dir, "trial01.csv"), row.names = FALSE)
write.csv(transform(example_df, id = paste0(id, "_2")),
          file.path(csv_dir, "trial02.csv"), row.names = FALSE)

dset <- read_tracks_dir(csv_dir, pattern = "\\.csv$")
length(dset)

unlink(csv_dir, recursive = TRUE)

## ----manifest-----------------------------------------------------------------
manifest_path <- tempfile(fileext = ".csv")
writeLines(
  c("file,type, arc", "trial01.csv, baseline,90", "trial02.csv,stimulus,135"),
  manifest_path
)
import_info(manifest_path, cond_cols = c("type", "arc"))

## ----custom-loader------------------------------------------------------------
dtrack_loader <- function(x) {
  # colClasses = "numeric" makes a malformed cell an error, not a silent NA
  d <- utils::read.delim(x, header = FALSE, colClasses = "numeric")
  if (ncol(d) < 3L)
    stop("dtrack: expected at least 3 columns (frame, x, y) in ", basename(x))
  data.frame(
    id    = tools::file_path_sans_ext(basename(x)),  # one file, one track
    frame = d[[1]],
    x     = d[[2]],
    y     = d[[3]]
  )
}
register_loader_dialect("dtrack", dtrack_loader)

## ----use-custom---------------------------------------------------------------
track_file <- file.path(track_dir, "10_10_point02.txt")
ts_raw <- read_tracks(track_file, dialect = "dtrack")
head(ts_raw@data[, c("id", "frame", "x", "y")])

## ----use-custom-dir-----------------------------------------------------------
ts_session <- read_tracks_dir(track_dir, pattern = "^10_.*_point02\\.txt$",
                              dialect = "dtrack")
ts_session

