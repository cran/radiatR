## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(radiatR)

## ----eval = requireNamespace("dplyr", quietly = TRUE)-------------------------
data(cpunctatus)
hd <- derive_headings(cpunctatus, rule = "distal")
hd2 <- dplyr::filter(hd, is.finite(heading))   # class + display preserved
class(hd2)
hf_display(hd2)

