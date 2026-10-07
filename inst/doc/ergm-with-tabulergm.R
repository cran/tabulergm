## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>"
)

## ----load-packages, message = FALSE, warning = FALSE--------------------------
library(ergm)
library(tabulergm)

## ----fit-model, message = FALSE, warning = FALSE------------------------------
data(florentine)

model <- ergm(
  flomarriage ~ edges + nodematch("wealth"),
  control = control.ergm(seed = 42)
)
summary(model)

## ----markdown-table, results = "asis"-----------------------------------------
tabulergm_table(
  model,
  include_math        = TRUE,
  include_description = TRUE,
  format              = "markdown"
)

## ----overrides, results = "asis"----------------------------------------------
tabulergm_table(
  model,
  include_title       = TRUE,
  include_description = TRUE,
  override.title      = c(edges = "Density"),
  override.desc       = c(edges = "Baseline propensity to form ties."),
  format              = "markdown"
)

## ----overrides-bulk, eval = FALSE---------------------------------------------
# tabulergm_table(
#   model,
#   override = list(
#     edges     = list(title = "Density", desc = "Baseline tie propensity."),
#     nodematch = list(citation = "doi:10.1016/S0378-8733(01)00029-6")
#   )
# )

## ----citations, results = "asis"----------------------------------------------
tabulergm_table(
  flomarriage ~ gwesp(0.5, fixed = TRUE) + gwdegree(0.5, fixed = TRUE),
  format = "markdown"
)

## ----compact-style, results = "asis"------------------------------------------
tabulergm_table(
  flomarriage ~ edges + nodematch("wealth") + triangle,
  include_description = FALSE,
  format = "markdown"
) |>
  with_style_name_over_formula()

## ----compact-style-save, eval = FALSE-----------------------------------------
# tabulergm_table(model, include_description = FALSE) |>
#   with_style_name_over_formula() |>
#   tabulergm_save("compact-ergm")
# 
# tabulergm_table(model, include_description = FALSE) |>
#   with_style_name_over_formula() |>
#   tabulergm_view()

## ----compact-style-layout, eval = FALSE---------------------------------------
# tabulergm_table(model, include_description = FALSE) |>
#   with_style_name_over_formula(
#     column_widths = c(Name = .5, Representation = .2),
#     figure_height = .8
#   ) |>
#   tabulergm_save("compact-ergm")

## ----formula-table, results = "asis"------------------------------------------
tabulergm_table(
  flomarriage ~ edges + nodematch("wealth") + triangle,
  format = "markdown"
)

## ----term-dictionary, results = "asis"----------------------------------------
dictionary_terms <- network ~
  edges + mutual + triangle +
  gwesp(0.5, fixed = TRUE) + gwdsp(0.5, fixed = TRUE) +
  gwdegree(0.5, fixed = TRUE) + altkstar(2, fixed = TRUE) +
  nodematch("attr") + nodefactor("attr") + nodemix("attr") +
  nodecov("attr") + absdiff("attr") + edgecov("cov") +
  transitiveties + cyclicalties +
  nodeicov("attr") + nodeocov("attr") +
  gwidegree(0.5, fixed = TRUE) + gwodegree(0.5, fixed = TRUE) +
  nodeifactor("attr") + nodeofactor("attr") +
  kstar(2) + istar(2) + ostar(2) +
  isolates + degree(1) + concurrent +
  dgwesp(0.5, fixed = TRUE) + dgwdsp(0.5, fixed = TRUE) +
  gwb1dsp(0.5, fixed = TRUE) + gwb2dsp(0.5, fixed = TRUE) +
  gwb1degree(0.5, fixed = TRUE) + gwb2degree(0.5, fixed = TRUE) +
  b1factor("type") + b2factor("group") +
  b1nodematch("type") + b2nodematch("group") +
  b1starmix(2, "type") + b2starmix(2, "group")

tabulergm_table(dictionary_terms, format = "markdown")

## ----term-coverage, include = FALSE-------------------------------------------
# Fails the render if a term in inst/terms/ is missing from the table above
yml_terms <- unique(
  sub(
    "\\.(un)?directed\\.yml$", "",
    list.files(system.file("terms", package = "tabulergm"))
  )
)
stopifnot(all(yml_terms %in% parse_ergm_formula(dictionary_terms)$term))

## ----view-demo, eval = FALSE--------------------------------------------------
# tabulergm_view(model, include_math = TRUE, include_description = TRUE)

