#' DisCoPals color palettes
#'
#' A named list of color vectors. Each element is a character vector of hex
#' codes.
#'
#' @note The palettes currently contain placeholder colors randomly
#' sampled from the X11 color palette. They will be replaced with
#' designed palettes before the first stable release.
#'
#' @format A named `list` of `r length(disco_palettes)` character vectors:
#'
#' `r paste0("- **", names(disco_palettes), "** (", vapply(disco_palettes, length, 1L), " colors)", collapse = "\n")`
#' @source DisCoPals internal design.
"disco_palettes"
