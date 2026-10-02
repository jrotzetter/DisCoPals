#' DisCoPals discrete color scales for ggplot2
#'
#' @param name Legend title. Defaults to `waiver()` (uses the mapped variable name).
#' @param pal Name of the palette (see [disco_palettes]).
#' @param ... Further arguments passed to [ggplot2::discrete_scale()]
#'   (e.g. `limits`, `breaks`, `labels`, `na.value`, `guide`).
#' @return A [ggplot2::discrete_scale()] object.
#' @export
#' @rdname scale_disco
#' @examplesIf .has_packages("ggplot2")
#' ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), fill = factor(gear))) +
#'   ggplot2::geom_bar() +
#'   scale_fill_disco(name = "Gear")
scale_fill_disco <- function(name = ggplot2::waiver(), pal = "default", ...) {
  .require_packages("ggplot2")
  ggplot2::discrete_scale(
    "fill",
    name = name,
    palette = function(n) disco_pal(n, pal = pal),
    ...
  )
}

#' @rdname scale_disco
#' @export
scale_color_disco <- function(name = ggplot2::waiver(), pal = "default", ...) {
  .require_packages("ggplot2")
  ggplot2::discrete_scale(
    "colour",
    name = name,
    palette = function(n) disco_pal(n, pal = pal),
    ...
  )
}

#' @rdname scale_disco
#' @export
scale_colour_disco <- scale_color_disco
