#' Sort a color palette by perceptual attribute
#'
#' Sorts a vector of hex colors by a perceptual attribute computed in
#' CIELAB color space. When sorting by `"hue"`, hue angles are normalized
#' to 0°--360° and sorted linearly. The result is a linear unwrapping of the
#' color wheel with a seam at red (0°/360°).
#' Near-gray colors (chroma < `gray_threshold`) are treated as having no hue and
#' are placed last when sorting by `"hue"`.
#' Duplicate hex values are removed (keeping the first occurrence) before
#' sorting.
#'
#' @param pal `Character` vector of hex color values (e.g., `"#FF5733"`).
#'   Must not contain `NA` values.
#' @param by Attribute to sort by. One of:
#'   \describe{
#'     \item{`"hue"`}{Hue angle (degrees) in CIELAB space.}
#'     \item{`"L"`}{Lightness (\eqn{L^*} component).}
#'     \item{`"C"`}{Chroma (\eqn{C^* = \sqrt{a^2 + b^2}}).}
#'     \item{`"warmth"`}{Warm–cool index: \eqn{a^* + b^*}, approximating
#'       a projection onto the warm–cool axis (orange ↔ cyan) in the
#'       \eqn{a^*b^*} plane. Higher values = warmer (reds, oranges, yellows);
#'       lower values = cooler (greens, blues, cyans). Note: this
#'       conflates hue direction with chroma, so a highly saturated
#'       green will sort as "cooler" than a desaturated blue.}
#'     \item{`"S"`}{Saturation (Richter/Lübbe):
#'       \eqn{C^* / \sqrt{C^{*2} + L^{*2}}}.
#'       Measures how chromatic the color appears relative to its brightness.
#'       Ranges from 0 (achromatic) approaching 1 for highly chromatic colors
#'       at low lightness.}
#'   }
#' @param decreasing `Logical`. If `TRUE`, sort in descending order. Default: `FALSE`.
#' @param gray_threshold `Numeric`. Chroma (\eqn{C^*}) below this value is treated as
#'   achromatic and placed last when sorting by `"hue"`. The `decreasing` argument
#'   does not affect this placement. Default: 1.
#'
#' @return A `character` vector of hex colors (duplicates removed),
#'   reordered according to `by`.
#'
#' @examples
#' pal <- c("#FF5733", "#33FF57", "#3357FF", "#FF3357")
#' sort_palette(pal, by = "hue")
#' sort_palette(pal, by = "L", decreasing = TRUE)
#' sort_palette(pal, by = "S")
#'
#' @export
sort_palette <- function(pal,
                         by = c("hue", "L", "C", "warmth", "S"),
                         decreasing = FALSE,
                         gray_threshold = 1) {
  by <- match.arg(by)

  if (!is.character(pal) || any(is.na(pal))) {
    stop("Argument 'pal' must be a character vector without NA values.", call. = FALSE)
  }
  if (!all(grepl("^#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})$", pal))) {
    stop("Argument 'pal' must contain valid hex color values (e.g., \"#FF5733\").",
      call. = FALSE
    )
  }

  pal <- pal[!duplicated(pal)]

  if (length(pal) < 2) {
    return(pal)
  }

  if (!is.logical(decreasing) || length(decreasing) != 1 || is.na(decreasing)) {
    stop("Argument 'decreasing' must be a single non-NA logical value.", call. = FALSE)
  }
  if (!is.numeric(gray_threshold) || length(gray_threshold) != 1) {
    stop("Argument 'gray_threshold' must be a single number.", call. = FALSE)
  }

  lab <- convert_hex2Lab(pal)
  chroma <- sqrt(lab[, "a"]^2 + lab[, "b"]^2)
  hue <- atan2(lab[, "b"], lab[, "a"]) * 180 / pi
  hue[hue < 0] <- hue[hue < 0] + 360 # normalize to [0, 360)
  hue[chroma < gray_threshold] <- NA # Treat near-grays as "no hue"

  # Richter/Lübbe saturation: chroma as a fraction of total color magnitude
  denom <- sqrt(chroma^2 + lab[, "L"]^2)
  S <- ifelse(denom > 0, chroma / denom, 0) # Guard for 0/0 (pure black -> S = 0)

  ord <- switch(by,
    hue = order(hue, na.last = TRUE, decreasing = decreasing),
    L = order(lab[, "L"], decreasing = decreasing),
    C = order(chroma, decreasing = decreasing),
    warmth = order(lab[, "a"] + lab[, "b"], decreasing = decreasing),
    S = order(S, decreasing = decreasing)
  )

  pal[ord]
}


#' Scatter a color palette to maximize adjacent perceptual distance
#'
#' Reorders a vector of hex colors so that each color is as far as possible
#' (in CIELAB space) from the color immediately preceding it. The algorithm
#' starts at the maximin point (the color whose nearest neighbor is the
#' farthest away) and then greedily picks, at each step, the unvisited color
#' that is farthest from the current one. Duplicate hex values are removed
#' (keeping the first occurrence) before reordering.
#'
#' @param pal `Character` vector of hex color values (e.g., `"#FF5733"`).
#'   Must not contain `NA` values.
#'
#' @return A `character` vector of hex colors (duplicates removed), reordered
#'   so that adjacent colors are perceptually distant from one another.
#'
#' @details
#'   This is a greedy heuristic. The result is not guaranteed to maximize the
#'   minimum adjacent distance globally, but in practice should produce
#'   well-spread palettes.
#'
#'   Distance is measured as Euclidean distance in CIELAB space (CIE76,
#'   \eqn{\Delta E^*_{ab}}). CIEDE2000 (\eqn{\Delta E_{00}}) is the current
#'   CIE-recommended metric and correlates more closely with perceived
#'   differences, particularly in the blue region. However, for the purpose of
#'   spreading well-separated colors, CIE76 is a reasonable approximation.
#'
#' @examples
#' pal <- c("#FF0000", "#CC0000", "#0000FF", "#0000CC", "#FFFF00")
#' scatter_palette(pal)
#'
#' @seealso [sort_palette()]
#'
#' @export
scatter_palette <- function(pal) {
  if (!is.character(pal) || any(is.na(pal))) {
    stop("Argument 'pal' must be a character vector without NA values.", call. = FALSE)
  }
  if (!all(grepl("^#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})$", pal))) {
    stop("Argument 'pal' must contain valid hex color values (e.g., \"#FF5733\").",
      call. = FALSE
    )
  }

  pal <- pal[!duplicated(pal)]

  n <- length(pal)
  if (n <= 2) {
    return(pal)
  }

  lab <- convert_hex2Lab(pal)
  d <- as.matrix(stats::dist(lab, method = "euclidean"))

  # For each color, find its distance to the nearest neighbor (row min)
  # Start at the color whose nearest neighbor is farthest away, this is the
  # "most isolated" color
  current <- which.max(apply(d, 1, min))

  used <- rep(FALSE, n)
  used[current] <- TRUE

  visited <- integer(n)
  visited[1] <- current

  # At each step, pick the unvisited color farthest from the current one
  for (i in 2:n) {
    d_row <- d[current, ]

    # Mask out already-visited indices so they can never be picked.
    d_row[used] <- -Inf

    next_i <- which.max(d_row)

    used[next_i] <- TRUE
    visited[i] <- next_i
    current <- next_i
  }

  pal[visited]
}
