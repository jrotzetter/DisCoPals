# --- OKLab conversion matrices ---

.srgb_to_oklab <- function(rgb) {
  # Values taken from https://bottosson.github.io/posts/oklab/#converting-from-linear-srgb-to-oklab

  # sRGB to Linear RGB (Decoding)
  lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)

  l <- 0.4122214708 * lin[, 1] + 0.5363325363 * lin[, 2] + 0.0514459929 * lin[, 3]
  m <- 0.2119034982 * lin[, 1] + 0.6806995451 * lin[, 2] + 0.1073969566 * lin[, 3]
  s <- 0.0883024619 * lin[, 1] + 0.2817188376 * lin[, 2] + 0.6299787005 * lin[, 3]

  # cube root of a given number; sign to handle negative inputs
  l_ <- sign(l) * abs(l)^(1 / 3)
  m_ <- sign(m) * abs(m)^(1 / 3)
  s_ <- sign(s) * abs(s)^(1 / 3)

  cbind(
    L = 0.2104542553 * l_ + 0.7936177850 * m_ - 0.0040720468 * s_,
    a = 1.9779984951 * l_ - 2.4285922050 * m_ + 0.4505937099 * s_,
    b = 0.0259040371 * l_ + 0.7827717662 * m_ - 0.8086757660 * s_
  )
}

.oklab_to_srgb <- function(lab) {
  # Values taken from https://bottosson.github.io/posts/oklab/#converting-from-linear-srgb-to-oklab

  l_ <- lab[, "L"] + 0.3963377774 * lab[, "a"] + 0.2158037573 * lab[, "b"]
  m_ <- lab[, "L"] - 0.1055613458 * lab[, "a"] - 0.0638541728 * lab[, "b"]
  s_ <- lab[, "L"] - 0.0894841775 * lab[, "a"] - 1.2914855480 * lab[, "b"]

  l <- l_^3
  m <- m_^3
  s <- s_^3

  lin <- cbind(
    r =  4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
    g = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
    b = -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s
  )

  # Linear RGB to sRGB (Encoding)
  srgb <- ifelse(lin <= 0.0031308, 12.92 * lin, 1.055 * lin^(1 / 2.4) - 0.055)
  # Clamp out-of-gamut results
  # Note: simple channel clipping is the crudest gamut mapping
  # Ottosson's gamut clipping post describes different strategies
  # https://bottosson.github.io/posts/gamutclipping/
  pmin(pmax(round(srgb * 255), 0), 255)
}

# --- OKLab <-> OKLCH (polar) ---

.oklab_to_oklch <- function(lab) {
  C <- sqrt(lab[, "a"]^2 + lab[, "b"]^2)
  H <- atan2(lab[, "b"], lab[, "a"]) * 180 / pi
  H[H < 0] <- H[H < 0] + 360
  cbind(L = lab[, "L"], C = C, H = H)
}

.oklch_to_oklab <- function(lch) {
  a <- lch[, "C"] * cos(lch[, "H"] * pi / 180)
  b <- lch[, "C"] * sin(lch[, "H"] * pi / 180)
  cbind(L = lch[, "L"], a = a, b = b)
}

# --- Weighted circular mean ---

.weighted_circular_mean <- function(angles_deg, weights) {
  rad <- angles_deg * pi / 180
  mean_deg <- atan2(sum(weights * sin(rad)), sum(weights * cos(rad))) * 180 / pi
  if (mean_deg < 0) mean_deg + 360 else mean_deg
}

# --- Main function ---

#' Compute the weighted mean of hex colors in a perceptually uniform color space
#'
#' Averages multiple hex colors in a perceptually uniform color space
#' (OKLCH or CIELUV); useful for reducing a set of similar colors to a
#' single representative when building a palette.
#'
#' @param hex A character vector of hex color values (e.g., `"#FF5733"`
#'   or `"#F53"`).
#' @param weights An optional numeric vector of non-negative weights, one per
#'   color. If `NULL` (default), all colors are weighted equally.
#' @param space The color space in which to compute the mean.
#'   `"oklch"` (default) uses the OKLCH space; `"hcl"` uses CIELUV.
#'
#' @return A single hex color string (`"#RRGGBB"`).
#'
#' @details
#' Hue is combined using a weighted circular mean, while lightness and
#' chroma use weighted arithmetic means. Operating in a perceptually uniform
#' space rather than RGB yields a more accurate result than a naive per-channel
#' RGB average.
#'
#' @export
#'
#' @examples
#' # Equal-weight mean of two colors
#' mean_color(c("#FF0000", "#0000FF"))
#'
#' # Weighted mean
#' mean_color(c("#FF0000", "#00FF00", "#0000FF"), weights = c(0.5, 0.3, 0.2))
#'
#' # Using CIELUV space
#' mean_color(c("#FF5733", "#33A1FF"), space = "hcl")
#'
#' @references
#' - Björn Ottosson, "OKLab" (2020). \url{https://bottosson.github.io/posts/oklab/}
#'
mean_color <- function(hex, weights = NULL, space = c("oklch", "hcl")) {
  space <- match.arg(space)

  if (!is.character(hex) || any(is.na(hex))) {
    stop("Argument 'hex' must be a character vector without NA values.", call. = FALSE)
  }
  if (!all(grepl("^#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})$", hex))) {
    stop("Argument 'hex' must contain valid hex color values (e.g., \"#FF5733\").")
  }
  if (!is.null(weights) && length(weights) != length(hex)) {
    stop("Argument 'weights' must either be NULL or of the same length as 'hex'.")
  }
  if (!is.null(weights) && sum(weights) == 0) {
    stop("Argument 'weights' must not sum to zero.", call. = FALSE)
  }

  # Normalize weights
  w <- if (is.null(weights)) rep(1 / length(hex), length(hex)) else weights / sum(weights)

  if (space == "hcl") {
    # hex -> sRGB -> CIELUV (Cartesian)
    rgb_mat <- t(grDevices::col2rgb(hex))
    luv <- grDevices::convertColor(rgb_mat, from = "sRGB", to = "Luv", scale.in = 255)
    # columns: L, u, v

    # Cartesian Luv -> polar (L, C, H)
    u <- luv[, "u"]
    v <- luv[, "v"]
    L <- luv[, "L"]
    C <- sqrt(u^2 + v^2)
    H <- atan2(v, u) * 180 / pi
    H[H < 0] <- H[H < 0] + 360

    # Weighted circular mean of hue
    h_mean <- .weighted_circular_mean(H, w)

    # Weighted arithmetic means of chroma and luminance
    c_mean <- sum(w * C)
    l_mean <- sum(w * L)

    # polarLUV -> sRGB hex (grDevices::hcl clips out-of-gamut to [0,255])
    return(grDevices::hcl(h_mean, c_mean, l_mean, fixup = TRUE))
  } else {
    rgb <- t(grDevices::col2rgb(hex)) / 255
    lch <- .oklab_to_oklch(.srgb_to_oklab(rgb))

    h_mean <- .weighted_circular_mean(lch[, "H"], w)
    c_mean <- sum(w * lch[, "C"])
    l_mean <- sum(w * lch[, "L"])

    lab <- .oklch_to_oklab(cbind(L = l_mean, C = c_mean, H = h_mean))
    rgb_out <- .oklab_to_srgb(lab)
    sprintf("#%02X%02X%02X", rgb_out[, 1], rgb_out[, 2], rgb_out[, 3])
  }
}
