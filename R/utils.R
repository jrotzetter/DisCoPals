#' Copy Palette to Clipboard
#'
#' Writes the R code to recreate the palette to the system clipboard, allowing
#' users to easily paste their palette into another script.
#'
#' @param pal A `character` vector of hex colors (named or unnamed).
#' @return Invisible `NULL`; called for side effect of copying to clipboard.
#'
#' @details
#' This function requires the \pkg{clipr} package. If not installed, the function
#' stops with an error. It also checks if a clipboard is available on the system.
#'
#' @export
#' @examplesIf interactive()
#' pal <- c("#FF0000", "#00FF00", "#0000FF")
#' copy_palette(pal)
copy_palette <- function(pal) {
  if (!interactive()) {
    warning("'copy_palette()' is only available in interactive sessions; no action taken.", call. = FALSE)
    return(invisible(NULL))
  }

  if (!requireNamespace("clipr", quietly = TRUE)) {
    stop("The 'clipr' package is required but not installed.\nPlease install it with: install.packages('clipr')", call. = FALSE)
  }

  # Validate input type
  if (!is.character(pal)) {
    stop("Argument 'pal' must be a character vector of hex colors.", call. = FALSE)
  }

  # Check clipboard availability
  if (!clipr::clipr_available()) {
    stop("No clipboard is available on this system. Cannot copy palette.", call. = FALSE)
  }

  clip_content <- paste(utils::capture.output(dput(pal)), collapse = "")
  clipr::write_clip(clip_content)

  message("Palette copied to clipboard!")
  invisible(NULL)
}


#' Check Required Packages
#'
#' @param pkgs A `character` vector of package names to check.
#'
#' @keywords internal
#' @export
#' @noRd
.require_packages <- function(pkgs) {
  installed <- vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)
  missing_pkgs <- pkgs[!installed]

  if (length(missing_pkgs) > 0) {
    if (length(missing_pkgs) == 1) {
      msg <- sprintf(
        "Package '%s' is required but not installed.\nPlease install it with: install.packages('%s')",
        missing_pkgs, missing_pkgs
      )
    } else {
      msg <- sprintf(
        "Packages '%s' are required but not installed.\nPlease install them with: install.packages(c('%s'))",
        paste(missing_pkgs, collapse = "', '"),
        paste(missing_pkgs, collapse = "', '")
      )
    }
    stop(msg, call. = FALSE)
  }
}


#' Check if Packages are Installed
#'
#' Returns a logical value indicating whether all specified packages are installed.
#' Useful for conditional execution in examples or tests.
#'
#' @param pkgs A `character` vector of package names.
#' @return `Logical`; TRUE if all packages are installed, FALSE otherwise.
#' @keywords internal
#' @export
#' @noRd
.has_packages <- function(pkgs) {
  if (length(pkgs) == 0) {
    return(TRUE)
  }
  all(vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE))
}


#' Convert Hex Colors to CIELAB Space
#'
#' Transforms a vector of hexadecimal color codes into the device-independent
#' **CIELAB** color space. This space was intended to be perceptually uniform,
#' meaning that Euclidean distances between coordinates approximate human
#' perception of color differences.
#'
#' @param hex A `character` vector of hex colors (e.g., `"#FF0000"`).
#'   Named or unnamed vectors are accepted; if named, row names of the output
#'   will reflect the unique hex values.
#' @param white_point A `character` string specifying the reference white point
#'   for Lab conversion. Must match standard illuminants accepted by [`grDevices::convertColor()`].
#'   `"D65"` (default) matches sRGB/screen standards.
#'   `"D50"` is typically used for print workflows.
#'
#' @return A numeric `matrix` with 3 columns:
#'   \describe{
#'     \item{L}{Lightness (0 = black, 100 = white).}
#'     \item{a}{Green (-) to Red (+) opponent channel.}
#'     \item{b}{Blue (-) to Yellow (+) opponent channel.}
#'   }
#'   Row names correspond to the unique input hex codes. Returns an empty
#'   matrix (0 rows) if input is empty.
#'
#' @details
#' Uses [`grDevices::col2rgb()`] to convert hex to RGB and
#' [`grDevices::convertColor`] to convert from sRGB to Lab. The input scale is
#' assumed to be 0-255 (standard 8-bit RGB). The `white_point` argument
#' controls the reference white used in the Lab conversion, allowing for
#' chromatic adaptation between screen (D65) and print (D50) workflows.
#' The function automatically deduplicates input colors to optimize performance;
#' the returned matrix contains only unique colors.
#'
#' @examples
#' # Convert a simple vector of colors
#' example_pal <- c("#FF0000", "#00FF00", "#0000FF")
#' convert_hex2Lab(example_pal)
#'
#' # Handle duplicates (returns only unique rows)
#' colors_dup <- c("#FF0000", "#FF0000", "#000000")
#' convert_hex2Lab(colors_dup)
#'
#' @seealso [grDevices::col2rgb()], [grDevices::convertColor()]
#' @export
convert_hex2Lab <- function(hex, white_point = "D65") {
  if (!is.character(hex) || any(is.na(hex))) {
    stop("Argument 'hex' must be a character vector without NA values.", call. = FALSE)
  }
  if (!all(grepl("^#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})$", hex))) {
    stop("Argument 'hex' must contain valid hex color values (e.g., \"#FF5733\").")
  }
  if (!is.character(white_point) || length(white_point) != 1) {
    stop("Argument 'white_point' must be a single character string.", call. = FALSE)
  }
  # Validate against known standard illuminants to prevent downstream errors
  valid_illuminants <- c("D65", "D50", "A", "B", "C", "E", "D55")
  if (!toupper(white_point) %in% valid_illuminants) {
    stop(sprintf(
      "Invalid 'white_point' '%s'. Must be one of: %s",
      white_point, paste(valid_illuminants, collapse = ", ")
    ), call. = FALSE)
  }

  unique_hex <- unique(hex)
  n <- length(unique_hex)

  # Handle empty input gracefully
  if (n == 0) {
    return(matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("L", "a", "b"))))
  }

  # Convert Hex -> RGB (col2rgb returns 3 x n matrix)
  rgb_mat <- grDevices::col2rgb(unique_hex)

  # Transpose to (n x 3) for convertColor
  sRGB <- t(rgb_mat)

  # Convert sRGB -> CIELAB
  # Ensure scale.in is set correctly (scale.in = 255 because col2rgb returns 0-255 values)
  lab_mat <- grDevices::convertColor(
    sRGB,
    from = "sRGB",
    to = "Lab",
    scale.in = 255,
    to.ref.white = white_point
  )

  # Assign dimension names
  rownames(lab_mat) <- unique_hex
  colnames(lab_mat) <- c("L", "a", "b")

  return(lab_mat)
}


#' Get a DisCoPals color palette
#'
#' Returns `n` colors from the specified palette. If `n` does not exceed the
#' palette length, the first `n` colors are returned; otherwise, colors are
#' interpolated via [grDevices::colorRampPalette()].
#'
#' @param n Number of colors to return.
#' @param pal Name of the palette (see [disco_palettes] for available
#'   names; default: "default").
#' @return A `character` vector of `n` hex color codes.
#' @export
disco_pal <- function(n, pal = "default") {
  palette <- disco_palettes[[pal]]
  if (is.null(palette)) {
    warning("Palette '", pal, "' not found, falling back to 'default'.")
    palette <- disco_palettes[["default"]]
  }
  palette <- unname(palette) # Prevent ggplot2 from interpreting the names as the expected levels (breaks)
  if (n <= length(palette)) {
    palette[seq_len(n)]
    # Alternatively for evenly-spaced sampling
    # palette[unique(round(seq(1, length(palette), length.out = n)))]
  } else {
    grDevices::colorRampPalette(palette)(n)
  }
}
