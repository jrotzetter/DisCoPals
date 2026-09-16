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
