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


#' Analyze Color Palette Accessibility and Perceptual Differences
#'
#' Calculates pairwise color differences for a given palette using contrast ratio
#' and DeltaE metrics. This function generates all unique pairs of colors in the
#' input palette and computes perceptual differences and accessibility contrast.
#'
#' @param pal A `character` vector of hexadecimal color codes (e.g., `c("#FF0000", "#00FF00")`).
#' @param include_background `Logical`. If `TRUE` (default), white (`#FFFFFF`) and
#'   black (`#000000`) are appended to the palette before comparison to evaluate
#'   readability against standard backgrounds.
#' @param metric A `character` string specifying the DeltaE formula to use.
#'   Must be one of `"2000"` (CIEDE2000, most accurate), `"1994"` (CIE94),
#'   or `"1976"` (CIE76). Defaults to `"2000"`.
#' @param white_point A `character` string specifying the reference white point
#'   for Lab conversion. `"D65"` (default) matches sRGB/screen standards.
#'   `"D50"` is typically used for print workflows. Passed to [convert_hex2Lab()].
#'
#' @return A `data.frame` with the following columns:
#'   \itemize{
#'     \item `Col_1`: Hex code of the first color in the pair.
#'     \item `Col_2`: Hex code of the second color in the pair.
#'     \item `contrast_ratio`: Numeric contrast ratio between the two colors.
#'     \item `deltaE`: Numeric DeltaE value representing perceptual color difference.
#'   }
#'   Returns `NULL` if the resulting palette has fewer than 2 colors.
#'
#' @details
#' The function relies on the `colorspace` package for contrast ratios and
#' `spacesXYZ` for DeltaE calculations. If `include_background = TRUE`, standard
#' black and white backgrounds are added unless they already exist in the palette
#' (case-insensitive match).
#'
#' **White Point Selection:**
#' - Use `"D65"` (default) for digital palettes (web, R figures, UI) as it matches
#'   the native white point of sRGB hex codes.
#' - Use `"D50"` if comparing against print standards or physical swatches under
#'   standard lighting conditions.
#'
#' @examplesIf .has_packages(c("colorspace", "spacesXYZ"))
#' # Define a simple palette
#' example_pal <- c("#E69F00", "#56B4E9", "#009E73")
#'
#' # Analyze colors with default settings
#' analyze_palette(example_pal)
#'
#' # Analyze without adding standard background colors using an older metric
#' analyze_palette(example_pal, include_background = FALSE, metric = "1976")
#'
#' @seealso [colorspace::contrast_ratio()], [spacesXYZ::DeltaE()]
#' @export
analyze_palette <- function(pal,
                            include_background = TRUE,
                            metric = c("2000", "1994", "1976"),
                            white_point = "D65") {
  .require_packages(c("colorspace", "spacesXYZ"))

  if (!is.character(pal) || any(is.na(pal))) {
    stop("Argument 'pal' must be a character vector without NA values.", call. = FALSE)
  }

  metric <- match.arg(metric, choices = c("2000", "1994", "1976"))

  # Ensure case-insensitive matching (e.g., "#ffffff" vs "#FFFFFF")
  pal <- toupper(pal)

  if (include_background) {
    # Define standard backgrounds
    std_backgrounds <- c("#FFFFFF", "#000000")

    # Only add backgrounds that are NOT already in the palette
    backgrounds_to_add <- std_backgrounds[!std_backgrounds %in% pal]

    # Append only the missing backgrounds
    if (length(backgrounds_to_add) > 0) {
      pal <- c(pal, backgrounds_to_add)
    }
  }

  n <- length(pal)

  # Return NULL if fewer than 2 colors to compare
  if (n < 2) {
    return(NULL)
  }

  # Generate all unique pairs of indices
  idx <- utils::combn(n, 2)
  col1_idx <- idx[1, ]
  col2_idx <- idx[2, ]

  col1_hex <- pal[col1_idx]
  col2_hex <- pal[col2_idx]

  # Calculate Contrast Ratios (vectorized)
  cr_vals <- colorspace::contrast_ratio(col1_hex, col2_hex)

  # Calculate DeltaE values
  # Convert only unique colors to Lab once for efficiency
  unique_hex <- unique(pal)
  lab_mat <- convert_hex2Lab(unique_hex, white_point = white_point)

  # Map pair indices to Lab coordinates
  # drop = FALSE ensures matrix structure is kept even if only 1 row
  lab1 <- lab_mat[col1_hex, , drop = FALSE]
  lab2 <- lab_mat[col2_hex, , drop = FALSE]

  # Calculate DeltaE (vectorized)
  de_vals <- spacesXYZ::DeltaE(lab1, lab2, metric = metric)

  data.frame(
    Col_1 = col1_hex,
    Col_2 = col2_hex,
    contrast_ratio = cr_vals,
    deltaE = de_vals,
    stringsAsFactors = FALSE
  )
}


#' Filter a color palette based on perceptual distance and contrast
#'
#' Filters a vector of color hex codes, retaining only those colors whose
#' minimum perceptual distance (deltaE) and/or minimum contrast ratio to any
#' other color in the palette is **greater than or equal to** specified thresholds.
#' This helps ensure that all colors in the resulting palette are sufficiently
#' distinguishable.
#'
#' @param pal A named or unnamed `character` vector of color hex codes
#'   (e.g., `c("#FF0000", "#00FF00")`). Duplicate hex codes are automatically
#'   removed (keeping the first occurrence) with a warning.
#' @param min_distance `Numeric` threshold for minimum deltaE (perceptual color distance).
#'   At least one of `min_distance` or `min_contrast` must be specified. Default is `NULL`.
#' @param min_contrast `Numeric` threshold for minimum contrast ratio.
#'   At least one of `min_distance` or `min_contrast` must be specified. Default is `NULL`.
#' @param include_background `Logical`. If `TRUE`, includes standard background colors
#'   (white `#FFFFFF` and black `#000000`) in pairwise comparisons to test
#'   distinguishability against backgrounds. These background colors are excluded
#'   from the final result **unless** they were present in the original input `pal`.
#'   Default is `FALSE`.
#' @param metric `Character` specifying the CIE deltaE formula to use. Options are
#'   `"2000"` (default, most perceptually uniform), `"1994"`, or `"1976"`.
#'
#' @return A `character` vector of hex codes filtered from `pal` that meet the
#'   specified thresholds (inclusive). Names from the original `pal` vector are
#'   preserved, and the original order is maintained. Returns an empty
#'   `character` vector if no comparisons can be made or no colors meet thresholds.
#'
#' @details
#' The function computes all pairwise comparisons using `analyze_palette()` and
#' determines the minimum deltaE and contrast ratio for each color relative to
#' all others. A color is retained only if its minimum values are **greater than
#' or equal to** the specified thresholds. This ensures that every remaining
#' color is sufficiently distinct from all others in the palette.
#'
#' **Thresholds are inclusive**: A color with a deltaE or contrast ratio exactly
#' equal to the threshold is retained. This aligns with accessibility standards
#' like WCAG, which treat threshold values as passing.
#'
#' **Background Handling**: When `include_background = TRUE`, the function
#' temporarily adds white and black to the comparison set. After filtering,
#' these backgrounds are removed from the result unless they were explicitly
#' provided in the original `pal` input.
#'
#' **Duplicate Handling**: Exact duplicate hex codes in the input are detected
#' and reduced to a single occurrence (the first one) before comparison, with
#' a warning issued.
#'
#' If no colors meet the thresholds, a warning is issued. If some colors are
#' filtered out, a message reports how many were retained.
#'
#' @examples
#' # Define a palette
#' example_pal <- c(
#'   "blue" = "#0000FF",
#'   "navy" = "#000080",
#'   "bright_red" = "#FF0000",
#'   "dark_red" = "#8B0000",
#'   "almost_blue" = "#0000FE" # Very close to blue
#' )
#'
#' # Filter by minimum distance (inclusive)
#' filter_palette(example_pal, min_distance = 10)
#'
#' # Filter by minimum contrast ratio (inclusive)
#' filter_palette(example_pal, min_distance = 0, min_contrast = 2)
#'
#' # Filter by both criteria
#' filter_palette(example_pal, min_distance = 5, min_contrast = 1.5)
#'
#' # Include backgrounds in comparison but exclude from result
#' filter_palette(example_pal, min_distance = 10, include_background = TRUE)
#'
#' @export
filter_palette <- function(pal, min_distance = NULL, min_contrast = NULL,
                           include_background = FALSE,
                           metric = c("2000", "1994", "1976")) {
  metric <- match.arg(metric, choices = c("2000", "1994", "1976"))

  # --- 1. Input Validation ---

  if (!is.null(min_distance) && !is.numeric(min_distance)) {
    stop("Argument 'min_distance' must be NULL or a numeric value.", call. = FALSE)
  }
  if (!is.null(min_contrast) && !is.numeric(min_contrast)) {
    stop("Argument 'min_contrast' must be NULL or a numeric value.", call. = FALSE)
  }
  if (is.null(min_distance) && is.null(min_contrast)) {
    stop("At least one of 'min_distance' or 'min_contrast' must be specified.", call. = FALSE)
  }

  # Handle empty or single-color input
  if (length(pal) == 0) {
    return(character(0))
  }
  if (length(pal) == 1) {
    message("Only one color provided; no comparisons possible. Returning input.")
    return(pal)
  }

  # Normalize input to uppercase to ensure exact matching with background
  pal <- toupper(pal)

  # --- 2. Snapshot Original Backgrounds ---

  # Define the standard backgrounds the analyze_palette function adds
  std_backgrounds <- c("#FFFFFF", "#000000")

  # Record which of these were ALREADY in the input palette
  # Ensure case-insensitive match if hex codes vary (e.g. "#ffffff" vs "#FFFFFF")
  original_backgrounds_present <- pal[toupper(pal) %in% toupper(std_backgrounds)]

  # Preserve names and track original indices
  pal_names <- names(pal)
  if (is.null(pal_names)) pal_names <- character(length(pal))

  pal_df <- data.frame(
    hex = unname(pal),
    name = pal_names,
    orig_idx = seq_along(pal), # Track original position
    stringsAsFactors = FALSE
  )

  # Handle duplicates: keep first occurrence
  if (anyDuplicated(pal_df$hex)) {
    warning("Duplicate colors detected. Keeping only first occurrence.", call. = FALSE)
    pal_df <- pal_df[!duplicated(pal_df$hex), ]
  }

  # --- 3. Get Comparisons ---

  combos <- analyze_palette(pal_df$hex, include_background = include_background, metric = metric)
  if (is.null(combos) || nrow(combos) == 0) {
    warning("No color comparisons returned.", call. = FALSE)
    return(character(0))
  }

  # --- 4. Minima Calculation ---

  # Create a symmetric list of all colors and their corresponding metrics
  all_colors <- c(combos$Col_1, combos$Col_2)
  all_de <- c(combos$deltaE, combos$deltaE)
  all_cr <- c(combos$contrast_ratio, combos$contrast_ratio)

  # Calculate minimum metric for each color across all its pairs
  min_de <- tapply(all_de, all_colors, min)
  min_cr <- tapply(all_cr, all_colors, min)

  # Create a lookup table
  minima <- data.frame(
    Color = names(min_de),
    Value_de = as.numeric(min_de),
    Value_cr = as.numeric(min_cr),
    stringsAsFactors = FALSE
  )

  # --- 5. Filter Logic ---

  keep <- rep(TRUE, nrow(minima))
  if (!is.null(min_distance)) keep <- keep & (minima$Value_de >= min_distance)
  if (!is.null(min_contrast)) keep <- keep & (minima$Value_cr >= min_contrast)

  valid_hex <- minima$Color[keep]

  # --- 6. Conditional Background Removal ---

  if (include_background && length(valid_hex) > 0) {
    # Identify backgrounds currently in the result
    backgrounds_in_result <- valid_hex[toupper(valid_hex) %in% toupper(std_backgrounds)]

    # Determine which to remove: those in result BUT NOT in original input
    to_remove <- setdiff(backgrounds_in_result, original_backgrounds_present)

    if (length(to_remove) > 0) {
      valid_hex <- valid_hex[!valid_hex %in% to_remove]
    }
  }

  # --- 7. Restore Order and Names ---

  # Match filtered hex codes back to the original (deduplicated) dataframe
  matched <- match(valid_hex, pal_df$hex)
  result_df <- pal_df[matched, ]

  # Sort by original index to preserve input order
  result_df <- result_df[order(result_df$orig_idx), ]

  result <- result_df$hex
  # Restore names only if the original input had names
  names(result) <- if (!all(pal_names == "")) result_df$name else NULL

  # --- 8. Messaging ---

  n_original <- nrow(pal_df)
  n_retained <- length(result)

  if (n_retained == 0) {
    warning("No colors met the specified thresholds.", call. = FALSE)
  } else if (n_retained < n_original) {
    message(sprintf("Filtered palette: %d of %d colors retained.", n_retained, n_original))
  }

  return(result)
}
