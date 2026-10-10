#' Analyze Color Palette Accessibility and Perceptual Differences
#'
#' Calculates pairwise color differences for a given palette using contrast ratio
#' and DeltaE metrics. All unique pairs of colors in the input palette are
#' generated and their perceptual differences and accessibility contrast computed.
#'
#' @param pal A `character` vector of hexadecimal color codes (e.g., `c("#FF0000", "#00FF00")`).
#' @param include_background `Logical`. If `TRUE` (default), white (`#FFFFFF`) and
#'   black (`#000000`) are appended to the palette before comparison to evaluate
#'   readability against standard backgrounds.
#' @param metric A `character` string specifying the DeltaE formula to use.
#'   Must be one of `"2000"` (CIEDE2000, best perceptual accuracy), `"1994"` (CIE94),
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
#'     \item `L_1`: Lightness (`L*`) of the first color.
#'     \item `C_1`: Chroma (`C*`) of the first color.
#'     \item `L_2`: Lightness (`L*`) of the second color.
#'     \item `C_2`: Chroma (`C*`) of the second color.
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
#' **Lightness and Chroma (returned as columns):**
#' - `L*` (lightness): how light or dark a color appears, on a scale from 0 (black)
#'   to 100 (white). A color with a high L* value (e.g., > 80) will look very pale
#'   and may be hard to read against a white background.
#' - `C*` (chroma): how "intense" or "vivid" a color appears. A value of 0 means
#'   the color is a shade of gray; higher values mean a more saturated, vivid color.
#'   For example, a pale pastel (e.g., light pink) has low chroma, while a
#'   bold/saturated red (e.g., fire-engine red) has high chroma.
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
    if (length(backgrounds_to_add) > 0) pal <- c(pal, backgrounds_to_add)
  }

  n <- length(pal)

  # Return NULL if fewer than 2 colors to compare
  if (n < 2) {
    return(NULL)
  }

  # Convert only unique colors to Lab once for efficiency
  unique_hex <- unique(pal)
  lab_mat <- convert_hex2Lab(unique_hex, white_point = white_point)

  # Per-color L* and C*
  L_star <- lab_mat[, "L"]
  C_star <- sqrt(lab_mat[, "a"]^2 + lab_mat[, "b"]^2)

  # Generate all unique pairs of indices
  idx <- utils::combn(n, 2)
  col1_idx <- idx[1, ]
  col2_idx <- idx[2, ]

  col1_hex <- pal[col1_idx]
  col2_hex <- pal[col2_idx]

  # Calculate Contrast Ratios (vectorized)
  cr_vals <- colorspace::contrast_ratio(col1_hex, col2_hex)

  # Calculate DeltaE values

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
    L_1 = L_star[col1_hex],
    C_1 = C_star[col1_hex],
    L_2 = L_star[col2_hex],
    C_2 = C_star[col2_hex],
    stringsAsFactors = FALSE
  )
}


#' Compute perceptual attributes for a color palette
#'
#' Converts a vector of hex colors to CIELAB space and returns a `data.frame`
#' with hue, lightness, chroma, warmth, and saturation for each color.
#' Duplicate hex values are removed (keeping the first occurrence).
#'
#' @param pal `Character` vector of hex color values (e.g., `"#FF5733"`).
#'   Must not contain `NA` values.
#' @param plot `Logical`. If `TRUE`, a swatch grid visualizing the attributes
#'   is printed. Default: `FALSE`.
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{`color`}{Hex string.}
#'     \item{`hue`}{Hue angle (degrees, 0–360).}
#'     \item{`L`}{Lightness (\eqn{L^*}).}
#'     \item{`C`}{Chroma (\eqn{C^* = \sqrt{a^{*2} + b^{*2}}}).}
#'     \item{`warmth`}{Warm–cool index (\eqn{a^* + b^*}), approximating
#'       a projection onto the warm–cool axis (orange ↔ cyan) in the
#'       \eqn{a^*b^*} plane. Higher values = warmer (reds, oranges, yellows);
#'       lower values = cooler (greens, blues, cyans). Note: this
#'       conflates hue direction with chroma, so a highly saturated
#'       green will be "cooler" than a desaturated blue.}
#'     \item{`S`}{Richter/Lübbe saturation (\eqn{C^* / \sqrt{C^{*2} + L^{*2}}}).
#'       Measures how chromatic the color appears relative to its brightness.
#'       Ranges from 0 (achromatic) approaching 1 for highly chromatic colors
#'       at low lightness.}
#'   }
#'
#' @examples
#' pal <- c("#FF5733", "#33FF57", "#3357FF", "#FF3357")
#' color_attributes(pal)
#'
#' @export
color_attributes <- function(pal, plot = FALSE) {
  if (!is.character(pal) || any(is.na(pal))) {
    stop("Argument 'pal' must be a character vector without NA values.", call. = FALSE)
  }
  if (!all(grepl("^#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})$", pal))) {
    stop("Argument 'pal' must contain valid hex color values (e.g., \"#FF5733\").",
      call. = FALSE
    )
  }
  if (!is.logical(plot) || length(plot) != 1 || is.na(plot)) {
    stop("Argument 'plot' must be a single non-NA logical value.", call. = FALSE)
  }

  pal <- pal[!duplicated(pal)]

  lab <- convert_hex2Lab(pal)
  chroma <- sqrt(lab[, "a"]^2 + lab[, "b"]^2)
  hue <- atan2(lab[, "b"], lab[, "a"]) * 180 / pi
  hue[hue < 0] <- hue[hue < 0] + 360

  warmth <- lab[, "a"] + lab[, "b"]

  denom <- sqrt(chroma^2 + lab[, "L"]^2)
  S <- ifelse(denom > 0, chroma / denom, 0)

  out <- data.frame(
    color = pal,
    hue = hue,
    lightness = lab[, "L"],
    chroma = chroma,
    warmth = warmth,
    saturation = S,
    stringsAsFactors = FALSE
  )

  if (plot) {
    print(plot_color_attributes(out))
  }

  out
}


#' Filter a color palette based on perceptual distance, contrast, lightness, and chroma
#'
#' Filters a vector of color hex codes, retaining only those colors whose
#' minimum perceptual distance (deltaE), minimum contrast ratio, maximum
#' lightness, and/or minimum chroma meet specified thresholds.
#' This helps ensure that all colors in the resulting palette are sufficiently
#' distinguishable, not too pale, and not too gray.
#'
#' @param pal A named or unnamed `character` vector of color hex codes
#'   (e.g., `c("#FF0000", "#00FF00")`). Duplicate hex codes are automatically
#'   removed (keeping the first occurrence) with a warning.
#' @param min_distance `Numeric` threshold for minimum deltaE (perceptual color distance).
#'   At least one threshold must be specified. Default is `NULL`.
#' @param min_contrast `Numeric` threshold for minimum contrast ratio.
#'   At least one threshold must be specified. Default is `NULL`.
#' @param max_lightness `Numeric` threshold for maximum lightness (L*). Colors with
#'   L* above this value are removed. Useful for excluding colors that are too
#'   pale to be readable against light backgrounds. At least one threshold must be
#'   specified. Default is `NULL`.
#' @param min_chroma `Numeric` threshold for minimum chroma (C*). Colors with
#'   C* below this value are removed. Useful for excluding near-gray or washed-out
#'   colors. At least one threshold must be specified. Default is `NULL`.
#' @param include_background `Logical`. If `TRUE`, includes standard background colors
#'   (white `#FFFFFF` and black `#000000`) in pairwise comparisons to test
#'   distinguishability against backgrounds. These background colors are excluded
#'   from the final result **unless** they were present in the original input `pal`.
#'   Default is `FALSE`.
#' @param metric `Character` specifying the CIE deltaE formula to use. Options are
#'   `"2000"` (default, CIEDE2000 - best correlation with human visual assessment),
#'    `"1994"`, or `"1976"`.
#'
#' @return A `character` vector of hex codes filtered from `pal` that meet the
#'   specified thresholds (inclusive). Names from the original `pal` vector are
#'   preserved, and the original order is maintained. Returns an empty
#'   `character` vector if no comparisons can be made or no colors meet thresholds.
#'
#' @details
#'   The function computes all pairwise comparisons using `analyze_palette()`
#'   and determines the minimum deltaE and contrast ratio for each color
#'   relative to all others, as well as each color's lightness (L*)
#'   and chroma (C*).
#'   A color is retained only if all specified thresholds are met. This ensures
#'   that every remaining color is sufficiently distinct, visible, and saturated.
#'
#'   **Thresholds are inclusive**: A color with a deltaE or contrast ratio exactly
#'   equal to the threshold is retained. For contrast ratio, this matches WCAG's
#'   convention that the threshold itself is a passing value. No analogous WCAG
#'   rule exists for deltaE.
#'
#'   **Background Handling**: When `include_background = TRUE`, the function
#'   temporarily adds white and black to the comparison set. After filtering,
#'   these backgrounds are removed from the result unless they were explicitly
#'   provided in the original `pal` input.
#'
#'   **Duplicate Handling**: Exact duplicate hex codes in the input are detected
#'   and reduced to a single occurrence (the first one) before comparison, with
#'   a warning issued.
#'
#'   If no colors meet the thresholds, a warning is issued. If some colors are
#'   filtered out, a message reports how many were retained.
#'
#'   ## Threshold guidance
#'
#'   **Contrast ratio** (W3C WCAG 2.2):
#'   `min_contrast` of 4.5 meets AA for normal text, 3.0 for large text
#'   and UI components, and 7.0 meets AAA for normal text.
#'
#'   **DeltaE** (CIEDE2000, categorical palettes):
#'   `min_distance` of 5 gives noticeable at-a-glance difference,
#'   10 gives strong distinction (recommended default), and 15 is
#'   high-distinction (useful for color-vision-deficiency robustness).
#'   The value of 10 is supported by empirical discrimination thresholds
#'   (\eqn{dE_{00} \approx 9.2}{dE_00 ~ 9.2} covers 99.7% of observers; see
#'   <https://commons.erau.edu/edt/103/>).
#'   Note: with *n* colors, all \eqn{\binom{n}{2} = n(n-1)/2} pairwise distances
#'   must meet the threshold simultaneously, so the constraint grows quadratically
#'   and practical palettes rarely exceed ~8--10 colors at `min_distance = 10`.
#'
#'   **Lightness** (L*):
#'   `max_lightness` of 80 removes very pale colors that may be hard to read
#'   against white backgrounds. Values of 70--85 should ensure visibility in
#'   digital contexts.
#'
#'   **Chroma** (C*):
#'   `min_chroma` of 10 removes near-gray colors that may be confused with
#'   neutral tones. Values of 15--20 ensure a minimum level of colorfulness,
#'   excluding colors that appear near-achromatic.
#'
#' @examples
#' # Define a palette
#' example_pal <- c(
#'   "blue" = "#0000FF",
#'   "navy" = "#000080",
#'   "bright_red" = "#FF0000",
#'   "dark_red" = "#8B0000",
#'   "almost_blue" = "#0000FE", # Very close to blue
#'   "pale_yellow" = "#F3E5AB", # Very light (L* = 90)
#'   "medium_gray" = "#848482" # Achromatic (C* = 1)
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
#' # Exclude colors that are too light
#' filter_palette(example_pal, max_lightness = 80)
#'
#' # Exclude near-gray colors
#' filter_palette(example_pal, min_chroma = 15)
#'
#' # Combine all criteria
#' filter_palette(example_pal,
#'   min_distance = 10,
#'   min_contrast = 1.5,
#'   max_lightness = 80,
#'   min_chroma = 15
#' )
#'
#' # Include backgrounds in comparison but exclude from result
#' filter_palette(example_pal, min_distance = 10, include_background = TRUE)
#'
#' @export
filter_palette <- function(pal, min_distance = NULL, min_contrast = NULL,
                           max_lightness = NULL, min_chroma = NULL,
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
  if (!is.null(max_lightness) && !is.numeric(max_lightness)) {
    stop("Argument 'max_lightness' must be NULL or a numeric value.", call. = FALSE)
  }
  if (!is.null(min_chroma) && !is.numeric(min_chroma)) {
    stop("Argument 'min_chroma' must be NULL or a numeric value.", call. = FALSE)
  }
  if (is.null(min_distance) && is.null(min_contrast) &&
    is.null(max_lightness) && is.null(min_chroma)) {
    stop("At least one threshold must be specified.", call. = FALSE)
  }

  # Handle empty or single-color input
  if (length(pal) == 0) {
    return(character(0L))
  }

  if (length(pal) == 1) {
    message("Only one color provided; no comparisons possible. Returning input.")
    return(pal)
  }

  # Normalize input to uppercase to ensure case-insensitive match if hex codes
  # vary, especially for exact matching with background
  pal <- toupper(pal)

  # --- 2. Snapshot Original Backgrounds ---

  # Define the standard backgrounds the analyze_palette function adds
  std_backgrounds <- c("#FFFFFF", "#000000")

  # Record which of these were ALREADY in the input palette
  original_backgrounds_present <- pal[pal %in% std_backgrounds]

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
    return(character(0L))
  }

  # --- 4. Minima Calculation ---

  # Create a symmetric list of all colors and their corresponding metrics
  all_colors <- c(combos$Col_1, combos$Col_2)
  all_de <- c(combos$deltaE, combos$deltaE)
  all_cr <- c(combos$contrast_ratio, combos$contrast_ratio)
  all_L <- c(combos$L_1, combos$L_2)
  all_C <- c(combos$C_1, combos$C_2)

  # Calculate minimum metric for each color across all its pairs
  min_de <- tapply(all_de, all_colors, min)
  min_cr <- tapply(all_cr, all_colors, min)
  min_L <- tapply(all_L, all_colors, min)
  min_C <- tapply(all_C, all_colors, min)

  # Create a lookup table
  minima <- data.frame(
    Color = names(min_de),
    Value_de = as.numeric(min_de),
    Value_cr = as.numeric(min_cr),
    Value_L = as.numeric(min_L),
    Value_C = as.numeric(min_C),
    stringsAsFactors = FALSE
  )

  # --- 5. Filter Logic ---

  keep <- rep(TRUE, nrow(minima))
  if (!is.null(min_distance)) keep <- keep & (minima$Value_de >= min_distance)
  if (!is.null(min_contrast)) keep <- keep & (minima$Value_cr >= min_contrast)
  if (!is.null(max_lightness)) keep <- keep & (minima$Value_L <= max_lightness)
  if (!is.null(min_chroma)) keep <- keep & (minima$Value_C >= min_chroma)

  valid_hex <- minima$Color[keep]

  # --- 6. Conditional Background Removal ---

  if (include_background && length(valid_hex) > 0) {
    # Identify backgrounds currently in the result
    backgrounds_in_result <- valid_hex[valid_hex %in% std_backgrounds]

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
