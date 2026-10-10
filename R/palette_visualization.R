#' Visualize a Color Palette
#'
#' Displays a vector of colors as a barplot or a grid of rectangles.
#'
#' @param pal A `character` vector of hex colors (e.g., `#FF0000`).
#' @param as_bar `Logical`. If `TRUE`, displays a barplot; otherwise, a grid of rectangles.
#' @param cex_label `Numeric`. Character expansion factor for labels. Controls text size. Default: 0.7.
#'
#' @return Invisible `NULL`. The function is called for its side effect (producing a plot).
#' @export
#' @examples
#' example_pal <- c("#E69F00", "#56B4E9", "#009E73")
#' show_colors(example_pal)
#' show_colors(example_pal, as_bar = FALSE)
show_colors <- function(pal, as_bar = TRUE, cex_label = 0.7) {
  pal_name <- deparse(substitute(pal))
  len_pal <- length(pal)

  if (len_pal == 0) {
    stop("Argument 'pal' is empty. No hex color codes to plot.")
  }

  # Validation for hex codes (3 or 6 digits, with optional alpha channel)
  hex_pattern <- "^#[0-9A-Fa-f]{3,8}$"
  if (!is.character(pal) || !all(grepl(hex_pattern, pal))) {
    stop("Argument 'pal' must be a character vector of valid hex color codes (e.g., '#RRGGBB').")
  }

  if (as_bar) {
    graphics::barplot(rep(1, len_pal),
      col = pal,
      border = NA,
      main = paste0(pal_name, " (n=", len_pal, ")"),
      axes = FALSE,
      axisnames = TRUE,
      names.arg = paste0(1:len_pal, ": ", pal),
      las = 2,
      cex.names = cex_label
    )
  } else {
    # Adapted and modified from http://www.r-graph-gallery.com/42-colors-names/
    num_col <- ceiling(sqrt(len_pal))
    num_row <- ceiling(len_pal / num_col)
    total_slots <- num_row * num_col

    orig_params <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(orig_params))

    graphics::par(mar = c(0, 0, 2, 0))
    graphics::plot(0,
      type = "n", xlim = c(0, 1), ylim = c(0, 1),
      axes = FALSE, xlab = "", ylab = "", main = paste0(pal_name, " (n=", len_pal, ")")
    )

    # Explicitly pad colors with NA for empty / transparent grid slots
    pal_padded <- c(pal, rep(NA, total_slots - len_pal))

    graphics::rect(
      xleft = rep((0:(num_col - 1) / num_col), num_row),
      ybottom = sort(rep((0:(num_row - 1) / num_row), num_col), decreasing = TRUE),
      xright = rep((1:num_col / num_col), num_row),
      ytop = sort(rep((1:num_row / num_row), num_col), decreasing = TRUE),
      border = "grey50",
      # col = pal[seq(1, num_row * num_col)]
      col = pal_padded
    )

    # Color index and hex code
    # pal_labels <- c(paste0(1:len_pal, ": ", pal), rep("", num_row * num_col - len_pal))
    pal_labels <- c(paste0(1:len_pal, ": ", pal), rep("", total_slots - len_pal))
    graphics::text(
      x = rep((0:(num_col - 1) / num_col), num_row) + 0.04,
      y = sort(rep((0:(num_row - 1) / num_row), num_col), decreasing = TRUE) + 0.02,
      labels = pal_labels,
      cex = cex_label
    )
  }
  invisible(NULL)
}


#' Visualize Color Combination Metrics
#'
#' Visualizes color contrast and deltaE metrics for pairs of colors as a grid of
#' labeled color swatches using `ggplot2`. For large datasets, plots can be
#' split by the first color to improve readability.
#'
#' @param combinations A `data.frame` containing color comparison metrics, where
#'   each row represents a color pair. This object is typically returned by
#'   \code{\link{analyze_palette}}. It must contain the following columns:
#'   \itemize{
#'     \item \code{Col_1}: The first color in the pair (used for grouping/splitting).
#'     \item \code{Col_2}: The second color in the pair (required for data integrity).
#'     \item \code{contrast_ratio}: Numeric contrast ratio between the colors.
#'     \item \code{deltaE}: Numeric color difference metric.
#'   }
#'   Row names are used as pair identifiers.
#' @param pairs_subset Optional `character` vector of row names to subset the combinations.
#'   If provided, only these pairs are plotted.
#' @param separation_threshold `Numeric`. If the number of rows exceeds this value,
#'   the function splits the output into separate plots per unique value in `Col_1`.
#'   Default is 50.
#'
#' @return A `ggplot` object if a single plot is generated. If the plot is split
#'   (due to `separation_threshold`), the function prints multiple plots and
#'   returns `NULL` invisibly.
#' @export
#'
#' @seealso \code{\link{analyze_palette}} for generating the required input data.
#'
#' @examplesIf .has_packages(c("tibble", "dplyr", "tidyr", "ggplot2", "stringr", "ggtext", "forcats", "colorspace", "spacesXYZ"))
#' example_pal <- c("#E69F00", "#56B4E9", "#009E73")
#' combos <- analyze_palette(example_pal, include_background = FALSE)
#' rownames(combos) <- c("Pair1", "Pair2", "Pair3")
#'
#' # Plot all combinations
#' plot_combinations(combos)
#'
#' # Plot a subset of pairs
#' plot_combinations(combos, pairs_subset = c("Pair1", "Pair3"))
#'
#' # Force splitting by first color
#' plot_combinations(combos, separation_threshold = 2)
plot_combinations <- function(combinations, pairs_subset = NULL, separation_threshold = 50) {
  .require_packages(c("tibble", "dplyr", "tidyr", "ggplot2", "stringr", "ggtext", "forcats", "rlang"))

  # Validate input
  required_cols <- c("contrast_ratio", "deltaE", "Col_1", "Col_2")
  if (!all(required_cols %in% names(combinations))) {
    stop("combinations must include 'contrast_ratio', 'deltaE', 'Col_1', and 'Col_2' columns.")
  }

  # Apply subset if requested
  if (!is.null(pairs_subset)) {
    combinations <- combinations[rownames(combinations) %in% pairs_subset, , drop = FALSE]
  }

  # Internal plotting function
  .plot_colorswatch <- function(df) {
    if (nrow(df) == 0) {
      return(NULL)
    }

    df |>
      tibble::rownames_to_column(var = "pair") |>
      dplyr::mutate(
        contrast_ratio = as.character(round(.data$contrast_ratio, 3)),
        deltaE = as.character(round(.data$deltaE, 3))
      ) |>
      dplyr::select(.data$pair, .data$Col_1, .data$Col_2, .data$contrast_ratio, .data$deltaE) |>
      tidyr::pivot_longer(!.data$pair, names_to = "category", values_to = "hex") |>
      dplyr::mutate(
        cell_text = .data$hex,
        hex = ifelse(grepl("^#", .data$hex), .data$hex, "#FFFFFF"),
        category = paste0("**", .data$category, "**")
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = .data$category, y = forcats::fct_rev(forcats::fct_inorder(.data$pair)))) +
      ggplot2::geom_tile(ggplot2::aes(fill = .data$hex), colour = "grey50") +
      ggplot2::scale_fill_identity() +
      ggplot2::geom_text(ggplot2::aes(label = .data$cell_text), size = 4) +
      ggplot2::labs(x = NULL, y = "Color Pair") +
      ggplot2::scale_y_discrete(expand = c(0, 0)) +
      ggplot2::scale_x_discrete(expand = c(0, 0), position = "top") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x.top = ggtext::element_markdown(
          box.color = "black", linewidth = 1, linetype = 1,
          padding = grid::unit(c(5, 50, 5, 50), "pt"), size = 10
        ),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
  }

  # Plotting logic
  if (nrow(combinations) > separation_threshold) {
    # Split by Col_1
    unique_colors <- unique(combinations$Col_1)
    for (color in unique_colors) {
      subset_df <- combinations[combinations$Col_1 == color, , drop = FALSE]
      p <- .plot_colorswatch(subset_df)
      if (!is.null(p)) print(p)
    }
    invisible(NULL)
  } else {
    # Single plot
    return(.plot_colorswatch(combinations))
  }
}


#' Plot color attributes as a labeled swatch grid
#'
#' Visualizes a color attribute data frame (as returned by [color_attributes()]
#' as a grid of labeled color swatches using `ggplot2`. Each row is a color;
#' columns show the swatch alongside its hue, lightness, chroma, warmth, and
#' saturation values.
#'
#' @param df A `data.frame` with columns: `color`, `hue`, `lightness`,
#'   `chroma`, `warmth`, `saturation`. Typically the return value of
#'   [color_attributes()].
#'
#' @return A `ggplot` object.
#' @export
#'
#' @seealso [color_attributes()]
#'
#' @examplesIf .has_packages(c("tibble", "dplyr", "tidyr", "ggplot2", "forcats", "ggtext"))
#' pal <- c("#FF5733", "#33FF57", "#3357FF", "#FF3357")
#' plot_color_attributes(color_attributes(pal))
#'
plot_color_attributes <- function(df) {
  .require_packages(c("tibble", "dplyr", "tidyr", "ggplot2", "forcats", "ggtext"))

  required_cols <- c("color", "hue", "lightness", "chroma", "warmth", "saturation")
  if (!all(required_cols %in% names(df))) {
    stop("df must include columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(df) == 0) {
    return(invisible(NULL))
  }

  .text_color <- function(hex) {
    if (!grepl("^#[0-9A-Fa-f]{6}$", hex)) {
      return(NA_character_)
    }

    hex <- sub("^#", "", hex)
    if (nchar(hex) == 3) hex <- paste0(strsplit(hex, "")[[1]], strsplit(hex, "")[[1]])
    r <- strtoi(substr(hex, 1, 2), 16L) / 255
    g <- strtoi(substr(hex, 3, 4), 16L) / 255
    b <- strtoi(substr(hex, 5, 6), 16L) / 255
    ifelse(0.299 * r + 0.587 * g + 0.114 * b > 0.5, "black", "white")
  }

  df |>
    tibble::as_tibble() |>
    dplyr::mutate(index = seq_len(dplyr::n())) |>
    dplyr::mutate(
      hue = round(.data$hue, 1),
      lightness = round(.data$lightness, 1),
      chroma = round(.data$chroma, 1),
      warmth = round(.data$warmth, 1),
      saturation = round(.data$saturation, 3)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(setdiff(required_cols, "color")),
      names_to = "category", values_to = "value"
    ) |>
    dplyr::mutate(
      value = as.character(.data$value)
    ) |>
    dplyr::bind_rows(
      tibble::tibble(
        index = seq_len(nrow(df)),
        color = df$color,
        category = "color",
        value = df$color
      )
    ) |>
    dplyr::mutate(
      category = factor(.data$category, levels = required_cols),
      hex = ifelse(.data$category == "color", .data$value, "#FFFFFF"),
      text_col = ifelse(.data$category == "color",
        vapply(.data$value, .text_color, character(1L)),
        "black"
      ),
      index = factor(.data$index)
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$category,
        y = forcats::fct_rev(.data$index)
      )
    ) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$hex), colour = "grey50") +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_text(ggplot2::aes(label = .data$value, color = .data$text_col), size = 4) +
    ggplot2::scale_colour_identity() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::scale_x_discrete(expand = c(0, 0), position = "top", labels = ~ paste0("**", .x, "**")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x.top = ggtext::element_markdown(
        size = 10
      ),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
    )
}
