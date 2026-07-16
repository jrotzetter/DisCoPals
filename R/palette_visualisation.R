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
