#' Use ggplot and afcharts to create accessible static charts
#'
#' Has a large number of options so that the need to modify using ggplot code
#' is minimised. Requires the y-axis to be continuous, but the x-axis can be of
#' any scale.
#'
#' @import ggplot2
#' @import afcharts
#' @importFrom ggrepel geom_text_repel
#'
#' @param plot_data Data to plot
#' @param aesthetics Mapping aesthetics using [ggplot2::aes()]; if you want a
#' line chart with a single group, remember to include `group = 1`
#' @param chart_type One of "stacked", "grouped", "line", or "distribution"; for
#' a bar chart with only one group, both "stacked" and "grouped" will work
#' ("stacked" is the default)
#' @param chart_direction One of "vertical" or "horizontal" ("horizontal" uses
#' [ggplot2::coord_flip()]; keep in mind that this will, by design, reverse the
#' order of items on the x-axis - this can be fixed using [rev()] in `aesthetics`
#' or by reversing the factor levels in `plot_data`)
#' @param labels If TRUE, adds [ggplot2::geom_text()] labels
#' @param label_size Default = 8; Font size of labels
#' @param label_colour Default = white; Colour of labels
#' @param label_position Default = `position_stack(0.5)` for labelling stacked
#' bar charts; Can take a ggplot `position_*` or a length 2 vector referring to
#' horizontal and vertical justification, respectively, e.g. `c(0, 0.5)` or
#' `c(NULL, 1)`
#' @param error_bars If TRUE, adds confidence intervals; for bar charts uses
#' [ggplot2::geom_errorbar()], and for line charts uses [ggplot2::geom_ribbon()]
#' @param series_breaks If supplied, will use the xintercept argument of
#' [ggplot2::geom_vline()] to add breaks to the x-axis at the specified points;
#' for line charts, remember to set the `group` argument in [aes()] to ensure
#' the lines break properly (for a single group, use the column containing the
#' series names, or for multiple groups, an [interaction()] between the grouping
#' column and the series column)
#' @param x_axis_title If supplied, adds a title to the x-axis
#' @param y_axis_title If supplied, adds a title to the y-axis
#' @param y_label_function Default = [scales::label_comma()]; a function which
#' is applied to the y-axis labels within the [ggplot2::scale_y_continuous()]
#' function (set to `NULL` to return the default labels)
#' @param y_axis_breaks Default = 8; the number of breaks to show on the y-axis
#' @param y_min Default = 0; The minimum value of the y-axis (must be either 0
#' or below 0)
#' @param y_max Default = NA; The maximum value of the y-axis
#' @param wrap_vars If supplied, will feed into the facets argument of
#' [ggplot2::facet_wrap()]
#' @param wrap_type One of "axis" (to return one plot with a wrapped axis) or
#' "plot" (to return multiple plots)
#' @param wrap_cols Feeds into the ncol argument of [ggplot2::facet_wrap()];
#' only used with `wrap_type = "plot"`
#' @param wrap_rows Feeds into the nrow argument of [ggplot2::facet_wrap()];
#' only used with `wrap_type = "plot"`
#' @param wrap_char_width Default = 25; Wraps facet labels to the specifies
#' number of characters
#' @param font_family Default = "GDS Transport Website"; This font must be
#' loaded for it to be used, and should only be used when publishing on gov.uk
#' @param font_size Default = 24; Font size for all chart text (except bar labels)
#' @param af_palette Which one of the [afcharts::af_colour_palettes] to use
#' @param custom_palette A vector of colours; If supplied, will use this rather
#' than any of the AF palettes (useful if you need to reorder an AF palette or
#' need more than 6 colours)
#' @param gridlines Fed into the grid argument of [afcharts::theme_af()]; If
#' supplied, will override defaults (i.e. "y" for vertical bar charts, "x"
#' for horizontal bar charts, and "xy" for line charts)
#' @param zero_line Default = TRUE; If set to FALSE, will remove origin line
#' (only do this if your y-axis does not include 0)
#' @param legend_position If supplied, will override defaults (i.e. "right"
#' for vertical charts and "top" for horizontal charts)
#' @param legend_justification If supplied, will override defaults (i.e. "top"
#' for vertical charts and "right" for horizontal charts)
#' @param legend_cols Fed into the ncol argument of [ggplot2::guide_legend()]
#' @param annotate_lines Uses [ggrepel::geom_text_repel()] to annotate lines in
#' line charts, instead of using a legend; set to `FALSE` to use a legend
#' instead, noting that it is more accessible if the resulting legend is in the
#' order of the lines (see examples for how to do this)
#' @param top_margin Default = 10; Top margin of plot, fed into the plot.margin
#' argument of [ggplot2::theme()] using [ggplot2::margin()]
#' @param right_margin Default = 0; Right margin of plot
#' @param bottom_margin Default = 0; Bottom margin of plot
#' @param left_margin Default = 0; Left margin of plot
#'
#' @examples
#' # Creating a line chart without annotating the lines
#' # but correcting the legend order
#'
#' test_df <- dplyr::tibble(
#'   letters = rep(LETTERS[1:4], 3),
#'   grouping = c(rep("1", 4), rep("2", 4), rep("3", 4)),
#'   numbers = c(seq(1000, 4000, 1000),
#'               seq(1250, 4250, 1000),
#'               seq(1500, 4500, 1000))
#' )
#'
#' var_order <- dplyr::filter(test_df, letters == max(letters)) |>
#'   dplyr::arrange(desc(numbers)) |>
#'   dplyr::pull(grouping)
#'
#' plot_df <- test_df |>
#'   dplyr::mutate(grouping = factor(grouping, levels = var_order))
#'
#' easy_plot(plot_df, aes(x = letters, y = numbers,
#'                        colour = grouping, group = grouping),
#'           chart_type = "line", annotate_lines = FALSE)
#'
#' @author Farm Business Survey team ([fbs.queries@defra.gov.uk](mailto:fbs.queries@defra.gov.uk))
#'
#' @export

easy_plot <- function(plot_data, aesthetics, chart_type = c("stacked", "grouped", "line", "distribution"),
                      chart_direction = c("vertical", "horizontal"),
                      labels = F, label_size = 8, label_colour = "white",
                      label_position = position_stack(0.5),
                      error_bars = F, series_breaks = NULL,
                      x_axis_title = NULL, y_axis_title = NULL,
                      y_label_function = scales::label_comma(),
                      y_axis_breaks = 8, y_min = 0, y_max = NA,
                      wrap_vars = NULL, wrap_type = c("axis", "plot"),
                      wrap_cols = NULL, wrap_rows = NULL, wrap_char_width = 25,
                      font_family = "GDS Transport Website", font_size = 24,
                      af_palette = names(afcharts::af_colour_palettes),
                      custom_palette = NULL, gridlines = NULL, zero_line = TRUE,
                      legend_position = NULL, legend_justification = NULL,
                      legend_cols = NULL, annotate_lines = TRUE,
                      top_margin = 10, right_margin = 0, bottom_margin = 0, left_margin = 0) {

  # Setup ####
  ## Match agruments ####
  chart_type <- arg_match(chart_type)
  chart_direction <- arg_match(chart_direction)
  wrap_type <- arg_match(wrap_type)

  ## Palette ####
  colour_palette <- unname(if (all(is.null(custom_palette))) afcharts::af_colour_palettes[[arg_match(af_palette)]] else custom_palette)

  ## Bar position ####
  bar_position <- if (chart_type %in% c("stacked", "distribution")) {
    "stack"
  } else {
    position_dodge2(preserve = "single")
  }

  ## Fix missing y label function ####
  if (is.null(y_label_function)) {
    y_label_function <- \(x) x
  }

  ## Y min over 1 error ####
  if (y_min > 0) {
    stop("The origin (y_min) must be either 0 or below 0")
  }

  # Base plot ####
  p_aes <- ggplot(plot_data, mapping = aesthetics) +
    scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0),
                       breaks = scales::pretty_breaks(y_axis_breaks),
                       labels = y_label_function) +
    labs(colour = NULL, fill = NULL, linetype = NULL,
         x = if (chart_direction == "vertical") x_axis_title else NULL,
         y = if (chart_direction == "horizontal") y_axis_title else NULL,
         subtitle = if (chart_direction == "horizontal") x_axis_title else y_axis_title)

  # Gridlines and data ####
  if (chart_type == "line") {

    gridlines <- if (is.null(gridlines)) "xy" else gridlines

    p <- p_aes +
      if ("colour" %in% names(aesthetics)) geom_line(linewidth = 1.5) else geom_line(linewidth = 1.5, colour = colour_palette[1])

  } else {

    gridlines <- if (is.null(gridlines) & chart_direction == "vertical") "y"
    else if (is.null(gridlines) & chart_direction == "horizontal") "x"
    else gridlines

    p <- p_aes +
      if ("fill" %in% names(aesthetics)) geom_col(position = bar_position) else geom_col(position = bar_position, fill = colour_palette[1])

  }

  # Labels ####
  if (labels & "Position" %in% class(label_position)) {

    p <- p +
      geom_text(position = label_position, size = label_size, colour = label_colour)

  } else if (labels) {

    if (is.null(label_position[1])) { label_position[1] <- 0.5 }
    if (is.null(label_position[2])) { label_position[2] <- 0.5 }

    p <- p +
      geom_text(size = label_size, colour = label_colour,
                hjust = label_position[1], vjust = label_position[2])

  }

  # Error bars ####
  if (error_bars & chart_type != "line") {
    p <- p +
      geom_errorbar(width = 0.3, linewidth = 3, position = position_dodge(.9), colour = "white") +
      geom_errorbar(width = 0.2, linewidth = 1, position = position_dodge(.9), colour = "black")
  }

  if (error_bars & chart_type == "line") {
    p <- p +
      geom_ribbon(alpha = 0.1, show.legend = F, colour = NA)
  }

  # Series breaks ####
  if (!is.null(series_breaks)) {
    p <- p +
      geom_vline(xintercept = series_breaks, colour = "#333333", linetype = "longdash", linewidth = 1)
  }

  # Theme and legend ####
  if (chart_direction == "vertical") {

    x_axis_text <- element_text()
    y_axis_text <- if (chart_type == "distribution") element_blank() else element_text(lineheight = .8)

    x_axis_line <- if (y_min < 0) element_blank() else element_line(colour = "black")
    y_axis_line <- if (chart_type == "distribution") element_blank() else element_line(colour = "black")

    legend_position <- if (chart_type == "line" & annotate_lines) {
      "none"
    } else if (is.null(legend_position) & chart_type != "grouped") {
      "right"
    } else if (is.null(legend_position) & chart_type == "grouped") {
      "top"
    } else { legend_position }

    legend_justification <- if (is.null(legend_justification) & chart_type != "grouped") {
      "top"
    } else if (is.null(legend_justification) & chart_type == "grouped") {
      "right"
    } else { legend_justification }

    p <- p +
      coord_cartesian(clip = "off") +
      theme_af(ticks = "none", legend = legend_position, grid = gridlines) +
      guides(fill = guide_legend(ncol = legend_cols))

  } else if (chart_direction == "horizontal") {

    colour_palette <- rev(colour_palette)

    x_axis_text <- if (chart_type == "distribution") element_blank() else element_text()
    y_axis_text <- element_text(lineheight = .8)

    x_axis_line <- if (chart_type == "distribution") element_blank() else element_line(colour = "black")
    y_axis_line <- if (y_min < 0) element_blank() else element_line(colour = "black")

    legend_position <- if (chart_type == "line" & annotate_lines) {
      "none"
    } else if (is.null(legend_position) & chart_type != "grouped") {
      "top"
    } else if (is.null(legend_position) & chart_type == "grouped") {
      "right"
    } else { legend_position }

    legend_justification <- if (is.null(legend_justification) & chart_type != "grouped") {
      "right"
    } else if (is.null(legend_justification) & chart_type == "grouped") {
      "top"
    } else { legend_justification }

    p <- p +
      coord_flip(clip = "off") +
      theme_af(ticks = "none", legend = legend_position, grid = gridlines) +
      guides(fill = guide_legend(reverse = TRUE, ncol = legend_cols))

  }

  # Line annotations (instead of legend) ####
  if (chart_type == "line" & annotate_lines) {

    p <- p +
      geom_text_repel(
        data = filter(plot_data, !!aesthetics$x == max(!!aesthetics$x)),
        mapping = aes(label = !!aesthetics$y), segment.color = NA,
        size = 8, hjust = "left", nudge_x = 0.8, lineheight = 0.7,
        show.legend = F, family = "GDS Transport Website", direction = "y")

  }

  # Facet wrapping ####
  if (!is.null(wrap_vars)) {

    p <- p +
      facet_wrap(wrap_vars, labeller = label_wrap_gen(width = wrap_char_width),
                 ncol = if (wrap_type == "plot") wrap_cols else if (chart_direction == "vertical") NULL else 1,
                 nrow = if (wrap_type == "plot") wrap_rows else if (chart_direction == "horizontal") NULL else 1,
                 strip.position = case_when(wrap_type == "plot" ~ "top",
                                            chart_direction == "horizontal" ~ "left",
                                            TRUE ~ "bottom")) +
      theme(strip.placement = "outside",
            strip.text.y.left = element_text(hjust = 1, angle = 0, lineheight = .8),
            strip.text.x.top = element_text(margin = margin(b = 15, t = 10)),
            panel.spacing = if (wrap_type == "plot") unit(1, "lines") else unit(7, "pt"))

  }

  # Subtitle position ####
  subtitle_vjust <- case_when(
    wrap_type == "plot" ~ "wrapped",
    chart_direction == "horizontal" & !is.null(x_axis_title) & legend_position == "top" ~ "legend",
    chart_type == "grouped" & legend_position == "top" ~ "legend",
    TRUE ~ "other")

  # Final adjustments ####
  p_final <- p +
    { if (zero_line) geom_hline(yintercept = 0) } +
    scale_fill_manual(values = colour_palette) +
    scale_colour_manual(values = colour_palette) +
    theme(text = element_text(family = font_family, size = font_size),
          plot.subtitle = element_text(vjust = case_when(subtitle_vjust == "wrapped" ~ -8,
                                                         subtitle_vjust == "legend" ~ -12,
                                                         TRUE ~ 0)),
          axis.text.x = x_axis_text,
          axis.text.y = y_axis_text,
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.line.x = x_axis_line,
          axis.line.y = y_axis_line,
          legend.justification = legend_justification,
          legend.key.spacing.y = unit(1, "lines"),
          legend.key.spacing.x = unit(1, "lines"),
          legend.key.width = unit(if_else(chart_type == "line", 2.5, 1.2), "lines"),
          plot.margin = margin(t = if_else(subtitle_vjust != "other", top_margin - 30, top_margin),
                               r = right_margin, b = bottom_margin, l = left_margin))

  return(p_final)

}
