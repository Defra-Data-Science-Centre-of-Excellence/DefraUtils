test_df <- dplyr::tibble(letters = LETTERS[1:4], numbers = seq(1000, 4000, 1000),
                         lower_ci = numbers - 100, upper_ci = numbers + 100)

test_that("default behaviour is as expected", {

  default_plot <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers, fill = letters)))

  # It's a bar plot
  expect_true("geom_col" %in% names(default_plot$plot$layers))

  default_plot_data <- set_names(default_plot$data, names(default_plot$plot$layers))

  # The x axis is correct
  expect_true(any(class(default_plot$layout$panel_params[[1]]$x$scale) == "ScaleDiscrete"))
  expect_true(all(default_plot$layout$panel_params[[1]]$x$breaks == test_df$letters))
  expect_null(default_plot$plot$labels$x)

  # The y axis is correct
  expect_true(any(class(default_plot$layout$panel_params[[1]]$y$scale) == "ScaleContinuous"))
  expect_null(default_plot$plot$labels$y)
  expect_equal(default_plot$layout$panel_params[[1]]$y$scale$get_labels(),
               c("0", "500", "1,000", "1,500", "2,000", "2,500", "3,000", "3,500", "4,000"))

  # There are no bar labels
  expect_false("geom_text" %in% names(default_plot$plot$layers))

  # There are no error bars
  expect_false("geom_errorbar" %in% names(default_plot$plot$layers))

  # There are no series breaks
  expect_false("geom_vline" %in% names(default_plot$plot$layers))

  # There is only one facet
  expect_equal(nrow(ggplot2::panel_cols(ggplot2::ggplot_gtable(default_plot))), 1)
  expect_equal(nrow(ggplot2::panel_rows(ggplot2::ggplot_gtable(default_plot))), 1)

  # The font is correct
  expect_equal(default_plot$plot$theme$text$family, "GDS Transport Website")
  expect_equal(default_plot$plot$theme$text$size, 24)

  # The palette is correct
  expect_true(all(default_plot_data$geom_col$fill == afcharts::af_colour_values[1:4]))

  # Zero line is there, and is the first layer
  expect_true(names(default_plot$plot$layers)[1] == "geom_hline")
  expect_equal(default_plot$plot$layers$geom_hline$data$yintercept, 0)

  # Gridlines are only on the y-axis
  expect_equal(default_plot$plot$theme$panel.grid.major.x, ggplot2::element_blank())
  expect_equal(class(default_plot$plot$theme$panel.grid.major.y)[1], "ggplot2::element_line")

  # Legend is correctly positioned
  expect_equal(default_plot$plot$theme$legend.position, "right")
  expect_equal(default_plot$plot$theme$legend.justification, "top")
  expect_null(default_plot$plot$guides$guides[[1]]$params$ncol)

  # Margins are correct
  expect_equal(default_plot$plot$theme$plot.margin, ggplot2::margin(10, 0, 0, 0))

})

test_that("chart_type argument works to create line plot", {

  line_chart <- ggplot2::ggplot_build(
    easy_plot(bind_rows(mutate(test_df, numbers = numbers - 500, l = "1"),
                        mutate(test_df, l = "2"),
                        mutate(test_df, numbers = numbers + 500, l = "3")),
              aes(x = letters, y = numbers, colour = l, group = l),
              chart_type = "line"))

  # It's a line chart
  expect_true("geom_line" %in% names(line_chart$plot$layers))

  line_chart_data <- set_names(line_chart$data, names(line_chart$plot$layers))

  # Axes are correct
  expect_true(any(class(line_chart$layout$panel_params[[1]]$x$scale) == "ScaleDiscrete"))
  expect_true(all(line_chart$layout$panel_params[[1]]$x$breaks == test_df$letters))
  expect_true(any(class(line_chart$layout$panel_params[[1]]$y$scale) == "ScaleContinuous"))
  expect_true(all(unique(line_chart_data$geom_line$colour) == afcharts::af_colour_values[1:3]))

  # Correct annotations have been added
  expect_true("geom_text_repel" %in% names(line_chart$plot$layers))
  expect_equal(unique(line_chart$plot$layers$geom_text_repel$data$l), c("1", "2", "3"))

  # Legend is not included
  expect_equal(line_chart$plot$theme$legend.position, "none")

})

test_that("annotate_lines argument works", {

  line_chart_legend <- ggplot2::ggplot_build(
    easy_plot(bind_rows(mutate(test_df, numbers = numbers - 500, l = "1"),
                        mutate(test_df, l = "2"),
                        mutate(test_df, numbers = numbers + 500, l = "3")),
              aes(x = letters, y = numbers, colour = l, group = l),
              chart_type = "line", annotate_lines = FALSE))

  # Annotations have not been added
  expect_false(names(line_chart_legend$plot$layers)[2] == "geom_text_repel")

  # Legend is included
  expect_equal(line_chart_legend$plot$theme$legend.position, "right")
  expect_equal(line_chart_legend$plot$theme$legend.justification, "top")

})

test_that("chart_type argument works to create boxplots", {

  # Normal boxplot method
  boxplot <- ggplot2::ggplot_build(
    easy_plot(tibble(letters = rep(LETTERS[1:4], 10),
                     value = runif(40, min = -15, max = 50)),
              aes(x = letters, y = value),
              chart_type = "boxplot", y_min = -15, y_max = 50))

  # It's a boxplot
  expect_true("geom_boxplot" %in% names(boxplot$plot$layers))

  boxplot_data <- set_names(boxplot$data, names(boxplot$plot$layers))

  # Axes are correct
  expect_true(any(class(boxplot$layout$panel_params[[1]]$x$scale) == "ScaleDiscrete"))
  expect_true(all(boxplot$layout$panel_params[[1]]$x$breaks == LETTERS[1:4]))
  expect_true(any(class(boxplot$layout$panel_params[[1]]$y$scale) == "ScaleContinuous"))
  expect_true(all(unique(boxplot_data$geom_boxplot$fill) == afcharts::af_colour_values["orange"]))

  # Using the 'identity' argument
  identity_boxplot <- ggplot2::ggplot_build(
    easy_plot(tibble(letters = LETTERS[1:4],
                     ymin = c(-1, -3, 4, 0),
                     lower = c(2, -1, 6, 3),
                     middle = c(4, 2, 7, 5),
                     upper = c(7, 4, 11, 8),
                     ymax = c(10, 8, 13, 11)),
              aes(x = letters, ymin = ymin, lower = lower,
                  middle = middle, upper = upper, ymax = ymax),
              chart_type = "boxplot", y_min = -5))

  # It's a boxplot
  expect_true("geom_boxplot" %in% names(identity_boxplot$plot$layers))

  identity_boxplot_data <- set_names(identity_boxplot$data, names(identity_boxplot$plot$layers))

  # Axes are correct
  expect_true(any(class(identity_boxplot$layout$panel_params[[1]]$x$scale) == "ScaleDiscrete"))
  expect_true(all(identity_boxplot$layout$panel_params[[1]]$x$breaks == LETTERS[1:4]))
  expect_true(any(class(identity_boxplot$layout$panel_params[[1]]$y$scale) == "ScaleContinuous"))
  expect_true(all(unique(identity_boxplot$geom_boxplot$fill) == afcharts::af_colour_values["orange"]))

})

test_that("chart_direction argument works", {

  horizontal_chart <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers),
              chart_direction = "horizontal"))

  # Axes are correct
  expect_true("geom_col" %in% names(horizontal_chart$plot$layers))
  expect_true(any(class(horizontal_chart$layout$panel_params[[1]]$x$scale) == "ScaleContinuous"))
  expect_true(any(class(horizontal_chart$layout$panel_params[[1]]$y$scale) == "ScaleDiscrete"))

  # Gridlines are only on the y-axis
  expect_equal(class(horizontal_chart$plot$theme$panel.grid.major.x)[1], "ggplot2::element_line")
  expect_equal(horizontal_chart$plot$theme$panel.grid.major.y, ggplot2::element_blank())

})

test_that("arguments to alter y-axis work", {

  # Provide no y label function
  null_y_label <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers), y_label_function = NULL))

  expect_equal(null_y_label$layout$panel_params[[1]]$y$scale$get_labels(),
               seq(0, 4000, 500))

  # Set y labels to show in £
  gbp_y_label <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers),
              y_label_function = scales::label_currency(prefix = "£")))

  expect_equal(gbp_y_label$layout$panel_params[[1]]$y$scale$get_labels(),
               c("£0", "£500", "£1,000", "£1,500", "£2,000",
                 "£2,500", "£3,000", "£3,500", "£4,000"))

  # Change number of y axis breaks
  fewer_y_breaks <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers), y_axis_breaks = 4))

  expect_equal(fewer_y_breaks$layout$panel_params[[1]]$y$scale$get_labels(),
               c("0", "1,000", "2,000", "3,000", "4,000"))

  # Change y limits
  change_y_limits <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers), y_min = -1000, y_max = 5000))

  expect_equal(dplyr::first(change_y_limits$layout$panel_params[[1]]$y$scale$get_labels()),
               "-1,000")
  expect_equal(dplyr::last(change_y_limits$layout$panel_params[[1]]$y$scale$get_labels()),
               "5,000")

  # y_min above 0 gives an error
  expect_error(easy_plot(test_df, aes(x = letters, y = numbers), y_min = 1000),
               "The origin \\(y_min\\) must be either 0 or below 0")

})

test_that("adding labels works", {

  add_labels <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers, label = letters), labels = TRUE))

  add_labels_data <- set_names(add_labels$data, names(add_labels$plot$layers))

  expect_true("geom_text" %in% names(add_labels$plot$layers))
  expect_equal(add_labels_data$geom_text$label, LETTERS[1:4])
  expect_equal(unique(add_labels_data$geom_text$hjust), 0.5)
  expect_equal(unique(add_labels_data$geom_text$vjust), 0.5)
  expect_equal(unique(add_labels_data$geom_text$size), 8)
  expect_equal(unique(add_labels_data$geom_text$colour), "white")

  alter_labels <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers, label = letters),
              labels = TRUE, label_size = 10, label_colour = "black",
              label_position = c(0, 1)))

  alter_labels_data <- set_names(alter_labels$data, names(alter_labels$plot$layers))

  expect_equal(unique(alter_labels_data$geom_text$hjust), 0)
  expect_equal(unique(alter_labels_data$geom_text$vjust), 1)
  expect_equal(unique(alter_labels_data$geom_text$size), 10)
  expect_equal(unique(alter_labels_data$geom_text$colour), "black")

})

test_that("adding error bars works", {

  add_error_bars <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers, ymin = lower_ci, ymax = upper_ci),
              error_bars = TRUE))

  expect_true("geom_errorbar" %in% names(add_error_bars$plot$layers))

  add_error_bars_data <- set_names(add_error_bars$data, names(add_error_bars$plot$layers))
  add_error_bars_data_eb <- add_error_bars_data[grepl("error", names(add_error_bars_data))]

  expect_equal(add_error_bars_data_eb[[1]]$ymin, seq(900, 3900, 1000))
  expect_equal(add_error_bars_data_eb[[1]]$ymax, seq(1100, 4100, 1000))
  expect_equal(unique(add_error_bars_data_eb[[1]]$colour), "white")
  expect_equal(unique(add_error_bars_data_eb[[1]]$linewidth), 3)

  expect_equal(add_error_bars_data_eb[[2]]$ymin, seq(900, 3900, 1000))
  expect_equal(add_error_bars_data_eb[[2]]$ymax, seq(1100, 4100, 1000))
  expect_equal(unique(add_error_bars_data_eb[[2]]$colour), "black")
  expect_equal(unique(add_error_bars_data_eb[[2]]$linewidth), 1)

})

test_that("adding series breaks works", {

  add_series_breaks <- ggplot2::ggplot_build(
    easy_plot(tibble::add_row(test_df, letters = c("B", "C"), numbers = c(2200, 3100)) |>
                dplyr::arrange(letters) |>
                dplyr::mutate(series = c("a", "a", "b", "b", "c", "c")),
              aes(x = letters, y = numbers, group = series), chart_type = "line",
              series_breaks = c("B", "C"), annotate_lines = F))

  expect_true("geom_vline" %in% names(add_series_breaks$plot$layers))

  add_series_breaks_data <- set_names(add_series_breaks$data, names(add_series_breaks$plot$layers))

  expect_equal(add_series_breaks$plot$layers$geom_vline$data$xintercept, c("B", "C"))
  expect_equal(unique(add_series_breaks_data$geom_line$group), 1:3)
  expect_equal(unique(add_series_breaks_data$geom_vline$colour), "#333333")
  expect_equal(unique(add_series_breaks_data$geom_vline$linetype), "longdash")

})

test_that("facet wrapping works", {

  # Default - wrap on axis
  wrap_axis <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers), wrap_vars = "letters"))

  expect_true(all(wrap_axis$data[[1]]$PANEL == 1:4))
  expect_equal(names(wrap_axis$layout$facet$params$facets), "letters")
  expect_equal(nrow(ggplot2::panel_cols(ggplot2::ggplot_gtable(wrap_axis))), 4)
  expect_equal(nrow(ggplot2::panel_rows(ggplot2::ggplot_gtable(wrap_axis))), 1)

  expect_equal(wrap_axis$layout$facet$params$strip.position, "bottom")
  expect_equal(wrap_axis$plot$theme$strip.placement, "outside")
  expect_equal(wrap_axis$plot$theme$strip.text.y.left,
               ggplot2::element_text(hjust = 1, angle = 0, lineheight = .8))
  expect_equal(wrap_axis$plot$theme$strip.text.x.top,
               ggplot2::element_text(margin = margin(b = 15, t = 10)))
  expect_equal(wrap_axis$plot$theme$panel.spacing, grid::unit(7, "pt"))

  # Change chart direction
  wrap_axis_h <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers),
              wrap_vars = "letters", chart_direction = "horizontal"))

  expect_equal(nrow(ggplot2::panel_cols(ggplot2::ggplot_gtable(wrap_axis_h))), 1)
  expect_equal(nrow(ggplot2::panel_rows(ggplot2::ggplot_gtable(wrap_axis_h))), 4)
  expect_equal(wrap_axis_h$layout$facet$params$strip.position, "left")

  # Change wrap type argument
  wrap_plot <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers), wrap_vars = "letters",
              wrap_type = "plot"))

  expect_equal(nrow(ggplot2::panel_cols(ggplot2::ggplot_gtable(wrap_plot))), 2)
  expect_equal(nrow(ggplot2::panel_rows(ggplot2::ggplot_gtable(wrap_plot))), 2)
  expect_equal(wrap_plot$plot$theme$panel.spacing, grid::unit(1, "lines"))

  # Change wrap cols and rows arguments
  change_wrap_cols <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers), wrap_vars = "letters",
              wrap_type = "plot", wrap_cols = 3))

  expect_equal(nrow(ggplot2::panel_cols(ggplot2::ggplot_gtable(change_wrap_cols))), 3)
  expect_equal(nrow(ggplot2::panel_rows(ggplot2::ggplot_gtable(change_wrap_cols))), 2)

  change_wrap_rows <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers), wrap_vars = "letters",
              wrap_type = "plot", wrap_rows = 4))

  expect_equal(nrow(ggplot2::panel_cols(ggplot2::ggplot_gtable(change_wrap_rows))), 1)
  expect_equal(nrow(ggplot2::panel_rows(ggplot2::ggplot_gtable(change_wrap_rows))), 4)

  # Can't find a way to test wrap_char_width argument, as the facets are
  # labelled using the label_wrap_gen function, which makes a quosure object

})

test_that("arguments to alter the font work", {

  change_font <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers),
              font_family = "Arial", font_size = 12))

  expect_equal(change_font$plot$theme$text$family, "Arial")
  expect_equal(change_font$plot$theme$text$size, 12)

})

test_that("arguments to alter the palette work", {

  change_palette <- ggplot2::ggplot_build(
    easy_plot(dplyr::filter(test_df, letters != "D"),
              aes(x = letters, y = numbers, fill = letters),
              af_palette = "sequential"))

  change_palette_data <- set_names(change_palette$data, names(change_palette$plot$layers))

  expect_true(all(change_palette_data$geom_col$fill == afcharts::af_colour_palettes$sequential))

  custom_palette <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers, fill = letters),
              custom_palette = c("wheat", "tomato", "forestgreen", "plum")))

  custom_palette_data <- set_names(custom_palette$data, names(custom_palette$plot$layers))

  expect_equal(custom_palette_data$geom_col$fill, c("wheat", "tomato", "forestgreen", "plum"))

})

test_that("argument to remove zero line works", {

  remove_zero_line <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers), zero_line = FALSE))

  expect_false("geom_hline" %in% names(remove_zero_line$plot$layers))

})

test_that("arguments to alter gridlines work", {

  swap_gridlines <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers), gridlines = "x"))

  expect_equal(class(swap_gridlines$plot$theme$panel.grid.major.x)[1], "ggplot2::element_line")
  expect_equal(swap_gridlines$plot$theme$panel.grid.major.y, ggplot2::element_blank())

  all_gridlines <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers), gridlines = "xy"))

  expect_equal(class(all_gridlines$plot$theme$panel.grid.major.x)[1], "ggplot2::element_line")
  expect_equal(class(all_gridlines$plot$theme$panel.grid.major.y)[1], "ggplot2::element_line")

  no_gridlines <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers), gridlines = "none"))

  expect_equal(no_gridlines$plot$theme$panel.grid.major.x, ggplot2::element_blank())
  expect_equal(no_gridlines$plot$theme$panel.grid.major.y, ggplot2::element_blank())

})

test_that("arguments to alter legend work", {

  move_legend <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers, fill = letters),
              legend_position = "top", legend_justification = "left"))

  expect_equal(move_legend$plot$theme$legend.position, "top")
  expect_equal(move_legend$plot$theme$legend.justification, "left")

  no_legend <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers, fill = letters),
              legend_position = "none"))

  expect_equal(no_legend$plot$theme$legend.position, "none")

  wrap_legend <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers, fill = letters),
              legend_cols = 2))

  expect_equal(wrap_legend$plot$guides$guides[[1]]$params$ncol, 2)

})

test_that("arguments to alter margin work", {

  change_margin <- ggplot2::ggplot_build(
    easy_plot(test_df, aes(x = letters, y = numbers),
              top_margin = 30, right_margin = 10, bottom_margin = 5, left_margin = 2))

  expect_equal(change_margin$plot$theme$plot.margin, ggplot2::margin(30, 10, 5, 2))

})

