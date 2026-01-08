#' Create stats infographic for sharing to Defra stats X account
#'
#' Uses [grid] to output an infographic into the Plots pane. To save it, click
#' 'Export', choose 'Save as Image...' and save as a PNG file. Adjust the height
#' and width to suit. NB: this function could use [grDevices::png()] rather than
#' manually exporting, but testing this has resulted in corrupted files. If you
#' can get it to work (including the option to edit height and width), please
#' feel free to submit your fix in a pull request.
#'
#' Once you have saved your infographic, send it to the user engagement team.
#' Include:
#' * The name of the publication
#' * A link to the publication
#' * The infographic .png file
#' * Plain text of the content of the infographic, which will be used in the alt
#' text of the image
#'
#' For example:
#'
#' Hi User Engagement team, my team have just published 'Energy use on farms in
#' England, 2023/24'. Please find the infographic attached. The text within the
#' infographic reads:
#'
#' Energy use on farms in England, 2023/24
#'
#' * Red diesel was used by __98%__ of farms
#' * __32% generate renewable energy__ (mostly solar power)
#' * __84%__ of solar-generating farm businesses have panels installed on
#' farm building rooftops
#' * __20%__ of farms had conducted a __carbon audit__
#'
#' Source: Defra, Farm Business Survey England 2023/24
#'
#' @importFrom grDevices dev.off
#' @importFrom grid grid.newpage pushViewport viewport grid.layout grid.rect
#' grid.raster grid.text grid.draw gpar unit
#' @importFrom gridtext richtext_grob
#' @importFrom brickster db_volume_list db_volume_read
#' @importFrom useful vplayout
#' @importFrom png readPNG
#' @importFrom stringr str_subset
#'
#' @param title The filepath to find the icons listed in the `icons` argument
#' @param subtitle The filepath to find the icons listed in the `icons` argument
#' @param source The source of the data within the infographic
#' @param key_points List of key points (use markdown formatting); between 3 and
#' 5 points works best
#' @param icons List of icons; the icons will go alongside each key point (for a
#' list of the available icons, run [view_available_icons()])
#'
#' @examples
#' title <- "Energy use on farms in England"
#' subtitle <- "2023/24"
#' source <- "Source: Defra, Farm Business Survey England 2023/24"
#' icons <- c("tractor", "sun", "barn", "form")
#' key_points <- list(
#'   "Red diesel was used<br>by **98%** of farms",
#'   "**32% generate renewable energy**<br>(mostly solar power)",
#'   "**84%** of solar-generating farm<br>businesses have panels installed on<br>farm building rooftops",
#'   "**20%** of farms had<br>conducted a **carbon audit**")
#'
#' create_defra_stats_infographic(title, subtitle, source, key_points, icons)
#'
#' @author Farm Business Survey team ([fbs.queries@defra.gov.uk](mailto:fbs.queries@defra.gov.uk))
#'
#' @export

create_defra_stats_infographic <- function(title, subtitle, source, key_points, icons) {

  # Ensure that the graphics content of the current device is clear
  if (names(dev.cur())[1] != "null device") dev.off()

  # Generate base
  grid.newpage()

  # Set columns and rows
  pushViewport(viewport(layout = grid.layout(nrow = 2 + length(key_points), ncol = 5,
                                             heights = unit(c(1, rep_len(1, length(key_points)), .5), "null"))))

  # Set background colour - Defra green
  defra_green <- c("#00A33B")
  grid.rect(gp = gpar(fill = defra_green, col = defra_green))

  # Add title and subtitle
  title_subtitle <- paste0("**", title, "**<br>", subtitle)
  grid.draw(richtext_grob(title_subtitle, vp = vplayout(1, 1:5), x = unit(0.02, "npc"), hjust = 0,
                          gp = gpar(col = "white", fontsize = 24, lineheight = 1.2)))

  # List all available icons
  icons_db_location <- "/Volumes/prd_dash_config/common/open/icons"

  icon_folders <- sapply(db_volume_list(icons_db_location)$contents, \(d) d$name)

  icon_list <- sapply(icon_folders, \(f) sapply(db_volume_list(file.path(icons_db_location, f))$contents,
                                                \(d) file.path(d$name))) %>%
    lapply(\(s) str_remove(s, "\\.png"))

  # Add all icons and key stats
  for (i in seq_along(key_points)) {

    icon_folder <- names(icon_list)[sapply(icon_list, \(l) icons[i] %in% l)]

    icon_file <- db_volume_read(
      path = file.path(icons_db_location, icon_folder, paste0(icons[i], ".png")),
      destination = tempfile())

    icon_img <- readPNG(icon_file)

    # Add icon
    grid.raster(icon_img, vp = vplayout(i + 1, 1), hjust = 0.5)

    # Add key stat
    grid.draw(richtext_grob(key_points[[i]], vp = vplayout(i + 1, 2:5), x = unit(0.05, "npc"), hjust = 0,
                            gp = gpar(col = "white", fontsize = 18, lineheight = 1.1)))
  }

  # Add source
  grid.text(source, vp = vplayout(length(key_points) + 2, 1:5),
            x = unit(0.02, "npc"), hjust = 0,
            gp = gpar(col = "white", fontsize = 14))

}


#' View icons
#'
#' Returns a graphic in the Plots pane of the available icons in
#' [create_defra_stats_infographic()]. If text is overlapping in the Plots pane,
#' click 'Zoom' and resize the window to suit.
#'
#' @importFrom dplyr %>% tibble
#' @importFrom brickster db_volume_list db_volume_read
#' @importFrom stringr str_remove
#' @importFrom tidyr separate
#' @importFrom grDevices dev.off
#' @importFrom grid grid.newpage pushViewport viewport grid.layout grid.rect
#' grid.raster grid.text grid.draw gpar unit
#' @importFrom useful vplayout
#' @importFrom png readPNG
#'
#' @author Farm Business Survey team ([fbs.queries@defra.gov.uk](mailto:fbs.queries@defra.gov.uk))
#'
#' @export

view_available_icons <- function() {

  icons_db_location <- "/Volumes/prd_dash_config/common/open/icons"

  icon_folders <- sapply(db_volume_list(icons_db_location)$contents, \(d) d$name)

  icon_list <- sapply(icon_folders, \(f) sapply(db_volume_list(file.path(icons_db_location, f))$contents,
                                                \(d) file.path(d$name))) %>%
    lapply(\(s) str_remove(s, "\\.png"))

  # Ensure that the graphics content of the current device is clear
  if (names(dev.cur())[1] != "null device") dev.off()

  # Generate base
  grid.newpage()

  # Set columns and rows
  pushViewport(viewport(layout = grid.layout(
    nrow = max(sapply(icon_list, length)) + 1,
    ncol = length(icon_list))))

  defra_green <- c("#00A33B")
  grid.rect(gp = gpar(fill = defra_green, col = defra_green))

  lapply(seq_along(icon_list), \(icon_type_i) {

    grid.text(names(icon_list)[icon_type_i],
              vp = vplayout(x = 1, y = icon_type_i),
              gp = gpar(col = "white", fontsize = 18))

    lapply(seq_along(icon_list[[icon_type_i]]), \(icon_i) {

      icon_file <- db_volume_read(
        path = paste0(icons_db_location, "/", names(icon_list)[icon_type_i],
                      "/", icon_list[[icon_type_i]][icon_i], ".png"),
        destination = tempfile())

      icon_img <- readPNG(icon_file)

      grid.raster(icon_img, vp = vplayout(x = 1 + icon_i, y = icon_type_i), hjust = 1)

      grid.text(icon_list[[icon_type_i]][icon_i],
                vp = vplayout(x = 1 + icon_i, y = icon_type_i), hjust = 0,
                gp = gpar(col = "white", fontsize = 13))

    })

  })

}
