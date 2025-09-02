#' Prepare publication files
#'
#' Copies publication files (run script and commentary scripts) to a specified
#' target directory. If you are using the SCE, will save them in a folder
#' with the same name as the `publication` argument. If you are on desktop,
#' will just save the files without putting them in a new folder.
#'
#' @importFrom usethis create_project proj_activate
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_replace
#'
#' @param publication String; The name of the publication in kebab-case, e.g.
#' "farm-accounts-england"
#' @param target_dir File path; Where to copy the publication files to. If using
#' the SCE, this should be a general folder where code is saved. If using
#' desktop, this should be a directory specific to the relevant publication run.
#' @param dev_mode Logical, default = FALSE; Are you in development mode (i.e.
#' working in a GitHub repo)?
#' @param overwrite Logical, default = TRUE; If files already exist, should they
#' be overwritten?
#' @param open Logical, default = TRUE; If `TRUE`,
#' [activates][usethis::proj_activate()] the new project:
#'
#'   * If using RStudio desktop, the package is opened in a new session.
#'   * If on RStudio server, the current RStudio project is activated.
#'   * Otherwise, the working directory and active project is changed.
#'
#' @return If open = TRUE, will open the newly created project (not recommended
#' in dev mode), otherwise will return a message stating whether the files have
#' been moved successfully or not
#'
#' @export

prepare_publication_files <- function(publication, target_dir, dev_mode = FALSE,
                                      overwrite = TRUE, open = TRUE) {

  source_dir <- if (!dev_mode) {

    file.path(find.package("FBSpublications"), publication)

  } else { file.path(getwd(), "inst", publication) }

  if (is.na(source_dir)) {

    stop("Publication directory not found")

  } else {

    on_sce <- grepl("ranch", Sys.info()['nodename'], fixed = TRUE)

    if (on_sce) {

      # If using the SCE, copy over the whole publication folder
      file.copy(source_dir, target_dir, overwrite = overwrite, recursive = TRUE)

      final_dir <- file.path(target_dir, publication)

    } else {

      # If using desktop, just copy over the files
      lapply(list.files(source_dir, full.names = T),
             file.copy, target_dir, overwrite = overwrite)

      final_dir <- target_dir

    }

    # Create a project within the new directory

    path <- file.path(final_dir, paste0(publication, ".Rproj"))
    template_path <- system.file("templates/template-project.Rproj", package = "FBSpublications")
    file.copy(template_path, path, overwrite = overwrite)

    #create_project(final_dir, open = FALSE)
    #file.rename(paste0(final_dir, "/", word(final_dir, -1, sep = "\\/"), ".Rproj"),
    #            paste0(final_dir, "/", publication, ".Rproj"))

    # Change project options
    #read_lines(paste0(final_dir, "/", publication, ".Rproj")) %>%
    #  str_replace("AlwaysSaveHistory: Default", "AlwaysSaveHistory: No") %>%
    #  write_lines(paste0(final_dir, "/", publication, ".Rproj"))

    # If open = TRUE, open the project
    if (open) {

      proj_activate(final_dir)

    } else if (on_sce) {

      message(file.path(final_dir, publication), " directory ",
              ifelse(dir.exists(file.path(final_dir, publication)),
                     "successfully ", "NOT "), "created")

    } else {

      message(publication, " files ",
              ifelse(file.exists(path), "successfully ", "NOT "), "created")

    }

  }

}
