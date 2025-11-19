#' @title Set a custom R console prompt with Git branch
#'
#' @author Josh Moatt
#'
#' @description Sets a custom R console prompt that displays the active Git
#'   branch (e.g., \code{[@main]>}) when working inside a Git-enabled project.
#'   If no Git repository is detected, the prompt defaults to \code{">"}.
#'
#'   The prompt is added to your .Rprofile file and can be scoped either
#'   globally (\code{"user"}) or to the current project (\code{"project"}). You
#'   can manually edit the profile using [usethis::edit_r_profile()].
#'
#' @details This function modifies your R console prompt to reflect the current
#'   Git branch, helping you stay aware of your working context. It works by
#'   appending a custom \code{.First()} function to your .Rprofile, which sets
#'   the prompt and attaches a task callback to keep it updated.
#'
#'   If a custom prompt already exists in the profile, the function will abort
#'   to avoid overwriting it. You can manually edit the profile if needed.
#'
#' @param scope Character. Either \code{"user"} to apply the prompt globally, or
#'   \code{"project"} to apply it only to the current project.
#'
#' @return No return value. Side effect: modifies the \code{.Rprofile} file to
#'   set the prompt.
#'
#' @examples
#' \dontrun{
#' # set user prompt
#' set_console_prompt("user")
#'
#' # set project prompt
#' set_console_prompt("project")
#' }
#'
#' @seealso [usethis::edit_r_profile()]
#'
#' @export
set_console_prompt <- function(scope) {
  # check correct scope selected
  if (!scope %in% c("user", "project")) {
    cli::cli_alert_danger("Invalid scope selected")
    cli::cli_alert_info('Set scope to either "user" or "project"')
    stop("Cancelled: Invalid scope selected!", call = FALSE)
  }

  # set path based on scope.
  if (scope == "user") {
    rprofile_path <- "~/.Rprofile"
  } else if (scope == "project") {
    rprofile_path <- "./.Rprofile"
  }

  # read current profile (if present)
  current_profile <- readLines(rprofile_path)

  # check for existing prompt in profile
  if (
    any(grepl("\\.First <- function\\(", current_profile)) &&
      any(grepl("my_prompt <- function\\(", current_profile))
  ) {
    cli::cli_alert_danger("Note: custom profile already present\n")
    cli::cli_alert_info("Edit using `usethis::edit_r_profile()` and try again.")
    stop("Cancelled: Prompt already present!", call = FALSE)
  } else {
    my_prompt <- '.First <- function() {
  my_prompt <- function(...) {
    git_branch <- suppressWarnings(
      system(
        "git rev-parse --abbrev-ref HEAD",
        ignore.stderr = TRUE,
        intern = TRUE
      )
    )
    # branch name
    if (length(git_branch) != 0) {
      console_msg <- paste0("[@", git_branch, "]> ")
    } else {
      console_msg  <- "> "
    }
    # set console_msg to prompt
    options(prompt = console_msg)
    invisible(TRUE)
  }
  my_prompt()
  addTaskCallback(my_prompt)
}
'
  }
  # write to file
  cat(my_prompt, file = rprofile_path, append = TRUE)

  cli::cli_alert_info("Restart R for prompt changes to take effect")
}
