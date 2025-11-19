#' @title Connect RStudio to your GitHub account using a Personal Access Token
#'
#' @author Josh Moatt ([Joshua.Moatt@defra.gov.uk](mailto:Joshua.Moatt@defra.gov.uk))
#'
#' @description This is a simple function which connects RStudio to GitHub using
#'   a Personal Access Token (PAT), allowing you to work on GitHub repositories.
#'   This function is for local RStudio installs only, not for on the DASH
#'   platform. For the dash platform, use [DefraUtils::connect_github_ssh()]
#'   instead.
#'
#' @details This function will set your GitHub credentials and Personal Access
#'   Token (PAT) and connect RStudio to GitHub. This is essential if you want to
#'   work in RStudio in projects stored as GitHub repositories.
#'
#'   It uses [system()] to run the code in the terminal to set your credentials,
#'   and [gitcreds::gitcreds_set()] and [credentials::set_github_pat()] packages
#'   to connect your RStudio to GitHub.
#'
#'   [gitcreds::gitcreds_set()] is an interactive function and will prompt users
#'   for input. To replace existing credentials and PAT choose option 2. You
#'   will then be prompted for you PAT. Your PAT should be changed every 30 days
#'   to ensure security.
#'
#'   Note: For this function to work you must:
#'
#' \itemize{
#'    \item have git installed on your local machine
#'    \item have a GitHub account
#'    \item have created a PAT on GitHub.
#' }
#'
#'   An additional feature of this function (not mentioned in the Defra
#'   instructions) is to add the [credentials::set_github_pat()] function call
#'   to an .Rprofile. This will ensure your PAT is set for every R session,
#'   meaning you won't need to provide your PAT when running functions such as
#'   [devtools::install_github()]. The function will check if an .Rprofile file
#'   already exists, if one does it will add the code to the bottom of the
#'   existing profile. If the .Rprofile file does not exist, the function will
#'   create one and add the code to it.
#'
#'   This function is for local RStudio installs only, not for on the DASH
#'   platform.
#'
#' @note This function is interactive, requiring user input. Therefore it is not
#'   suitable for automated scripts.
#'
#' @param username string containing GitHub username
#'
#' @param email string containing email address used for GitHub account
#'
#' @return GitHub credentials and PAT set
#'
#' @examples
#' \dontrun{
#' # Set GitHub credentials
#' connect_github_pat(
#'   username = "my_github_username",
#'   email = "my_email"
#' )
#' }
#'
#' @seealso [gitcreds::gitcreds_set()], [credentials::set_github_pat()],
#'   [DefraUtils::connect_github_ssh()]
#'
#' @export
connect_github_pat <- function(
  username,
  email
) {
  # set proxy
  Sys.setenv(http_proxy = "http://127.0.0.1:9000")

  ## this will run the terminal commands ----
  # proxy
  system("git config --global http.proxy http://127.0.0.1:9000")
  # GitHub UUN
  system(glue::glue('git config --global user.name "{username}"'))
  # email
  system(glue::glue('git config --global user.email "{email}"'))

  # check list
  cli::cli_alert_success("GitHub credentials set too:")
  system("git config --global --list")

  # set gitcreds - this will prompt for user input
  gitcreds::gitcreds_set()

  # set PAT
  credentials::set_github_pat()

  # check pat set
  pat <- Sys.getenv("GITHUB_PAT")
  cli::cli_alert_success("GitHub pat is {pat}")
  cli::cli_alert_warning("Check this is as expected!")

  ## update .Rprofile ----
  # set path
  rprofile_path <- "~/.Rprofile"

  # create R profile if not exist already
  if (!file.exists(rprofile_path)) {
    file.create(rprofile_path)
  }
  # check if function call present
  rprofile_lines <- readLines(rprofile_path)

  # check for function
  cred_line <- grep("credentials", rprofile_lines)

  # update .Rprofile
  if (length(cred_line) == 0) {
    # create function as string
    cred_func <- "credentials::set_github_pat(verbose = FALSE)\n"

    # add to .Rprofile
    rprofile_lines <- c(rprofile_lines, cred_func)

    # write lines
    writeLines(rprofile_lines, rprofile_path)

    cli::cli_alert_success(
      "Added {.code credentials::set_github_pat()} to .Rprofile",
      wrap = TRUE
    )
  } else if (length(cred_line) > 0) {
    cli::cli_alert_success(
      "{.code credentials::set_github_pat()} already in .Rprofile",
      wrap = TRUE
    )
  }
}
