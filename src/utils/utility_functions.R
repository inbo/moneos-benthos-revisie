find_file <- function(...) {
  rprojroot::find_root_file(...,
                            criterion = rprojroot::is_git_root)
}


render_report <- function(...) {
  oldwd <- getwd()
  setwd(find_file("src", "rapport"))
  bookdown::render_book(...)
  setwd(oldwd)
}


get_map_data_moneos <- function() {
  if (Sys.getenv("MAP_DATA_MONEOS") == "") {
    usethis::edit_r_environ("project")
    stop(
      paste0(
        "No MAP_DATA_MONEOS environmental variable found, ",
        "please edit the .Renviron file and add a line ",
        "MAP_DATA_MONEOS = '<path to the team drive procesbeheer folder>' ",
        "and restart R after you have done this."))
  }
  path <- Sys.getenv("MAP_DATA_MONEOS")
  return(path)
}
