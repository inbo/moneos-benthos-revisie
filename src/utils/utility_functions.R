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
