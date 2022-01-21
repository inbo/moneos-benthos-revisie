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

load_excel <- function(path) {
  name <- path |>
    fs::path_file() |>
    fs::path_ext_remove() |>
    tolower()
  assign(name, readxl::read_excel(path), inherits = TRUE)
}


genereer_landschap <- function(
  bedekking_vector = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3),
  intens_per_m2 = 0.002, #= 100 punten per 5 ha
  clusrad = 25, # radius van een cluster
  mufactor = 10,
  opp_ha = 50,
  verspreiding = c("geclusterd", "random", "regular"),
  mask = TRUE) {
  require("spatstat")
  rescale_factor <- sqrt(opp_ha) * 100
  if ("geclusterd" %in% verspreiding) {
    # geclusterd
    clu <- rMatClust(intens_per_m2/mufactor, scale = clusrad, mu = mufactor,
                     win = owin(c(0,rescale_factor),c(0,rescale_factor)))
    #clu <- rescale(clu, 1/rescale_factor, unitname = c("metre", "metres"))
    # permutatie van quantielen
    rqs <- sample(x = seq(0.01, 0.99, length.out = clu$n),
                  size = clu$n,
                  replace = FALSE)
    clup <- vector(mode = "list", length = length(bedekking_vector))
    for (i in 1:length(bedekking_vector)) {
      shape <- optim(par = 1, method = "Brent", lower = 0, upper = 50,
                     fn = function(x) {
                       (bedekking_vector[i] -
                          area.owin(discs(centres = clu,
                                          radii = qgamma(rqs,
                                                         shape = x,
                                                         scale = 1) ,
                                          mask = TRUE, eps = 1)) /
                          area.owin(Window(clu)))^2})
      clup[[i]] <- discs(centres = clu,
                         radii = qgamma(rqs,
                                        shape = shape$par,
                                        scale = 1),
                         mask = mask, eps = 1)
    }
  } else {
    clup <- NULL
  }
  
  if ("random" %in% verspreiding) {
    # random
    ran <- rpoispp(intens_per_m2, win = owin(c(0,rescale_factor),
                                             c(0,rescale_factor)))
    #ran <- rescale(ran, 1/rescale_factor, unitname = c("metre", "metres"))
    # permutatie van quantielen
    rqs <- sample(x = seq(0.01, 0.99, length.out = ran$n),
                  size = ran$n,
                  replace = FALSE)
    ranp <- vector(mode = "list", length = length(bedekking_vector))
    for (i in 1:length(bedekking_vector)) {
      shape <- optim(par = 1, method = "Brent", lower = 0, upper = 50,
                     fn = function(x) {
                       (bedekking_vector[i] -
                          area.owin(discs(centres = ran,
                                          radii = qgamma(rqs,
                                                         shape = x,
                                                         scale = 1) ,
                                          mask = TRUE, eps = 1)) /
                          area.owin(Window(ran)))^2})
      ranp[[i]] <- discs(centres = ran,
                         radii = qgamma(rqs,
                                        shape = shape$par,
                                        scale = 1),
                         mask = mask, eps = 1)
    }
  } else {
    ranp <- NULL
  }
  
  if ("regular" %in% verspreiding) {
    # regular
    reg <- rSSI(r = rescale_factor/(2*sqrt(intens_per_m2)),
                n = intens_per_m2,
                win = owin(c(0,rescale_factor),c(0,rescale_factor)))
    #reg <- rescale(reg, 1/rescale_factor, unitname = c("metre", "metres"))
    # permutatie van quantielen
    rqs <- sample(x = seq(0.01, 0.99, length.out = reg$n),
                  size = reg$n,
                  replace = FALSE)
    regp <- vector(mode = "list", length = length(bedekking_vector))
    for (i in 1:length(bedekking_vector)) {
      shape <- optim(par = 1, method = "Brent", lower = 0, upper = 50,
                     fn = function(x) {
                       (bedekking_vector[i] -
                          area.owin(discs(centres = reg,
                                          radii = qgamma(rqs,
                                                         shape = x,
                                                         scale = 1) ,
                                          mask = TRUE, eps = 1)) /
                          area.owin(Window(reg)))^2})
      regp[[i]] <- discs(centres = reg,
                         radii = qgamma(rqs,
                                        shape = shape$par,
                                        scale = 1),
                         mask = mask, eps = 1)
    }
  } else {
    regp  <- NULL
  }
  
  
  return(list(geclusterd = clup,
              random = ranp,
              regular = regp))
}
