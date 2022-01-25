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



# de functie focal laat toe om via een moving window w berekeningen te doen op
# raster x
# hier berekenen we het gemiddelde (via weighted sum) van de 1/0 data, 
# dus dit is de proportie van een MZB in het window 
# = de gemiddelde werkelijke biomassa (zonder meetfout)
# (veronderstel biomassa in bodem overal homogeen waar MZB voorkomt)
# het resultaat is een raster met de berekende waarde in elke focal cel
# (centrum van window)
# daarna berekenen we de standaarddeviatie op deze biomassa (dit is de
# intrinsieke ruimtelijke populatievariantie)
# voor de locale variantie bereken we het gemiddelde van de 
# gekwadrateerde afwijkingen tov de locale gemiddelden
twostage <- function(
  spatial_distribution, #spatstat owin object
  m = 1L, #aantal subsamples (meetplaatsen binnen locatie) (aantal pixels)
  nrow = 3L, #locatie is nrow x ncol pixels
  ncol = nrow) {
  
  spdistr <- raster::raster(as.matrix(spatial_distribution))
  
  local_pop_mean <- raster::focal(
    x = spdistr, 
    w = matrix(data = 1 / (nrow * ncol), nrow = nrow, ncol = ncol),
    fun = sum)
  
  # systematic subsamples inside a plot
  pos1 <- floor(nrow * ncol / m / 2) + 1
  if (m > 1) {
    positions <- c(pos1, pos1 + cumsum(rep(pos1, m - 1)))
  } else {
    positions <- pos1
  } 
  
  samplevec <- rep(0, nrow * ncol)
  samplevec[positions] <- 1
  stopifnot(all.equal(sum(samplevec), m))
  
  local_sample_mean <- raster::focal(
    x = spdistr, 
    w = matrix(data = samplevec,
               nrow = nrow, ncol = ncol) / m,
    fun = sum)
  
  local_var <- raster::focal(
    x = (local_sample_mean - local_pop_mean) ^ 2, 
    w = matrix(data = 1 / (nrow * ncol), nrow = nrow, ncol = ncol),
    fun = sum)
  
  # eindige (locale) populatie correctiefactor toepassen
  local_var <- local_var * (1 - m / (nrow * ncol))
  
  popvar <- var(local_pop_mean@data@values, na.rm = TRUE)
  locvar <- mean(local_var@data@values, na.rm = TRUE)
  
  result <- data.frame(nrow = nrow,
                       m = m,
                       tussenvar = popvar,
                       binnenvar = locvar)
  return(result)
}
