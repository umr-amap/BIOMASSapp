library(shiny)
library(shinydashboard)
library(data.table)
# library(leaflet)
library(shinyjs)
library(shinyalert)
library(shinyFeedback)
library(BIOMASS)

# set maximum input file size (here 30Mo)
options(shiny.maxRequestSize = 30 * 1024^2)

# ajoute un id a un box de maniere a pouvoir le montrer/cacher
boxWithId <- function(..., title = NULL, footer = NULL, status = NULL,
                      solidHeader = FALSE, background = NULL, width = 6, height = NULL,
                      collapsible = FALSE, collapsed = FALSE, id = NULL) {
  b <- match.call(expand.dots = TRUE)
  bid <- id
  b$id <- NULL
  b[[1]] <- as.name("box")
  b <- eval(b, parent.frame())
  b$attribs$id <- bid
  b
}

hideMenuItem <- function(tabName) {
  shinyjs::hide(selector = sprintf("a[data-value='%s']", tabName))
}

showMenuItem <- function(tabName) {
  shinyjs::show(selector = sprintf("a[data-value='%s']", tabName))
}

# to use for the heigth if there is a long but no lat (or inverse) error and
# if there is no argument to either H long and lat
ifheigth <- function(x, y, z) {
  if (y && z) {
    if (!x) {
      return(F)
    }
  }

  if (!y && !z) {
    return(F)
  }

  return(T)
}

tstrsplit_NA <- function(x, pattern = " ", count = 2) {
  # NOTE extraneous columns ignored maybe better paste them together
  split <- utils::head(tstrsplit(x, pattern), count)

  # pad with NA
  if (length(split) < count) {
    split <- c(split, rep(NA_character_, count - length(split)))
  }
  split
}



# for the AGB predict
AGB_predict <- function(AGBmod, D, WD, errWD, H = NULL, errH = NULL, HDmodel = NULL, coord = NULL, region = NULL) {
  # if there is the coordinate
  if (!is.null(coord)) {
    return(ifelse(AGBmod == "agb",
      computeAGB(D, WD, coord = coord),
      AGBmonteCarlo(D, WD, errWD, coord = coord, Dpropag = "chave2004")
    ))
  }

  # if the user want the AGB
  if (AGBmod == "agb") {
    if (!is.null(HDmodel)) {
      H <- retrieveH(D, model = HDmodel)$H
    }
    if (!is.null(region)){
      H = retrieveH(D, region = region)$H
    }
    AGB <- computeAGB(D, WD, H = H)
  }

  if (AGBmod == "agbe") {
    if (!is.null(region)){
      H = retrieveH(D, region = region)
      errH = H$RSE
      H = H$H
    }

    AGB = AGBmonteCarlo(D, WD, errWD,
                  H = ifelse(is.null(H), NULL, H),
                  errH = ifelse(is.null(errH), NULL, errH),
                  HDmodel = ifelse(is.null(HDmodel), NULL, HDmodel),
                  Dpropag = "chave2004"
                  )

  }

  return(AGB)
}
