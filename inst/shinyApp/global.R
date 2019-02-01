library(shiny)
library(shinydashboard)
library(data.table)
library(leaflet)
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






# split the genus in multiple columns
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
AGB_predict <- function(AGBmod, D, WD, errWD = NULL, H = NULL, HDmodel = NULL, coord = NULL, region = NULL) {

  #### parameters verification

  if (is.null(errWD) && AGBmod != "agb") {
    shinyalert("oops", "You did attribute the WD vector,\n you can not do the propagation error",
      type = "warning"
    )
    AGBmod <- "agb"
  }

  if (!is.null(H) && AGBmod != "agb") {
    shinyalert("oops", paste0(
      "You did attribute the H vector,\n",
      "you can not do the propagation error,\n",
      "whithout an HD model"
    ),
    type = "warning"
    )
    AGBmod <- "agb"
  }

  ##### calcul of the AGB


  # if there is the coordinate
  if (!is.null(coord)) {
    AGB <- if (AGBmod == "agb") {
      computeAGB(D, WD, coord = coord)
    } else {
      AGBmonteCarlo(D, WD, errWD, coord = coord, Dpropag = "chave2004")
    }
    return(AGB)
  }


  # if the user want the AGB without error
  if (AGBmod == "agb") {
    if (!is.null(HDmodel)) { # HD model
      H <- retrieveH(D, model = HDmodel)$H
    }
    if (!is.null(region)) { # feld region
      H <- retrieveH(D, region = region)$H
    }
    AGB <- computeAGB(D, WD, H = H)
  }

  if (AGBmod == "agbe") {
    if (!is.null(region)) { # feld region
      H <- retrieveH(D, region = region)
      errH <- H$RSE
      H <- H$H
    }

    AGB <- AGBmonteCarlo(D, WD, errWD,
      H = if (!is.null(H)) H,
      errH = if (!is.null(H)) errH,
      HDmodel = if (!is.null(HDmodel)) HDmodel,
      Dpropag = "chave2004"
    )
  }

  return(AGB)
}
