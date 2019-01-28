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
