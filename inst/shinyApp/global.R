library(shiny)
library(shinydashboard)
library(data.table)
#library(leaflet)
library(shinyjs)
library(shinyalert)
library(shinyFeedback)

# set maximum input file size (here 30Mo)
options(shiny.maxRequestSize=30*1024^2)

# ajoute un id a un box de maniere a pouvoir le montrer/cacher
boxWithId <- function(..., title = NULL, footer = NULL, status = NULL,
  solidHeader = FALSE, background = NULL, width = 6, height = NULL,
  collapsible = FALSE, collapsed = FALSE,id = NULL) {
  b <- match.call(expand.dots = TRUE)
  bid <- id
  b$id <- NULL
  b[[1]] <- as.name("box")
  b <- eval(b, parent.frame())
  b$attribs$id <- bid
  b
}

hideMenuItem <- function(tabName) {
  shinyjs::hide(selector=sprintf("a[data-value='%s']",tabName))
}

showMenuItem <- function(tabName) {
  shinyjs::show(selector=sprintf("a[data-value='%s']",tabName))
}

