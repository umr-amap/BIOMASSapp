library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)
library(maps)
library(shinyjs)
library(shinyalert)
library(shinyFeedback)
library(rmarkdown)
library(BIOMASS)
library(measurements)

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
    shinyalert("oops", "You did attribute the WD vector,\n the propagation error for the wood dentity will be 0",
      type = "warning"
    )
    errWD <- rep(0, length(WD))
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






plot_list <- function(list, color) {
  is_vector <- nrow(list[[1]]) == 1

  # take the order of the first result
  if (!is_vector) {
    plot_order <- order(list[[1]]$AGB)
  }

  # sekeleton of the plot
  plot(if (!is_vector) plot_order else 1:length(list),
    main = "", type = "n",
    ylim = range(sapply(list, function(x) {
      range(x[, -1], na.rm = T)
    })),
    xlab = "", ylab = "AGB",
    xaxt = "n"
  )
  if (!is_vector) {
    axis(1, at = 1:length(plot_order), labels = list[[1]]$plot[plot_order], las = 2)
  } else {
    axis(1, at = 1:length(list), labels = names(list))
  }

  if (is_vector) {
    points(1:length(list), sapply(list, function(x) x[, "AGB"]), pch = 20)
    if (ncol(list[[1]]) > 2) {
      segments(1:length(list), sapply(list, function(x) x[, "Cred_2.5"]), y1 = sapply(list, function(x) x[, "Cred_97.5"]))
    }
  } else {
    # if it's the AGB without the error propagtion
    if (ncol(list[[1]]) == 2) {

      # trace the points in the graph
      lapply(names(list), function(x) {
        points(1:length(plot_order), list[[x]][plot_order, "AGB"], col = color[x], pch = 20)
      })
    } else {
      # transparent color expect the first which is HDlocal
      color <- rgb(t(col2rgb(color[1:3])) / 255,
        alpha = c(1, 0.5, 0.5),
        names = names(color[1:3])
      )

      # trace the polygon expect for the first value
      lapply(names(list)[-1], function(x) {
        with(list[[x]], {
          polygon(
            x = c(seq_along(plot_order), length(plot_order):1),
            y = c(Cred_2.5[plot_order], rev(Cred_97.5[plot_order])),
            col = color[x], border = NA
          )
        })
      })

      # trace the points + segments for the first HD model
      with(list[[1]], {
        points(seq_along(plot_order), AGB[plot_order], pch = 20, cex = 1.5, col = color[names(list)[1]])
        segments(seq_along(plot_order), Cred_2.5[plot_order], y1 = Cred_97.5[plot_order], col = color[names(list)[1]])
      })
    }


    # draw the legend
    legend("bottomright", legend = names(list), col = color[names(list)], pch = 20)
  }
}
