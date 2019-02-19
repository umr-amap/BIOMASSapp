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
AGB_predict <- function(AGBmod, D, WD, errWD = NULL, H = NULL, HDmodel = NULL, coord = NULL, region = NULL, plot = NULL) {

  ##### calcul of the AGB

  # if there is the coordinate
  if (!is.null(coord)) {
    if (nrow(coord) == 1) {
      coord <- c(coord[1, 1], coord[1, 2])
    }
    AGB <- if (AGBmod == "agb") {
      computeAGB(D, WD, coord = coord)
    } else {
      AGBmonteCarlo(D, WD, errWD, coord = coord, Dpropag = "chave2004")
    }
    return(AGB)
  }

  if (!is.null(HDmodel) && !is.null(plot)) {
    sorting <- plot %in% names(HDmodel)

    D <- D[sorting]
    WD <- WD[sorting]
    errWD <- if (!is.null(errWD)) errWD[sorting]
    plot <- plot[sorting]
  }

  # if the user want the AGB without error
  if (AGBmod == "agb") {
    if (!is.null(HDmodel)) { # HD model
      H <- retrieveH(D, model = HDmodel, plot = plot)$H
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
      H = H,
      errH = errH,
      HDmodel = HDmodel,
      plot = plot,
      Dpropag = "chave2004"
    )
  }

  return(AGB)
}






plot_list <- function(list, color, plot = NULL) {
  nr <- nrow(list[[1]])

  is_vector <- nr == 1

  if (!is.null(plot)) {
    list <- lapply(list, function(x) {
      x[x$plot %in% plot, ]
    })
    nr <- nrow(list[[1]])
  }


  # take the order of the first result
  if (!is_vector) {
    plot_order <- with(list[[1]], match(AGB, sort(AGB)))
    list <- lapply(list, function(x) {
      x$plot_order <- plot_order
      x
    })
  } else {
    list <- rbindlist(lapply(names(list), function(i) {
      x <- list[[i]][1, -1]
      names(x) <- names(list[[i]])[-1]
      x <- as.list(x)
      x$plot <- i
      x
    }))

    list[, plot_order := seq(.N)]
    nr <- list[, .N]
    list <- list(comp = list)
  }

  plot <- ggplot(cbind(name = names(list[1]), list[[1]]), aes(x = plot_order)) + xlab(NULL) + ylab("AGB (Mg)")

  if (ncol(list[[1]]) > 3) {
    if (is_vector || "HD_local" %in% names(list)) {
      plot <- plot + geom_pointrange(aes(y = AGB, ymin = Cred_2.5, ymax = Cred_97.5, colour = name, na.rm = T))
    }
    if (!is_vector) {
      for (i in names(list)[names(list) != "HD_local"]) {
        plot <- plot + geom_ribbon(
          data = cbind(name = i, list[[i]]),
          aes(ymin = Cred_2.5, ymax = Cred_97.5, fill = name),
          alpha = 0.3, na.rm = T
        )
      }
    }
  } else {
    for (i in names(list)) {
      plot <- plot + geom_point(
        data = cbind(name = i, list[[i]]),
        aes(y = AGB, colour = name),
        na.rm = T
      )
    }
  }

  # legend + size of text + color
  if (!is_vector) {
    plot <- plot +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(1.2)),
        legend.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.2))
      ) +
      with(list[[1]], scale_x_continuous(breaks = 1:nr, labels = plot[order(AGB)])) +
      scale_fill_manual(values = color) + scale_color_manual(values = color)
  } else {
    plot <- plot + theme(legend.position = "none", axis.text.x = element_text(size = rel(1.5))) +
      scale_colour_manual(values = "black") +
      scale_x_continuous(breaks = 1:nr, labels = list[[1]]$plot)
  }


  return(plot)
}
