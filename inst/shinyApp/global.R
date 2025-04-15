#GCO ATTENTION Globalement éviter d'utiliser T et F à la place de TRUE et FALSE
#GCO pour s'en rendre compte il suffit de faire T <- FALSE et le monde s'écroule :-)
#GCO TRUE <- FALSE provoquera une erreur

#GCO réduire le bruit dans la console
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinycssloaders)
  library(data.table)
  library(ggplot2)
  library(leaflet)
  library(shinyjs)
  library(shinyalert)
  library(shinyFeedback)
  library(rmarkdown)
  library(measurements)
  library(DT)
  library(BIOMASS)
})

# set maximum input file size (here 30Mo)
options(shiny.maxRequestSize = 30 * 1024^2)

#GCO fermeture de l'application uniquement en local (compatbilité avec le mode serveur)
# SERVER auto close application when session ends
# only if run locally (ie not on a remote server)
autoCloseApp <- function(session=getDefaultReactiveDomain()) {
  isLocal <- Sys.getenv("SHINY_PORT") == ""
  if(isLocal) {
    session$onSessionEnded(function() {
      stopApp()
    })
  }
}

#GCO fonction générique pour ajouter un id au tag
# UI add an id to a tag
setId <- function(tag, id) {
  htmltools::tagAppendAttributes(tag, id=id)
}

#GCO réécriture de boxWithId utilisant setId
#GCO mais on pourrait aussi utiliser directement setId(box(...), id="") ou box(...) |> setId("") dans le code
# add an id to a box so that it can be shown/hidden (instead of wrap the box() on a div() )
boxWithId <- function(..., id=NULL) {
  box(...) |> setId(id)
}

hideMenuItem <- function(tabName) {
  shinyjs::hide(selector = sprintf("a[data-value='%s']", tabName))
}

showMenuItem <- function(tabName) {
  shinyjs::show(selector = sprintf("a[data-value='%s']", tabName))
}


# split the genus in multiple columns (genus species)
tstrsplit_NA <- function(x, pattern = " ", count = 2) {

  split <- utils::head(tstrsplit(x, pattern), count)

  if (length(split) < count) {
    split <- c(split, rep(NA_character_, count - length(split)))
  }
  split
}


AGB_predict <- function(AGBmod, D, WD, errWD = NULL, H = NULL, HDmodel = NULL, errH = NULL, region = NULL, E_vec = NULL, coord = NULL, model_by = NULL) {

  # Setting parameters for stand-specific HD local model
  if (!is.null(HDmodel) && !is.null(model_by)) {
    valid_plots <- model_by %in% names(HDmodel) # names(HDmodel) doesn't contain removed plots (the ones containing less than 15 non-NA values for exemple)
    D <- D[valid_plots]
    WD <- WD[valid_plots]
    errWD <- if (!is.null(errWD)) errWD[valid_plots]
    model_by <- model_by[valid_plots]
  }

  # AGB without error propagation
  if (AGBmod == "agb") {

    if (!is.null(E_vec)) { # Chave method
      # Modified Eq 7 from Chave et al. 2014 Global change biology
      # We apply the formula instead of calling computeAGB with a coord argument (which call computeE() which is time consuming)
      AGB <- exp(-2.023977 - 0.89563505 * E_vec + 0.92023559 * log(WD) + 2.79495823 * log(D) - 0.04606298 * (log(D)^2)) / 1000
      return(as.matrix(AGB))
    }

    if (!is.null(HDmodel)) { # HD model
      H <- retrieveH(D, model = HDmodel, plot = model_by)$H
    }

    if (!is.null(region)) { # feld region
      H <- retrieveH(D, region = region)$H
    }

    AGB <- as.matrix(computeAGB(D, WD, H = H))
  }

  # AGB with error
  if (AGBmod == "agbe") {

    if( !is.null(errH) ) { # heights (and errH) provided by the user
      AGB <- AGBmonteCarlo(
        D, Dpropag = "chave2004",
        WD, errWD,
        H = H, errH = errH,
        plot = model_by
      )
    }

    if (!is.null(region)) { # feld region
      H <- retrieveH(D, region = region)
      errH <- H$RSE
      H <- H$H
      AGB <- AGBmonteCarlo(
        D, Dpropag = "chave2004",
        WD, errWD,
        H = H, errH = errH,
        plot = model_by
      )
    }

    if(!is.null(HDmodel)) { # HD local model
      AGB <- AGBmonteCarlo(
        D, Dpropag = "chave2004",
        WD, errWD,
        HDmodel = HDmodel,
        plot = model_by
      )
    }

    if(!is.null(coord)) { # Chave's AGB equation
      AGB <- AGBmonteCarlo(
        D, Dpropag = "chave2004",
        WD, errWD,
        coord = coord
      )
    }
  }

  return(AGB)
}


plot_list <- function(list, color, AGBmod, removedPlot = NULL) {

  ### Formatting the data-frame which will be used for ggplot
  df_res <- rbindlist(lapply(names(list), function(i) { # looping on methods
    x <- list[[i]]
    x$method <- i
    if(!is.null(removedPlot)) {
      # Adding a row containing NA's for plots which were not in local HD model
      removed_plot_res <- rbindlist(lapply(removedPlot[! removedPlot %in% x$plot], function(x) data.frame(plot=x, AGB=NA, Cred_2.5=NA, Cred_97.5=NA, method=i)))
      x <- rbindlist(list(x,removed_plot_res))
    }
    x
  }))

  # Defining the plot's order along x-axis: in increasing order of biomass and NA's (removed plots) in last position)
  df_res[, mean_AGB := mean(AGB), by = plot]
  df_res[, plot_order := order(mean_AGB), by = method]
  df_res[, plot := factor(plot, levels = unique(df_res$plot)[unique(df_res$plot_order)] )]

  render_plot <- ggplot(df_res, aes(x = plot, colour = method)) +
    xlab(NULL) + ylab("AGB (Mg)") +
    theme_minimal() +
    scale_color_manual(values = color) +
    xlab(NULL) + ylab("AGB (Mg)") +
    theme(axis.title = element_text(size = rel(1.2)))

  if(AGBmod == "agb") {
    render_plot <- render_plot + geom_point(aes(y = AGB), size = 2, position = position_dodge(width = 0.2) )
  } else {
    render_plot <- render_plot + geom_errorbar(aes(ymin = Cred_2.5, ymax = Cred_97.5), position = "dodge",  width = 0.1)
  }

  if ( length(unique(df_res$method)) != 1 ) { # if several method for estimating tree heights
    # Add a great legend
    render_plot <- render_plot +
      guides(color = guide_legend(title="method for\nestimating\ntree heights")) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(1.2))
      )
  } else { # if one method for estimating tree heights
    # do not display the legend and set the colour to black
    render_plot <- render_plot +
      theme(legend.position = "none") +
      scale_colour_manual(values = "black")
  }

  if( length(unique(df_res$plot)) != 1 ) { #  if several plots
    # display plot's names along x-axis in a 45° angle
    render_plot <- render_plot +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(1.2)))
  } else { # if single plot
    # do not display plot name
    render_plot <- render_plot +
      theme(axis.text.x = element_blank())
  }

  return(render_plot)
}
