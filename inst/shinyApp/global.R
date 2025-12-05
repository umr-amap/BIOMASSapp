suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinycssloaders)
  library(shinyjs)
  library(shinyalert)
  library(shinyFeedback)
  library(shinyhelper)
  library(shinyWidgets)
  library(data.table)
  library(ggplot2)
  library(leaflet)
  library(rmarkdown)
  library(knitr)
  library(measurements)
  library(DT)
  remotes::install_github('umr-amap/BIOMASS')
  library(BIOMASS)
  library(terra)
  library(bslib)
})

source("legal_notice.R")

# Set custom theme for ggplot
custom_theme <- theme(
  legend.text = element_text(size=13),
  legend.title = element_text(size=15),
  plot.title = element_text(size=18))

# Set method's color for heights
color_height <- c(local_model = "#619CFF", Feldpausch = "#F8766D", Chave = "#00BA38", user_height = "black")

# Defining available functions for subplot_summary()
available_functions <- list("mean"=mean,"sum"=sum,"median"=median,"sd"=sd,"var"=var)
Ndens <- function(x, na.rm = TRUE) length(na.omit(x))


# set maximum input file size (here 30Mo)
options(shiny.maxRequestSize = 30 * 1024^2)

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

# suppress warnings of ggplots (essentially for "Removed x row containing missing values or values outside the scale range")
silentPlot <- function(p) {
  if(in_devmode()) {
    print(p)
  } else {
    suppressMessages(
      suppressWarnings(print(p))
    )
  }
}

# split the genus in multiple columns (genus species)
tstrsplit_NA <- function(x, pattern = " ", count = 2) {

  split <- utils::head(tstrsplit(x, pattern), count)

  if (length(split) < count) {
    split <- c(split, rep(NA_character_, count - length(split)))
  }
  split
}

# AGB_predict() returns the output of AGB_MonteCarlo(), with an additional element representing individual estimations (rather than the median of the simulations to ensure reproducibility of the estimations).
AGB_predict <- function(D, WD, errWD = NULL, H = NULL, HDmodel = NULL, errH = NULL, region = NULL, E_vec = NULL, coord = NULL, model_by = NULL) {

  # Setting parameters for stand-specific HD local model
  if (!is.null(HDmodel) && !is.null(model_by)) {
    valid_plots <- model_by %in% names(HDmodel) # names(HDmodel) doesn't contain removed plots (the ones containing less than 15 non-NA values for exemple)
    D <- D[valid_plots]
    WD <- WD[valid_plots]
    errWD <- if (!is.null(errWD)) errWD[valid_plots]
    model_by <- model_by[valid_plots]
  }

  if( !is.null(errH) ) { # heights (and errH) provided by the user
    AGB <- AGBmonteCarlo(
      D, Dpropag = "chave2004",
      WD, errWD,
      H = H, errH = errH
    )
    AGB[["AGB_pred"]] <-  as.matrix(computeAGB(D, WD, H = H))
  }

  if (!is.null(region)) { # feld region
    H <- retrieveH(D, region = region)
    errH <- H$RSE
    H <- H$H
    AGB <- AGBmonteCarlo(
      D, Dpropag = "chave2004",
      WD, errWD,
      H = H, errH = errH
    )
    AGB[["AGB_pred"]] <- as.matrix(computeAGB(D, WD, H = H))
  }

  if(!is.null(HDmodel)) { # HD local model
    AGB <- AGBmonteCarlo(
      D, Dpropag = "chave2004",
      WD, errWD,
      HDmodel = HDmodel
    )
    H <- retrieveH(D, model = HDmodel, plot = model_by)$H
    AGB[["AGB_pred"]] <- as.matrix(computeAGB(D, WD, H = H))
  }

  if(!is.null(coord)) { # Chave's AGB equation
    AGB <- AGBmonteCarlo(
      D, Dpropag = "chave2004",
      WD, errWD,
      coord = coord
    )
    # Modified Eq 7 from Chave et al. 2014 Global change biology
    # We apply the formula instead of calling computeAGB with a coord argument (which call computeE() which is time consuming)
    pred_AGB <- exp(-2.023977 - 0.89563505 * E_vec + 0.92023559 * log(WD) + 2.79495823 * log(D) - 0.04606298 * (log(D)^2)) / 1000
    AGB[["AGB_pred"]] <- as.matrix(pred_AGB)
  }

  return(AGB)
}

indiv_pred <- function(inv, rad_height, H, AGB_res, chkgrp_HEIGHT, sel_HDmodel_by, hd_data, hd_model, D, region, E, coord_plot){

  inv_h_pred <- inv

  if(!is.null(rad_height) && rad_height == "h_each_tree") {
    inv_h_pred$H_mes <- H
    inv_h_pred$H_Lorey_mes <- H * inv_h_pred$BA
    inv_h_pred$AGB <- round(as.vector(AGB_res[["user_height"]]$AGB_pred), 3)
    return(inv_h_pred)
  }
  if ("HDloc" %in% chkgrp_HEIGHT) {
    # if stand-specific models
    if( !is.null(sel_HDmodel_by) && sel_HDmodel_by != "<unselected>" ) {
      inv_h_pred$H_local_model <- round(retrieveH(hd_data$D, hd_model, plot = hd_data$model_for)$H, 2)

      # if some tree heights have been provided, we need to replace the estimated height by the measured heights
      if( rad_height == "h_some_tree") {
        inv_h_pred$H_local_model[!is.na(hd_data$H)] <- round(hd_data$H[!is.na(hd_data$H)], 2)
      }

    } else {
    # if stand-specific models
      inv_h_pred$H_local_model <- round(retrieveH(D, hd_model)$H, 2)
      if( rad_height == "h_some_tree") {
        # if some tree heights have been provided, we need to replace the estimated height by the measured heights
        inv_h_pred$H_local_model[!is.na(hd_data$H)] <- round(hd_data$H[!is.na(hd_data$H)], 2)
      }
    }
    inv_h_pred$H_Lorey_local_model <- inv_h_pred$H_local_model * inv_h_pred$BA
    inv_h_pred$AGB_local_model <- round(as.vector(AGB_res[["local_model"]]$AGB_pred), 3)
  }
  if ("feld" %in% chkgrp_HEIGHT) {
    inv_h_pred$H_Feldpausch <- round( retrieveH(D,
                                                region = region[ match( inv_h_pred[["plot"]] ,
                                                                        table = region$plot),
                                                                 "feld_region"]
    )$H, 2)
    inv_h_pred$H_Lorey_Feldpausch <- inv_h_pred$H_Feldpausch * inv_h_pred$BA
    inv_h_pred$AGB_Feldpausch <- round(as.vector(AGB_res[["Feldpausch"]]$AGB_pred), 3)
  }
  if ("chave" %in% chkgrp_HEIGHT) {
    df_E <- data.frame(plot = coord_plot$plot, E = E)
    logD <- log(D)
    logH <- 0.893 - df_E[match(inv$plot , table = df_E$plot) , "E"] + 0.760 * logD - 0.0340 * I(logD^2) # eq 6a Chave et al. 2014
    RSE <- 0.243
    inv_h_pred$H_Chave = round(as.numeric(exp(logH + 0.5 * RSE^2)),2)
    inv_h_pred$H_Lorey_Chave <- inv_h_pred$H_Chave * inv_h_pred$BA
    inv_h_pred$AGB_Chave <- round(as.vector(AGB_res[["Chave"]]$AGB_pred), 3)

  }
  return(inv_h_pred)
}


plot_list <- function(list, removedPlot = NULL) {

  ### Formatting the data-frame which will be used for ggplot
  df_res <- rbindlist(lapply(names(list), function(i) { # looping on methods
    x <- list[[i]][["summary"]]
    x$method <- i
    if(!is.null(removedPlot)) {
      # Adding a row containing NA's for plots which were not in local_model
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
    scale_color_manual(values = color_height) +
    xlab(NULL) + ylab("AGB (Mg)") +
    theme(axis.title = element_text(size = rel(1.2))) +
    geom_errorbar(aes(ymin = Cred_2.5, ymax = Cred_97.5), position = position_dodge(width = 0.1), width = 0.1) +
    geom_point(aes(y = AGB), position = position_dodge(width=0.1))

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

# Function that merge a list of subplot_summary outputs into a single standard output
merge_subplot_summary <- function(list_sub_sum) {

  # Create standard empty outputs
  out <- list()
  out$tree_summary <- list_sub_sum[[1]][["tree_summary"]][,"subplot_ID"]
  out$polygon <- list_sub_sum[[1]][["polygon"]][,c("plot_ID","subplot_ID","sf_subplot_polygon")]
  out$long_AGB_simu <- list_sub_sum[[1]][["long_AGB_simu"]][,c("plot_ID","subplot_ID","N_simu","x_center","y_center")]
  if( length(list_sub_sum[[1]][["plot_design"]]) == 1 ) {
    out$plot_design <- list()
  } else {
    out$plot_design <- lapply(list_sub_sum[[1]][["plot_design"]], function(x) NULL)
  }

  for (H_method in names(list_sub_sum)) { # H_method = "Feldpausch"

    res <- list_sub_sum[[H_method]]

    # rename AGBD variables to match the current method
    if(H_method != "user_height") {
      names(res$tree_summary) <- gsub("AGBD",paste0("AGBD_",H_method), names(res$tree_summary))
      names(res$polygon) <- gsub("AGBD",paste0("AGBD_",H_method), names(res$polygon))
      names(res$long_AGB_simu) <- gsub("AGBD",paste0("AGBD_",H_method), names(res$long_AGB_simu))
      if( length(res$plot_design) != 1 ) {
        res$plot_design <- lapply(res$plot_design, function(x) {
          new_title <- gsub("AGBD",paste0("AGBD_",H_method), x$labels$title)
          x <- x + labs(title = new_title)
        })
      } else {
        new_title <- gsub("AGBD",paste0("AGBD_",H_method), res$plot_design$labels$title)
        res$plot_design <- res$plot_design + labs(title = new_title)
      }
    }

    # merge current outputs to "out"
    out$tree_summary <- out$tree_summary[res$tree_summary, on ="subplot_ID"]
    out$polygon <- cbind(out$polygon, res$polygon[, grep("AGBD_", names(res$polygon))])
    out$polygon$sf_subplot_polygon.1 <- NULL
    out$long_AGB_simu <- out$long_AGB_simu[res$long_AGB_simu, on = c("plot_ID","subplot_ID","N_simu","x_center","y_center")]
    if (length(res$plot_design) != 1) {
      for(plot_name in names(out$plot_design)) {
        out$plot_design[[plot_name]] <- c(out$plot_design[[plot_name]], res$plot_design[[plot_name]])
      }
    } else {
      out$plot_design <- c(out$plot_design, res$plot_design)
    }
  }

  return(out)

}

# Function to retrieve latitude/longitude from projected coordinates
UTM_to_longlat <- function(xy_dt, df_UTM_code) { # xy_dt = xy_dat[]
  out <- proj4::project(xy = xy_dt[,c("x_center","y_center")],
                        proj = unique(df_UTM_code$UTM_code[df_UTM_code$plot_ID == unique(xy_dt$plot_ID)]),
                        inverse = TRUE)
  return(out)
}

# Function to creat FOS like results at subplot level
FOS_subplot_res <- function(checked_plot, divide_output, subplot_summary_output) {

  ### results will contain for each (sub)plot:
  # Plot_ID, subplot_ID, Ndens, Lat_cnt, Lon_cnt, MinDBH, MaxDBH, BA, Wood density(mean), H_Lorey_..., H_max_..., AGBD_..., AGBD_..._Cred_2.5, AGBD_..._Cred_97.5, metric_function_per_ha

  # Create Lat_cnt and Lon_cnt columns
  xy_dat <- subplot_summary_output$long_AGB_simu[N_simu==1,] # One simulation contains center coordinates of each subplot
  xy_dat[, c("Lon_cnt","Lat_cnt") := UTM_to_longlat(.SD, df_UTM_code = checked_plot$UTM_code),
         by = c("plot_ID"), .SDcols=colnames(xy_dat)]
  xy_dat <- xy_dat[,c("plot_ID","subplot_ID","Lon_cnt","Lat_cnt")]

  ### divide_output$tree_data contains plot_ID, subplot_ID, Lat, Long, Diameter, BA, Wood density, H_Lorey_... and H_...
  if( ! "Plot_ID" %in% names(divide_output$tree_data)) setnames(divide_output$tree_data, old = "plot_ID", new = "Plot_ID")
  if( ! "D" %in% names(divide_output$tree_data)) setnames(divide_output$tree_data, old = input$sel_DIAMETER, new = "D")

  # Summarize by plot: Ndens, MinDBH, MaxDBH, BA (sum of individual BA), Wood density (mean), H_Lorey_...(sum) and H_... (max)
  subplot_values <- c("D", "D", "D", "BA", "WD")

  H_Lorey_names <- grep("H_Lorey", names(divide_output$tree_data), value = TRUE)
  H_method_names <- grep("^H_(?!Lorey).*", names(divide_output$tree_data), value = TRUE, perl = TRUE)
  subplot_values <- c(subplot_values, H_Lorey_names, H_method_names)

  fun_list <- list(Ndens = Ndens, MinDBH = min, MaxDBH = max, BA = sum, WD = mean)
  fun_H_Lorey <- lapply(H_Lorey_names, function(x) return(sum))
  fun_H_method <- lapply(H_method_names, function(x) return(max))
  fun_list <- c(fun_list, fun_H_Lorey, fun_H_method)
  per_ha_vec <- c(Ndens = TRUE, MinDBH = FALSE, MaxDBH = FALSE, BA = TRUE, WD = FALSE)
  per_ha_vec <- c(per_ha_vec, rep(FALSE, length(H_Lorey_names)), rep(FALSE, length(H_method_names)))

  res_subplot <- subplot_summary(subplots = divide_output,
                                 value = subplot_values, per_ha = per_ha_vec,
                                 fun = fun_list, draw_plot = FALSE, na.rm=TRUE)

  res_subplot_tree_summary <- res_subplot$tree_summary
  # Dividing Lorey's column by BA to get the correct Lorey's heights
  for(x in grep("H_Lorey_", names(res_subplot_tree_summary), value = TRUE)) {
    res_subplot_tree_summary[[x]] <- round(res_subplot_tree_summary[[x]] / res_subplot_tree_summary$BA, 2)
  }

  # Setting standard names
  setnames(res_subplot_tree_summary,
           old = c("D_Ndens_per_ha", "D_min",  "D_max",  "BA_sum_per_ha"), # subplot_summary can't deal with fun = arg_fun = list(user_def_fun, ...). But this would be happen only in this app and in this situation.
           new = c("Ndens",          "MinDBH", "MaxDBH", "BA"))
  # Remove "_sum" in H_Lorey_..._sum"
  names(res_subplot_tree_summary)[grep("^H_Lorey.*sum$", names(res_subplot_tree_summary))] <- gsub(
    pattern = "_sum", replacement = "",
    x = names(res_subplot_tree_summary)[grep("^H_Lorey.*sum$", names(res_subplot_tree_summary))])

  ### Merging corner and res_subplot_tree_summary
  res <- xy_dat[res_subplot_tree_summary, on = "subplot_ID"]

  ### Merging res and subplot_summary_output$tree_summary (which contains subplot_ID, AGBD_... columns and the summarised metrics (including the raster one))
  res <- res[data.table(subplot_summary_output$tree_summary), on = "subplot_ID"]

  return(res)
}

