function(input, output, session) {

  # Initialisation ----

  # stop the serveur at the end of the session
  autoCloseApp() # version compatible local/server
  observe_helpers(help_dir = "helpfiles")
  legalNoticeHandler(includeMarkdown("helpfiles/legal_notice.md"), size = "l")

  # Legal notice
  legal_content <- shiny::includeMarkdown("helpfiles/legal_notice.md")
  legalNoticeBslib(2025, "UMR AMAP")

  observeEvent(input$selected_tab, {
    # Hide all nav items
    shinyjs::hide("tab_LOAD")
    shinyjs::hide("tab_TAXO")
    shinyjs::hide("tab_HEIGHT")
    shinyjs::hide("tab_AGB")
    shinyjs::hide("tab_SPATIALISATION")
    shinyjs::hide("tab_SP_SUMMARY")

    # remove the active class of all links
    shinyjs::removeClass("nav_link_load", "active")
    shinyjs::removeClass("nav_link_taxo", "active")
    shinyjs::removeClass("nav_link_height", "active")
    shinyjs::removeClass("nav_link_agb", "active")
    shinyjs::removeClass("nav_link_spatial", "active")
    shinyjs::removeClass("nav_link_summary", "active")

    # Display the selected item and active the link
    shinyjs::show(input$selected_tab)
    link_id <- switch(input$selected_tab,
                      "tab_LOAD" = "nav_link_load",
                      "tab_TAXO" = "nav_link_taxo",
                      "tab_HEIGHT" = "nav_link_height",
                      "tab_AGB" = "nav_link_agb",
                      "tab_SPATIALISATION" = "nav_link_spatial",
                      "tab_SP_SUMMARY" = "nav_link_summary"
    )
    shinyjs::addClass(link_id, "active")
  })



  # Preparing forest_inv to receive input$file_DATASET thanks to <<-
  forest_inv <- NULL

  ## Reactive values:  ----
  rv <- reactiveValues(
    inv = NULL, # forest inventory data-frame (created when clicking on 'Continue' in 'Load dataset' item)
    df_h_sup = NULL, # supplementary height data-frame (e.g NouraguesHD)
    df_coord = NULL, # supplementary plot coordinates data-frame (e.g NouraguesCoord)
    wd = NULL, # output of getWoodDensity()
    taxo = NULL, # output of correctTaxo()
    hd_data = NULL, # data-table containing height and diameter data to build local HD model(s)
    hd_model = NULL, # output of modelHD() for the method which has the lowest RMSE
    coord = NULL, # data.table containing plotID and the coordinates of each tree used in AGB item
    coord_plot = NULL, # data.table containing the unique coordinates of the plot(s)
    region = NULL, # a data-frame containing 2 columns : plot and Feldpausch's region
    feld_already_ticked = FALSE, # when selecting a method in Height item, used to not re-execute the code when another box is ticked (HDlocal or Chave)
    chave_already_ticked = FALSE, # same as above but with the Chave method
    E = NULL, # a vector of E parameter for each plot
    AGB_res = list(), # output of AGB_predict() (in global.R)
    inv_pred = NULL, # data-frame containing tree-level results (for downloads and spatialisation)
    removedPlot = NULL, #removedPlot will contain plots that don't have enough height measurements when building a model stand/region specific or those with diameter not well distributed
    plot_hd = NULL, # reactive value to access the plot for the report
    checked_plot = NULL, # the output of check_plot_coord()
    file_rast = NULL, # the raster uploaded
    gg_check_plot = NULL, # the plot visulisation of check_plot_coord()
    divide_output = NULL, # the output of divide_plot() applied when clicking on "continue" in the spatialisation tab
    grid_dat = NULL, # the grid to display after divide_plot()
    gg_subplot_summary = NULL, # the plot list of suplot_summary() output
    subplot_summary_output = NULL,
    FOS_subplot = NULL # the FOS like results for subplots downloads
  )


  # LOAD DATASET ---------------------------------------------------------------

  ## Forest inventory file actions ----
  observeEvent(input$file_DATASET, ignoreInit = TRUE, {
    # Read forest inventory upload
    forest_inv <<- fread(
      file = req(input$file_DATASET)$datapath,
      data.table = FALSE,
    )

    # show forest inventory content
    output$table_DATASET <- renderDT(forest_inv, options = list(scrollX = TRUE))

    # update the values of the select inputs with the column names
    int_num_col <- names(forest_inv)[ sapply(forest_inv, class) %in% c("integer", "numeric")]
    print("observe file_dataset")
    for (id in c("sel_DIAMETER", "sel_WD")) {
      updateSelectInput(session, id, choices = c("<unselected>", int_num_col))
    }
    char_col <- names(forest_inv)[ sapply(forest_inv, class) %in% "character" ]
    for (id in c("sel_GENUS", "sel_SPECIES")) {
      updateSelectInput(session, id, choices = c("<unselected>", char_col))
    }
    updateSelectInput(session, "sel_PLOT", choices = c("<unselected>", names(forest_inv)))

    # show other hidden boxes
    showElement("box_DATASET")
    if(!is.null(input$rad_several_plots)){
      showElement("box_FIELDS")
      showElement("box_COORD")
    }
  })

  # Dowload button for the forest inventory example (NouraguesTrees with ~ 120 simulated heights)
  output$dwl_inv_ex <- downloadHandler(
    filename = "forest_inv_exemple.csv",
    content = function(file) {
      file.copy("exemple_data/forest_inv_exemple.csv", file)
    },
    contentType = "text/csv"
  )

  # Reaction to "Does your dataset contain several plots?"
  observeEvent(input$rad_several_plots, {
    # show other boxes if the forest inv file has been uploaded
    if(!is.null(forest_inv)){
      showElement("box_FIELDS")
      showElement("box_DATASET")
      showElement("box_COORD")
    }
    # Show plot's IDs selection
    toggleElement("id_sel_PLOT", condition = req(input$rad_several_plots) == "several_plots")
  })

  ## Warnings ----

  # If plot IDs is unselected => red box
  observeEvent(list(input$rad_several_plots,input$sel_PLOT), {
    feedbackWarning("sel_PLOT",
                    show = input$sel_PLOT == "<unselected>",
                    text = "Compulsory argument", icon = NULL)
  })

  # If the diameter is unselected => red box
  observeEvent(input$sel_DIAMETER, {
    feedbackWarning("sel_DIAMETER",
                    show = input$sel_DIAMETER == "<unselected>",
                    text = "Compulsory argument", icon=NULL)
  })
  # if the wd or genus not selected (but not both)
  observe({
    toggleElement("msg_wd",
                  condition =
                    !xor(input$sel_WD == "<unselected>", input$sel_GENUS == "<unselected>") )
  })


  ## WD ----
  # Show the setting of errWD when the user provides the wood density
  observeEvent(input$sel_WD, {
    toggleElement("id_set_errWD",
                  condition = input$sel_WD != "<unselected>")
  })

  ## Height ----
  ### height radio button actions ----
  observeEvent(input$rad_height, {
    int_num_col <- names(forest_inv)[ sapply(forest_inv, class) %in% c("integer", "numeric")]
    updateSelectInput(session, "sel_H", choices = c("<unselected>", int_num_col))
    updateSelectInput(session, "sel_HDmodel_by", choices = c("<unselected>", names(forest_inv)))
    toggleElement("id_sel_h",
                  condition = input$rad_height %in% c("h_each_tree","h_some_tree"))
    toggleElement("id_set_errH",
                  condition = input$rad_height == "h_each_tree")
    toggleElement("id_sel_HDmodel_by", # stand-specific local HD models only if height of some tree in the same dataset and several plots
                  condition = (input$rad_height == "h_some_tree" & req(input$rad_several_plots) == "several_plots") )
    toggleElement("id_file_h_sup",
                  condition = input$rad_height == "h_sup_data")
  })
  ### height file actions ----
  observeEvent(input$file_h_sup, {
    # Read HD supplementary dataset
    rv$df_h_sup <- fread(
      file = req(input$file_h_sup)$datapath,
      data.table = FALSE)

    # show coordinates table content
    output$table_h_sup <- renderDT(rv$df_h_sup,
                                   options = list(scrollX = TRUE))
    toggleElement("box_h_sup_preview", condition = input$rad_height == "h_sup_data")

    # Update Diameter & Height seletions
    int_num_col <- names(rv$df_h_sup)[ sapply(rv$df_h_sup, class) %in% c("integer", "numeric") ]
    updateSelectInput(session, "sel_D_sup_data", choices = c("<unselected>", int_num_col))
    updateSelectInput(session, "sel_H_sup_data", choices = c("<unselected>", int_num_col))
  })

  ## Coordinates ----

  ###  Dowload the coordinates csv as an example (NouraguesCoord)
  output$dwl_coord_ex <- downloadHandler(
    filename = "plot_coordinates_exemple.csv",
    content = function(file) {
      write.csv(BIOMASS::NouraguesCoords, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  ### coordinates radio button actions ----
  observeEvent(list(input$rad_coord,input$rad_several_plots), { # observe on rad_several_plots to toggle Plot ID's when sup data and several plots
    req(input$rad_coord)
    # Coordinates of each trees:
    if(input$rad_coord == "coord_each_tree") {
      # Show latitude and longitude select inputs for forest inventory table
      int_num_col <- names(forest_inv)[ sapply(forest_inv, class) %in% c("integer", "numeric")]
      updateSelectInput(session, "sel_LAT", choices = c("<unselected>", int_num_col))
      updateSelectInput(session, "sel_LONG", choices = c("<unselected>", int_num_col))
    }
    toggleElement("id_sel_coord", condition = input$rad_coord == "coord_each_tree") # show latitude & longitude selection from forest inventory data
    # Coordinates in sup data
    toggleElement("id_file_coord", condition = input$rad_coord == "coord_plot") # show file input
    toggleElement("id_sel_coord_plot", condition = input$rad_coord == "coord_plot") # show latitude & longitude selection from coordinates data
    toggleElement("sel_plot_coord", # show plot ID selection is several plots
                  condition = input$rad_coord == "coord_plot" & input$rad_several_plots == "several_plots")
    toggleElement("id_sel_plot_display", # show which plot to display in spatialisation tab
                  condition = input$rad_coord == "coord_plot" & input$rad_several_plots == "several_plots")
    toggleElement("sel_plot_display_summary", # show which plot to display in summarised metrics tab
                  condition = input$rad_coord == "coord_plot" & input$rad_several_plots == "several_plots")
    # Coordinates given manually
    toggleElement("id_num_lat_long", condition = input$rad_coord == "coord_manually") # show latitude & longitude numeric input for manually given coordinates

  })

  ### coordinates file actions  ----
  observeEvent(input$file_coord, {
    # Read plot coordinates upload
    rv$df_coord <- fread(
      file = req(input$file_coord)$datapath,
      data.table = FALSE)

    # show coordinates table content
    output$table_coord <- renderDT(rv$df_coord,
                                   options = list(scrollX = TRUE))
    toggleElement("box_coord_preview", condition = input$rad_coord == "coord_plot")

    # Update latitude & longitude & plot's IDs selections
    int_num_col <- names(rv$df_coord)[ sapply(rv$df_coord, class) %in% c("integer", "numeric") ]
    updateSelectInput(session, "sel_LAT_sup_coord", choices = c("<unselected>", int_num_col))
    updateSelectInput(session, "sel_LONG_sup_coord", choices = c("<unselected>", int_num_col))
    if(input$rad_several_plots == "several_plots") {
      updateSelectInput(session, "sel_plot_coord", choices = c("<unselected>", names(rv$df_coord)))
    }
    # Update x_rel & y_rel for spatialisation tab
    updateSelectInput(session, "sel_x_rel_corner", choices = c("<unselected>", int_num_col))
    updateSelectInput(session, "sel_y_rel_corner", choices = c("<unselected>", int_num_col))
  })
  # Update which plot to display in spatialisation tab
  observeEvent(input$sel_plot_coord, {
    if(input$sel_plot_coord != "<unselected>") {
      updateSelectInput(session, "sel_plot_display", choices = unique(rv$df_coord[,input$sel_plot_coord]))
      updateSelectInput(session, "sel_plot_display_summary", choices = unique(rv$df_coord[,input$sel_plot_coord]))
    }
  })

  ## Reaction to 'Continue' button ----
  observeEvent(input$btn_DATASET_LOADED, ignoreInit = TRUE, {
    print("Reaction to btn_DATASET_LOADED")

    ### Error management ----
    error_occured <- FALSE
    if ( is.null(input$file_DATASET) ) {
      # if no dataset is provided
      error_occured <- TRUE
      shinyalert("Oops!", "You need to load a forest inventory file !", type = "error")
      return()
    } else if ( is.null(input$rad_several_plots) ) {
      # if the number of plot is not provide
      error_occured <- TRUE
      shinyalert("Oops!", "You need to answer to the question : Does your dataset contain several plots?", type = "error")
      return()
    } else if (input$rad_several_plots =="several_plots" && input$sel_PLOT == "<unselected>" ) {
      # if the column corresponding to the plot IDs is unselected
      error_occured <- TRUE
      shinyalert("Oops!", "The column containing the plots IDs is unselected", type = "error")
      return()
    } else if (input$rad_several_plots == "several_plots" && input$sel_PLOT != "<unselected>" && length(unique(forest_inv[,input$sel_PLOT])) == 1 ) {
      # if only one plot is detected
      error_occured <- TRUE
      shinyalert("Oops!", "You specified that your dataset contains several plots but only one is detected.", type = "error")
      return()
    } else if (input$sel_DIAMETER == "<unselected>") { # if diameter is not selected
      error_occured <- TRUE
      shinyalert("Oops!", "D is unselected", type = "error")
      return()
    } else if (!xor(input$sel_WD == "<unselected>", input$sel_GENUS == "<unselected>")) {
      # if the wd is not selected or genus not selected but not the two
      error_occured <- TRUE
      shinyalert("Oops!", "To estimate the Above Ground Biomass, you either need the wood density or the taxonomy of the trees", type = "error")
    } else if (is.null(input$rad_height)) {
      # if the H radio button has not been ticked
      error_occured <- TRUE
      shinyalert("Oops!", "Height information has not been provided", type = "error")
      return()
    } else if ( input$rad_height %in% c("h_each_tree","h_some_tree") && input$sel_H == "<unselected>" ) {
      # if the H column is unselected
      error_occured <- TRUE
      shinyalert("Oops!", "Height column is unselected", type = "error")
      return()
    } else if ( input$rad_height == "h_sup_data" && is.null(input$file_h_sup)){
      # if the H-D relationship is in another dataset which have not been loaded
      error_occured <- TRUE
      shinyalert("Oops!", "The dataset containing a subset of well-measured trees has not been loaded", type = "error")
    } else if ( input$rad_height == "h_sup_data" && (input$sel_H_sup_data == "<unselected>" | input$sel_D_sup_data == "<unselected>") ){
      # if the H or D column (of the sup dataset containing H-D relationship) are unselected
      error_occured <- TRUE
      shinyalert("Oops!", "Diameter and/or Height column(s) of the dataset containing a subset of well-measured trees is/are unselected ", type = "error")
      return()
    } else if (input$rad_height == "h_none" && (is.null(input$rad_coord) || input$rad_coord %in% c("","coord_none"))  ) {
      # if no height measurements and no coordinates
      error_occured <- TRUE
      shinyalert("Oops!", "To estimate tree heights, you either need a subset of well-measured trees or the coordinates of the plots", type = "error")
      return()
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_each_tree" & (input$sel_LONG == "<unselected>" | input$sel_LAT == "<unselected>")) {
      # if the coordinates of each tree is ticked but one of the two (long or lat) is not selected
      error_occured <- TRUE
      shinyalert("Oops!", "Tree's longitude and/or latitude is/are unselected", type = "error")
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_plot" && is.null(input$file_coord) ) {
      # if the coordinates of the plots (in another dataset) is ticked but the dataset has not been loaded
      error_occured <- TRUE
      shinyalert("Oops!", "The dataset containing the coordinates of the plot(s) has not been loaded", type = "error")
      return()
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_plot" && (input$sel_LAT_sup_coord == "<unselected>" | input$sel_LONG_sup_coord == "<unselected>") ) {
      # if the Latitude or Longitude column (of the sup dataset containing plot's coordinates) are unselected
      error_occured <- TRUE
      shinyalert("Oops!", "Latitude and/or Longitude column(s) of the dataset containing the coordinates of the plot(s) is/are unselected ", type = "error")
      return()
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_plot" && input$rad_several_plots == "several_plots" & input$sel_plot_coord == "<unselected>") {
      # if the plots IDs column for sup coord is unselected
      error_occured <- TRUE
      shinyalert("Oops!", "Plots IDs of the dataset containing the coordinates of the plot(s) is unselected ", type = "error")
      return()
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_manually" && (is.na(input$num_lat) | is.na(input$num_long))) {
      error_occured <- TRUE
      shinyalert("Oops!", "The manually specified latitude/longitude are not recognised as a numerical value. Make sure that there is no space character at the beginning/end of your input.", type = "error")
      return()
    } else if (input$sel_WD == "<unselected>") {
      # if the WD is not selected then show the tab TAXO
      shinyjs::show("nav_item_taxo")
      shinyjs::hide("tab_LOAD")
      shinyjs::show("tab_TAXO")
      shinyjs::removeClass("nav_link_load", "active")
      shinyjs::addClass("nav_link_taxo", "active")

    } else { # If no error and WD already exists
      if (input$rad_height == "h_each_tree") {
        # if heights are not to estimate, show AGB tab
        shinyjs::show("nav_item_agb")
        shinyjs::hide("tab_LOAD")
        shinyjs::show("tab_AGB")
        shinyjs::removeClass("nav_link_load", "active")
        shinyjs::addClass("nav_link_agb", "active")
      } else {
        # else show the height tab
        shinyjs::show("nav_item_height")
        shinyjs::hide("tab_LOAD")
        shinyjs::show("tab_HEIGHT")
        shinyjs::removeClass("nav_link_load", "active")
        shinyjs::addClass("nav_link_height", "active")
      }
    }

    ## Reset the "Choose the HD model" button and result boxes (used if the user go back to the Load dataset after Retrieving tree heights) ----
    updateCheckboxGroupInput(session, inputId = "chkgrp_HEIGHT",
                             choices = c("Local HD model" = "HDloc","Feldpausch" = "feld","Chave" = "chave"),
                             selected = NULL, inline = T)
    hideElement("box_RESULT_HDMOD")
    hideElement("box_RESULT_FELD")
    hideElement("box_result_chave")

    ## Setting rv$inv ----
    if(!error_occured) {

      rv$inv <- forest_inv

      ### Setting the plot column that contains plotID
      if(input$rad_several_plots == "several_plots" && input$sel_PLOT != "<unselected>" ) {
        rv$inv$plot <- as.character(rv$inv[,input$sel_PLOT])
      } else if (input$rad_several_plots == "single_plot") {
        rv$inv$plot <- ""
      }

      ### Unit conversions
      # Diameter
      rv$inv[,input$sel_DIAMETER] <- conv_unit(rv$inv[,input$sel_DIAMETER], input$rad_units_diameter, "cm")
      if(sum(is.na(rv$inv[,input$sel_DIAMETER]))!=0) {
        shinyalert("Warning:", "Some NA's are detected in your diameter values. The calculation of the AGB will not take into account trees linked to these values.", type = "warning")
        rv$inv <- rv$inv[!is.na(rv$inv[,input$sel_DIAMETER]),]
      }
      # WD (note that errWD unit will be changed when clicking on the 'Go on' button for AGB calculation)
      if (input$sel_WD != "<unselected>") {
        if(input$rad_units_wd == "kg.m-3") {
          rv$inv[,input$sel_WD] <- rv$inv[,input$sel_WD] / 1000
        }
      }
      # Height
      if (input$sel_H != "<unselected>") {
        rv$inv[,input$sel_H] <- conv_unit(rv$inv[,input$sel_H], input$rad_units_height, "m")
      }
      # diameter and height for sup data
      if (!is.null(input$file_h_sup)) {
        rv$df_h_sup[,input$sel_D_sup_data] <- conv_unit(rv$df_h_sup[,input$sel_D_sup_data], input$rad_units_D_sup, "cm")
        rv$df_h_sup[,input$sel_H_sup_data] <- conv_unit(rv$df_h_sup[,input$sel_H_sup_data], input$rad_units_H_sup, "m")
      }

      ## Formatting coordinates for Feldpausch and Chave methods -------------------
      req(input$rad_coord)

      # If coordinates of each trees:
      if( input$rad_coord == "coord_each_tree") {
        rv$coord <- data.table(rv$inv[, c(input$sel_LONG, input$sel_LAT,"plot")]) # coord is simply the inventory dataset
        setnames(rv$coord, c(input$sel_LONG, input$sel_LAT), c("long","lat"))
        rv$coord_plot <- rv$coord # coord_plot will be averaging by plot above
      }

      # If plot's coordinates in another dataset:
      if( input$rad_coord == "coord_plot") {
        rv$coord_plot <- data.table(rv$df_coord[, c(input$sel_LONG_sup_coord, input$sel_LAT_sup_coord)]) # coord_plot is simply the coordinates sup dataset
        setnames(rv$coord_plot, names(rv$coord_plot), c("long","lat"))

        if(input$rad_several_plots == "several_plots") {
          rv$coord_plot[, plot := as.character(rv$df_coord[,input$sel_plot_coord])]
        } else {
          rv$coord_plot[, plot := ""]
        }
      }

      # If coordinates manually specified:
      if( input$rad_coord == "coord_manually") {
        rv$coord <- data.table(rv$inv)[, c("long","lat") := list(input$num_long, input$num_lat)]
        rv$coord_plot <- data.table(long = input$num_long, lat = input$num_lat, plot = unique(rv$inv$plot))
      }

      if(input$rad_coord %in% c("coord_each_tree","coord_plot")) { # when rad_coord = coord_each_tree or coord_plot
        # get the median coordinates of each/the plot
        rv$coord_plot <- rv$coord_plot[, .(long = median(long, na.rm = TRUE), lat = median(lat, na.rm = TRUE)), by = plot]
        # remove all NA and take the unique coordinates
        rv$coord_plot <- unique(na.omit(rv$coord_plot))
      }

      if(input$rad_coord == "coord_plot") {
        rv$coord <- merge(rv$inv , rv$coord_plot)[c("long","lat","plot")]
      }

    } # end of if(!error_occured)
  }) # end of "reaction to 'Continue' button


  # TAXONOMY -------------------------------------------------------------------

  observeEvent(input$btn_TAXO_RESULT, {
    showElement("box_RESULT_TAXO")
    # show a progress bar
    withProgress(message = "Correcting the taxonomy", value = 0, {
      # if the users have selected the correct taxo + get wd
      if (input$rad_WD == "corr") {
        # correct the taxo and catch the error if there is error
        rv$taxo <- tryCatch({
          suppressMessages(correctTaxo(
            genus = rv$inv[, input$sel_GENUS],
            species = if (input$sel_SPECIES != "<unselected>") rv$inv[, input$sel_SPECIES],
            useCache = getOption("BIOMASSapp.taxoCache",FALSE)
          ))
        }, error = function(e) e)

        # if there is an error display it
        if (!is.data.frame(rv$taxo)) {
          output$out_taxo_error <- renderPrint({
            rv$taxo$message
          })
        } else {
          output$out_taxo_error <- renderPrint({
            cat("Summary of taxonomy corrections (in number of trees):\n")
            taxo_display <- factor(rv$taxo$nameModified,
                                   levels = c("FALSE","SpNotFound","TaxaNotFound","TRUE"),
                                   labels = c("Correct spelling of taxa (unmodified)","Species not found (unmodified)","Taxa not found (unmodified)","Taxa found and corrected (modified)"))
            table(taxo_display, dnn = "")
          })
        }
        # update the progression
        incProgress(1 / 2, detail = "Taxonomy correction completed", message = "Extracting wood density values")

        genus <- rv$taxo$genusCorrected
        species <- rv$taxo$speciesCorrected

        # add genusCorrected and speciesCorrected to inv for the download of the tree level results
        rv$inv$genusCorrected <- rv$taxo$genusCorrected
        rv$inv$speciesCorrected <- rv$taxo$speciesCorrected

      } else {
        # if the users do not choose the correct taxo
        genus <- rv$inv[, input$sel_GENUS]
        if (input$sel_SPECIES == "<unselected>") {
          split <- tstrsplit_NA(genus)
          genus <- split[[1]]
          species <- split[[2]]
        } else {
          species <- rv$inv[, input$sel_SPECIES]
        }
        hideSpinner(id = "out_taxo_error")
      }
      rv$wd <- tryCatch(suppressMessages(getWoodDensity(genus, species, stand = if (input$sel_PLOT != "<unselected>") rv$inv[, input$sel_PLOT])),
                        error = function(e) e,
                        warning = function(e) e
      )

      # if there is an error display it
      if (!is.data.frame(rv$wd)) {
        output$out_wd_error <- renderPrint({
          rv$taxo$message
        })
        rv$wd <- NULL
      } else { # if not display the results

        # add WD and sdWD columns to inv for the download of the tree level results
        rv$inv$WD <- rv$wd$meanWD
        rv$inv$sd_WD = rv$wd$sdWD

        output$out_wd_error <- renderPrint({
          cat("Taxonomic levels at which wood density was attributed to trees (in %):\n")
          levelswd <- 100 * table(rv$wd$levelWD) / nrow(rv$wd)
          if (input$sel_PLOT != "<unselected>") {
            data.frame(
              "Species level" = round(levelswd["species"], 1),
              "Genus level" = round(levelswd["genus"], 1),
              "Plot level" = round(sum(levelswd[!names(levelswd) %in% c("genus", "species")]), 1),
              #"User dataset level" = round(levelswd["dataset"], 1),
              check.names = FALSE
            )
          } else {
            data.frame(
              "Species level" = round(levelswd["species"], 1),
              "Genus level" = round(levelswd["genus"], 1),
              "Plot level" = round(levelswd["dataset"], 1),
              #"User dataset level" = round(levelswd["dataset"], 1),
              check.names = FALSE
            )
          }
        })
      }
      incProgress(1, detail = "Wood density extraction completed")
    })

    showElement(id = "btn_TAXO_DONE")
  })

  # when the taxo is done
  observeEvent(input$btn_TAXO_DONE, {
    if (!is.data.frame(rv$wd)) {
      shinyalert("Oops", "Somethings went wrong, please check this", type = "error")
    } else {
      if (input$rad_height == "h_each_tree") { # if heights are not to estimate
        shinyjs::show("nav_item_agb")
        shinyjs::hide("tab_TAXO")
        shinyjs::show("tab_AGB")
        shinyjs::removeClass("nav_link_taxo", "active")
        shinyjs::addClass("nav_link_agb", "active")
      } else {
        # else show the height tab
        shinyjs::show("nav_item_height")
        shinyjs::hide("tab_TAXO")
        shinyjs::show("tab_HEIGHT")
        shinyjs::removeClass("nav_link_taxo", "active")
        shinyjs::addClass("nav_link_height", "active")
      }
    }
  })


  # HEIGHT ---------------------------------------------------------------------

  # Reset the "Choose the HD model" button (used if the user go back to the Load dataset after Retrieving tree heights)
  observeEvent(
    list(input$btn_TAXO_DONE,
         input$sel_H, input$sel_H_sup_data, input$sel_D_sup_data,
         input$sel_DIAMETER, input$sel_PLOT, input$sel_HDmodel_by, input$btn_DATASET_LOADED), ignoreInit = TRUE , priority = 10, {

           print("Reset chkgrp_HEIGHT radio buttons")
           updateCheckboxGroupInput(session, inputId = "chkgrp_HEIGHT",
                                    choices = c("Local HD model" = "HDloc","Feldpausch" = "feld","Chave" = "chave"),
                                    selected = NULL, inline = T)

         })


  ## HD local models -----------------------------------------------------------

  observeEvent( input$chkgrp_HEIGHT, ignoreNULL = TRUE, ignoreInit = TRUE, priority = 10, {
    # priority = 10 to format hd_data before any other observeEvent on chkgrp_HEIGHT
    if ("HDloc" %in% input$chkgrp_HEIGHT) {

      if (input$rad_height == "h_none") { # if no height at all, not possible
        shinyalert("Oops", "Local HD model cannot be build if no height measurements have been provided.", type = "error")
        updateCheckboxGroupInput(session, inputId = "chkgrp_HEIGHT",
                                 selected = input$chkgrp_HEIGHT[input$chkgrp_HEIGHT != "HDloc"] )
        return()
      }

      print("Building HD local models")

      ### Formatting hd_data which will be used to build the models ------------

      if (input$rad_height == "h_some_tree") { # if height of some trees in the same dataset

        rv$hd_data <- data.table(rv$inv[, c(input$sel_DIAMETER, input$sel_H)])
        # Setting D and H column names of rv$hd_data
        setnames(rv$hd_data, names(rv$hd_data)[1:2], c("D", "H"))

        if(input$sel_HDmodel_by != "<unselected>"){ # if one model by plot or region or whatever
          rv$hd_data[, model_for := as.character(rv$inv[[input$sel_HDmodel_by]])]
          new_hd_data <- rv$hd_data
          # remove all the plots with less than 15 non NA value
          rv$removedPlot <- unique(new_hd_data[, .(nbNonNA = sum(!is.na(H))), by = model_for][nbNonNA < 15, model_for])
          new_hd_data <- new_hd_data[!model_for %in% rv$removedPlot]
          # remove plots for which D is not well distributed
          new_hd_data <- new_hd_data[, quantile := findInterval(D, c(-1, quantile(D, probs = c(0.5, 0.75)), max(D) + 1)), by = model_for]
          rv$removedPlot <- c(rv$removedPlot, unique(new_hd_data[, .N, by = .(model_for, quantile)][N < 3, model_for]))
          rv$hd_data <- new_hd_data[!model_for %in% rv$removedPlot]

          # if there is a least one plot in the removed plot -> warning message
          if (length(rv$removedPlot) != 0) {
            shinyalert("Be carefull !", paste(
              "Local HD model cannot be built for:",
              paste(rv$removedPlot, collapse = ", "),
              "\n either:",
              "\n\t - there are not enough local height measurements",
              "\n\t - height measurements are likely not representative of tree size distribution",
              "\n You may have to build a single model grouping all the plots together by unselecting corresponding column."
            ), type = "warning")
          }
        } else { # if one model for the whole dataset
          rv$hd_data$model_for <- "all"
        }
      }

      if (input$rad_height == "h_sup_data") { # if height in another dataset
        rv$hd_data <- data.table(rv$df_h_sup[, c(input$sel_D_sup_data, input$sel_H_sup_data)])
        rv$hd_data$model_for <- "all"
        # Setting D and H column names of rv$hd_data
        setnames(rv$hd_data, old = c(input$sel_D_sup_data, input$sel_H_sup_data), new = c("D", "H"))
      }


      ## Building and compare the 4 local HD models ----------------------------
      tab_modelHD <- tryCatch({
        suppressMessages(modelHD(
          D = rv$hd_data$D,
          H = rv$hd_data$H,
          plot = rv$hd_data$model_for
        ))
      }, error = function(e) NULL)

      if(is.null(tab_modelHD)) { # if models cannot be built
        shinyalert("Oops !", "Local H-D models cannot be built. You should check your data, in particular the units of your diameter and height values (if provided)", type = "error")
        # Reset the checkbox
        updateCheckboxGroupInput(session, inputId = "chkgrp_HEIGHT",
                                 choices = c("Local HD model" = "HDloc","Feldpausch" = "feld","Chave" = "chave"),
                                 selected = NULL, inline = T)
        return()
      } else {
        # render the table

        if (input$sel_HDmodel_by == "<unselected>"){
          render_tab_modelHD <- DT::formatRound(table = DT::datatable(tab_modelHD[, -3],
                                                                      options(dom = 't')),
                                                columns=c("RSE","Average_bias"),
                                                digits = 3)
          output$out_tab_HD <- renderDT(render_tab_modelHD)
        } else { # If one model per plot/region/whatever, compute the mean of RMSE and Average_bias over all models
          tab_modelHD <- do.call(rbind,tab_modelHD)
          tab_modelHD <- data.table(tab_modelHD[,-3])
          tab_modelHD <- tab_modelHD[, lapply(.SD, function(x) round(mean(x), 3)) , by = method]
          output$out_tab_HD <- renderDT(DT::datatable(tab_modelHD,
                                                      options(dom = 't'))
          )
        }

        # update the radio button with the method and choose the minimum of the RSE
        # (we first need to reset rad_HDMOD in case user changed settings in 'Load dataset' but the min RSE method remains the same)
        print("Updating radio button for HD model")
        updateRadioButtons(session, inputId = "rad_HDMOD", choices = tab_modelHD$method, selected =character(0), inline = TRUE)
        updateRadioButtons(session, inputId = "rad_HDMOD", choices = tab_modelHD$method,
                           selected = tab_modelHD$method[which.min(tab_modelHD$RSE)], inline = TRUE)

        ### Show the box containing the result of hd_model
        showElement("box_RESULT_HDMOD")
      }
    } else { # if ("HDloc" not in input$chkgrp_HEIGHT)
      hideElement("box_RESULT_HDMOD")
    }
  })

  ### Building lowest RMSE local HD model or the one chosen by the user --------
  observeEvent(input$rad_HDMOD, ignoreNULL = TRUE, ignoreInit = TRUE, priority = 9, {
    # priority = 9 to build hd_model before any other observeEvent on chkgrp_HEIGHT but after formatting hd_data (priority = 10)

    print("Building HD lowest RSE local model")

    rv$hd_model <- tryCatch({
      modelHD(
        D = rv$hd_data$D,
        H = rv$hd_data$H,
        method = input$rad_HDMOD,
        plot = rv$hd_data$model_for,
        useWeight = TRUE
      )
    }, error = function(e) NULL, warning = function(e) NULL, message = function(e) NULL)
  })


  ## Feldpausch method -------

  observeEvent(input$chkgrp_HEIGHT,
               ignoreNULL = FALSE, ignoreInit = TRUE, priority = 10, {

                 if ("feld" %in% input$chkgrp_HEIGHT) {
                   print("Observing chkrgrp_HEIGHT for Feldpausch method")

                   # Skip if Feldpausch/Chave button were already ticked
                   if( rv$feld_already_ticked ) { # if Feldpausch was already ticked
                     return() # don't do anything
                   } else {

                     print("Feldpausch method:")

                     rv$feld_already_ticked <- TRUE

                     # If there is no coordinates: error
                     if(is.null(input$rad_coord) || input$rad_coord == "coord_none") {
                       shinyalert("Oops", "You need to provide tree's or plot's coordinates to access to region-specific model proposed by Feldpausch et al. (2012)", type = "error")
                       updateCheckboxGroupInput(session, inputId = "chkgrp_HEIGHT",
                                                choices = c("Local HD model" = "HDloc","Feldpausch" = "feld","Chave" = "chave"),
                                                selected = input$chkgrp_HEIGHT[input$chkgrp_HEIGHT != "feld"],
                                                inline = T)

                       return()

                     } else {

                       ### Compute Feldspausch regions for rv$coord_plot
                       rv$region <- data.frame(
                         plot = as.character(isolate(rv$coord_plot$plot)),
                         feld_region = computeFeldRegion(isolate(rv$coord_plot[, c("long", "lat")]))
                       )
                       # Advert user if some coordinates don't fall in Feldpausch regions
                       if( "Pantropical" %in% rv$region$feld_region) {
                         shinyalert("Mhhh",
                                    text = paste(
                                      ifelse(nrow(rv$region)==1,
                                             yes = "The plot doesn't",
                                             no = paste(c("Plots",isolate(rv$coord_plot$plot[rv$region$feld_region!="Pantropical"]),"don't"), collapse=" ")),
                                      "fall within the regions defined in Feldpausch et al. (2012), the whole pantropical region will be used."),
                                    type = "warning")
                       }

                       # print the list of regions in data-frame format
                       render_region <- rv$region
                       names(render_region)[2] <- "Feldpausch region"
                       output$out_tab_feld <- renderDT(DT::datatable(render_region,
                                                                     options(dom = 't'))
                       )

                       showElement("box_RESULT_FELD")
                     }
                   }

                 } else { # if "feld" not in input$chkgrp_HEIGHT
                   rv$feld_already_ticked <- FALSE
                   hideElement("box_RESULT_FELD")
                 }
               })


  ## Chave method -------
  observeEvent(input$chkgrp_HEIGHT, ignoreNULL = FALSE, ignoreInit = TRUE, priority = 10, {

    if ("chave" %in% input$chkgrp_HEIGHT) {

      print("Observing chkrgrp_HEIGHT for Chave method")

      if( rv$chave_already_ticked ) { # if Chave was already ticked
        return() # don't do anything
      } else {

        print("Chave's method:")

        rv$chave_already_ticked <- TRUE

        # If there is no coordinates: error
        if(is.null(input$rad_coord) || input$rad_coord == "coord_none") {
          shinyalert("Oops", "You need to provide tree's or plot's coordinates to access to generic H-D model proposed by Chave et al. (2014)", type = "error")
          updateCheckboxGroupInput(session, inputId = "chkgrp_HEIGHT",
                                   choices = c("Local HD model" = "HDloc","Feldpausch" = "feld","Chave" = "chave"),
                                   selected = input$chkgrp_HEIGHT[input$chkgrp_HEIGHT != "chave"],
                                   inline = T)
          return()

        } else {

          output$plot_MAP <- renderLeaflet({
            rv$coord_plot |>
              leaflet() |>
              addProviderTiles("OpenStreetMap.Mapnik") |>
              addCircleMarkers(
                lng = ~long, lat = ~lat,
                color = "#459433") |>
              fitBounds(lng1 = -90, lng2 = 90, lat1 = -60, lat2 = 75)

          })

          showElement("box_MAP")

          # Compute bioclimatic parameter E for each plot
          rv$E <- tryCatch(computeE(isolate(rv$coord_plot[, c("long", "lat")])), error = function(e) e)

          # print the list of E parameters in data-frame format
          render_E <- data.frame(plot = as.character(isolate(rv$coord_plot$plot)),
                                 E =  round(rv$E,4))
          names(render_E)[2] <- "Bioclimatic predictor E"
          output$out_tab_chave <- renderDT(DT::datatable(render_E,
                                                         options(dom = 't'))
          )

          print("End of Chave's method !")

          showElement("box_result_chave")
        }
      }
    } else { # if "Chave" not in input$chkgrp_HEIGHT
      rv$chave_already_ticked <- FALSE
      hideElement("box_result_chave")
      hideElement("box_MAP")
    }
  })


  ## Plotting height predictions --------------
  D <- NULL # D will contain increasing values from 1 to D_max, used to plot line predictions for all methods

  ### Basic plot for HD-methods comparison ----
  observeEvent( list(input$chkgrp_HEIGHT, input$rad_HDMOD),
                ignoreNULL = TRUE, ignoreInit = TRUE, priority = 10, {

                  print("Plotting basic plot")
                  toggleElement("box_plot_comparison", condition = !is.null(input$chkgrp_HEIGHT))

                  D_max <- max(rv$inv$D, ifelse(test = is.null(rv$df_h_sup), yes = 0, no = max(rv$df_h_sup[,input$sel_D_sup_data], na.rm=TRUE)), na.rm = TRUE)
                  D <<- 1:D_max

                  rv$plot_hd <- ggplot(data = NULL, aes(x = D, col="measured trees")) + # col="measured_trees" to silence the warning message "No shared levels found between `names(values)` of the manual scale and the data's colour values."
                    xlab("Diameter (cm)") +
                    ylab("Height (m)") +
                    scale_colour_manual(name = "model predictions", values = c("local_model" = "#619CFF", "Feldpausch" = "#00BA38", "Chave" = "#F8766D", "measured trees" = "black")) +
                    theme_minimal() +
                    theme(
                      legend.position = "bottom",
                      legend.title = element_blank(),
                      legend.text = element_text(size = rel(1.4)),
                      axis.title = element_text(size = rel(1.5)),
                      axis.text = element_text(size = rel(1.3)),
                      legend.key.width = unit(3, "line") ) +
                    guides(color = guide_legend(override.aes = list(lwd = 1)),
                           shape = guide_legend(override.aes = list(size = 3)))
                })

  observeEvent(list(input$rad_HDMOD, input$chkgrp_HEIGHT), ignoreNULL = TRUE, ignoreInit = TRUE, priority = 8, {

    ### Including HD local model in comparison ----
    if ("HDloc" %in% input$chkgrp_HEIGHT) {
      print("Plotting HD local model")

      if ( !is.null(rv$hd_model) ) {
        if (input$sel_HDmodel_by == "<unselected>"){
          rv$plot_hd <- rv$plot_hd +
            geom_point(data = rv$hd_data, mapping = aes(x = D, y = H, col="measured trees"), size=1.5, na.rm=TRUE) + # measured trees
            geom_line(aes(y = retrieveH(D, model = rv$hd_model)$H, col = "local_model"), lwd=1.2) + # HD model
            ylim( c(0, max(rv$hd_data$H, na.rm = TRUE) + 5))
        } else {
          # creating model lines for each plots/regions/...
          df_lines <- do.call(rbind, lapply(names(rv$hd_model), function(x) {
            data.frame( D = D,
                        H_pred = retrieveH(D, model = rv$hd_model[[x]])$H,
                        model_for = x)
          }))
          rv$plot_hd <- rv$plot_hd +
            geom_point(data = rv$hd_data, mapping = aes(x = D, y = H, col = "measured trees", shape = model_for), size=1.5, na.rm=TRUE) + # measured trees
            geom_line(data = df_lines, mapping = aes(x = D, y = H_pred, col = "local_model", lty = model_for), lwd=1.2) + #HD model for each plots/regions..
            ylim( c(0, max(rv$hd_data$H, na.rm = TRUE) + 5))
        }
      }
    }

    ### Including Feldpausch's model in comparison ----
    if ("feld" %in% input$chkgrp_HEIGHT) {
      print("Plotting Feldpausch's model")
      if(!is.null(rv$region)) {
        # creating model prediction lines for each plots/regions/...
        df_lines <- do.call(rbind, lapply(unique(rv$region$feld_region), function(x) {
          data.frame( D = D,
                      H_pred = retrieveH(D, region = x)$H,
                      model_for = x)
        }))
        if(length(unique(df_lines$model_for)) == 1 ) { # if one single region
          rv$plot_hd <- rv$plot_hd + geom_line(data = df_lines, mapping = aes(x = D, y = H_pred, colour = "Feldpausch"), lwd=1.2, na.rm=TRUE)
        } else {
          rv$plot_hd <- rv$plot_hd + geom_line(data = df_lines, mapping = aes(x = D, y = H_pred, colour = "Feldpausch", lty=model_for), lwd=1.2, na.rm=TRUE)
        }
      }
    }

    ### Including Chave's model in comparison ----
    if ("chave" %in% input$chkgrp_HEIGHT) {
      print("Plotting Chave's model")
      # creating model prediction lines for each plots/regions/...
      if (!is.null(rv$E) && !is.list(rv$E)) { # E is a list if an error was caught
        df_lines <- do.call(rbind, lapply(1:length(rv$E), function(x) {
          logD <- log(D)
          logH <- 0.893 - rv$E[x] + 0.760 * logD - 0.0340 * I(logD^2) # eq 6a Chave et al. 2014
          RSE <- 0.243
          data.frame(
            D = D,
            H_pred = as.numeric(exp(logH + 0.5 * RSE^2)),
            model_for = as.character(rv$coord_plot[x,"plot"])
          )
        }))
        rv$plot_hd <- rv$plot_hd +
          geom_line(data = df_lines, mapping = aes(x=D, y=H_pred, colour = "Chave", lty = model_for), lwd=1.2, na.rm = TRUE)
      }
    }

  })
  # Render the plot
  output$out_plot_comp <- renderPlot(silentPlot(rv$plot_hd))

  ## Done button actions -------------------------------------------------------
  observe({
    toggle("btn_HD_DONE", condition = !is.null(input$chkgrp_HEIGHT))
  })
  observeEvent(input$btn_HD_DONE, {
    if (is.null(input$chkgrp_HEIGHT)) {
      shinyalert("Oops", "Select at least one HD model", type = "error")
    } else {
      shinyjs::show("nav_item_agb")
      shinyjs::hide("tab_HEIGHT")
      shinyjs::show("tab_AGB")
      shinyjs::removeClass("nav_link_height", "active")
      shinyjs::addClass("nav_link_agb", "active")

      hideElement("box_AGB_res") # if user has already calculate AGBs
      hideElement("id_AGB_Report")
    }
  })



  # AGB ----

  observeEvent(input$btn_AGB_DONE, {

    print("Reaction to 'Go on' button for AGB calculation")

    ## Reset the individual tree results (created when downloading results)
    if(!is.null(rv$inv_pred)) rv$inv_pred <- NULL

    ### Retrieving and checking parameters ----

    # Retrieve diameters
    D <- rv$inv[, input$sel_DIAMETER]

    # Retrieve WD and its uncertainties
    if (is.data.frame(rv$wd)) {
      WD <- rv$wd[, "meanWD"]
      errWD <- rv$wd[, "sdWD"]
    } else {
      WD <- rv$inv[, input$sel_WD]
      errWD <- rep(input$set_errWD, length(WD))
      if(input$rad_units_wd == "kg.m-3") errWD <- errWD / 1000
    }

    # Retrieve heights (and its uncertainties if heights provided by the user)
    if (input$sel_H != "<unselected>") {
      H <- rv$inv[, input$sel_H]
    }
    if(input$rad_height %in% c("h_each_tree","h_some_tree")) {
      errH = conv_unit(rv$inv[, input$sel_H] * input$set_errH / 100, from = input$rad_units_height, to = "m")
    }

    # Add BA (basal area) to inv for the download of the tree level results
    rv$inv$BA <- (pi * (D / 2)^2) / 10000

    # Get the number of height estimation's methods
    length_progression <- length(input$chkgrp_HEIGHT)*2

    # Calculation of AGB ----

    withProgress(message = "AGB calculation", value = 0, {
      #newValue <- list()

      ## Heights provided by the user ----
      if(input$rad_height == "h_each_tree") {
        print("AGB calculation for user's heights: ")
        if( sum(is.na(H)) != 0 ) {
          shinyalert("There is some NA values in given heights. For those trees the function will return NA AGB,
          you may construct a height-diameter model to overcome that issue.",
                     type = "warning")
        }
        rv$AGB_res[[names(color_height)[4]]] <- AGB_predict(D = D, WD = WD, errWD = errWD, H = H, errH = errH)
        rv$AGB_res[[names(color_height)[4]]][["summary"]] <- summaryByPlot(rv$AGB_res[[names(color_height)[4]]], plot = rv$inv$plot)
      }


      ## Heights from HD local model ----
      if ("HDloc" %in% input$chkgrp_HEIGHT) {

        print("AGB calculation for HD local model: ")
        incProgress(1 / length_progression, detail = "AGB using HD local: Calculating...")

        if( input$sel_HDmodel_by != "<unselected>" ) { # if stand-specific models
          rv$AGB_res[[names(color_height)[1]]] <- AGB_predict(D = rv$hd_data$D,
                                                              WD = WD[rv$inv$plot %in% rv$hd_data$model_for],
                                                              errWD = errWD[rv$inv$plot %in% rv$hd_data$model_for],
                                                              HDmodel = rv$hd_model, model_by =  rv$hd_data$model_for)

          # if incomplete heights have been provided, for these heights, we need to replace the AGB estimates calculated with HDmodel by the AGB estimates calculated directly with H and errH
          if( input$rad_height == "h_some_tree") {
            AGB_user_H <- suppressWarnings(AGB_predict(D = rv$hd_data$D, WD = WD[rv$inv$plot %in% rv$hd_data$model_for], errWD = errWD[rv$inv$plot %in% rv$hd_data$model_for], H = rv$hd_data$H, errH = errH))
            rv$AGB_res[[names(color_height)[1]]]$AGB_pred[!is.na(AGB_user_H$AGB_pred)] <- AGB_user_H$AGB_pred[!is.na(AGB_user_H$AGB_pred)]
          }

          rv$AGB_res[[names(color_height)[1]]][["summary"]] <- summaryByPlot(rv$AGB_res[[names(color_height)[1]]], plot = rv$hd_data$model_for)

        } else { # if not stand-specific
          rv$AGB_res[[names(color_height)[1]]] <- AGB_predict(D, WD, errWD, HDmodel = rv$hd_model)

          # for incomplete heights provided
          if( input$rad_height == "h_some_tree") {
            AGB_user_H <- suppressWarnings(AGB_predict(D = rv$hd_data$D, WD = WD, errWD = errWD, H = rv$hd_data$H, errH = errH))
            rv$AGB_res[[names(color_height)[1]]]$AGB_pred[!is.na(AGB_user_H$AGB_pred)] <- AGB_user_H$AGB_pred[!is.na(AGB_user_H$AGB_pred)]
          }

          rv$AGB_res[[names(color_height)[1]]][["summary"]] <- summaryByPlot(rv$AGB_res[[names(color_height)[1]]], plot = rv$inv$plot)
        }
        incProgress(1 / length_progression, detail = "AGB using HD local: Done")
      }


      ## Heights from Feldpausch model ----
      if ("feld" %in% input$chkgrp_HEIGHT) {

        print("AGB calculation for Feldpausch's method: ")
        incProgress(1 / length_progression, detail = "AGB using Feldpausch region: Calculating...")

        rv$AGB_res[[names(color_height)[2]]] <- AGB_predict(D, WD, errWD, region = rv$region[match(rv$inv$plot , table = rv$region$plot) , "feld_region"])
        rv$AGB_res[[names(color_height)[2]]][["summary"]] <- summaryByPlot(AGB_val = rv$AGB_res[[names(color_height)[2]]], plot = rv$inv$plot)

        incProgress(1 / length_progression, detail = "AGB using Feldpausch region: Done")
      }

      # Heights from Chave model ----
      if ("chave" %in% input$chkgrp_HEIGHT) {

        print("AGB calculation for Chave's method: ")
        incProgress(1 / length_progression, detail = "AGB using Chave E: Calculating...")

        df_E <- data.frame(plot = rv$coord_plot$plot, E = rv$E)

        rv$AGB_res[[names(color_height)[3]]] <- AGB_predict(D=D, WD=WD, errWD=errWD,
                                                            coord = isolate(rv$coord[,c("long","lat")]),
                                                            E_vec = df_E[match(rv$inv$plot , table = df_E$plot) , "E"])

        rv$AGB_res[[names(color_height)[3]]][["summary"]] <- summaryByPlot(AGB_val = rv$AGB_res[[names(color_height)[3]]], plot = rv$inv$plot)

        incProgress(1 / length_progression, detail = "AGB using Chave E: Done")
      }

    })

    ## Calculation of individual tree metrics
    rv$inv_pred <- indiv_pred(inv = rv$inv, D = D,
                              rad_height = input$rad_height, chkgrp_HEIGHT = input$chkgrp_HEIGHT, H = H,
                              sel_HDmodel_by = input$sel_HDmodel_by, hd_data = rv$hd_data, hd_model = rv$hd_model,
                              region = rv$region, E=rv$E, coord_plot = rv$coord_plot,
                              AGB_res = rv$AGB_res)

    showElement(id = "box_AGB_res")
    showElement(id = "id_AGB_Report")
    showElement("id_btn_continue_sp")
  })
  ## plot the output ----
  output$out_plot_AGB <- renderPlot({
    plot_list(rv$AGB_res, removedPlot = rv$removedPlot)
  })

  # button "Continue to spatialisation"s
  observeEvent( input$btn_continue_sp, ignoreInit = TRUE, {
    if(is.null(rv$AGB_res)) {
      shinyalert("Oops,","You need first to calculate the AGB before continuing to Spatialization.")
      return(NULL)
    }

    if(input$rad_coord == "coord_plot") {
      shinyjs::show("nav_item_spatial")
      shinyjs::hide("tab_AGB")
      shinyjs::show("tab_SPATIALISATION")
      shinyjs::removeClass("nav_link_agb", "active")
      shinyjs::addClass("nav_link_spatial", "active")
    } else {
      shinyalert("To continue with the spatialisation of the AGB, you need to provide the coordinates of the plot corners in the 'Geographic coordinates' box on the 'Load dataset' tab.", type = "error")
    }
  })


  ## Download part -----------------------------------------------------------

  ### Report ----
  output$dwl_report <- downloadHandler(
    filename = function() {
      paste0("BIOMASS_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in case we don't have write permissions to the current working dir (which can happen when deployed).
      tempReport <- file.path(tempdir(), "report_BIOMASS.Rmd")
      file.copy(
        #from = system.file("Rmarkdown", "report_BIOMASS.Rmd", package = "BIOMASSapp"),
        from = "~/BIOMASSapp/inst/Rmarkdown/report_BIOMASS.Rmd",
        tempReport,
        overwrite = TRUE
      )

      if (file.exists(file)) {
        file.remove(file)
      }

      rmarkdown::render(tempReport, output_file = file)
    },
    contentType = "text/html"
  )


  ### Tree level results ----

  output$dwl_tree_file <- downloadHandler(
    filename = function() {
      paste0("tree_level_results_", Sys.Date(), ".csv")
    },
    content = function(file) {

      out <- rv$inv_pred

      # Round sd_WD (if exists) and BA columns
      if(!is.null(out$sd_WD)) out$sd_WD <- round(out$sd_WD, 4)
      out$BA <- round(out$BA, 4)

      # Remove the "plot" column to leave the original name
      n_plot_col <- match("plot", names(out))[length(match("plot", names(out)))] # column number of the plot columnn (use of [length(...)] in case user has already a "plot" column)
      # Remove the Lorey"s height columns (used in plot level results but not in tree level ones)
      n_lorey_col <- grep("H_Lorey", names(out)) # column number of the plot columnn (use of [length(...)] in case user has already a "plot" column)

      write.csv(as.data.frame(out[,-c(n_plot_col,n_lorey_col)]), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )


  ### Plot level results (FOS like csv) ----
  output$dwl_plot_file <- downloadHandler(
    filename = function() {
      paste0("plot_level_results_", Sys.Date(), ".csv")
    },
    content = function(file) {

      # Display a message to reassure users if the download is not immediate
      showModal(modalDialog("Loading... (may take a moment for Chave's height predictions if you have a poor connexion)", footer=NULL))
      on.exit(removeModal())

      ### results will contain for each plot (if the column exists):
      # Plot_ID, Ndens, Lat_cnt, Lon_cnt, MinDBH, MaxDBH, BA, Wood density(mean), H_Lorey_..., H_max_..., AGB_..., AGB_..._Cred_2.5, AGB_..._Cred_97.5

      out <- data.table(rv$inv_pred)
      if( ! "Plot_ID" %in% names(out)) setnames(out, old = "plot", new = "Plot_ID")
      if( ! "D" %in% names(out)) setnames(out, old = input$sel_DIAMETER, new = "D")

      # Adding Lat_cnt and Lon_cnt if exists
      if(!is.null(input$rad_coord) && input$rad_coord != "coord_none") {
        out <- merge(x = out, y = rv$coord_plot, by.x = "Plot_ID", by.y = "plot")
        setnames(out, old = c("long","lat"), new = c("Lon_cnt","Lat_cnt"))
      }

      # Add Ndens (number of tree)
      out[, Ndens := .N , by=Plot_ID]

      # Add MinDBH and maxDBH (cannot deal with two columns named "D" when summarizing by plot (line code below))
      out[, MinDBH := rv$inv_pred$D]
      out[, MaxDBH := rv$inv_pred$D]

      ## Summarize by plot: Ndens, MinDBH, BA (sum of individual BA), Wood density (mean), H_Lorey_...(sum) and H_... (max)
      list_fun <- list("Ndens" = min, "MinDBH" = min, "MaxDBH" = max,
                       "BA" = function(x) {round(sum(x), 2)}, "WD" = function(x) {round(mean(x), 4)},
                       "H_Lorey_mes" = sum,"H_Lorey_local_model" = sum, "H_Lorey_Feldpausch" = sum, "H_Lorey_Chave" = sum,
                       "H_mes" = max,"H_local_model" = max, "H_Chave" = max, "H_Feldpausch" = max)
      list_fun <- list_fun[ names(list_fun)[names(list_fun) %in% names(out)] ]

      # Apply each function to each column:
      out <- out[, mapply(function(fun, var) { fun(.SD[[var]]) }, list_fun, names(list_fun), SIMPLIFY = FALSE), by=Plot_ID]
      setnames(out, old = "WD", new = "MeanWD")

      # Dividing Lorey's column by BA to get the correct Lorey's heights
      for(x in grep("H_Lorey_", names(out), value = TRUE)) out[[x]] <- round(out[[x]] / out$BA, 2)

      ## Adding AGB's columns by merging rv$AGB_res[[method]][["summary"]] (and rename H_... by H_max_...)
      for(method in names(rv$AGB_res)) {
        # get the AGB summary for the current height estimation method
        res <- data.frame(rv$AGB_res[[method]][["summary"]])
        # round the results to 1 digit
        res[,c("AGB", "Cred_2.5", "Cred_97.5")] <- round(res[,c("AGB", "Cred_2.5", "Cred_97.5")], 1)

        # Rename columns depending on the method
        if(method != "user_height") { # if x == height, it means that heights were provided by the user, so the column renaming is different
          setnames(res, old = c("AGB","Cred_2.5","Cred_97.5"), new = c(paste0("AGB_",method), paste0("AGB_",method,"_Cred_2.5"), paste0("AGB_",method,"_Cred_97.5")))
          out <- merge(x = out, y = res, by.x = "Plot_ID", by.y = "plot")
          setnames(out, old = paste0("H_",method), new = paste0("H_max_",method))
        } else {
          out <- merge(x = out, y = res, by.x = "Plot_ID", by.y = "plot")
          setnames(out, old = "H_mes", new = "H_mes_max")
        }
      }

      write.csv(as.data.frame(out), file, row.names = FALSE)

    },
    contentType = "text/csv"
  )


  # SPATIALISATION -------------------------------------------------------------

  ## show coordinates table content ----
  output$table_coord_spatialisation <- renderDT(rv$df_coord, options = list(scrollX = TRUE))

  ## Execute check_plot_coord() and render the plot ----
  observeEvent(
    list(input$sel_x_rel_corner, input$sel_y_rel_corner,
         input$check_trust_GPS_corners, input$num_max_dist,
         input$sel_x_rel_trees, input$sel_y_rel_trees, input$sel_prop_trees,
         input$file_RASTER, input$btn_reset_raster),
    ignoreInit = TRUE, {

      req(input$sel_LONG_sup_coord, input$sel_LAT_sup_coord, input$sel_x_rel_corner, input$sel_y_rel_corner)

      if(input$sel_x_rel_corner!="<unselected>" & input$sel_y_rel_corner!="<unselected>" &
         input$sel_LONG_sup_coord!="<unselected>" & input$sel_LAT_sup_coord!="<unselected>") {

        # Prepare arguments
        if(input$rad_several_plots=="several_plots") {
          arg_plot_ID <- input$sel_plot_coord
        } else {
          arg_plot_ID <- NULL
        }
        arg_tree_data <- NULL
        arg_tree_coords <- NULL
        arg_tree_plot_ID <- NULL
        arg_prop_trees <- NULL

        if(!input$sel_x_rel_trees %in% c("<unselected>","") & !input$sel_y_rel_trees %in% c("<unselected>","")) {
          arg_tree_data <- rv$inv_pred
          arg_tree_coords <- c(input$sel_x_rel_trees, input$sel_y_rel_trees)
          if(input$rad_several_plots=="several_plots") {
            arg_tree_plot_ID <- input$sel_PLOT
          }
        }

        if( !input$sel_prop_trees %in% c("<unselected>","")) {
          arg_prop_trees <- input$sel_prop_trees
        }

        # Execute check_plot_coord
        rv$checked_plot <- tryCatch({
          check_plot_coord(corner_data = rv$df_coord,
                           longlat = c(input$sel_LONG_sup_coord, input$sel_LAT_sup_coord),
                           rel_coord = c(input$sel_x_rel_corner, input$sel_y_rel_corner),
                           trust_GPS_corners = input$check_trust_GPS_corners,
                           draw_plot = FALSE, ask = FALSE,
                           max_dist = input$num_max_dist,
                           plot_ID = arg_plot_ID,
                           tree_data = arg_tree_data, tree_coords = arg_tree_coords,
                           tree_plot_ID = arg_tree_plot_ID, prop_tree = arg_prop_trees,
                           ref_raster = rv$file_rast)
        }, error = function(e) e$message)

        # Generate plot visualization
        if(!is.character(rv$checked_plot)) {
          if(length(rv$checked_plot$plot_design) == 1) { # if one plot
            rv$gg_check_plot <- rv$checked_plot$plot_design + custom_theme
          } else { # if multiple plot, render the plot selected by the user
            plot_index <- match( input$sel_plot_display, names(rv$checked_plot$plot_design) )
            rv$gg_check_plot <- rv$checked_plot$plot_design[[plot_index]] + custom_theme
          }
        } else {
          print("rv$checked_plot error:")
          print(rv$checked_plot)
          rv$gg_check_plot <- ggplot() +
            annotate("text", x = 0, y = 0,
                     label = paste(
                       "An error occurred when creating the plot visualisation.",
                       "Ensure that your data is entered correctly in both the settings",
                       "and the 'load dataset' tab.",
                       "(an exemple is available at the end of this tab)",
                       sep = "\n"
                     ),
                     size = 5, color = "#e74c3c", fontface = "bold") +
            theme_void()
        }
      } else { # if relative corner coordinates aren't supplied
        rv$gg_check_plot <- ggplot() +
          annotate("text", x = 0, y = 0,
                   label = "Please fill the relative coordinates of the corners. ",
                   size = 5, color = "black", fontface = "bold") +
          theme_void()

      }
    })
  output$out_gg_check_plot <- renderPlot(silentPlot(rv$gg_check_plot))

  ## Update plot display ----
  observeEvent(input$sel_plot_display, ignoreInit = TRUE, {
    if(!is.null(rv$checked_plot) && !is.character(rv$checked_plot)) {
      plot_index <- match( input$sel_plot_display, names(rv$checked_plot$plot_design) )
      rv$gg_check_plot <- rv$checked_plot$plot_design[[plot_index]]
      rv$gg_check_plot <- rv$gg_check_plot + custom_theme
    }
  })
  ## Toggle max_dist setting ----
  observeEvent(input$check_max_dist, ignoreInit = TRUE, {
    toggleElement("id_max_dist", condition = input$check_max_dist==TRUE)
  })

  ## Show tree coordinates options ----
  observeEvent(list(input$sel_x_rel_corner, input$sel_y_rel_corner), ignoreInit = TRUE, {
    # Show only if check_plot_coord() with corner coordinates only is successful
    if(input$sel_x_rel_corner!="<unselected>" & input$sel_y_rel_corner!="<unselected>") {

      if(!is.null(rv$checked_plot) && !is.character(rv$checked_plot)) {
        showElement("id_param_check_plot")
        showElement("id_coord_trees")

        int_num_col <- names(rv$inv_pred)[ sapply(rv$inv_pred, class) %in% c("integer", "numeric")]
        for (id in c("sel_x_rel_trees", "sel_y_rel_trees","sel_prop_trees")) {
          updateSelectInput(session, id, choices = c("<unselected>", int_num_col))
        }
        # show inv_pred table content
        output$table_indiv_pred <- renderDT(rv$inv_pred, options = list(scrollX = TRUE))
      }
    }
  })

  ## Show raster and divide_plot options ----
  observeEvent(list(input$sel_x_rel_trees, input$sel_y_rel_trees, input$sel_prop_trees), ignoreInit = TRUE, {
    # Show only if check_plot_coord() with corner coordinates and trees is successful
    if(input$sel_x_rel_trees!="<unselected>" & input$sel_y_rel_trees!="<unselected>") {
      if(!is.null(rv$checked_plot) && !is.character(rv$checked_plot)) {
        showElement("id_raster")
        showElement("id_divide_plot")
      }
    }
  })

  ## Handle raster upload ----
  observeEvent(input$file_RASTER, ignoreInit = TRUE, {
    rv$file_rast <- tryCatch({
      terra::rast(input$file_RASTER$datapath)
    }, error = function(e) NULL)
    if(!is.null(rv$file_rast)) {
      showElement("id_raster_function")
    }
  })
  # Reset the file raster if needed
  observeEvent(input$btn_reset_raster, {
    shinyjs::reset("file_RASTER")
    rv$file_rast <- NULL
    hideElement("id_raster_function")
  })

  ## Toggle divide_plot settings ----
  observeEvent(input$check_divide_plot, ignoreInit = TRUE, {
    toggleElement("id_divide_plot_settings", condition = input$check_divide_plot==TRUE)
  })

  ## Process "Continue" button ----
  observeEvent(input$btn_check_plot_done, {

    ### Error management ----
    # Corner relative coordinates should be filed
    if ( input$sel_x_rel_corner=="<unselected>" | input$sel_y_rel_corner=="<unselected>") {
      shinyalert("Oops!", "You must enter the relative coordinates of the corners.", type = "error")
      return()
    }
    # Tree relative coordinates should be filed
    if ( input$sel_x_rel_trees=="<unselected>" | input$sel_y_rel_trees=="<unselected>") {
      shinyalert("Oops!", "You must enter the relative coordinates of the trees.", type = "error")
      return()
    }
    # check_plot_coord has to be OK
    if (is.null(rv$checked_plot) | is.character(rv$checked_plot)) {
      shinyalert("Oops!", "Something went wrong when checking for the coordinates. Please ensure that you have entered the information correctly. You will then be able to continue once the plot has been displayed correctly.", type = "error")
      return()
    }

    ## Execute divide_plot ----
    if(input$check_divide_plot) {

      rv$divide_output <- tryCatch({
        if(input$rad_several_plots == "several_plots") {
          arg_corner_plot_ID <- "plot_ID"
          arg_tree_plot_ID <- "plot_ID"
        } else {
          arg_corner_plot_ID <- NULL
          arg_tree_plot_ID <- NULL
        }
        divide_plot(corner_data = rv$checked_plot$corner_coord,
                    rel_coord = c("x_rel","y_rel"),
                    proj_coord = c("x_proj","y_proj"),
                    grid_size = input$num_grid_size,
                    tree_data =  rv$checked_plot$tree_data, tree_coords = c("x_rel","y_rel"),
                    corner_plot_ID = arg_corner_plot_ID, tree_plot_ID = arg_tree_plot_ID,
                    grid_tol = 0.9)
      }, error = function(e) e$message)

    } else { # if no plot division, recreate the output

      # recreate sub_corner_coord output
      sub_corner_coord <- rv$checked_plot$corner_coord
      if(input$rad_several_plots=="single_plot") {
        sub_corner_coord$plot_ID <- ""
      }
      sub_corner_coord$subplot_ID <- as.character(sub_corner_coord$plot_ID)
      # recreate tree_data output
      tree_data <- rv$checked_plot$tree_data
      tree_data$plot_ID <- as.character(tree_data$plot) #column "plot" has been created when setting rv$inv()
      tree_data$subplot_ID <- as.character(tree_data$plot) #column "plot" has been created when setting rv$inv()
      tree_data$subplot_ID[!tree_data$is_in_plot] <- NA

      rv$divide_output <- list(sub_corner_coord = sub_corner_coord, tree_data = tree_data)
    }

    if(is.character(rv$divide_output)) {
      print("error in divide_plot:")
      print(rv$divide_output)
      shinyalert("Oops!", paste0("Something went wrong when dividing your plot.\nError:", rv$divide_output), type = "error")
      return()
    } else {
      # If no error on divide_plot, update tab_SP_SUMMARY
      shinyjs::show("nav_item_summary")
      shinyjs::hide("tab_SPATIALISATION")
      shinyjs::show("tab_SP_SUMMARY")
      shinyjs::removeClass("nav_link_spatial", "active")
      shinyjs::addClass("nav_link_summary", "active")
      # update input selections in the next tab if not already done
      if(input$sel_first_metric=="") {
        int_num_col <- names(rv$divide_output$tree_data)[ sapply(rv$divide_output$tree_data, class) %in% c("integer", "numeric")]
        int_num_col <- int_num_col[! int_num_col %in% c("plot","plot_ID","subplot_ID","x_proj","y_proj")]
        updateSelectInput(session, "sel_first_metric", choices = c("<unselected>", int_num_col))
        updateSelectInput(session, "sel_first_function", choices = c("<unselected>", names(available_functions)))
      }
      toggleElement("id_sel_plot_summary", condition = input$rad_several_plots=="several_plots")
      if(input$rad_several_plots=="several_plots") {
        updateSelectInput(session,
                          "sel_plot_display_summary",
                          choices = unique(rv$df_coord[,"Plot"]),
                          selected = input$sel_plot_display)
      }

      ## Apply subplot_summary() to calculated AGBD values from AGBmonteCarlo() ----
      list_sub_sum <- lapply(rv$AGB_res, function(dat) {
        subplot_summary(subplots = rv$divide_output, AGB_simu = dat[["AGB_simu"]], per_ha = TRUE)
      })
      rv$subplot_summary_output <- merge_subplot_summary(list_sub_sum)

      # update sel_metric_display_summary which will trigger plot display
      updateSelectInput(session, "sel_metric_display_summary", choices = NULL)
      metric_names <- names(rv$subplot_summary_output$tree_summary)[!names(rv$subplot_summary_output$tree_summary) %in% c("plot_ID","subplot_ID")]
      metric_names <- metric_names[!grepl("_cred_",metric_names)]
      updateSelectInput(session, "sel_metric_display_summary", choices = metric_names)

    }
  })

  # render the tree_data table
  output$table_divide_plot <- renderDT(rv$divide_output$tree_data, options = list(scrollX = TRUE))


  # SUMMARISE METRICS ----------------------------------------------------------

  ## Add a metric when clicking "Add a metric" ----
  observeEvent(input$btn_add_metric, {
    # Create a unique ID for each new selectInput
    unique_id <- paste0("select_", input$btn_add_metric)
    metric_choices <- names(rv$divide_output$tree_data)[!names(rv$divide_output$tree_data) %in%
                                                          c("is_in_plot","plot","plot_ID","subplot_ID","x_proj","y_proj")]
    insertUI(
      selector = "#container_selec_metric",
      where = "beforeEnd",

      ui = layout_columns(
        col_widths = c(5, 5, 2),
        selectInput(paste0("sel_metric_", unique_id),
                    label = NULL,
                    choices = c("<unselected>", metric_choices)
        ),
        selectInput(paste0("sel_function_", unique_id),
                    label=NULL,
                    choices = c("<unselected>",names(available_functions))
        ),
        checkboxInput(paste0("checkbox_per_ha_", unique_id),
                      label = "per ha", value = TRUE)
      )
    )
  })

  ## Reaction to "Summarise" button ----
  observeEvent(input$btn_summarise, {

    if(input$sel_first_metric == "<unselected>" & input$sel_first_function == "<unselected>" & is.null(rv$file_rast)) {
      shinyalert("Oops", "You didn't provide any additionnal metric to summarised. As AGBD is automatically summarised, you can proceed to the download of the results.", type = "error")
    } else {

      ### Formatting subplot_summary arguments ----
      name_arg_value <- c("sel_first_metric", grep("^sel_metric_select_", names(input), value = TRUE))
      name_arg_per_ha <- c("check_first_per_ha", grep("^checkbox_per_ha_select", names(input), value = TRUE))
      name_arg_fun <- c("sel_first_function", grep("^sel_function_select_", names(input), value = TRUE))
      name_arg_raster_fun <- ifelse(input$sel_raster_function!="",input$sel_raster_function,"mean")
      arg_raster_fun <- available_functions[[name_arg_raster_fun]]

      filter_select <- sapply(name_arg_value, function(x) input[[x]] != "<unselected>") &
        sapply(name_arg_fun, function(x) input[[x]] != "<unselected>")

      if(input$btn_add_metric>=1 && sum(filter_select)>1 ) {
        arg_value <- sapply(name_arg_value, function(x) input[[x]])[filter_select]
        arg_per_ha <- sapply(name_arg_per_ha, function(x) input[[x]])[filter_select]
        arg_fun <- lapply(name_arg_fun, function(x) available_functions[[input[[x]]]])[filter_select]
        names(arg_fun) <- name_arg_fun[filter_select]
      } else {
        if(input$sel_first_metric == "<unselected>" || input$sel_first_function == "<unselected>") {
          arg_value <- NULL
          arg_fun <- sum
        } else {
          arg_value <- input$sel_first_metric
          arg_fun <- available_functions[[input$sel_first_function]]
        }
        arg_per_ha <- input$check_first_per_ha
      }

      ## Call subplot_summary() ----
      subplot_summary_value <- tryCatch({
        subplot_summary(
          subplots = rv$divide_output, draw_plot = FALSE,
          value = arg_value, per_ha = arg_per_ha, fun = arg_fun,
          ref_raster = rv$file_rast, raster_fun = arg_raster_fun)
      }, error = function(e) e$message)

      if(!is.character(subplot_summary_value)) {

        # Merge subplot_summary_value with rv$subplot_summary_output
        rv$subplot_summary_output$tree_summary <- cbind(
          rv$subplot_summary_output$tree_summary,
          subplot_summary_value$tree_summary[, .SD, .SDcols = !"subplot_ID"])

        rv$subplot_summary_output$polygon <- cbind(
          rv$subplot_summary_output$polygon,
          subplot_summary_value$polygon[, .SD, .SDcols = !c("plot_ID","subplot_ID","sf_subplot_polygon")])

        if(input$rad_several_plots == "several_plots") {
          for(plot_name in names(subplot_summary_value$plot_design)) {
            rv$subplot_summary_output$plot_design[[plot_name]] <- c(
              rv$subplot_summary_output$plot_design[[plot_name]], subplot_summary_value$plot_design[[plot_name]])
          }
        } else {
          rv$subplot_summary_output$plot_design <- c(
            rv$subplot_summary_output$plot_design, subplot_summary_value$plot_design)

        }

        # Update metric selection
        metric_names <- names(rv$subplot_summary_output$tree_summary)[!names(rv$subplot_summary_output$tree_summary) %in% c("plot_ID","subplot_ID")]
        metric_names <- metric_names[!grepl("_cred_",metric_names)]
        updateSelectInput(session, "sel_metric_display_summary", choices = metric_names)
        updateMaterialSwitch(session, "switch_ggplot", TRUE)

        ### Create FOS like results at subplot level ----

        rv$FOS_subplot <- tryCatch({
          FOS_subplot_res(
            checked_plot = rv$checked_plot,
            divide_output = rv$divide_output,
            subplot_summary_output = rv$subplot_summary_output)
        }, error = function(e) e$message)

      } else { # print message error
        print(subplot_summary_value)
      }
    }
  })

  ## Change the plot and metric visualisation ----
  observeEvent(list(input$sel_plot_display_summary, input$sel_metric_display_summary, input$switch_ggplot), ignoreInit = TRUE, {

    if(input$switch_ggplot) { # summary visualisation
      metric_names <- names(rv$subplot_summary_output$tree_summary)[!names(rv$subplot_summary_output$tree_summary) %in% c("plot_ID","subplot_ID")]
      metric_names <- metric_names[!grepl("_cred_",metric_names)]

      if(input$rad_several_plots=="several_plots") {
        #Render the plot_design output (plot_design[[plot_ID]][[metric]])
        rv$gg_subplot_sum <- rv$subplot_summary_output$plot_design[[
          match(input$sel_plot_display_summary, names(rv$subplot_summary_output$plot_design))]][[
            match(input$sel_metric_display_summary, metric_names)
          ]]
      } else {
        #Render the plot_design output (plot_design[[metrics]] or just plot_design if only one metric)
        rv$gg_subplot_sum <- rv$subplot_summary_output$plot_design[[
          match(input$sel_metric_display_summary, metric_names)
        ]]

      }
    } else { # check_plot (and grid) visualisation

      if(input$rad_several_plots=="several_plots") {

        plot_index <- match( input$sel_plot_display_summary, names(rv$checked_plot$plot_design) )
        rv$gg_check_plot <- rv$checked_plot$plot_design[[plot_index]]
        rv$gg_check_plot <- rv$gg_check_plot + custom_theme

        grid_dat <- rv$divide_output$sub_corner_coord[rv$divide_output$sub_corner_coord$plot_ID == input$sel_plot_display_summary,]

      } else {

        rv$gg_check_plot <- rv$checked_plot$plot_design
        rv$gg_check_plot <- rv$gg_check_plot + custom_theme

        grid_dat <- rv$divide_output$sub_corner_coord

      }

      if(input$check_divide_plot) {
        rv$gg_subplot_sum <- rv$gg_check_plot +
          geom_polygon(data = grid_dat,
                       mapping = aes(x = x_proj, y=y_proj, group=subplot_ID),
                       fill = NA, colour="black", linewidth=1) +
          custom_theme
      } else {
        rv$gg_subplot_sum <- rv$gg_check_plot + custom_theme
      }
    }

  })
  output$out_gg_subplot_sum <- renderPlot(rv$gg_subplot_sum)





  ## Spatialised (sub)plot level results (FOS like csv) ----
  output$dwl_subplot_file <- downloadHandler(
    filename = function() {
      paste0("spatialised_results_", Sys.Date(), ".csv")
    },
    content = function(file) {

      if(is.null(rv$FOS_subplot)) {
        rv$FOS_subplot <- FOS_subplot_res(
          checked_plot = rv$checked_plot,
          divide_output = rv$divide_output,
          subplot_summary_output = rv$subplot_summary_output)
      }

      write.csv(rv$FOS_subplot, file, row.names = FALSE)

    },
    contentType = "text/csv"
  )

  ## Shapefile download ----
  output$dwl_shapefile <- downloadHandler(
    filename = function() {
      paste0("spatialised_results_", Sys.Date(), ".gpkg")
    },
    content = function(file) {

      if(length(unique(rv$checked_plot$UTM_code$UTM_code)) != 1 ) {
        shinyalert("Oops", "It seems that your plots do not share the same UTM code, which is required in order to download the shapefiles.", type = "error")
        return(NULL)
      }

      if(is.null(rv$FOS_subplot)) {
        rv$FOS_subplot <- FOS_subplot_res(
          checked_plot = rv$checked_plot,
          divide_output = rv$divide_output,
          subplot_summary_output = rv$subplot_summary_output)
      }

      # Add rv$FOS_subplot to rv$subplot_summary_output$polygon
      polygons <- rv$subplot_summary_output$polygon[,1:3]
      polygons <- cbind(polygons, rv$FOS_subplot[,-c(1,2)])

      sf::st_crs(polygons) <- unique(unique(rv$checked_plot$UTM_code$UTM_code))
      sf::st_write(polygons, file, delete_dsn = TRUE)

    },
    contentType = NULL
  )

  ### Report ----
  output$dwl_sp_report <- downloadHandler(
    filename = function() {
      paste0("BIOMASS_sp_report_", Sys.Date(), ".html")
    },
    content = function(file) {

      if(is.null(rv$FOS_subplot)) {
        rv$FOS_subplot <- FOS_subplot_res(
          checked_plot = rv$checked_plot,
          divide_output = rv$divide_output,
          subplot_summary_output = rv$subplot_summary_output)
      }

      # Copy the report file to a temporary directory before processing it, in case we don't have write permissions to the current working dir (which can happen when deployed).
      tempReport <- file.path(tempdir(), "report_BIOMASS.Rmd")
      file.copy(
        #from = system.file("Rmarkdown", "report_BIOMASS.Rmd", package = "BIOMASSapp"),
        from = "~/BIOMASSapp/inst/Rmarkdown/report_BIOMASS.Rmd",
        tempReport,
        overwrite = TRUE
      )

      if (file.exists(file)) {
        file.remove(file)
      }

      rmarkdown::render(tempReport, output_file = file)
    },
    contentType = "text/html"
  )

}
