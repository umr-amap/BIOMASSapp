function(input, output, session) {

  # stop the serveur in the end of the session
  session$onSessionEnded(function() {
    stopApp()
  })

  observe({
    # hide few menu at the begining
    hideMenuItem("tab_TAXO")
    hideMenuItem("tab_HEIGHT")
    hideMenuItem("tab_AGB")
  })


  # LOAD DATASET ---------------------------------------------------------------

  inv <- reactiveVal(label = "data frame")
  ## Forest inventory file actions ----
  observeEvent(ignoreInit = T, {
    input$file_DATASET
    input$rad_decimal
    #input$num_skip_line
  }, {
    # Read forest inventory upload
    inv(fread(
      file = req(input$file_DATASET)$datapath,
      #skip = ifelse(is.na(input$num_skip_line) || input$num_skip_line == 0, "__auto__", input$num_skip_line),
      data.table = F,
      dec = input$rad_decimal
    ))

    # show the box
    showElement("box_FIELDS")
    showElement("box_DATASET")
    showElement("box_COORD")

    # show forest inventory content
    output$table_DATASET <- renderDT(inv(), options = list(scrollX = TRUE))


    # update the values of the select inputs with the column names
    int_num_col <- names(inv())[ sapply(inv(), class) %in% c("integer", "numeric")]
    for (id in c("sel_DIAMETER", "sel_WD")) {
      updateSelectInput(session, id, choices = c("<unselected>", int_num_col))
    }
    char_col <- names(inv())[ sapply(inv(), class) %in% "character" ]
    for (id in c("sel_GENUS", "sel_SPECIES")) {
      updateSelectInput(session, id, choices = c("<unselected>", char_col))
    }
    name <- names(inv())
  })

  # Plot's IDs selection when several plots
  observeEvent(input$rad_several_plots, {
    updateSelectInput(session, "sel_PLOT", choices = c("<unselected>", names(inv())))
    toggleElement("sel_PLOT", condition = input$rad_several_plots == "several_plots")
  })

  # Hide/Show "information required" message
  observe(
    if( (req(input$rad_several_plots) == "single_plot") | (req(input$rad_several_plots) =="several_plots" & ! req(input$sel_PLOT) %in% c("<unselected>","")) ) {
      hideElement("msg_several_plots")
    } else {
      showElement("msg_several_plots")
    }
  )

  ## Height ----
  ### height radio button actions ----
  observeEvent(input$rad_height, {
    int_num_col <- names(inv())[ sapply(inv(), class) %in% c("integer", "numeric")]
    updateSelectInput(session, "sel_H", choices = c("<unselected>", int_num_col))
    updateSelectInput(session, "sel_HDmodel_by", choices = c("<unselected>", names(inv())))
    toggleElement("id_sel_h",
                  condition = input$rad_height %in% c("h_each_tree","h_some_tree"))
    toggleElement("id_sel_HDmodel_by",
                  condition = input$rad_height == "h_some_tree")
    toggleElement("id_file_h_sup",
                  condition = input$rad_height == "h_sup_data")
  })
  ### height file actions ----
  observeEvent(req(input$file_h_sup), {
    # Read HD supplementary dataset
    rv_h_sup <- reactiveVal(label = "data frame")
    rv_h_sup(fread(
      file = req(input$file_h_sup)$datapath,
      data.table = F))

    # show coordinates table content
    output$table_h_sup <- renderDT(rv_h_sup(),
                                   options = list(scrollX = TRUE))
    toggleElement("box_h_sup_preview", condition = input$rad_height == "h_sup_data")

    # Update Diameter & Height seletions
    int_num_col <- names(rv_h_sup())[ sapply(rv_h_sup(), class) %in% c("integer", "numeric") ]
    updateSelectInput(session, "sel_D_sup_data", choices = c("<unselected>", int_num_col))
    updateSelectInput(session, "sel_H_sup_data", choices = c("<unselected>", int_num_col))
  })

  ## Coordinates ----
  ### coordinates radio button actions ----
  observeEvent(input$rad_coord, {
    if(input$rad_coord == "coord_each_tree") {
      # Show latitude and longitude select inputs for forest inventory table
      int_num_col <- names(inv())[ sapply(inv(), class) %in% c("integer", "numeric")]
      updateSelectInput(session, "sel_LAT", choices = c("<unselected>", int_num_col))
      updateSelectInput(session, "sel_LONG", choices = c("<unselected>", int_num_col))
    }
    toggleElement("id_sel_coord", condition = input$rad_coord == "coord_each_tree") # show latitude & longitude selection from forest inventory data
    toggleElement("id_file_coord", condition = input$rad_coord == "coord_plot") # show file input
    toggleElement("id_sel_coord_plot", condition = input$rad_coord == "coord_plot") # show latitude & longitude selection from coordinates data
    toggleElement("sel_plot_coord", # show plot ID selection
                  condition = input$rad_coord == "coord_plot" &
                    !is.null(input$rad_several_plots) &&
                    input$rad_several_plots == "several_plots")
  })

  ### coordinates file actions  ----
  observeEvent(input$file_coord, {
    # Read plot coordinates upload
    rv_coord <- reactiveVal(label = "data frame")
    rv_coord(fread(
      file = req(input$file_coord)$datapath,
      data.table = F))

    # show coordinates table content
    output$table_coord <- renderDT(rv_coord(),
                                   options = list(scrollX = TRUE))
    toggleElement("box_coord_preview", condition = input$rad_coord == "coord_plot")

    # Update latitude & longitude & plot's IDs selections
    int_num_col <- names(rv_coord())[ sapply(rv_coord(), class) %in% c("integer", "numeric") ]
    updateSelectInput(session, "sel_LAT_sup_coord", choices = c("<unselected>", int_num_col))
    updateSelectInput(session, "sel_LONG_sup_coord", choices = c("<unselected>", int_num_col))
    updateSelectInput(session, "sel_plot_coord", choices = c("<unselected>", names(rv_coord())))
  })


  ## Errors management when clicking on continue ----
  observeEvent(input$btn_DATASET_LOADED, {
    error <- F
    if ( is.null(input$rad_several_plots) ) {
      # if the plot number information is not provide
      error <- T
      shinyalert("Oops!", "You need to answer to the question : Does your dataset contain several plots?", type = "error")
    } else if (input$rad_several_plots =="several_plots" && input$sel_PLOT == "<unselected>" ) {
      # if the column corresponding to the plot IDs is unselected
      error <- T
      shinyalert("Oops!", "The column containing the plots IDs is unselected", type = "error")
    } else if (input$sel_DIAMETER == "<unselected>") { # if diameter is not selected
      error <- T
      shinyalert("Oops!", "D is unselected", type = "error")
    } else if (!xor(input$sel_WD == "<unselected>", input$sel_GENUS == "<unselected>")) {
      # if the wd is not selected or genus not selected but not the two
      error <- T
      shinyalert("Oops!", "To estimate the Above Ground Biomass, you either need the wood density or the taxonomy of the trees", type = "error")
    } else if (is.null(input$rad_height)) {
      # if the H radio button has not been ticked
      error <- T
      shinyalert("Oops!", "Height information has not been provided", type = "error")
    } else if ( input$rad_height %in% c("h_each_tree","h_some_tree") && input$sel_H == "<unselected>" ) {
      # if the H column is unselected
      error <- T
      shinyalert("Oops!", "Height column is unselected", type = "error")
    } else if ( input$rad_height == "h_sup_data" && is.null(input$file_h_sup)){
      # if the H-D relationship is in another dataset which have not been loaded
      error <- T
      shinyalert("Oops!", "The dataset containing a subset of well-measured trees has not been loaded", type = "error")
    } else if ( input$rad_height == "h_sup_data" && (input$sel_H_sup_data == "<unselected>" | input$sel_D_sup_data == "<unselected>") ){
      # if the H or D column (of the sup dataset containing H-D relationship) are unselected
      error <- T
      shinyalert("Oops!", "Diameter and/or Height column(s) of the dataset containing a subset of well-measured trees is/are unselected ", type = "error")
    } else if (input$rad_height == "h_none" && (is.null(input$rad_coord) || input$rad_coord %in% c("","coord_none"))  ) {
      # if no height measurements and no coordinates
      error <- T
      shinyalert("Oops!", "To estimate tree heights, you either need a subset of well-measured trees or the coordinates of the plots", type = "error")
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_each_tree" & (input$sel_LONG == "<unselected>" | input$sel_LAT == "<unselected>")) {
      # if the coordinates of each tree is ticked but one of the two (long or lat) is not selected
      error <- T
      shinyalert("Oops!", "Tree's longitude and/or latitude is/are unselected", type = "error")
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_plot" & is.null(input$file_coord) ) {
      # if the coordinates of the plots (in another dataset) is ticked but the dataset has not been loaded
      error <- T
      shinyalert("Oops!", "The dataset containing the coordinates of the plot(s) has not been loaded", type = "error")
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_plot" & (input$sel_LAT_sup_coord == "<unselected>" | input$sel_LAT_sup_coord == "<unselected>") ) {
      # if the Latitude or Longitude column (of the sup dataset containing plot's coordinates) are unselected
      error <- T
      shinyalert("Oops!", "Latitude and/or Longitude column(s) of the dataset containing the coordinates of the plot(s) is/are unselected ", type = "error")
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_plot" & input$rad_several_plots == "several_plots" & input$sel_plot_coord == "<unselected>") {
      # if the plots IDs column for sup coord is unselected
      error <- T
      shinyalert("Oops!", "Plots IDs of the dataset containing the coordinates of the plot(s) is unselected ", type = "error")
    } else if (input$sel_WD == "<unselected>") {
      # if the WD is not selected then show the tab TAXO
      showMenuItem("tab_TAXO")
      updateTabItems(session, "mnu_MENU", "tab_TAXO")
    } else { # If no error
      if (input$rad_height == "h_each_tree") { # if heights are not to estimate
        showMenuItem("tab_AGB")
        updateTabItems(session, "mnu_MENU", "tab_AGB")
      } else {
        # else show the height tab
        showMenuItem("tab_HEIGHT")
        updateTabItems(session, "mnu_MENU", "tab_HEIGHT")
      }
    }

    if (!error) {
      newData <- inv()
      if (input$sel_DIAMETER != "<unselected>") {
        newData[, input$sel_DIAMETER] <- conv_unit(newData[, input$sel_DIAMETER], input$rad_units_diameter, "cm")
      }
      if (input$sel_H != "<unselected>") {
        newData[, input$sel_H] <- conv_unit(newData[, input$sel_H], input$rad_units_height, "m")
      }
      inv(newData)
    }
  })

  ## Warnings ----
  # If the diameter is unselected => red box
  observeEvent(input$sel_DIAMETER, {
    feedbackDanger("sel_DIAMETER",
                   show = input$sel_DIAMETER == "<unselected>",
                   text = "Compulsory argument"
    )
  })
  # if the wd is not selected or genus not selected but not the two
  observe({
    toggleElement("msg_wd",
      condition =
        !xor(input$sel_WD == "<unselected>", input$sel_GENUS == "<unselected>")
    )
  })


  # TAXONOMY -------------------------------------------------------------------

  wd <- reactiveVal(NULL, label = "wood density")

  observeEvent(input$btn_TAXO_RESULT, {
    showElement("box_RESULT_TAXO")
    # show a progress bar
    withProgress(message = "Correcting the taxonomy", value = 0, {
      # if the users have selected the correct taxo + get wd
      if (input$rad_WD == "corr") {
        # correct the taxo and catch the error if there is error
        taxo <- tryCatch({
          correctTaxo(
            genus = inv()[, input$sel_GENUS],
            species = if (input$sel_SPECIES != "<unselected>") inv()[, input$sel_SPECIES]
          )
        }, error = function(e) e)

        # if there is an error display it
        if (!is.data.frame(taxo)) {
          output$out_taxo_error <- renderPrint({
            taxo$message
          })
        } else { # if not a message will appear
          output$out_taxo_error <- renderPrint({
            cat("Summary of taxonomy corrections (in number of trees):\n")
            taxo_display <- factor(taxo$nameModified, labels = c("Correct spelling of taxa (unmodified)","Species not found (unmodified)","Taxa not found (unmodified)","Taxa found and corrected (modified)"))
            print(table(taxo_display, dnn = ""))
          })
        }
        # update the progression
        incProgress(1 / 2, detail = "Taxonomy correction completed", message = "Extracting wood density values")
        genus <- taxo$genusCorrected
        species <- taxo$speciesCorrected
      } else {
        # if the users do not choose the correct taxo
        genus <- inv()[, input$sel_GENUS]
        if (input$sel_SPECIES == "<unselected>") {
          split <- tstrsplit_NA(genus)
          genus <- split[[1]]
          species <- split[[2]]
        } else {
          species <- inv()[, input$sel_SPECIES]
        }
        hideSpinner(id = "out_taxo_error")
      }
      wd(tryCatch(getWoodDensity(genus, species, stand = if (input$sel_PLOT != "<unselected>") inv()[, input$sel_PLOT]),
        error = function(e) e,
        warning = function(e) e
      ))

      # if there is an error display it
      if (!is.data.frame(wd())) {
        output$out_wd_error <- renderPrint({
          taxo$message
        })
        wd(NULL)
      } else { # if not a message will appear

        output$out_wd_error <- renderPrint({
          cat("Taxonomic levels at which wood density was attributed to trees (in %):\n")
          levelswd <- 100 * table(wd()$levelWD) / nrow(wd())
          if (input$sel_PLOT != "<unselected>") {
            data.frame(
              "Species level" = round(levelswd["species"], 1),
              "Genus level" = round(levelswd["genus"], 1),
              "Plot level" = round(sum(levelswd[!names(levelswd) %in% c("dataset", "genus", "species")]), 1),
              "User dataset level" = round(levelswd["dataset"], 1),
              check.names = FALSE
            )
          } else {
            data.frame(
              "Species level" = round(levelswd["species"], 1),
              "Genus level" = round(levelswd["genus"], 1),
              "User dataset level" = round(levelswd["dataset"], 1),
              check.names = FALSE
            )
          }
        })
      }
      incProgress(1, detail = "Wood density extraction completed")
    })

    show(id = "btn_TAXO_DONE")
  })

  # when the taxo is done
  observeEvent(input$btn_TAXO_DONE, {
    if (!is.data.frame(wd())) { # verify if there is all the column present
      shinyalert("Oops", "Somethings went wrong, please check this", type = "error")
    } else {
      showMenuItem("tab_HEIGHT")
      updateTabItems(session, "mnu_MENU", "tab_HEIGHT")
    }
  })


  # HEIGHT ---------------------------------------------------------------------

  # Affichage du boutton Done : il faut au moins une des cases du model choisi et c'est tout ???
  observe({
    toggle("btn_HD_DONE", condition = !is.null(input$chkgrp_HEIGHT))
  })

  # feldRgion en reactiveVal ??
  feldRegion <- reactiveVal(c(
    Africa = "Africa",
    CAfrica = "Central Africa",
    EAfrica = "Eastern Africa",
    WAfrica = "Western Africa",
    SAmerica = "South America",
    BrazilianShield = "Brazilian Shield",
    ECAmazonia = "Eastern-central Amazonia",
    GuianaShield = "Guiana Shield",
    WAmazonia = "Western Amazonia",
    SEAsia = "Southeast Asia",
    NAustralia = "Northern Australia",
    Pantropical = "Pantropical"
  ))

  # If Local HD model, Feldpausch or Chave :
  #observeEvent(input$chkgrp_HEIGHT, ignoreNULL = F, {
  observe({

    print("prout")

    ## HD local model ----------------------------------------------------------
    if ("HDloc" %in% req(input$chkgrp_HEIGHT)) {

      if (input$rad_height == "h_none") { # if no height at all, not possible
        shinyalert("Oops", "Local HD model cannot be build if no height measurements have been provided.", type = "error")
      }

      ### Formatting the dataset which will be used to build the models
      #hd_data <- reactiveVal(label = "HD data") # hd_data must be reactive to changes in column selection

      if (input$rad_height == "h_some_tree") { # if height of some trees in the same dataset
        #hd_data(setDT(inv()[, c(input$sel_DIAMETER, input$sel_H)]))
        hd_data <- setDT(inv()[, c(input$sel_DIAMETER, input$sel_H)])
        if(input$sel_HDmodel_by != "<unselected>"){ # if one model by plot or region or whatever
          #hd_data()[, model_by := input$sel_HDmodel_by]
          hd_data[, model_by := input$sel_HDmodel_by]
          # remove all the plots with less than 15 non NA value
          removedPlot <- unique(hd_data()[, .(nbNonNA = sum(!is.na(H))), by = model_by][nbNonNA < 15, model_by])
          hd_data()[!model_by %in% removedPlot]
          # remove plots for which D is not well distributed --> WHY ???
          hd_data()[, quantile := findInterval(D, c(-1, quantile(D, probs = c(0.5, 0.75)), max(D) + 1)), by = model_by]
          removedPlot <- c(removedPlot, unique(hd_data()[, .N, by = .(model_by, quantile)][N < 3, model_by]))
          hd_data()[!model_by %in% removedPlot]
          # if there is a least one plot in the removed plot -> warning message
          if (length(removedPlot) != 0) {
            shinyalert("Oops", paste(
              "Local HD model cannot be built for:",
              paste(removedPlot, collapse = ", "),
              "\n either:",
              "\n\t - there are not enough local height measurements",
              "\n\t - height measurements are likely not representative of tree size distribution",
              "\n You may have to build a single model grouping all the plots together by unselecting corresponding column."
            ), type = "warning")
          }
        }
      }

      if (input$rad_height == "h_sup_data") { # if height in another dataset
        hd_data(setDT(rv_h_sup()[, c(input$sel_D_sup_data, input$sel_H_sup_data)]))
      }

      # Setting D and H column names of hd_data()
      #setnames(hd_data(), names(hd_data())[1:2], c("D", "H"))
      setnames(hd_data, names(hd_data)[1:2], c("D", "H"))

      #print(summary(hd_data()$H))
      print(summary(hd_data$H))

      ### Building and compare the 4 local HD models
      tab_modelHD <- modelHD(
        #D = hd_data()$D,
        D = hd_data$D,
        #H = hd_data()$H,
        H = hd_data$H,
        plot = ifelse(test = req(input$sel_HDmodel_by) != "<unselected>", yes = inv()[, input$sel_HDmodel_by], no = "all")
      )
      # render the table
      output$out_tab_HD <- renderTable(tab_modelHD[, -3], digits = 4)
      # update the radio button with the method and choose the minimun of the RSE
      if(req(input$rad_HDMOD) != tab_modelHD$method[which.min(tab_modelHD$RSE)]) {
        updateRadioButtons(session,
                           inputId = "rad_HDMOD",
                           choices = tab_modelHD$method,
                           selected = tab_modelHD$method[which.min(tab_modelHD$RSE)],
                           inline = T)
      }


      # with(
      #   tab_modelHD,
      #   updateRadioButtons(session,
      #     inputId = "rad_HDMOD",
      #     choices = method,
      #     selected = method[which.min(RSE)],
      #     inline = T
      #   )
      # )

      ### Building the model with the lowest RMSE or chosen by the user
      #hd_model <- reactiveVal(label = "HD local model")
      #hd_model(tryCatch({
      hd_model <- (tryCatch({
        modelHD(
          #D = hd_data()$D,
          D = hd_data$D,
          #H = hd_data()$H,
          H = hd_data$H,
          method = input$rad_HDMOD,
          plot = ifelse(input$sel_HDmodel_by != "<unselected>", yes = inv()[, input$sel_HDmodel_by], no = "all"),
          useWeight = T
        )
      }, error = function(e) NULL, warning = function(e) NULL, message = function(e) NULL))

      ### Show the box containing the result of hd_model
      showElement("box_RESULT_HDMOD")

    } else {
      hideElement("box_RESULT_HDMOD")
    }

    # Feldpausch method --------------------------------------------------------

    if ("feld" %in% input$chkgrp_HEIGHT) {

    long_lat <- input$sel_LONG != "<unselected>" && input$sel_LAT != "<unselected>"

    toggleElement("box_MAP",
      condition = (any(c("feld", "chave") %in% id) || long_lat)
    )

    # If Feldpausch box is ticked
    toggleElement("box_RESULT_FELD", condition = "feld" %in% id)

    # If Chave box is ticked
    toggleElement("box_result_chave", condition = "chave" %in% id)
    }
  })


  observeEvent(input$btn_HD_DONE, {
    if (is.null(input$chkgrp_HEIGHT)) {
      shinyalert("Oops", "Select at least one HD model", type = "error")
    } else {
      showMenuItem("tab_AGB")
      updateTabItems(session, "mnu_MENU", "tab_AGB")
    }
  })


  # observe({
  #   if ("HDloc" %in% input$chkgrp_HEIGHT & input$sel_H == "<unselected>") {
  #     updateCheckboxGroupInput(session, "chkgrp_HEIGHT",
  #       selected = input$chkgrp_HEIGHT[!input$chkgrp_HEIGHT %in% "HDloc"]
  #     )
  #   }
  # })

  # observeEvent({
  #   if (input$btn_DATASET_LOADED >= 1) {
  #     input$sel_PLOT
  #     input$sel_H
  #     input$sel_DIAMETER
  #   }
  #   input$rad_HDMOD
  #   input$chkgrp_HEIGHT
  # }, ignoreNULL = F, ignoreInit = T, {
  #   if (("HDloc" %in% input$chkgrp_HEIGHT) & (input$sel_H != "<unselected>") & (input$rad_HDMOD != "NULL")) {
  #     # take the data D, H, plot
  #     data <- setDT(inv()[, c(input$sel_DIAMETER, input$sel_H)])
  #     setnames(data, names(data), c("D", "H"))
  #     data[, plot := if (input$sel_PLOT == "<unselected>") "plot" else inv()[, input$sel_PLOT]]
  #
  #     # remove all the plots with 15 non NA value
  #     removedPlot <- unique(data[, .(nbNonNA = sum(!is.na(H))), by = plot][nbNonNA < 15, plot])
  #     data <- data[!plot %in% removedPlot]
  #
  #     # remove the plots where there the plot is not really distributed
  #     data[, quantile := findInterval(D, c(-1, quantile(D, probs = c(0.5, 0.75)), max(D) + 1)), by = plot]
  #     removedPlot <- c(removedPlot, unique(data[, .N, by = .(plot, quantile)][N < 3, plot]))
  #     data <- data[!plot %in% removedPlot]
  #
  #     # do the model
  #     model(tryCatch({
  #       modelHD(
  #         D = data$D,
  #         H = data$H,
  #         method = input$rad_HDMOD,
  #         plot = data$plot,
  #         useWeight = T
  #       )
  #     }, error = function(e) NULL, warning = function(e) NULL, message = function(e) NULL))
  #
  #     # if there is a least one plot in the removed plot -> warning message
  #     if (length(removedPlot) != 0) {
  #       shinyalert("Oops", paste(
  #         "Local HD model cannot be built for plot(s):",
  #         paste(removedPlot, collapse = ", "),
  #         "\n either:",
  #         "\n\t - there are not enough local height measurements",
  #         "\n\t - height measurements are likely not representative of tree size distribution"
  #       ), type = "warning")
  #     }
  #   } else {
  #     model(NULL)
  #   }
  # })

  ## Maps + comparison of methods --------------------------------------------------------------------

  # observe({
  #   toggleElement("box_MAP",
  #     condition = any(c("chave", "feld") %in% input$chkgrp_HEIGHT) || (input$sel_LONG != "<unselected>" && input$sel_LAT != "<unselected>")
  #   )
  # })


  # create a layer of borders
  mapWorld <- reactiveVal(borders("world", colour = "gray50", fill = "gray50"))


  # create the table of coordinate and update it when NULL
  coord <- reactiveVal()
  # observe({
  #   if (input$btn_DATASET_LOADED >= 1 && !err) {
  #     coord(data.table(
  #       plot = if (input$sel_PLOT != "<unselected>") inv()[, input$sel_PLOT] else "plot",
  #       longitude = if (input$sel_LONG != "<unselected>") inv()[, input$sel_LONG] else input$num_LONG,
  #       latitude = if (input$sel_LAT != "<unselected>") inv()[, input$sel_LAT] else input$num_LAT
  #     ))
  #   }
  # })

  # observeEvent({
  #   coord()
  #   model()
  #   input$chkgrp_HEIGHT
  #   input$rad_HDMOD
  # }, ignoreNULL = F, ignoreInit = T, {
  #   toggleElement("box_plot_comparison", condition = !is.null(input$chkgrp_HEIGHT))
  #
  #   D <- seq(0, 250)
  #
  #   ### Basic plot for HD-methods comparison
  #   plot <- ggplot(data = NULL, aes(x = D)) +
  #     xlab("Diameter (cm)") +
  #     ylab("Predicted Height (m)") +
  #     theme(
  #       legend.position = "top",
  #       legend.title = element_blank(),
  #       legend.text = element_text(size = rel(1.5)),
  #       axis.title = element_text(size = rel(1.3))
  #     ) +
  #     scale_fill_manual(values = c(HD_local = "blue", Feldpausch = "green", Chave = "red")) +
  #     scale_colour_manual(values = c(HD_local = "blue", Feldpausch = "green", Chave = "red")) +
  #     theme_minimal()
  #
  #
  #   #### Including HD local model in comparison
  #   if (!is.null(model())) {
  #
  #     plot <- plot + if (input$sel_PLOT == "<unselected>") { # if Plot variable has not been provided
  #       geom_line(aes(y = retrieveH(D, model = model())$H, colour = "HD_local"))
  #     } else { # # if Plot variable has been provided
  #       H <- sapply(model(), function(x) {
  #         retrieveH(D, model = x)$H
  #       }) # retrieve all the data of H for the D value for each models
  #       geom_ribbon(aes(
  #         ymin = apply(H, 1, min, na.rm = T),
  #         ymax = apply(H, 1, max, na.rm = T),
  #         fill = "HD_local"
  #       ), alpha = 0.3) # take all the H min and max for each line (each different D)
  #     }
  #   }
  #
  #   # # Create the table of coordinate
  #   # coordinate <- coord()[, .(
  #   #   longitude = mean(longitude, na.rm = T),
  #   #   latitude = mean(latitude, na.rm = T)
  #   # ),
  #   # by = plot
  #   # ]
  #
  #   # remove all NA and take the unique coordinate
  #   # coordinate <- unique(na.omit(coordinate))
  #
  #   # draw the coordinate if there is one remaining
  #   # if (nrow(coordinate) != 0) {
  #   #   output$plot_MAP <- renderPlot({
  #   #     ggplot(coordinate) + xlab("longitude") + ylab("latitude") +
  #   #       mapWorld() +
  #   #       geom_point(aes(x = longitude, y = latitude), color = "red", size = 2) +
  #   #       theme_minimal()
  #   #   })
  #   # }
  #
  #   if ("feld" %in% input$chkgrp_HEIGHT) {
  #     region <- computeFeldRegion(coord()[, cbind(longitude, latitude)])
  #     region[is.na(region)] <- "Pantropical"
  #     output$txt_feld <- renderText({
  #       paste("Feldpausch region(s):", paste(unique(feldRegion()[region]), collapse = ", "))
  #     })
  #     # continuation with the plot whith feld
  #     plot <- plot + if (length(unique(region)) >= 2) {
  #       H <- sapply(region, function(x) {
  #         retrieveH(D, region = x)$H
  #       })
  #       geom_ribbon(aes(
  #         ymin = apply(H, 1, min, na.rm = T),
  #         ymax = apply(H, 1, max, na.rm = T),
  #         fill = "Feldpausch"
  #       ), alpha = 0.3)
  #     } else {
  #       geom_line(aes(y = retrieveH(D, region = unique(region))$H, colour = "Feldpausch"))
  #     }
  #   }
  #
  #   if ("chave" %in% input$chkgrp_HEIGHT) {
  #     E <- tryCatch(computeE(coord()[, cbind(longitude, latitude)]), error = function(e) e)
  #     output$txt_chave <- renderText({
  #       if (!is.list(E)) {
  #         if (length(unique(E)) > 1) {
  #           paste(
  #             "E parameter of Chave et al. (2014):",
  #             paste(round(range(E), digits = 3), collapse = " to ")
  #           )
  #         } else {
  #           paste(
  #             "E parameter of Chave et al. (2014):",
  #             round(unique(E), digits = 3)
  #           )
  #         }
  #       } else {
  #         "E cannot be retrieved for those coordinates"
  #       }
  #     })
  #     # continuation with the plot whith chave
  #     if (!is.list(E)) {
  #       plot <- plot + if (length(unique(E)) >= 2) { # if there is multiple E
  #         geom_ribbon(aes(
  #           ymax = retrieveH(D, coord = coord()[which.min(E), c(longitude, latitude)])$H,
  #           ymin = retrieveH(D, coord = coord()[which.max(E), c(longitude, latitude)])$H,
  #           fill = "Chave"
  #         ), alpha = 0.3)
  #       } else { # if there is just one E
  #         geom_line(aes(y = retrieveH(D, coord = coord()[, c(mean(longitude), mean(latitude))])$H, colour = "Chave"))
  #       }
  #     }
  #   }
  #
  #   if (!is.null(input$chkgrp_HEIGHT)) {
  #     # show the plot of the comparison of the methods
  #     output$out_plot_comp <- renderPlot(plot)
  #   }
  # })






  # AGB ----

  AGB_sum <- reactiveVal(list(), label = "summary by plot")
  observeEvent(input$btn_AGB_DONE, {

    ## Retrieving and checking parameters ----

    # AGB list
    AGB_res <- list()

    # AGB alone or with error propagation
    AGBmod <- input$rad_AGB_MOD

    # Retrieve plot's ID
    if (input$sel_PLOT != "<unselected>") {
      plot_id <- inv()[, input$sel_PLOT]
    } else {
      plot_id <- rep("plot", nrow(inv()))
    }

    multiple_model_loc <- F
    # if there is plot to remove from the dataset
    if (!is.null(model()) && length(model()[[1]]) != 2) {
      multiple_model_loc <- T
    }

    # Retrieve diameters
    D <- inv()[, input$sel_DIAMETER]


    # Retrieve WD and its uncertainties
    if (is.data.frame(wd())) {
      WD <- wd()[, "meanWD"]
      errWD <- wd()[, "sdWD"]
    } else {
      WD <- inv()[, input$sel_WD]
      errWD <- NULL
    }
    if (is.null(errWD) && AGBmod != "agb") {
      shinyalert("WARNING", "Error associated with wood dentity estimates will not be accounted for \n (if you want to, please provide the genus or species at the beginning)",
        type = "warning"
      )
      errWD <- rep(0, length(WD))
    }

    # Retrieve heights
    if (input$sel_H != "<unselected>") {
      H <- inv()[, input$sel_H]
    }

    # Get the number of height estimation's methods
    length_progression <- length(input$chkgrp_HEIGHT)


    # Set method's color
    color <- c(HD_local = "#619CFF", feldpausch = "#F8766D", chave = "#00BA38", height = "black")



    ## Calculation of AGB ----

    withProgress(message = "AGB calculation", value = 0, {
      newValue <- AGB_sum()

      # if we have an HD local model
      if ("HDloc" %in% input$chkgrp_HEIGHT) {
        AGB_res <- AGB_predict(AGBmod, D, WD, errWD, HDmodel = model(), plot = if (multiple_model_loc) plot_id)
        newValue[[names(color)[1]]] <- summaryByPlot(AGB_res, if (!multiple_model_loc) plot_id else plot_id[plot_id %in% names(model())])
        incProgress(1 / length_progression, detail = "AGB using HD local: Done")
      }

      # if we want the feldpausch region
      if ("feld" %in% input$chkgrp_HEIGHT) {
        region <- computeFeldRegion(coord()[, cbind(longitude, latitude)])
        region[is.na(region)] <- "Pantropical"
        AGB_res <- AGB_predict(AGBmod, D, WD, errWD, region = region)
        newValue[[names(color)[2]]] <- summaryByPlot(AGB_res, plot_id)
        incProgress(1 / length_progression, detail = "AGB using Feldpausch region: Done")
      }

      # if we want the chave model
      if ("chave" %in% input$chkgrp_HEIGHT) {
        AGB_res <- AGB_predict(AGBmod, D, WD, errWD, coord = coord()[, cbind(longitude, latitude)])
        newValue[[names(color)[3]]] <- summaryByPlot(AGB_res, plot_id)
        incProgress(1 / length_progression, detail = "AGB using Chave E: Done")
      }

      AGB_sum(newValue)
    })

    ## Render AGB plot's ----
    # plot the output
    output$out_plot_AGB <- renderPlot({
      plot_list(AGB_sum(), color, if (multiple_model_loc) names(model()))
    })

    showElement(id = "box_AGB_res")
    showElement(id = "box_AGB_Report")
  })



  # Download part -----------------------------------------------------------


  ##### download the report
  output$dwl_report <- downloadHandler(
    filename = function() {
      paste0("Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy(system.file("Rmardown", "report_BIOMASS.Rmd", package = "BIOMASSapp"),
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


  #### download the file FOS like
  output$dwl_file <- downloadHandler(
    filename = function() {
      paste0("file_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (file.exists(file)) {
        file.remove(file)
      }

      # create a temporary file to use to copy the file
      tempFile <- tempfile(fileext = ".csv")

      # select the column we need
      selectColumn <- c(
        input$sel_PLOT, input$sel_DIAMETER,
        input$sel_H, input$sel_LONG, input$sel_LAT
      )
      selectedColumn <- selectColumn != "<unselected>"

      # create a database with those data
      data <- as.data.table(inv()[, selectColumn[selectedColumn]])
      setnames(data, names(data), c(
        "plot", "D",
        "H", "longitude", "latitude"
      )[selectedColumn])

      # create the output data
      out <- data.table(
        plot = if ("plot" %in% names(data)) data$plot else rep("plot", nrow(data)),
        longitude = if ("longitude" %in% names(data)) data$longitude else rep(input$num_LONG, nrow(data)),
        latitude = if ("latitude" %in% names(data)) data$latitude else rep(input$num_LAT, nrow(data))
      )

      # select the H we need
      if (is.null(input$chkgrp_HEIGHT)) {
        out[, H := data$H]
      }

      if ("HDloc" %in% input$chkgrp_HEIGHT) {
        model_multi <- length(model()[[1]]) != 2

        if (model_multi) {
          H <- rep(NA_real_, nrow(data))
          index_plot_model <- data[plot %in% names(model()), .I]
          H[index_plot_model] <- data[index_plot_model, retrieveH(D, model = model(), plot = plot)$H]
        } else {
          H <- data[, retrieveH(D, model = model())$H]
        }

        out[, H_local := H]
      }

      if ("feld" %in% input$chkgrp_HEIGHT) {
        out[, H_feld := retrieveH(data$D, region = computeFeldRegion(cbind(longitude, latitude)))$H]
      }

      if ("chave" %in% input$chkgrp_HEIGHT) {
        out[, H_chave := retrieveH(data$D, coord = if (.N != 1) cbind(longitude, latitude) else c(longitude, latitude))$H ]
      }

      # create the Lorey database whith the lorey height
      Lorey <- suppressWarnings(out[, c(1, grep("^H", names(out))), with = F][, ":="(D = data$D, plot = out$plot)])
      Lorey[, BAm := (pi * (D / 2)^2) / 10000]
      Lorey <- Lorey[, lapply(.SD, function(x) {
        sum(x * BAm, na.rm = T) / sum(BAm, na.rm = T)
      }), .SDcols = patterns("^H"), by = plot]
      Lorey[Lorey == 0] <- NA
      setnames(Lorey, names(Lorey), gsub("^H", "LoreyH", names(Lorey)))

      # take the data for reduction by plot
      out <- suppressWarnings(out[, lapply(.SD, max, na.rm = T), .SDcols = patterns("^H"), by = plot][
        out[, .(
          Long_cnt = mean(longitude),
          Lat_cnt = mean(latitude)
        ), by = plot],
        on = "plot"
      ])
      setnames(out, names(out), gsub("^H", "H_max", names(out)))

      # merge the AGB
      for (i in names(AGB_sum())) {
        a <- ncol(out)
        tab <- AGB_sum()[[i]]
        out <- merge(out, setDT(tab), by = "plot", all.x = T)
        name <- names(out)[(a + 1):ncol(out)]
        if (i == "HD_local") {
          i <- "local"
        }
        if (i != "height") {
          setnames(out, name, paste(name, i, sep = "_"))
        }
      }


      # Merge the Lorey table
      out <- out[Lorey, on = "plot"]


      # Few manipulation on the dataset
      setnames(out, "plot", "Plot_ID")
      setnames(out, names(out), sub("^Cred", "AGB_Cred", names(out)))
      setcolorder(out, c("Plot_ID", "Long_cnt", "Lat_cnt"))
      out[out == -Inf] <- NA
      out[out == Inf] <- NA

      # write the file
      fwrite(out, tempFile)

      # copy the file in the file for this purpose
      file.copy(tempFile, file, overwrite = T)
    },
    contentType = "text/csv"
  )
}
