#GCO Globalement Attention T/F -> TRUE/FALSE
#GCO Globalement on peut regrouper les req() en début d'observateur/render (notion de prérequis)
#GCO Globalement Attention dans les if c'est || et && qu'il faut utiliser pas | et &
#GCO Globalement Eviter les prints, l'utilisateur interagit avec le navigateur, pas la console
#GCO   en plus ce ne sera pas utilisable en mode server (il n'y a pas de console)
#GCO Globalement Attention pour observer plusieurs input il faut utiliser list() et pas {}
#GCO   un événement est un changement de valeur, la valeur de {a;b;c;d} est d !!!
#GCO Globalement Attention les shinyalert sont déconnectés du flux d'exécution. Elles sont
#GCO   modales à l'écran mais ne bloquent pas l'exécution
#GCO Globalement Je ne comprends pas la logique de l'implémentation. Le file de la fonction
#GCO   content = function(file) est le nom d'un fichier temporaire prêt à recevoir ce qu'il
#GCO   faut renvoyer. Pourquoi créer un autre fichier temporaire pour finalement le copier dedans ?
#GCO Au cours de la production du rapport il ne trouve pas les champs longitude et latitude

function(input, output, session) {

  #GCO version compatible local/server
  autoCloseApp()

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

    #GCO ça ne devrait pas être là ! Eviter d'imbriquer les blocs réactifs
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
  })

  # Plot's IDs selection when several plots
  observeEvent({
    input$rad_several_plots
    input$file_DATASET
  },{
    toggleElement("sel_PLOT", condition = req(input$rad_several_plots) == "several_plots")
    updateSelectInput(session, "sel_PLOT", choices = c("<unselected>", names(inv())))
  })

  # Hide/Show "information required" message
  observe(
    if( (req(input$rad_several_plots) == "single_plot") | (req(input$rad_several_plots) =="several_plots" & ! req(input$sel_PLOT) %in% c("<unselected>","")) ) {
      hideElement("msg_several_plots")
    } else {
      showElement("msg_several_plots")
    }
  )

  # Set the plot column that contains plotID
  observe(
    if(!is.null(inv())) {
      inv_plot <- inv()
      if(req(input$rad_several_plots) == "several_plots" && req(input$sel_PLOT) != "<unselected>" ) {
        inv_plot$plot <- as.character(inv_plot[,input$sel_PLOT])
        inv(inv_plot)
      } else if (req(input$rad_several_plots) == "single_plot") {
        inv_plot$plot <- ""
        inv(inv_plot)
      }
    }
  )

  ## WD ----
  observeEvent(input$sel_WD, {
    toggleElement("id_set_errWD",
                  condition = input$sel_WD != "<unselected>")
  })

  ## Height ----
  ### height radio button actions ----
  observeEvent(input$rad_height, {
    int_num_col <- names(inv())[ sapply(inv(), class) %in% c("integer", "numeric")]
    updateSelectInput(session, "sel_H", choices = c("<unselected>", int_num_col))
    updateSelectInput(session, "sel_HDmodel_by", choices = c("<unselected>", names(inv())))
    toggleElement("id_sel_h",
                  condition = input$rad_height %in% c("h_each_tree","h_some_tree"))
    toggleElement("id_set_errH",
                  condition = input$rad_height == "h_each_tree")
    toggleElement("id_sel_HDmodel_by", # stand-specific local HD models only if height of some tree in the same dataset and several plots
                  condition = (input$rad_height == "h_some_tree" & input$rad_several_plots == "several_plots") )
    toggleElement("id_file_h_sup",
                  condition = input$rad_height == "h_sup_data")
  })
  ### height file actions ----
  rv_h_sup <- reactiveVal(label = "data frame")
  observeEvent(input$file_h_sup, {
    # Read HD supplementary dataset
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
  rv_coord <- reactiveVal(label = "data frame")
  observeEvent({
    input$file_coord
    input$rad_several_plots}, {
      # Read plot coordinates upload
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
  #GCO pourquoi utiliser une variable réactive pour error ? Tout tes test sont mutuellement exclusifs
  #GCO if else if else if else ....
  #GCO je lui aurais donné un nom plus parlant (had_error, error_occured, any_error, ...)
  error <- reactiveVal()
  observeEvent(input$btn_DATASET_LOADED, {
    print("Reaction to btn_DATASET_LOADED -> error management")
    error(F)
    if ( is.null(input$file_DATASET) ) {
      # if no dataset is provided
      error(T)
      shinyalert("Oops!", "You need to load a forest inventory file !", type = "error")
    } else if ( is.null(input$rad_several_plots) ) {
      # if the number of plot is not provide
      error(T)
      shinyalert("Oops!", "You need to answer to the question : Does your dataset contain several plots?", type = "error")
    } else if (input$rad_several_plots =="several_plots" && input$sel_PLOT == "<unselected>" ) {
      # if the column corresponding to the plot IDs is unselected
      error(T)
      shinyalert("Oops!", "The column containing the plots IDs is unselected", type = "error")
    } else if (input$sel_DIAMETER == "<unselected>") { # if diameter is not selected
      error(T)
      shinyalert("Oops!", "D is unselected", type = "error")
    } else if (!xor(input$sel_WD == "<unselected>", input$sel_GENUS == "<unselected>")) {
      # if the wd is not selected or genus not selected but not the two
      error(T)
      shinyalert("Oops!", "To estimate the Above Ground Biomass, you either need the wood density or the taxonomy of the trees", type = "error")
    } else if (is.null(input$rad_height)) {
      # if the H radio button has not been ticked
      error(T)
      shinyalert("Oops!", "Height information has not been provided", type = "error")
    } else if ( input$rad_height %in% c("h_each_tree","h_some_tree") && input$sel_H == "<unselected>" ) {
      # if the H column is unselected
      error(T)
      shinyalert("Oops!", "Height column is unselected", type = "error")
    } else if ( input$rad_height == "h_sup_data" && is.null(input$file_h_sup)){
      # if the H-D relationship is in another dataset which have not been loaded
      error(T)
      shinyalert("Oops!", "The dataset containing a subset of well-measured trees has not been loaded", type = "error")
    } else if ( input$rad_height == "h_sup_data" && (input$sel_H_sup_data == "<unselected>" | input$sel_D_sup_data == "<unselected>") ){
      # if the H or D column (of the sup dataset containing H-D relationship) are unselected
      error(T)
      shinyalert("Oops!", "Diameter and/or Height column(s) of the dataset containing a subset of well-measured trees is/are unselected ", type = "error")
    } else if (input$rad_height == "h_none" && (is.null(input$rad_coord) || input$rad_coord %in% c("","coord_none"))  ) {
      # if no height measurements and no coordinates
      error(T)
      shinyalert("Oops!", "To estimate tree heights, you either need a subset of well-measured trees or the coordinates of the plots", type = "error")
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_each_tree" & (input$sel_LONG == "<unselected>" | input$sel_LAT == "<unselected>")) {
      # if the coordinates of each tree is ticked but one of the two (long or lat) is not selected
      error(T)
      shinyalert("Oops!", "Tree's longitude and/or latitude is/are unselected", type = "error")
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_plot" & is.null(input$file_coord) ) {
      # if the coordinates of the plots (in another dataset) is ticked but the dataset has not been loaded
      error(T)
      shinyalert("Oops!", "The dataset containing the coordinates of the plot(s) has not been loaded", type = "error")
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_plot" & (input$sel_LAT_sup_coord == "<unselected>" | input$sel_LAT_sup_coord == "<unselected>") ) {
      # if the Latitude or Longitude column (of the sup dataset containing plot's coordinates) are unselected
      error(T)
      shinyalert("Oops!", "Latitude and/or Longitude column(s) of the dataset containing the coordinates of the plot(s) is/are unselected ", type = "error")
    } else if (!is.null(input$rad_coord) && input$rad_coord == "coord_plot" & input$rad_several_plots == "several_plots" & input$sel_plot_coord == "<unselected>") {
      # if the plots IDs column for sup coord is unselected
      error(T)
      shinyalert("Oops!", "Plots IDs of the dataset containing the coordinates of the plot(s) is unselected ", type = "error")
    } else if (input$sel_WD == "<unselected>") {
      # if the WD is not selected then show the tab TAXO
      updateCheckboxGroupInput(session, inputId = "chkgrp_HEIGHT", #reset HD models
                               selected = character(0) )
      showMenuItem("tab_TAXO")
      updateTabItems(session, "mnu_MENU", "tab_TAXO")
    } else { # If no error and WD already exists
      updateCheckboxGroupInput(session, inputId = "chkgrp_HEIGHT", #reset HD models
                               selected = character(0) )
      if (input$rad_height == "h_each_tree") { # if heights are not to estimate
        showMenuItem("tab_AGB")
        updateTabItems(session, "mnu_MENU", "tab_AGB")
      } else {
        # else show the height tab
        showMenuItem("tab_HEIGHT")
        updateTabItems(session, "mnu_MENU", "tab_HEIGHT")
      }
    }

    if (!error()) {
      newData <- inv()
      if (input$sel_DIAMETER != "<unselected>") {
        newData[, var := conv_unit(var, input$rad_units_diameter, "cm"), env=list(var=input$sel_DIAMETER)]
      }
      if (input$sel_H != "<unselected>") {
        newData[, var := conv_unit(var, input$rad_units_diameter, "m"), env=list(var=input$sel_H)]
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
  # if the wd or genus not selected (but not both)
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
            species = if (input$sel_SPECIES != "<unselected>") inv()[, input$sel_SPECIES],
            useCache = TRUE
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
            taxo_display <- factor(taxo$nameModified,
                                   levels = c("FALSE","SpNotFound","TaxaNotFound","TRUE"),
                                   labels = c("Correct spelling of taxa (unmodified)","Species not found (unmodified)","Taxa not found (unmodified)","Taxa found and corrected (modified)"))
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
    if (!is.data.frame(wd())) {
      shinyalert("Oops", "Somethings went wrong, please check this", type = "error")
    } else {
      if (input$rad_height == "h_each_tree") { # if heights are not to estimate
        showMenuItem("tab_AGB")
        updateTabItems(session, "mnu_MENU", "tab_AGB")
      } else {
        # else show the height tab
        showMenuItem("tab_HEIGHT")
        updateTabItems(session, "mnu_MENU", "tab_HEIGHT")
      }
    }
  })


  # HEIGHT ---------------------------------------------------------------------

  ## HD local models -----------------------------------------------------------
  hd_data <- reactiveVal(label = "HD data")

  observeEvent({
    input$chkgrp_HEIGHT
    # List of event that can modify data and affecting model fitting:
    input$sel_DIAMETER
    input$sel_H
    input$sel_D_sup_data
    input$sel_H_sup_data
    input$sel_HDmodel_by
  }, ignoreNULL = F, ignoreInit = T, {

    if ("HDloc" %in% input$chkgrp_HEIGHT) {

      print("HD local models --------")

      if(input$sel_H == "<unselected>" & (input$sel_H_sup_data == "<unselected>" | is.null(input$sel_H_sup_data))) return() #if the height column isn't selected (possible if the user go back to the 'Load dataset' Item)
      if (input$rad_height == "h_none") { # if no height at all, not possible
        shinyalert("Oops", "Local HD model cannot be build if no height measurements have been provided.", type = "error")
        updateCheckboxGroupInput(session, inputId = "chkgrp_HEIGHT",
                                 selected = input$chkgrp_HEIGHT[input$chkgrp_HEIGHT != "HDloc"] )
        return()
      }

      ### Formatting hd_data which will be used to build the models ------------

      if (input$rad_height == "h_some_tree") { # if height of some trees in the same dataset

        hd_data(setDT(inv()[, c(input$sel_DIAMETER, input$sel_H)]))

        if(input$sel_HDmodel_by != "<unselected>"){ # if one model by plot or region or whatever
          hd_data()[, model_for := as.character(inv()[[input$sel_HDmodel_by]])]
          new_hd_data <- hd_data()
          # remove all the plots with less than 15 non NA value
          removedPlot <- unique(new_hd_data[, .(nbNonNA = sum(!is.na(H))), by = model_for][nbNonNA < 15, model_for])
          new_hd_data <- new_hd_data[!model_for %in% removedPlot]
          # remove plots for which D is not well distributed --> WHY ???
          new_hd_data <- new_hd_data[, quantile := findInterval(D, c(-1, quantile(D, probs = c(0.5, 0.75)), max(D) + 1)), by = model_for]
          removedPlot <- c(removedPlot, unique(new_hd_data[, .N, by = .(model_for, quantile)][N < 3, model_for]))
          hd_data(new_hd_data[!model_for %in% removedPlot])

          # if there is a least one plot in the removed plot -> warning message
          if (length(removedPlot) != 0) {
            shinyalert("Be carefull !", paste(
              "Local HD model cannot be built for:",
              paste(removedPlot, collapse = ", "),
              "\n either:",
              "\n\t - there are not enough local height measurements",
              "\n\t - height measurements are likely not representative of tree size distribution",
              "\n You may have to build a single model grouping all the plots together by unselecting corresponding column."
            ), type = "warning")
          }
        } else { # if one model for the whole dataset
          hd_data()[, model_for := 'all']
        }
      }

      if (input$rad_height == "h_sup_data") { # if height in another dataset
        print("Est-ce que ça recréé bien la colonne model_for ???")
        hd_data(setDT(rv_h_sup()[, c(req(input$sel_D_sup_data), req(input$sel_H_sup_data))]))
        hd_data()[, model_for := 'all']
        print("hd_data()")
        print(head(hd_data()))
      }

      # Setting D and H column names of hd_data()
      setnames(hd_data(), names(hd_data())[1:2], c("D", "H"))

      ## Building and compare the 4 local HD models ----------------------------
      tab_modelHD <- modelHD(
        D = hd_data()$D,
        H = hd_data()$H,
        plot = hd_data()$model_for
      )

      # render the table
      if (input$sel_HDmodel_by == "<unselected>"){
        output$out_tab_HD <- renderTable(tab_modelHD[, -3], digits = 4)
      } else { # If one model per plot/region/whatever, compute the mean of RMSE and Average_bias over all models
        tab_modelHD <- do.call(rbind,tab_modelHD)
        tab_modelHD <- data.table(tab_modelHD[,-3])
        tab_modelHD <- tab_modelHD[, lapply(.SD, mean) , by = method]
        output$out_tab_HD <- renderTable(tab_modelHD, digits = 4)
      }

      # update the radio button with the method and choose the minimum of the RSE
      print("Updating radio button for HD model")
      updateRadioButtons(session,
                         inputId = "rad_HDMOD",
                         choices = tab_modelHD$method,
                         selected = tab_modelHD$method[which.min(tab_modelHD$RSE)],
                         inline = T)

      ### Show the box containing the result of hd_model
      showElement("box_RESULT_HDMOD")

    } else {
      hideElement("box_RESULT_HDMOD")
    }
  })

  ### Building lowest RMSE local HD model or the one chosen by the user --------
  hd_model <- reactiveVal(label = "HD lowest RSE local model")
  observeEvent(input$rad_HDMOD, ignoreNULL = T, ignoreInit = T, {
    ######## Problem: I thought that the following code would be executed when updateRadioButtons(session,inputId = "rad_HDMOD") was called, but no... Guillaume ??
    print("Building HD lowest RSE local model")
    hd_model(tryCatch({
      modelHD(
        D = hd_data()$D,
        H = hd_data()$H,
        method = input$rad_HDMOD,
        plot = hd_data()$model_for,
        useWeight = T
      )
    }, error = function(e) NULL, warning = function(e) NULL, message = function(e) NULL))
  })


  ## Formatting coordinates for Feldpausch and Chave methods -------------------

  coord <- reactiveVal() # data.table containing the coordinates of each tree used in AGB item and the plotID
  coord_plot <- reactiveVal() # data.table containing the unique coordinates of the plot(s) used in height-diameter item (to display height predictions by plot/region)

  observeEvent(
    input$btn_DATASET_LOADED,
    ignoreInit = TRUE, {

      if(!error() && req(input$rad_coord != "coord_none")) {

        print("Reaction to input$btn_DATASET_LOADED for formatting coordinates dataset ")

        # If coordinates of each trees:
        if(req(input$rad_coord) == "coord_each_tree") {
          coord(setDT(inv()[, c(input$sel_LONG, input$sel_LAT,"plot")])) # coord is simply the inventory dataset
          setnames(coord(), c(input$sel_LONG, input$sel_LAT), c("long","lat"))
          coord_plot(coord()) # coord_plot will be averaging by plot above
        }

        # If plot's coordinates in another dataset:
        if(req(input$rad_coord) == "coord_plot") {
          coord_plot(setDT(rv_coord()[, c(input$sel_LONG_sup_coord, input$sel_LAT_sup_coord)])) # coord_plot is simply the coordinates sup dataset
          setnames(coord_plot(), names(coord_plot()), c("long","lat"))
          if(input$rad_several_plots == "several_plots") {
            coord_plot()[, plot := as.character(rv_coord()[,input$sel_plot_coord])]
          } else {
            coord_plot()[, plot := ""]
          }
        }

        # get the median coordinates of each/the plot
        coord_plot(coord_plot()[, .(long = median(long, na.rm = T), lat = median(lat, na.rm = T)), by = plot])

        # remove all NA and take the unique coordinates
        coord_plot(unique(na.omit(coord_plot())))

        if(req(input$rad_coord) == "coord_plot") {
          coord(merge(inv() , coord_plot())[c("long","lat","plot")])
        }
      }
    }

  )



  ## Feldpausch method -------

  region <- reactiveVal() # a data-frame containing 2 columns : plot and Feldpausch's region

  feld_already_ticked <- reactiveVal(F) # in order to not re-execute the code when another box is ticked (HDlocal or Chave)

  observeEvent( {
    input$chkgrp_HEIGHT
    input$btn_DATASET_LOADED # if the user go back to the Load dataset item
  },
  ignoreNULL = FALSE, ignoreInit = TRUE, {

    if ("feld" %in% input$chkgrp_HEIGHT) {
      print("Observing chkrgrp_HEIGHT for Feldpausch/Chave method")
      print(paste("feld_already_ticked: ", feld_already_ticked()))
      print(paste("chave_already_ticked: ", chave_already_ticked()))

      # Skip if Feldpausch/Chave button were already ticked
      if( feld_already_ticked() ) { # if Feldpausch was already ticked
        return() # don't do anything
      } else {

        print("Feldpausch method ! ")

        feld_already_ticked(T)

        # If there is no coordinates: error
        if(is.null(input$rad_coord) || input$rad_coord == "coord_none") {
          shinyalert("Oops", "You need to provide tree's or plot's coordinates to access to region-specific model proposed by Feldpausch et al. (2012)", type = "error")
          updateCheckboxGroupInput(session, inputId = "chkgrp_HEIGHT",
                                   selected = input$chkgrp_HEIGHT[input$chkgrp_HEIGHT != "feld"] )
          return()
        }

        ### Compute Feldspausch regions for coord_plot()
        region(
          data.frame(
            plot = as.character(isolate(coord_plot()[["plot"]])),
            feld_region = computeFeldRegion(isolate(coord_plot()[, c("long", "lat")]))
          )
        )
        # Advert user if some coordinates don't fall in Feldpausch regions
        if( "Pantropical" %in% region()$feld_region) {
          shinyalert("Mhhh",
                     text = paste(
                       ifelse(nrow(region())==1,
                              yes = "The plot doesn't",
                              no = paste(c("Plots",isolate(coord_plot()$plot[region()$feld_region!="Pantropical"]),"don't"), collapse=" ")),
                       "fall within the regions defined in Feldpausch et al. (2012), the whole pantropical region will be used."),
                     type = "warning")
        }

        # print the list of regions
        render_region <- region()
        names(render_region)[2] <- "Feldpausch region"
        output$out_tab_feld <- renderTable(render_region, digits = 4)

      }
      showElement("box_RESULT_FELD")

    } else { # if "feld" not in input$chkgrp_HEIGHT
      feld_already_ticked(F)
      hideElement("box_RESULT_FELD")
    }
  })


  ## Chave method -------
  # create a layer of borders
  mapWorld <- reactiveVal(borders("world", colour = "gray50", fill = "gray50"))
  E <- reactiveVal() # a vector of E parameters for each plot
  chave_already_ticked <- reactiveVal(F) # in order to not re-execute the code when another box is ticked (HDlocal or Feldpausch)

  observeEvent( {
    input$chkgrp_HEIGHT
    input$btn_DATASET_LOADED # if the user go back to the Load dataset item
  },
  ignoreNULL = FALSE, ignoreInit = TRUE, {

    showElement("box_result_chave")

    if ("chave" %in% input$chkgrp_HEIGHT) {

      if( chave_already_ticked() ) { # if Chave was already ticked
        return() # don't do anything
      } else {

        chave_already_ticked(T)

        output$plot_MAP <- renderLeaflet({
          addCircleMarkers(addProviderTiles(leaflet(data = coord_plot()), "OpenStreetMap.Mapnik"), lng = ~long, lat = ~lat, color = "#10A836")
          })

        showElement("box_MAP")

        # Compute bioclimatic parameter E for each plot
        E(tryCatch(computeE(coord_plot()[, c("long", "lat")]), error = function(e) e))

        print("E() :")
        print(E())

        # print "E parameter of Chave et al. (2014): ..."
        output$txt_chave <- renderText({
          if (!is.list(E())) {
            if (length(unique(E())) > 1) {
              paste(
                "E parameter of Chave et al. (2014):",
                paste(round(range(E()), digits = 3), collapse = " to ")
              )
            } else {
              paste(
                "E parameter of Chave et al. (2014):",
                round(unique(E()), digits = 3)
              )
            }
          } else {
            "E cannot be retrieved for those coordinates"
          }
        })
      }
    } else { # if "Chave" not in input$chkgrp_HEIGHT
      chave_already_ticked(F)
      hideElement("box_result_chave")
      hideElement("box_MAP")
    }
  })


  ## Plotting height predictions --------------
  observeEvent({
    coord()
    hd_model()
    input$chkgrp_HEIGHT
    input$rad_HDMOD
    input$btn_DATASET_LOADED
  }, ignoreNULL = T, ignoreInit = T, {
    print("Creating basic plot")
    toggleElement("box_plot_comparison", condition = !is.null(input$chkgrp_HEIGHT))

    D <- seq(1, max( c( inv()$D, hd_data()$D) ) )

    ### Basic plot for HD-methods comparison ----
    plot <- ggplot(data = NULL, aes(x = D)) +
      xlab("Diameter (cm)") +
      ylab("Height (m)") +
      #scale_fill_manual(name = "model confidence interval", values = c("local HD model" = "#619CFF", Feldpausch = "#00BA38", Chave = "#F8766D")) +
      scale_colour_manual(name = "model predictions", values = c("local HD model" = "#619CFF", Feldpausch = "#00BA38", Chave = "#F8766D")) +
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


    ## Including HD local model in comparison ----
    if ("HDloc" %in% input$chkgrp_HEIGHT) {
      if ( !is.null(hd_model()) ) {
        if (input$sel_HDmodel_by == "<unselected>"){
          plot <- plot +
            geom_point(data = hd_data(), mapping = aes(x = D, y = H), size=1.5) + # measured trees
            geom_line(aes(y = retrieveH(D, model = hd_model())$H, colour = "local HD model"), lwd=1.2) + # HD model
            ylim( c(0, max(hd_data()$H, na.rm = T) + 5))
        } else {
          # creating model lines for each plots/regions/...
          df_lines <- do.call(rbind, lapply(names(hd_model()), function(x) {
            data.frame( D = D,
                        H_pred = retrieveH(D, model = hd_model()[[x]])$H,
                        model_for = x)
          }))
          plot <- plot +
            geom_point(data = hd_data(), mapping = aes(x = D, y = H, shape = model_for), size=1.5) + # measured trees
            geom_line(data = df_lines, mapping = aes(x = D, y = H_pred, colour = "local HD model", lty = model_for), lwd=1.2) + #HD model for each plots/regions..
            ylim( c(0, max(hd_data()$H, na.rm = T) + 5))
        }
      }
    }


    ## Including Feldpausch's model in comparison ----
    if ("feld" %in% input$chkgrp_HEIGHT) {
      # creating model prediction lines for each plots/regions/...
      df_lines <- do.call(rbind, lapply(unique(region()$feld_region), function(x) {
        data.frame( D = D,
                    H_pred = retrieveH(D, region = x)$H,
                    model_for = x)
      }))
      if(length(unique(df_lines$model_for)) == 1 ) { # if one single region
        plot <- plot + geom_line(data = df_lines, mapping = aes(x = D, y = H_pred, colour = "Feldpausch"), lwd=1.2)
      } else {
        plot <- plot + geom_line(data = df_lines, mapping = aes(x = D, y = H_pred, colour = "Feldpausch", lty=model_for), lwd=1.2)
      }
    }

    ## Including Chave's model in comparison ----
    if ("chave" %in% input$chkgrp_HEIGHT) {

      # creating model prediction lines for each plots/regions/...
      if (!is.list(E())) { # E is a list if an error was caught
        df_lines <- do.call(rbind, lapply(1:length(E()), function(x) {
          logD <- log(D)
          logH <- 0.893 - E()[x] + 0.760 * logD - 0.0340 * I(logD^2) # eq 6a Chave et al. 2014
          RSE <- 0.243
          data.frame(
            D = D,
            H_pred = as.numeric(exp(logH + 0.5 * RSE^2)),
            model_for = as.character(coord_plot()[x,"plot"])
          )
        }))

        plot <- plot +
          geom_line(data = df_lines, mapping = aes(x=D, y=H_pred, colour = "Chave", lty = model_for), lwd=1.2)
      }
    }

    #if (!is.null(input$chkgrp_HEIGHT)) {
      # show the plot of the comparison of the methods
      output$out_plot_comp <- renderPlot(plot)
    #}
  })

  ## Done button actions -------------------------------------------------------
  observe({
    toggle("btn_HD_DONE", condition = !is.null(input$chkgrp_HEIGHT))
  })
  observeEvent(input$btn_HD_DONE, {
    if (is.null(input$chkgrp_HEIGHT)) {
      shinyalert("Oops", "Select at least one HD model", type = "error")
    } else {
      showMenuItem("tab_AGB")
      updateTabItems(session, "mnu_MENU", "tab_AGB")
    }
  })



  # AGB ----

  AGB_sum <- reactiveVal(list(), label = "summary by plot")
  observeEvent(input$btn_AGB_DONE, {

    print("Reaction to 'Go on' button for AGB calculation")

    ## Retrieving and checking parameters ----

    # AGB only or with error propagation
    AGBmod <- input$rad_AGB_MOD

    # Retrieve diameters
    D <- inv()[, input$sel_DIAMETER]

    # Retrieve WD and its uncertainties
    if (is.data.frame(wd())) {
      WD <- wd()[, "meanWD"]
      errWD <- wd()[, "sdWD"]
    } else {
      WD <- inv()[, input$sel_WD]
      errWD <- rep(input$set_errWD, length(WD))
    }

    # Retrieve heights (and its uncertainties if heights provided by the user)
    if (input$sel_H != "<unselected>") {
      H <- inv()[, input$sel_H]
    }
    if(input$rad_height == "h_each_tree") {
      errH = conv_unit(input$set_errH, from = input$rad_units_height, to = "m")
    }

    # Get the number of height estimation's methods
    length_progression <- length(input$chkgrp_HEIGHT)


    # Set method's color
    color <- c("local HD model" = "#619CFF", Feldpausch = "#F8766D", Chave = "#00BA38", height = "black")

    # Calculation of AGB ----

    withProgress(message = "AGB calculation", value = 0, {
      newValue <- list()

      print("input$sel_HDmodel_by :")
      print(input$sel_HDmodel_by)

      ## Heights provided by the user ----
      if(input$rad_height == "h_each_tree") {
        print("AGB calculation for user's heights: ")
        if( sum(is.na(H)) != 0 ) {
          shinyalert("There is some NA values in given heights. For those trees the function will return NA AGB,
          you may construct a height-diameter model to overcome that issue.",
                     type = "warning")
        }
        AGB_res <- AGB_predict(AGBmod, D = D, WD = WD, errWD = errWD, H = H, errH = errH)
        newValue[[names(color)[4]]] <- summaryByPlot(AGB_res, plot = inv()[["plot"]] )
      }

      ## Heights from HD local model ----
      if ("HDloc" %in% input$chkgrp_HEIGHT) {

        print("AGB calculation for HD local model: ")

        if( input$sel_HDmodel_by != "<unselected>" ) { # if stand-specific models
          AGB_res <- AGB_predict(AGBmod, D = hd_data()$D,
                                 WD = WD[inv()$plot %in% hd_data()$model_for],
                                 errWD = errWD[inv()$plot %in% hd_data()$model_for],
                                 HDmodel = hd_model(), model_by =  hd_data()$model_for)
          newValue[[names(color)[1]]] <- summaryByPlot(AGB_res, plot = hd_data()$model_for )
        } else {
          AGB_res <- AGB_predict(AGBmod, D, WD, errWD, HDmodel = hd_model())
          newValue[[names(color)[1]]] <- summaryByPlot(AGB_res, plot = inv()[["plot"]])
        }

        incProgress(1 / length_progression, detail = "AGB using HD local: Done")
        print("AGB using HD local: Done")
      }

      ## Heights from Feldpausch model ----
      if ("feld" %in% input$chkgrp_HEIGHT) {
        print("AGB calculation for Feldpausch method: ")
        AGB_res <- AGB_predict(AGBmod, D, WD, errWD, region = region()[match(inv()[["plot"]] , table = region()$plot) , "feld_region"])
        newValue[[names(color)[2]]] <- summaryByPlot(AGB_val = AGB_res, plot = inv()[["plot"]])
        incProgress(1 / length_progression, detail = "AGB using Feldpausch region: Done")
      }

      # Heights from Chave model ----
      if ("chave" %in% input$chkgrp_HEIGHT) {
        if (AGBmod == "agb") {
          df_E <- data.frame(plot = coord_plot()$plot, E = E())
          AGB_res <- AGB_predict(AGBmod, D, WD, errWD, E_vec = df_E[match(inv()[["plot"]] , table = df_E$plot) , "E"])
        }
        if (AGBmod == "agbe") {
          AGB_res <- AGB_predict(AGBmod, D, WD, errWD, coord = coord()[c("long","lat")])
        }

        newValue[[names(color)[3]]] <- summaryByPlot(AGB_res, inv()[["plot"]])
        incProgress(1 / length_progression, detail = "AGB using Chave E: Done")
      }

      AGB_sum(newValue)
    })

    ## Render AGB plot's ----
    # plot the output
    output$out_plot_AGB <- renderPlot({
      plot_list(AGB_sum(), color, AGBmod = AGBmod)
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
        if (i == "local HD model") {
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
