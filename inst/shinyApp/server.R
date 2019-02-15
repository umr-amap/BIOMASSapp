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


  # load dataset ------------------------------------------------------------

  inv <- reactiveVal()
  observeEvent(ignoreInit = T, {
    input$file_DATASET
    input$num_skip_line
  }, {
    # file importation
    if (!is.null(input$file_DATASET)) {
      inv(fread(
        file = input$file_DATASET$datapath,
        skip = ifelse(is.na(input$num_skip_line) || input$num_skip_line == 0, "__auto__", input$num_skip_line),
        data.table = F
      ))

      # show the box
      showElement("box_DATASET")
      showElement("box_FIELDS")

      # show the content
      output$table_DATASET <- renderDataTable(inv())

      int_num_col <- names(inv())[ sapply(inv(), class) %in% c("integer", "numeric") ]

      # fill the selector with the column name
      for (id in c("sel_DIAMETER", "sel_WD", "sel_H", "sel_LONG", "sel_LAT")) {
        updateSelectInput(session, id, choices = c("<unselected>", int_num_col))
      }

      char_col <- names(inv())[ sapply(inv(), class) %in% "character" ]

      for (id in c("sel_GENUS", "sel_SPECIES")) {
        updateSelectInput(session, id, choices = c("<unselected>", char_col))
      }

      name <- names(inv())
      updateSelectInput(session, "sel_PLOT", choices = c("<unselected>", name))
    }
  })

  # If the diameter is unselected => red box
  observeEvent(input$sel_DIAMETER, {
    feedbackDanger("sel_DIAMETER",
      condition = input$sel_DIAMETER == "<unselected>",
      text = "Compulsory argument"
    )
  })

  # If the diameter is unselected => red box
  observeEvent(input$sel_PLOT, {
    feedbackWarning("sel_PLOT",
      condition = input$sel_PLOT == "<unselected>",
      text = "Optional (if you want to get AGB estimates per plot)"
    )
  })


  # error when the user click on the button on the first page
  observeEvent(input$btn_DATASET_LOADED, {
    error <- F
    if (input$sel_DIAMETER == "<unselected>") { # if diameter is not selected
      error <- T
      shinyalert("Oops!", "D is unselected", type = "error")
    } else if (!xor(input$sel_WD == "<unselected>", input$sel_GENUS == "<unselected>")) {
      # if the wd is not selected or genus not selected but not the two
      error <- T
      shinyalert("Oops!", "Please either select the wood density or genus and species or genus", type = "error")
    } else if (xor(input$sel_LONG == "<unselected>", input$sel_LAT == "<unselected>")) {
      # if the H is not selected and one of the two (long or lat) is not selected
      error <- T
      shinyalert("Oops!", "Please either select or deselect both the longitude and latitude", type = "error")
    } else if (input$sel_WD == "<unselected>") {
      # if the WD is not selected then show the tab TAXO
      showMenuItem("tab_TAXO")
      updateTabItems(session, "mnu_MENU", "tab_TAXO")
    } else {
      # else show the height tab
      showMenuItem("tab_HEIGHT")
      updateTabItems(session, "mnu_MENU", "tab_HEIGHT")
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

  # if the wd is not selected or genus not selected but not the two
  observe({
    toggleElement("msg_wd",
      condition =
        !xor(input$sel_WD == "<unselected>", input$sel_GENUS == "<unselected>")
    )
  })

  # for the height long lat
  observe({
    toggleElement("msg_h",
      condition = xor(input$sel_LONG == "<unselected>", input$sel_LAT == "<unselected>")
    )
  })





  # taxonomy ----------------------------------------------------------------

  observeEvent(input$btn_TAXO_RESULT, {
    showElement("box_RESULT_TAXO")
    # show a progress bar
    withProgress(message = "Extracting wood density values", value = 0, {
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
            print("Number of modified taxa names:")
            table(taxo$nameModified)
          })
        }
        # update the progression
        incProgress(1 / 2, detail = "Taxonomy correction completed")
        genus <- taxo$genusCorrected
        species <- taxo$speciesCorrected
      } else {

        # if the users do not choose the coorect taxo
        genus <- inv()[, input$sel_GENUS]
        if (input$sel_SPECIES == "<unselected>") {
          split <- tstrsplit_NA(genus)
          genus <- split[, 1]
          species <- split[, 2]
        } else {
          species <- inv()[, input$sel_SPECIES]
        }
      }
      wd <- tryCatch(getWoodDensity(genus, species), error = function(e) e, warning = function(e) e)

      # if there is an error display it
      if (!is.data.frame(wd)) {
        output$out_wd_error <- renderPrint({
          taxo$message
        })
      } else { # if not a message will appear
        inv(cbind(inv(), wd[, -(1:3)])) # bind + remove family genus and species columns
        output$out_wd_error <- renderPrint({
          print("Taxonomic levels at which wood density was attributed to trees:")
          table(wd$levelWD)
        })
      }
      incProgress(1, detail = "Wood density extraction completed")
    })

    showElement(id = "box_TAXO_DONE")
  })

  # when the taxo is done
  observeEvent(input$btn_TAXO_DONE, {
    if (any(!c("meanWD", "sdWD", "levelWD", "nInd") %in% names(inv()))) { # verify if there is all the column present
      shinyalert("Oops", "Somethings went wrong, please check this", type = "error")
    } else {
      showMenuItem("tab_HEIGHT")
      updateTabItems(session, "mnu_MENU", "tab_HEIGHT")
    }
  })


  # height parameters -------------------------------------------------------
  observe({
    toggleElement("box_RESULT_HDEND", condition = !is.null(input$chkgrp_HEIGHT) || input$sel_H != "<unselected>")
  })


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



  observeEvent(input$chkgrp_HEIGHT, ignoreNULL = F, {
    id <- input$chkgrp_HEIGHT

    ## If they want to construct an HD model
    if ("HDloc" %in% id) {
      if (input$sel_H != "<unselected>") { # if there is H selected

        # Do the HD model
        tab_modelHD <- modelHD(
          D = inv()[, input$sel_DIAMETER],
          H = inv()[, input$sel_H]
        )

        # render the table
        output$out_tab_HD <- renderTable(tab_modelHD[, -2], digits = 4)
        # update the radio button with the method and choose the minimun of the RSE
        with(
          tab_modelHD,
          updateRadioButtons(session,
            inputId = "rad_HDMOD",
            choices = method,
            selected = method[which.min(RSE)],
            inline = T
          )
        )

        # show the box for the result of HDmod
        showElement("box_RESULT_HDMOD")
      } else {
        shinyalert(title = "Oops", text = "Local height measurements have not been provided", type = "error")
        updateCheckboxGroupInput(session, "chkgrp_HEIGHT",
          selected = input$chkgrp_HEIGHT[!input$chkgrp_HEIGHT %in% "HDloc"]
        )
      }
    } else {
      hideElement("box_RESULT_HDMOD")
    }


    long_lat <- input$sel_LONG != "<unselected>" && input$sel_LAT != "<unselected>"

    toggleElement("box_MAP",
      condition = (any(c("feld", "chave") %in% id) || long_lat)
    )

    # if the user command a feldpausch region
    toggleElement("box_RESULT_FELD", condition = "feld" %in% id)


    # if the user command a chave
    toggleElement("box_result_chave", condition = "chave" %in% id)
  })


  observeEvent(input$btn_HD_DONE, {
    if (is.null(input$chkgrp_HEIGHT)) {
      shinyalert("Oops", "Select at least one HD model", type = "error")
    } else {
      showMenuItem("tab_AGB")
      updateTabItems(session, "mnu_MENU", "tab_AGB")
    }
  })





  # HD model local ----------------------------------------------------------------

  model <- reactiveVal()

  observeEvent({
    input$btn_DATASET_LOADED
    if (input$btn_DATASET_LOADED >= 1) {
      input$sel_PLOT
      input$sel_H
      input$sel_DIAMETER
    }
    input$rad_HDMOD
    input$chkgrp_HEIGHT
  }, {
    if (!"HDloc" %in% input$chkgrp_HEIGHT || input$sel_H == "<unselected>" || input$rad_HDMOD == "NULL") {
      model(NULL)
    } else {

      # take the data D, H, plot
      data <- setDT(inv()[, c(input$sel_DIAMETER, input$sel_H)])
      setnames(data, names(data), c("D", "H"))
      data[, plot := if (input$sel_PLOT == "<unselected>") "plot" else inv()[, input$sel_PLOT]]

      # remove all the plots with 15 non NA value
      removedPlot <- unique(data[, .(nbNonNA = sum(!is.na(H))), by = plot][nbNonNA < 15, plot])
      data <- data[!plot %in% removedPlot]

      # remove the plots where there the plot is not really distributed
      data[, quantile := findInterval(D, c(-1, quantile(D, probs = c(0.5, 0.75)), max(D) + 1)), by = plot]
      removedPlot <- c(removedPlot, unique(data[, .N, by = .(plot, quantile)][N < 3, plot]))
      data <- data[!plot %in% removedPlot]

      # do the model
      model(modelHD(
        D = data$D,
        H = data$H,
        method = input$rad_HDMOD,
        plot = data$plot
      ))

      # if there is a least one plot in the removed plot -> warning message
      if (length(removedPlot) != 0) {
        shinyalert("Oops", paste(
          "Local HD model cannot be built for plot(s):",
          paste(removedPlot, collapse = ", "),
          "\n either:",
          "\n\t - there are not enough local height measurements",
          "\n\t - height measurements are likely not representative of tree size distribution"
        ), type = "warning")
      }
    }
  })


  # Maps + comparison of methods --------------------------------------------------------------------

  observe({
    toggleElement("box_MAP",
      condition = any(c("chave", "feld") %in% input$chkgrp_HEIGHT) || (input$sel_LONG != "<unselected>" && input$sel_LAT != "<unselected>")
    )
  })


  observe({
    # show the elements when the condition are complete
    toggleElement("num_LONG",
      condition = (input$sel_LONG == "<unselected>" && input$sel_LAT == "<unselected>")
    )
    toggleElement("num_LAT",
      condition = (input$sel_LONG == "<unselected>" && input$sel_LAT == "<unselected>")
    )
  })


  # create a layer of borders
  mapWorld <- reactiveVal(borders("world", colour = "gray50", fill = "gray50"))


  observeEvent({
    input$btn_DATASET_LOADED
    if (input$btn_DATASET_LOADED >= 1) {
      input$sel_LAT
      input$sel_LONG
      input$num_LAT
      input$num_LONG
    }
    input$chkgrp_HEIGHT
    input$rad_HDMOD
  }, ignoreNULL = F, ignoreInit = T, {
    toggleElement("box_plot_comparison", condition = !is.null(input$chkgrp_HEIGHT))

    D <- seq(0, 250)


    # Plot with the comparison of the method with ggplot
    plot <- ggplot(data = NULL, aes(x = D)) +
      xlab("Diameter (cm)") +
      ylab("Predicted Height (m)") +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3))
      ) +
      scale_fill_manual(values = c(HD_local = "blue", Feldpausch = "green", Chave = "red")) +
      scale_colour_manual(values = c(HD_local = "blue", Feldpausch = "green", Chave = "red"))


    # comparison of the method: comparison with the HD local
    if ("HDloc" %in% input$chkgrp_HEIGHT && input$rad_HDMOD != "NULL") {
      plot <- plot + if (length(model()[[1]]) == 2) {
        geom_line(aes(y = retrieveH(D, model = model())$H, colour = "HD_local"))
      } else {
        H <- sapply(model(), function(x) {
          retrieveH(D, model = x)$H
        })
        geom_ribbon(aes(
          ymin = apply(H, 1, min, na.rm = T),
          ymax = apply(H, 1, max, na.rm = T),
          fill = "HD_local"
        ), alpha = 0.3)
      }
    }

    # Create the table of coordinate
    coord <- data.table(
      longitude = if (input$sel_LONG != "<unselected>") inv()[, input$sel_LONG] else input$num_LONG,
      latitude = if (input$sel_LAT != "<unselected>") inv()[, input$sel_LAT] else input$num_LAT
    )

    # if the user as made a mistake
    if (all(sapply(coord, class) %in% c("numeric", "integer"))) {

      # if the plot are renseigned take the mean of each plot
      if (nrow(coord) == nrow(inv()) && input$sel_PLOT != "<unselected>") {
        coord <- rbindlist(by(
          coord, inv()[, input$sel_PLOT],
          function(x) {
            data.frame(
              longitude = mean(x$longitude),
              latitude = mean(x$latitude)
            )
          }
        ))
      }

      # remove all NA and take the unique coordinate
      coord <- unique(na.omit(coord))

      # draw the coordinate if there is one remaining
      if (nrow(coord) != 0) {
        output$plot_MAP <- renderPlot({
          ggplot(coord) + xlab("longitude") + ylab("latitude") +
            mapWorld() +
            geom_point(aes(x = longitude, y = latitude), color = "red", size = 2)
        })
      }

      if ("feld" %in% input$chkgrp_HEIGHT) {
        region <- unique(computeFeldRegion(coord[, cbind(longitude, latitude)]))
        region[is.na(region)] <- "Pantropical"
        output$txt_feld <- renderText({
          paste("Feldpausch region(s):", paste(unique(feldRegion()[region]), collapse = ", "))
        })
        # continuation with the plot whith feld
        plot <- plot + if (length(region) >= 2) {
          H <- sapply(region, function(x) {
            retrieveH(D, region = x)$H
          })
          geom_ribbon(aes(
            ymin = apply(H, 1, min, na.rm = T),
            ymax = apply(H, 1, max, na.rm = T),
            fill = "Feldpausch"
          ), alpha = 0.3)
        } else {
          geom_line(aes(y = retrieveH(D, region = region)$H, colour = "Feldpausch"))
        }
      }

      if ("chave" %in% input$chkgrp_HEIGHT) {
        E <- tryCatch(computeE(coord), error = function(e) e)
        output$txt_chave <- renderText({
          if (!is.list(E)) {
            if(length(E)>1){
              paste(
              "E parameter of Chave et al. (2014):",
              paste(round(range(E), digits = 3), collapse = " to ")
              )}else{
                paste(
                  "E parameter of Chave et al. (2014):",
                  paste(round(E, digits = 3), collapse = " ")
                )
            }
          } else {
            "E cannot be retrieved for those coordinates"
          }
        })
        # continuation with the plot whith chave
        if (!is.list(E)) {
          plot <- plot + if (length(unique(E)) >= 2) { # if there is multiple E
            geom_ribbon(aes(
              ymax = retrieveH(D, coord = coord[which.min(E), c(longitude, latitude)])$H,
              ymin = retrieveH(D, coord = coord[which.max(E), c(longitude, latitude)])$H,
              fill = "Chave"
            ), alpha = 0.3)
          } else { # if there is just one E
            geom_line(aes(y = retrieveH(D, coord = c(mean(coord$longitude), mean(coord$latitude)))$H, colour = "Chave"))
          }
        }
      }

      if (!is.null(input$chkgrp_HEIGHT)) {
        # show the plot of the comparison of the methods
        output$out_plot_comp <- renderPlot(plot)
      }
    } else {
      shinyalert("Oops", text = "Longitude and latitude must be numeric")
    }
  })









  # AGB section -------------------------------------------------------------


  AGB_sum <- reactiveVal(list())
  observeEvent(input$btn_AGB_DONE, {


    # AGB list
    AGB_res <- list()

    # take the mode of AGB
    AGBmod <- input$rad_AGB_MOD

    # take the plot ID
    if (input$sel_PLOT != "<unselected>" && "HDloc" %in% input$chkgrp_HEIGHT) {
      plot_id <- inv()[, input$sel_PLOT]
    } else {
      plot_id <- rep("plot", nrow(inv()))
    }


    multiple_model_loc <- F
    # if there is plot to remove from the dataset
    if (!is.null(model()) && length(model()[[1]]) != 2) {
      multiple_model_loc <- T
    }

    # take the diameter
    D <- inv()[, input$sel_DIAMETER]



    # WD treatement
    if (all(c("meanWD", "sdWD", "levelWD", "nInd") %in% names(inv()))) {
      WD <- inv()[, "meanWD"]
      errWD <- inv()[, "sdWD"]
    } else {
      WD <- inv()[, input$sel_WD]
      errWD <- NULL
    }


    #### parameters verification
    if (is.null(errWD) && AGBmod != "agb") {
      shinyalert("WARNING", "Error associated with wood dentity estimates will not be accounted for \n (if you want to, please provide the genus or species at the beginning)",
        type = "warning"
      )
      errWD <- rep(0, length(WD))
    }

    # coord treatement
    coord <- data.table(
      longitude = if (input$sel_LONG != "<unselected>") inv()[, input$sel_LONG] else input$num_LONG,
      latitude = if (input$sel_LAT != "<unselected>") inv()[, input$sel_LAT] else input$num_LAT
    )

    # height treatement
    if (input$sel_H != "<unselected>") {
      H <- inv()[, input$sel_H]
    } # if H is unselected

    # take the length of the input of check box for the height
    length_progression <- length(input$chkgrp_HEIGHT)



    # plain color
    # use for names too
    color <- c(HD_local = "blue", feldpausch = "red", chave = "green", height = "black")



    ####### calculation of the AGB

      withProgress(message = "AGB build", value = 0, {
        newValue = AGB_sum()
        # if we have an HD local
        if ("HDloc" %in% input$chkgrp_HEIGHT) {
          AGB_res <- AGB_predict(AGBmod, D, WD, errWD, HDmodel = model(), plot = if (multiple_model_loc) plot_id)
          newValue[[names(color)[1]]] = summaryByPlot(AGB_res, if (!multiple_model_loc) plot_id else plot_id[plot_id %in% names(model())])
          incProgress(1 / length_progression, detail = "AGB using HD local: Done")
        }

        # if we want the feldpausch region
        if ("feld" %in% input$chkgrp_HEIGHT) {
          region <- computeFeldRegion(coord)
          region[is.na(region)] <- "Pantropical"
          AGB_res <- AGB_predict(AGBmod, D, WD, errWD, region = region)
          newValue[[names(color)[2]]] = summaryByPlot(AGB_res, plot_id)
          incProgress(1 / length_progression, detail = "AGB using Feldpausch region: Done")
        }

        # if we want the chave model
        if ("chave" %in% input$chkgrp_HEIGHT) {
          AGB_res <- AGB_predict(AGBmod, D, WD, errWD, coord = coord)
          newValue[[names(color)[3]]] = summaryByPlot(AGB_res, plot_id)
          incProgress(1 / length_progression, detail = "AGB using Chave E: Done")
        }

        AGB_sum(newValue)
      })

    ###### After the calculation of the AGB
      # plot the output
      output$out_plot_AGB <- renderPlot({
        plot_list(AGB_sum(), color, if (multiple_model_loc) names(model()))
      })

    showElement(id = "box_AGB_res")
    showElement(id = "box_AGB_Report")
  })



  # download part -----------------------------------------------------------


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


      browser()
      if ("HDloc" %in% input$chkgrp_HEIGHT) {
        model_multi = length(model()[[1]]) != 2

        if (model_multi){
          H = rep(0, nrow(data))
          index_plot_model = data[plot %in% names(model()), .I]
          H[index_plot_model] = data[index_plot_model, retrieveH(D, model = model(), plot = plot)$H]
        } else {
          H = data[, retrieveH(D, model = model())$H]
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
      Lorey <- out[, c(1, grep("^H", names(out))), with = F][, ":="(D = data$D, plot = out$plot)]
      Lorey[, BAm := (pi * (D / 2)^2) / 10000]
      Lorey <- Lorey[, lapply(.SD, function(x) {
        sum(x * BAm, na.rm = T) / sum(BAm, na.rm = T)
      }), .SDcols = patterns("^H"), by = plot]
      setnames(Lorey, names(Lorey), gsub("^H", "LoreyH", names(Lorey)))

      # take the data for reduction by plot
      out <- out[, lapply(.SD, max, na.rm = T), .SDcols = patterns("^H"), by = plot][
        out[, .(
          Long_cnt = mean(longitude),
          Lat_cnt = mean(latitude)
        ), by = plot],
        on = "plot"
      ]
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

      # write the file
      fwrite(out, tempFile)

      # copy the file in the file for this purpose
      file.copy(tempFile, file, overwrite = T)
    },
    contentType = "text/csv"
  )
}
