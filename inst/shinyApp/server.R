function(input, output, session) {

  # stop the serveur in the end of the session
  session$onSessionEnded(function() {
    stopApp()
  })

  observe({
    # hide few menu at the begining
    hideMenuItem("tab_TAXO")
    hideMenuItem("tab_HEIGHT")
    hideMenuItem("tab_MAP")
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

      selectionField <- c("sel_DIAMETER", "sel_PLOT", "sel_WD", "sel_GENUS", "sel_SPECIES", "sel_H", "sel_LONG", "sel_LAT")
      # fill the selector with the column name
      for (id in selectionField) {
        updateSelectInput(session, id, choices = c("<unselected>", names(inv())))
      }
    }
  })

  # If the diameter is unselected => red box
  observeEvent(input$sel_DIAMETER, {
    feedbackDanger("sel_DIAMETER",
      condition = input$sel_DIAMETER == "<unselected>",
      text = "argument obligatory"
    )
  })

  # If the diameter is unselected => red box
  observeEvent(input$sel_PLOT, {
    feedbackWarning("sel_PLOT",
      condition = input$sel_PLOT == "<unselected>",
      text = "Optional (use if you want to compute AGB per plot)"
    )
  })

  # error when the user click on the button on the first page
  observeEvent(input$btn_DATASET_LOADED, {
    if (input$sel_DIAMETER == "<unselected>") { # if diameter is not selected
      shinyalert("Oops!", "D is unselected", type = "error")
    } else if (!xor(input$sel_WD == "<unselected>", input$sel_GENUS == "<unselected>")) {
      # if the wd is not selected or genus not selected but not the two
      shinyalert("Oops!", "Either select the wood density or genus and species or genus", type = "error")
    } else if (xor(input$sel_LONG == "<unselected>", input$sel_LAT == "<unselected>")) {
      # if the H is not selected and one of the two (long or lat) is not selected
      shinyalert("Oops!", "Please either select or deselect the longitude and latitude", type = "error")
    } else if (input$sel_WD == "<unselected>") {
      # if the WD is not selected then show the tab TAXO
      showMenuItem("tab_TAXO")
      updateTabItems(session, "mnu_MENU", "tab_TAXO")
    } else {
      # else show the heigth tab
      showMenuItem("tab_HEIGHT")
      updateTabItems(session, "mnu_MENU", "tab_HEIGHT")
    }
  })

  # if the wd is not selected or genus not selected but not the two
  observe({
    toggleElement("msg_wd",
      condition =
        !xor(input$sel_WD == "<unselected>", input$sel_GENUS == "<unselected>")
    )
  })

  # for the Heigth long lat
  observe({
    toggleElement("msg_h",
      condition = xor(input$sel_LONG == "<unselected>", input$sel_LAT == "<unselected>")
    )
  })





  # taxonomy ----------------------------------------------------------------

  observeEvent(input$btn_TAXO_RESULT, {
    showElement("box_RESULT_TAXO")
    # show a progress bar
    withProgress(message = "Searching the wood density", value = 0, {
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
            print("How much name has been modified:")
            table(taxo$nameModified)
          })
        }
        # update the progression
        incProgress(1 / 2, detail = "Correct the taxonomy completed")
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
          print("How much tree level at which wood density has been calculated:")
          table(wd$levelWD)
        })
      }
      incProgress(1, detail = "Get the wood density completed")
    })

    showElement(id = "box_TAXO_DONE")
  })

  # when the taxo is done
  observeEvent(input$btn_TAXO_DONE, {
    if (any(!c("meanWD", "sdWD", "levelWD", "nInd") %in% names(inv()))) { # verify if there is all the column present
      shinyalert("Oops", "Somethings went wrong with the function, please check this", type = "error")
    } else {
      showMenuItem("tab_HEIGHT")
      updateTabItems(session, "mnu_MENU", "tab_HEIGHT")
    }
  })


  # Heigth parameters -------------------------------------------------------
  observeEvent(input$sel_H, {
    if (input$sel_H != "<unselected>") {
      showElement("box_RESULT_HDEND")
    } else if (is.null(input$chkgrp_HEIGHT) && input$sel_H == "<unselected>") {
      hideElement("box_RESULT_HDEND")
    }
  })






  observeEvent(input$chkgrp_HEIGHT, ignoreNULL = F, {
    id <- input$chkgrp_HEIGHT

    ## If they want to construct an HD model
    if ("HDloc" %in% id) {
      if (input$sel_H != "<unselected>") { # if there is H selected
        # command a new plot for the render plot
        plot.new()
        dev.control(displaylist = "enable")

        # Do the HD model
        tab_modelHD <- modelHD(
          D = inv()[, input$sel_DIAMETER],
          H = inv()[, input$sel_H]
        )

        # record the plot
        plotHD <- recordPlot()
        dev.off()

        # render the plot
        output$out_plot_HD <- renderPlot(replayPlot(plotHD))
        # render the table
        output$out_tab_HD <- renderTable(tab_modelHD[, -2], digits = 4)
        # update the radio button with the method and choose the minimun of the RSE
        updateRadioButtons(session,
          inputId = "rad_HDMOD",
          choices = tab_modelHD[, "method"],
          selected = tab_modelHD$method[which.min(tab_modelHD$RSE)],
          inline = T
        )

        # show the box for the result of HDmod
        showElement("box_RESULT_HDMOD")
      } else {
        shinyalert(title = "Oops", text = "You do not have H selected", type = "error")
        updateCheckboxGroupInput(session, "chkgrp_HEIGHT",
          selected = input$chkgrp_HEIGHT[!input$chkgrp_HEIGHT %in% "HDloc"]
        )
      }
    } else {
      hideElement("box_RESULT_HDMOD")
    }

    # if the user command a feldpausch region
    if ("feld" %in% id) {
      updateSelectInput(session, "sel_FELD",
        choices = c(
          "<automatic>",
          "Africa",
          "Central Africa" = "CAfrica",
          "Eastern Africa" = "EAfrica",
          "Western Africa" = "WAfrica",
          "South America" = "SAmerica",
          "Brazilian Shield" = "BrazilianShield",
          "Eastern-central Amazonia" = "ECAmazonia",
          "Guiana Shield" = "GuianaShield",
          "Western Amazonia" = "WAmazonia",
          "Southeast Asia" = "SEAsia",
          "Northern Australia" = "NAustralia",
          "Pantropical"
        )
      )
      showElement("box_RESULT_FELD")
    } else {
      hideElement("box_RESULT_FELD")
    }

    if (is.null(id) && input$sel_H == "<unselected>") {
      hideElement("box_RESULT_HDEND")
    } else {
      showElement("box_RESULT_HDEND")
    }
  })

  observeEvent(input$btn_HD_DONE, {
    if (is.null(input$chkgrp_HEIGHT) && input$sel_H == "<unselected>") {
      shinyalert("Oops", "There is no H and HD model", type = "error")
    } else if (input$sel_LONG != "<unselected>" || "chave" %in% input$chkgrp_HEIGHT || ("feld" %in% input$chkgrp_HEIGHT && input$sel_FELD == "<unselected>")) {
      showMenuItem("tab_MAP")
      updateTabItems(session, "mnu_MENU", "tab_MAP")
    } else {
      showMenuItem("tab_AGB")
      updateTabItems(session, "mnu_MENU", "tab_AGB")
    }
  })



  # Maps --------------------------------------------------------------------

  observeEvent(input$sel_LONG, {
    if (input$sel_LONG != "<unselected>") {
      hideElement("box_long_lat")
    } else {
      showElement("box_long_lat")
    }
  })


  observeEvent({
    if (input$btn_HD_DONE >= 1) {
      input$sel_LAT
      input$sel_LONG
      input$num_LAT
      input$num_LONG
    }
  }, ignoreInit = T, {

    # Create the table of coordinate
    coord <- data.table(
      longitude = if (input$sel_LONG != "<unselected>") inv()[, input$sel_LONG] else input$num_LONG,
      latitude = if (input$sel_LAT != "<unselected>") inv()[, input$sel_LAT] else input$num_LAT
    )

    # if the user as made a mistake
    if (all((sapply(coord, class) == "numeric"))) {

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

      # remove all NA coordinate
      coord <- unique(na.omit(coord))

      # draw the coordinate if there is one remaining
      if (nrow(coord) != 0) {
        output$map <- renderLeaflet({
          leaflet(coord) %>%
            addTiles() %>%
            addMarkers(lng = ~longitude, lat = ~latitude)
        })
      }
    } else {
      shinyalert("Oops", text = "Either the column longitude or latitude are not numeric")
    }
  })

  # End of the map section
  observeEvent(input$btn_MAP_END, {
    showMenuItem("tab_AGB")
    updateTabItems(session, "mnu_MENU", "tab_AGB")
  })









  # AGB section -------------------------------------------------------------


  AGB_sum <- reactiveVal()
  observeEvent(input$btn_AGB_DONE, {


    # AGB list
    AGB_res <- list()

    # take the mode of AGB
    AGBmod <- input$rad_AGB_MOD

    # take the diameter
    D <- inv()[, input$sel_DIAMETER]

    # take the plot ID
    if (input$sel_PLOT != "<unselected>") {
      plot_id <- inv()[, input$sel_PLOT]
    } else {
      plot_id <- NULL
    }

    # WD treatement
    if (all(c("meanWD", "sdWD", "levelWD", "nInd") %in% names(inv()))) {
      WD <- inv()[, "meanWD"]
      errWD <- inv()[, "sdWD"]
    } else {
      WD <- inv()[, input$sel_WD]
      errWD <- NULL
    }

    # coord treatement
    coord <- data.table(
      longitude = if (input$sel_LONG != "<unselected>") inv()[, input$sel_LONG] else input$num_LONG,
      latitude = if (input$sel_LAT != "<unselected>") inv()[, input$sel_LAT] else input$num_LAT
    )

    # Heigth treatement
    if (input$sel_H != "<unselected>") {
      H <- inv()[, input$sel_H]
    } # if H is unselected

    # take the length of the input of check box for the heigth
    length_progression <- length(input$chkgrp_HEIGHT)



    # plain color
    # use for names too
    color <- c(HD_local = "blue", feldpausch = "red", chave = "green", heigth = "black")



    ####### calculation of the AGB

    if (length_progression != 0) { # if we have a model

      withProgress(message = "AGB build", value = 0, {

        # if we have an HD local
        if ("HDloc" %in% input$chkgrp_HEIGHT) {
          HD_mod <- modelHD(D, H, method = input$rad_HDMOD) # compute the model

          AGB_res[[names(color)[1]]] <- AGB_predict(AGBmod, D, WD, errWD, HDmodel = HD_mod)
          incProgress(1 / length_progression, detail = "AGB using HD local: Done")
        }

        # if we want the feldpausch region
        if ("feld" %in% input$chkgrp_HEIGHT) {
          if (input$sel_FELD == "<automatic>") {
            region <- computeFeldRegion(coord)
            if (anyNA(region)) {
              region[is.na(region)] <- "Pantropical"
            }
          } else {
            region <- input$sel_FELD
          }
          AGB_res[[names(color)[2]]] <- AGB_predict(AGBmod, D, WD, errWD, region = region)
          incProgress(1 / length_progression, detail = "AGB using feldpausch region: Done")
        }

        # if we want the chave model
        if ("chave" %in% input$chkgrp_HEIGHT) {
          AGB_res[[names(color)[3]]] <- AGB_predict(AGBmod, D, WD, errWD, coord = coord)
          incProgress(1 / length_progression, detail = "AGB using chave: Done")
        }
      })
    } else { # if we have not any model HD
      AGB_res[[names(color)[4]]] <- AGB_predict(AGBmod, D, WD, H = H)
    }




    ###### After the calculation of the AGB

    # if the plot id is not null
    if (!is.null(plot_id)) {
      # list that use to stock the result
      AGB_sum(lapply(AGB_res, summaryByPlot, plot_id))

      # plot the output
      output$out_plot_AGB <- renderPlot({
        plot_list(AGB_sum(), color)
      })
    } else {
      AGB_sum(AGB_res)
    }

    showElement(id = "box_AGB_res")
    showElement(id = "box_AGB_Report")
  })

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
      data <- setDT(inv()[, selectColumn[selectedColumn]])
      setnames(data, names(data), c(
        "plot", "D",
        "H", "longitude", "latitude"
      )[selectedColumn])


      # create the output data
      out <- data.table(
        plot = if ("plot" %in% names(data)) data$plot else "plot",
        longitude = if ("longitude" %in% names(data)) data$longitude else input$num_LONG,
        latitude = if ("latitude" %in% names(data)) data$latitude else input$num_LAT
      )


      # select the H we need
      H <- if (is.null(input$chkgrp_HEIGHT)) {
        data[, H]
      } else if ("HDloc" %in% input$chkgrp_HEIGHT) {
        retrieveH(data[, D], model = modelHD(data[, D], data[, H], method = input$rad_HDMOD))$H
      } else if ("feld" %in% input$chkgrp_HEIGHT) {
        retrieveH(data[, D], region = if (input$sel_FELD == "<automatic>") {
          computeFeldRegion(cbind(out$longitude, out$latitude))
        } else {
          input$sel_FELD
        })$H
      } else if ("chave" %in% input$chkgrp_HEIGHT) {
        retrieveH(data[, D], coord = cbind(out$longitude, out$latitude))
      }


      # cbind
      out[, H := H]

      # create the Lorey database whith the lorey heigth
      Lorey <- data.table(D = data$D, H = out$H, plot = out$plot)
      Lorey[, BAm := (pi * (D / 2)^2) / 10000]
      Lorey[, HBA := H * BAm]
      Lorey <- Lorey[, .(LoreyH = sum(HBA, na.rm = T) / sum(BAm, na.rm = T)), by = plot]

      # take the data for reduction by plot
      out <- out[, .(
        Long_cnt = mean(longitude),
        Lat_cnt = mean(latitude),
        H_max_Local = max(H)
      ), by = plot]

      # merge the AGB and the Lorey table
      out[setDT(AGB_sum()[[1]]), on = "plot", AGB_local := i.AGB]
      out[Lorey, on = "plot", H_Lorey_local := LoreyH]

      setnames(out, "plot", "Plot_ID")

      # write the file
      fwrite(out, tempFile)

      # copy the file in the file for this purpose
      file.copy(tempFile, file, overwrite = T)
    },
    contentType = "text/csv"
  )
}
