function(input, output, session) {

  # stop the serveur in the end of the session
  session$onSessionEnded(function() {
    stopApp()
  })

  observe({
    # hide few menu at the begining
    hideMenuItem("tab_TAXO")
    hideMenuItem("tab_MAP")
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
    } else {
      # else show the heigth tab
      showMenuItem("tab_MAP")
      updateTabItems(session, "mnu_MENU", "tab_MAP")
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


  # Maps --------------------------------------------------------------------

  observeEvent(input$sel_LONG, {
    if (input$sel_LONG != "<unselected>") {
      hideElement("box_long_lat")
    } else {
      showElement("box_long_lat")
    }
  })


  observeEvent({
    if (input$btn_DATASET_LOADED >= 1) {
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
        coord <- coord[, .(
          longitude = mean(longitude),
          latitude = mean(latitude)
        ),
        by = inv()[, input$sel_PLOT]
        ][ .(longitude, latitude) ]
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

  observeEvent(input$btn_MAP_END, {
    if (input$sel_WD == "<unselected>") {
      # if the WD is not selected then show the tab TAXO
      showMenuItem("tab_TAXO")
      updateTabItems(session, "mnu_MENU", "tab_TAXO")
    } else {
      # else show the heigth tab
      showMenuItem("tab_HEIGHT")
      updateTabItems(session, "mnu_MENU", "tab_HEIGHT")
    }
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
        output$out_tab_HD <- renderTable(tab_modelHD, digits = 4)
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
          ifelse(input$sel_LONG == "<unselected>", NULL, "<automatic>"),
          rownames(feldCoef)
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
    } else {
      showMenuItem("tab_AGB")
      updateTabItems(session, "mnu_MENU", "tab_AGB")
    }
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
    if (input$sel_LONG != "<unselected>") {
      coord <- data.table(
        long = inv()[, input$sel_LONG],
        lat = inv()[, input$sel_LAT]
      )
    }

    # Heigth treatement
    if (input$sel_H != "<unselected>") {
      H <- inv()[, input$sel_H]
    } # if H is unselected

    # take the length of the input of check box for the heigth
    length_progression <- length(input$chkgrp_HEIGHT)



    ####### calculation of the AGB

    if (length_progression != 0) { # if we have a model

      withProgress(message = "AGB build", value = 0, {

        # if we have an HD local
        if ("HDloc" %in% input$chkgrp_HEIGHT) {
          HD_mod <- modelHD(D, H, method = input$rad_HDMOD) # compute the model

          AGB_res$HDlocal <- AGB_predict(AGBmod, D, WD, errWD, HDmodel = HD_mod)
          incProgress(1 / length_progression, detail = "AGB using HD local: Done")
        }

        # if we want the feldpausch region
        if ("feld" %in% input$chkgrp_HEIGHT) {
          if (input$sel_FELD == "<automatic>") {
            region <- computeFeldRegion(coord)
          } else {
            region <- input$sel_FELD
          }
          AGB_res$feld <- AGB_predict(AGBmod, D, WD, errWD, region = region)
          incProgress(1 / length_progression, detail = "AGB using feldpausch region: Done")
        }

        # if we want the chave model
        if ("chave" %in% input$chkgrp_HEIGHT) {
          AGB_res$chave <- AGB_predict(AGBmod, D, WD, errWD, coord = coord)
          incProgress(1 / length_progression, detail = "AGB using chave: Done")
        }
      })
    } else { # if we have not any model HD
      AGB_res$heigth <- AGB_predict(AGBmod, D, WD, H = H)
    }




    ###### After the calculation of the AGB

    # if the plot id is not null
    if (!is.null(plot_id)) {
      # list that use to stock the result
      AGB_sum(lapply(AGB_res, summaryByPlot, plot_id))

      # plot the output
      output$out_plot_AGB <- renderPlot({

        # take the order of the first result
        plot_order <- order(AGB_sum()[[1]]$AGB)

        # sekeleton of the plot
        plot(plot_order,
          main = "", type = "n",
          ylim = range(sapply(AGB_sum(), function(x) {
            range(x[, -1], na.rm = T)
          })),
          xlab = "", ylab = "AGB",
          xaxt = "n"
        )
        axis(1, at = 1:length(plot_order), labels = AGB_sum()[[1]]$plot[plot_order], las = 2)

        # plain color
        color <- c(HDlocal = "blue", feld = "red", chave = "green", heigth = "black")

        # if it's the AGB without the error propagtion
        if (ncol(AGB_sum()[[1]]) == 2) {

          # trace the points in the graph
          lapply(names(AGB_sum()), function(x) {
            points(1:length(plot_order), AGB_sum()[[x]][plot_order, "AGB"], col = color[x], pch = 20)
          })
        } else {
          # transparent color expect the first which is HDlocal
          color <- rgb(t(col2rgb(color[1:3])) / 255,
            alpha = c(1, 0.5, 0.5),
            names = names(color[1:3])
          )

          # trace the polygon expect for the first value
          lapply(names(AGB_sum())[-1], function(x) {
            with(AGB_sum()[[x]], {
              polygon(
                x = c(seq_along(plot_order), length(plot_order):1),
                y = c(Cred_2.5[plot_order], rev(Cred_97.5[plot_order])),
                col = color[x], border = NA
              )
            })
          })

          # trace the points + segments for the first HD model
          with(AGB_sum()[[1]], {
            points(seq_along(plot_order), AGB[plot_order], pch = 20, cex = 1.5, col = color[names(AGB_sum())[1]])
            segments(seq_along(plot_order), Cred_2.5[plot_order], y1 = Cred_97.5[plot_order], col = color[names(AGB_sum())[1]])
          })
        }


        # draw the legend
        legend("bottomright", legend = names(AGB_sum()), col = color[names(AGB_sum())], pch = 20)
      })
    } else {
      AGB_sum(AGB_res)
    }

    showElement(id = "box_AGB_res")
  })
}
