function(input, output, session) {

  # stopper le serveur si la session est terminee
  session$onSessionEnded(function() {
    stopApp()
  })

  observe({
    # cacher certains menus au demarrage
    hideMenuItem("tab_TAXO")
    hideMenuItem("tab_HEIGHT")
    # hideMenuItem("tab_MAP")
  })


  # load dataset ------------------------------------------------------------

  inv <- reactiveVal()
  observeEvent(input$file_DATASET, {
    # importer le fichier
    inv(fread(file = input$file_DATASET$datapath))

    # montrer les boites
    showElement("box_DATASET")
    showElement("box_FIELDS")

    # afficher son contenu
    output$table_DATASET <- renderDataTable(inv())

    selectionField <- c("sel_DIAMETER", "sel_PLOT", "sel_WD", "sel_GENUS", "sel_SPECIES", "sel_H", "sel_LONG", "sel_LAT")
    # remplir les selecteurs de champs avec les noms de champs
    for (id in selectionField) {
      updateSelectInput(session, id, choices = c("<unselected>", names(inv())))
    }

    # plots <- inv()[,by=plotId,.(long=mean(long),lat=mean(lat))]
    # output$map_PLOT <- renderLeaflet(
    #   leaflet(plots) %>%
    #     addTiles() %>%
    #     addMarkers(lng=~long,lat=~lat)
    # )
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
    feedbackDanger("sel_PLOT",
      condition = input$sel_PLOT == "<unselected>",
      text = "argument obligatory"
    )
  })

  # error when the user click on the button on the first page
  observeEvent(input$btn_DATASET_LOADED, {
    if (input$sel_DIAMETER == "<unselected>") { # if diameter is not selected
      shinyalert("Oops!", "D is unselected", type = "error")
    } else if (input$sel_PLOT == "<unselected>") { # if the plot is not selected
      shinyalert("Oops!", "Plot id is unselected", type = "error")
    } else if (!xor(input$sel_WD == "<unselected>", input$sel_GENUS == "<unselected>")) {
      # if the wd is not selected or genus not selected but not the two
      shinyalert("Oops!", "Either select the wood density or genus and species or genus", type = "error")
    } else if (ifheigth(
      input$sel_H == "<unselected>",
      input$sel_LONG == "<unselected>",
      input$sel_LAT == "<unselected>"
    )) {
      # if the H is not selected and one of the two (long or lat) is not selected
      shinyalert("Oops!", "Please select the heigth or longitude and latitude", type = "error")
    } else if (input$sel_WD == "<unselected>") {
      # if the WD is not selected then show the menue of the tab TAXO
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
      condition = ifheigth(
        input$sel_H == "<unselected>",
        input$sel_LONG == "<unselected>",
        input$sel_LAT == "<unselected>"
      )
    )
  })



  # taxonomy ----------------------------------------------------------------


  observeEvent(input$btn_TAXO_DONE, {

    # show a progress bar
    withProgress(message = "Searching the wood density", value = 0, {
      # if the users have selected the correct taxo + get wd
      if (input$rad_WD == "corr") {
        # correct the taxo and catch the error if there is error
        taxo <- tryCatch({
          if (input$sel_SPECIES == "<unselected>") {
            correctTaxo(genus = inv()[, eval(parse(text = input$sel_GENUS))])
          } else {
            correctTaxo(
              genus = inv()[, eval(parse(text = input$sel_GENUS))],
              species = inv()[, eval(parse(text = input$sel_SPECIES))]
            )
          }
        }, error = function(e) e)

        # if there is an error display it
        if (length(taxo) == 2) {
          output$out_taxo_error <- renderPrint({ taxo$message })
        } else { # if not a message will appear
          output$out_taxo_error <- renderPrint({
            print("How much name has been modified:")
            table(taxo$nameModified)
          })
        }
        # update the progression
        incProgress(1 / 2, detail = "Correct the taxonomy completed")
        genus = taxo$genusCorrected
        species = taxo$speciesCorrected
      } else {

        # if the users do not choose the coorect taxo
        genus = inv()[eval(parse(text = input$sel_GENUS))]
        if (input$sel_SPECIES == "<unselected>"){
          split = tstrsplit_NA(genus)
          genus = split[, 1]
          species = split[, 2]
        } else {
          species = inv()[eval(parse(text = input$sel_SPECIES))]
        }
      }
      wd = tryCatch(getWoodDensity(genus, species), error = function(e) e, warning = function(e) e)

      # if there is an error display it
      if (length(wd) == 2){
        output$out_wd_error <- renderPrint({ taxo$message })
      } else { # if not a message will appear
        output$out_wd_error = renderPrint({
          print("How much tree level at which wood density has been calculated:")
          table(wd$levelWD)
        })
      }

      incProgress(1, detail = "Get the wood density completed")
    })

    # showMenuItem("tab_HEIGHT")
    # updateTabItems(session, "mnu_MENU", "tab_HEIGHT")
  })



  observeEvent(input$chkgrp_HEIGHT, {
    print(input$chkgrp_HEIGHT)
  })
}
