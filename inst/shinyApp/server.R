function(input,output,session) {

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

  # load dataset
  inv <- reactiveVal()
  observeEvent(input$file_DATASET, {
    # importer le fichier
    inv(fread(file=input$file_DATASET$datapath))

    # montrer les boites
    showElement("box_DATASET")
    showElement("box_FIELDS")

    # afficher son contenu
    output$table_DATASET <- renderDataTable(inv())

    selectionField = c("sel_DIAMETER","sel_PLOT","sel_WD","sel_GENUS","sel_SPECIES", "sel_H", "sel_LONG", "sel_LAT")
    # remplir les selecteurs de champs avec les noms de champs
    for(id in selectionField) {
      updateSelectInput(session,id,choices=c("<unselected>",names(inv())))
    }

    # plots <- inv()[,by=plotId,.(long=mean(long),lat=mean(lat))]
    # output$map_PLOT <- renderLeaflet(
    #   leaflet(plots) %>%
    #     addTiles() %>%
    #     addMarkers(lng=~long,lat=~lat)
    # )
  })

  observeEvent(input$sel_DIAMETER, {
    feedbackDanger("sel_DIAMETER",
                   condition = input$sel_DIAMETER == "<unselected>",
                   text = "argument obligatory")
  })

  observeEvent(input$sel_PLOT, {
    feedbackDanger("sel_PLOT",
                   condition = input$sel_PLOT == "<unselected>",
                   text = "argument obligatory")
  })

  observeEvent(input$btn_DATASET_LOADED, {


    showMenuItem("tab_TAXO")
    updateTabItems(session,"mnu_MENU","tab_TAXO")
  })

  observeEvent(input$btn_TAXO_DONE,{
    print(input$rad_WD)
    showMenuItem("tab_HEIGHT")
    updateTabItems(session,"mnu_MENU","tab_HEIGHT")
  })

  observe({
    toggleElement("msg",condition=
    (input$sel_WD!="<unselected>")&& ( (input$sel_GENUS!="<unselected>")||(input$sel_SPECIES!="<unselected>")) )
  })

  observeEvent(input$chkgrp_HEIGHT, {
    print(input$chkgrp_HEIGHT)
  })

}
