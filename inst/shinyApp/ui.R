dashboardPage(
  dashboardHeader(title = "BIOMASS application"),
  dashboardSidebar(
    sidebarMenu(
      id = "mnu_MENU",
      menuItem("Load dataset", tabName = "tab_LOAD"),
      menuItem("Taxonomy and Wood density", tabName = "tab_TAXO"),
      menuItem("Model HD", tabName = "tab_HEIGHT")
      # ,menuItem("Map",tabName = "tab_MAP")
    )
  ),
  dashboardBody(
    useShinyFeedback(),
    useShinyjs(),
    tabItems(
      tabItem(
        "tab_LOAD",
        fluidRow(
          box(
            title = "Inventory file", width = 6,
            fileInput("file_DATASET", "Select data file")
          ),

          hidden(boxWithId(
            id = "box_FIELDS", title = "Column selection", width = 6,
            selectInput("sel_DIAMETER", "Diameter", choices = NULL),
            selectInput("sel_PLOT", "Plot name", choices = NULL),

            hr(),
            selectInput("sel_WD", "Wood density", choices = NULL),
            selectInput("sel_GENUS", "Genus (if unspecified), species is assumed to be 'genus species'", choices = NULL),
            selectInput("sel_SPECIES", "Species", choices = NULL),
            hidden(div("Imposible combinaison", id = "msg", style = "color:red;")),
            hr(),

            selectInput("sel_H", "Height", choices = NULL),
            selectInput("sel_LONG", "Coordinate longitude", choices = NULL),
            selectInput("sel_LAT", "Coordinate latitude", choices = NULL),

            hr(),
            actionButton("btn_DATASET_LOADED", "Continue", color = "#0040FF")
          )),

          hidden(boxWithId(
            id = "box_DATASET", title = "Inventory file preview content", width = 12,
            dataTableOutput("table_DATASET")
          ))
        )
      ),
      tabItem(
        "tab_TAXO",
        box(
          title = "Choose the action", width = 12,
          radioButtons(
            "rad_WD", "Choose the things you want to do to calculate the wood density",
            c(
              "correct the taxonomy + get the WD" = "corr",
              "get the WD" = "WD"
            )
          ),
          hr(),
          actionButton("btn_TAXO_DONE", "Go on")
        )
      ),
      tabItem(
        "tab_HEIGHT",
        box(title = "HD model",
            checkboxGroupInput("chkgrp_HEIGHT", "Choose the HD model:",
                               c(
                                 "HD local model" = "HDloc",
                                 "Feldpausch" = "feld",
                                 "Chave" = "chave"
                                 )))
      )
      # ,tabItem("tab_MAP",
      #   fluidRow(
      #     box("Inventory plot map",width=12,
      #       leafletOutput("map_PLOT")
      #     )
      #   )
      # )
    )
  )
)
