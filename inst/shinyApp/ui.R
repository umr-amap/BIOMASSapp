dashboardPage(
  dashboardHeader(title = "BIOMASS application"),
  dashboardSidebar(
    sidebarMenu(
      id = "mnu_MENU",
      menuItem("Load dataset", tabName = "tab_LOAD"),
      menuItem("Taxonomy and Wood density", tabName = "tab_TAXO"),
      menuItem("Model HD", tabName = "tab_HEIGHT"),
      menuItem("AGB calculation", tabName = "tab_AGB")
      # ,menuItem("Map",tabName = "tab_MAP")
    )
  ),
  dashboardBody(
    useShinyalert(),
    useShinyFeedback(),
    useShinyjs(),
    tabItems(

      # load dataset ------------------------------------------------------------

      tabItem(
        "tab_LOAD",
        fluidRow(
          box( # box with the file input
            title = "Inventory file", width = 6,
            fileInput("file_DATASET", "Select data file", accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv")),
            numericInput("num_skip_line", "Skip lines", value = 0, min = 0)
          ),

          hidden(boxWithId( # box for the input
            id = "box_FIELDS", title = "Column selection", width = 6,

            # obligatory argument
            selectInput("sel_DIAMETER", "Diameter", choices = NULL),
            selectInput("sel_PLOT", "Plot name", choices = NULL),

            # wood density argument
            hr(),
            selectInput("sel_WD", "Wood density", choices = NULL),
            selectInput("sel_GENUS", "Genus (if unspecified), species is assumed to be 'genus species'", choices = NULL),
            selectInput("sel_SPECIES", "Species", choices = NULL),
            hidden(div("Imposible combinaison", id = "msg_wd", style = "color:red;")),

            # Heigth argument
            hr(),
            selectInput("sel_H", "Height", choices = NULL),
            selectInput("sel_LONG", "Coordinate longitude", choices = NULL),
            selectInput("sel_LAT", "Coordinate latitude", choices = NULL),
            hidden(div("Imposible combinaison", id = "msg_h", style = "color:red;")),

            # action button to continue
            hr(),
            actionButton("btn_DATASET_LOADED", "Continue", color = "#0040FF")
          )),

          hidden(boxWithId(
            id = "box_DATASET", title = "Inventory file preview content", width = 12,
            dataTableOutput("table_DATASET")
          ))
        )
      ),



      # Taxonomy ----------------------------------------------------------------

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
          actionButton("btn_TAXO_RESULT", "Go on")
        ),
        hidden(boxWithId(
          id = "box_RESULT_TAXO", title = "Result", width = 12,
          verbatimTextOutput("out_taxo_error"),
          hr(),
          verbatimTextOutput("out_wd_error"),
          actionButton("btn_TAXO_DONE", "continue")
        ))
      ),


      # heigth ------------------------------------------------------------------


      tabItem(
        "tab_HEIGHT",
        box(
          title = "HD model", width = 12,
          checkboxGroupInput(
            "chkgrp_HEIGHT", "Choose the HD model:", inline = T,
            c(
              "HD local model" = "HDloc",
              "Feldpausch" = "feld",
              "Chave" = "chave"
            )
          )
        ),
        hidden(boxWithId(id = "box_RESULT_FELD", title = "Feldpausch",
                  selectInput("sel_FELD", "Choose your Feldpausch region:", choices = rownames(feldCoef))
                  )),
        hidden(boxWithId(id = "box_RESULT_HDMOD", title = "HD local model", width = 12,
                         plotOutput("out_plot_HD"),
                         tableOutput("out_tab_HD"),
                         radioButtons("rad_HDMOD", "Choose your HD model:", choices = "NULL")
        )),
        hidden(boxWithId(id = "box_RESULT_HDEND", title = NULL,
                         actionButton("btn_HD_DONE", "continue")
        ))
      )


# AGB -----------------------------------------------------------------
      , tabItem("tab_AGB",
                fluidRow(
                  box(title = "AGB mode",
                      radioButtons("rad_AGB_MOD", NULL, choices = c("AGB" = "agb", "AGB + error" = "agbe"), inline = T),
                      actionButton("btn_AGB_DONE", "Go on")),
                  hidden(boxWithId(id = "box_AGB_res", title = "AGB result"),
                        plotOutput("out_plot_AGB"))
                ))
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
