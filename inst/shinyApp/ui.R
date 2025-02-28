dashboardPage(
  skin="green",
  dashboardHeader(title = "BIOMASS application"),
  dashboardSidebar(
    sidebarMenu(
      id = "mnu_MENU",
      menuItem("Load dataset", tabName = "tab_LOAD"),
      menuItem("Taxonomy and Wood density", tabName = "tab_TAXO"),
      menuItem("Height-diameter model", tabName = "tab_HEIGHT"),
      menuItem("AGB calculation", tabName = "tab_AGB")
    )
  ),
  dashboardBody(
    useShinyFeedback(),
    useShinyjs(),
    tabItems(

      # Load dataset -----------------------------------------------------------

      tabItem(
        "tab_LOAD",
        fluidRow(
          box( # box with the file input
            title = "Forest inventory file", width = 6,
            fileInput("file_DATASET", "Select data file",
                      accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")
            ),
            numericInput("num_skip_line", "Skip lines", value = 0, min = 0),
            radioButtons("rad_decimal", "Decimal:", choices = c(Dot = ".",
                                                                Comma = ","))
          ),

          hidden(boxWithId( # box for the input
            id = "box_FIELDS", title = "Column selection", width = 6,

            # obligatory argument
            column(9, selectInput("sel_DIAMETER", "Diameter", choices = NULL)),
            column(3, radioButtons("rad_units_diameter", "Unit:", choices = c("mm", "cm", "m"), selected = "cm")),

            # wood density argument
            hr(),
            h4("Provide either wood density values or the taxonomy"),
            selectInput("sel_WD", "Wood density", choices = NULL),
            selectInput("sel_GENUS", "Genus (e.g. Terminalia) or scientific name (e.g. Terminalia superba or Terminalia superba Engl. & Diels)", choices = NULL),
            selectInput("sel_SPECIES", "Species (e.g. superba)", choices = NULL),
            hidden(div("Impossible combination", id = "msg_wd", style = "color:red;")),

            # Height argument
            hr(),
            h4("Optional"),
            column(9, selectInput("sel_H", "Height", choices = NULL)),
            column(3, radioButtons("rad_units_height", "Unit:", choices = c("cm", "m"), selected = "m")),
            selectInput("sel_LONG", "Coordinate longitude", choices = NULL),
            selectInput("sel_LAT", "Coordinate latitude", choices = NULL),
            hidden(div("Impossible combination", id = "msg_h", style = "color:red;")),

            # plot id
            hr(),
            selectInput("sel_PLOT", "Plot name", choices = NULL),

            # action button to continue
            hr(),
            actionButton("btn_DATASET_LOADED", "Continue", color = "#0040FF")
          )),

          hidden(boxWithId(
            id = "box_DATASET", title = "Inventory file preview content", width = 12,
            DT::DTOutput("table_DATASET")
          ))
        )
      ),



      # Taxonomy ----------------------------------------------------------------

      tabItem(
        "tab_TAXO",
        fluidRow(
          box(
            title = "Wood density (WD) extraction", width = 6,
            radioButtons(
              "rad_WD", "Correct the taxonomy (mispelling) before wood density extraction?",
              c(
                "Correct taxonomy and extract WD" = "corr",
                "Extract WD without taxonomic correction" = "WD"
              )
            ),
            actionButton("btn_TAXO_RESULT", "Go on")
        )),
        fluidRow(
          hidden(boxWithId(
            id = "box_RESULT_TAXO", title = "Results", width = 12,
            withSpinner(
              verbatimTextOutput("out_taxo_error"),
              type = getOption("spinner.type", default = 5),
              color = getOption("spinner.color", default = "#158A0C")),
            hr(),
            verbatimTextOutput("out_wd_error")
          ))
        ),
        fluidRow(
          column(
            width = 12,
            hidden(actionButton("btn_TAXO_DONE", "Continue")),
            align = "center"
          )
        )
      ),


      # Height -----------------------------------------------------------------


      tabItem(
        "tab_HEIGHT",
        fluidRow(
          box(
            title = "Retrieving tree heights via a Height-Diameter model",
            checkboxGroupInput(
              "chkgrp_HEIGHT", "Choose the HD model:",
              inline = T,
              c(
                "Local HD model" = "HDloc",
                "Feldpausch" = "feld",
                "Chave" = "chave"
              )
            )
        )),
        fluidRow(
          column( 4 ,
            ## HD model
            hidden(boxWithId(
              width = 12,
              id = "box_RESULT_HDMOD", title = "Local HD model",
              # (model accuracy is estimated on all HD data)
              tableOutput("out_tab_HD"),
              radioButtons("rad_HDMOD", "Choose your local HD model:", choices = "NULL")
            ))
          ),
          column( 3 ,
            ## Feldpausch
            hidden(boxWithId(
              width = 12,
              id = "box_RESULT_FELD", title = "Feldpausch et al. (2012)",
              textOutput("txt_feld")
            ))
          ),
          column( 5 ,
            ## Chave
            hidden(boxWithId(
              width = 12,
              id = "box_result_chave", title = "Chave et al. (2014)",
              withSpinner(textOutput("txt_chave"),
                          type = getOption("spinner.type", default = 5),
                          color = getOption("spinner.color", default = "#158A0C")
              )
            )),
            ## Map
            hidden(boxWithId(
              width = 12,
              id = "box_MAP", title = "Map",
              numericInput("num_LONG", "longitude", 3.8614, min = -180, max = 180, step = 0.01),
              numericInput("num_LAT", "latitude", 43.652, min = -90, max = 90, step = 0.01),
              plotOutput("plot_MAP")
            ))
          )
        ),
        fluidRow(
          ## Comparison of the methods
          column(
            width = 12,
            hidden(boxWithId(
              id = "box_plot_comparison", title = "Model comparison",
              plotOutput("out_plot_comp"),
              align = "center"
          )))
        ),
        fluidRow(
          column(
            width = 12,
            hidden(actionButton("btn_HD_DONE", "Continue")),
            align = "center"
          )
        )
      ),



      # AGB -----------------------------------------------------------------
      tabItem(
        "tab_AGB",
        fluidRow(
          box(
            title = "AGB estimation", width = 3,
            radioButtons("rad_AGB_MOD", NULL, choices = c("AGB" = "agb", "AGB + error" = "agbe"), inline = T),
            actionButton("btn_AGB_DONE", "Go on")
          ),
          hidden(boxWithId(
            id = "box_AGB_res", title = "AGB result", width = 12,
            withSpinner(plotOutput("out_plot_AGB"),
                        type = getOption("spinner.type", default = 5),
                        color = getOption("spinner.color", default = "#158A0C")
            )
          )),
          hidden(boxWithId(
            id = "box_AGB_Report", width = 2,
            downloadButton("dwl_report", label = "Report"),
            downloadButton("dwl_file", label = "Download results")
          ))
        )
      )
    )
  )
)
