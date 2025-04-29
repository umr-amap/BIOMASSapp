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
          column(6,
                 box( # Forest inventory file's box
                   title = h3("Forest inventory file"), width = 12,
                   fileInput("file_DATASET", "Choose a CSV file",
                             accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")
                   ),
                   #numericInput("num_skip_line", "Skip lines", value = 0, min = 0),
                   radioButtons("rad_decimal", "Decimal:", choices = c(Dot = ".",
                                                                       Comma = ",")),
                   radioButtons("rad_several_plots", "Does your dataset contain several plots?",
                                choices = c(Yes = "several_plots", No = "single_plot"), selected = character(0)),
                   div("information required", id = "msg_several_plots", style = "color:red;"),
                   hidden(selectInput("sel_PLOT", "Which column contains the plots IDs?", choices = NULL))
                 ),
                 hidden(boxWithId( # Coordinate's box
                   id = "box_COORD", title = h3("Geographic coordinates (optional)"), width = 12,
                   radioButtons("rad_coord", "Do you have:",
                                choices = c("the columns corresponding to the coordinates of each tree" = "coord_each_tree",
                                            "the coordinates of the plot(s) in another dataset (see below for an overview)" = "coord_plot",
                                            "no coordinates" = "coord_none"),
                                selected = character(0)),
                   # If coordinates of each tree
                   hidden(div(id = "id_sel_coord",
                              selectInput("sel_LAT", "Latitude", choices = NULL),
                              selectInput("sel_LONG", "Longitude", choices = NULL),
                   )),
                   # If coordinates of plot(s)
                   hidden(div(id = "id_file_coord",
                              fileInput("file_coord", "Choose a CSV file",
                                        accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
                   )),
                   hidden(div(id = "id_sel_coord_plot",
                              selectInput("sel_LAT_sup_coord", "Latitude", choices = NULL),
                              selectInput("sel_LONG_sup_coord", "Longitude", choices = NULL)
                   )),
                   # If coordinates of plots and several plots
                   hidden(selectInput("sel_plot_coord", "Plots IDs", choices = NULL))
                 ))
          ),
          column(6,
                 hidden(boxWithId( # AGB inputs box
                   id = "box_FIELDS", title = h3("Required parameters"), width = 12,
                   hr(),

                   ## Diameter (compulsory) ----
                   h4("Diameter"),
                   column(9, selectInput("sel_DIAMETER", "", choices = NULL)),
                   column(3, radioButtons("rad_units_diameter", "Unit:", choices = c("mm", "cm", "m"), selected = "cm")),
                   hr(), hr(), hr(), #just to get one horizontal row

                   ## Wood density or taxonmy (compulsory) ----
                   hr(), hr(), hr(),
                   h4("Wood density or taxonomy"),
                   column(9,
                          selectInput("sel_WD", "Wood density", choices = NULL)),
                   column(3, radioButtons("rad_units_wd", "Unit:",
                                          choices = c("g.cm-3","kg.m-3"),
                                          selected = "g.cm-3")),
                   column(12, hidden(div(id = "id_set_errWD",
                                         numericInput("set_errWD", label = "What is the assumed error associated with the wood densities measurements (in m) ?",
                                                      value = 0.07, min = 0)))),
                   h5("or"),
                   column(12, selectInput("sel_GENUS", "Genus (e.g. Terminalia) or scientific name (e.g. Terminalia superba or Terminalia superba Engl. & Diels)", choices = NULL)), # column to keep the same layout than Diameter and Wood density selections
                   column(12, selectInput("sel_SPECIES", "Species (e.g. superba)", choices = NULL)),
                   hidden(div("Provide either wood density or taxonomy information ", id = "msg_wd", style = "color:red;")),

                   ## Height ----
                   hr(),
                   h4("Height"),
                   column(12, radioButtons("rad_height", "Do you have:",

                                           choices = c("The height of each tree" = "h_each_tree",
                                                       "The height of some trees in the same dataset" = "h_some_tree",
                                                       "A subset of well-measured trees in another dataset (see below for an overview)" = "h_sup_data",
                                                       "No height measurements (use coordinates to estimate height)" = "h_none"),
                                           selected = character(0))),
                   # If height of each tree or some trees
                   hidden(div(id = "id_sel_h",
                              column(9 , selectInput("sel_H", "Select height column", choices = NULL)),
                              column(3, radioButtons("rad_units_height", "Unit:", choices = c("cm", "m"), selected = "m"))
                   )),
                   hidden(div(id = "id_sel_HDmodel_by",
                              column(12,
                                     h5("The heights of non-measured trees will be estimated using Height-Diameter relationships on measured trees."),
                                     h5("If you want to create a Height-Diameter model by plot (or by any category), please specify the column corresponding to the plot (or category) IDs:"),
                                     selectInput("sel_HDmodel_by", "", choices = NULL))
                   )),
                   hidden(div(id = "id_set_errH",
                              column(12, numericInput("set_errH", label = "What is the assumed error associated with the individual height measurements ?",
                                                      value = 4.22, min = 0)),
                   )),
                   # If height in another dataset
                   hidden(div(id = "id_file_h_sup",
                              column(12, fileInput("file_h_sup", "Choose a CSV file",
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"))),
                              column(9 , selectInput("sel_D_sup_data", "Diameter", choices = NULL)),
                              column(3, radioButtons("rad_units_D_sup", "Unit:", choices = c("mm","cm", "m"), selected = "cm")),
                              column(9 , selectInput("sel_H_sup_data", "Height", choices = NULL)),
                              column(3, radioButtons("rad_units_H_sup", "Unit:", choices = c("cm", "m"), selected = "m"))
                   ))
                 )))),

        # Action button to continue
        fluidRow(
          column(
            width = 12,
            actionButton("btn_DATASET_LOADED", "Continue", color = "#0040FF"),
            align = "center"
          )
        ),

        # Inventory data preview
        fluidRow(
          hidden(boxWithId(
            id = "box_DATASET", title = "Preview of forest inventory data", width = 12,
            DT::DTOutput("table_DATASET")
          ))
        ),
        # Coordinates data preview
        fluidRow(
          hidden(boxWithId(
            id = "box_coord_preview", title = "Preview of plot's coordinates data", width = 12,
            DT::DTOutput("table_coord")
          ))
        ),
        # Supplementary H-D data preview
        fluidRow(
          hidden(boxWithId(
            id = "box_h_sup_preview", title = "Preview of Height-Diameter supplementary data", width = 12,
            DT::DTOutput("table_h_sup")
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
              c("Correct taxonomy and extract WD" = "corr",
                "Extract WD without taxonomic correction" = "WD")
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
            title = h3("Retrieving tree heights via a Height-Diameter model"),
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
          column( 4 ,
                  ## Feldpausch
                  hidden(boxWithId(
                    width = 12,
                    id = "box_RESULT_FELD", title = "Feldpausch et al. (2012)",
                    #textOutput("txt_feld")
                    tableOutput("out_tab_feld"),
                  ))
          ),
          column( 4 ,
                  ## Chave
                  hidden(boxWithId(
                    width = 12,
                    id = "box_result_chave", title = "Chave et al. (2014)",
                    withSpinner(tableOutput("out_tab_chave"),
                                type = getOption("spinner.type", default = 5),
                                color = getOption("spinner.color", default = "#158A0C")
                    )
                  ))
          )
        ),
        fluidRow(
          ## Comparison of the methods and map if Chave is ticked
          column(
            width = 8,
            hidden(boxWithId(
              id = "box_plot_comparison", title = "Model comparison",
              plotOutput("out_plot_comp", height = "600px"),
              align = "center", width = 12
            ))),
          ## Map
          column(
            width = 4,
            hidden(boxWithId(
              id = "box_MAP", title = "Map",
              width = 12,
              withSpinner(leafletOutput(outputId = "plot_MAP"),
                          type = getOption("spinner.type", default = 5),
                          color = getOption("spinner.color", default = "#158A0C")
              )))
          )
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
            id = "box_AGB_Report", width = 5,
            downloadButton("dwl_tree_file", label = "Download tree level results"),
            downloadButton("dwl_plot_file", label = "Download plot level results"),
            downloadButton("dwl_report", label = "Downloal report"),
          ))
        )
      )
    )
  )
)
