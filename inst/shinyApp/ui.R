page <- dashboardPage(
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
          column(12,
                 p("  To estimate the ", strong("above ground biomass (AGB)"), " of a forest inventory, ", strong("3 parameters"), " are required:"),
                 p("- The ", strong("diameter"), "(DBH: Diameter at Breast Height for trees > 10 cm)"),
                 p("- The ", strong("wood density"), " (a method for estimating this parameter based on taxonomy is proposed when wood density data are not available)"),
                 p("- The ", strong("height"), " (three methods for estimating this parameter are proposed when height data are not available)"),
                 br()
        )),
        fluidRow(
          column(6,
                 box( # Forest inventory file's box
                   title = h3(strong("Forest inventory file")), width = 12,
                   fileInput("file_DATASET", "Choose a CSV file (see below for a preview)",
                             accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")) |>
                     helper(colour = "#158A0C", content = "inv_dataset"),
                   #numericInput("num_skip_line", "Skip lines", value = 0, min = 0),
                   radioButtons("rad_several_plots", "Does your dataset contain several plots?",
                                choices = c(Yes = "several_plots", No = "single_plot"), selected = character(0)),
                   div("information required", id = "msg_several_plots", style = "color:red;"),
                   hidden(selectInput("sel_PLOT", "Which column contains the plots IDs?", choices = NULL)),
                   br(),
                   br(),
                   p("Do you need an example of forest inventory data? Click the button below."),
                   downloadButton("dwl_inv_ex", label = "Download an example") |>
                     helper(colour = "#158A0C", content = "inv_example", size = "l")
                 ),

                 ## Coordinate's box ----
                 hidden(boxWithId(
                   id = "box_COORD", title = h3("Geographic coordinates (optional)"), width = 12,
                   p("GPS coordinates are optional and will be used to estimate tree heights ", strong("in cases where height data is unvailable"),". These coordinates - ", strong("latitude and longitude - "), "must be expressed in ", strong("decimal degrees"), ", e.g: (4.0849 ; -52.6844)."),
                   radioButtons("rad_coord", "Do you have:",
                                choices = c("the columns corresponding to the coordinates of each tree" = "coord_each_tree",
                                            "the coordinates of the plot(s) in another dataset" = "coord_plot",
                                            "the coordinates of the plot or region that you want to specify manually" = "coord_manually",
                                            "no coordinates" = "coord_none"),
                                selected = character(0)),
                   # If coordinates of each tree
                   hidden(div(id = "id_sel_coord",
                              selectInput("sel_LAT", "Latitude", choices = NULL),
                              selectInput("sel_LONG", "Longitude", choices = NULL)
                   )),
                   # If coordinates of plot(s)
                   hidden(div(id = "id_file_coord",
                              fileInput("file_coord", "Choose a CSV file (see below for a preview)",
                                        accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"))
                   )),
                   hidden(div(id = "id_sel_coord_plot",
                              selectInput("sel_LAT_sup_coord", "Latitude", choices = NULL) |>
                                helper(colour = "#158A0C", content = "lat_sup_data"),
                              selectInput("sel_LONG_sup_coord", "Longitude", choices = NULL)
                   )),
                   # If coordinates of plots and several plots
                   hidden(selectInput("sel_plot_coord", "Plots IDs", choices = NULL)),
                   # If coordinates specified manually
                   hidden(div(id = "id_num_lat_long",
                              numericInput("num_lat", "Latitude", value = 0, min = -90, max=90),
                              numericInput("num_long", "Longitude", value = 0, min = -180, max=180))),
                   br(),
                   br(),
                   p("Do you need an example of coordinates data? Click the button below."),
                   downloadButton("dwl_coord_ex", label = "Download an example") |>
                     helper(colour = "#158A0C", content = "coord_example")
                 ))
          ),
          column(6,
                 hidden(boxWithId( # AGB inputs box
                   id = "box_FIELDS", title = h3(strong("Required parameters")), width = 12,
                   column(12, hr()),

                   ## Diameter (compulsory) ----
                   h4(strong("Diameter")),
                   p("Select the column corresponding to the diameter of your trees."),
                   column(9, selectInput("sel_DIAMETER", "", choices = NULL)) |>
                     helper(colour = "#158A0C", content = "diameter"),
                   column(3, radioButtons("rad_units_diameter", "Unit:", choices = c("mm", "cm", "m"), selected = "cm")),
                   column(12, hr()),

                   ## Wood density or taxonomy (compulsory) ----
                   h4(strong("Wood density or taxonomy")) ,
                   p("Select the column(s) corresponding to ", strong("either "), "the wood density ", strong("or "), "the taxonomy of your trees") |>
                     helper(colour = "#158A0C", content = "wood_density"),
                   column(9,
                          selectInput("sel_WD", "Wood density", choices = NULL)),
                   column(3, radioButtons("rad_units_wd", "Unit:",
                                          choices = c("g.cm-3","kg.m-3"),
                                          selected = "g.cm-3")),
                   column(12, hidden(div(id = "id_set_errWD",
                                         numericInput("set_errWD", label = "What is the assumed error associated with the wood densities measurements ?",
                                                      value = 0.07, min = 0) |>
                                           helper(colour = "#158A0C", content = "set_errWD")))),
                   h5(strong("or")),
                   column(12, selectInput("sel_GENUS", "Genus (e.g. Terminalia) or scientific name (e.g. Terminalia superba or Terminalia superba Engl. & Diels)", choices = NULL)), # column to keep the same layout than Diameter and Wood density selections
                   column(12, selectInput("sel_SPECIES", "Species (e.g. superba)", choices = NULL)),
                   div("Provide either wood density or taxonomy information ", id = "msg_wd", style = "color:red;"),
                   column(12, hr()),

                   ## Height ----
                   h4(strong("Height")),
                   column(12, radioButtons("rad_height", "Do you have:",

                                           choices = c("The height of each tree" = "h_each_tree",
                                                       "The height of some trees in the same dataset" = "h_some_tree",
                                                       "A subset of well-measured trees in another dataset" = "h_sup_data",
                                                       "No height measurements (use coordinates to estimate height)" = "h_none"),
                                           selected = character(0)) |>
                            helper(colour = "#158A0C", content = "rad_height")),
                   # If height of each tree or some trees
                   hidden(div(id = "id_sel_h",
                              column(9 , selectInput("sel_H", "Select height column", choices = NULL)),
                              column(3, radioButtons("rad_units_height", "Unit:", choices = c("cm", "m"), selected = "m")),
                              column(9, numericInput("set_errH", label = helper(shiny_tag = "What is the assumed relative error (in %) associated with individual height measurements ?",colour = "#158A0C", content = "set_errH"), value = 10))
                                ,
                              column(3, p(""))
                   )),
                   # hidden(div(id = "id_set_errH",
                   #            column(9, numericInput("set_errH", label = "What is the assumed relative error (in %) associated with individual height measurements ?",
                   #                                    value = 10, min = 0)|>
                   #                     helper(colour = "#158A0C", content = "set_errH")),
                   #            column(3, p(""))
                   # )),
                   hidden(div(id = "id_sel_HDmodel_by",
                              column(12,
                                     h5("The heights of non-measured trees will be estimated using Height-Diameter relationships on measured trees."),
                                     h5("If you want to create a Height-Diameter model by plot (or by any category), please specify the column corresponding to the plot (or category) IDs:"),
                                     selectInput("sel_HDmodel_by", "", choices = NULL))
                   )),
                   # If height in another dataset
                   hidden(div(id = "id_file_h_sup",
                              column(12, fileInput("file_h_sup", "Choose a CSV file (see below for a preview)",
                                                   accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"))),
                              column(9 , selectInput("sel_D_sup_data", "Select diameter column:", choices = NULL)),
                              column(3, radioButtons("rad_units_D_sup", "Unit:", choices = c("mm","cm", "m"), selected = "cm")),
                              column(9 , selectInput("sel_H_sup_data", "Select height column: ", choices = NULL)),
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
          br(),
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
            title = strong("Wood density (WD) extraction"), width = 4,
            radioButtons(
              "rad_WD", "Correct the taxonomy (mispelling) before wood density extraction?",
              c("Correct taxonomy and extract WD" = "corr",
                "Extract WD without taxonomic correction" = "WD")
            ) |>
              helper(colour = "#158A0C", content = "wd_extraction"),
            actionButton("btn_TAXO_RESULT", "Go on")
          )),
        fluidRow(
          hidden(boxWithId(
            id = "box_RESULT_TAXO", title = strong("Results"), width = 12,
            h4("Taxonomy corrections"),
            withSpinner(
              verbatimTextOutput("out_taxo_error"),
              type = getOption("spinner.type", default = 5),
              color = getOption("spinner.color", default = "#158A0C")),
            p(" "),
            hr(),
            h4("WD extraction"),
            verbatimTextOutput("out_wd_error"),
            p("Wood density values are assigned to each taxon by averaging the wood density values present in the reference database at species- or genus-level only if at least one wood density value is available."),
            p("For unidentified trees or if the genus is missing in the reference database, the plot-level mean wood density is assigned to the tree.")
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
            title = strong("Retrieving tree heights via a Height-Diameter model"),
            checkboxGroupInput(
              "chkgrp_HEIGHT",
              label = helper("Choose the HD model:         ?", colour = "#158A0C", content = "HD_model", size = "l"),
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
                    tableOutput("out_tab_HD") |>
                      helper(colour = "#158A0C", content = "local_HD_model"),
                    radioButtons("rad_HDMOD", "Choose your local HD model:", choices = "NULL")
                  ))
          ),
          column( 4 ,
                  ## Feldpausch
                  hidden(boxWithId(
                    width = 12,
                    id = "box_RESULT_FELD", title = "Feldpausch et al. (2012)",
                    #textOutput("txt_feld")
                    p("See below for the region(s) used in Feldpausch model:"),
                    tableOutput("out_tab_feld") |>
                      helper(colour = "#158A0C", content = "feld_region")
                  ))
          ),
          column( 4 ,
                  ## Chave
                  hidden(boxWithId(
                    width = 12,
                    id = "box_result_chave", title = "Chave et al. (2014)",
                    p("See below for the location of the plot(s) and the bioclimatic predictor E used in eqn 6a of Chave et al. 2014:"),
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
              plotOutput("out_plot_comp", height = "600px"), align = "center", width = 12,
              p("This graph presents a visual representation of the relationship between measured tree heights and diameter as dots, and between model predictions and diameter as lines.")
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
        ),
        fluidRow(
          column(width = 12,
                 br(),
                 p(" "),
                 br())
        )
      ),



      # AGB -----------------------------------------------------------------
      tabItem(
        "tab_AGB",
        fluidRow(
          box(
            title = strong("AGB estimation"), width = 3,
            radioButtons("rad_AGB_MOD", NULL, choices = c("AGB" = "agb", "AGB + error" = "agbe"), inline = T) |>
              helper(colour = "#158A0C", content = "error_prop"),
            p("AGB + error will calculate estimates of AGB with 95% confidence intervals, using error propagation."),
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
            downloadButton("dwl_tree_file", label = "Download tree level results"),
            downloadButton("dwl_plot_file", label = "Download plot level results"),
            downloadButton("dwl_report", label = "Downloal report") |>
              helper(colour = "#158A0C", content = "downloads", size = "l")
          ))
        )
      )
    )
  )
)

dashboardAddFooter(page, legalNotice(2025, "UMR AMAP"))
