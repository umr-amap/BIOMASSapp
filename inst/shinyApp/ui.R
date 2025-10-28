page <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "BIOMASS application"),
  dashboardSidebar(
    sidebarMenu(
      id = "mnu_MENU",
      menuItem("Spatialized metrics", tabName = "tab_SPATIALISATION"),
      menuItem("Summarising metrics", tabName = "tab_SP_SUMMARY"),
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

      # SPATIALISATION ---------------------------------------------------------
      tabItem(
        "tab_SPATIALISATION",
        ## Brief introduction text ----
        fluidRow(column(
          12,
          p("  This tab allows you to:"),
          p("- calculate the projected/geographic coordinates of the plot’s corners and the trees from the relative coordinates (or local coordinates, i.e. those of the field)"),
          p("- validate plot’s corners and tree coordinates by visualisation."),
          p("- divide plot(s) into subplots"),
          br(),
          actionButton("btn_skip", "AGB done !")
        )),

        fluidRow(
          ## Plot visualisation box ----
          column(
            8,
            box(
              title = h2(strong("Plot visualisation")),
              width = 12,
              plotOutput("out_gg_check_plot", height = "600px"),
              hidden(div(
                id = "id_sel_plot_display", column(4, selectInput("sel_plot_display", "Plot to display", choices = NULL))
              ))
            )),
          ## Settings box ----
          column(
            4,
            box(
              title = h1(strong("Settings")),
              width = 12,
              ### Plot corners ----
              fluidRow(
                column(12,
                       h3(strong("Coordinates of plot corners"))
                )),
              # Selection of relative corner coordinates
              fluidRow(
                column(12,
                       h6(" (see below for an overview)"),
                       p("Select the column corresponding to the relative coordinates of the corners:") ,
                       column(4, selectInput("sel_x_rel_corner", "relative X coordinates", choices = NULL)),
                       column(4, selectInput("sel_y_rel_corner", "relative Y coordinates", choices = NULL) |>
                                helper(colour = "#158A0C", content = "sel_rel_coord_corners"))
                )),

              # Checkbox for trust_GPS_corners argument
              fluidRow(
                column( 9,
                        checkboxInput(
                          inputId = "check_trust_GPS_corners",
                          label = "Do you trust the GPS coordinates of the plot's corners?",
                          value = TRUE )|>
                          helper(colour = "#158A0C", content = "trust_GPS_corners"))
              ),
              # Checkbox for repeated measurements
              fluidRow(
                column( 10,
                        checkboxInput(
                          inputId = "check_max_dist",
                          label = "Have you taken multiple GPS measurements at each corner?",
                          value = FALSE ))
              ),
              # numericInput for max_dist argument
              hidden(div(
                id = "id_max_dist",
                fluidRow(
                  column(9,
                         numericInput( "num_max_dist",
                                       h5("Provide the maximum distance (in meters) above which a corner should be considered outlier"),
                                       value = 15, min = 0.1)|>
                           helper(colour = "#158A0C", content = "max_dist"))
                )
              )),

              ### Plot trees ----
              hidden(div(
                id = "id_coord_trees",
                h3(strong("Coordinates of the trees")),
                # Selection of relative tree coordinates
                fluidRow(
                  column(
                    12,
                    h6(" (see below for an overview)"),
                    p("Select the column corresponding to the relative coordinates of the trees:"),
                    column(4, selectInput("sel_x_rel_trees", "relative X coordinates", choices = NULL)),
                    column(4, selectInput("sel_y_rel_trees", "relative Y coordinates", choices = NULL) |>
                             helper(colour = "#158A0C", content = "sel_rel_coord_trees"))
                  )),
                fluidRow(
                  column(7,
                         tags$br(),
                         p("Select a tree metric to display proportionally:")),
                  column(4, selectInput("sel_prop_trees","",choices = NULL)),
                )
              )),

              ### Raster file ----
              hidden(div(
                id = "id_raster",
                fluidRow(
                  column(6,
                         h3(strong("Raster (optional)")) |>
                           helper(colour = "#158A0C", content = "raster_file")
                  )),

                fluidRow(
                  column(6, fileInput("file_RASTER", "Upload a raster file",
                                      accept = c(".tif",".grd",".jpg",".jpeg",".png",".hgt",".vrt",".hdf",".hdf5",".adf"))),
                  column(2,
                         tags$br(),
                         actionButton("btn_reset_raster", "Reset"))
                )
              )),

              ### divide_plot settings ----
              hidden(div(
                id = "id_divide_plot",
                h3(strong("Dividing plot (optional)")),
                fluidRow(
                  column(
                    12,
                    checkboxInput(
                      inputId = "check_divide_plot",
                      label = "Do you want to divide your plot(s) into subplots ?",
                      value = FALSE
                    )
                  ))
              )),
              hidden(div(
                id = "id_divide_plot_settings",
                fluidRow(
                  column(
                    6,
                    column( 12,
                            numericInput("num_grid_size", "Grid size", value = 50, min = 1),
                            checkboxInput("check_centred_grid", "Centre the grid ?", value = TRUE)
                    )
                  ))
              ))
            )
          )),

        ## Action button to continue ----
        fluidRow(column(
          width = 12,
          actionButton("btn_check_plot_done", "Continue"),
          align = "center"
        )),

        ## Coordinates data preview ----
        fluidRow(
          br(),
          box(
            title = "Preview of plot's coordinates data",
            width = 12,
            DT::DTOutput("table_coord_spatialisation")
          )),

        ### Inventory data preview ----
        fluidRow(
          br(),
          box(
            title = "Preview of forest inventory data",
            width = 12,
            DT::DTOutput("table_indiv_pred")
          ))
      ),


      # SUMMARY OF SPATIALISED METRICS -----------------------------------------
      tabItem(
        "tab_SP_SUMMARY",
        fluidRow(
          ## Plot visualisation box ----
          column(7,
                 box(
                   title = h3(strong("Plot visualisation")),
                   width = 12,
                   withSpinner(
                     plotOutput("out_gg_subplot_sum", height = "600px"),
                     type = getOption("spinner.type", default = 5),
                     color = getOption("spinner.color", default = "#158A0C")
                   ),
                   hidden(div(
                     id = "id_sel_plot_summary",
                     column(4, selectInput("sel_plot_display_summary", "Plot to display", choices = NULL))
                   )),
                   hidden(div(
                     id = "id_sel_metric_summary",
                     column(4, selectInput("sel_metric_display_summary", "Metric to display", choices = NULL))
                   )),
                   hidden(div(
                     id = "id_switch_ggplot",
                     column(4, materialSwitch("switch_ggplot", strong("Switch plot visualisation")))
                   ))
                 ),
          ),
          ## Settings ----
          column(5,
                 box(
                   title = h3(strong("Summarising tree metrics")),
                   width = 12,
                   p("Provide the informations to be summarised:"),
                   column(12,
                          column(5, selectInput("sel_first_metric", "Which metric", choices = NULL)),
                          column(5, selectInput("sel_first_function", "Which function to apply?", choices = NULL)),
                          column(2, checkboxInput("check_first_per_ha", "per hectare", value = TRUE))) |>
                     helper(colour = "#158A0C", content = "summarising_metrics"),
                   div(id = "container_selec_metric"),
                   actionButton("btn_add_metric", "Add a metric"),

                   hidden(div(
                     id = "id_raster_function",
                     br(),
                     p("Which function should be applied to the values in the provided raster?"),
                     column(5, selectInput("sel_raster_function", "select a function", choices = NULL))
                   )),

                   column(
                     width = 12,
                     actionButton("btn_summarise", "Summarise !"),
                     align = "center"
                   )
                 )
          )
        ),

        ## Inventory data preview ----
        fluidRow(
          br(),
          box( title = "Preview of forest inventory data",
               width = 12,
               DT::DTOutput("table_divide_plot")
          ))
      ),



      # LOAD DATASET -----------------------------------------------------------

      tabItem(
        "tab_LOAD",
        fluidRow(column(
          12,
          p(
            "  To estimate the ",
            strong("above ground biomass (AGB)"),
            " of a forest inventory, ",
            strong("3 parameters"),
            " are required:"
          ),
          p(
            "- The ",
            strong("diameter"),
            "(DBH: Diameter at Breast Height for trees > 10 cm)"
          ),
          p(
            "- The ",
            strong("wood density"),
            " (a method for estimating this parameter based on taxonomy is proposed when wood density data are not available)"
          ),
          p(
            "- The ",
            strong("height"),
            " (three methods for estimating this parameter are proposed when height data are not available)"
          ),
          br()
        )),
        fluidRow(column(
          6,
          box(
            # Forest inventory file's box
            title = h3(strong("Forest inventory file")),
            width = 12,
            fileInput(
              "file_DATASET",
              "Choose a CSV file (see below for a preview)",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
            ) |>
              helper(colour = "#158A0C", content = "inv_dataset"),
            #numericInput("num_skip_line", "Skip lines", value = 0, min = 0),
            radioButtons(
              "rad_several_plots",
              "Does your dataset contain several plots?",
              choices = c(Yes = "several_plots", No = "single_plot"),
              selected = character(0)
            ),
            div("information required", id = "msg_several_plots", style = "color:red;"),
            hidden(
              selectInput("sel_PLOT", "Which column contains the plots IDs?", choices = NULL)
            ),
            br(),
            br(),
            p(
              "Do you need an example of forest inventory data? Click the button below."
            ),
            downloadButton("dwl_inv_ex", label = "Download an example") |>
              helper(
                colour = "#158A0C",
                content = "inv_example",
                size = "l"
              )
          ),

          ## Coordinate's box ----
          hidden(
            boxWithId(
              id = "box_COORD",
              title = h3(strong("Geographic coordinates"), "(optional)"),
              width = 12,
              p("GPS coordinates are optional but will be used in two cases:"),
              p("- To estimate tree heights when", strong("height data is not available"),"."),
              p("- To obtain spatialised Above Ground Biomass",strong("Density"),"(or any other tree metric), i.e. to calculate", strong("AGB per hectare"), "for plots or subplot divisions and download the associated shapefiles."),
              p("These coordinates - ", strong("latitude and longitude - "), "must be expressed in ", strong("decimal degrees"), ", e.g: (4.0849 ; -52.6844)."),

              radioButtons(
                "rad_coord",
                "Do you have:",
                choices = c(
                  "the columns corresponding to the coordinates of each tree" = "coord_each_tree",
                  "the coordinates of the plot(s) in another dataset" = "coord_plot",
                  "the coordinates of the plot or region that you want to specify manually" = "coord_manually",
                  "no coordinates" = "coord_none"
                ),
                selected = character(0)
              ),
              # If coordinates of each tree
              hidden(div(
                id = "id_sel_coord",
                selectInput("sel_LAT", "Latitude", choices = NULL),
                selectInput("sel_LONG", "Longitude", choices = NULL)
              )),
              # If coordinates of plot(s)
              hidden(div(
                id = "id_file_coord",
                fileInput(
                  "file_coord",
                  "Choose a CSV file (see below for a preview)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                )
              )),
              hidden(
                div(
                  id = "id_sel_coord_plot",
                  selectInput("sel_LAT_sup_coord", "Latitude", choices = NULL) |>
                    helper(colour = "#158A0C", content = "lat_sup_data"),
                  selectInput("sel_LONG_sup_coord", "Longitude", choices = NULL)
                )
              ),
              # If coordinates of plots and several plots
              hidden(selectInput("sel_plot_coord", "Plots IDs", choices = NULL)),
              # If coordinates specified manually
              hidden(div(
                id = "id_num_lat_long",
                numericInput(
                  "num_lat",
                  "Latitude",
                  value = 0,
                  min = -90,
                  max = 90
                ),
                numericInput(
                  "num_long",
                  "Longitude",
                  value = 0,
                  min = -180,
                  max = 180
                )
              )),
              br(),
              br(),
              p("Do you need an example of coordinates data? Click the button below."),
              downloadButton("dwl_coord_ex", label = "Download an example") |>
                helper(colour = "#158A0C", content = "coord_example")
            )
          )
        ), column(6, hidden(
          boxWithId(
            # AGB inputs box
            id = "box_FIELDS",
            title = h3(strong("Required parameters")),
            width = 12,
            column(12, hr()),

            ## Diameter (compulsory) ----
            h4(strong("Diameter")),
            p("Select the column corresponding to the diameter of your trees."),
            column(9, selectInput("sel_DIAMETER", "", choices = NULL)) |>
              helper(colour = "#158A0C", content = "diameter"),
            column(
              3,
              radioButtons(
                "rad_units_diameter",
                "Unit:",
                choices = c("mm", "cm", "m"),
                selected = "cm"
              )
            ),
            column(12, hr()),

            ## Wood density or taxonomy (compulsory) ----
            h4(strong("Wood density or taxonomy")) ,
            p(
              "Select the column(s) corresponding to ",
              strong("either "),
              "the wood density ",
              strong("or "),
              "the taxonomy of your trees"
            ) |>
              helper(colour = "#158A0C", content = "wood_density"),
            column(9, selectInput("sel_WD", "Wood density", choices = NULL)),
            column(
              3,
              radioButtons(
                "rad_units_wd",
                "Unit:",
                choices = c("g.cm-3", "kg.m-3"),
                selected = "g.cm-3"
              )
            ),
            column(12, hidden(
              div(
                id = "id_set_errWD",
                numericInput(
                  "set_errWD",
                  label = "What is the assumed error associated with the wood densities measurements ?",
                  value = 0.07,
                  min = 0
                ) |>
                  helper(colour = "#158A0C", content = "set_errWD")
              )
            )),
            h5(strong("or")),
            column(
              12,
              selectInput(
                "sel_GENUS",
                "Genus (e.g. Terminalia) or scientific name (e.g. Terminalia superba or Terminalia superba Engl. & Diels)",
                choices = NULL
              )
            ),
            # column to keep the same layout than Diameter and Wood density selections
            column(
              12,
              selectInput("sel_SPECIES", "Species (e.g. superba)", choices = NULL)
            ),
            div(
              "Provide either wood density or taxonomy information ",
              id = "msg_wd",
              style = "color:red;"
            ),
            column(12, hr()),

            ## Height ----
            h4(strong("Height")),
            column(
              12,
              radioButtons(
                "rad_height",
                "Do you have:",

                choices = c(
                  "The height of each tree" = "h_each_tree",
                  "The height of some trees in the same dataset" = "h_some_tree",
                  "A subset of well-measured trees in another dataset" = "h_sup_data",
                  "No height measurements (use coordinates to estimate height)" = "h_none"
                ),
                selected = character(0)
              ) |>
                helper(colour = "#158A0C", content = "rad_height")
            ),
            # If height of each tree or some trees
            hidden(div(
              id = "id_sel_h",
              column(9 , selectInput("sel_H", "Select height column", choices = NULL)),
              column(
                3,
                radioButtons(
                  "rad_units_height",
                  "Unit:",
                  choices = c("cm", "m"),
                  selected = "m"
                )
              ),
              column(9, numericInput(
                "set_errH",
                label = helper(
                  shiny_tag = "What is the assumed relative error (in %) associated with individual height measurements ?",
                  colour = "#158A0C",
                  content = "set_errH"
                ),
                value = 10
              ))
              ,
              column(3, p(""))
            )),

            hidden(div(id = "id_sel_HDmodel_by", column(
              12,
              h5(
                "The heights of non-measured trees will be estimated using Height-Diameter relationships on measured trees."
              ),
              h5(
                "If you want to create a Height-Diameter model by plot (or by any category), please specify the column corresponding to the plot (or category) IDs:"
              ),
              selectInput("sel_HDmodel_by", "", choices = NULL)
            ))),
            # If height in another dataset
            hidden(
              div(
                id = "id_file_h_sup",
                column(
                  12,
                  fileInput(
                    "file_h_sup",
                    "Choose a CSV file (see below for a preview)",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                  )
                ),
                column(
                  9 ,
                  selectInput("sel_D_sup_data", "Select diameter column:", choices = NULL)
                ),
                column(
                  3,
                  radioButtons(
                    "rad_units_D_sup",
                    "Unit:",
                    choices = c("mm", "cm", "m"),
                    selected = "cm"
                  )
                ),
                column(
                  9 ,
                  selectInput("sel_H_sup_data", "Select height column: ", choices = NULL)
                ),
                column(
                  3,
                  radioButtons(
                    "rad_units_H_sup",
                    "Unit:",
                    choices = c("cm", "m"),
                    selected = "m"
                  )
                )
              )
            )
          )
        ))),

        # Action button to continue
        fluidRow(column(
          width = 12,
          actionButton("btn_DATASET_LOADED", "Continue"),
          align = "center"
        )),

        # Inventory data preview
        fluidRow(br(), hidden(
          boxWithId(
            id = "box_DATASET",
            title = "Preview of forest inventory data",
            width = 12,
            DT::DTOutput("table_DATASET")
          )
        )),
        # Coordinates data preview
        fluidRow(hidden(
          boxWithId(
            id = "box_coord_preview",
            title = "Preview of plot's coordinates data",
            width = 12,
            DT::DTOutput("table_coord")
          )
        )),
        # Supplementary H-D data preview
        fluidRow(hidden(
          boxWithId(
            id = "box_h_sup_preview",
            title = "Preview of Height-Diameter supplementary data",
            width = 12,
            DT::DTOutput("table_h_sup")
          )
        ))
      ),



      # TAXONOMY ---------------------------------------------------------------

      tabItem(
        "tab_TAXO",
        fluidRow(
          box(
            title = strong("Wood density (WD) extraction"),
            width = 4,
            radioButtons(
              "rad_WD",
              "Correct the taxonomy (mispelling) before wood density extraction?",
              c(
                "Correct taxonomy and extract WD" = "corr",
                "Extract WD without taxonomic correction" = "WD"
              )
            ) |>
              helper(colour = "#158A0C", content = "wd_extraction"),
            actionButton("btn_TAXO_RESULT", "Go on")
          )
        ),
        fluidRow(hidden(
          boxWithId(
            id = "box_RESULT_TAXO",
            title = strong("Results"),
            width = 8,
            column(
              12,
              fluidRow(
                h4("Taxonomy corrections"),
                withSpinner(
                  verbatimTextOutput("out_taxo_error"),
                  type = getOption("spinner.type", default = 5),
                  color = getOption("spinner.color", default = "#158A0C")
                )
              ),
              fluidRow(
                h4("WD extraction"),
                verbatimTextOutput("out_wd_error"),
                p("Wood density values are assigned to each taxon by averaging the wood density values present in the reference database at species- or genus-level only if at least one wood density value is available."),
                p("For unidentified trees or if the genus is missing in the reference database, the plot-level mean wood density is assigned to the tree.")
              )
            )
          )
        )),
        fluidRow(column(
          width = 12, hidden(actionButton("btn_TAXO_DONE", "Continue")), align = "center"
        ))
      ),


      # Height -----------------------------------------------------------------

      tabItem(
        "tab_HEIGHT",
        fluidRow(box(
          title = strong("Retrieving tree heights via a Height-Diameter model"),
          checkboxGroupInput(
            "chkgrp_HEIGHT",
            label = helper(
              "Choose the HD model:         ?",
              colour = "#158A0C",
              content = "HD_model",
              size = "l"
            ),
            inline = T,
            c(
              "Local HD model" = "HDloc",
              "Feldpausch" = "feld",
              "Chave" = "chave"
            )
          )
        )),
        fluidRow(column(
          4 ,
          ## HD model
          hidden(
            boxWithId(
              width = 12,
              id = "box_RESULT_HDMOD",
              title = "Local HD model",
              # (model accuracy is estimated on all HD data)
              tableOutput("out_tab_HD") |>
                helper(colour = "#158A0C", content = "local_HD_model"),
              radioButtons("rad_HDMOD", "Choose your local HD model:", choices = "NULL")
            )
          )
        ), column(
          4 ,
          ## Feldpausch
          hidden(
            boxWithId(
              width = 12,
              id = "box_RESULT_FELD",
              title = "Feldpausch et al. (2012)",
              #textOutput("txt_feld")
              p("See below for the region(s) used in Feldpausch model:"),
              tableOutput("out_tab_feld") |>
                helper(colour = "#158A0C", content = "feld_region")
            )
          )
        ), column(
          4 ,
          ## Chave
          hidden(
            boxWithId(
              width = 12,
              id = "box_result_chave",
              title = "Chave et al. (2014)",
              p(
                "See below for the location of the plot(s) and the bioclimatic predictor E used in eqn 6a of Chave et al. 2014:"
              ),
              withSpinner(
                tableOutput("out_tab_chave"),
                type = getOption("spinner.type", default = 5),
                color = getOption("spinner.color", default = "#158A0C")
              )
            )
          )
        )),
        fluidRow(
          ## Comparison of the methods and map if Chave is ticked
          column(width = 8, hidden(
            boxWithId(
              id = "box_plot_comparison",
              title = "Model comparison",
              plotOutput("out_plot_comp", height = "600px"),
              align = "center",
              width = 12,
              p(
                "This graph presents a visual representation of the relationship between measured tree heights and diameter as dots, and between model predictions and diameter as lines."
              )
            )
          )),
          ## Map
          column(width = 4, hidden(
            boxWithId(
              id = "box_MAP",
              title = "Map",
              width = 12,
              withSpinner(
                leafletOutput(outputId = "plot_MAP"),
                type = getOption("spinner.type", default = 5),
                color = getOption("spinner.color", default = "#158A0C")
              )
            )
          ))
        ),
        fluidRow(column(
          width = 12, hidden(actionButton("btn_HD_DONE", "Continue")), align = "center"
        )),
        fluidRow(column(width = 12, br(), p(" "), br()))
      ),



      # AGB -----------------------------------------------------------------
      tabItem("tab_AGB", fluidRow(
        box(
          title = strong("AGB estimation"),
          width = 6,
          p( "AGB and its uncertainty are estimated with error propagation of the three main parameters (diameter, wood density and height)."),
          p("The resulting confidence intervals will represent the 2.5th and 97.5th percentiles of all error propagation simulations."),
          column(
            width = 12,
            actionButton("btn_AGB_DONE", "Go on"),
            align="center"
          )
        ),
        hidden(
          boxWithId(
            id = "box_AGB_res",
            title = "AGB result",
            width = 12,
            withSpinner(
              plotOutput("out_plot_AGB"),
              type = getOption("spinner.type", default = 5),
              color = getOption("spinner.color", default = "#158A0C")
            )
          )
        )
      ),
      # Action button to continue
      fluidRow(
        hidden( div(
          id = "id_btn_continue_sp",
          column(
            width = 12,
            actionButton("btn_continue_sp", "Continue to spatialisation"),
            align = "center"
          )
        ))
      ),
      fluidRow(
        hidden( div (
          id = "id_AGB_Report",
          column(
            width = 2,
            downloadButton("dwl_tree_file", label = "Download tree level results"),
            downloadButton("dwl_plot_file", label = "Download plot level results"),
            downloadButton("dwl_report", label = "Downloal report") |>
              helper(
                colour = "#158A0C",
                content = "downloads",
                size = "l"
              ),
            p(" ")
          )
        ))
      )
      )
    )
  )
)

dashboardAddFooter(page, legalNotice(2025, "UMR AMAP"))
