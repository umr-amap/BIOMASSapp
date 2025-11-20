# Modern BIOMASS Application UI with bslib
library(bslib)
library(shiny)
library(shinyjs)
library(shinyFeedback)
library(shinyhelper)
library(DT)
library(shinycssloaders)
library(leaflet)

# Define theme
app_theme <- bs_theme(
  version = 5,
  preset = "bootstrap",
  primary = "#459433",
  secondary = "#002d00",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Raleway"),
  code_font = font_google("Fira Code")
)

page <- page_sidebar(
  title = "BIOMASS Application",
  theme = app_theme,
  fillable = FALSE,

  # Sidebar with vertical navigation
  sidebar = sidebar(
    width = 250,
    bg = "#459433",

    tags$head(
      tags$style(HTML("
        /* background color of the main page */
        body, .bslib-page-sidebar {
          background-color: #f2f5f2 !important;
        }

        /* Title banner at the top */
        .navbar, .bslib-page-title {
          background-color: #002d00 !important;
          color: white !important;
        }

        /* sidebar styles */
        .bslib-sidebar-layout > .sidebar { color: white; }
        .bslib-sidebar-layout > .sidebar a { color: white; text-decoration: none; }
        .nav-item { margin-bottom: 8px; }
        .nav-link-custom {
          display: block;
          padding: 12px 15px;
          border-radius: 5px;
          transition: background-color 0.2s;
        }
        .nav-link-custom:hover {
          background-color: rgba(255,255,255,0.1);
        }
        .nav-link-custom.active {
          background-color: rgba(255,255,255,0.2);
          font-weight: bold;
        }
        .card {
          background-color: #f2f5f2 !important;
          overflow: visible !important; }
        .card-body {
          overflow: visible !important;
          max-height: none !important; }
        .custom_card_header {
          background-color: #459433;
          color: rgba(255,255,255,1);
          font-size: 20px; font-weight: bold; }

        .shiny-spinner > div > div {
          background-color: #459433 !important; }
      "))
    ),


    useShinyFeedback(),
    useShinyjs(),

    div(class = "nav-item",
        tags$a(
          class = "nav-link-custom active",
          id = "nav_link_load",
          href = "#",
          onclick = "Shiny.setInputValue('selected_tab', 'tab_LOAD', {priority: 'event'}); return false;",
          icon("upload"), " Load Dataset"
        )
    ),

    hidden(div(
      class = "nav-item",
      id = "nav_item_taxo",
      tags$a(
        class = "nav-link-custom",
        id = "nav_link_taxo",
        href = "#",
        onclick = "Shiny.setInputValue('selected_tab', 'tab_TAXO', {priority: 'event'}); return false;",
        icon("tree"), " Taxonomy & Wood Density"
      )
    )),

    hidden(div(
      class = "nav-item",
      id = "nav_item_height",
      tags$a(
        class = "nav-link-custom",
        id = "nav_link_height",
        href = "#",
        onclick = "Shiny.setInputValue('selected_tab', 'tab_HEIGHT', {priority: 'event'}); return false;",
        icon("chart-line"), " Height-Diameter Model"
      )
    )),

    hidden(div(
      class = "nav-item",
      id = "nav_item_agb",
      tags$a(
        class = "nav-link-custom",
        id = "nav_link_agb",
        href = "#",
        onclick = "Shiny.setInputValue('selected_tab', 'tab_AGB', {priority: 'event'}); return false;",
        icon("calculator"), " AGB Calculation"
      )
    )),

    hidden(div(
      class = "nav-item",
      id = "nav_item_spatial",
      tags$a(
        class = "nav-link-custom",
        id = "nav_link_spatial",
        href = "#",
        onclick = "Shiny.setInputValue('selected_tab', 'tab_SPATIALISATION', {priority: 'event'}); return false;",
        icon("map"), " Spatialized Metrics"
      )
    )),

    hidden(div(
      class = "nav-item",
      id = "nav_item_summary",
      tags$a(
        class = "nav-link-custom",
        id = "nav_link_summary",
        href = "#",
        onclick = "Shiny.setInputValue('selected_tab', 'tab_SP_SUMMARY', {priority: 'event'}); return false;",
        icon("chart-bar"), " Summarizing Metrics"
      )
    )),

    HTML('
         <div style="text-align: center;">
          <img src="logo/biomass_logo.png" width="70%" height="auto" >
         </div>'),

    legalNoticeBslib(2025, "UMR AMAP")
  ),

  # Load dataset ----
  div(
    id = "tab_LOAD",
    class = "tab-content",

    card(
      #card_header(h4("Getting Started",  style = "font-weight: bold;"), class = "bg-primary text-white"),
      card_header("Getting Started", class = "custom_card_header"),
      card_body(
        markdown("
To estimate the **Above Ground Biomass (AGB)** of a forest inventory, **3 parameters** are required:

- The **diameter** (DBH: Diameter at Breast Height for trees > 10 cm)
- The **wood density** (a method for estimating this parameter based on taxonomy is proposed when wood density data are not available)
- The **height** (three methods for estimating this parameter are proposed when height data are not available)
        ")
      )
    ),

    layout_columns(
      # width = 1/3,
      # heights_equal = "row",
      col_widths = c(3, 5, 4),

      ## Forest Inventory File Card ----
      card(
        width = 12,
        card_header("Forest Inventory File", class = "custom_card_header"),
        card_body(
          fileInput(
            "file_DATASET",
            "Choose a CSV file",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ) |> helper(colour = "#158A0C", content = "inv_dataset"),

          radioButtons( inputId = "rad_several_plots",
                        label = "Does your dataset contain several plots?",
                        choices = c(Yes = "several_plots", No = "single_plot"),
                        selected = character(0),
                        inline = FALSE, width = '100%'
          ),

          hidden(div(
            id = "id_sel_PLOT",
            p("Which column contains the plots IDs?"),
            selectInput("sel_PLOT", label = NULL, choices = NULL)
          )),

          hr(),

          p("Do you ", strong("need an example"), " of forest inventory data?"),
          p("Click the button below to download it."),
          downloadButton("dwl_inv_ex", "Download Example", class = "btn-outline-primary") |>
            helper(colour = "#158A0C", content = "inv_example", size = "l")
        )
      ),

      ## Required Parameters Card ----
      hidden(
        card(
          id = "box_FIELDS",
          card_header("Required Parameters", class = "custom_card_header"),
          card_body(
            ### Diameter ----
            h4("Diameter", class = "text-primary"),
            p("Select the column corresponding to the diameter of your trees:"),
            layout_columns(
              col_widths = c(6, 6),
              selectInput("sel_DIAMETER",
                          label = " ", choices = NULL) |>
                helper(colour = "#158A0C", content = "diameter"),
              radioButtons( inputId = "rad_units_diameter",
                            label = "Unit:",
                            choices = c("mm", "cm", "m"),
                            selected = "cm",
                            inline = TRUE
              )
            ),

            hr(),

            ### Wood Density ----
            h4("Wood Density or Taxonomy", class = "text-primary"),
            p("Select the column(s) corresponding to either the wood density or the taxonomy of your trees") |>
              helper(colour = "#158A0C", content = "wood_density"),

            layout_columns(
              col_widths = c(6, 6),
              selectInput("sel_WD", "Wood Density", choices = NULL),
              radioButtons(inputId = "rad_units_wd",
                           label = "Unit:",
                           choices = c("g.cm-3", "kg.m-3"),
                           selected = "g.cm-3",
                           inline = TRUE
              )
            ),

            hidden(
              div(
                id = "id_set_errWD",
                numericInput(
                  "set_errWD",
                  label = "Assumed error for wood density measurements",
                  value = 0.07,
                  min = 0, width = '100%'
                ) |> helper(colour = "#158A0C", content = "set_errWD")
              )
            ),

            h5("or", class = "text-left text-muted"),

            layout_columns(
              col_widths = c(6, 6),
              selectInput("sel_GENUS",
                          label = "Genus (e.g. Terminalia) or Scientific Name",
                          choices = NULL,
                          width = '100%'),
              selectInput("sel_SPECIES",
                          "Species (e.g. superba)",
                          choices = NULL,
                          width = '100%')
            ),

            div(
              id = "msg_wd",
              "Provide either wood density or taxonomy information",
              class = "text-left text-warning")
            #style = "color:warning;"
          ),


          hr(),

          ### Height Section ----
          h4("Height", class = "text-primary"),
          radioButtons( inputId = "rad_height",
                        label = "Do you have:",
                        choices = c(
                          "The height of each tree" = "h_each_tree",
                          "The height of some trees in the same dataset" = "h_some_tree",
                          "A subset of well-measured trees in another dataset" = "h_sup_data",
                          "No height measurements (use coordinates to estimate height)" = "h_none"
                        ),
                        width = '100%',
                        selected = character(0)
          ) |> helper(colour = "#158A0C", content = "rad_height"),

          # If height of each tree or some trees
          hidden(div(
            id = "id_sel_h",
            layout_columns(
              col_widths = c(6, 6),
              selectInput("sel_H", "Select height column", choices = NULL),
              radioButtons(inputId = "rad_units_height",
                           label = "Unit:",
                           choices = c("cm", "m"),
                           selected = "m",
                           inline = TRUE
              )
            )
          )),

          # set relative error if height of each trees
          hidden(div(
            id = "id_set_errH",
            numericInput(
              "set_errH",
              label = "Assumed relative error (%) for individual height measurements",
              value = 10, width = '100%'
            ) |> helper(colour = "#158A0C", content = "set_errH")
          )),

          hidden(div(
            id = "id_sel_HDmodel_by",
            p("Heights of non-measured trees will be estimated using Height-Diameter relationships.", class = "text-muted"),
            selectInput("sel_HDmodel_by", "Create HD model by plot or category:", choices = NULL)
          )),

          # If height in another dataset
          hidden(div(
            id = "id_file_h_sup",
            fileInput(
              "file_h_sup",
              "Upload supplementary Height-Diameter data (.csv)",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
              width = "100%"
            ),
            layout_columns(
              col_widths = c(6, 6),
              selectInput("sel_D_sup_data", "Diameter column:", choices = NULL),
              radioButtons(inputId = "rad_units_D_sup",
                           label = "Unit:",
                           choices = c("mm", "cm", "m"),
                           selected = "cm",
                           inline = TRUE
              )
            ),
            layout_columns(
              col_widths = c(6, 6),
              selectInput("sel_H_sup_data", "Height column:", choices = NULL),
              radioButtons(inputId = "rad_units_H_sup",
                           label = "Unit:",
                           choices = c("cm", "m"),
                           selected = "m",
                           inline = TRUE
              )
            )
          ))
        )
      ),

      ## Geographic Coordinates Card ----
      hidden(
        card(
          id = "box_COORD",
          card_header("Geographic Coordinates (Optional)", class = "custom_card_header"),
          card_body(
            markdown("
GPS coordinates are optional but will be used in two cases:

- To estimate tree heights when **height data is not available**
- To obtain spatialized Above Ground Biomass **Density** (AGB per hectare)

Coordinates (**latitude and longitude**) must be expressed in **decimal degrees**, e.g: (4.0849, -52.6844).
          "),

            radioButtons(inputId = "rad_coord",
                         label = "Do you have:",
                         choices = c(
                           "Coordinates of each tree" = "coord_each_tree",
                           "Coordinates of plot corners in another dataset" = "coord_plot",
                           "Coordinates of the zone/region" = "coord_manually",
                           "No coordinates" = "coord_none"
                         ),
                         width = '100%', selected = character(0)
            ) |>
              helper(colour = "#158A0C", content = "rad_height"),

            # If coordinates of each tree
            hidden(div(
              id = "id_sel_coord",
              layout_columns(
                col_widths = c(6, 6),
                selectInput("sel_LAT", "Latitude", choices = NULL),
                selectInput("sel_LONG", "Longitude", choices = NULL)
              )
            )),

            # If coordinates of plot(s)
            hidden(div(
              id = "id_file_coord",
              fileInput(
                "file_coord",
                "Upload coordinates CSV file",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
              )
            )),

            hidden(div(
              id = "id_sel_coord_plot",
              layout_columns(
                col_widths = c(6, 6),
                selectInput("sel_LAT_sup_coord", "Latitude", choices = NULL) |>
                  helper(colour = "#158A0C", content = "lat_sup_data"),
                selectInput("sel_LONG_sup_coord", "Longitude", choices = NULL)
              )
            )),

            # If coordinates of plots and several plots
            hidden(selectInput("sel_plot_coord", "Plot IDs", choices = NULL)),

            # If coordinates specified manually
            hidden(div(
              id = "id_num_lat_long",
              layout_columns(
                col_widths = c(6, 6),
                numericInput("num_lat", "Latitude", value = 0, min = -90, max = 90),
                numericInput("num_long", "Longitude", value = 0, min = -180, max = 180)
              )
            )),

            hr(),
            p("Do you ", strong("need an example")," of coordinates data?"),
            p("Click the button below to download it."),
            downloadButton("dwl_coord_ex", "Download Example", class = "btn-outline-primary") |>
              helper(colour = "#158A0C", content = "coord_example")
          )
        )
      )
    ),

    # Continue Button
    div(
      class = "text-center my-4",
      actionButton("btn_DATASET_LOADED", strong("Continue"), class = "btn-lg btn-primary text-white")
    ),

    ## Data Preview Cards ----
    hidden(
      card(
        id = "box_DATASET",
        card_header("Forest Inventory Data Preview"),
        card_body(
          div(
            DT::DTOutput("table_DATASET"),
            style = "max-height: 400px; overflow-y: auto;"
          )
        )
      )
    ),

    hidden(
      card(
        id = "box_coord_preview",
        card_header("Plot Coordinates Data Preview"),
        card_body(
          div(
            DT::DTOutput("table_coord"),
            style = "max-height: 400px; overflow-y: auto;"
          )
        )
      )
    ),

    hidden(
      card(
        id = "box_h_sup_preview",
        card_header("Height-Diameter Supplementary Data Preview"),
        card_body(
          div(DT::DTOutput("table_h_sup"),
              style = "max-height: 400px; overflow-y: auto;")
        )
      )
    )
  ),

  # TAXONOMY ----
  hidden(div(
    id = "tab_TAXO",
    class = "tab-content",

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header("Wood Density Extraction", class = "custom_card_header"),
        card_body(
          radioButtons(inputId = "rad_WD",
                       label = "Correct taxonomy before wood density extraction?",
                       choices = c( "Correct taxonomy and extract WD" = "corr",
                                    "Extract WD without taxonomic correction" = "WD"
                       ),
                       width="100%")
        ) |> helper(colour = "#158A0C", content = "wd_extraction"),

        actionButton("btn_TAXO_RESULT", strong("Process"), class = "btn-primary w-100 mt-3 text-white")
      )
    ),

    hidden(
      card(
        id = "box_RESULT_TAXO",
        card_header("Results", class = "custom_card_header"),
        card_body(
          h4("Taxonomy Corrections", class = "text-primary"),
          withSpinner(
            verbatimTextOutput("out_taxo_error"),
            type = 5,
            color = "#158A0C"
          ),

          hr(),

          h4("Wood Density Extraction", class = "text-primary"),
          verbatimTextOutput("out_wd_error"),
          markdown(
            "Wood density values are assigned to each taxon by averaging the values present in the reference database at species- or genus-level.

            For unidentified trees or if the genus is missing in the reference database, the plot-level mean wood density is assigned to the tree.")
        )
      )
    ),

    div(
      class = "text-center my-4",
      hidden(actionButton("btn_TAXO_DONE", strong("Continue"), class = "btn-lg btn-primary text-white"))
    )
  )),

  # H-D MODEL ----
  hidden(div(
    id = "tab_HEIGHT",
    class = "tab-content",

    card(
      card_header("Retrieving tree heights via a Height-Diameter model", class = "custom_card_header"),
      card_body(
        checkboxGroupInput(
          "chkgrp_HEIGHT",
          label = "Choose HD model(s):",
          inline = TRUE,
          choices = c(
            "Local HD model" = "HDloc",
            "Feldpausch" = "feld",
            "Chave" = "chave"
          )
        ) |> helper(colour = "#158A0C", content = "HD_model", size = "l")
      )
    ),

    layout_columns(
      col_widths = c(4, 4, 4),

      ## HD model
      hidden(
        card(
          id = "box_RESULT_HDMOD",
          card_header("Local HD Model", class = "custom_card_header"),
          card_body(
            DT::DTOutput("out_tab_HD") |>
              helper(colour = "#158A0C", content = "local_HD_model"),
            radioButtons(inputId = "rad_HDMOD",
                         label = "Choose your model:",
                         choices = "NULL")
          )
        )
      ),

      ## Feldpausch
      hidden(
        card(
          id = "box_RESULT_FELD",
          card_header("Feldpausch et al. (2012)", class = "custom_card_header"),
          card_body(
            p("Region(s) used in Feldpausch model:"),
            DT::DTOutput("out_tab_feld") |>
              helper(colour = "#158A0C", content = "feld_region")
          )
        )
      ),

      ## Chave
      hidden(
        card(
          id = "box_result_chave",
          card_header("Chave et al. (2014)", class = "custom_card_header"),
          card_body(
            p("Plot location and bioclimatic predictor E (eqn 6a):"),
            withSpinner(
              DT::DTOutput("out_tab_chave"),
              type = 5,
              color = "#158A0C"
            )
          )
        )
      )
    ),

    layout_columns(
      col_widths = c(8, 4),
      # Model Comparison
      hidden(
        card(
          id = "box_plot_comparison",
          full_screen = TRUE,
          card_header("Model Comparison", class = "custom_card_header"),
          card_body(
            plotOutput("out_plot_comp", height = "600px"),
            p("This graph presents a visual representation of the relationship between measured tree heights and diameter as dots, and between model predictions and diameter as lines.",
              class = "text-muted")
          )
        )
      ),

      # Map
      hidden(
        card(
          id = "box_MAP",
          card_header("Location Map", class = "custom_card_header"),
          card_body(
            withSpinner(
              leafletOutput("plot_MAP", height = "500px"),
              type = 5,
              color = "#158A0C"
            )
          )
        )
      )
    ),

    div(
      class = "text-center my-4",
      hidden(actionButton("btn_HD_DONE", strong("Continue"), class = "btn-lg btn-primary text-white"))
    )
  )),

  # AGB ESTIMATION ----
  hidden(div(
    id = "tab_AGB",
    class = "tab-content",

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header("AGB Estimation", class = "custom_card_header"),
        card_body(
          markdown("
AGB and its uncertainty are estimated with **error propagation** of the three main parameters:
- Diameter
- Wood density
- Height

The resulting confidence intervals represent the **2.5th and 97.5th percentiles** of all error propagation simulations.
          "),
          div(
            class = "text-center mt-4",
            actionButton("btn_calculate_AGB", "Calculate AGB", class = "btn-primary btn-lg text-white")
          )
        )
      ),

      hidden(
        card(
          id = "box_AGB_res",
          full_screen = TRUE,
          card_header("AGB Results", class = "custom_card_header"),
          card_body(
            withSpinner(
              plotOutput("out_plot_AGB", height = "500px"),
              type = 5,
              color = "#158A0C"
            )
          )
        )
      )
    ),

    hidden(
      div(
        id = "id_btn_continue_sp",
        class = "text-center my-4",
        actionButton("btn_continue_sp", strong("Continue to Spatialization"), class = "btn-lg btn-primary text-white")
      )
    ),

    hidden(
      div(
        id = "id_AGB_Report",
        class = "my-4",
        card(
          card_header("Download Results", class = "custom_card_header")|>
            helper(colour = "#158A0C", content = "downloads", size = "l"),
          card_body(
            layout_columns(
              col_widths = c(4, 4, 4),
              downloadButton("dwl_tree_file", "Tree Level Results", class = "btn-outline-primary w-100"),
              downloadButton("dwl_plot_file", "Plot Level Results", class = "btn-outline-primary w-100"),
              downloadButton("dwl_report", "Download Report", class = "btn-outline-primary w-100")
            )
          )
        )
      )
    )
  )),

  # SPATIALIZATION ----
  hidden(div(
    id = "tab_SPATIALISATION",
    class = "tab-content",

    card(
      card_body(
        markdown("
        This tab allows you to:

        - Validate plot corners and tree coordinates by visualization
        - Upload a raster file
        - Divide plot(s) into subplots" )
      )
    ),

    layout_columns(
      col_widths = c(8, 4),

      ## Plot visualisation box ----
      div(
        card(
          full_screen = TRUE,
          card_header("Plot Visualization", class = "custom_card_header"),
          card_body(
            div(
              style = "height: 800px;",
              plotOutput("out_gg_check_plot", height = "90%"),
              hidden(div(
                id = "id_sel_plot_display",
                selectInput("sel_plot_display", "Plot to display:", choices = NULL)
              ))
            )
          )
        )
      ),

      ## Settings box ----
      div(
        card(
          card_header("Settings", class = "custom_card_header"),
          card_body(

            ### Plot corners ----
            h4("Coordinates of plot corners", class = "text-primary"),
            # Selection of relative corner coordinates
            p("Select the columns corresponding to the relative coordinates of the corners (in meters):", class = "text-muted"),
            layout_columns(
              col_widths = c(6, 6),
              selectInput("sel_x_rel_corner", "Relative X", choices = NULL),
              selectInput("sel_y_rel_corner", "Relative Y", choices = NULL) |>
                helper(colour = "#158A0C", content = "sel_rel_coord_corners")
            ),

            # Checkboxes for check_plot_coord arguments
            hidden(
              div(
                id = "id_param_check_plot",
                checkboxInput(
                  "check_trust_GPS_corners",
                  "Trust GPS coordinates of corners",
                  value = TRUE
                ) |> helper(colour = "#158A0C", content = "trust_GPS_corners"),

                checkboxInput(
                  width='100%',
                  "check_max_dist",
                  "Multiple GPS measurements per corner",
                  value = FALSE
                )
              )),

            hidden(div( # numericInput for max_dist argument
              id = "id_max_dist",
              numericInput(
                "num_max_dist",
                "Maximum distance for outlier detection (m):",
                value = 15,
                min = 0.1, width='100%'
              ) |> helper(colour = "#158A0C", content = "max_dist")
            )),

            ### Tree Coordinates ----
            hidden(div(
              id = "id_coord_trees",
              hr(),
              h4("Tree Coordinates", class = "text-primary"),
              p("Select the column corresponding to the relative coordinates of the trees (in meters):", class = "text-muted"),
              layout_columns(
                col_widths = c(6, 6),
                selectInput("sel_x_rel_trees", "Relative X", choices = NULL),
                selectInput("sel_y_rel_trees", "Relative Y", choices = NULL) |>
                  helper(colour = "#158A0C", content = "sel_rel_coord_trees")
              ),
              selectInput("sel_prop_trees", "Select a tree metric to display proportionally:",
                          choices = NULL, width='100%')
            )),

            ###  Raster Upload ----
            hidden(div(
              id = "id_raster",
              hr(),
              h4("Raster File (Optional)", class = "text-primary") |>
                helper(colour = "#158A0C", content = "raster_file"),
              layout_columns(
                col_widths = c(9, 3),
                fileInput(
                  "file_RASTER",
                  "Upload a raster file",
                  accept = c(".tif", ".grd", ".jpg", ".jpeg", ".png", ".hgt", ".vrt", ".hdf", ".hdf5", ".adf")
                ),
                actionButton("btn_reset_raster", "Reset", style = "margin-top: 32px; background-color: white; color: black; border: 1px solid #ccc;" )
              )
            )),

            ### divide_plot settings ----
            hidden(div(
              id = "id_divide_plot",
              hr(),
              h4("Divide Plot (Optional)", class = "text-primary"),
              checkboxInput(
                "check_divide_plot",
                "Divide plot(s) into subplots",
                value = FALSE
              )
            )),

            hidden(div(
              id = "id_divide_plot_settings",
              numericInput("num_grid_size", "Grid size:", value = 50, min = 1)
              #checkboxInput("check_centred_grid", "Centre the grid", value = TRUE)
            ))
          )
        )
      )
    ),

    ## Action button to continue ----
    div(
      class = "text-center my-4",
      actionButton("btn_check_plot_done", strong("Continue"), class = "btn-lg btn-primary text-white")
    ),

    ## Coordinates data preview ----
    card(
      card_header("Plot Coordinates Data Preview"),
      card_body(
        div(
          DT::DTOutput("table_coord_spatialisation"),
          style = "max-height: 400px; overflow-y: auto;"
        ))
    ),
    ## Inventory data preview ----
    card(
      card_header("Forest Inventory Data Preview"),
      card_body(
        div(
          DT::DTOutput("table_indiv_pred"),
          style = "max-height: 400px; overflow-y: auto;"
        ))
    )
  )),

  # SUMMARY OF SPATIALISED METRICS ----
  hidden(div(
    id = "tab_SP_SUMMARY",
    class = "tab-content",

    layout_columns(
      col_widths = c(7, 5),

      ## Plot visualisation box ----
      card(
        full_screen = TRUE,
        card_header("Plot Visualization", class = "custom_card_header"),
        card_body(
          withSpinner(
            plotOutput("out_gg_subplot_sum", height = "600px"),
            type = 5,
            color = "#158A0C"
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            hidden(div(
              id = "id_sel_plot_summary",
              selectInput("sel_plot_display_summary", "Plot:", choices = NULL)
            )),
            selectInput("sel_metric_display_summary", "Metric:", choices = NULL),
            materialSwitch("switch_ggplot", "Switch visualization", value = TRUE)
          )
        )
      ),

      ## Settings ----
      card(
        card_header("Summarizing Tree Metrics", class = "custom_card_header"),
        card_body(
          markdown("
          AGB has been **automatically spatialized** (expressed as AGBD: AGB per hectare per plot division).

          However,You can also **summarize other metrics** (e.g., mean tree height per subplot).
          "),

          layout_columns(
            col_widths = c(5, 5, 2),
            selectInput("sel_first_metric", "Metric:", choices = NULL),
            selectInput("sel_first_function", "Function to apply:", choices = NULL),
            div(
              checkboxInput("check_first_per_ha", "per ha", value = TRUE),
              style = "margin-top: 32px;"),
          ) |> helper(colour = "#158A0C", content = "summarising_metrics"),

          div(id = "container_selec_metric"),

          actionButton("btn_add_metric", "Add Metric", class = "btn-outline-primary"),

          hidden(div(
            id = "id_raster_function",
            hr(),
            selectInput("sel_raster_function",
                        label = p("Function to applied to the values of the provided", strong("raster"),":"),
                        choices = names(available_functions), width='100%')
          )),

          hr(),

          div(
            class = "text-center",
            actionButton("btn_summarise", "Summarize!", class = "btn-primary btn-lg text-white")
          )
        )
      )
    ),

    ## Download results ----
    card(
      card_header("Download Results", class = "custom_card_header"),
      card_body(
        p("A list of metrics will be automatically calculated per subplot when downloading.", class = "text-muted") |>
          helper(colour = "#158A0C", content = "sp_downloads", size = "l"),
        layout_columns(
          col_widths = c(4, 4, 4),
          downloadButton("dwl_subplot_file", "CSV Results", class = "btn-outline-primary w-100"),
          downloadButton("dwl_shapefile", "Shapefile", class = "btn-outline-primary w-100"),
          downloadButton("dwl_sp_report", "Report", class = "btn-outline-primary w-100")
        )
      )
    ),

    ## Inventory data preview ----
    card(
      card_header("Forest Inventory Data Preview"),
      card_body(
        div(
          DT::DTOutput("table_divide_plot"),
          style = "max-height: 400px; overflow-y: auto;"
        )
      )
    )
  ))
)
