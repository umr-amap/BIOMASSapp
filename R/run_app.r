#' run_app
#'
#' @description Runs the BIOMASS app
#'
#' @importFrom shiny runApp
#'
#' @export
#'
run_app <- function() {
  shiny::runApp(system.file("shinyApp", package = "BIOMASSapp"), launch.browser = T)
}
