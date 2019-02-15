#' @export
app <- function() {
  shiny::runApp(system.file("shinyApp", package = "BIOMASSapp"), launch.browser = T)
}
