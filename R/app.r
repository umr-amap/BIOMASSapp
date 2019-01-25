#' @export
app <- function() {
  shiny::runApp(system.file('shinyApp', package='BIOMASSapp'))
}
