#' Power shiny dashboard
#'
#' @export

powerDashboard <- function() {
  shiny::runApp(system.file('power', package='MDES'))
}