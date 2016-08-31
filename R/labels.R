#' Label with help
#'
#' @export
label_with_help <- function(label, id, size=h4) {
  mylabel <- label
  myid <- id
  mytag <- paste0("#",myid," {vertical-align: top;}")
  size(mylabel,
       tags$style(type = "text/css",mytag),
       bsButton(myid,
                label = "",
                icon = icon("question"),
                style = "info",
                size = "extra-small"
       )
  )
}
