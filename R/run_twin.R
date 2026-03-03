#' Launch the Digital Twin Shiny App
#' @export
run_twin <- function() {
  app_dir <- system.file("shiny-app", package = "CVmodel")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `CVmodel`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
