#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options

run_cal <- function(
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = cal_ui, 
      server = cal_server
    ), 
    golem_opts = list(...)
  )
}
