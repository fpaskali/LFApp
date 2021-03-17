#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_core <- function(
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = core_ui, 
      server = core_server
    ), 
    golem_opts = list(...)
  )
}
