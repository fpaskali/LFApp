#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_quan <- function(
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = quan_ui, 
      server = quan_server
    ), 
    golem_opts = list(...)
  )
}
