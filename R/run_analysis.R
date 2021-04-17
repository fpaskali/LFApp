run_analysis <- function(
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = analysis_ui, 
      server = analysis_server
    ), 
    golem_opts = list(...)
  )
}
