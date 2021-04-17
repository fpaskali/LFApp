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
