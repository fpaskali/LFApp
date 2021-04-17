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
