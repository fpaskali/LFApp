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
