run_analysis <- function(){
  shinyApp(ui = analysis_ui, server = analysis_server)
}
run_cal <- function(){
  shinyApp(ui = cal_ui, server = cal_server)
}
run_core <- function(){
  shinyApp(ui = core_ui, server = core_server)
}
run_quan <- function(){
  shinyApp(ui = quan_ui, server = quan_server)
}
