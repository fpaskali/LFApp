## desktop versions
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

## mobile versions
run_mobile_analysis <- function(){
  shinyApp(ui = analysis_mobile_ui, server = analysis_mobile_server)
}
run_mobile_cal <- function(){
  shinyApp(ui = cal_mobile_ui, server = cal_mobile_server)
}
run_mobile_core <- function(){
  shinyApp(ui = core_mobile_ui, server = core_mobile_server)
}
run_mobile_quan <- function(){
  shinyApp(ui = quan_mobile_ui, server = quan_mobile_server)
}
