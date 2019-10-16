






output$ui_imputation_main_header <- renderUI({
  out <- NULL
  val <- obj$cur_selection_results
  ## Categorical (defined in controller/ui_results_imputation.R)
  if (val=="btn_impute_results_1") {
    return(uiOutput("ui_view_a_data_header"))
  }
  if (val=="btn_impute_results_2") {
    return(uiOutput("ui_random_forest_header"))
  }
  if (val=="btn_impute_results_3") {
    return( uiOutput("ui_knn_header"))
  }
  
  if (val=="btn_impute_results_4") {
    return(uiOutput("ui_f_analysis_header"))
  }
  #if (val=="btn_impute_results_5") {
  #return(uiOutput("ui_bivariate_tab_header"))
  #}
  #if (val=="btn_impute_results_6") {
  #return(uiOutput("ui_rescat_recodes_header"))
  #}
  #if (val=="btn_impute_results_7") {
  #return(uiOutput("ui_rescat_violating_kanon_header"))
  #}
  #out
})


output$ui_impute_sidebar_left <- renderUI({
  output$ui_sel_impute_btns <- renderUI({
    cc1 <- c("View Anonymized Data")
    cc2 <- c("Random Forests", "K neighbors KNN", "factorial analysis")
    #cc3 <- c("err.missforest", "err.knn", "err.missmda")
    
    df <- data.frame(lab=c(cc1,cc2), header=NA)
    df$header[1] <- "Anonymized Data"
    df$header[2] <- "Imputation of missing data resulting from the
    anonymisation"
    #df$header[5] <- "Erreur Imputation"
    
    out <- NULL
    for (i in 1:nrow(df)) {
      id <- paste0("btn_impute_results_",i)
      if (obj$cur_selection_results==id) {
        style <- "primary"
      } else {
        style <- "default"
      }
      if (!is.na(df$header[i])) {
        out <- list(out, fluidRow(column(12, h4(df$header[i]), align="center")))
      }
      out <- list(out, fluidRow(
        # TODO: See issue https://github.com/skounis/sdcMicro/issues/48
        # column(12, bsButton(id, label=df$lab[i], block=TRUE, size="extra-small", style=style), tags$br())
        column(12, bsButton(id, label=df$lab[i], block=TRUE, size="extra-small", style=style))
      ))
    }
    out
  })
  # required observers that update the color of the active button!
  eval(parse(text=genObserver_menus(pat="btn_impute_results_", n=1:4, updateVal="cur_selection_results")))
  return(uiOutput("ui_sel_impute_btns"))
})

output$ui_imputation <- renderUI({
  if (is.null(inputdata())) {
    return(list(
      noInputData(uri="ui_export_data"),
      fluidRow(column(12, tags$br(), p("or go to the Undo tab and upload a previously saved problem instance."), align="center")),
      fluidRow(column(12, myActionButton("nodata_script_uploadproblem", label="Upload a previously saved problem", btn.style="primary"), align="center"))))
  }
  if (is.null(sdcObj())) {
    return(list(
      noSdcProblem(uri="ui_export_data"),
      fluidRow(column(12, tags$br(), p("or go to the Undo tab and upload a previously saved problem instance."), align="center")),
      fluidRow(column(12, myActionButton("nodata_export_uploadproblem", label="Upload a previously saved problem", btn.style="primary"), align="center"))
    ))
  }
  return(fluidRow(
    column(2, uiOutput("ui_impute_sidebar_left"), class="wb_sidebar"),
    column(10, uiOutput("ui_imputation_main_header"), class="wb-maincolumn")))
  out
})
