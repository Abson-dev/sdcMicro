# UI-Output for exporting the imputation data
output$ui_exportimpute_data_btn <- renderUI({
  req(input$dat_expimpute_type,input$dataexport_path)
  if (!dir.exists(input$dataexport_path)) {
    return(myActionButton("btn_export_impute_data_xxx", "Error: The specified directory does not exist!", btn.style="danger"))
  }
  if (file.access(input$dataexport_path, mode=2)!=0) {
    return(myActionButton("btn_export_impute_data_xxx", "Error: The specified directory is not writeable!", btn.style="danger"))
  }
  return(myActionButton("btn_export_impute_data", "Save the imputation dataset", btn.style="primary"))
})

output$ui_export2_data <- renderUI({
  output$dt_exportimputeData <- DT::renderDataTable({
    #req(input$rb_export_randomizeorder)
    conditionalPanel(
      condition = "input.apply_missForest !=0",
      tabl<-table_filtree(),
      tmp<-tabl$ximp
      )
    conditionalPanel(
      condition = "input.apply_analysis !=0",
      tmp<-dffamd()
      )
    conditionalPanel(
      condition = "input.apply_Knn !=0",
      tmp<-table_KNN()
      )

    
    tmp
  }, options=list(scrollX=TRUE, lengthMenu=list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All')), pageLength=10), rownames=FALSE)
  
  # specific (gui)-options for csv-export
  output$ui_export2_csv <- renderUI({
    txt <- list("Set options for exporting a", code("csv"), "file")
    rb1 <- radioButtons("export2_csv_header", label=p("Include variable names in first row?"), choices=c("Yes"=TRUE,"No"=FALSE), inline=TRUE)
    rb2 <- radioButtons("export2_csv_sep", label=p("Field separator"), choices=c(Comma=",", Semicolon=";", Tab="\t"), inline=TRUE)
    rb3 <- radioButtons("export2_csv_dec", label=p("Decimal separator"), choices=c("Decimal point"=".", "Decimal comma"=","), inline=TRUE)
    return(fluidRow(
      column(12, p(txt, align="center")),
      column(4, rb1, align="center"),
      column(4, rb2, align="center"),
      column(4, rb3, align="center")))
  })
  output$ui_export2_spss <- renderUI({
    txt <- list("The file will be exported using", code("write_sav()"), "from package", code("haven"),".")
    fluidRow(column(12, p(txt, align="center")))
  })
  output$ui_export2_sas <- renderUI({
    txt <- list("The file will be exported using", code("write_sas()"), "from package", code("haven"),".")
    fluidRow(column(12, p(txt, align="center")))
  })
  output$ui_export2_stata <- renderUI({
    txt1 <- list("The file will be exported using", code("write_dta()"), "from package", code("haven"),".")
    rb1 <- radioButtons("export_dta_version2", label=p("To which version of STATA would you like to export?"), 
                        choices=c("Version 8" = 8,"Version 9" = 9, "Version 10" = 10, "Version 11" = 11,
                                  "Version 12" = 12, "Version 13" = 13, "Version 14" = 14), inline=TRUE)
    
    return(fluidRow(
      column(12, p(txt1, align="center")),
      column(12, rb1, align="center")))
  })
  rb_exptype2 <- radioButtons("dat_expimpute_type", label=p("Select file format for export", align="center"),
                             choices=c("R-dataset (.RData)"="rdata","SPSS-file (.sav)"="sav","CSV-file (.csv)"="csv", "STATA-file (.dta)"="dta", "SAS-file (.sas7bdat)"="sas"),
                             width="100%", selected=input$dat_expimpute_type, inline=TRUE)
  
  out <- fluidRow(
    column(12, h3("Export imputation microdata"), class="wb-header"),
    column(12, p("Select the file format to export the data to. If necessary, the order of the records can be randomized before exporting."), class="wb-header-hint")
  )
  
  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Trying to export the imputation data resulted in the following error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))), class = "wb-error-toast")
  }
  out <- list(out, fluidRow(
    column(12, h5("Imputation data"), align="center"),
    column(12, DT::dataTableOutput("dt_exportimputeData")),
    column(12, rb_exptype2, align="center")
  ))
  
  if (!is.null(input$dat_expimpute_type)) {
    if (input$dat_expimpute_type == "csv") {
      out <- list(out, uiOutput("ui_export2_csv"))
    }
    if (input$dat_expimpute_type == "sav") {
      out <- list(out, uiOutput("ui_export2_spss"))
    }
    if (input$dat_expimpute_type == "sas") {
      out <- list(out, uiOutput("ui_export2_sas"))
    }
    if (input$dat_expimpute_type == "dta") {
      out <- list(out, uiOutput("ui_export2_stata"))
    }
    
    # output$rb_randomize_export <- renderUI({
    #   curObj <- sdcObj()
    #   if (is.null(curObj)) {
    #     return(NULL)
    #   }
    #   txt_randomize     <- "Often the order of the records can be used to reconstruct the original data, such as suppressed values. To prevent this, it is advisable to randomize the order of records before releasing the data."
    #   txt_randomize     <- paste(txt_randomize, "In a subsequent step, the record IDs should be replaced, if any are available in the data.")
    #   txt_randomize_ind <- paste(txt_randomize, "The records are randomized at the record level.", tags$br(), tags$br(), "If this is the household level file to be merged with the individual level file, randomization should be postponed to the merged file.")
    #   txt_randomize_hh  <- paste(txt_randomize, "If a hierarchical structure is available in the data, randomization should occur by the hierarchcial identifier and, if necessary, within the hierachical units,")
    #   txt_randomize_hh  <- paste(txt_randomize_hh, " to preserve the hierarchical structure.", tags$br(), tags$br(), "Two options are available:", tags$br())
    #   txt_randomize_hh  <- paste(txt_randomize_hh, " - Randomize by hierarchical identifier: the order of the hierchical units is randomized and the order of the records within the hierarchical units is preserved", tags$br())
    #   txt_randomize_hh  <- paste(txt_randomize_hh, " - Randomize by hierarchical identifier and within hierarchical units: the order of the hierchical units is randomized as well as the order of the records within the hierarchcial units")
    #   choices <- c("No randomization"="no","Randomization at record level"="simple",
    #                "Randomize by hierarchical identifier"="byHH", "Randomize by hierarchical identifier and within hierarchical units"="withinHH")
    #   
    #   if (!is.null(curObj@hhId)) {
    #     rb <- radioButtons("rb_export_randomizeorder", label=p("Randomize order of records", tipify(icon("info-circle"), title=txt_randomize_hh, placement="top")), choices=choices[-2], inline=TRUE)
    #   } else {
    #     rb <- radioButtons("rb_export_randomizeorder", label=p("Randomize order of records", tipify(icon("info-circle"), title=txt_randomize_ind, placement="top")), choices=choices[1:2], inline=TRUE)
    #   }
    #   return(fluidRow(
    #     column(12, rb, align="center")
    #   ))
    # })
    #out <- list(out, uiOutput("rb_randomize_export"))
    out <- list(out, fluidRow(
      column(12, myActionButton("btn_export_impute_data", "Save dataset", btn.style="primary"), align="center")
    ))
    
    if (!is.null(obj$lastdataexport) & is.null(lastError())) {
      out <- list(out, fluidRow(
        column(12, tags$br(), p("The dataset was saved as", code(obj$lastdataexport)), align="center")))
    }
  }
  out
})