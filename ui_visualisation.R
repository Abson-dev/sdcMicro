output$ui_catvar_I1 <- renderUI({
  #Keys_Var <- c(get_keyVars_names(), get_pramVars_names())
  Keys_Var <- c(colnames(get_origData()), get_pramVars_names())
  if (length(Keys_Var)>1) {
    Keys_Var <- setdiff(Keys_Var, input$sel_catvar_I2)
  }
  if (is.null(input$sel_catvar_I1)) {
    sel <- Keys_Var[1]
  } else {
    sel <- input$sel_catvar_I1
  }
  selectInput("sel_catvar_I1", label=p("Variable 1"), choices=Keys_Var, selected=sel, width="100%")
})
output$ui_catvar_I2 <- renderUI({
  Keys_Var <- c("none",setdiff(c(get_keyVars_names(), get_pramVars_names()), input$sel_catvar_I1))
  if (length(Keys_Var)==0) {
    return(NULL)
  }
  if (is.null(input$sel_catvar_I2)) {
    sel <- "none"
  } else {
    if (input$sel_catvar_I2==input$sel_catvar_I1) {
      sel <- "none"
    } else {
      sel <- input$sel_catvar_I2
    }
  }
  selectInput("sel_catvar_I2", label=p("Variable 2"), choices=Keys_Var, selected=sel, width="100%")
})



# UI output for bivariate tabulation of (modified) key variables
output$ui_select_none <- renderUI({})
#MissForest#############################"
output$ui_select_tab <- renderUI({
  if (input$visualize == "Tabulations"){
  out <- fluidRow(
    column(12, p("Tabular representation of original and modified data"), offset = 0, class = "wb-header"),
    column(12, p("Here you can view univariate and bivariate tabulations of categorical key variables to compare the variables before and after applying anonymization methods."), offset = 0, class = "wb-header-hint")
  )
  output$ui_biv_selectionI<- renderUI({
    fluidRow(
      column(6, uiOutput("ui_catvar_I1"), align="center"),
      column(6, uiOutput("ui_catvar_I2"), align="center"))
  })
  output$biv_tab_oI <- renderTable({
    req(input$sel_catvar_I1)
    df <- get_origData()
    vars <- c(input$sel_catvar_I1, input$sel_catvar_I2)
    if (vars[2]=="none") {
      tab <- as.data.frame.table(addmargins(table(df[[vars[1]]], useNA="always")))
      tab$Freq <- as.integer(tab$Freq)
      colnames(tab) <- c(vars[1], "Freq")
    } else {
      tab <- as.data.frame.matrix(formatC(addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA = "always")), format = "f", digits = 0))
      colnames(tab)[is.na(colnames(tab))] <- "NA"
      rownames(tab)[is.na(rownames(tab))] <- "NA"
      tab <- cbind(rownames(tab), tab)
      colnames(tab)[1] <- ""
    }
    tab
  }, include.rownames = FALSE)

  output$biv_tab_anonI <- renderTable({
    req(input$sel_catvar_I1,input$apply_missForest)
    df <- exportData()
    vars <- c(input$sel_catvar_I1, input$sel_catvar_I2)
    if (vars[2]=="none") {
      tab <- as.data.frame.table(addmargins(table(df[[vars[1]]], useNA="always")))
      tab$Freq <- as.integer(tab$Freq)
      colnames(tab) <- c(vars[1], "Freq")
    } else {
      tab <- as.data.frame.matrix(formatC(addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA = "always")), format = "f", digits = 0))
      colnames(tab)[is.na(colnames(tab))] <- "NA"
      rownames(tab)[is.na(rownames(tab))] <- "NA"
      tab <- cbind(rownames(tab), tab)
      colnames(tab)[1] <- ""
    }
    tab
  }, include.rownames = FALSE)
  
  
  output$biv_tab_resII <- renderPrint({
    req(input$sel_catvar_I2)
    if(!is.null(input$sel_catvar_I2) & input$sel_catvar_I2 == "none"){
      tabs <- fluidRow(column(6, h4("Original data"), align="center"),
                       column(6, tableOutput("biv_tab_oI")),
                       column(6, h4("Anonymized data"), align="center"),
                       column(6, tableOutput("biv_tab_anonI")),
                       column(6, h4("Imputation data"), align="center"),
                       column(6, tableOutput("biv_tab_mI")))
    } else {
      tabs <- fluidRow(column(12, h4("Original data"), align="center"),
                       column(12, tableOutput("biv_tab_oI"), align="center", class="wn-info-table wn-row-title"),
                       column(12, h4("Anonymized data"), align="center"),
                       column(12, tableOutput("biv_tab_anonI"), align="center", class="wn-info-table wn-row-title"),
                       column(12, h4("Imputation data"), align="center"),
                       column(12, tableOutput("biv_tab_mI"), align="center", class="wn-info-table wn-row-title"))
    }
    tabs
  })

  out <- list(uiOutput("ui_biv_selectionI"), uiOutput("biv_tab_resII"))

  out
  }
})

table_filtree <- reactive({
  req(input$apply_missForest)
  data_anon<-exportData()
  missF<-missForest(data_anon,maxiter=input$input_max,ntree=input$input_ntree)
  missF
  #data_impute<-missF$ximp
  #data_impute
})



observeEvent(input$apply_missForest, {
   input$input_max
  input$input_ntree
  
  
})  
    ##################

dfforest <- eventReactive(input$apply_missForest, {
  #req(input$apply_missForest)
  #missF<-missForest(exportData(),maxiter=input$input_max,ntree=input$input_ntree)
  tabl<-table_filtree()
  data_impute<-tabl$ximp
  data_impute
})

dfplot<-eventReactive(input$apply_missForest,{
  #missF<-missForest(exportData(),maxiter=input$input_max,ntree=input$input_ntree)
  tabl<-table_filtree()
  errors_Imputation<-tabl$OOBerror
  error1 = data.frame(errors_Imputation)
  if(length(errors_Imputation)!=1){
    error2 = data.frame(
      colonne = row.names(error1),
      Error_Imputation = c(errors_Imputation[1],errors_Imputation[2]))}
  if(length(errors_Imputation)==1){error2 = data.frame(
    colonne = row.names(error1),
    Error_Imputation = c(errors_Imputation[1]))}
  a <- ggplot(error2, aes(x = colonne, y = Error_Imputation)) 
  a + geom_bar(aes(fill = colonne), stat="identity",width = .5)
})




output$table <- DT::renderDataTable({
  dfforest()
}, options=list(scrollX=TRUE, lengthMenu=list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All')), pageLength=10), rownames=FALSE)


output$ui_select_eror_IMissF <- renderPlot({
  dfplot()
})
output$biv_tab_mI <- renderTable({
  req(input$sel_catvar_I1)
  tabl<-table_filtree()
  df<-tabl$ximp
  vars <- c(input$sel_catvar_I1, input$sel_catvar_I2)
  if (vars[2]=="none") {
    tab <- as.data.frame.table(addmargins(table(df[[vars[1]]], useNA="always")))
    tab$Freq <- as.integer(tab$Freq)
    colnames(tab) <- c(vars[1], "Freq")
  } else {
    tab <- as.data.frame.matrix(formatC(addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA = "always")), format = "f", digits = 0))
    colnames(tab)[is.na(colnames(tab))] <- "NA"
    rownames(tab)[is.na(rownames(tab))] <- "NA"
    tab <- cbind(rownames(tab), tab)
    colnames(tab)[1] <- ""
  }
  tab
}, include.rownames = FALSE)




#KNN
table_KNN <- reactive({
  req(input$apply_Knn)
  anondata<-exportData()
  knn=kNN(anondata,k=input$input_k,imp_var = FALSE)
  knn
})

output$ui_select_tabKNN <- renderUI({
  if (input$visualizeKNN == "Tabulations"){
    out <- fluidRow(
      column(12, p("Tabular representation of original and modified data"), offset = 0, class = "wb-header"),
      column(12, p("Here you can view univariate and bivariate tabulations of categorical key variables to compare the variables before and after applying anonymization methods."), offset = 0, class = "wb-header-hint")
    )
    output$ui_biv_selectionI<- renderUI({
      fluidRow(
        column(6, uiOutput("ui_catvar_I1"), align="center"),
        column(6, uiOutput("ui_catvar_I2"), align="center"))
    })
    output$biv_tab_oI <- renderTable({
      req(input$sel_catvar_I1)
      df <- get_origData()
      vars <- c(input$sel_catvar_I1, input$sel_catvar_I2)
      if (vars[2]=="none") {
        tab <- as.data.frame.table(addmargins(table(df[[vars[1]]], useNA="always")))
        tab$Freq <- as.integer(tab$Freq)
        colnames(tab) <- c(vars[1], "Freq")
      } else {
        tab <- as.data.frame.matrix(formatC(addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA = "always")), format = "f", digits = 0))
        colnames(tab)[is.na(colnames(tab))] <- "NA"
        rownames(tab)[is.na(rownames(tab))] <- "NA"
        tab <- cbind(rownames(tab), tab)
        colnames(tab)[1] <- ""
      }
      tab
    }, include.rownames = FALSE)
    
    output$biv_tab_anonI <- renderTable({
      req(input$sel_catvar_I1,input$apply_Knn)
      df <- exportData()
      vars <- c(input$sel_catvar_I1, input$sel_catvar_I2)
      if (vars[2]=="none") {
        tab <- as.data.frame.table(addmargins(table(df[[vars[1]]], useNA="always")))
        tab$Freq <- as.integer(tab$Freq)
        colnames(tab) <- c(vars[1], "Freq")
      } else {
        tab <- as.data.frame.matrix(formatC(addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA = "always")), format = "f", digits = 0))
        colnames(tab)[is.na(colnames(tab))] <- "NA"
        rownames(tab)[is.na(rownames(tab))] <- "NA"
        tab <- cbind(rownames(tab), tab)
        colnames(tab)[1] <- ""
      }
      tab
    }, include.rownames = FALSE)
    
    
    output$biv_tab_resII <- renderPrint({
      req(input$sel_catvar_I2)
      if(!is.null(input$sel_catvar_I2) & input$sel_catvar_I2 == "none"){
        tabs <- fluidRow(column(6, h4("Original data"), align="center"),
                         column(6, tableOutput("biv_tab_oI")),
                         column(6, h4("Anonymized data"), align="center"),
                         column(6, tableOutput("biv_tab_anonI")),
                         column(6, h4("Imputation data"), align="center"),
                         column(6, tableOutput("biv_tab_mKNN")))
      } else {
        tabs <- fluidRow(column(12, h4("Original data"), align="center"),
                         column(12, tableOutput("biv_tab_oI"), align="center", class="wn-info-table wn-row-title"),
                         column(12, h4("Anonymized data"), align="center"),
                         column(12, tableOutput("biv_tab_anonI"), align="center", class="wn-info-table wn-row-title"),
                         column(12, h4("Imputation data"), align="center"),
                         column(12, tableOutput("biv_tab_mKNN"), align="center", class="wn-info-table wn-row-title"))
      }
      tabs
    })
    
    out <- list(uiOutput("ui_biv_selectionI"), uiOutput("biv_tab_resII"))
    
    out
  }
})
observeEvent(input$apply_Knn, {
  input$input_k
  
})

dfplotknn<-eventReactive(input$apply_Knn,{
  table<-table_KNN()
  errors_Imputationknn<-mixError(ximp=table, xmis=exportData(), xtrue=get_origData())
  error1 = data.frame(errors_Imputationknn)
  if(length(errors_Imputationknn)!=1){
    error2 = data.frame(
      colonne = row.names(error1),
      Error_Imputationknn = c(errors_Imputationknn[1],errors_Imputationknn[2]))}
  if(length(errors_Imputationknn)==1){error2 = data.frame(
    colonne = row.names(error1),
    Error_Imputationknn = c(errors_Imputationknn[1]))}
  b <- ggplot(error2, aes(x = colonne, y = Error_Imputationknn)) 
  b + geom_bar(aes(fill = colonne), stat="identity",width = .5)
  #plot(errors_Imputation)
  # data<-rbind(errors_Imputation)
  # a<-barplot(t(data), beside=T, ylim=c(0,1), col=c("blue","red"),ylab="Erreur Imputation")
  # a+legend(x="topleft",legend=colnames(data), cex=0.4, fill=c("blue","red"))
})

output$ui_select_eror_IKNN <- renderPlot({
  dfplotknn()
})


output$biv_tab_mKNN <- renderTable({
  req(input$sel_catvar_I1)
  df<-table_KNN()
  vars <- c(input$sel_catvar_I1, input$sel_catvar_I2)
  if (vars[2]=="none") {
    tab <- as.data.frame.table(addmargins(table(df[[vars[1]]], useNA="always")))
    tab$Freq <- as.integer(tab$Freq)
    colnames(tab) <- c(vars[1], "Freq")
  } else {
    tab <- as.data.frame.matrix(formatC(addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA = "always")), format = "f", digits = 0))
    colnames(tab)[is.na(colnames(tab))] <- "NA"
    rownames(tab)[is.na(rownames(tab))] <- "NA"
    tab <- cbind(rownames(tab), tab)
    colnames(tab)[1] <- ""
  }
  tab
}, include.rownames = FALSE)












dfknn <- eventReactive(input$apply_Knn, {
  table_KNN()
})
output$tableKnn <- DT::renderDataTable({
  dfknn()
})



#FAMD
dffamd <- reactive({
  if(input$input_method=="reg"){
    dataI<-imputeFAMD(exportData(),ncp=input$input_nboot,method="Regularized",maxiter=input$input_maxiter)
  }
  else {dataI<-imputeFAMD(exportData(),ncp=input$input_nboot,method="EM",maxiter=input$input_maxiter)}
  dataI<-dataI$completeObs
  dataI
})

output$ui_select_tabA <- renderUI({
  if (input$visualizeA == "Tabulations"){
    out <- fluidRow(
      column(12, p("Tabular representation of original and modified data"), offset = 0, class = "wb-header"),
      column(12, p("Here you can view univariate and bivariate tabulations of categorical key variables to compare the variables before and after applying anonymization methods."), offset = 0, class = "wb-header-hint")
    )
    output$ui_biv_selectionI<- renderUI({
      fluidRow(
        column(6, uiOutput("ui_catvar_I1"), align="center"),
        column(6, uiOutput("ui_catvar_I2"), align="center"))
    })
    output$biv_tab_oI <- renderTable({
      req(input$sel_catvar_I1)
      df <- get_origData()
      vars <- c(input$sel_catvar_I1, input$sel_catvar_I2)
      if (vars[2]=="none") {
        tab <- as.data.frame.table(addmargins(table(df[[vars[1]]], useNA="always")))
        tab$Freq <- as.integer(tab$Freq)
        colnames(tab) <- c(vars[1], "Freq")
      } else {
        tab <- as.data.frame.matrix(formatC(addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA = "always")), format = "f", digits = 0))
        colnames(tab)[is.na(colnames(tab))] <- "NA"
        rownames(tab)[is.na(rownames(tab))] <- "NA"
        tab <- cbind(rownames(tab), tab)
        colnames(tab)[1] <- ""
      }
      tab
    }, include.rownames = FALSE)
    
    output$biv_tab_anonI <- renderTable({
      req(input$sel_catvar_I1,input$apply_analysis)
      df <- exportData()
      vars <- c(input$sel_catvar_I1, input$sel_catvar_I2)
      if (vars[2]=="none") {
        tab <- as.data.frame.table(addmargins(table(df[[vars[1]]], useNA="always")))
        tab$Freq <- as.integer(tab$Freq)
        colnames(tab) <- c(vars[1], "Freq")
      } else {
        tab <- as.data.frame.matrix(formatC(addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA = "always")), format = "f", digits = 0))
        colnames(tab)[is.na(colnames(tab))] <- "NA"
        rownames(tab)[is.na(rownames(tab))] <- "NA"
        tab <- cbind(rownames(tab), tab)
        colnames(tab)[1] <- ""
      }
      tab
    }, include.rownames = FALSE)
    
    
    output$biv_tab_resII <- renderPrint({
      req(input$sel_catvar_I2)
      if(!is.null(input$sel_catvar_I2) & input$sel_catvar_I2 == "none"){
        tabs <- fluidRow(column(6, h4("Original data"), align="center"),
                         column(6, tableOutput("biv_tab_oI")),
                         column(6, h4("Anonymized data"), align="center"),
                         column(6, tableOutput("biv_tab_anonI")),
                         column(6, h4("Imputation data"), align="center"),
                         column(6, tableOutput("biv_tab_mA")))
      } else {
        tabs <- fluidRow(column(12, h4("Original data"), align="center"),
                         column(12, tableOutput("biv_tab_oI"), align="center", class="wn-info-table wn-row-title"),
                         column(12, h4("Anonymized data"), align="center"),
                         column(12, tableOutput("biv_tab_anonI"), align="center", class="wn-info-table wn-row-title"),
                         column(12, h4("Imputation data"), align="center"),
                         column(12, tableOutput("biv_tab_mA"), align="center", class="wn-info-table wn-row-title"))
      }
      tabs
    })
    
    out <- list(uiOutput("ui_biv_selectionI"), uiOutput("biv_tab_resII"))
    
    out
  }
})

observeEvent(input$apply_analysis, {
  input$input_nboot
  input$input_maxiter
  input$input_method
  
})

dfamd <- eventReactive(input$apply_analysis, {
  dffamd()
})


output$tableMIMCA <- DT::renderDataTable({
  dfamd()
})

dfplotfamd<-eventReactive(input$apply_analysis,{
  table<-dffamd()
  errors_Imputationfamd<-mixError(ximp=table, xmis=exportData(), xtrue=get_origData())
  error1 = data.frame(errors_Imputationfamd)
  if(length(errors_Imputationfamd)!=1){
    error2 = data.frame(
      colonne = row.names(error1),
      Error_Imputationfamd = c(errors_Imputationfamd[1],errors_Imputationfamd[2]))}
  if(length(errors_Imputationfamd)==1){error2 = data.frame(
    colonne = row.names(error1),
    Error_Imputationfamd = c(errors_Imputationfamd[1]))}
  b <- ggplot(error2, aes(x = colonne, y = Error_Imputationfamd)) 
  b + geom_bar(aes(fill = colonne), stat="identity",width = .5)
  #plot(errors_Imputation)
  # data<-rbind(errors_Imputation)
  # a<-barplot(t(data), beside=T, ylim=c(0,1), col=c("blue","red"),ylab="Erreur Imputation")
  # a+legend(x="topleft",legend=colnames(data), cex=0.4, fill=c("blue","red"))
})

output$ui_select_eror_IMIMCA <- renderPlot({
  dfplotfamd()
})


output$biv_tab_mA <- renderTable({
  req(input$sel_catvar_I1)
  df<-dffamd()
  vars <- c(input$sel_catvar_I1, input$sel_catvar_I2)
  if (vars[2]=="none") {
    tab <- as.data.frame.table(addmargins(table(df[[vars[1]]], useNA="always")))
    tab$Freq <- as.integer(tab$Freq)
    colnames(tab) <- c(vars[1], "Freq")
  } else {
    tab <- as.data.frame.matrix(formatC(addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA = "always")), format = "f", digits = 0))
    colnames(tab)[is.na(colnames(tab))] <- "NA"
    rownames(tab)[is.na(rownames(tab))] <- "NA"
    tab <- cbind(rownames(tab), tab)
    colnames(tab)[1] <- ""
  }
  tab
}, include.rownames = FALSE)
