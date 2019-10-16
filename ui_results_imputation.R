library(VIM)
library(missForest)
library(FactoMineR)
library(missMDA)
library(ggplot2)

output$ui_view_a_data_header <- renderUI({
  output$ImputeData <- DT::renderDataTable({
    exportData()
    
  }, options=list(scrollX=TRUE, lengthMenu=list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All')), pageLength=10), rownames=FALSE)
  out <- fluidRow(
    column(12, h3("Anonymized microdata"), class="wb-header")
  )
  out <- list(out, fluidRow(
    column(12, h5("Anonymized data"), align="center"),
    column(12, DT::dataTableOutput("ImputeData"))
  ))
})



output$ui_f_analysis_header <- renderUI({
  txt<-"Impute the missing values of a mixed dataset (with continuous and categorical variables) using the principal component method factorial analysis for mixed data (FAMD). Can be used as a preliminary step before performing FAMD on an incomplete dataset."
  out <- fluidRow(
    column(12, h3("Impute mixed dataset"), class="wb-header"),
    column(12, p(txt)),
    column(12, sliderInput(inputId = "input_nboot", label = "ncp:integer corresponding to the number of components used to predict the missing entries",
                           min = 1, max = 100,value=2)),
    column(12, sliderInput(inputId = "input_maxiter", label = "maxiter:maximum number of iteration for the algorithm",
                           min = 100, max = 10000,value=1000)),
    column(12, radioButtons(inputId = "input_method", label = "Regularized or EM",
                           choices=c("Regularized"="reg","EM"="em"),inline=TRUE)),
    actionButton("apply_analysis", "Apply FAMD",color = "primary",
                 style = "bordered",
                 icon = icon("sliders")),
    conditionalPanel(
      condition = "input.apply_analysis !=0",
    column(12, h5("Imputation data"), align="center")),
    column(12,DT::dataTableOutput("tableMIMCA")),
    conditionalPanel(
      condition = "input.apply_analysis !=0",
      column(12, h5("Error Imputation resulting by inputeFAMD"), align="center")),
    column(12,plotOutput("ui_select_eror_IMIMCA")),
    conditionalPanel(
      condition = "input.apply_analysis !=0",
      column(12,selectInput(inputId = "visualizeA", label = "Visualizations",
                            choices = c("none", "Tabulations"), multiple = FALSE))),
    column(12,uiOutput("ui_select_none")),
    column(12,uiOutput("ui_select_tabA"))
)
})


output$ui_knn_header<-renderUI({
  txt<-"k-Nearest Neighbour Imputation based on a variation of the Gower Distance for numerical, categorical, ordered and semi-continous variables."
  out <- fluidRow(
    column(12, h3("k-Nearest Neighbour Imputation"), class="wb-header"),
    column(12, p(txt)),
    column(12, sliderInput(inputId = "input_k", label = "number of Nearest Neighbours used",
                           min = 1, max = 15,value=5)),
     
    actionButton("apply_Knn", "Apply KNN",color = "primary",
                 style = "bordered",
                 icon = icon("sliders")),
    conditionalPanel(
      condition = "input.apply_Knn !=0",
    column(12, h5("Imputation data"), align="center"),
    column(12,DT::dataTableOutput("tableKnn")),
      column(12, h5("Error Imputation resulting by KNN"), align="center"),
    column(12,plotOutput("ui_select_eror_IKNN")),
      column(12,selectInput(inputId = "visualizeKNN", label = "Visualizations",
                            choices = c("none", "Tabulations"), multiple = FALSE)),
    column(12,uiOutput("ui_select_none")),
    column(12,uiOutput("ui_select_tabKNN")))
    
  )
  
  
})  



output$ui_random_forest_header<-renderUI({
  txt<-"'missForest' is used to impute missing values particularly in the case of mixed-type data. It can be used to impute continuous and/or categorical data including complex interactions and nonlinear relations. It yields an out-of-bag (OOB) imputation error estimate. Moreover, it can be run parallel to save computation time."
  #dat_exp_type=dat_exp_typeF
  # rb_exptypeForest <- radioButtons("dat_exp_typeF", label=p("Select file format for export", align="center"),
  #                                  choices=c("R-dataset (.RData)"="rdata","SPSS-file (.sav)"="sav","CSV-file (.csv)"="csv", "STATA-file (.dta)"="dta", "SAS-file (.sas7bdat)"="sas"),
  #                                  width="100%", selected=input$dat_exp_type, inline=TRUE)
  # 
  out <- fluidRow(
    column(12, h3("Nonparametric Missing Value Imputation using Random Forest"), class="wb-header"),
    column(12, p(txt)),
    column(12, sliderInput(inputId = "input_max", label = "maximum number of iterations to be performed given the stopping criterion is not met beforehand.",
                           min = 1, max = 1000,value=1)),
    column(12, sliderInput(inputId = "input_ntree", label = "number of trees to grow in each forest.",
                           min = 1, max = 1000,value=1)),
    actionButton("apply_missForest", " Apply MissForest",color = "primary",
                 style = "bordered",
                 icon = icon("sliders")),
    #req(input$input_max & input$input_ntree),
    conditionalPanel(
      condition = "input.apply_missForest !=0",
    column(12, h5("Imputation data"), align="center")),
    column(12,DT::dataTableOutput("table")),
    #rb_exptype=rb_exptypeForest
    #column(12, rb_exptypeForest, align="center"),
    conditionalPanel(
      condition = "input.apply_missForest !=0",
    column(12, h5("Error Imputation resulting by missForest"), align="center")),
    column(12,plotOutput("ui_select_eror_IMissF")),
    conditionalPanel(
      condition = "input.apply_missForest !=0",
    column(12,selectInput(inputId = "visualize", label = "Visualizations",
                          choices = c("none", "Tabulations"), multiple = FALSE))),
    column(12,uiOutput("ui_select_none")),
    column(12,uiOutput("ui_select_tab"))
    
    
   )
  
})