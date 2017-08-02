#' SeaClass Interactive GUI
#'
#' The SeaClass function starts up the interactive GUI for binary response variable analysis.
#' @details Detailed instructions are included on the application's info tab. The application can be initiated using the command \code{SeaClass()} or from RStudio's Addins dropdown menu. Note that categorical predictors with a large number of levels can be problematic for some of the application modules. Hence, categorical variables with only one level or more than 10 levels are often ignored. The user can manually adjust \code{max_num_of_levels} after downloading the R analysis code.
#' @return None.
#' @examples
#' \dontrun{
#' ### Fake Data:
#' X <- matrix(rnorm(10000,0,1),ncol=10,nrow=1000)
#' X[1:100,1:2] <- X[1:100,1:2] + 3
#' Y <- c(rep(1,100), rep(0,900))
#' Fake_Data <- data.frame(Y = Y , X)
#' ### SeaClass Rare Failure Data:
#' data("rareFailData")
#' SeaClass()}
#' @export
#' @import shiny

SeaClass = function(){
  ui_values <- reactiveValues(uihelp = 0, alertgo = 0)
  values <- reactiveValues(response_var=NULL,
                           response_var01 = NULL,
                           minority_class = NULL,
                           majority_class=NULL,
                           analysis_ready_var = 0,
                           dfvar=NULL,
                           responsevar=NULL,
                           methodvar=NULL,
                           analysisvar=NULL,
                           go_time=NULL,
                           categorical_message=NULL,
                           list_output=NULL,
                           df_input=NULL,
                           df_output=NULL,
                           ui_update=NULL,
                           df_selected=NULL,
                           possible_test_sets=NULL)
  plotNULL <- ggplot2::ggplot(data.frame()) + ggplot2::geom_point() + ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
               ggplot2::theme(axis.line=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.ticks=ggplot2::element_blank(),
                   axis.title.x=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),
                   legend.position="none",
                   panel.background=ggplot2::element_blank(),
                   panel.border=ggplot2::element_blank(),
                   panel.grid.major=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank(),
                   plot.background=ggplot2::element_blank())
  tableNULL <- DT::datatable(matrix(rep(" ", 4),nrow=1), options = list(dom = 't'),rownames= FALSE, colnames = c(" ", " ", " ", " "))
  printNULL <- "  "
  myresponse = c("Select Binary Response.")
  myanalysis = c("Select a Category.", "Sampling Procedures","Simple Models","Complex Models","Anomaly Detection")
  anomaly_methods = c("Select a Method.","Mahalanobis Distance", "One Class SVM")
  simple_methods = c("Select a Method.","Single Variable: All", "Single Variable: Categorical", "Thresholds: Accuracy", "Thresholds: True Positive", "Decision Tree")
  complex_methods = c("Select a Method.", "Prediction Models", "Cost Models", "Bias Correction")
  sampling_methods = c("Select a Method.", "Train/Test Split", "Variable Selection: Supervised", "Variable Selection: PCA", "SMOTE")
  jsCode1 <- "shinyjs.titleCol = function(params){var x = document.getElementsByClassName('col-sm-12'); x[0].style.background=params;}"
  alignCenter <- function(el) {htmltools::tagAppendAttributes(el,style="margin-left:auto;margin-right:auto;")}
  app <- shinyApp(
   ui <- fluidPage(
    tags$head(tags$link(rel="shortcut icon", href="http://www.seagate.com/favicon.ico")),
    tags$head(tags$style(HTML(".col-sm-12 {background-color: #1E90FF"))),
    headerPanel(h1("SeaClass",style="color:white;font-size:36px;font-family:Arial;text-align:center;")),
    title = "SeaClass",
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jsCode1),
    tags$style(type="text/css",".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }"),
    tags$head(tags$style(HTML(".blink {animation: blink-animation 1s steps(5, start) infinite;
                              -webkit-animation: blink-animation 1s steps(5, start) infinite;
                              color:#CF5300;font-size:20px;font-family:Arial;text-align:center}
                              @keyframes blink-animation {to {visibility: hidden;}}
                              @-webkit-keyframes blink-animation {to {visibility: hidden;}}"))),
    tags$head(tags$style(HTML(".noblink {color:#006400;font-size:20px;font-family:Arial;text-align:center}"))),
    fillRow(column(6, align="center", offset = 3, style="margin-top:10px;margin-bottom:10px;",
         shinyjs::hidden(tags$button(id="redobtn",type="button",style = "background:#FF881A;",class = "btn btn-default action-button shiny-bound-input",HTML('<span style="color:white;font-size:14px;font-family:Arial;"><strong>Start Over</strong></span>'))),
         actionButton("helpbtn",label=strong("Show App Info")),
         tags$button(id="donebtn",type="button",style = "background:#1E90FF;",class = "btn btn-default action-button shiny-bound-input",
                     onclick = "setTimeout(function(){window.close();},10);",
                     HTML('<span style="color:white;font-size:14px;font-family:Arial;"><strong>EXIT</strong></span>'))
      )),
    div(
    shinyjs::hidden(div(id="helpinfo",
        hr(),
        div(HTML("<p style='font-size:26px;'><strong>App Info</strong></p>"),style="text-align:center;width:100%"),
        HTML('<p style=\"font-size:18px;margin:20px;\">This application provides tools for binary response variable analysis. The guidelines for using the app are as follows:</p>'),
        HTML("<p style=\"font-size:18px;margin-left:35px;margin-top:10px;margin-right:35px;\"><strong>Step 1:</strong> Load your data prior to opening the app. The app will automatically detect all stored data frame objects. If no data frames are detected, then you need to close the app and load your data.</p>"),
        HTML("<p style=\"font-size:18px;margin-left:35px;margin-top:10px;margin-right:35px;\"><strong>Step 2:</strong> Prepare your data prior to opening the app. The data should be stored as a data fame object and should contain a binary response variable (e.g. PASS/FAIL, 0/1, TRUE/FALSE). The rest of the columns should contain only predictor variables.  It's wise to keep the number of predictors small (e.g. less than 100) since most methods will struggle as the feature space grows. Make sure to remove all other non-predictor variables and replace or remove any missing values. Each row should represent a unique observation. Hence, time series data must be summarized.</p>"),
        HTML("<p style=\"font-size:18px;margin-left:35px;margin-top:10px;margin-right:35px;\"><strong>Step 3:</strong> Start the app. (You are here!)</p>"),
        HTML("<p style=\"font-size:18px;margin-left:35px;margin-top:10px;margin-right:35px;\"><strong>Step 4:</strong> Select data and enter input parameters using the menus below. Insights and recommendations will be provided dynamically. Finally, submit your settings to build the analysis.</p>"),
        HTML("<p style=\"font-size:18px;margin-left:35px;margin-top:10px;margin-right:35px;\"><strong>Step 5:</strong> Results will appear below once the analysis is complete. Some methods provide further input options for interactive results.</p>"),
        HTML("<p style=\"font-size:18px;margin-left:35px;margin-top:10px;margin-right:35px;\"><strong>Step 6:</strong> R code based on the most recent analysis and user selections can be downloaded using the tool which will appear at the end of the page.</p>"),
        HTML('<p style=\"font-size:18px;margin:20px;\">This application was built by the advanced analytics group at Seagate.</p>'),
        HTML('<p style=\"font-size:18px;margin:20px;\">The public github repository is github.com/TheLondonPeacock/SeaClass.</p>')
    )),
        style = "padding-top:40px;"
    ),
    hr(),
    div(id="inputs",
     div(HTML("<p style='font-size:26px;'><strong>User Inputs</strong></p>"),style="text-align:center;width:100%"),
     fluidRow(
      column(1, HTML("<div></div>")),
      column(3,
             uiOutput("df_select"),
             shinyjs::hidden(selectInput("responsevar", "Select Binary Response.", choices = myresponse),
                             selectInput("analysisvar", "Select Analysis Category.", choices = myanalysis),
                             selectInput("methodvar", "Select a Method.", choices = "Select a Method."),
                             actionButton("runbtn",label=strong("Build Analysis")),
                             div(id = "running", HTML("<div style=\"margin-left:100px;padding:20px;\"><span class=\"blink\"><strong>Building Analysis...</strong></span></div>")),
                             div(id = "ready", HTML("<div style=\"margin-left:100px;padding:20px;\"><span class=\"noblink\"><strong>Resubmit?</strong></span></div>"))
                         )
       ),
       column(6, offset=1,shinyjs::hidden(htmlOutput("pass_text", style="font-size:20px;padding-right:20px;margin:20px;"))),
       column(1, HTML("<div></div>"))
     ),
     hr()
    ),
    shinyjs::hidden(div(id = "analysisdiv",
      div(HTML("<p style='font-size:26px;'><strong>Analysis Results</strong></p>"),style="text-align:center;width:100%"),
      shinyjs::hidden(div(id = "divnotes",
        htmlOutput("notes", style="font-size:20px;padding-left:40px;padding-right:40px;padding-top:10px;padding-bottom:0px;")
      )),
      uiOutput("analysis_placeholder"),
      hr()
    )),
    shinyjs::hidden(div(id = "codediv",
      div(HTML("<p style='font-size:26px;'><strong>Download Analysis Code</strong></p>"),style="text-align:center;width:100%"),
      div(
        div(p(id ="download_text", HTML("The download button provides R code for the above analysis."),style="font-size:20px;"),
          br(),
          downloadButton('code.R', strong('Download Current Analysis Code')),
          style="margin-top:50px;margin-bottom:50px;font-size:20px;"
        ), style="text-align:center;"
      ),
      hr()
      )),
     br(),
     br(),
     br()
    ),

  server <- function(input, output, session){

    observeEvent(input$helpbtn,{
      if(ui_values$uihelp == 0){
        shinyjs::show(id="helpinfo", anim=TRUE, animType="fade")
        updateActionButton(session, "helpbtn", label = HTML("<strong>Hide App Info</strong>"))
        ui_values$uihelp <- 1
      } else {
        shinyjs::hide(id="helpinfo", anim=TRUE, animType="fade")
        updateActionButton(session, "helpbtn", label = HTML("<strong>Show App Info</strong>"))
        ui_values$uihelp <- 0
      }

    })

    myDFs_reactive <- reactive({
      values$ui_update
      myObjects = ls(envir = .GlobalEnv)
      if(length(myObjects) == 0){myDFs = NULL}
      if(length(myObjects) != 0){myDFs = myObjects[sapply(myObjects, function(x) is(get(x),"data.frame"))]}
      if(length(myDFs) > 0){myDFs = c("Select a data frame.", myDFs)}
      if(length(myDFs)==0){myDFs = "No Data Frame Objects Found."}
      values$ui_update = NULL
      return(myDFs)
    })

    output$df_select <- renderUI({
      selectInput("df", "Select Data Source.", choices = myDFs_reactive(), selected = values$df_selected)
    })

    reactiveData <- reactive({
      dataString <- input$df
      if (dataString == "No Data Frame Objects Found." | dataString == "Select a data frame.")
        return(list())
      data <- get(dataString, envir = .GlobalEnv)
    })

    observeEvent(input$df, {
      if(!(input$df %in% c("No Data Frame Objects Found.", "Select a data frame."))){
        data <- reactiveData()
        shinyjs::show(id="responsevar", anim=TRUE, animType="fade")
        updateSelectInput(session, "responsevar", choices = c("Select Binary Response.", names(data)))
      }
      if(input$df %in% c("No Data Frame Objects Found.", "Select a data frame.")){
        shinyjs::hide(id="responsevar", anim=FALSE)
        updateSelectInput(session, "responsevar", choices = "Select Binary Response.")
      }
      if(input$df == "No Data Frame Objects Found."){
        shinyjs::js$titleCol("#D52F2F")
      }
    })

    output$pass_text <- renderText({
      data <- reactiveData()
      tmp_test = !(input$responsevar %in% names(data))
      if(input$responsevar == "Select Binary Response." | tmp_test){return(list())}
      na_or_infinite_check = any(apply(data, 2, function(x) any(is.na(x) | is.infinite(x))))
      if(na_or_infinite_check){
        return("<span style=\"color:red;\"><strong>ERROR:</strong></span> Your selected data frame has NAs and/or infinite values. These values may cause unexpected results and should be removed using \"na.omit()\" or replaced using imputation methods.<br/>")
      }
      tmp_test2 = (ncol(data) <= 2)
      if(tmp_test2){
        return("<span style=\"color:red;\"><strong>ERROR:</strong></span> Your selected data frame has two or fewer columns. Multiple modules require more than one predictor.<br/>")
      }
      if(input$responsevar != "Select Binary Response." & !na_or_infinite_check & !tmp_test2){
        my_cases = data[,input$responsevar]
        L = length(unique(my_cases))
        if(L != 2){
          my_text = paste0("<span style=\"color:red;\"><strong>ERROR:</strong></span> The variable selected is not binary.<br/>",input$responsevar ," has ",L," levels.")
        }
        if(L == 2){
          TAB = table(my_cases)
          NAME_TAB = names(TAB)
          small_n = min(TAB)
          small_pct = 100*small_n/sum(TAB)
          data_balanced = (sum(TAB == small_n) == 2)
          minority_text = ""
          if(data_balanced){minority_text = "Each class accounts for 50% of the data."}
          if(!data_balanced){minority_text = paste0("The minority class accounts for ",round(100*small_n/sum(TAB),digits=4), "% of the data.")}
          if(small_pct < 10){
            if(small_n < 10){methods_recommendation = "You have very few minority class cases. We recommend trying Anomaly Detection or Thresholds. Prediction models are at risk for overfitting."}
            if(small_n >=10 & small_n < 100){methods_recommendation = "You have a small to moderate number of minority class cases. We recommend trying Anomaly Detection or Simple Models. Complex Models may be at risk for overfitting."}
            if(small_n >= 100){methods_recommendation = "You appear to have sufficient data to try any of the available methods. If trying prediction methods, you may consider applying one of the sampling procedures first."}
          }
          if(small_pct >= 10){
            if(small_n < 10){methods_recommendation = "You have very few minority class cases. We recommend trying thresholds or very simple models. Most prediction methods are at risk for overfitting."}
            if(small_n >=10 & small_n < 100){methods_recommendation = "You have a small to moderate number of minority class cases. We recommend trying Simple Models. Complex Models may be at risk for overfitting."}
            if(small_n >= 100){methods_recommendation = "You appear to have sufficient data to try any of the available methods. If trying prediction methods, you may consider applying one of the sampling procedures first."}
          }
          my_text = paste0("<strong>Data Overview:</strong><br/>Variable ", input$responsevar, " contains ", TAB[[1]], " cases for class ", NAME_TAB[1], " and ",
                           TAB[[2]], " cases for class ", NAME_TAB[2], ". ", minority_text,
                           "<br/><br/><strong>Recommendation:</strong><br/>",
                           methods_recommendation)
        }
        big_data_warning = ""
        nc = ncol(data)
        nr = nrow(data)
        if(nc > 30 | nr > 10000){
          big_data_warning = paste0("<br/><br/><span style=\"color:red;\"><strong>NOTE:</strong></span> Interactive modules may become slow when row and/or column dimensions are large. Your data set has ",
                                    nr, " rows and ", nc, " columns. If latency is an issue, try using Train/Test Split or Variable Selection modules under Sampling Procedures.")
        }
        my_text = paste0(my_text,big_data_warning)
        return(my_text)
      }
    })

    observeEvent(input$responsevar, {
      if(input$responsevar != "Select Binary Response."){
        shinyjs::show(id="pass_text", anim=TRUE, animType="fade")
        shinyjs::hide(id="analysisvar", anim=FALSE)
        data <- reactiveData()
        na_or_infinite_check = any(apply(data, 2, function(x) any(is.na(x) | is.infinite(x))))
        L = length(unique(data[,input$responsevar]))
        tmp_test2 = (ncol(data) <= 2)
        if(L == 2 & !na_or_infinite_check & !tmp_test2){
          shinyjs::show(id="analysisvar", anim=TRUE, animType="fade")
        }
      }
      if(input$responsevar == "Select Binary Response."){
        shinyjs::hide(id="pass_text", anim=FALSE)
        shinyjs::hide(id="analysisvar", anim=FALSE)
        updateSelectInput(session, "analysisvar", selected = "Select a Category.")
      }
    })

    observeEvent(input$analysisvar, {
      if(input$analysisvar != "Select a Category."){
        shinyjs::show(id="methodvar", anim=TRUE, animType="fade")
        if(input$analysisvar == "Anomaly Detection"){updateSelectInput(session, "methodvar", choices = anomaly_methods)}
        if(input$analysisvar == "Simple Models"){updateSelectInput(session, "methodvar", choices = simple_methods)}
        if(input$analysisvar == "Complex Models"){updateSelectInput(session, "methodvar", choices = complex_methods)}
        if(input$analysisvar == "Sampling Procedures"){updateSelectInput(session, "methodvar", choices = sampling_methods)}
      }
      if(input$analysisvar == "Select a Category."){
        shinyjs::hide(id="methodvar", anim=FALSE)
        updateSelectInput(session, "methodvar", choices = "Select a Method.")
      }
    })

    observeEvent(input$methodvar, {
      if(!(input$methodvar == "Select a Method." | startsWith(input$methodvar,"Coming"))){
        shinyjs::show(id="runbtn", anim=FALSE)
      }
      if(input$methodvar == "Select a Method." | startsWith(input$methodvar,"Coming")){
        shinyjs::hide(id="runbtn", anim=FALSE)
        shinyjs::hide(id="running", anim=FALSE)
        shinyjs::hide(id="ready", anim=FALSE)
      }
    })

    observeEvent(input$runbtn, {
      shinyjs::hide(id="codediv", anim=FALSE)
      shinyjs::hide(id="analysisdiv", anim=FALSE)
      shinyjs::hide(id="divnotes", anim=FALSE)
      shinyjs::hide(id="ready", anim=FALSE)
      shinyjs::show(id="running", anim=FALSE)
      if(values$analysis_ready_var > 0){
        removeUI(selector = "#plot", multiple = TRUE, immediate = TRUE)
        removeUI(selector = "#tableDT", multiple = TRUE, immediate = TRUE)
        removeUI(selector = "#tableDT_2", multiple = TRUE, immediate = TRUE)
        removeUI(selector = "#tablePrint", multiple = TRUE, immediate = TRUE)
        removeUI(selector = "div:has(> #tool1)", multiple = TRUE, immediate = TRUE)
        removeUI(selector = "div:has(> #tool2)", multiple = TRUE, immediate = TRUE)
        removeUI(selector = "#text", multiple = TRUE, immediate = TRUE)
        removeUI(selector = "div:has(> #uiButton)", multiple = TRUE, immediate = TRUE)
        removeUI(selector = "div:has(> #divnewdata)", multiple = TRUE, immediate = TRUE)
        values$df_input = NULL
        values$df_output = NULL
        values$list_output = NULL
      }
       values$go_time = 1
    })

    observeEvent(values$go_time,{
      myalert = 0
      myalerttext = ""
      data <- reactiveData()
      values$response_var = data[,input$responsevar]
      TAB = table(values$response_var)
      small_n = min(TAB)
      data_balanced = (sum(TAB == small_n) == 2)
      if(!data_balanced){
        minority_class = names(which.min(TAB))
        majority_class = names(which.max(TAB))
      }else{
        minority_class = names(TAB)[2]
        majority_class = names(TAB)[1]
      }
      values$minority_class = minority_class
      values$majority_class = majority_class
      if(is.numeric(values$response_var)){values$response_var01 = ifelse(values$response_var == as.numeric(minority_class),1,0)}
      if(!is.numeric(values$response_var)){values$response_var01 = ifelse(values$response_var == minority_class,1,0)}
      data = data[,which(names(data) != input$responsevar)]

      ##### Anomaly Detection Setup
      if(input$methodvar == "Mahalanobis Distance"){
        # variable checks:
        keep_cols = which(unlist(lapply(data,is.numeric)))
        myalerttext = ifelse(length(keep_cols) == ncol(data), "", paste0(ncol(data) - length(keep_cols)," non-numeric variable(s) excluded. "))
        values$df_input = data[, keep_cols]
        my_check = cov_check(cov = cov(values$df_input), cut_point = 0.9999)
        if(my_check$remove_length > 0){values$df_input = values$df_input[,-my_check$remove_vars]}
        # compute mahalanobis distances:
        my_mahal = mahalanobis(values$df_input, center = colMeans(values$df_input), cov = cov(values$df_input), inverted = FALSE)
        # theoretical cut point under gaussian data:
        n = length(values$response_var01)
        p = ncol(values$df_input)
        informed_prob = sum(values$response_var01)/n
        chisq_cut = qchisq(1-informed_prob, p)
        scored_chisq = score_threshold(x = my_mahal, group = values$response_var01, pos_class = 1, cut = chisq_cut, type = "upper")
        # boxplot methods:
        boxplot_cuts = boxplot_cutpoints(x = my_mahal, iqr_mult = 3)
        scored_boxplot = score_threshold(x = my_mahal, group = values$response_var01, pos_class = 1, cut = boxplot_cuts$standard_rule[[2]], type = "upper")
        scored_adjusted = score_threshold(x = my_mahal, group = values$response_var01, pos_class = 1, cut = boxplot_cuts$adjusted_rule[[2]], type = "upper")
        # accuracy threshold:
        accuracy_cut = accuracy_threshold(x=my_mahal, group=values$response_var01, pos_class=1)
        if(!is.na(accuracy_cut$cut)){
          scored_accuracy = score_threshold(x = my_mahal, group = values$response_var01, pos_class = 1, cut=accuracy_cut$cut, type=accuracy_cut$direction)
        } else {
          scored_accuracy = list(FP=0, TP=0, Misclass=mean(values$response_var01), Direction = "Flag None")
        }
        # combine and save:
        my_table = data.frame(Method = c("Chi-Square", "Boxplot", "Adjusted Boxplot", "Accuracy Threshold", "User Specified"),
                              Cutpoint = round(c(chisq_cut,boxplot_cuts$standard_rule[[2]],boxplot_cuts$adjusted_rule[[2]],accuracy_cut$cut,0),digits=3),
                              FP  = round(c(scored_chisq[[1]],scored_boxplot[[1]],scored_adjusted[[1]],scored_accuracy[[1]],0),digits=3),
                              TP  = round(c(scored_chisq[[2]],scored_boxplot[[2]],scored_adjusted[[2]],scored_accuracy[[2]],0),digits=3))
        if(is.na(accuracy_cut$cut)){my_table$Cutpoint[4] = "None"}
        values$list_output = list(my_mahal = my_mahal, my_table = my_table)
        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "This module displays a histogram of Mahalanobis distances as well as several threshold rules and their respective performance (e.g. True and False positive percentages). The user may also provide their own rule by adjusting the slider.",style="font-size:20px;padding:40px;"),
            fluidRow(
              column(1, HTML("<div></div>")),
              column(4, offset=0,align="center",
                     plotOutput('plot', height = 300),
                     br(),
                     div(sliderInput(inputId = "tool1", label="User Cutpoint:", width="100%", min=0, max=round(max(my_mahal)+1,digits=0), value = quantile(my_mahal, probs = .99)),style="display:inline-block;width:75%;text-align:center;")
              ),
              column(1, HTML("<div></div>")),
              column(4, offset=0, align="center", DT::dataTableOutput('tableDT')),
              column(1, HTML("<div></div>"))
            )
          )
        })
        # compile alert messages
        myalerttext = paste0(myalerttext, my_check$remove_text)
        if(myalerttext != ""){myalert = 1}
      }

      if(input$methodvar == "One Class SVM"){
        # variable checks:
        keep_cols = which(unlist(lapply(data,is.numeric)))
        myalerttext = ifelse(length(keep_cols) == ncol(data), "", paste0(ncol(data) - length(keep_cols)," non-numeric variable(s) excluded. "))
        tmp = data[, keep_cols]
        my_check = cov_diag_check(cov = cov(tmp))
        if(my_check$remove_length > 0){
          values$list_output$svmdata = tmp[,-my_check$remove_vars]
          myalerttext = paste0(myalerttext, my_check$remove_text)
        }else{
          values$list_output$svmdata = tmp
        }
        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "This module allows the user to fit a one class SVM and adjust the results based on a tuning parameter which controls the approximate upper bound on the percentage of outliers.",style="font-size:20px;padding:40px;"),
            fluidRow(
              column(2, HTML("<div></div>")),
              column(3, offset=0,align="center",alignCenter(verbatimTextOutput('tablePrint'))),
              column(1, HTML("<div></div>")),
              column(4, div(sliderInput(inputId = "tool1", label="Approximate Outlier Percentage", width="100%", min = 0.1, max = 5, value = 1, step = 0.1),style="display:inline-block;width:75%;text-align:center;")),
              column(2, HTML("<div></div>"))
            )
           )
        })
        # compile alert messages
        if(myalerttext != ""){myalert = 1}
      }

      ##### Simple Models Setup
      if(input$methodvar == "Decision Tree"){
        Y = factor(values$response_var, levels = c(minority_class, majority_class), ordered=TRUE)
        # Limiting any categorical variables to a predefined number of levels:
        max_num_of_levels <- 10
        categorical_vars <- which(!unlist(lapply(data,is.numeric)))
        if(length(categorical_vars) > 0){
          remove_cat = NULL
          for(cv in categorical_vars){
            if(length(unique(data[,cv])) > max_num_of_levels){remove_cat = c(remove_cat, cv)}
          }
          if(!is.null(remove_cat)){
              data <- data[,-remove_cat]
              myalert = 1
              myalerttext <- paste0(length(remove_cat), " categorical variable(s) dropped because exceeded maximum number of levels.")
          }
        }
        values$list_output = data.frame(YYY = Y, data)
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "The decision tree module allows the user to modify the prior probability for the minority class. This allows the user to adjust for sampling biases, adjust for asymmetric losses, or optimize with respect to false/true positive rates. The initial prior is based on the observed frequencies.",style="font-size:20px;padding:40px;"),
            fluidRow(
              column(2, HTML("<div></div>")),
              column(4, div(sliderInput(inputId = "tool1", label="Tree Depth:", width="100%", min = 1, max = 30, value = 5, step = 1),style="display:inline-block;width:75%;text-align:center;")),
              column(4, div(sliderInput(inputId = "tool2", label="Minority Class Prior:", width="100%", min = 0.001, max = 0.999, value = min(c(max(c(0.001,round(small_n/sum(TAB),digits=3))),0.999)),step=0.001),style="display:inline-block;width:75%;text-align:center;")),
              column(2, HTML("<div></div>"))
            ),
            br(),
            fluidRow(
              column(1, HTML("<div></div>")),
              column(4, offset=0,align="center",alignCenter(verbatimTextOutput('tablePrint'))),
              column(1, HTML("<div></div>")),
              column(5, offset=0,align="center", plotOutput('plot', height = 300)),
              column(1, HTML("<div></div>"))
            )
          )
        })
      }

      if(input$methodvar == "Single Variable: All"){
        # Prepare Response:
        response_var <- factor(values$response_var, levels=c(majority_class,minority_class))
        # Removing any constant variables:
        data_input <- data
        unique_check <- apply(data_input, 2, function(x)length(unique(x)))
        constant_check <- which(unique_check == 1)
        myalerttext = ""
        if(length(constant_check) > 0){
          data_input <- data_input[,-constant_check]
          unique_check <- unique_check[-constant_check]
          myalerttext <- paste0(length(constant_check)," constant variable(s) were excluded. ")
        }
        # Limiting any categorical variables to a predefined number of levels:
        max_num_of_levels <- 10
        categorical_vars <- which(!unlist(lapply(data_input,is.numeric)))
        if(length(categorical_vars) > 0){
          level_check <- categorical_vars[which(unique_check[categorical_vars] > max_num_of_levels)]
          if(length(level_check) > 0){
            data_input <- data_input[,-level_check]
            myalerttext <- paste0(myalerttext, length(level_check), " categorical variable(s) dropped because exceeded maximum number of levels.")
          }
        }
        # Converting any character columns to factors:
        character_test <- which(!unlist(lapply(data_input,is.numeric)))
        if(length(character_test) > 0){
          for(ct in character_test){
            data_input[,ct] <- as.factor(data_input[,ct])
          }
        }
        # Performing univariate logistic regression analysis:
        NC <- ncol(data_input)
        my_output_table <- data.frame(matrix(NA, nrow=NC, ncol=6))
        names(my_output_table) <- c("Variable", "Logistic P-Val", "Logistic Misclass %", "Logistic AIC", "Logistic AUC", "RF Importance")
        my_output_table$Variable <- names(data_input)
        for(nc in 1:NC){
          logistic_model <- suppressWarnings(glm(response_var ~ data_input[,nc], family=binomial(link='logit')))
          my_output_table[nc,2] <- anova(logistic_model, test="Chisq")$`Pr(>Chi)`[2]
          my_output_table[nc,3] <- 100*mean(ifelse(logistic_model$fitted.values > 0.5,minority_class,majority_class) != response_var)
          my_output_table[nc,4] <- logistic_model$aic
          my_output_table[nc,5] <- PRROC::roc.curve(scores.class0 = logistic_model$fitted.values,weights.class0 = values$response_var01)$auc
        }
        # Random forest analysis:
        rf_model <- randomForest::randomForest(x = data_input, y = response_var, ntree=100)
        my_output_table[match(row.names(rf_model$importance), names(data_input)),6] <- rf_model$importance
        # Preparing table:
        for(ii in 2:6){my_output_table[,ii] <- round(my_output_table[,ii], digits=4)}
        my_output_table <- my_output_table[order(my_output_table[,5], decreasing = TRUE),]
        # Save table:
        values$list_output = my_output_table
        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "This module displays results from single predictor logistic regression models as well as random forest variable importance. Presorting on AUC is done by default.",style="font-size:20px;padding:40px;"),
            fluidRow(
              column(1, HTML("<div></div>")),
              column(10, offset=0,align="center",DT::dataTableOutput('tableDT')),
              column(1, HTML("<div></div>"))
            )
          )
        })
        # compile alert messages
        if(myalerttext != ""){myalert = 1}
      }

      if(input$methodvar == "Single Variable: Categorical"){
        # Prepare Response:
        response_var <- factor(values$response_var, levels=c(majority_class,minority_class))
        # Removing any numeric variables, and limiting categorical variables to a predefined number of levels:
        data_input <- data
        categorical_vars <- which(!unlist(lapply(data_input,is.numeric)))
        if(length(categorical_vars)>0){
          level_test <- rep(0,length(categorical_vars))
          for(cv in 1:length(categorical_vars)){level_test[cv] <- length(unique(data_input[,categorical_vars[cv]]))}
          max_num_of_levels <- 10
          min_num_of_levels <- 2
          keep_vars <- categorical_vars[which(level_test <= max_num_of_levels & level_test >= min_num_of_levels)]
        }else{
          keep_vars <- NULL
        }
        if(length(keep_vars) == 0){
          values$categorical_message = "<span style=\"color:red;\"><strong>Note:</strong></span> No categorical variables with unique level count between 2 and 10, inclusive."
          values$list_output = NULL
        } else {
          values$categorical_message = ""
          tmp_names <- names(data_input)[keep_vars]
          data_input <- as.data.frame(data_input[,keep_vars],stringsAsFactors=TRUE)
          names(data_input) <- tmp_names
          # Prepare output table:
          NC <- ncol(data_input)
          my_output_table <- data.frame(matrix(NA, nrow=NC, ncol=6))
          names(my_output_table) <- c("Variable", "Logistic P-Val", "Logistic Misclass %", "Logistic AIC", "Logistic AUC", "Chi-Square Test P-Val")
          my_output_table$Variable <- names(data_input)
          # Performing univariate logistic regression analysis:
          for(nc in 1:NC){
            logistic_model <- suppressWarnings(glm(response_var ~ data_input[,nc], family=binomial(link='logit')))
            my_output_table[nc,2] <- anova(logistic_model, test="Chisq")$`Pr(>Chi)`[2]
            my_output_table[nc,3] <- 100*mean(ifelse(logistic_model$fitted.values > 0.5,"FAIL","PASS") != response_var)
            my_output_table[nc,4] <- logistic_model$aic
            my_output_table[nc,5] <- PRROC::roc.curve(scores.class0 = logistic_model$fitted.values,weights.class0 = values$response_var01)$auc
            tmp_TAB <- table(data_input[,nc],response_var)
            my_output_table[nc,6] <- chisq.test(tmp_TAB, simulate.p.value = TRUE, B = 100000)$p.value
          }
          # Preparing table:
          for(ii in 2:6){my_output_table[,ii] <- round(my_output_table[,ii], digits=4)}
          my_output_table <- my_output_table[order(my_output_table[,6], decreasing = FALSE),]
          # Save table:
          values$list_output = my_output_table
        }
        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "This module considers each categorical predictor with unique level count between 2 and 10, inclusive. The table displays single predictor logistic regression results as well as the contingency table Chi-Square test. Presorting on Chi-Square test p-value is done by default.",style="font-size:20px;padding:40px;"),
            p(id ="text", HTML(values$categorical_message),style="font-size:20px;padding-right:40px;padding-left:40px;"),
            br(),
            fluidRow(
              column(1, HTML("<div></div>")),
              column(10, offset=0,align="center",DT::dataTableOutput('tableDT')),
              column(1, HTML("<div></div>"))
            )
          )
        })
        # compile alert messages
        if(myalerttext != ""){myalert = 1}
      }

      if(input$methodvar == "Thresholds: Accuracy"){
        # variable checks:
        keep_cols = which(unlist(lapply(data,is.numeric)))
        myalerttext = ifelse(length(keep_cols) == ncol(data), "", paste0(ncol(data) - length(keep_cols)," non-numeric variable(s) excluded. "))
        values$df_input = data[, keep_cols]
        # run analysis:
        Nc = ncol(values$df_input)
        user_matrix = as.data.frame(matrix(0,ncol=5,nrow=Nc))
        names(user_matrix) = c("Variable","Rule","FP %", "TP %", "Misclass %")
        user_matrix$Variable = names(values$df_input)
        for(nc in 1:Nc){
          my_thresh = accuracy_threshold(x=values$df_input[,nc], group=values$response_var01, pos_class=1)
          if(!is.na(my_thresh$cut)){
            scored_thresh = score_threshold(x=values$df_input[,nc], group=values$response_var01, pos_class=1, cut=my_thresh$cut, type=my_thresh$direction)
            user_matrix[nc,c(3,4,5,2)] = unlist(scored_thresh)
          } else {
            user_matrix[nc,2:5] = c("Flag None",0,0,100*mean(values$response_var01 == 1))
          }
        }
        user_matrix = user_matrix[order(as.numeric(user_matrix[,5])),]
        user_matrix[,3:5] = apply(user_matrix[,3:5], 2, function(x) round(as.numeric(x),digits=4) )
        values$list_output = user_matrix
        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "This module displays univariate threshold rules which are selected to minimize misclassification rate or maximize accuracy. Rules which fail to achieve a better misclassification rate than the majority class model (",round(100*mean(values$response_var01),digits=4),"%) are reported as \"Flag None\". The resulting rules are sumarized by various performance metrics (e.g. Misclassification %, True and False positive %).",style="font-size:20px;padding:40px;"),
            fluidRow(
              column(2, HTML("<div></div>")),
              column(8, offset=0,align="center",DT::dataTableOutput('tableDT')),
              column(2, HTML("<div></div>"))
            )
          )
        })
        # compile alert messages
        if(myalerttext != ""){myalert = 1}
      }

      if(input$methodvar == "Thresholds: True Positive"){
        # variable checks:
        keep_cols = which(unlist(lapply(data,is.numeric)))
        myalerttext = ifelse(length(keep_cols) == ncol(data), "", paste0(ncol(data) - length(keep_cols)," non-numeric variable(s) excluded. "))
        values$list_output = data[, keep_cols]
        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "This module displays the best univariate threshold rules based on true positive rates. Rules are returned if the true positive rate is non-zero and the minimum false positive rate does not exceed the user specified maximum value.",style="font-size:20px;padding:40px;"),
            fluidRow(
              column(4, HTML("<div></div>")),
              column(4, align="center", alignCenter(sliderInput(inputId = "tool1", label="Maximum False Positive %", width="75%", min = 0, max = 10, value = 1, step = 0.1))),
              column(4, HTML("<div></div>"))
            ),
            fluidRow(
              column(2, HTML("<div></div>")),
              column(8, offset=0,align="center",DT::dataTableOutput('tableDT')),
              column(2, HTML("<div></div>"))
            )
          )
        })
        # compile alert messages
        if(myalerttext != ""){myalert = 1}
      }

      ##### Sampling Procedures Setup
      if(input$methodvar == "SMOTE"){
        # variable checks:
        keep_cols = which(unlist(lapply(data,is.numeric)))
        myalerttext = ifelse(length(keep_cols) == ncol(data), "", paste0(ncol(data) - length(keep_cols)," non-numeric variable(s) excluded. "))
        values$df_input = data.frame(my_y = values$response_var, data[,keep_cols])
        values$df_input[,1] = as.factor(values$df_input[,1])
        names(values$df_input)[1] <- input$responsevar
        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "This module allows the user to generate a new training data set based on the SMOTE algorithm. The user is allowed to adjust settings for minority class over-sampling and majority class under-sampling. After the user clicks the create data button, the new data set will be saved to local memory with the \"SMOTE\" prefix attached.",style="font-size:20px;padding-left:40px;padding-right:40px;padding-top:40px;padding-bottom:30px;"),
            shinyjs::hidden(div(id = "divnewdata",htmlOutput("newdata", style="font-size:20px;padding-left:40px;padding-right:40px;padding-top:0px;padding-bottom:30px;"))),
            br(),
            fluidRow(
              column(1, HTML("<div></div>")),
              column(4, div(sliderInput(inputId = "tool1", label="Minority Class Multiplier", width="100%", min = 2, max = 10, value = 2, step = 1),style="display:inline-block;width:100%;text-align:center;")),
              column(2, offset=0,align="center", div(actionButton("uiButton",label = strong("Create Data Set")),style="display:inline-block;width:75%;text-align:center;")),
              column(4, div(sliderInput(inputId = "tool2", label="Majority Class Relative Multiplier", width="100%", min = 1, max = 10, value = 1, step = 1),style="display:inline-block;width:100%;text-align:center;")),
              column(1, HTML("<div></div>"))
            ),
            br(),
            fluidRow(
              column(4, HTML("<div></div>")),
              column(4, offset=0,align="center",verbatimTextOutput('tablePrint')),
              column(4, HTML("<div></div>"))
            )
          )
        })
        # compile alert messages
        if(myalerttext != ""){myalert = 1}
      }

      if(input$methodvar == "Train/Test Split"){
        # data prep:
        df_input = data.frame(my_y = values$response_var, data)
        names(df_input)[1] <- input$responsevar
        class_index = which(df_input[,input$responsevar] == minority_class)
        values$list_output = list(majorclass = df_input[-class_index,], minorclass = df_input[class_index,])
        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "This module allows the user to randomly split a data set into training and testing subsets. The user is allowed to make adjustments based on their desired training set, and the remaining observations are allocated to the testing set. The new data sets are saved with \"TRAIN\" and \"TEST\" prefixes attached. The user can over (under) sample the minority (majority) class by choosing slider values which are different.",style="font-size:20px;padding-left:40px;padding-right:40px;padding-top:40px;padding-bottom:30px;"),
            shinyjs::hidden(div(id = "divnewdata",htmlOutput("newdata", style="font-size:20px;padding-left:40px;padding-right:40px;padding-top:0px;padding-bottom:30px;"))),
            br(),
            fluidRow(
              column(1, HTML("<div></div>")),
              column(4, div(sliderInput(inputId = "tool1", label="Percent of Majority Class Allocated to Training", width="100%", min = 10, max = 90, value = 80, step = 10),style="display:inline-block;width:100%;text-align:center;")),
              column(2, offset=0,align="center", div(actionButton("uiButton",label = strong("Create Data Sets")),style="display:inline-block;width:75%;text-align:center;")),
              column(4, div(sliderInput(inputId = "tool2", label="Percent of Minority Class Allocated to Training", width="100%", min = 10, max = 90, value = 80, step = 10),style="display:inline-block;width:100%;text-align:center;")),
              column(1, HTML("<div></div>"))
            ),
            br(),
            fluidRow(
              column(4, HTML("<div></div>")),
              column(4, offset=0,align="center",verbatimTextOutput('tablePrint')),
              column(4, HTML("<div></div>"))
            )
          )
        })
      }

      if(input$methodvar == "Variable Selection: Supervised"){
        # Prepare Response:
        response_var <- factor(values$response_var, levels=c(majority_class,minority_class))
        # Removing any constant variables:
        data_input <- data
        unique_check <- apply(data_input, 2, function(x)length(unique(x)))
        constant_check <- which(unique_check == 1)
        myalerttext = ""
        if(length(constant_check) > 0){
          data_input <- data_input[,-constant_check]
          unique_check <- unique_check[-constant_check]
          myalerttext <- paste0(length(constant_check)," constant variable(s) were excluded. ")
        }
        # Limiting any categorical variables to a predefined number of levels:
        max_num_of_levels <- 10
        categorical_vars <- which(!unlist(lapply(data_input,is.numeric)))
        if(length(categorical_vars) > 0){
          level_check <- categorical_vars[which(unique_check[categorical_vars] > max_num_of_levels)]
          if(length(level_check) > 0){
            data_input <- data_input[,-level_check]
            myalerttext <- paste0(myalerttext, length(level_check), " categorical variable(s) dropped because exceeded maximum number of levels.")
          }
        }
        # Converting any character columns to factors:
        character_test <- which(!unlist(lapply(data_input,is.numeric)))
        if(length(character_test) > 0){
          for(ct in character_test){
            data_input[,ct] <- as.factor(data_input[,ct])
          }
        }
        # Performing univariate logistic regression analysis:
        NC <- ncol(data_input)
        my_output_table <- data.frame(matrix(NA, nrow=NC, ncol=4))
        names(my_output_table) <- c("Variable", "Logistic AUC", "Logistic AIC", "RF Importance")
        my_output_table$Variable <- names(data_input)
        for(nc in 1:NC){
          logistic_model <- suppressWarnings(glm(response_var ~ data_input[,nc], family=binomial(link='logit')))
          my_output_table[nc,2] <- PRROC::roc.curve(scores.class0 = logistic_model$fitted.values,weights.class0 = values$response_var01)$auc
          my_output_table[nc,3] <- logistic_model$aic
        }
        # Random forest analysis:
        rf_model <- randomForest::randomForest(x = data_input, y = response_var, ntree=100)
        my_output_table[match(row.names(rf_model$importance), names(data_input)),4] <- rf_model$importance
        # Save table:
        values$list_output = my_output_table
        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "This module allows the user to perform variable selection based on single predictor logistic regression (AIC or AUC) or random forest variable importance. The data is sorted based on the user's selected method.",style="font-size:20px;padding-left:40px;padding-right:40px;padding-top:40px;padding-bottom:30px;"),
            shinyjs::hidden(div(id = "divnewdata",htmlOutput("newdata", style="font-size:20px;padding-left:40px;padding-right:40px;padding-top:0px;padding-bottom:30px;"))),
            br(),
            fluidRow(
              column(2, HTML("<div></div>")),
              column(3, div(selectInput(inputId = "tool1", label="Selection Method", choices = c("Logistic AUC", "Logistic AIC", "RF Importance"), selected = "Logistic AUC",width="100%"),style="display:inline-block;width:100%;text-align:center;")),
              column(2, offset=0,align="center", div(actionButton("uiButton",label = strong("Create Data Set")),style="display:inline-block;width:75%;text-align:center;")),
              column(4, div(sliderInput(inputId = "tool2", label="Selection Count", width="100%", min = 1, max = ncol(data_input), value = min(10,ncol(data_input)), step = 1),style="display:inline-block;width:100%;text-align:center;")),
              column(1, HTML("<div></div>"))
            ),
            br(),
            fluidRow(
              column(1, HTML("<div></div>")),
              column(10, offset=0,align="center",DT::dataTableOutput('tableDT')),
              column(1, HTML("<div></div>"))
            )
          )
        })
        # compile alert messages
        if(myalerttext != ""){myalert = 1}
      }

      if(input$methodvar == "Variable Selection: PCA"){
        # Prepare Response:
        response_var <- factor(values$response_var, levels=c(majority_class,minority_class))
        # Remove categorical variables:
        data_input <- data
        keep_cols <- which(unlist(lapply(data_input,is.numeric)))
        myalerttext = ifelse(length(keep_cols) == ncol(data), "", paste0(ncol(data) - length(keep_cols)," non-numeric variable(s) excluded. "))
        data_input <- data_input[, keep_cols]
        # Remove zero variance and highly correlated variables:
        my_check <- cov_check(cov = cov(data_input), cut_point = 0.9999)
        if(my_check$remove_length > 0){data_input <- data_input[,-my_check$remove_vars]}
        # Perform PCA:
        df_pca <- prcomp(data_input, center = TRUE, scale. = TRUE)
        # Store Results:
        df_tmp = data.frame(Ytmp = values$response_var, df_pca$x)
        names(df_tmp) = c(input$responsevar, paste0("PC", 1:ncol(df_pca$x)))
        cum_var  = 100*cumsum(df_pca$sdev^2)/sum(df_pca$sdev^2)
        values$list_output = list(df_tmp=df_tmp, cum_var=cum_var)
        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "This module allows the user to perform variable selection based on Principal Component Analysis (PCA). The plot displays the proportion of variation explained by the user specified number of principal components.",style="font-size:20px;padding-left:40px;padding-right:40px;padding-top:40px;padding-bottom:30px;"),
            shinyjs::hidden(div(id = "divnewdata",htmlOutput("newdata", style="font-size:20px;padding-left:40px;padding-right:40px;padding-top:0px;padding-bottom:30px;"))),
            br(),
            fluidRow(
              column(2, HTML("<div></div>")),
              column(2, offset=0,align="center", div(actionButton("uiButton",label = strong("Create Data Set")),style="display:inline-block;width:75%;text-align:center;")),
              column(2, HTML("<div></div>")),
              column(4, div(sliderInput(inputId = "tool1", label="Number of PCs", width="100%", min = 1, max = ncol(df_pca$x), value = which(cum_var>90)[1], step = 1),style="display:inline-block;width:100%;text-align:center;")),
              column(2, HTML("<div></div>"))
            ),
            br(),
            fluidRow(
              column(3, HTML("<div></div>")),
              column(6, offset=0,align="center",plotOutput('plot', height = 300)),
              column(3, HTML("<div></div>"))
            )
          )
        })
        # compile alert messages
        myalerttext = paste0(myalerttext, my_check$remove_text)
        if(myalerttext != ""){myalert = 1}
      }

      ##### Complex Models Setup
      if(input$methodvar == "Prediction Models"){
        # Removing any constant variables:
        df_input <- data
        unique_check <- apply(df_input, 2, function(x)length(unique(x)))
        constant_check <- which(unique_check == 1)
        myalerttext = ""
        if(length(constant_check) > 0){
          df_input <- df_input[,-constant_check]
          unique_check <- unique_check[-constant_check]
          myalerttext <- paste0(length(constant_check)," constant variable(s) were excluded. ")
        }
        # Limiting any categorical variables to a predefined number of levels:
        max_num_of_levels <- 10
        categorical_vars <- which(!unlist(lapply(df_input,is.numeric)))
        if(length(categorical_vars) > 0){
          level_check <- categorical_vars[which(unique_check[categorical_vars] > max_num_of_levels)]
          if(length(level_check) > 0){
            df_input <- df_input[,-level_check]
            myalerttext <- paste0(myalerttext, length(level_check), " categorical variable(s) dropped because exceeded maximum number of levels.")
          }
        }
        # Converting any character columns to factors:
        character_test <- which(!unlist(lapply(df_input,is.numeric)))
        if(length(character_test) > 0){
          for(ct in character_test){
            df_input[,ct] <- as.factor(df_input[,ct])
          }
        }
        # Prepare numeric only data set:
        keep_cols = which(unlist(lapply(df_input,is.numeric)))
        df_input_numeric = df_input[, keep_cols]
        my_check = cov_check(cov = cov(df_input_numeric), cut_point = 0.9999)
        if(my_check$remove_length > 0){
          df_input_numeric = df_input_numeric[,-my_check$remove_vars]
        }
        values$df_input = df_input_numeric
        # Data Prep
        values$response_var = factor(values$response_var, levels=c(majority_class,minority_class))
        pos_index = which(values$response_var == minority_class)
        # Training Output Prep:
        my_models = c("Baseline","Random Forest","LDA","Stepwise Logistic","Penalized Logistic","XGBoost","SVM","KNN (k=5)")
        my_train_table = data.frame( Model = my_models, matrix(0,ncol=5,nrow=length(my_models)))
        names(my_train_table)[2:6] = c("Misclass %","AUC-ROC","AUC-PR","TP %", "FP %")
        # Baseline Stupid Model:
        my_train_table[1,2:6] = c(100*mean(values$response_var01), 0.5, 0.5, 0, 0)
        # Random Forest Model:
        rf_model = randomForest::randomForest(x = df_input, y = values$response_var, ntree=100)
        minority_index = which(attributes(rf_model$votes)$dimnames[[2]] == minority_class)
        majority_index = which(attributes(rf_model$votes)$dimnames[[2]] == majority_class)
        my_train_table[2,2:6] = c(100*mean(rf_model$predicted != values$response_var),
                                  PRROC::roc.curve(scores.class0 = rf_model$votes[,minority_index],weights.class0 = values$response_var01)$auc,
                                  PRROC::pr.curve(rf_model$votes[,majority_index],rf_model$votes[,minority_index])$auc.integral,
                                  100*mean(rf_model$predicted[pos_index] == values$response_var[pos_index]),
                                  100*mean(rf_model$predicted[-pos_index] != values$response_var[-pos_index]))
        # LDA Model:
        lda_model = MASS::lda(x=df_input_numeric,grouping=values$response_var)
        lda_pred  = predict(lda_model, newdata = df_input_numeric)
        minority_index = which(attributes(lda_pred$posterior)$dimnames[[2]] == minority_class)
        majority_index = which(attributes(lda_pred$posterior)$dimnames[[2]] == majority_class)
        my_train_table[3,2:6] = c(100*mean(lda_pred$class != values$response_var),
                                  PRROC::roc.curve(scores.class0 = lda_pred$posterior[,minority_index],weights.class0 = values$response_var01)$auc,
                                  PRROC::pr.curve(lda_pred$posterior[,majority_index],lda_pred$posterior[,minority_index])$auc.integral,
                                  100*mean(lda_pred$class[pos_index] == values$response_var[pos_index]),
                                  100*mean(lda_pred$class[-pos_index] != values$response_var[-pos_index]))
        # Stepwise Logistic Regression:
        my_stepwise_logistic_df = cbind(YYY = values$response_var, df_input)
        intercept_only = suppressWarnings(glm(YYY ~ 1, data=my_stepwise_logistic_df, family=binomial))
        upper_formula = formula(paste0("YYY ~ ", paste(names(my_stepwise_logistic_df)[-1],collapse = " + ")))
        logistic_step_model = suppressWarnings(step(intercept_only,scope=list(lower=formula(intercept_only),upper=upper_formula),
                                                    direction="forward", trace = FALSE, k = log(nrow(my_stepwise_logistic_df))))
        logistic_step_prob = logistic_step_model$fitted.values
        logistic_step_pred = ifelse(logistic_step_prob >= 0.5, minority_class, majority_class)
        my_train_table[4,2:6] = c(100*mean(logistic_step_pred  != values$response_var),
                                  PRROC::roc.curve(scores.class0 = logistic_step_prob,weights.class0 = values$response_var01)$auc,
                                  PRROC::pr.curve(1-logistic_step_prob,logistic_step_prob)$auc.integral,
                                  100*mean(logistic_step_pred[pos_index] == values$response_var[pos_index]),
                                  100*mean(logistic_step_pred[-pos_index] != values$response_var[-pos_index]))
        # Penalized (Ridge) Logistic Regression:
        tmp_factor = factor(values$response_var01)
        penalized_cv_model = suppressWarnings(glmnet::cv.glmnet(alpha=0,x=as.matrix(df_input_numeric),y=tmp_factor,nfolds=10,family="binomial",type.measure="class"))
        penalized_cv_prob  = predict(penalized_cv_model,newx=as.matrix(df_input_numeric),type="response",s="lambda.min")
        penalized_cv_pred = ifelse(penalized_cv_prob>=0.5,minority_class,majority_class)
        my_train_table[5,2:6] = c(100*mean(penalized_cv_pred  != values$response_var),
                                  PRROC::roc.curve(scores.class0 = penalized_cv_prob,weights.class0 = values$response_var01)$auc,
                                  PRROC::pr.curve(1-penalized_cv_prob,penalized_cv_prob)$auc.integral,
                                  100*mean(penalized_cv_pred[pos_index] == values$response_var[pos_index]),
                                  100*mean(penalized_cv_pred[-pos_index] != values$response_var[-pos_index]))
        # XGBoost Model:
        YYY = ifelse(values$response_var == minority_class,1,0)
        df_xgb = apply(df_input_numeric,2,function(x) if(is.numeric(x)){return(as.numeric(x))}else{return(x)})
        xgb_model = xgboost::xgboost(data = df_xgb,label = YYY,objective = "binary:logistic",nrounds = 100,verbose=0)
        xgb_prob = predict(xgb_model, df_xgb)
        xgb_pred = ifelse(xgb_prob>=0.5,minority_class,majority_class)
        my_train_table[6,2:6] = c(100*mean(xgb_pred  != values$response_var),
                                  PRROC::roc.curve(scores.class0 = xgb_prob,weights.class0 = values$response_var01)$auc,
                                  PRROC::pr.curve(1-xgb_prob,xgb_prob)$auc.integral,
                                  100*mean(xgb_pred[pos_index] == values$response_var[pos_index]),
                                  100*mean(xgb_pred[-pos_index] != values$response_var[-pos_index]))
        # SVM Model:
        svm_model <- e1071::svm(x=df_input_numeric,y=values$response_var,scale=TRUE,kernel="radial",probability=TRUE)
        svm_pred <- predict(svm_model, df_input_numeric, probability = TRUE)
        svm_prob <- attr(svm_pred, "probabilities")
        minority_index <- which(attributes(svm_prob)$dimnames[[2]] == minority_class)
        majority_index <- which(attributes(svm_prob)$dimnames[[2]] == majority_class)
        my_train_table[7,2:6] <- c(100*mean(svm_pred != values$response_var),
                                   PRROC::roc.curve(scores.class0 = svm_prob[,minority_index],weights.class0 = values$response_var01)$auc,
                                   PRROC::pr.curve(svm_prob[,majority_index],svm_prob[,minority_index])$auc.integral,
                                   100*mean(svm_pred[pos_index] == values$response_var[pos_index]),
                                   100*mean(svm_pred[-pos_index] != values$response_var[-pos_index]))
        # KNN (k=5) Model:
        knn_model <-  class::knn.cv(train=df_input_numeric,cl=values$response_var,prob=TRUE,k=5)
        knn_pred  <-  as.vector(knn_model)
        knn_prob  <- attributes(knn_model)$prob
        knn_prob[knn_pred == majority_class] <- 1-knn_prob[knn_pred == majority_class]
        my_train_table[8,2:6] <- c(100*mean(knn_pred  != values$response_var),
                                   PRROC::roc.curve(scores.class0 = knn_prob,weights.class0 = values$response_var01)$auc,
                                   PRROC::pr.curve(1-knn_prob,knn_prob)$auc.integral,
                                   100*mean(knn_pred[pos_index] == values$response_var[pos_index]),
                                   100*mean(knn_pred[-pos_index] != values$response_var[-pos_index]))

        # Store Output For Tables:
        my_train_table = my_train_table[order(my_train_table[,2]),]
        for(ii in 2:6){
          my_digits = ifelse(ii > 4, 2, 6)
          if(ii == 2){my_digits = 4}
          my_train_table[,ii] = round(my_train_table[,ii],digits=my_digits)
        }
        values$list_output = list(my_table=my_train_table,
                                  rf_model=rf_model,
                                  lda_model=lda_model,
                                  logistic_step_model=logistic_step_model,
                                  penalized_cv_model=penalized_cv_model,
                                  xgb_model=xgb_model,
                                  svm_model=svm_model)

        # Detect possible test sets:
        myObjects = ls(envir = .GlobalEnv)
        myDFs = myObjects[sapply(myObjects, function(x) is(get(x),"data.frame"))]
        myDFs = myDFs[which(myDFs != input$df)]
        possible_test_sets = "None."
        if(length(myDFs) > 0){
          name_check = c(input$responsevar, names(values$df_input))
          for(ii in 1:length(myDFs)){
            test = all(name_check %in% names(get(myDFs[ii], envir = .GlobalEnv)))
            if(test){possible_test_sets = c(possible_test_sets, myDFs[ii])}
          }
        }
        if(length(possible_test_sets) == 1){possible_test_sets = "None Detected."}
        values$possible_test_sets = possible_test_sets

        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "This module displays the performance of various classifiers. The user can choose to score test data by selecting from a drop down menu of possible data sets. Training and testing data sets can be generated using the Sampling Procedures category on the Data/Inputs screen.",style="font-size:20px;padding:40px;"),
            fluidRow(
              column(4, HTML("<div></div>")),
              column(4, offset=0,align="center",div(selectInput(inputId = "tool1", label="Test Data Sets:", width="100%",choices=values$possible_test_sets),style="display:inline-block;width:75%;text-align:center;")),
              column(4, HTML("<div></div>"))
            ),
            fluidRow(
              column(1, HTML("<div></div>")),
              column(5, offset=0,align="center",DT::dataTableOutput('tableDT')),
              #column(1, HTML("<div></div>")),
              column(5, offset=0, align="center", DT::dataTableOutput('tableDT_2')),
              column(1, HTML("<div></div>"))
            )
          )
        })
        # compile alert messages
        if(myalerttext != ""){myalert = 1}
      }

      if(input$methodvar == "Cost Models"){
        # Data Prep
        values$response_var = factor(values$response_var, levels = c(minority_class, majority_class), ordered=TRUE)
        pos_index = which(values$response_var == minority_class)
        neg_index = (1:length(values$response_var))[-pos_index]
        # Store data for later:
        n0 = length(neg_index)
        n1 = length(pos_index)
        test_index = c(neg_index[sample.int(n=n0, size=ceiling(n0*0.20), replace=FALSE)],
                       pos_index[sample.int(n=n1, size=ceiling(n1*0.20), replace=FALSE)])
        y_train = values$response_var[-test_index]
        y_test  = values$response_var[test_index]
        train_pos = (y_train == minority_class)
        test_pos = (y_test == minority_class)
        # Removing any constant variables based on training data:
        df_input <- data
        unique_check <- apply(df_input[-test_index,], 2, function(x)length(unique(x)))
        constant_check <- which(unique_check == 1)
        myalerttext = ""
        if(length(constant_check) > 0){
          df_input <- df_input[,-constant_check]
          unique_check <- unique_check[-constant_check]
          myalerttext <- paste0(length(constant_check)," constant variable(s) were excluded based on training set. ")
        }
        # Limiting any categorical variables to a predefined number of levels:
        max_num_of_levels <- 10
        categorical_vars <- which(!unlist(lapply(df_input,is.numeric)))
        if(length(categorical_vars) > 0){
          level_check <- categorical_vars[which(unique_check[categorical_vars] > max_num_of_levels)]
          if(length(level_check) > 0){
            df_input <- df_input[,-level_check]
            myalerttext <- paste0(myalerttext, length(level_check), " categorical variable(s) dropped because exceeded maximum number of levels.")
          }
        }
        # Converting any character columns to factors:
        character_test <- which(!unlist(lapply(df_input,is.numeric)))
        if(length(character_test) > 0){
          for(ct in character_test){
            df_input[,ct] <- as.factor(df_input[,ct])
          }
        }
        # Prepare numeric only data set:
        keep_cols = which(unlist(lapply(df_input,is.numeric)))
        df_input_numeric = df_input[, keep_cols]
        my_check = cov_check(cov = cov(df_input_numeric[-test_index,]), cut_point = 0.9999)
        if(my_check$remove_length > 0){
          df_input_numeric = df_input_numeric[,-my_check$remove_vars]
        }
        # Save more data:
        values$list_output = list(test_index = sort(test_index))
        # Training Output Prep:
        my_models = c("Baseline","Decision Tree","LDA","Stepwise Logistic", "Penalized Logistic","Random Forest","XGBoost","SVM")
        my_table = data.frame( Model = my_models, matrix(0,ncol=4,nrow=length(my_models)))
        names(my_table)[2:5] = c("Train Misclass %", "Test Misclass %","Train Cost","Test Cost")
        # Baseline Stupid Model:
        my_table[1,2:5] = c(round(100*mean(y_train == minority_class),digits=4),
                            round(100*mean(y_test == minority_class),digits=4),
                            sum(y_train == minority_class),
                            sum(y_test == minority_class))
        # Decision Tree Model:
        model_df = data.frame(YYY = values$response_var, df_input)
        tree_fit = rpart::rpart(YYY ~., data = model_df[-test_index,], method = "class",
                                parms = list(loss = matrix(c(0,1,1,0),ncol=2)),
                                control = list(minsplit=20, minbucket=5, maxdepth = 10))
        tree_prob <- predict(tree_fit, type = "prob", newdata = model_df)
        minority_index <- which(attributes(tree_prob)$dimnames[[2]] == values$minority_class)
        tree_prob <- tree_prob[,minority_index]
        Yhat = ifelse(tree_prob >= 0.5, values$minority_class, values$majority_class)
        Yhat_train = Yhat[-test_index]
        Yhat_test= Yhat[test_index]
        my_table[2,2:5] = c(round(100*mean(y_train != Yhat_train),digits=4),
                            round(100*mean(y_test != Yhat_test),digits=4),
                            sum(y_train[train_pos] != Yhat_train[train_pos]) + sum(y_train[!train_pos] != Yhat_train[!train_pos]),
                            sum(y_test[test_pos] != Yhat_test[test_pos]) + sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # LDA Model:
        lda_model <- MASS::lda(x=df_input_numeric[-test_index,],grouping=y_train)
        lda_prob <- predict(lda_model, newdata = df_input_numeric)
        minority_index <- which(attributes(lda_prob$posterior)$dimnames[[2]] == values$minority_class)
        lda_prob = lda_prob$posterior[,minority_index]
        Yhat <- ifelse(lda_prob >= 0.50,values$minority_class,values$majority_class)
        Yhat_train <- Yhat[-test_index]
        Yhat_test <- Yhat[test_index]
        my_table[3,2:5] <- c(round(100*mean(y_train != Yhat_train),digits=4),
                             round(100*mean(y_test != Yhat_test),digits=4),
                             sum(y_train[train_pos] != Yhat_train[train_pos]) + sum(y_train[!train_pos] != Yhat_train[!train_pos]),
                             sum(y_test[test_pos] != Yhat_test[test_pos]) + sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # Stepwise Logistic Regression Model:
        model_df$YYY <- ifelse(values$response_var== values$minority_class, 1, 0)
        intercept_only <- suppressWarnings(glm(YYY ~ 1, data=model_df[-test_index,], family=binomial))
        upper_formula <- formula(paste0("YYY ~ ", paste(names(model_df)[-1],collapse = " + ")))
        logistic_step_model <- suppressWarnings(step(intercept_only,scope=list(lower=formula(intercept_only),upper=upper_formula),direction="forward", trace = FALSE, k = log(nrow(model_df[-test_index,]))))
        logistic_prob <- predict(logistic_step_model, newdata=model_df, type="response")
        Yhat <- ifelse(logistic_prob >= 0.50, values$minority_class, values$majority_class)
        Yhat_train <- Yhat[-test_index]
        Yhat_test <- Yhat[test_index]
        my_table[4,2:5] =
          c(round(100*mean(y_train != Yhat_train),digits=4),
            round(100*mean(y_test != Yhat_test),digits=4),
            sum(y_train[train_pos] != Yhat_train[train_pos]) + sum(y_train[!train_pos] != Yhat_train[!train_pos]),
            sum(y_test[test_pos] != Yhat_test[test_pos]) + sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # Penalized (Ridge) Logistic Regression Model:
        penalized_cv_model <- suppressWarnings(glmnet::cv.glmnet(alpha=0,x=as.matrix(df_input_numeric[-test_index,]),y=model_df$YYY[-test_index],nfolds=10,family="binomial",type.measure="class"))
        penalized_cv_prob  <- predict(penalized_cv_model,newx=as.matrix(df_input_numeric),type="response",s="lambda.min")
        Yhat <- ifelse(penalized_cv_prob >= 0.50, minority_class, majority_class)
        Yhat_train <- Yhat[-test_index]
        Yhat_test <- Yhat[test_index]
        my_table[5,2:5] =
          c(round(100*mean(y_train != Yhat_train),digits=4),
            round(100*mean(y_test != Yhat_test),digits=4),
            sum(y_train[train_pos] != Yhat_train[train_pos]) + sum(y_train[!train_pos] != Yhat_train[!train_pos]),
            sum(y_test[test_pos] != Yhat_test[test_pos]) + sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # Random Forest Model:
        rf_model <- randomForest::randomForest(x = df_input[-test_index,], y = y_train, ntree=100)
        minority_index <- which(attributes(rf_model$votes)$dimnames[[2]] == minority_class)
        rf_prob <- rep(0,length(values$response_var))
        rf_prob[(1:length(values$response_var))[-test_index]] <- rf_model$votes[,minority_index]
        rf_prob_test <- predict(rf_model, df_input[test_index,], type="prob")
        minority_index <- which(attributes(rf_prob_test)$dimnames[[2]] == minority_class)
        rf_prob[test_index] <- rf_prob_test[,minority_index]
        Yhat <- ifelse(rf_prob >= 0.50, minority_class, majority_class)
        Yhat_train <- Yhat[-test_index]
        Yhat_test <- Yhat[test_index]
        my_table[6,2:5] =
          c(round(100*mean(y_train != Yhat_train),digits=4),
            round(100*mean(y_test != Yhat_test),digits=4),
            sum(y_train[train_pos] != Yhat_train[train_pos]) + sum(y_train[!train_pos] != Yhat_train[!train_pos]),
            sum(y_test[test_pos] != Yhat_test[test_pos]) + sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # XGBoost Model:
        df_xgb <- apply(df_input_numeric,2,function(x) if(is.numeric(x)){return(as.numeric(x))}else{return(x)})
        response_var01 = ifelse(values$response_var == minority_class,1,0)
        xgb_model <- xgboost::xgboost(data = df_xgb[-test_index,],label = response_var01[-test_index],objective = "binary:logistic",nrounds = 100,verbose=0)
        xgb_prob <- predict(xgb_model, df_xgb)
        Yhat <- ifelse(xgb_prob>=0.50,minority_class,majority_class)
        Yhat_train <- Yhat[-test_index]
        Yhat_test <- Yhat[test_index]
        my_table[7,2:5] =
          c(round(100*mean(y_train != Yhat_train),digits=4),
            round(100*mean(y_test != Yhat_test),digits=4),
            sum(y_train[train_pos] != Yhat_train[train_pos]) + sum(y_train[!train_pos] != Yhat_train[!train_pos]),
            sum(y_test[test_pos] != Yhat_test[test_pos]) + sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # SVM Model:
        svm_model <- e1071::svm(x=df_input_numeric[-test_index,],y=values$response_var[-test_index],scale=TRUE,kernel="radial",probability=TRUE)
        svm_prob <- attr(predict(svm_model, df_input_numeric, probability = TRUE), "probabilities")
        minority_index <- which(attributes(svm_prob)$dimnames[[2]] == minority_class)
        svm_prob <- svm_prob[,minority_index]
        Yhat <- ifelse(xgb_prob>=0.50,minority_class,majority_class)
        Yhat_train <- Yhat[-test_index]
        Yhat_test <- Yhat[test_index]
        my_table[8,2:5] =
          c(round(100*mean(y_train != Yhat_train),digits=4),
            round(100*mean(y_test != Yhat_test),digits=4),
            sum(y_train[train_pos] != Yhat_train[train_pos]) + sum(y_train[!train_pos] != Yhat_train[!train_pos]),
            sum(y_test[test_pos] != Yhat_test[test_pos]) + sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # Save model probabilities:
        values$list_output$tree_prob         = tree_prob
        values$list_output$lda_prob          = lda_prob
        values$list_output$logistic_prob     = logistic_prob
        values$list_output$penalized_cv_prob = penalized_cv_prob
        values$list_output$rf_prob           = rf_prob
        values$list_output$xgb_prob          = xgb_prob
        values$list_output$svm_prob          = svm_prob
        # Save initial table output:
        values$list_output$my_table = my_table[order(my_table[,5]),]
        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", "This module displays the performance of various classifiers in terms of user specified costs. Minority cases are considered positive outcomes, and majority cases are negative examples. Training/testing is automatically done using an 80/20 split. The table is presorted using 'Test Cost' by default.",style="font-size:20px;padding:40px;"),
            fluidRow(
              column(3, HTML("<div></div>")),
              column(2, div(numericInput(inputId = "tool1", label="False Positive Cost:", width="100%",min = 1, step = 1, value=1),style="display:inline-block;width:100%;text-align:center;")),
              column(2, offset=0,align="center", div(actionButton("uiButton",label = strong("Update Table")),style="display:inline-block;width:75%;text-align:center;")),
              column(2, div(numericInput(inputId = "tool2", label="False Negative Cost:", width="100%",min = 1, step = 1, value=1),style="display:inline-block;width:100%;text-align:center;")),
              column(3, HTML("<div></div>"))
            ),
            br(),
            fluidRow(
              column(2, HTML("<div></div>")),
              column(8, offset=0,align="center",DT::dataTableOutput('tableDT')),
              column(2, HTML("<div></div>"))
            )
          )
        })
        # compile alert messages
        if(myalerttext != ""){myalert = 1}
      }

      if(input$methodvar == "Bias Correction"){
        # Prepare response
        values$response_var <- ifelse(values$response_var== minority_class, 1, 0)
        # Removing any constant variables based on training data:
        df_input <- data
        unique_check <- apply(df_input, 2, function(x)length(unique(x)))
        constant_check <- which(unique_check == 1)
        myalerttext = ""
        if(length(constant_check) > 0){
          df_input <- df_input[,-constant_check]
          unique_check <- unique_check[-constant_check]
          myalerttext <- paste0(length(constant_check)," constant variable(s) were excluded. ")
        }
        # Limiting any categorical variables to a predefined number of levels:
        max_num_of_levels <- 10
        categorical_vars <- which(!unlist(lapply(df_input,is.numeric)))
        if(length(categorical_vars) > 0){
          level_check <- categorical_vars[which(unique_check[categorical_vars] > max_num_of_levels)]
          if(length(level_check) > 0){
            df_input <- df_input[,-level_check]
            myalerttext <- paste0(myalerttext, length(level_check), " categorical variable(s) dropped because exceeded maximum number of levels.")
          }
        }
        # Converting any character columns to factors:
        character_test <- which(!unlist(lapply(df_input,is.numeric)))
        if(length(character_test) > 0){
          for(ct in character_test){
            df_input[,ct] <- as.factor(df_input[,ct])
          }
        }
        # Stepwise Logistic Regression Model:
        model_df <- data.frame(YYY = values$response_var, df_input)
        intercept_only <- suppressWarnings(glm(YYY ~ 1, data=model_df, family=binomial))
        upper_formula <- formula(paste0("YYY ~ ", paste(names(model_df)[-1],collapse = " + ")))
        logistic_step_model <- suppressWarnings(step(intercept_only,scope=list(lower=formula(intercept_only),upper=upper_formula),direction="forward", trace = FALSE, k = log(nrow(model_df))))
        logistic_prob <- predict(logistic_step_model, newdata=model_df, type="response")
        values$list_output <- list(logistic_formula = logistic_step_model$formula, logistic_prob = logistic_prob, model_df = model_df)
        # create analysis ui components
        output$analysis_placeholder <- renderUI({
          div(
            p(id ="text", paste0("This module is appropriate when the minority class is either over or under represented in the sample. The user can provide the true percentage of the minority class in the underlying population, which reduces the observed sampling bias. The plot compares fitted probabilities between best subset logisitc regression and an adjusted version which corrects for various biases. Points below (above) the dashed red line reflect downward (upward) bias adjustments. For reference: class ", values$minority_class, " represents ", round(100*small_n/sum(TAB),digits=4),"% of the sampled data."),style="font-size:20px;padding:40px;"),
            fluidRow(
              column(2, HTML("<div></div>")),
              column(2, div(selectInput("tool1",label = "True Minority Class:", width="100%", choices=c(minority_class,majority_class), selected=minority_class),style="display:inline-block;width:100%;text-align:center;")),
              column(2, HTML("<div></div>")),
              column(4, div(sliderInput(inputId = "tool2", label="True Minority Class Percentage:", width="100%", min = 0.1, max = 50, value = 10, step = 0.5),style="display:inline-block;width:100%;text-align:center;")),
              column(2, HTML("<div></div>"))
            ),
            br(),
            fluidRow(
              column(3, HTML("<div></div>")),
              column(6, offset=0,align="center",plotOutput('plot', height = 300)),
              column(3, HTML("<div></div>"))
            )
          )
        })
        # compile alert messages
        if(myalerttext != ""){myalert = 1}
      }

      values$dfvar       = input$df
      values$responsevar = input$responsevar
      values$methodvar   = input$methodvar
      values$analysisvar = input$analysisvar
      values$analysis_ready_var = values$analysis_ready_var + 1
      values$go_time = NULL
      shinyjs::show(id="analysisdiv", anim=TRUE, animType = "slide")
      shinyjs::show(id="codediv", anim=TRUE, animType = "slide")
      shinyjs::hide(id="running", anim=FALSE)
      shinyjs::show(id="ready", anim=FALSE)
      shinyjs::hide(id="inputs",anim=FALSE)
      shinyjs::show(id="redobtn",anim=TRUE, animType = "fade")
      if(myalert > 0){
        ui_values$alertgo = paste0("<span style='color:red'><strong>Data Prep Notes:</strong></span> ", myalerttext)
        output$notes <- renderText({return(ui_values$alertgo)})
        shinyjs::show(id="divnotes", anim = TRUE, animType = "fade")
      }
   }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$redobtn,{
      shinyjs::hide(id="analysisdiv", anim=FALSE)
      shinyjs::hide(id="codediv", anim=FALSE)
      shinyjs::hide(id="redobtn", anim=FALSE)
      shinyjs::show(id="inputs",anim=TRUE,animType = "slide")
    })

    output$plot <- renderPlot({

      if( !(values$methodvar%in%c("Mahalanobis Distance","Decision Tree","Variable Selection: PCA","Bias Correction"))){
        print(plotNULL)
      }

      if(values$methodvar == "Mahalanobis Distance"){
       p <- try({
        X = data.frame(y = values$list_output$my_mahal, x = seq(1, length(values$list_output$my_mahal)))
        p <- ggplot2::ggplot(X, ggplot2::aes(x=y)) +
          ggplot2::geom_histogram(bins=30,colour="blue",fill = "#1E90FF") +
          ggplot2::ggtitle("Mahalanobis Distance") +
          ggplot2::xlab("Distance") +
          ggplot2::ylab("Frequency") +
          ggplot2::geom_vline(xintercept = input$tool1, colour = "red", size = 2) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,face="bold"))
       },silent = TRUE)
       if("try-error" %in% class(p)){p<-plotNULL}
       print(p)
      }

      if (values$methodvar == "Decision Tree"){
        if(is.null(values$list_output) | !is.numeric(input$tool1) | !is.numeric(input$tool2)){
          print(plotNULL)
        }else{
          tree_fit = rpart::rpart(YYY ~., data = values$list_output, method = "class",
                                parms = list(prior = c(input$tool2, 1-input$tool2)),
                                control = list(minsplit=20, minbucket=5, maxdepth = input$tool1))
          Yhat = as.character(predict(tree_fit, type = "class"))
          par(mfrow = c(1,1), xpd = NA)
          if(!is.null(tree_fit$splits)){
            plot(tree_fit, uniform=TRUE, main="Decision Tree")
            text(tree_fit, use.n=TRUE, cex=.8)
          }
          if(is.null(tree_fit$splits)){
            plot(-1,-1,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",axes=FALSE,main="Decision Tree")
            text(x=0.5,y=0.5,labels="Tree is a root!")
          }
        }
      }

      if(values$methodvar == "Variable Selection: PCA"){
        if(is.null(values$list_output$cum_va) | !is.numeric(input$tool1)){
          print(plotNULL)
        }else{
          # Variance explained Plot
          plot(c(0:length(values$list_output$cum_var)),c(0,values$list_output$cum_var),xlab="Number of PCs", ylab="Variance Explained (%)",ylim=c(0,100),type="l",
             main=paste0(round(values$list_output$cum_var[input$tool1],digits=2),"% of Variance Explained by ",input$tool1, " PCs"))
          lines(c(input$tool1,input$tool1),c(0,100),lty="solid",col="red")
        }
      }

      if(values$methodvar == "Bias Correction"){
        if(is.null(values$list_output$logistic_prob) | is.null(values$list_output$model_df) | !is.character(input$tool1) | !is.numeric(input$tool2)){
          print(plotNULL)
        }else{
          x = values$list_output$logistic_prob
          model_df = values$list_output$model_df
          if(input$tool1 == values$majority_class){
            x = 1 - x
            model_df$YYY = ifelse(model_df$YYY == 1, 0, 1)
          }
          bias_adjusted <- suppressWarnings(Zelig::zelig(values$list_output$logistic_formula, data = model_df,
                                                       case.control = "prior", bias.correct = TRUE,
                                                       model = "relogit", tau = input$tool2/100, cite=FALSE))
          corrected_coef <- bias_adjusted$get_coef()[[1]]
          pred_link <- rowSums(scale(model_df[,names(corrected_coef)[-1]], center=FALSE, scale=1/corrected_coef[-1])) + corrected_coef[1]
          adjusted_prob <- 1/(1 + exp(-1*pred_link))
          # Plot probability differences:
          plot(x,adjusted_prob, xlab="Unadjusted Probabilities",
             ylab="Bias Corrected Probabilites", xlim=c(0,1), ylim=c(0,1))
          lines(c(0,1),c(0,1), lty="dashed", lwd=2, col="red")
        }
      }
    })

    output$tableDT <- DT::renderDataTable({

        if(!(values$methodvar %in% c("Mahalanobis Distance","Single Variable: All","Single Variable: Categorical","Thresholds: Accuracy","Thresholds: True Positive","Prediction Models","Cost Models","Variable Selection: Supervised"))){
          return(tableNULL)
        }

        if(values$methodvar == "Mahalanobis Distance"){
          if(is.null(values$list_output$my_table) | is.null(values$list_output$my_mahal) | is.null(input$tool1)){
            return(tableNULL)
          } else {
            tmp_table = values$list_output$my_table
            tmp_table$Cutpoint[5] = input$tool1
            scored_user = score_threshold(x = values$list_output$my_mahal, group = values$response_var01,  pos_class = 1, cut = input$tool1, type = "upper")
            tmp_table$TP[5] = round(scored_user[[2]], digits = 3)
            tmp_table$FP[5] = round(scored_user[[1]], digits = 3)
            return(DT::datatable(tmp_table, options = list(dom = 't'),rownames= FALSE, colnames = c("Method ", "Cut Point", "FP %", "TP %")))
          }
        }

        if(values$methodvar == "Single Variable: All"){
          if(is.null(values$list_output)){
            return(tableNULL)
          } else {
            return(DT::datatable(values$list_output, options = list(pageLength = 8,dom = 'tp'),rownames= FALSE))
          }
        }

        if(values$methodvar == "Single Variable: Categorical"){
        if(values$categorical_message != "" | is.null(values$list_output)){
          return(DT::datatable(matrix(c("No Data"),nrow=1), options = list(pageLength = 8,dom = 'tp'),rownames= FALSE))
        }else{
          return(DT::datatable(values$list_output, options = list(pageLength = 8,dom = 'tp'),rownames= FALSE))
        }
      }

        if(values$methodvar == "Thresholds: Accuracy"){
        if(is.null(values$list_output)){
          return(tableNULL)
        } else {
          return(DT::datatable(values$list_output, options = list(pageLength = 6,dom = 'tp'),rownames= FALSE))
        }
      }

        if(values$methodvar == "Thresholds: True Positive"){
        if(is.null(values$list_output) | is.null(values$response_var01) | !is.numeric(input$tool1) | (input$tool1 > 10) | (input$tool1 < 0)){
          return(tableNULL)
        } else {
          Nc = ncol(values$list_output)
          user_matrix = as.data.frame(matrix(0,ncol=5,nrow=Nc))
          names(user_matrix) = c("Variable","Rule","FP %", "TP %", "Misclass %")
          user_matrix$Variable = names(values$list_output)
          for(nc in 1:Nc){user_matrix[nc,c(3,4,5,2)] = unlist(max_fp_threshold(x=values$list_output[,nc], group=values$response_var01, pos_class=1, max_fp = input$tool1/100))}
          user_matrix = user_matrix[order(as.numeric(user_matrix[,4]),decreasing = TRUE),]
          user_matrix[,3:5] = apply(user_matrix[,3:5], 2, function(x) round(as.numeric(x),digits=4) )
          return(DT::datatable(user_matrix, options = list(pageLength = 6,dom = 'tp'),rownames= FALSE))
        }
      }

        if(values$methodvar == "Prediction Models"){
          if(is.null(values$list_output$my_table)){
            return(tableNULL)
          } else {
            return(DT::datatable(values$list_output$my_table, options = list(dom = 't'),rownames= FALSE,caption=htmltools::tags$caption(strong("Training Performance"))))
          }
        }

        if(values$methodvar == "Cost Models"){
          if(is.null(values$list_output$my_table)){
            return(tableNULL)
          } else {
          return(DT::datatable(values$list_output$my_table, options = list(dom = 't'),rownames= FALSE,caption=htmltools::tags$caption(strong("Cost Performance"))))
          }
        }

        if(values$methodvar == "Variable Selection: Supervised"){
        if(is.null(values$list_output) | is.null(values$responsevar) | !is.character(input$tool1) | !is.numeric(input$tool2)){
          return(tableNULL)
        } else {
          # Preparing table:
          new_output_table = values$list_output
          for(ii in 2:4){new_output_table[,ii] <- as.numeric(as.character(new_output_table[,ii]))}
          order_by_loc <- which(input$tool1 == names(new_output_table))
          order_direction <- ifelse(input$tool1 == "Logistic AIC", FALSE, TRUE)
          new_output_table <- new_output_table[order(new_output_table[,order_by_loc], decreasing = order_direction),]
          for(ii in 2:4){new_output_table[,ii] <- round(new_output_table[,ii], digits=4)}
          row.names(new_output_table) <- 1:nrow(new_output_table)
          new_output_table$`Selected?` <- "No"
          new_output_table$`Selected?`[1:input$tool2] = "Yes"
          # Subset data:
          subset_variables <-  new_output_table$Variable[1:input$tool2]
          data <- reactiveData()
          values$df_output <- data[,c(values$responsevar, subset_variables)]
          return(DT::datatable(new_output_table, options = list(pageLength = 6,dom = 'tp'),rownames=TRUE))
        }
      }
    })

    output$tableDT_2 <- DT::renderDataTable({
      if(values$methodvar != "Prediction Models"){return(tableNULL)}

      if(values$methodvar == "Prediction Models"){
        if(startsWith(input$tool1, "None")){
          my_models = c("Baseline","Random Forest","LDA","Stepwise Logistic","Penalized Logistic","XGBoost","SVM","KNN (k=5)")
          my_test_table = data.frame( Model = my_models, matrix(0,ncol=5,nrow=length(my_models)))
          names(my_test_table)[2:6] = c("Misclass %","AUC-ROC","AUC-PR", "TP %", "FP %")
          return(DT::datatable(my_test_table[-(1:length(my_models)),], options = list(dom = 't'),rownames= FALSE,caption=htmltools::tags$caption(strong("Testing Performance"))))
        }else{
          # If a data set is selected:
          minority_class = values$minority_class
          majority_class = values$majority_class
          df_to_score = get(input$tool1, envir = .GlobalEnv)
          test_response = df_to_score[,values$responsevar]
          test_response01 = ifelse(test_response == minority_class, 1, 0)
          pos_index = which(test_response01 == 1)
          # Converting any character columns to factors:
          character_test <- which(!unlist(lapply(df_to_score,is.numeric)))
          if(length(character_test) > 0){
            for(ct in character_test){
              df_to_score[,ct] <- as.factor(df_to_score[,ct])
            }
          }
          # Training Output Prep:
          my_models = c("Baseline","Random Forest","LDA","Stepwise Logistic","Penalized Logistic","XGBoost","SVM","KNN (k=5)")
          my_test_table = data.frame( Model = my_models, matrix(0,ncol=5,nrow=length(my_models)))
          names(my_test_table)[2:6] = c("Misclass %","AUC-ROC","AUC-PR", "TP %", "FP %")
          # Baseline Stupid Model:
          my_test_table[1,2:6] = c(100*mean(test_response01), 0.5, 0.5, 0, 0)
          # Random Forest Model:
          rf_prob = predict(values$list_output$rf_model, df_to_score, type="prob")
          rf_pred = predict(values$list_output$rf_model, df_to_score, type="response")
          minority_index = which(attributes(rf_prob)$dimnames[[2]] == minority_class)
          majority_index = which(attributes(rf_prob)$dimnames[[2]] == majority_class)
          my_test_table[2,2:6] = c(100*mean(rf_pred != test_response),
                                   PRROC::roc.curve(scores.class0 = rf_prob[,minority_index],weights.class0 = test_response01)$auc,
                                   PRROC::pr.curve(rf_prob[,majority_index],rf_prob[,minority_index])$auc.integral,
                                   100*mean(rf_pred[pos_index] == test_response[pos_index]),
                                   100*mean(rf_pred[-pos_index] != test_response[-pos_index]))
          # LDA model:
          lda_pred  = predict(values$list_output$lda_model, newdata = df_to_score[,colnames(values$list_output$lda_model$means)])
          minority_index = which(attributes(lda_pred$posterior)$dimnames[[2]] == minority_class)
          majority_index = which(attributes(lda_pred$posterior)$dimnames[[2]] == majority_class)
          my_test_table[3,2:6] = c(100*mean(lda_pred$class != test_response),
                                   PRROC::roc.curve(scores.class0 = lda_pred$posterior[,minority_index],weights.class0 = test_response01)$auc,
                                   PRROC::pr.curve(lda_pred$posterior[,majority_index],lda_pred$posterior[,minority_index])$auc.integral,
                                   100*mean(lda_pred$class[pos_index] == test_response[pos_index]),
                                   100*mean(lda_pred$class[-pos_index] != test_response[-pos_index]))
          # Stepwise Logistic Regression:
          logistic_step_prob = predict(values$list_output$logistic_step_model, newdata = df_to_score, type="response")
          logistic_step_pred = ifelse(logistic_step_prob >= 0.5, minority_class, majority_class)
          my_test_table[4,2:6] = c(100*mean(logistic_step_pred  != test_response),
                                   PRROC::roc.curve(scores.class0 = logistic_step_prob,weights.class0 = test_response01)$auc,
                                   PRROC::pr.curve(1-logistic_step_prob,logistic_step_prob)$auc.integral,
                                   100*mean(logistic_step_pred[pos_index] == test_response[pos_index]),
                                   100*mean(logistic_step_pred[-pos_index] != test_response[-pos_index]))
          # Penalized (Ridge) Logistic Regression:
          penalized_cv_prob  = predict(values$list_output$penalized_cv_model,newx=as.matrix(df_to_score[,rownames(coef(values$list_output$penalized_cv_model))[-1]]),type="response")
          penalized_cv_pred = ifelse(penalized_cv_prob>=0.5,minority_class,majority_class)
          my_test_table[5,2:6] = c(100*mean(penalized_cv_pred  != test_response),
                                   PRROC::roc.curve(scores.class0 = penalized_cv_prob,weights.class0 = test_response01)$auc,
                                   PRROC::pr.curve(1-penalized_cv_prob,penalized_cv_prob)$auc.integral,
                                   100*mean(penalized_cv_pred[pos_index] == test_response[pos_index]),
                                   100*mean(penalized_cv_pred[-pos_index] != test_response[-pos_index]))
          # XGBoost Model:
          df_xgb = apply(df_to_score[,names(values$df_input)],2,function(x) if(is.numeric(x)){return(as.numeric(x))}else{return(x)})
          xgb_prob = predict(values$list_output$xgb_model, df_xgb)
          xgb_pred = ifelse(xgb_prob>=0.5,minority_class,majority_class)
          my_test_table[6,2:6] = c(100*mean(xgb_pred  != test_response),
                                   PRROC::roc.curve(scores.class0 = xgb_prob,weights.class0 = test_response01)$auc,
                                   PRROC::pr.curve(1-xgb_prob,xgb_prob)$auc.integral,
                                   100*mean(xgb_pred[pos_index] == test_response[pos_index]),
                                   100*mean(xgb_pred[-pos_index] != test_response[-pos_index]))
          # SVM Model:
          svm_pred <- predict(values$list_output$svm_model, df_to_score[,names(values$list_output$svm_model$x.scale$`scaled:center`)], probability = TRUE)
          svm_prob <- attr(svm_pred, "probabilities")
          minority_index <- which(attributes(svm_prob)$dimnames[[2]] == minority_class)
          majority_index <- which(attributes(svm_prob)$dimnames[[2]] == majority_class)
          my_test_table[7,2:6] <- c(100*mean(svm_pred != test_response),
                                    PRROC::roc.curve(scores.class0 = svm_prob[,minority_index],weights.class0 = test_response01)$auc,
                                    PRROC::pr.curve(svm_prob[,majority_index],svm_prob[,minority_index])$auc.integral,
                                    100*mean(svm_pred[pos_index] == test_response[pos_index]),
                                    100*mean(svm_pred[-pos_index] != test_response[-pos_index]))
          # KNN (k=5) Model:
          knn_test <-  class::knn(train=values$df_input, test=df_to_score[,names(values$df_input)], cl=values$response_var,prob=TRUE,k=5)
          knn_pred  <-  as.vector(knn_test)
          knn_prob  <- attributes(knn_test)$prob
          knn_prob[knn_pred == majority_class] <- 1-knn_prob[knn_pred == majority_class]
          my_test_table[8,2:6] <- c(100*mean(knn_pred  != test_response),
                                    PRROC::roc.curve(scores.class0 = knn_prob,weights.class0 = test_response01)$auc,
                                    PRROC::pr.curve(1-knn_prob,knn_prob)$auc.integral,
                                    100*mean(knn_pred[pos_index] == test_response[pos_index]),
                                    100*mean(knn_pred[-pos_index] != test_response[-pos_index]))

          # Prepare table:
          my_test_table = my_test_table[order(my_test_table[,2]),]
          for(ii in 2:6){
            my_digits = ifelse(ii > 4, 2, 6)
            if(ii == 2){my_digits = 4}
            my_test_table[,ii] = round(my_test_table[,ii],digits=my_digits)
          }
          return(DT::datatable(my_test_table, options = list(dom = 't'),rownames= FALSE,caption=htmltools::tags$caption(strong("Testing Performance"))))#, colnames = c("Method ", "Cut Point", "FP %", "TP %")))
        }
      }
    })

    output$tablePrint <- renderPrint({
      if( !(values$methodvar%in%c("Decision Tree","SMOTE","Train/Test Split","One Class SVM"))){
        cat(printNULL)
      }

      if(values$methodvar == "Decision Tree"){
        if(is.null(values$list_output) | !is.numeric(input$tool1) | !is.numeric(input$tool2)){
          cat(printNULL)
        }else{
          tree_fit = rpart::rpart(YYY ~., data = values$list_output, method = "class",
                                parms = list(prior = c(input$tool2, 1-input$tool2)),
                                control = list(minsplit=20, minbucket=5, maxdepth = input$tool1))
          Yhat = as.character(predict(tree_fit, type = "class"))
          Y = values$list_output$YYY
          cat(paste0("Total Cases: ", length(Y),"\n",
                   "Total Fail:  ", sum(Y == values$minority_class), "\n",
                   "Total Pass:  ", sum(Y != values$minority_class), "\n \n",
                   "True Positive Rate:      ", round(100*mean(Yhat[Y == values$minority_class] == values$minority_class), digits = 2), "\n",
                   "False Positive Rate:     ", round(100*mean(Yhat[Y != values$minority_class] == values$minority_class), digits = 2), "\n",
                   "Misclassification Rate:  ", round(100*mean(Yhat != Y), digits = 2)))
        }
      }

      if(values$methodvar == "SMOTE"){
        if(is.null(values$df_input) | is.null(values$responsevar) | is.null(values$dfvar) | !is.numeric(input$tool1) | !is.numeric(input$tool2)){
          cat(printNULL)
        }else{
          myFormula = as.formula(paste0(values$responsevar, "~."))
          df_output = DMwR::SMOTE(myFormula, data=values$df_input ,perc.over=(input$tool1 - 1)*100, perc.under=(input$tool2)*(input$tool1/(input$tool1-1))*100, k=5)
          values$df_output = df_output
          cat(paste0("Original data set: ", values$dfvar))
          print(table(values$df_input[,1]))
          cat(paste0("\nSMOTE'd data set: SMOTE_", values$dfvar))
          print(table(values$df_output[,1]))
        }
      }

      if(values$methodvar == "Train/Test Split"){
        if(is.null(values$list_output$majorclass) | is.null(values$list_output$minorclass) | is.null(values$responsevar) | !is.numeric(input$tool1) | !is.numeric(input$tool2)){
          cat(printNULL)
        }else{
          n0 = nrow(values$list_output$majorclass)
          n1 = nrow(values$list_output$minorclass)
          index_major = sample.int(n=n0, size=ceiling(n0*input$tool1/100), replace=FALSE)
          index_minor = sample.int(n=n1, size=ceiling(n1*input$tool2/100), replace=FALSE)
          train_df = rbind(values$list_output$majorclass[index_major,],values$list_output$minorclass[index_minor,])
          test_df  = rbind(values$list_output$majorclass[-index_major,],values$list_output$minorclass[-index_minor,])
          TAB_train = table(train_df[,values$responsevar])
          TAB_test  = table(test_df[,values$responsevar])
          cat(paste0("Training data set (", round(100*TAB_train[values$minority_class]/sum(TAB_train),digits=4) , "% minority class):"))
          print(TAB_train)
          cat(paste0("Testing data set (", round(100*TAB_test[values$minority_class]/sum(TAB_test),digits=4) , "% minority class):"))
          print(TAB_test)
          values$df_output = list(TRAIN = train_df, TEST = test_df)
        }
      }

      if(values$methodvar == "One Class SVM"){
        if(is.null(values$list_output$svmdata) | !is.numeric(input$tool1)){
          cat(printNULL)
        } else {
            svm_model <- e1071::svm(x=values$list_output$svmdata,y=NULL,type='one-classification',nu=input$tool1/100,scale=TRUE,kernel="radial")
            svm_pred <- ifelse(svm_model$fitted, values$majority_class, values$minority_class)
            pos_index <- which(values$response_var==values$minority_class)
            cat(paste0("True Positive %: ",round(100*mean(values$response_var[pos_index] == svm_pred[pos_index]),digits=4)))
            cat(paste0("\nFalse Positive %: ",round(100*mean(values$response_var[-pos_index] != svm_pred[-pos_index]),digits=4)))
            cat(paste0("\nMisclassification %: ",round(100*mean(values$response_var != svm_pred),digits=4),"\n\n"))
            print(table(Predicted=svm_pred,Truth=values$response_var))
        }
      }
    })

    observeEvent(input$uiButton,{
      if(values$methodvar == "SMOTE"){
        my_data_set_name = paste0("SMOTE_",values$dfvar)
        assign(my_data_set_name, values$df_output, envir=globalenv())
        smote_alert = paste0("<span style=\"color:red;\"><strong>Success:</strong></span> The SMOTE'd data set named ",my_data_set_name, " is now stored in memory. You can use it by clicking the start over button appearing above.")
        values$ui_update = 1
        values$df_selected = my_data_set_name
        output$newdata<- renderText({return(smote_alert)})
        shinyjs::show(id="divnewdata", anim = TRUE, animType = "fade")
      }

      if(values$methodvar == "Train/Test Split"){
        my_data_set_name_train = paste0("TRAIN_",values$dfvar)
        my_data_set_name_test = paste0("TEST_",values$dfvar)
        assign(my_data_set_name_train, values$df_output$TRAIN, envir=globalenv())
        assign(my_data_set_name_test, values$df_output$TEST, envir=globalenv())
        data_alert = paste0("<span style=\"color:red;\"><strong>Success:</strong></span> The new data sets named ",my_data_set_name_train, " and ", my_data_set_name_test," are now stored in memory. You can use them for prediction models by clicking the start over button appearing above.")
        values$ui_update = 1
        values$df_selected = my_data_set_name_train
        output$newdata<- renderText({return(data_alert)})
        shinyjs::show(id="divnewdata", anim = TRUE, animType = "fade")
      }

      if(values$methodvar == "Variable Selection: Supervised"){
        my_data_set_name = paste0("SUBSET_",values$dfvar)
        assign(my_data_set_name, values$df_output, envir=globalenv())
        data_alert = paste0("<span style=\"color:red;\"><strong>Success:</strong></span> The subset data is named ",my_data_set_name, " and is now stored in memory. You can use it by clicking the start over button appearing above.")
        values$ui_update = 1
        values$df_selected = my_data_set_name
        output$newdata<- renderText({return(data_alert)})
        shinyjs::show(id="divnewdata", anim = TRUE, animType = "fade")
      }

      if(values$methodvar == "Variable Selection: PCA"){
        # Subset data:
        my_data_set_name = paste0("PCA_",values$dfvar)
        assign(my_data_set_name, values$list_output$df_tmp[,1:(input$tool1+1)], envir=globalenv())
        data_alert = paste0("<span style=\"color:red;\"><strong>Success:</strong></span> The PCA data is named ",my_data_set_name, " and is now stored in memory. You can use it by clicking the start over button appearing above.")
        values$ui_update = 1
        values$df_selected = my_data_set_name
        output$newdata<- renderText({return(data_alert)})
        shinyjs::show(id="divnewdata", anim = TRUE, animType = "fade")
      }

      if(values$methodvar == "Cost Models"){
        # Update table:
        my_table = values$list_output$my_table
        new_decision_rule = input$tool1/(input$tool1 + input$tool2)
        y_train = values$response_var[-values$list_output$test_index]
        y_test  = values$response_var[values$list_output$test_index]
        train_pos = (y_train == values$minority_class)
        test_pos = (y_test == values$minority_class)
        # Baseline Stupid Model:
        my_table[(my_table[,1] == "Baseline"),4:5] =
          c(input$tool2*sum(y_train == values$minority_class),
            input$tool2*sum(y_test == values$minority_class))
        # Decision Tree Model:
        Yhat <- ifelse(values$list_output$tree_prob >= new_decision_rule,values$minority_class,values$majority_class)
        Yhat_train = Yhat[-values$list_output$test_index]
        Yhat_test= Yhat[values$list_output$test_index]
        my_table[which(my_table[,1] == "Decision Tree"),2:5] =
          c(round(100*mean(y_train != Yhat_train),digits=4),
            round(100*mean(y_test != Yhat_test),digits=4),
            input$tool2*sum(y_train[train_pos] != Yhat_train[train_pos]) + input$tool1*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
            input$tool2*sum(y_test[test_pos] != Yhat_test[test_pos]) + input$tool1*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # LDA Model:
        Yhat <- ifelse(values$list_output$lda_prob >= new_decision_rule,values$minority_class,values$majority_class)
        Yhat_train <- Yhat[-values$list_output$test_index]
        Yhat_test <- Yhat[values$list_output$test_index]
        my_table[which(my_table[,1] == "LDA"),2:5] =
          c(round(100*mean(y_train != Yhat_train),digits=4),
            round(100*mean(y_test != Yhat_test),digits=4),
            input$tool2*sum(y_train[train_pos] != Yhat_train[train_pos]) + input$tool1*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
            input$tool2*sum(y_test[test_pos] != Yhat_test[test_pos]) + input$tool1*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # Logistic Stepwise Regression Model:
        Yhat <- ifelse(values$list_output$logistic_prob >= new_decision_rule, values$minority_class, values$majority_class)
        Yhat_train <- Yhat[-values$list_output$test_index]
        Yhat_test <- Yhat[values$list_output$test_index]
        my_table[which(my_table[,1] == "Stepwise Logistic"),2:5] =
          c(round(100*mean(y_train != Yhat_train),digits=4),
            round(100*mean(y_test != Yhat_test),digits=4),
            input$tool2*sum(y_train[train_pos] != Yhat_train[train_pos]) + input$tool1*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
            input$tool2*sum(y_test[test_pos] != Yhat_test[test_pos]) + input$tool1*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # Penalized (Ridge) Logistic Regression Model:
        Yhat <- ifelse(values$list_output$penalized_cv_prob >= new_decision_rule, values$minority_class, values$majority_class)
        Yhat_train <- Yhat[-values$list_output$test_index]
        Yhat_test <- Yhat[values$list_output$test_index]
        my_table[which(my_table[,1] == "Penalized Logistic"),2:5] =
          c(round(100*mean(y_train != Yhat_train),digits=4),
            round(100*mean(y_test != Yhat_test),digits=4),
            input$tool2*sum(y_train[train_pos] != Yhat_train[train_pos]) + input$tool1*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
            input$tool2*sum(y_test[test_pos] != Yhat_test[test_pos]) + input$tool1*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # Random Forest Model:
        Yhat <- ifelse(values$list_output$rf_prob >= new_decision_rule, values$minority_class, values$majority_class)
        Yhat_train <- Yhat[-values$list_output$test_index]
        Yhat_test <- Yhat[values$list_output$test_index]
        my_table[which(my_table[,1] == "Random Forest"),2:5] =
          c(round(100*mean(y_train != Yhat_train),digits=4),
            round(100*mean(y_test != Yhat_test),digits=4),
            input$tool2*sum(y_train[train_pos] != Yhat_train[train_pos]) + input$tool1*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
            input$tool2*sum(y_test[test_pos] != Yhat_test[test_pos]) + input$tool1*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # XGBoost Model:
        Yhat <- ifelse(values$list_output$xgb_prob >= new_decision_rule, values$minority_class, values$majority_class)
        Yhat_train <- Yhat[-values$list_output$test_index]
        Yhat_test <- Yhat[values$list_output$test_index]
        my_table[which(my_table[,1] == "XGBoost"),2:5] =
          c(round(100*mean(y_train != Yhat_train),digits=4),
            round(100*mean(y_test != Yhat_test),digits=4),
            input$tool2*sum(y_train[train_pos] != Yhat_train[train_pos]) + input$tool1*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
            input$tool2*sum(y_test[test_pos] != Yhat_test[test_pos]) + input$tool1*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # SVM Model:
        Yhat <- ifelse(values$list_output$svm_prob >= new_decision_rule, values$minority_class, values$majority_class)
        Yhat_train <- Yhat[-values$list_output$test_index]
        Yhat_test <- Yhat[values$list_output$test_index]
        my_table[which(my_table[,1] == "SVM"),2:5] =
          c(round(100*mean(y_train != Yhat_train),digits=4),
            round(100*mean(y_test != Yhat_test),digits=4),
            input$tool2*sum(y_train[train_pos] != Yhat_train[train_pos]) + input$tool1*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
            input$tool2*sum(y_test[test_pos] != Yhat_test[test_pos]) + input$tool1*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
        # Sort output:
        values$list_output$my_table = my_table[order(my_table[,5]),]
      }
    })

    output$code.R <- downloadHandler(
      filename = function() { paste0(gsub(" ","_",gsub(":","", values$methodvar)),"_","script.R") },
      content = function(file) {
        writeLines(text=code_output(methodvar=values$methodvar,
                                    dfvar=values$dfvar,
                                    responsevar=values$responsevar,
                                    tool1=input$tool1,
                                    tool2=input$tool2)
                   ,con = file)
      },
      contentType = NA
    )

  # End program
  observeEvent(input$donebtn,{stopApp(returnValue = invisible())})
  session$onSessionEnded(function() {stopApp()})

 })
 runApp(app, launch.browser = TRUE)
}


