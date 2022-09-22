sep <- setNames(c(",", ".", ":", ";", ""), c('comma', 'dot', 'colon', 'semicolon', 'white space'))
dec <- setNames(c(".", ","),c('dot', 'comma'))
not_sel <- "No selected"
# Define server logic
server <- function(input, output) {
  

  data <- reactiveVal(NULL)
  plotSP <- reactiveVal(NULL)
  formulaOLS <- reactiveVal(NULL)
  model_ols <-reactiveVal(NULL)
  model_ols_assumption <-reactiveVal(NULL)
  
  formulaPROBIT<- reactiveVal(NULL)
  model_probit <-reactiveVal(NULL)
  model_probit_assumption <-reactiveVal(NULL)
  model_probit_margins <- reactiveVal(NULL)
  
  model1_report <- reactiveVal(NULL)
  model2_report <- reactiveVal(NULL)
  plot1_report <- reactiveVal(NULL)
  plot2_report <- reactiveVal(NULL)
  
  #1. DATA PREPERATION ----
  observeEvent(input$bChangeVarName, {
    df <- data()
    colnames(df)[colnames(df) == input$VarChangeName] <- input$VarNewName 
    data(df)
  })
  
  observeEvent(input$bUpdateData, {
    df <- data()
    df <- df %>% filter(!(!!as.symbol(input$Variable) %in% input$ValuesToDrop))
    data(df)
  })
  
  observeEvent(input$loadData, {
    req(input$file)
    if (input$file$type != "text/csv") stop("No CSV")
    df <- read.csv(input$file$datapath, header = input$header, sep = input$sep, dec = input$dec)
    
    msg = "Data has been loaded"
    showNotification(msg, duration = 5, type = 'message')
    data(df)

  })
  
  output$Variable <- renderUI({
    if (is.null(data()))
      return()
    
    selectInput('Variable',"",choices = colnames(data()))
  })
  output$ValuesToDrop <- renderUI({
    if (is.null(data()))
      return()
    
    selectInput('ValuesToDrop', "", choices = unique(data()[,input$Variable]), multiple = TRUE)
  })
  
  
  output$VarDeleteMissing <- renderUI({
    if (is.null(data()))
      return()
    
    selectInput('VarDeleteMissing',"",choices = colnames(data()),  multiple = TRUE)
  })
  
  output$bDeleteMissing <- renderUI({
    if (is.null(data()))
      return()
    
    actionButton('bDeleteMissing','Delete missing values')
    
  })
  
  observeEvent(input$bDeleteMissing, {
    df <- data()
    df <- df[complete.cases(df[ , input$VarDeleteMissing]),]
    data(df)
  })
  
  
  
  
  
  output$VarNewName <- renderUI({
    if (is.null(data()))
      return()
    textInput('VarNewName', '')
  })
  
  output$VarChangeName <- renderUI({
    if (is.null(data()))
      return()
    selectInput('VarChangeName',"",choices = colnames(data()))
  })
  
  output$bChangeVarName <- renderUI({
    if (is.null(data()))
      return()
    actionButton('bChangeVarName','Change Column Name')
  })
  
  output$bUpdateData <- renderUI({
    if (is.null(data()))
      return()
    actionButton('bUpdateData','Delete rows')
  })
  
  
  output$hist1Variable <- renderUI({
    if (is.null(data()))
      return()
    selectInput('hist1Variable', "", choices = c("No selected", colnames(data())))
  })
  
  
  output$hist1PlotBins <- renderUI({
    if (is.null(data()))
      return()
    
    if (input$hist1Variable == not_sel){
      min_val = 1
      max_val = 100
      df_val = 10
    } else {
      ColumnRNumb <- length(data()[, input$hist1Variable])
      min_val = 1
      max_val = ColumnRNumb
      df_val = as.integer(sqrt(ColumnRNumb))
    }
    numericInput('hist1PlotBins', "", min = min_val, max = max_val ,value = df_val )

  })
  
  output$hist1Plot <-renderPlot({
    req(input$hist1Variable)
    req(input$hist1PlotBins)
    if (input$hist1Variable == not_sel)
      return()
    Column <- data()[, input$hist1Variable] 
    if (!is.numeric(Column)) {
      msg <- 'Selected variable in hist 1 is not numeric'
      showNotification(msg, type = 'error')
      
      return()
    }
    
    numbBins = input$hist1PlotBins
    g <- ggplot(data(), aes(x = !!as.symbol(input$hist1Variable)))
    
    g <-  g + geom_histogram(bins = numbBins, fill = 'steelblue', col = 'black') + theme_minimal()
    plot1_report(g)
    g
  })
  
  output$ScaterPlotDim1 <- renderUI({
    if (is.null(data()))
      return()
    selectInput('ScaterPlotDim1', label = "Select Numeric Variable", choices = c("No selected", colnames(data())))
  })
  
  output$ScaterPlotDim2 <- renderUI({
    if (is.null(data()))
      return()
    selectInput('ScaterPlotDim2',label = "Select Numeric Variable", choices = c("No selected", colnames(data())))
  })
  
  output$ScaterPlotDim3 <- renderUI({
    if (is.null(data()))
      return()
    selectInput('ScaterPlotDim3',label = "Select Factor Variable", choices = c("No selected", colnames(data())))
  })
  output$MakeScatterPlot <- renderUI({
    if (is.null(data()))
      return()
    actionButton('MakeScatterPlot', 'Run', icon = icon("fa-solid fa-play"))
  })
  
  observeEvent(input$MakeScatterPlot, {
    if (is.null(data()))
      return()
    p <- draw_plot_1(data(), input$ScaterPlotDim1, input$ScaterPlotDim2, input$ScaterPlotDim3)
    plot2_report(p)
    plotSP(p)
  })

  
  output$ScaterPlot4Dim <-renderPlot({
    if (is.null(data()))
      return()
    plotSP()
  })

  output$DisplayFile <- renderDataTable(data(), 
                                        options = list(
                                          lengthMenu = list(c(5, 10, 15, -1), c('5', '15', 'All')),
                                          pageLength =5,
                                          searching = FALSE,
                                          scrollX = T))
  
  output$StatsVar1 <- renderPrint({
    if(input$hist1Variable == ''){
      return()
    }
    variable <- data()[, input$hist1Variable] 
    summary(variable)
  })  
  
  
  
  
  
  #2. MODELING ----
  ##*** OLS ***----
  output$Dynamic_Bucket_OLS <-  renderUI({
    
    columns <- colnames(data())
    
    bucket_list(
      header = "Select variables to your model",
      group_name = "bucket_list_group",
      orientation = "horizontal",
      add_rank_list(
        text = "Drag from here",
        labels = columns,
        input_id = "choose_var_OLS"
      ),
      add_rank_list(
        text = "to here",
        labels = NULL,
        input_id = "selected_var_OLS"
      )
    )
  })
  
  
  output$OLS_Formula <- renderPrint({

    if(length(input$selected_var_OLS) < 2){
      print("Select at least two variables")
      return()
    }
    
    vars <- input$selected_var_OLS
    if(!is.numeric(data()[,vars[1]])){
      print("Y variable should be numeric")
      return()
    }
    formula <- formula_model(vars[1], vars[-1])
    
    formulaOLS(formula)
    print(formula)
    
  })
  
  observeEvent(input$OLS_Estimate, {
    if (is.null(data())){
      msg = "No data loaded"
      showNotification(msg, duration = 5, type = 'error')
      return()
    }
    if (is.null(formulaOLS())){
      msg = "No formula of the model selected"
      showNotification(msg, duration = 5, type = 'error')
      return()
    }
    
    if (formulaOLS() == 'Select at least two variables' || formulaOLS() == 'Y variable should be numeric'){
      msg = "No formula of the model selected"
      showNotification(msg, duration = 5, type = 'error')
      return()
    }
    formula = formulaOLS()
    df <- data()
    
    df[is.na(df) | df=="Inf"] = NA
    model_ols_temp = lm(formula, data = df)
    model_ols(model_ols_temp)
    
  })
  
  output$OLS_Summary <- renderPrint({
    if(is.null(model_ols()))
      return()
    
    res <- stargazer(model_ols(), type="text", 
              align=TRUE, style="default", df=FALSE)
    model1_report(res)
    res
    # model1_report(summary(model_ols()))
    # print(summary(model_ols()))
  })
  
  observeEvent(input$bCheckAssumption_OLS, {
    if(is.null(model_ols())){
      msg = "No model have been estimated."
      showNotification(msg, duration = 5, type = 'error')
      return()
    }

    model <- model_ols()
    
    # specification test
    reset <- resettest(model, power=2:3, type="fitted")
    
    # Breusch's and Pagan's test
    bp <- bptest(model, studentize=TRUE)
    
    # Is the error term normally distributed?
    jb_plot <- ggplot() + geom_histogram(aes(x = model$residuals) ,  fill = 'steelblue', col = 'black') + theme_minimal()
    jb <- jarque.bera.test(model$residuals)
    
    
    results <- list(reset, bp, jb, jb_plot)
    
    model_ols_assumption(results)
  })
  
  output$Reset_test_OLS <- renderPrint({
    if(is.null(model_ols_assumption()))
      return()
    
    res <- model_ols_assumption()
    print(res[[1]])
  })
  
  output$BP_test_OLS <- renderPrint({
    if(is.null(model_ols_assumption()))
      return()
    
    res <- model_ols_assumption()
    print(res[[2]])
  })
  
  output$JB_test_OLS <- renderPrint({
    if(is.null(model_ols_assumption()))
      return()
    
    res <- model_ols_assumption()
    print(res[[3]])
  })
  
  output$JB_plot_OLS <- renderPlot({
    if(is.null(model_ols_assumption()))
      return()
    
    res <- model_ols_assumption()
    res[[4]]
  })
  
  ##*** PROBIT LOGIT ***----
  output$Dynamic_Bucket_Probit <-  renderUI({
    columns <-colnames(data())

    bucket_list(
      header = "Select variables to your model",
      group_name = "bucket_list_group",
      orientation = "horizontal",
      add_rank_list(
        text = "Drag from here",
        labels = columns,
        input_id = "choose_var_Probit"
      ),
      add_rank_list(
        text = "to here",
        labels = NULL,
        input_id = "selected_var_Probit"
      )
    )
  })
  
  output$Probit_Formula <- renderPrint({
    
    if(length(input$selected_var_Probit) < 2){
      print("Select at least two variables")
      return()
    }
    
    vars <- input$selected_var_Probit
    y_var <- data()[,vars[1]]
    y_var <- sort(unique(na.omit(y_var)))
    
    if(length(y_var) != 2){
      print("The explanatory variable is not correct. It should be a binary variable with two levels {1,0}")
      return()
    }
    
    if(y_var[1] != 0 || y_var[2] != 1){
      print("The explanatory variable is not correct.It should be a binary variable with two levels {1,0}")
      return()
    }
    
    formula <- formula_model(vars[1], vars[-1])
    
    formulaPROBIT(formula)
    print(formula)
  })
  
  observeEvent(input$Probit_Estimate, {
    if(is.null(data())){
      msg = "No data loaded"
      showNotification(msg, duration = 5, type = 'error')
      return()
    }
    if (is.null(formulaPROBIT())){
      msg = "No formula of the model selected"
      showNotification(msg, duration = 5, type = 'error')
      return()
    }
    
    if (formulaPROBIT() == 'Select at least two variables'  ||  formulaPROBIT()  == 'The explanatory variable is not correct.It should be a binary variable with two levels {1,0}'){
      msg = "No formula of the model selected"
      showNotification(msg, duration = 5, type = 'error')
      return()
    }
    
    
    formula = formulaPROBIT()
    df = data()
    df[is.na(df) | df=="Inf"] = NA
    model_probit_temp = glm(formula, data = data(), family=binomial(link="probit")) 
    
    model_probit(model_probit_temp)
    
  })
  
  output$Probit_Summary <- renderPrint({
    if(is.null(model_probit()))
      return()
    
    res <- stargazer(model_probit(), type="text", 
                     align=TRUE, style="default", df=FALSE)
    model2_report(res)
    res
    # model2_report(summary(model_probit()))
    # print(summary(model_probit()))
  })
  
  
  observeEvent(input$CalculateMargins, {
    
    if(is.null(model_probit())){
      msg = "No model has been estimated."
      showNotification(msg, duration = 5, type = 'error')
      return()
    }
    if(is.null(data())){
      msg = "No data has been loaded."
      showNotification(msg, duration = 5, type = 'error')
      return()
    }

    form_probit <- formulaPROBIT()
    
    # marginal effects for the average observation -> atmean - TRUE
    meff = probitmfx(formula=form_probit, data = data(), atmean=TRUE)
    
    model_probit_margins(meff)
  })
  
  output$Probit_Margins <- renderPrint({
    if(is.null(model_probit_margins()))
      return()
    print(model_probit_margins())
  })
  
  
  
  observeEvent(input$bCheckAssumption_PROBIT, { 
    
    if(is.null(model_probit())){
      msg = "No model has been estimated."
      showNotification(msg, duration = 5, type = 'error')
      return()
    }
    if(is.null(data())){
      msg = "No data has been loaded."
      showNotification(msg, duration = 5, type = 'error')
      return()
    }
    
    # # Joint insignificance of all variables test
    zero_formula = formula_model(input$selected_var_Probit[1], c(1))
    null_probit = glm(zero_formula, data=data(), family=binomial(link="probit")) # restricted model
    myprobit <- model_probit()
    lrt <-  lrtest(myprobit, null_probit) 
    
    # making you sure that our specification is correct
    # Linktest
    linktest_result = linktest(myprobit)
    linktest_sum <- summary(linktest_result)
    # the second power of yhat should be insignificant
    
    results <- list(lrt, linktest_sum)
    
    model_probit_assumption(results)
  })
  
  output$Linktest_PROBIT <- renderPrint({
    if(is.null(model_probit_assumption()))
      return()
    
    res <- model_probit_assumption()
    res[[2]]
  })
  
  output$JointInsig_PROBIT <- renderPrint({
    if(is.null(model_probit_assumption()))
      return()
    
    res <- model_probit_assumption()
    res[[1]]
  })
  
  
  #3. CREATING RAPORT  ----
  output$eksport <- downloadHandler(
    

    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {

      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(path, "report.Rmd")
      file.copy("report_copy.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(df = data(),
                     df_description = input$Report_data_DataDescription,
                     title = input$Report_title,
                     author = input$Report_author,
                     introduction = input$Report_introduction,
                     conclusion = input$Report_conclusion,
                     Plot1 = plot1_report(),
                     Plot1_desc = input$Report_PlotDesc1,
                     Plot2 = plot2_report(),
                     Plot2_desc = input$Report_PlotDesc2,
                     model1 = model1_report(),
                     model1_desc = input$Report_Model1,
                     model2 = model2_report(),
                     model2_desc = input$Report_Model2
                     )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}