
body <-   dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "InfoTab",
            br(),
            box(class="par", align="right",
                width=14,
                p("About the application", class="ch2"),
                h2("A Shiny app to perform automated OLS and PROBIT/LOGIT Regressions in R."),
                br(),
                h4("Users need to provide a data frame with variables and able to analyze them in several tabs"),  
                br(),
                h4("Possibility to dynamically add graphs/charts and see dependencies for chosen variables"),
                br(),
                h4("The ability to generate and export a summarized performed analysis report in R markdown."),
            )
            
    ),
    
    # Second tab content
    tabItem(tabName = "DataTab",
            fluidRow(
              box(
                title = "Load the data",
                solidHeader = TRUE,
                width = 6,
                collapsible = FALSE,
                fileInput("file", label = "", accept= c('.csv')),
                checkboxInput("header", "Header", TRUE),
                selectInput('sep', 'Separator', choices = sep),
                selectInput('dec', 'Decimal points', dec),
                actionButton('loadData', 'Load'),
                uiOutput("VarDeleteMissing"),
                uiOutput("bDeleteMissing")
                
              ),
              box(
                title = "Create a subset",
                solidHeader = TRUE,
                width = 6,
                collapsible = FALSE,
                uiOutput("VarChangeName"),
                uiOutput("VarNewName"),
                uiOutput("bChangeVarName"),
                uiOutput("Variable"),
                uiOutput("ValuesToDrop"),
                uiOutput("bUpdateData"),

              )),
            fluidRow(
              box(
                width = 12,           
                solidHeader = TRUE,
                collapsible = TRUE,
                dataTableOutput('DisplayFile'),
                textAreaInput('Report_data_DataDescription', '', value = 'Write a comment here to include it in the report')
              )
            ),
            fluidRow(
              tabBox(
                title = "Histogram",
                width = 6,
                
                id = "tabset1", height = "250px",
                tabPanel("Tab1",
                         uiOutput('filter1Variable'),
                         uiOutput("hist1Variable"),
                         uiOutput("hist1PlotBins"),
                         plotOutput("hist1Plot")
                         
                ),
                tabPanel("Tab2", 
                         verbatimTextOutput('StatsVar1'),
                         textAreaInput('Report_PlotDesc1', '', value = 'Write a comment here to include it in the report')
                )
              ),
              tabBox(
                title = "Scatter Plot",
                width = 6,
                
                id = "tabset2", height = "250px",
                
                
                tabPanel("Tab1", 
                         uiOutput('ScaterPlotDim1'),
                         uiOutput("ScaterPlotDim2"),
                         uiOutput("ScaterPlotDim3"),
                         uiOutput("MakeScatterPlot"),
                         plotOutput("ScaterPlot4Dim")
                ),
                tabPanel("Tab2",
                         textAreaInput('Report_PlotDesc2', '', value = 'Write a comment here to include it in the report'))
              )
            )
    ),
    
    # Third tab content
    tabItem(tabName = "ModelTab",
            
            tabsetPanel(type = "tabs",
                        tabPanel("OLS",
                                 
                                 fluidRow(
                                   column(
                                     
                                     width = 12,
                                     uiOutput('Dynamic_Bucket_OLS'),
                                     verbatimTextOutput('OLS_Formula'),
                                     actionButton('OLS_Estimate', label = 'Estimate', icon = icon('fa-solid fa-play'))
                                     
                                   )),
                                 
                                 fluidRow(
                                   column(
                                     width = 12,
                                     verbatimTextOutput('OLS_Summary'),
                                     textAreaInput('Report_Model1', '', value = 'Write a comment here to include it in the report')
                                   )),
                                 fluidRow(
                                   column(
                                     width = 12,
                                     actionButton('bCheckAssumption_OLS', label = 'Check Assumptions'),

                                     verbatimTextOutput('Reset_test_OLS'),
                                     
                                     verbatimTextOutput('BP_test_OLS'),
                                     
                                     verbatimTextOutput('JB_test_OLS'),
                                     
                                     plotOutput("JB_plot_OLS")
                                   )
                                 )
                        ),
                        
                        tabPanel("PROBIT/LOGIT", 
                                 
                                 fluidRow(
                                   column(

                                     width = 12,
                                     uiOutput('Dynamic_Bucket_Probit'),
                                     verbatimTextOutput('Probit_Formula'),
                                     actionButton('Probit_Estimate', label = 'Estimate', icon = icon('fa-solid fa-play'))
                                     
                                   )),
                                 
                                 fluidRow(
                                   column(
                                     width = 12,
                                     verbatimTextOutput('Probit_Summary'),
                                     textAreaInput('Report_Model2', '', value = 'Write a comment here to include it in the report')
                                   )),
                                 fluidRow(
                                   column(
                                     width = 12,
                                     actionButton('CalculateMargins', label = 'Calculate marginal effect'),
                                     h5("Marginal effect for the observation with the average values of each variables in the model's formula"),
                                     verbatimTextOutput('Probit_Margins'),
                                     actionButton('bCheckAssumption_PROBIT', label = 'Check Assumptions'),
                                     h5('Test of joint insignificance of all variables in the model'),
                                     h6('The second power of yhat should be insignificant to say that the specification of the model is fine'),
                                     verbatimTextOutput('JointInsig_PROBIT'),
                                     h6('Linktest - to check if a specification of the model is correct'),
                                     verbatimTextOutput('Linktest_PROBIT')
                                   )
                                 )
                              )
            )

    ),
    
    # Fourth tab contentperformed
    tabItem(tabName = "RaportTab",
            box(class="par", align="center",
                width=14,
                p("Report", class="ch2"),
                p('Click button to generate report in R markdown', class="par4"),
                div(
                  downloadButton("eksport", "Generate report", style='jelly', color = "royal",
                                 size = "md",), align="center"
                )
            ),
            box(class="par", align="center",
                width=14,
                textInput('Report_author', "Name of the author of the report"),
                textInput('Report_title', "Report title"),
                textAreaInput('Report_introduction', 'Write an introduction to the report'),
                textAreaInput('Report_conclusion', 'Write a conclusion to the report')
                
                )
    )
  ) 
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Info", tabName = "InfoTab", icon = icon("fa-solid fa-info")), # First tab
    menuItem("Load and Explore your Data", tabName = "DataTab", icon = icon("fa-regular fa-database")), # Second tab
    menuItem("Make a model", tabName = "ModelTab", icon = icon("fa-solid fa-chart-line")), # Third tab
    menuItem("Create a report", tabName = "RaportTab", icon = icon("fa-solid fa-scroll", lib="font-awesome")) # Fourth tab
  )
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Model your data"),
  sidebar,
  body
)


