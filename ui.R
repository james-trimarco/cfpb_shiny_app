ui <- navbarPage("CFPB Bank Tracker",
                 
    tabPanel("Compare banks",
             sidebarLayout(
                 sidebarPanel(
                     dateRangeInput("date_range_comp", "Date range:",
                                    min = "2010-01-01",
                                    start = "2019-01-01",
                                    end   = Sys.Date() - 1),
                     
                     numericInput(inputId = "thresh", label = "Select a minimum count for inclusion.", 
                                  value = 10, min = 1, max = 100), 
                     actionButton("comp_go", "Go!")
                     
                 ),
                 mainPanel(
                     div(
                         style = "position:relative",
                     #tableOutput("raw")
                     plotlyOutput("older_plot")
                     ), 
                     div(
                         style = "position:relative",
                         #tableOutput("raw")
                         plotlyOutput("resolved_plot")
                     )
                 )
             )), 
    
    tabPanel("Focus on one bank",
             sidebarLayout(
                 sidebarPanel(  ### sidebar
                     
                     dateRangeInput("date_range_foc", "Date range:",
                                    min = "2010-01-01",
                                    start = "2019-01-01",
                                    end   = Sys.Date() - 1),
                     
                     selectInput(inputId="company", label="Select a bank", 
                                 choices = large_banks, selected = "BANK OF AMERICA, NATIONAL ASSOCIATION", 
                                 multiple = FALSE), 
                     actionButton("foc_go", "Go!")
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotOutput('map1'),
                     plotOutput('time_trend'), 
                     plotOutput('sub_issue') 
                     
                 )
             )
             
             ),
         

             
    tabPanel("What happened yesterday?", 
             dataTableOutput('yesterday'))
            
)

