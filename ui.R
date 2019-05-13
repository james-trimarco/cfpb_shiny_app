ui <- navbarPage("CFPB Bank Tracker",
                 
    tabPanel("Compare banks",
             sidebarLayout(
                 sidebarPanel(
                     dateRangeInput("date_range_bro", "Date range:",
                                    min = "2010-01-01",
                                    start = "2018-01-01",
                                    end   = Sys.Date() - 1)
                 ),
                 mainPanel(
                     div(
                         style = "position:relative",
                     #tableOutput("raw")
                     plotlyOutput("dotplot")
                     )
                     
                 )
             )), 
    
    tabPanel("Focus on one bank",
             sidebarLayout(
                 sidebarPanel(  ### sidebar
                     
                     dateRangeInput("date_range_foc", "Date range:",
                                    min = "2010-01-01",
                                    start = "2018-01-01",
                                    end   = Sys.Date() - 1),
                     
                     selectInput(inputId="company", label="Select a bank", 
                                 choices = large_banks, selected = "BANK OF AMERICA, NATIONAL ASSOCIATION", 
                                 multiple = FALSE)
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotOutput('map1'),
                     plotOutput('time_trend'), 
                     plotOutput('sub_issue') 
                     
                 )
             )
             
             ),
         

             
    tabPanel("What happened yesterday?")
            
)

