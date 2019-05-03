ui <- dashboardPage(
    dashboardHeader(
        title = "CFPB Consumer Complaints"
    ),
    
    dashboardSidebar(
        dateRangeInput("date_range", "Date range:",
                       min = "2010-01-01",
                       start = '2019-01-01',
                       end   = "2019-02-01"),
        selectInput(inputId="company", label="Select column", 
                    choices = large_banks, selected = "BANK OF AMERICA, NATIONAL ASSOCIATION", 
                    multiple = FALSE)
    ),


        # Show a plot of the generated distribution
        dashboardBody(
            fluidRow(
                valueBoxOutput("total_count", width = 4),
                valueBoxOutput("", width = 4),
                valueBoxOutput("", width = 4)
            ),
            fluidRow(
                tabBox(id = "tab", width = 12,
                    tabPanel("General information", 
                             

                        plotOutput('company_response'), 
                        plotOutput('sub_issue')
                       
                        
                        ), 
                    tabPanel("Map",
                             plotOutput('map1')
                    ),
                    tabPanel("Data",
                             dataTableOutput('table')
                    )
        )
        )
    )
)
