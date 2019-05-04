ui <- dashboardPage(
    dashboardHeader(
        title = "CFPB Consumer Complaints"
    ),
    
    dashboardSidebar(
        checkboxInput("smooth", "Smooth"),
        conditionalPanel(
            condition = "input.smooth == true",
            selectInput("smoothMethod", "Method",
                        list("lm", "glm", "gam", "loess", "rlm"))
        ),
        dateRangeInput("date_range", "Date range:",
                       min = "2010-01-01",
                       start = Sys.Date() - 365,
                       end   = Sys.Date() - 1),
        selectInput(inputId="company", label="Select column", 
                    choices = large_banks, selected = "BANK OF AMERICA, NATIONAL ASSOCIATION", 
                    multiple = FALSE)
    ),


        # Show a plot of the generated distribution
        dashboardBody(
            fluidRow(
                valueBoxOutput("total_count", width = 4),
                valueBoxOutput("prop_paid", width = 4),
                valueBoxOutput("tags", width = 4)
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
