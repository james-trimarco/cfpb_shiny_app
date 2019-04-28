ui <- fluidPage(

    # Application title
    titlePanel("CFPB Consumer Complaints"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="bank", label="Select column", 
                        choices = large_banks, selected = "BANK OF AMERICA, NATIONAL ASSOCIATION", 
                        multiple = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(

            plotOutput('company_response'), 
            plotOutput('sub_issue'), 
            plotOutput('map1'),
            plotOutput('state_dist'), 
            dataTableOutput('table')
            
           
        )
    )
)
