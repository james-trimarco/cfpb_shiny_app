library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CFPB Consumer Complaints"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="col", label="Select column", 
                        choices=colnames(df)) #,
        ),

        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput('table') #,
           
            
        )
    )
)
