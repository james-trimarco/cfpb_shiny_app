library(ggplot2)
library(shiny)
#library(shinydashboard)
library(RSocrata)

url <- "https://data.consumerfinance.gov/resource/s6ew-h6mp.json?"

shinyServer(function(input, output, session) {
    
    data <- reactive({
        
        select <- paste0("$select=company,", input$col,"&")
        where <- "product=Checking or savings account"
        
        df <- read.socrata(
            paste0(url, select, "$limit=100"),
            app_token = "miXRQS8VKB8qiEsJLGSiTNewP",
            email     = "james.trimarco@gmail.com",
            password  = "acquaint@3Syrinx"
        )
        
    })
    
    
    output$table <- renderDataTable({
        df <- data()
        
        head(df)
        })
    
})
