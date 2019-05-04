shinyServer(function(input, output, session) {
    
    # Record the time that the session started.
    start_time <- as.numeric(Sys.time())
    
    # get data from API
    selected_data <- reactive({
        validate(
            need(is.Date(input$date_range[1]), "Please specify a date range"), 
            need(is.Date(input$date_range[2]), "Please specify a date range"), 
            need(input$company, "Please specify a company")
        )
        
        start_date <- lubridate::as_datetime(input$date_range[1])
        end_date <- lubridate::as_datetime(input$date_range[2])
        company <- input$company
        product <- 'Checking or savings account'
        limit <- '100000'
        
        #print(glue("Printing start date: {start_date}"))
        # build url for API call
        url <- build_url(company, product, start_date, end_date, limit)

        print(url)

        # fetch data from socrata
        df <- read.socrata(
            url = url,
            app_token = app_token
        )
    })
    
    # Get the total count of complaints for current 
    # selection. 
    output$total_count <- renderValueBox({
        print(glue("Counting up rows:", nrow(selected_data())))
        
        selected_data() %>%
            nrow() %>%
            format(big.mark = ",") %>%
            valueBox("Total complaints")
    })
    
    # 
    output$prop_paid <- renderValueBox({
        
        selected_data() %>%
            group_by(company_response) %>%
            summarize(n = n()) %>%
            mutate(prop = n*100/sum(n)) %>%
            filter(company_response == "Closed with monetary relief") %>%
            pull(prop) %>%
            format(digits = 3) %>%
            valueBox("Percent getting $ relief") 
    })
    
    output$tags <- renderValueBox({
        
    selected_data() %>%
            group_by(tags) %>%
            summarize(n = n()) %>%
            mutate(prop = n*100/sum(n)) %>%
            filter(tags %in% c("Older American, Servicemember", 
                               "Servicemember")) %>%
            pull(prop) %>%           
            sum() %>%
            format(digits = 3) %>%
            valueBox("Percent servicemembers") 
                
        
    })
    
    
    # company response
    output$company_response <- renderPlot({
        dat <- selected_data() %>%
            group_by(company_response) %>%
            summarize(n = n()) %>%
            arrange(desc(n)) %>% head(8)
        
        p <- ggplot(dat, aes(x = company_response, y = n)) +
            geom_bar(stat = "identity")
            
        
        return(p)
    })

    
    # sub-issue
    output$sub_issue <- renderPlot({

        dat <- selected_data() %>%
            group_by(sub_issue) %>%
            summarize(n = n()) %>%
            arrange(desc(n)) %>% head(5) %>%
            mutate(prop = n/sum(n)) 

        p <- ggplot(dat, aes(x = reorder(sub_issue, -prop), y = prop)) +
            scale_y_continuous(limits = c(0, 1)) +
            geom_bar(stat = "identity") 
        
        
        return(p)
    }) 
    
    output$map1 <- renderPlot({
        req(state_pop)
        
        map_data <- selected_data() %>%
            group_by(state) %>%
            summarize(n = n()) %>%
            drop_na(state) %>%
            right_join(state_pop) %>%
            drop_na(fips) %>%
            mutate(pop = as.numeric(unlist(pop)), 
                   per_cap = (n*100000/pop)) 
            
        print(head(map_data))
        
        states_map <- map_data("state")
            
        ggplot(data = map_data, aes(map_id = state_name_lc)) +
            geom_map(aes(fill = per_cap), map = states_map) +
            expand_limits(x = states_map$long, y = states_map$lat) +
            labs(title = "Complaints Per 100,000 State Residents")
              
    })
    
    output$table <- renderDataTable({
        
        df <- selected_data() %>%
            select(company, complaint_id, date_received)
        
        return(df)
        })
})