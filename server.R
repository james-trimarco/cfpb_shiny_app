shinyServer(function(input, output, session) {
    
    # Record the time that the session started.
    start_time <- as.numeric(Sys.time())
    date_range_bro <- reactive({lubridate::as_datetime(input$date_range_bro)})
    # Get user-selected date range
    date_range_foc <- reactive({lubridate::as_datetime(input$date_range_foc)})
    
    # get data from API
    broad_data <- reactive({
        validate(
            need(is.Date(input$date_range_bro[1]), "Please specify a date range"), 
            need(is.Date(input$date_range_bro[2]), "Please specify a date range")
        )
        
        start_date <- date_range_bro()[1]
        end_date <- date_range_bro()[2]
        
        print(paste0("START DATE: ", start_date, "END DATE", end_date))
        product <- 'Checking or savings account'
        company <- "all"
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
    
    # get data from API
    derived_data <- eventReactive(broad_data(), {
        
        raw <- req(broad_data())
        
        thresh <- 10
        
        derived <- raw %>%
            mutate(tags = as.factor(tags), 
                   tags = fct_explicit_na(tags, na_level = "Regular")) %>%
            group_by(company, tags) %>%
            summarise(n = n()) %>%
            filter(n > thresh, tags != "Older American, Servicemember") %>%
            mutate(prop = n/sum(n)) %>%
            select(-n) %>%
            spread(tags, prop) %>%
            mutate(older = `Older American`, 
                   service = Servicemember) %>%
            drop_na()
        
    })

  #  Test table display
    # output$raw <- renderTable({
    #     derived <- req(derived_data())
    #     print(nrow(derived))
    #     print(colnames(derived))
    # 
    #     derived
    # 
    # })
    
    # Test table display
    output$dotplot <- renderPlotly({
        derived <- req(derived_data())

        p <- ggplot(derived, aes(x = older, y = service)) +
                scale_x_continuous(limits = c(0, NA)) +
                scale_y_continuous(limits = c(0, NA)) +
                geom_point() +
                labs(title = "How Banks Fare", 
                     x = "Proportion from Senior Citizens", 
                     y = "Proportion from Servicemembers")    
        
        mytext = paste("Company: ", derived$company)
        
        pp <- plotly_build(p)
        style(pp, text=mytext, hoverinfo = "text")
        
        
    })
    
    # get data from API
    selected_data <- reactive({
        validate(
            need(is.Date(input$date_range_foc[1]), "Please specify a date range"), 
            need(is.Date(input$date_range_foc[2]), "Please specify a date range"), 
            need(input$company, "Please specify a company")
        )
        
        start_date <- date_range_foc()[1]
        end_date <- date_range_foc()[2]
        
        print(paste0("START DATE: ", start_date, "END DATE", end_date))
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

    # time plot
    output$time_trend <- renderPlot({
        
        dat <- selected_data() %>%
            mutate(date_received = as_date(date_received), 
                   month = floor_date(date_received, unit = "month")) %>%
            group_by(month) %>%
            summarize(n = n())
        
        
        p <- ggplot(dat, aes(x = month, y = n)) +
            scale_x_date(date_labels =  "%b %y", labels = scales::wrap_format(3), 
                         date_breaks = "1 month", limits = c(as_date("2017-05-01"), 
                                                             as_date(date_range_foc()[2]))) +
            scale_y_continuous(limits = c(0, NA)) +
            geom_bar(stat = "identity") +
            labs(title = "How has the number of complaints per month changed over time?")
        
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
                geom_bar(stat = "identity") +
                labs(title = "Which problems are bank customers reporting?")
        
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
            labs(title = "Where are people complaining about this bank?", 
                 x = NULL, y = NULL)
              
    })
    
    output$table <- renderDataTable({
        
        df <- selected_data() %>%
            select(company, complaint_id, date_received, issue, product, sub_issue, tags, complaint_what_happened)
        
        return(df)
        })
    
    
})