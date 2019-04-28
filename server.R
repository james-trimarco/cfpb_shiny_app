
shinyServer(function(input, output, session) {
    
    # Record the time that the session started.
    startTime <- as.numeric(Sys.time())
    
    # get data from API
    all_data <- reactive({
        # dynamically create API call
        select <- paste0("$select=*", '',"&")
        where <- paste0("product='Checking or savings account'")
        limit <- "$limit=100"
        
        # query below at least works
        url <- paste0(url_stem, "$select=*&product='Checking or savings account'&$limit=100")
        #print(url_query)
        
        # get data from socrata
        df <- read.socrata(
            url = url,
            app_token = app_token,
            email     = email,
            password  = password
        )
    })
    
    # get data from API
    bank_spec <- reactive({
        req(input$bank)
        # dynamically create API call
        select <- paste0("$select=*", '',"&")
        where <- paste0("product='Checking or savings account'", "AND ", "company='", input$bank, "'")
        limit <- "$limit=10000"
        
        # query below at least works
        url <- paste0(url_stem, "$select=*&", where, "&", "$limit=10000")
        #print(url_query)
        
        # get data from socrata
        df <- read.socrata(
            url = url,
            app_token = app_token,
            email     = email,
            password  = password
        )
    })
    
    output$company_response <- renderPlot({
        dat <- bank_spec() %>%
            group_by(company_response) %>%
            summarize(n = n()) %>%
            arrange(desc(n)) %>% head(8)
        
        p <- ggplot(dat, aes(x = company_response, y = n)) +
            scale_y_continuous(limits = c(0, 800)) +
            geom_bar(stat = "identity")
            
        
        return(p)
    })

    output$state_dist <- renderPlot({
        
        dat <- bank_spec() %>%
            group_by(state) %>%
            summarize(n = n()) %>%
            arrange(desc(n)) %>% head(8)
        
        #print(dat)

        p <- ggplot(dat, aes(x = reorder(state, -n), y = n)) +
            scale_y_continuous() +
            geom_bar(stat = "identity") +
            labs(tite = "state distribution")
        
        return(p)
    })
    
    output$sub_issue <- renderPlot({
        dat <- bank_spec() %>%
            group_by(sub_issue) %>%
            summarize(n = n()) %>%
            arrange(desc(n)) %>% head(8)
        
        p <- ggplot(dat, aes(x = reorder(sub_issue, -n), y = n)) +
            scale_y_continuous(limits = c(0, 800)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        
        return(p)
    }) 
    
    output$map1 <- renderPlot({
        
        
        
        # call the updateInputDataForMapByJobTitle1() to get the filtered data 
        # This function call to updateInputDataForMapByJobTitle1() enables to 
        # synchronously react user input and show the updated results.
        map_data <- bank_spec() %>%
            drop_na(state) %>%
            group_by(state) %>%
            join(state_match, by = )
            summarize(n = n())
        
        print(map_data)
        
        states_map <- map_data("state")
            
        ggplot(data = map_data, aes(map_id = state)) +
            geom_map(aes(fill = factor(n), map = states_map) +
            expand_limits(x = states_map$long, y = states_map$lat)
               
        # # Render the map using the filtered data
        # gvisGeoChart(map_data, locationvar= "state", colorvar = 'n',
        #              options=list(region="US", displayMode="regions",
        #                           resolution="provinces", 
        #                           width="100%", 
        #                           backgroundColor="gray"
        #              )
        # )
    })

    
    output$table <- renderDataTable({
        
        df <- all_data() 
        
        #print(head(df))
        
        return(df)
        })
    
})
