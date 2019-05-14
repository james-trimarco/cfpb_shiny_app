shinyServer(function(input, output, session) {

  # Get user-selected date ranges  
    date_range_comp <- reactive({lubridate::as_datetime(input$date_range_comp)})
    date_range_foc <- reactive({lubridate::as_datetime(input$date_range_foc)})
    
#-----
# First tab
    
    # get data from API
    comp_data <- eventReactive(input$comp_go, {
      validate(
        need(is.Date(input$date_range_comp[1]), "Please specify a date range"),
        need(is.Date(input$date_range_comp[2]), "Please specify a date range")
      )
      
      start_date <- date_range_comp()[1]
      end_date <- date_range_comp()[2]
      
      print(paste0("START DATE: ", start_date, "END DATE", end_date))
      product <- 'Checking or savings account'
      company <- "all"
      limit <- '100000'
      
      # build url for API call
      url <- build_url(company, product, start_date, end_date, limit)
      
      print(url)
      
      # fetch data from socrata
      df <- read.socrata(
        url = url,
        app_token = app_token
      )
    },  ignoreNULL = FALSE)
      
    
    # 
    output$older_plot <- renderPlotly({
      
      raw <- req(comp_data())
      thresh <- req(input$thresh)
      
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

        p <- ggplot(derived, aes(x = older, y = service)) +
                scale_x_continuous(limits = c(0, NA)) +
                scale_y_continuous(limits = c(0, NA)) +
                geom_point() +
                labs(title = "Who Complaints About the Banks?", 
                     x = "Proportion from Senior Citizens", 
                     y = "Proportion from Servicemembers")    
        
        mytext = paste("Company: ", derived$company)
        
        pp <- plotly_build(p)
        style(pp, text=mytext, hoverinfo = "text")
    })
    
    output$resolved_plot <- renderPlotly({
      
      raw <- req(comp_data())
      thresh <- req(input$thresh)
      
      derived <- raw %>%
        select(company, company_response, product, issue, sub_issue) %>%
        filter(issue == "Managing an account") %>%
        mutate(sub_issue = as.factor(sub_issue), 
               sub_issue = fct_collapse(sub_issue, 
                                        Fees = "Fee problem", 
                                        Withdrawals = "Deposits and withdrawals", 
                                        group_other = TRUE)) %>%
        group_by(company, sub_issue) %>%
        summarise(n = n()) %>%
        filter(n > thresh) %>%
        mutate(prop = n/sum(n)) %>%
        select(-n) %>%
        spread(sub_issue, prop) %>%
        replace_na(list(company = 0, Fees = 0, Withdrawals = 0, Other = 0))
        
  
      p <- ggplot(derived, aes(x = Fees, y = Withdrawals)) +
        scale_x_continuous(limits = c(0, NA)) +
        scale_y_continuous(limits = c(0, NA)) +
        geom_point() +
        labs(title = "How many complaints are \nabout fees or withdrawals?",
             x = "% About Fees",
             y = "% About Withdrawals")

      mytext = paste("Company: ", derived$company)

      pp <- plotly_build(p)
      style(pp, text=mytext, hoverinfo = "text")
    })
    
    # get data from API
    # der_monet <- eventReactive(input$comp_go, {
    #   
    #   raw <- req(broad_data())
    #   thresh <- req(input$thresh)
    #   
    #   derived <- raw %>%
    #     select(consumer_disputed, company_response) %>%
    #     head(20)
    # 
    #   print(derived)     
    #   
    # },  ignoreNULL = FALSE)
    
#----------
# SECOND TAB
    
    # get data from API
    selected_data <- eventReactive(input$foc_go, {
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
    },  ignoreNULL = FALSE)

    # time plot
    output$time_trend <- renderPlot({
      
        raw <- selected_data()
        
        dat <- raw %>%
            mutate(date_received = as_date(date_received), 
                   month = floor_date(date_received, unit = "month")) %>%
            group_by(month) %>%
            summarize(n = n())
        
        
        p <- ggplot(dat, aes(x = month, y = n)) +
            scale_x_date(date_labels =  "%b", labels = scales::wrap_format(3), 
                         date_breaks = "1 month") +
            
            scale_y_continuous(limits = c(0, NA)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            labs(title = "How does the number of complaints per month \nchange over time?")
        
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
                scale_x_discrete(labels = scales::wrap_format(5)) +
                geom_bar(stat = "identity", color = "steelblue") +
                labs(title = "Which problems are customers reporting?")
        
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
            
        #print(tail(map_data, 30))
        
        map_data$per_cap[is.na(map_data$per_cap)] <- 0
        
        states_map <- map_data("state")
            
        ggplot(data = map_data, aes(map_id = state_name_lc)) +
            geom_map(aes(fill = per_cap), map = states_map) +
            expand_limits(x = states_map$long, y = states_map$lat) +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank()) +
            theme(axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank()) +
            labs(title = "Where are people complaining about this bank?", 
                 x = NULL, y = NULL, 
                 fill = "Complaints \nper capita")
              
    })
#-----
#TAB THREE
    
    yest_data <- reactive({

      yest_date <- Sys.Date() - 1
      product <- 'Checking or savings account'
      company <- "all"
      limit <- '100000'
      
      # build url for API call
      url <- glue("https://data.consumerfinance.gov/resource/s6ew-h6mp.json?$select=*&product='Checking or savings account' AND date_received = '{yest_date}'&$limit=100000")
      
      print(url)
      
      # fetch data from socrata
      df <- read.socrata(
        url = url,
        app_token = app_token
      )
    })
    
    output$yesterday <- renderDataTable({
      yest <- req(yest_data())
      
      print(head(yest))

        return(as.data.frame(yest))
        })
    
    
})