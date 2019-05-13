library(shiny)
library(RSocrata)
library(tidyverse)

library(lubridate)
library(maps)
library(here)
library(glue)
library(httr)
library(plotly)
library(wesanderson)

source('config.R')

options(shiny.reactlog = TRUE)

#----------
# Handle fonts & design
library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("spectral", "spectral")
font_add_google("roboto slab", "roboto")

showtext_auto(enable = TRUE)

theme_set(theme_minimal() + theme(text=element_text(size=14,family="roboto"), 
                                  plot.title = element_text(size=22, vjust=2),
                                  plot.margin = unit(c(1,1,b = 1,1), "cm"),
                                  axis.title.y= element_text(vjust = 2), 
                                  axis.title.x = element_text(margin = margin(t = 0, r = 0, 
                                                                              b = 0, l = 0, unit = "cm"), vjust = -2) ))
#----------
# Handle API requests

url_stem <- "https://data.consumerfinance.gov/resource/s6ew-h6mp.json?"

state_match <- read.csv(here::here("state_match.csv"), stringsAsFactors = FALSE) %>%
    mutate(state_name_lc = str_to_lower(state_name))

cfpb_cols <- list("company", "company_public_response", "company_response", "complaint_id",
                  "consumer_consent_provided","consumer_disputed", "date_received", "date_sent_to_company",     
                  "issue", "product", "state", "sub_issue", "submitted_via", "timely", "zip_code",
                  "sub_product", "complaint_what_happened", "tags")                     

large_banks <-  list("All Companies" = "all",
                     "JPMorgan Chase" = "JPMORGAN CHASE %26 CO.",
                     "Bank of America" = "BANK OF AMERICA, NATIONAL ASSOCIATION", 
                     "Wells Fargo" = "WELLS FARGO %26 COMPANY", 
                     "Citibank" = "CITIBANK, N.A.", 
                     "PNC Bank" = "PNC Bank N.A.", 
                     "U.S. Bancorp" = "U.S. BANCORP", 
                     "Capital One" = "CAPITAL ONE FINANCIAL CORPORATION", 
                     "SunTrust Bank" = "SUNTRUST BANKS, INC.", 
                     "TD Bank" = "TD BANK US HOLDING COMPANY", 
                     "BBVA Compass" = "BBVA COMPASS FINANCIAL CORPORATION")

#--------
# Get state population info
pops <- httr::GET("https://api.census.gov/data/2018/pep/population?get=DATE_CODE,POP,GEONAME&for=state:*")

state_pop <- do.call(rbind, content(pops))
state_pop <- data.frame(state_pop)[-1, ]
names(state_pop) <- c("date_code", "pop", "geoname", "state_no")

state_pop <- state_pop %>%
    mutate(state_name_lc = tolower(geoname)) %>%
    filter(date_code == 8) %>%
    left_join(state_match)

head(state_pop)

#-------------
# Function defs
build_url <- function(company="all", product, start_date, end_date, limit){
    select_p <- glue("$select=*", '',"&")
    product_p <- glue("product='{product}'", " AND ")
    company_p <- glue("company='{company}'", " AND ")
    date_p <- glue( "date_received > '{start_date}' AND date_received < '{end_date}'&")

    limit_p <- "$limit={limit}"
    
    if (company == "all") {
        where_p <- glue(product_p, date_p)
        url <- glue(url_stem, select_p, limit_p)
    }
    else {
        where_p <- glue(product_p, company_p, date_p)
        url <- glue(url_stem, select_p, limit_p)
    }
    
    url <- glue(url_stem, select_p, where_p, limit_p)
    return(url)
}

### issue
# [1] "Managing an account"                                         
# [2] "Problem with a lender or other company charging your account"
# [3] "Closing an account"                                          
# [4] "Problem caused by your funds being low"                      
# [5] "Opening an account"                                          
# [6] "Incorrect information on your report"                        
# [7] "Credit monitoring or identity theft protection services"  
# 10 SUNTRUST BANKS, INC.                     26