library(shiny)
library(RSocrata)
#library(shinySignals)   # devtools::install_github("hadley/shinySignals")
#library(dplyr)
library(tidyverse)
library(shinydashboard)
#library(bubbles)        # devtools::install_github("jcheng5/bubbles")
library(lubridate)
library(maps)
library(here)
library(glue)
library(httr)

source('config.R')

options(shiny.reactlog = TRUE)

url_stem <- "https://data.consumerfinance.gov/resource/s6ew-h6mp.json?"

state_match <- read.csv(here::here("state_match.csv"), stringsAsFactors = FALSE) %>%
    mutate(state_name_lc = str_to_lower(state_name))

cfpb_cols <- list("company", "company_public_response", "company_response", "complaint_id",
                  "consumer_consent_provided","consumer_disputed", "date_received", "date_sent_to_company",     
                  "issue", "product", "state", "sub_issue", "submitted_via", "timely", "zip_code",
                  "sub_product", "complaint_what_happened", "tags")                     

large_banks <-  list("All Companies" = "all",
                     "JPMORGAN CHASE & CO." = "JPMORGAN CHASE %26 CO.",
                     "BANK OF AMERICA, NATIONAL ASSOCIATION", 
                     "WELLS FARGO %26 COMPANY", 
                     "CITIBANK, N.A.", 
                     "PNC Bank N.A.", 
                     "U.S. BANCORP", 
                     "CAPITAL ONE FINANCIAL CORPORATION", 
                     "SUNTRUST BANKS, INC.", 
                     "TD BANK US HOLDING COMPANY", 
                     "BBVA COMPASS FINANCIAL CORPORATION")

#-----
# get state population info
pops <- httr::GET("https://api.census.gov/data/2018/pep/population?get=DATE_CODE,POP,GEONAME&for=state:*")

state_pop <- do.call(rbind, content(pops))
state_pop <- data.frame(state_pop)[-1, ]
names(state_pop) <- c("date_code", "pop", "geoname", "state_no")

state_pop <- state_pop %>%
    mutate(state_name_lc = tolower(geoname)) %>%
    filter(date_code == 8) %>%
    left_join(state_match)

head(state_pop)

print("Hello there")
#print(glue("State population colnames:", colnames(state_pop)))

#-------------
# Function defs
build_url <- function(company, product, start_date, end_date, limit){
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


# 7 CAPITAL ONE FINANCIAL CORPORATION        33
# 8 TD BANK US HOLDING COMPANY               33
# 9 NAVY FEDERAL CREDIT UNION                30

### issue
# [1] "Managing an account"                                         
# [2] "Problem with a lender or other company charging your account"
# [3] "Closing an account"                                          
# [4] "Problem caused by your funds being low"                      
# [5] "Opening an account"                                          
# [6] "Incorrect information on your report"                        
# [7] "Credit monitoring or identity theft protection services"  
# 10 SUNTRUST BANKS, INC.                     26