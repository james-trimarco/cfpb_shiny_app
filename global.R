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

#shinyloadtest generates fake traffic to test how where it lags
#profvis

source('config.R')

options(shiny.reactlog = TRUE)

url_stem <- "https://data.consumerfinance.gov/resource/s6ew-h6mp.json?"

state_match <- read.csv(here("state_match.csv"), stringsAsFactors = FALSE) %>%
    mutate(state_name_lc = str_to_lower(state_name))

cfpb_cols <- list("company", "company_public_response", "company_response", "complaint_id",
                  "consumer_consent_provided","consumer_disputed", "date_received", "date_sent_to_company",     
                  "issue", "product", "state", "sub_issue", "submitted_via", "timely", "zip_code",
                  "sub_product", "complaint_what_happened", "tags")                     

large_banks <-  list("JPMORGAN CHASE & CO." = "JPMORGAN CHASE %26 CO.",
                     "BANK OF AMERICA, NATIONAL ASSOCIATION", 
                     "WELLS FARGO %26 COMPANY", 
                     "CITIBANK, N.A.", 
                     "PNC Bank N.A.", 
                     "U.S. BANCORP", 
                     "CAPITAL ONE FINANCIAL CORPORATION", 
                     "SUNTRUST BANKS, INC.")
    
#-------------
# Function defs
build_url <- function(company, start_date, end_date, limit){
    select_p <- glue("$select=*", '',"&")
    where_p <- glue("product='Checking or savings account'", " AND ", 
                    "company=", "'{company}'", " AND ",
                    "date_received > '{start_date}' AND date_received < '{end_date}'&")

    limit_p <- "$limit={limit}"
    
    url <- glue(url_stem, select_p, where_p, limit_p)
    return(url)
}


# 7 CAPITAL ONE FINANCIAL CORPORATION        33
# 8 TD BANK US HOLDING COMPANY               33
# 9 NAVY FEDERAL CREDIT UNION                30
# 10 SUNTRUST BANKS, INC.                     26