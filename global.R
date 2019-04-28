library(shiny)
library(RSocrata)
#library(shinySignals)   # devtools::install_github("hadley/shinySignals")
#library(dplyr)
library(tidyverse)
library(shinydashboard)
#library(bubbles)        # devtools::install_github("jcheng5/bubbles")
source('config.R')
library(lubridate)
library(maps)
library(here)



options(shiny.reactlog = TRUE)

url_stem <- "https://data.consumerfinance.gov/resource/s6ew-h6mp.json?"
state_match <- read.csv("./state_match.csv")

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
    


# 
# 7 CAPITAL ONE FINANCIAL CORPORATION        33
# 8 TD BANK US HOLDING COMPANY               33
# 9 NAVY FEDERAL CREDIT UNION                30
# 10 SUNTRUST BANKS, INC.                     26