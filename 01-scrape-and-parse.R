# Begin by using Selenium to extract key incident data

# Set parameters ----------------------------------------------------------
MAX_DAYS = 30

# Load Packages -----------------------------------------------------------
pacman::p_load(RSelenium, 
               rvest, 
               tidyverse, 
               stringr, 
               netstat,
               lubridate
               )

# As using Selenium, ensure before running that no drivers are running such that
# scraping begins on a free port.

if (exists("rD")) try(rD$server$stop(), silent = TRUE)
try(
  system("pkill -f selenium", ignore.stdout = TRUE, ignore.stderr = TRUE),
  silent = TRUE
)


# Set-up driver on Firefox

rD = rsDriver(
  browser    = "chrome",
  port       = netstat::free_port(),
  phantomver = NULL,
  chromever  = NULL
)
# Set up client/driver object.

remDr = rD$client


# 01: UKTMO Recent Incidents ----------------------------------------------

remDr$navigate("https://www.ukmto.org/recent-incidents")

Sys.sleep(4)  # let Cloudflare / JS load

html = remDr$getPageSource()[[1]]
page = read_html(html)

# Time for cookies to load
Sys.sleep(1.5)

cookie_button = remDr$findElement(
  using = "css selector",
  value = "#ccc-notify-accept"
)

# Accept cookies
cookie_button$clickElement()

# Wait to finish clicking / loading
Sys.sleep(1)


# 02: Extract incident information ----------------------------------------

html = remDr$getPageSource()[[1]]
page = read_html(html)

# Create incident object

incident_df = page %>%
  html_elements(".IncidentList_incident__CfgOM") %>%
  map_dfr(~ {
    tibble(
      title = .x %>%
        html_element(".IncidentList_title__uflPM button") %>%
        html_text2(),
      
      date = .x %>%
        html_element(".IncidentList_meta__IBGJr span") %>%
        html_text2(),
      
      details = .x %>%
        html_element(".IncidentList_details__PlEuF") %>%
        html_text2()
    )
  }) %>%
  filter(!is.na(title)) %>%
  mutate(
    location = str_extract(
      details,
      "\\d+\\s*NM\\s+[A-Za-z\\-]+\\s+of\\s+[^\\.\\n]+"
    )
  )

head(incident_df)


# 03: Parse data ----------------------------------------------------------

UKMTO_raw = incident_df %>%
  mutate(
    # Get ID
    incident_id = str_extract(title, "#\\d+|#\\d{4}") %>%
      str_remove("#"),
    
    # Event type
    event_type = case_when(
      str_detect(title, regex("Attack", ignore_case = TRUE)) ~ "Attack",
      str_detect(title, regex("SUSPICIOUS ACTIVITY", ignore_case = TRUE)) ~ "Suspicious Activitiy",
      str_detect(title, regex("Hijack", ignore_case = TRUE)) ~ "Hijacking",
      str_detect(title, regex("Advisory", ignore_case = TRUE)) ~ "Advisory",
      TRUE ~ NA_character_
    ),
    
    # Ensure correct date format
    date = dmy(date),
    
    # Create gravity/recency variable
    days_since = as.numeric(Sys.Date() - date), # How far away, since today, did incident occur?
    recency_weight = 1 / (days_since + 1) # More recent incidents higher weight
  ) 


# 04: Save for next stage -------------------------------------------------
remDr$close() # Close Selenium
saveRDS(UKMTO_raw, "data/UKMTO_raw.rds")


    