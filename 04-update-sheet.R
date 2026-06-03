pacman::p_load(googlesheets4, tidyverse, lubridate)

writeLines(
  Sys.getenv("GSHEET_AUTH_JSON"),
  "service-account.json"
)

gs4_auth(path = "service-account.json")


# Packages

pacman::p_load(
  googlesheets4,
  tidyverse,
  lubridate,
  glue
)

# Sheet
SHEET_URL <- "https://docs.google.com/spreadsheets/d/1Junr-s2PV1ljxz3bT5zshkh59LYtF-2v-Om6OknHnng/edit"

# Load new and old data to work with
new_data = read_rds("data/UKMTO.rds") %>% mutate(incident_id = as.character(incident_id))
old_data = read_sheet(ss = SHEET_URL, sheet = "incidents")  %>% mutate(incident_id = as.character(incident_id))

# Update dataset with new data appending, update dates
updated_data = bind_rows(old_data, anti_join(new_data, old_data, by = "incident_id"))

final_data = updated_data %>%
  mutate(
    date = as.Date(date),
    days_since = as.numeric(Sys.Date() - date),
    recency_weight = 1 / (days_since + 1)
  ) %>%
  arrange(desc(date)) %>%
  filter(days_since <= 90)

# Write sheet
sheet_write(data = final_data, ss = SHEET_URL, sheet="incidents")

# Confirm output
message(glue("Sheet refreshed: New rows appended (+ {nrow(final_data) - nrow(old_data)}), old dates updated."))