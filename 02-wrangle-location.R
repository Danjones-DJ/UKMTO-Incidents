# Set parameters ----------------------------------------------------------
MAX_DAYS = 30

# Load Packages -----------------------------------------------------------
pacman::p_load(rvest, 
               tidyverse, 
               stringr, 
               netstat,
               lubridate,
               maps,
               rnaturalearth, 
               tidygeocoder, geosphere, dplyr
)

library(sf)

# Load data
ukmto_raw = read_rds("data/UKMTO_raw.rds")

# 01: Country and City Names ----------------------------------------------

# Cities
all_cities = world.cities %>%
  as_tibble() %>% distinct(name) %>% filter(nchar(name) >= 3) %>% pull(name)

city_names = str_c("\\b(", str_c(all_cities, collapse = "|"), ")\\b")

# Countries
all_countries = ne_countries(returnclass = "sf") %>% sf::st_drop_geometry() %>%
  select(country_full = admin,
         country_short = name,
         iso3 = iso_a3,
         iso2 = iso_a2) %>% pull(country_full)

country_names = str_c("\\b(", str_c(all_countries, collapse = "|"), ")\\b")


# 02: Extract Place and Country -------------------------------------------

ukmto_cc = ukmto_raw %>%
  
  mutate(
    # Extract country
    country_location = str_extract(
      location,
      regex(country_names, ignore_case = TRUE)
    ),
    
    country_details = str_extract(
      details,
      regex(country_names, ignore_case = TRUE)
    ),
    
    country = coalesce(
      country_location,
      country_details
    ),
    
    # Handle common abbreviations
    country = case_when(
      str_detect(
        location, regex("\\bUAE\\b", ignore_case = TRUE)) ~ "United Arab Emirates",
      TRUE ~ country
    ),
    
    # Extract specific place (after "_ NM direction of")
    place = str_match(
      location,
      regex(
        "\\d+\\s*NM\\s+(north|south|east|west|northeast|northwest|southeast|southwest|north east|north west|south east|south west)\\s+of\\s+([^,\\.\\n]+)", 
        ignore_case=TRUE))[,3],
    
    # Clean place names
    place = str_remove(place, regex("\\s+in\\s+.*$", ignore_case = TRUE)),
    place = str_remove(place, regex("\\s+transiting\\s+.*$", ignore_case = TRUE)),
    place = str_remove(place, regex(country_names, ignore_case = TRUE)),
    place = str_squish(place),
    
    # Extracting Nautical Miles
    nautical_miles = str_extract(
      details,
      "\\d+\\s*NM"
    ),
    # Extract directions
    direction = str_extract(
      details,
      "(north|south|east|west|northeast|northwest|southeast|southwest)"
    ),
    
    # Formatting
    place = str_to_title(place),
    country = str_to_title(country)
    ) %>%
  select(-c(country_location, country_details)) %>%
  drop_na(location)

# skimr::skim(ukmto_cc)

# 03: Fetch co-ordinates --------------------------------------------------

# Define bearings / dictionary
bearings = c(
  north = 0,   northeast = 45,  east = 90,   southeast = 135,
  south = 180, southwest = 225, west = 270,  northwest = 315
)

# Geocode unique place names
places <- ukmto_cc %>%
  distinct(place, country) %>% # Search by country city pairs
  mutate(
    # If no city, use country, if no country use city.
    address = case_when(
      !is.na(place) & !is.na(country) ~ str_c(place, ", ", country),
      !is.na(place) ~ place,
      TRUE ~ country
    ),
    address = str_squish(
      str_remove(address, "^\\s*,\\s*")
      )
  ) %>%
  geocode( # Extract Long/Lat using geocode function
    address = address,
    method = "osm",
    lat = base_lat,
    long = base_lon
  )

# Join co-ords alongside bearing
ukmto_geo = ukmto_cc %>%
  left_join(places) %>%
  mutate(bearings = bearings[direction]) %>%
  filter(!is.na(base_lat))

# Add offset
has_offset <- !is.na(ukmto_geo$bearings) & !is.na(ukmto_geo$nautical_miles)

offset = destPoint(
  p = cbind(ukmto_geo$base_lon, ukmto_geo$base_lat),
  b = ifelse(has_offset, ukmto_geo$bearings, 0),
  d = ifelse(has_offset, readr::parse_number(ukmto_geo$nautical_miles) * 1852, 0)
)

ukmto_geo <- ukmto_geo %>%
  mutate(
    longitude = offset[, "lon"],
    latitude = offset[, "lat"]
  )


# 04: Clean and save ------------------------------------------------------

UKMTO = ukmto_geo %>%
  select(
    date, details, incident_id, event_type, days_since, recency_weight, address, longitude, latitude
  ) 

saveRDS(UKMTO, "data/UKMTO.rds")

