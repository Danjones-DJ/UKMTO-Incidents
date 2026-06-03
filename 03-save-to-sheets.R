pacman::p_load(googlesheets4, googledrive, tidyverse)

df = read_rds("data/UKMTO.rds")


# 01: Save to new Sheet ---------------------------------------------------

gs4_auth()   # opens a browser — sign in with the Google account that should own sheet

ss <- gs4_create("ukmto-incident-tracker", sheets = list(incidents = df))
ss

sheet_write(
  ukmto_geo,
  ss    = "https://docs.google.com/spreadsheets/d/1Junr-s2PV1ljxz3bT5zshkh59LYtF-2v-Om6OknHnng/edit?gid=653274003#gid=653274003",
  sheet = "incidents"
)