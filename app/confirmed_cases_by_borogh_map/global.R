if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("tibble")) {
  install.packages("tibble")
  library(tibble)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}

if (!require("sf")) {
  install.packages("sf")
  library(sf)
}
if (!require("RCurl")) {
  install.packages("RCurl")
  library(RCurl)
}
if (!require("tmap")) {
  install.packages("tmap")
  library(tmap)
}
if (!require("rgdal")) {
  install.packages("rgdal")
  library(rgdal)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("viridis")) {
  install.packages("viridis")
  library(viridis)
}


if (!require("zipcodeR")) {
  install.packages("zipcodeR")
  library(zipcodeR)
}

library(httr)
library(rvest)
library(stringr)
library(jsonlite)


# Data Sources
# nyc Health data
#
# get NYC covid data based on Modified Zip code
# First get ZCTA (zip code) to MODZCTA data:

zcta_to_modzcta <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/Geography-resources/ZCTA-to-MODZCTA.csv" )

# NYC Covid data by MODZCTA(cummulative):

data_by_modzcta <- read.csv('https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/data-by-modzcta.csv' )

nyc_covid_data <- data_by_modzcta %>%
  select(
    "MODIFIED_ZCTA","NEIGHBORHOOD_NAME", 
    "BOROUGH_GROUP", "COVID_CASE_COUNT", 
    "COVID_CASE_RATE", "PERCENT_POSITIVE", "COVID_DEATH_COUNT",
    "TOTAL_COVID_TESTS"
  )

# import geojson file from NYC open data
nyc_zipcode_geo <- st_read("ZIP_CODE_040114/ZIP_CODE_040114.shp") %>%
  st_transform('+proj=longlat +datum=WGS84')
nyc_zipcode_geo$ZIPCODE <- type.convert(nyc_zipcode_geo$ZIPCODE, as.is = TRUE)


# import longitude and latitude data
uszips <- read.csv("uszips.csv")
nyczips <- uszips[uszips$state_name == "New York",]

nyc_lat_table <- nyczips %>%
  select(
    "zip", "lat", "lng")

# match zipcode with longitude and latitude data and merge new data
nyc_covid_data <- nyc_covid_data %>%
  mutate(
    LAT_repre = nyczips$lat[
      match(nyc_covid_data$MODIFIED_ZCTA, nyczips$zip) ]
  ) %>%
  mutate(
    LNG_repre = nyczips$lng[ 
      match(nyc_covid_data$MODIFIED_ZCTA, nyczips$zip) ]
  )

## NYC Shooting Data
nyc_shooting_data <- read.csv("NYPD_Shooting_Incident_Data__Year_To_Date_.csv")





