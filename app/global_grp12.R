#--------------------------------------------------------------------
#------------------------Install Packages----------------------------

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
if (!require("readr")) {
  install.packages("readr")
  library(readr)
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
if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}
if (!require("viridis")) {
  install.packages("viridis")
  library(viridis)
}
if (!require("tigris")) {
  install.packages("tigris")
  library(tigris)
}
if (!require("geosphere")) {
  install.packages("geosphere")
  library(geosphere)
}
if (!require("dbplyr")) {
  install.packages("dbplyr")
  library(dbplyr)
}
if (!require("dbplot")) {
  install.packages("dbplot")
  library(dbplot)
}
if (!require("DBI")) {
  install.packages("DBI")
  library(DBI)
}
if (!require("htmltools")) {
  install.packages("htmltools")
  library(htmltools)
}
if (!require("htmlwidgets")) {
  install.packages("htmlwidgets")
  library(htmlwidgets)
}
if (!require("httr")) {
  install.packages("httr")
  library(httr)
}
if (!require("rvest")) {
  install.packages("rvest")
  library(rvest)
}
if (!require("stringr")) {
  install.packages("stringr")
  library(stringr)
}
if (!require("jsonlite")) {
  install.packages("jsonlite")
  library(jsonlite)
}


#--------------------------------------------------------------------
#------------------------Dataset Processing--------------------------
# NYC Covid Map

# Get NYC covid data based on Modified Zip code
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

# Import geojson file from NYC open data
nyc_zipcode_geo <- st_read("../data/ZIP_CODE_040114/ZIP_CODE_040114.shp") %>%
  st_transform('+proj=longlat +datum=WGS84')
nyc_zipcode_geo$ZIPCODE <- type.convert(nyc_zipcode_geo$ZIPCODE, as.is = TRUE)

# Import longitude and latitude data
uszips <- read.csv("../data/uszips.csv")
nyczips <- uszips[uszips$state_name == "New York",]

nyc_lat_table <- nyczips %>%
  select(
    "zip", "lat", "lng")

# Match zipcode with longitude and latitude data and merge new data
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
nyc_shooting_data <- read.csv("../data/NYPD_Shooting_Incident_Data__Year_To_Date_.csv")

## Hate Crime Data Manipulate
#Import Data
hate_data <- read.csv("../data/NYPD_Hate_Crimes.csv")
covid_data <- read.csv("../data/cases-by-day.csv")

#Add date before 2020/02 for Covid data 
tmp <- data.frame("CASE_COUNT"=rep(0,13),"BX_CASE_COUNT"=rep(0,13),"BK_CASE_COUNT"=rep(0,13),"MN_CASE_COUNT"=rep(0,13),"QN_CASE_COUNT"=rep(0,13),"SI_CASE_COUNT"=rep(0,13),"MM"=rep(0,13),"YY"=c(rep("2019",12),"2020"),"YY_MM"=c("2019/1", "2019/2", "2019/3", "2019/4", "2019/5", "2019/6", "2019/7", "2019/8", "2019/9", "2019/10", "2019/11", "2019/12", "2020/1"))
covid_data <- rbind(tmp, covid_data[c("CASE_COUNT","BX_CASE_COUNT","BK_CASE_COUNT","MN_CASE_COUNT","QN_CASE_COUNT","SI_CASE_COUNT","MM","YY","YY_MM")])
colnames(covid_data) <- c("CASE_COUNT", "BRONX", "KINGS","MANHATTAN","QUEENS","STATEN_ISLAND","MM","YY","YY_MM")
covid_data <- covid_data %>%
  mutate(YY_MM = case_when(YY_MM =="2019/1"~"2019/01", YY_MM =="2019/2"~"2019/02",YY_MM =="2019/3"~"2019/03",YY_MM =="2019/4"~"2019/04",YY_MM =="2019/5"~"2019/05",YY_MM =="2019/6"~"2019/06",YY_MM =="2019/7"~"2019/07",YY_MM =="2019/8"~"2019/08",YY_MM =="2019/9"~"2019/09",YY_MM =="2019/10"~"2019/10",YY_MM =="2019/11"~"2019/11",YY_MM =="2019/12"~"2019/12",YY_MM =="2020/1"~"2020/01", YY_MM =="2020/2"~"2020/02",YY_MM =="2020/3"~"2020/03",YY_MM =="2020/4"~"2020/04",YY_MM =="2020/5"~"2020/05",YY_MM =="2020/6"~"2020/06",YY_MM =="2020/7"~"2020/07",YY_MM =="2020/8"~"2020/08",YY_MM =="2020/9"~"2020/09",YY_MM =="2020/10"~"2020/10",YY_MM =="2020/11"~"2020/11",YY_MM =="2020/12"~"2020/12",YY_MM =="2021/1"~"2021/01", YY_MM =="2021/2"~"2021/02",YY_MM =="2021/3"~"2021/03",YY_MM =="2021/4"~"2021/04",YY_MM =="2021/5"~"2021/05",YY_MM =="2021/6"~"2021/06",YY_MM =="2021/7"~"2021/07",YY_MM =="2021/8"~"2021/08",YY_MM =="2021/9"~"2021/09",YY_MM =="2021/10"~"2021/10",YY_MM =="2021/11"~"2021/11",YY_MM =="2021/12"~"2021/12"))
covid_data$YY_MM <- factor(covid_data$YY_MM)

#Manipulate hate crime data
mod_data <- hate_data %>%
  mutate(Bias.Motive.Description = case_when(Bias.Motive.Description=="ANTI-JEWISH" ~ "ANTI JEWISH", Bias.Motive.Description=="ANTI-ASIAN" ~ "ANTI ASIAN", Bias.Motive.Description=="ANTI-MALE HOMOSEXUAL (GAY)" ~ "ANTI MALE HOMOSEXUAL (GAY)", Bias.Motive.Description=="ANTI-BLACK" ~ "ANTI BLACK", Bias.Motive.Description=="ANTI-ASIAN" ~ "ANTI_ASIAN", TRUE~"OTHER ANTI TYPE"))%>%
  mutate(YY_MM=paste(Complaint.Year.Number, Month.Number, sep = '/')) %>%
  mutate(YY_MM = case_when(YY_MM =="2019/1"~"2019/01", YY_MM =="2019/2"~"2019/02",YY_MM =="2019/3"~"2019/03",YY_MM =="2019/4"~"2019/04",YY_MM =="2019/5"~"2019/05",YY_MM =="2019/6"~"2019/06",YY_MM =="2019/7"~"2019/07",YY_MM =="2019/8"~"2019/08",YY_MM =="2019/9"~"2019/09",YY_MM =="2019/10"~"2019/10",YY_MM =="2019/11"~"2019/11",YY_MM =="2019/12"~"2019/12",YY_MM =="2020/1"~"2020/01", YY_MM =="2020/2"~"2020/02",YY_MM =="2020/3"~"2020/03",YY_MM =="2020/4"~"2020/04",YY_MM =="2020/5"~"2020/05",YY_MM =="2020/6"~"2020/06",YY_MM =="2020/7"~"2020/07",YY_MM =="2020/8"~"2020/08",YY_MM =="2020/9"~"2020/09",YY_MM =="2020/10"~"2020/10",YY_MM =="2020/11"~"2020/11",YY_MM =="2020/12"~"2020/12",YY_MM =="2021/1"~"2021/01", YY_MM =="2021/2"~"2021/02",YY_MM =="2021/3"~"2021/03",YY_MM =="2021/4"~"2021/04",YY_MM =="2021/5"~"2021/05",YY_MM =="2021/6"~"2021/06",YY_MM =="2021/7"~"2021/07",YY_MM =="2021/8"~"2021/08",YY_MM =="2021/9"~"2021/09",YY_MM =="2021/10"~"2021/10",YY_MM =="2021/11"~"2021/11",YY_MM =="2021/12"~"2021/12"))%>%
  mutate(County = case_when(County=="NEW YORK"~"MANHATTAN",County=="RICHMOND"~"STATEN ISLAND",County=="BRONX"~"BRONX", County=="KINGS"~"KINGS",County=="QUEENS"~"QUEENS")) %>%
  select(Full.Complaint.ID, Complaint.Year.Number, YY_MM, 	
         County, Offense.Description, Bias.Motive.Description, Offense.Category)
mod_data$Complaint.Year.Number <- factor(mod_data$Complaint.Year.Number, levels=c("2019","2020","2021"))
mod_data$Bias.Motive.Description <- factor(mod_data$Bias.Motive.Description)
mod_data$County <- factor(mod_data$County)
mod_data$YY_MM <- factor(mod_data$YY_MM)

#Manipulate covid data by month
covid_data_m <- covid_data %>%
  group_by(YY_MM) %>%
  summarise(sum(CASE_COUNT))
colnames(covid_data_m) <- c("YY_MM", "cases")

# Count hate crime by bias motive by month and by year
anti_data_m <- mod_data %>%
  group_by(YY_MM, Bias.Motive.Description) %>%
  summarise(count = n())

anti_data_y <- mod_data %>%
  group_by(Complaint.Year.Number, Bias.Motive.Description) %>%
  summarise(count = n())
# Count covid data by location by month
covid_data_m_l<- covid_data %>%
  group_by(YY_MM) %>%
  summarise(sum(MANHATTAN),sum(BRONX),sum(QUEENS),sum(KINGS), sum(STATEN_ISLAND))
# Count Anti-Asian by location by month
data_m_loc <- mod_data[mod_data$Bias.Motive.Description=="ANTI ASIAN",] %>%
  group_by(YY_MM, County) %>%
  summarise(count=n())





#### #### #### ####    shooting tap data processing #### #### #### #### #### #### 

######################## shooting trend plot data wrangling
shooting_recent <- read_csv("../data/NYPD_Shooting_Incident_Data__Year_To_Date_.csv")
shooting_historic <- read_csv("../data/NYPD_Shooting_Incident_Data__Historic.csv")
shooting_recent <- shooting_recent %>% rename(`Lon_Lat` = `New Georeferenced Column`)
shooting_all <- rbind(shooting_recent, shooting_historic) # combine two datasets using rbind()

shooting_all_overall <- shooting_all %>% 
  mutate(OCCUR_DATE = as.Date(OCCUR_DATE, "%m/%d/%Y"),
         OCCUR_YM =  floor_date(OCCUR_DATE, unit = "month"),
         BORO = as.factor(BORO)) %>%
  group_by(OCCUR_YM, BORO) %>%
  summarise(count = n())

shooting_all_covid <- shooting_all %>% 
  mutate(OCCUR_DATE = as.Date(OCCUR_DATE, format = "%m/%d/%Y"),
         OCCUR_YM = floor_date(OCCUR_DATE, unit = "month"),
         BORO = as.factor(BORO)) %>%
  filter(OCCUR_YM >= "2020-03-01") %>% 
  group_by(OCCUR_YM, BORO) %>%
  summarise(count = n())

######################## shooting map data 
#shooting_map_data <- shooting_all %>% 
#  mutate(OCCUR_DATE = as.Date(OCCUR_DATE, "%m/%d/%Y"),
#         OCCUR_YM =  floor_date(OCCUR_DATE, unit = "month"),
#         BORO = as.factor(BORO)) %>%
#  filter(OCCUR_YM >= as.Date("2020-03-01")) %>%
#  select(BORO,Longitude,Latitude,OCCUR_YM)

#zipcode <- read.csv("../data/zc_geo.csv", sep = ";") %>% 
#  select(Zip, Latitude, Longitude) 

#match <- distm(shooting_map_data[,c("Longitude","Latitude")], zipcode[,c("Longitude","Latitude")], fun=distVincentyEllipsoid)
#shooting_map_data$Zip <- zipcode$Zip[max.col(-match)]

#zip_convert <- read_csv("../data/Geography-resources/ZCTA-to-MODZCTA.csv")

# match with modified zip code 
#shooting_map_data_join <- shooting_map_data %>%
#  mutate(Zip = as.double(Zip)) %>%
#  left_join(zip_convert, by= c("Zip" = "ZCTA")) %>%
#  select(-c("Longitude","Latitude", "Zip")) %>%
#  mutate(MODZCTA = as.character(MODZCTA)) %>%
#  group_by(OCCUR_YM, MODZCTA, BORO) %>%
#  summarise(count = n())


#import shapefile 
#modzcta <- sf::st_read("../data/Geography-resources/MODZCTA_2010.shp")
# join spatial data 
#shooting_map_sp <- geo_join(modzcta, shooting_map_data_join, 'MODZCTA', 'MODZCTA', how = "inner")
#saveRDS(shooting_map_sp, "shooting_map_sp.RDS")


shooting_map_sp <- readRDS("../data/shooting_map_sp.RDS")
shooting_map_sp <- shooting_map_sp %>% arrange(OCCUR_YM)





