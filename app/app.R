#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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




#########################Data processing for arrest part
df <- read.csv("../data/Arrest_2019_2021.csv")
df$ARREST_DATE = as.Date(df$ARREST_DATE,format = "%Y-%m-%d")
df$year = format(df$ARREST_DATE,'%Y')
df$month = format(df$ARREST_DATE,'%m')
df$YY_MM = paste(df$year, df$month, sep = '/')
df["ARREST_BORO"][df["ARREST_BORO"] == "B"] <- "Bronx"
df["ARREST_BORO"][df["ARREST_BORO"] == "K"] <- "Brooklyn"
df["ARREST_BORO"][df["ARREST_BORO"] == "M"] <- "Manhattan"
df["ARREST_BORO"][df["ARREST_BORO"] == "Q"] <- "Queens"
df["ARREST_BORO"][df["ARREST_BORO"] == "S"] <- "Staten Island"

df["LAW_CAT_CD"][df["LAW_CAT_CD"] == "F"] <- "Felony"
df["LAW_CAT_CD"][df["LAW_CAT_CD"] == "M"] <- "Misdemeanor"
df["LAW_CAT_CD"][df["LAW_CAT_CD"] == "V"] <- "Violation"
df["LAW_CAT_CD"][df["LAW_CAT_CD"] == "I"] <- "Infractions"

# Define UI ----
ui <- navbarPage(
  title = "NYC Crime Analysis",
  #################### tab 1: Homepage ####################
  tabPanel(
    "Homepage",
    tags$img(
      src = "https://nypost.com/wp-content/uploads/sites/2/2022/02/042421policetape3CS-5.jpg?quality=90&strip=all&w=1488",
      width = "100%",
      style = "opacity: 1"
    ),
    fluidRow(
      absolutePanel(
        style = "background-color: white",
        style = "opacity: 0.90",
        top = "10%",
        left = "25%",
        right = "25%",
        height = 200,
        tags$h3("NYC Crime Analysis in the Context of Covid-19 Pandemic", style = "color: white"),
        tags$h4("Introduction", style = "color: white"),
        tags$hr(),
        tags$p(
          style = "padding: 3%; background-color: white; font-family: alegreya; font-size: 100%; opacity: 0.90",
          "The hit of Covid-19 pandemic has surly impacted every aspect of our lives. This application specifically draws attention of how the pandemic affected the", strong("crime events in New York City"),". 
          We start by examining the overall trend, and provide some interesting insights on the change of shooting cases, with a special focus on how the hate crime toward Asian people arose after the hit of covid."
        )),
      absolutePanel(
        style = "background-color: white",
        style = "opacity: 0.90",
        top = "50%",
        left = "25%",
        right = "25%",
        height = 100,
        tags$h4("Target User", style = "color: white"),
        tags$hr(),
        tags$p(
          style = "padding: 3%; background-color: white; font-family: alegreya; font-size: 100%; opacity: 0.90",
          "This application is designed to provide", strong("government officials"), "with a better visualization of how the pandemic has affected different kinds of crimes, as well as to assist them in better decision-making when similar events happen again."
        ))
    )
  ),
  #################### tab 2:  ####################
  tabPanel("Map",
           h2("Confirmed Cases in Each Borough", align = 'center'),
           leafletOutput("nyc_map_covid", width = "100%", height = 800)
  ),
  
  
  
  #################### tab 3: Map#################### 
  
  
  tabPanel(
    "Shooting Trends",
    sidebarLayout(
      sidebarPanel(
        
        selectInput(inputId = "period",
                    label = "Choose a time period", 
                    choices = c("Overall Period","Covid Period")),
        selectInput(inputId = "date",
                    label = "Select a date (month ending in):",
                    choices = levels(as.factor(unique(shooting_map_sp$OCCUR_YM))))
      ),
      
      mainPanel(
        plotlyOutput(outputId = "shooting"),
        leafletOutput("shooting_map_interactive")
      )
    )
  ),
  
  
  
  #################### tab 4: #################### 
  tabPanel(
    "Hate Crime Trends",
    fluidPage(
      titlePanel("Did the number of hate crimes increase after COVID?"),
      sidebarLayout(
        sidebarPanel( position="left",
                      helpText("Observe the number of different types of hate crimes in New York City between 2019 and 2021
                 by year or by month"),
                      selectInput("option1", label = h5("Select By Year or By Month"), 
                                  choices = list("Year"=1 ,"Month"=2), selected = 1),
                      helpText("Compare COVID new cases with one type of hate crimes in New York City between 2019 and 2021 by month"),
                      selectInput("option2", label = h5("Select ANTI Type"), 
                                  choices = list("ANTI-ASIAN"=1 ,"ANTI-BLACK"=2,"ANTI-JEWISH"=3,"ANTI MALE HOMOSEXUAL"=4, "OTHER ANTI TYPE"=5), selected = 1),
                      helpText("Compare COVID new cases with ANTI-ASIAN hate crimes in different areas between 2019 and 2021 by month"),
                      selectInput("option3", label = h5("Select Area"), 
                                  choices = list("Manhattan"=1 ,"Bronx"=2,"Kings"=3,"Queens"=4, "Staten Island"=5), selected = 1)),
        
        mainPanel(h3("Different Types of Hate Crimes Vs Covid New Cases in Different Areas"), 
                  plotlyOutput("Plot1"), 
                  h3("Hate Crime Vs Covid New Cases"), 
                  plotOutput("Plot2"), 
                  h3("ANTI-ASIAN Cases in Different Areas"), 
                  plotOutput("Plot3"), 
                  position="right")
        
      )
    )
    
  ),
  
  
  ################### tab 5: Arrest
  tabPanel("Arrest Records",
           fluidPage(
             titlePanel("Did the number of arrests in New York affected COVID?"),
             tags$p("An unexpected fact: number of arrests decreased in 2020 than before COVID-19. But it is worth mentioning, with New York City reportedly experienced a dramatic increase of number in arrests in the month of May,2020 where the pandemic was at its peak.", style="color:black"),
             #tags$p("This analysis helps individuals to learn more about the rise of arrests in NYC. It shows the rate of arrests with respect to each of the five boroughs in NYC", style="color:black"),
             
             
             sidebarLayout(
               sidebarPanel(
                 
                 helpText("Observe the number of arrests occured in New York City between 2019 and 2021
                 by year or by month"),
                 selectInput(inputId = "by", 
                             label = h5("Select By Year or By Month"), 
                             choices = list("Year" ,"Month")),
                 
                 helpText("Explore yearly arrest data with respect to location, level of offense, and perpetrator's race"),
                 selectInput(inputId = "year",
                             label = h5("Choose a specific year"), 
                             choices = c("2019","2020","2021")),
               ),
               
               mainPanel(
                 h3("Overall information of the arrest data:" ),
                 plotlyOutput('Plotall1'),
                 
                 h3("Detailed information in your choosing year:" ),
                 plotlyOutput('ARPlot1'),
                 plotlyOutput('ARPlot2'),
                 plotlyOutput('ARPlot3'),
                 position = "right"
               )
             )
           )
  ),
  
  
  
  
  
  
  
  
  
  
  
  #################### tab 6: Reference#################### 
  tabPanel(
    "Reference",
    tags$h2(
      "Data Sources"
    ),
    tags$a(
      href = "https://github.com/nychealth/coronavirus-data",
      "github.com/nychealth/coronavirus-data"
    ),br(),
    tags$a(
      href = "https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Year-To-Date-/5ucz-vwe8",
      "data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Year-To-Date"
    ),br(),
    tags$a(
      href = "https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data",
      "data.cityofnewyork.us/Business/Zip-Code-Boundaries"
    ),br(),
    tags$a(
      href = "https://data.cityofnewyork.us/Public-Safety/NYPD-Arrests-Data-Historic-/8h9b-rp9u",
      "data.cityofnewyork.us/Public-Safety/NYPD-Arrests-Data-Historic"
    ),br(),
    tags$a(
      href = "https://data.cityofnewyork.us/Public-Safety/NYPD-Arrest-Data-Year-to-Date-/uip8-fykc",
      "data.cityofnewyork.us/Public-Safety/NYPD-Arrest-Data-Year-to-Date"
    ),br(),
    tags$a(
      href = "https://data.cityofnewyork.us/Public-Safety/NYPD-Hate-Crimes/bqiq-cu78",
      "data.cityofnewyork.us/Public-Safety/NYPD-Hate-Crimes"
    ),br(),
    tags$h2(
      "Created By:"
    ),
    tags$p(
      "Nallamaddi, Shriya (sn2956@columbia.edu)"
    ),
    tags$p(
      "Tang, Shiqi (st3349@columbia.edu)"
    ),
    tags$p(
      "Wang, Jiuru (jw4150@columbia.edu)"
    ),
    tags$p(
      "Yu, Jiaxin (jy3161@columbia.edu)"
    ),
    tags$p(
      "Zhang, Nichole (qz2446@columbia.edu)"
    ),
    tags$h2(
      "GitHub Repository"
    ),
    tags$a(
      href = "https://github.com/TZstatsADS/spring-2022-project2-group-12",
      "github.com/TZstatsADS/spring-2022-project2-group-12"
    )
    
  ),
)

shinyServer <-function(input, output, session) {
  ####################### Tab 2 Map ##################
  map_base <-
    leaflet(options = leafletOptions(dragging = T, minZoom = 10, maxZoom = 16)) %>%
    setView(lng = -73.92,lat = 40.72, zoom = 11) %>% 
    addTiles() %>%
    addProviderTiles("CartoDB.Positron")
  
  # join zipcode geo with covid data from nyc_recent_4w_data
  nyc_zipcode_geo = nyc_zipcode_geo %>%
    left_join(nyc_covid_data, by = c("ZIPCODE"="MODIFIED_ZCTA"))
  #left_join(nyc_shooting_data, by = c("ZIPCODE"="Zip"))
  
  pal <- colorFactor(palette =c('lightblue1', 'lightpink', 'darksalmon', 'darkseagreen2', 'lavender'), domain = nyc_covid_data$BOROUGH_GROUP)
  
  observe({
    output$nyc_map_covid = renderLeaflet({
      
      nyc_map_output = map_base %>% 
        addPolygons( 
          data = nyc_zipcode_geo,
          weight = 0.5, color = "#41516C", fillOpacity = 0,
          popup = ~(paste0( 
            "<b>Zip Code: ",ZIPCODE ,
            "</b><br/>Borough: ",BOROUGH_GROUP,
            "<br/>Confirmed Cases: ", COVID_CASE_COUNT
          )),
          highlight = highlightOptions(
            weight = 2, color = "red", bringToFront = F) ) %>%
        addCircleMarkers(
          data = nyc_zipcode_geo,
          lng = ~LNG_repre, lat = ~LAT_repre,
          color = ~pal(BOROUGH_GROUP), fillOpacity = 0.8,
          radius = ~(COVID_CASE_COUNT)/1000, 
          popup = ~(paste0(
            "<b>Zip Code: ", ZIPCODE,
            "</b><br/>Confirmed Cases: ", COVID_CASE_COUNT
          )),
          group = "Covid Cases"
        ) 
    }) # end of observe
    
    leafletProxy("nyc_map_covid")
  }) # end of tab
  
  
  ####################### Tab 3 Shooting  ##################
  
  output$shooting <- renderPlotly({
    if("Overall Period" %in% input$period){
      ggplot(shooting_all_overall, aes(x = OCCUR_YM, y = count, group = BORO, color = BORO)) +
        geom_line()+
        scale_color_viridis(discrete = TRUE, option = "G") +
        scale_x_date(breaks = "1 year", date_labels = "%b-%y") +
        labs(y = "Number of Shooting Occurred", x = "Date", title = "NYC Shooting Occurrence Since 2006") +
        guides(color=guide_legend(title="Location")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45),
              axis.text.y = element_text(angle = 45),
              plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
      
      ggplotly(width = 1000)
    }
    else if("Covid Period" %in% input$period){
      ggplot(shooting_all_covid, aes(x = OCCUR_YM, y = count, group = BORO, color = BORO)) +
        geom_line()+
        scale_color_viridis(discrete = TRUE, option = "G") +
        scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
        labs(y = "Number of Shooting Occurred", x = "Date", title = "NYC Shooting Occurrence During Covid") +
        guides(color=guide_legend(title="Location")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45),
              axis.text.y = element_text(angle = 45),
              plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
      
      ggplotly(width = 1000)
    }
    
  })
  
  monthly_shooting <- reactive({
    m <- shooting_map_sp %>% filter(OCCUR_YM == input$date)
    return(m)
  })
  
  output$shooting_map_interactive <- renderLeaflet({
    labels <- sprintf("<strong>%s</strong><br/>%s<br/>%s<br/>%g shooting cases", monthly_shooting()$BORO, monthly_shooting()$MODZCTA, 
                      monthly_shooting()$OCCUR_YM,monthly_shooting()$count) %>%
      lapply(htmltools::HTML)
    
    pal <- colorBin(palette = "OrRd", 9, domain= shooting_map_sp$count, bins = c(1,2,4,6,8,10,15,20,31)) # caution: the bins are not equally sized 
    
    monthly_shooting() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-73.9, 40.7, zoom = 10) %>%     # for initial view of NYC
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = .5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(monthly_shooting()$count),
                  highlightOptions = highlightOptions(weight =5,
                                                      fillOpacity = 1,
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~ count,
                title = "Shooting Cases",
      )
  })
  # end of tab
  
  
  ####################### Tab 4 Hate Crime ##################
  output$Plot1 <- renderPlotly({
    if(input$option1 == 1) {
      ggplot(data=anti_data_y)+
        geom_line(mapping = aes(x=Complaint.Year.Number,  y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        labs(x="Year", y="Number of Crimes")+
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=14),axis.text.y = element_text(face="bold", size=11))
      
      ggplotly()}
    else if(input$option1 == 2){
      ggplot(data=anti_data_m)+
        geom_line(mapping = aes(x=YY_MM,  y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        labs(xlabel="YY/MM", y="Number of Crimes")+
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90), axis.text.y = element_text(face="bold", size=11))
      
      ggplotly()
    }
  })
  
  output$Plot2 <- renderPlot({
    if(input$option2 == 1){
      anti_data_m_aa <- anti_data_m[anti_data_m$Bias.Motive.Description=="ANTI ASIAN",]
      ggplot()+
        geom_line(data=anti_data_m_aa,mapping = aes(x=YY_MM, y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        geom_line(data=covid_data_m,mapping = aes(x=YY_MM, y=cases/10000, group=1), col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes",
          sec.axis = sec_axis( trans=~.*10000, name="Covid Cases in NYC")
        ) +
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90), axis.text.y = element_text(face="bold", size=11))+
        theme(legend.title = element_blank())+
        scale_color_manual(labels = c("ANTI-ASIAN", 
                                      "Covid New Cases"), 
                           values = c("ANTI-ASIAN"="black", 
                                      "Covid New Cases"="light blue"))
      
    }
    else if(input$option2 == 2){
      anti_data_m_aa <- anti_data_m[anti_data_m$Bias.Motive.Description=="ANTI BLACK",]
      ggplot()+
        geom_line(data=anti_data_m_aa,mapping = aes(x=YY_MM, y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        geom_line(data=covid_data_m,mapping = aes(x=YY_MM, y=cases/10000, group=1), col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-BLACK Crimes",
          sec.axis = sec_axis( trans=~.*10000, name="Covid Cases in NYC")
        ) +
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        theme(legend.title = element_blank())+
        scale_color_manual(labels = c("ANTI-ASIAN", 
                                      "Covid New Cases"), 
                           values = c("ANTI-BLACK"="black", 
                                      "Covid New Cases"="light blue"))
      
    }
    else if(input$option2 == 3){
      anti_data_m_aa <- anti_data_m[anti_data_m$Bias.Motive.Description=="ANTI JEWISH",]
      ggplot()+
        geom_line(data=anti_data_m_aa,mapping = aes(x=YY_MM, y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        geom_line(data=covid_data_m,mapping = aes(x=YY_MM, y=cases/10000, group=1), col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-JEWISH Crimes",
          sec.axis = sec_axis( trans=~.*10000, name="Covid Cases in NYC")
        ) +
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        theme(legend.title = element_blank())+
        scale_color_manual(labels = c("ANTI-JEWISH", 
                                      "Covid New Cases"), 
                           values = c("ANTI-JEWISH"="black", 
                                      "Covid New Cases"="light blue"))
      
    }
    else if(input$option2 == 4){
      anti_data_m_aa <- anti_data_m[anti_data_m$Bias.Motive.Description=="ANTI MALE HOMOSEXUAL (GAY)",]
      ggplot()+
        geom_line(data=anti_data_m_aa,mapping = aes(x=YY_MM, y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        geom_line(data=covid_data_m,mapping = aes(x=YY_MM, y=cases/10000, group=1), col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ANTI MALE HOMOSEXUAL (GAY) Crimes",
          sec.axis = sec_axis( trans=~.*10000, name="Covid Cases in NYC")
        ) +
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90))+
        theme(legend.title = element_blank())+
        scale_color_manual(labels = c("ANTI-ANTI MALE HOMOSEXUAL (GAY)", 
                                      "Covid New Cases"), 
                           values = c("ANTI-ANTI MALE HOMOSEXUAL (GAY)"="black", 
                                      "Covid New Cases"="light blue"))
      
    }
    else if(input$option2 == 5){
      anti_data_m_aa <- anti_data_m[anti_data_m$Bias.Motive.Description=="OTHER ANTI TYPE",]
      ggplot()+
        geom_line(data=anti_data_m_aa,mapping = aes(x=YY_MM, y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
        geom_line(data=covid_data_m,mapping = aes(x=YY_MM, y=cases/10000, group=1), col="light blue")+
        scale_y_continuous(
          name = "The Number of OTHER ANTI TYPE Crimes",
          sec.axis = sec_axis( trans=~.*10000, name="Covid Cases in NYC")
        ) +
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        theme(legend.title = element_blank())+
        scale_color_manual(labels = c("OTHER ANTI TYPE", 
                                      "Covid New Cases"), 
                           values = c("OTHER ANTI TYPE"="black", 
                                      "Covid New Cases"="light blue"))
      
    }
  })
  
  output$Plot3 <- renderPlot({
    if(input$option3 == 1){
      
      data_m_with_loc <- data_m_loc[data_m_loc$County=="MANHATTAN",]
      ggplot()+
        geom_line(data=data_m_with_loc, mapping = aes(x=YY_MM,  y=count, group = 1))+
        geom_line(data=covid_data_m_l, mapping = aes(x=YY_MM,  y=`sum(MANHATTAN)`/1000, group=1),col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes in Manhattan",
          sec.axis = sec_axis( trans=~.*1000, name="Covid New Cases in Manhattan")
        ) +
        
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        scale_color_manual(labels = c("ANTI-ASIAN Crimes in Manhattan", 
                                      "Covid New Cases in Manhattan"), 
                           values = c("ANTI-ASIAN Crimes in Manhattan"="black", 
                                      "Covid New Cases in Manhattan"="light blue"))
      
    }
    
    else if(input$option3 == 2){
      data_m_with_loc <- data_m_loc[data_m_loc$County=="BRONX",]
      ggplot()+
        geom_line(data=data_m_with_loc, mapping = aes(x=YY_MM,  y=count, group = 1))+
        geom_line(data=covid_data_m_l, mapping = aes(x=YY_MM,  y=`sum(MANHATTAN)`/1000,group=1),col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes in Bronx",
          sec.axis = sec_axis( trans=~.*1000, name="Covid New Cases in Bronx")
        ) +
        
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        theme(legend.title = element_blank())+
        scale_color_manual(labels = c("ANTI-ASIAN Crimes in Bronx", 
                                      "Covid New Cases in Bronx"), 
                           values = c("ANTI-ASIAN Crimes in Bronx"="black", 
                                      "Covid New Cases in Bronx"="light blue"))
      
    }
    
    else if(input$option3 == 3){
      
      data_m_with_loc <- data_m_loc[data_m_loc$County=="KINGS",]
      ggplot()+
        geom_line(data=data_m_with_loc, mapping = aes(x=YY_MM,  y=count, group = 1))+
        geom_line(data=covid_data_m_l, mapping = aes(x=YY_MM,  y=`sum(MANHATTAN)`/1000,group=1),col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes in Kings",
          sec.axis = sec_axis( trans=~.*1000, name="Covid New Cases in Kings")
        ) +
        
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        scale_color_manual(labels = c("ANTI-ASIAN Crimes in Kings", 
                                      "Covid New Cases in Kings"), 
                           values = c("ANTI-ASIAN Crimes in Kings"="black", 
                                      "Covid New Cases in Kings"="light blue"))
      
    }
    
    else if(input$option3 == 4){
      data_m_with_loc <- data_m_loc[data_m_loc$County=="QUEENS",]
      ggplot()+
        geom_line(data=data_m_with_loc, mapping = aes(x=YY_MM,  y=count, group = 1))+
        geom_line(data=covid_data_m_l, mapping = aes(x=YY_MM,  y=`sum(MANHATTAN)`/1000,group=1),col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes in Queens",
          sec.axis = sec_axis( trans=~.*1000, name="Covid New Cases in Queens")
        ) +
        
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        scale_color_manual(labels = c("ANTI-ASIAN Crimes in Queens", 
                                      "Covid New Cases in Queens"), 
                           values = c("ANTI-ASIAN Crimes in Queens"="black", 
                                      "Covid New Cases in Queens"="light blue"))
      
    }
    
    else if(input$option3 == 5){
      data_m_with_loc <- data_m_loc[data_m_loc$County=="STATEN ISLAND",]
      ggplot()+
        geom_line(data=data_m_with_loc, mapping = aes(x=YY_MM,  y=count, group = 1))+
        geom_line(data=covid_data_m_l, mapping = aes(x=YY_MM,  y=`sum(MANHATTAN)`/1000,group=1),col="light blue")+
        scale_y_continuous(
          name = "The Number of ANTI-ASIAN Crimes in Staten Island",
          sec.axis = sec_axis( trans=~.*1000, name="Covid New Cases in Staten Island")
        ) +
        
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90),axis.text.y = element_text(face="bold", size=11))+
        scale_color_manual(labels = c("ANTI-ASIAN Crimes in Staten Island", 
                                      "Covid New Cases in Staten Island"), 
                           values = c("ANTI-ASIAN Crimes in Staten Island"="black", 
                                      "Covid New Cases in Staten Island"="light blue"))
      
    }
  }) 
  
  # end of tab
  
  
  ####################### Tab 5 Arrest ##################
  
  
  year_chosen <- reactive({
    if ( "2019" %in% input$year){
      df1_2019 = df[df$year=='2019',]
      data <- df1_2019
      return( data ) 
    }
    if ( "2020" %in% input$year){
      df1_2020 = df[df$year=='2020',]
      data <- df1_2020
      return( data ) 
    }
    if ( "2021" %in% input$year){
      df1_2021 = df[df$year=='2021',]
      data <- df1_2021
      return( data ) 
    }
  })
  
  
  output$Plotall1 <- renderPlotly(
    if ( "Year" %in% input$by){
      data <- df %>% group_by(ARREST_BORO, year) %>%
        summarise(count = n())
      ggplot(data, aes(x = year, y = count, group = ARREST_BORO, color = ARREST_BORO)) +
        geom_line()+
        scale_color_viridis(discrete = TRUE, option = "C") +
        #scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
        labs(y = "Number of Arrests", x = "Period", title = "NYC Arrests Occurrence By Year") +
        guides(color=guide_legend(title="Location")) +
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90), axis.text.y = element_text(face="bold", size=11))
      
      ggplotly(width = 750)
      
    } else if ("Month" %in% input$by){
      data <- df %>% group_by(ARREST_BORO, YY_MM) %>%
        summarise(count = n())
      ggplot(data, aes(x = YY_MM, y = count, group = ARREST_BORO, color = ARREST_BORO)) +
        geom_line()+
        scale_color_viridis(discrete = TRUE, option = "C") +
        #scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
        labs(y = "Number of Arrests", x = "Period", title = "NYC Arrests Occurrence By Month" ) +
        guides(color=guide_legend(title="Location")) +
        theme_classic() +
        theme(axis.text.x = element_text(face="bold", size=11, angle = 90), axis.text.y = element_text(face="bold", size=11))
      
      ggplotly(width = 750)
    }
  )
  
  
  output$ARPlot1 <- renderPlotly({
    target <- c('Felony', 'Misdemeanor', 'Violation', 'Infractions')
    data <- year_chosen() %>% filter(LAW_CAT_CD %in% target) %>%
      group_by(ARREST_BORO, LAW_CAT_CD) %>%
      summarise(count = n())
    
    ggplot(data, aes(fill=ARREST_BORO, y=count, x=LAW_CAT_CD)) + 
      geom_bar(position="fill", stat="identity") + scale_fill_brewer(palette = "Pastel2")+
      labs(
        title = paste("1. NYC Arrests in", input$year),
        x = "Level of offense",
        y = "Percent arrests w.r.t location"
      ) +
      theme_bw(base_size = 12)
    
    ggplotly(width = 750)
  })
  
  
  
  
  output$ARPlot2 <- renderPlotly({
    target <- c('Felony', 'Misdemeanor', 'Violation', 'Infractions')
    data <- year_chosen() %>% 
      filter(LAW_CAT_CD %in% target) %>%
      group_by(LAW_CAT_CD,ARREST_BORO) %>%
      summarise(count = n())
    
    ggplot(data, aes(fill = LAW_CAT_CD, y = count, x = ARREST_BORO)) +
      geom_bar(position="stack", stat="identity") +scale_fill_brewer(palette = "Pastel2")+
      #scale_color_viridis(discrete = TRUE, option = "G") +
      labs(y = "Number of crimes Occurred",
           x = "Location",
           title = paste("2. Level of Offense in Each Borough in", input$year)) +
      guides(fill=guide_legend(title="Level of Offense")) +
      theme_bw(base_size = 12)
    
    
    ggplotly(width = 750)
  })
  
  
  output$ARPlot3 <- renderPlotly({
    target <- c('Felony', 'Misdemeanor', 'Violation', 'Infractions')
    data <- year_chosen() %>%
      filter(LAW_CAT_CD %in% target) %>%
      filter(PERP_RACE != 'AMERICAN INDIAN/ALASKAN NATIVE') %>%
      group_by(LAW_CAT_CD,PERP_RACE) %>%
      summarise(count = n())
    
    ggplot(data, aes(fill = LAW_CAT_CD, y = count, x = PERP_RACE)) +
      geom_bar(position="stack", stat="identity") +scale_fill_brewer(palette = "Pastel2")+
      #scale_color_viridis(discrete = TRUE, option = "G") +
      labs(y = "Number of crimes Occurred",
           x = "Perpetrator’s Race",
           title = paste("3. Level of Offense in Each Race in", input$year)) +
      guides(fill=guide_legend(title="Level of Offense")) +
      theme_bw(base_size = 12) +
      theme(axis.text.x = element_text(face="bold", size=11, angle = 90))
    
    ggplotly(width = 750, height = 550)
  })
  
  # end of tab
  
}


# Run the application 
shinyApp(ui = ui, server = shinyServer)
