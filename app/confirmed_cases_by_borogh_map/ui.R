#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(viridis)
library(dplyr)
library(tibble)
library(tidyverse)
library(shinythemes)
library(sf)
library(RCurl)
library(tmap)
library(rgdal)
library(leaflet)
library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(shinydashboard)

# Define UI ----
body <- dashboardBody(
  
  
    # ------------------ Map-----------------------------------
    tabItem(tabName = "Map",
            h2("Confirmed Cases in Each Borough", align = 'center'),
            leafletOutput("nyc_map_covid", width = "100%", height = 800)
    )
)