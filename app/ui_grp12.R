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
library(shinythemes)
library(plotly)
library(ggplot2)
library(shinydashboard)

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
          top = "40%",
          left = "25%",
          right = "25%",
          height = 100,
          tags$h3("Target User", style = "color: white"),
          tags$hr(),
          tags$p(
            style = "padding: 3%; background-color: white; font-family: alegreya; font-size: 100%; opacity: 0.90",
            "This application is designed to provide", strong("government officials"), "with a better visualization of how the pandemic has affected different kinds of crimes, as well as to assist them in better decision-making when similar events happen again."
          )),
        absolutePanel(
          style = "background-color: white",
          style = "opacity: 0.90",
          top = "65%",
          left = "25%",
          right = "25%",
          height = 150,
          tags$h3("Interaction with the App", style = "color: white"),
          tags$hr(),
          tags$ul(
            style = "padding: 4%; background-color: white; font-family: alegreya; font-size: 100%; opacity: 0.90",
            tags$li("The", strong("first"), "tab: Introduction"),
            tags$li("The", strong("second"), "tab: "),
            tags$li("The", strong("third and fourth"), "tab: "),
            tags$li("The", strong("fifth"),"tab: Reference")
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
    "Shooting",
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
    "Hate Crime",
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
                  plotOutput("Plot1"), 
                  h3("Hate Crime Vs Covid New Cases"), 
                  plotOutput("Plot2"), 
                  h3("ANTI-ASIAN Cases in Different Areas"), 
                  plotOutput("Plot3"), 
                  position="right")
        
      )
    )
    
  ),
  #################### tab 5: Reference#################### 
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
    ),
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
