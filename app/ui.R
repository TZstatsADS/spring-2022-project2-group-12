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
          The application start by examining the", strong("overall trend"), ", and provide some interesting insights on the change of", strong("shooting cases"), ", with a special focus on how the", strong("hate crime"), "towards Asian people increased after the hit of Covid."
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
  #################### tab 2: map  ####################
  tabPanel("Map",
           h2("Confirmed Cases in Each Borough", align = 'center'),
           tags$p("Interactive Map: Zoom in or out to see the size of confirmed Covid cases in each borough"),
           tags$p("Note that the size of the circles represents the number of comfirmed Covid cases within each zipcode boundaries"),
           leafletOutput("nyc_map_covid", width = "100%", height = 800),
           h5("Note: Radius = Number of Comfirmed Covid Cases/1000", align = 'right')
  ),



  ################### tab 3: Arrest ##################
  tabPanel("Arrest Records",
           fluidPage(
             titlePanel("Did the number of arrests in New York affected COVID?"),
             tags$p("An unexpected fact: number of arrests decreased in 2020 than before COVID-19. But it is worth mentioning, with New York City reportedly experienced a dramatic increase of number in arrests in the month of May, 2020 where the pandemic was at its peak.", style="color:black"),
             tags$p("Use the dropbox to choose a time frame to have a closer look"),
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
                             label = h5("Choose a specific time period"), 
                             choices = c("Before Covid", "After Covid","2019","2020","2021")),
               ),
               
               mainPanel(
                 h3("Overall information of the arrest data:" ),
                 plotlyOutput('Plotall1'),
                 plotlyOutput('Plotall2'),
                 h3("Detailed information in your choosing time period:" ),
                 plotlyOutput('ARPlot1'),
                 plotlyOutput('ARPlot2'),
                 plotlyOutput('ARPlot3'),
                 position = "right"
               )
             )
           )
  ),
  
  
  
  
  
  
  
  
  
  #################### tab 4: shooting #################### 


  tabPanel(
    "Shooting Trends",
    titlePanel("Shooting Cases Before & After Covid Started: A Comparison"),
    tags$p("Use the drop box to examine shooting cases across different boroughs"),
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
        leafletOutput("shooting_map_interactive"),
        tags$p("Notice: the color bins are not equally sized.")
      )
    )
  ),


  
  #################### tab 5: hate crime #################### 
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
    ),
    tags$h2(
      "Thank you for using the application!"
    )
  ),
)
