#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
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
)
