if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("readr")) {
  install.packages("readr")
  library(readr)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(lubridate)
}
if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}
if (!require("viridis")) {
  install.packages("viridis")
  library(viridis)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}


getwd()
setwd('/Users/jiuruwang/Documents/GitHub/Stat 5243 Projects/spring-2022-project2-group-12/app/overall arrest')
df <- read.csv("../../data/Arrest_2019_2021.csv")
#df$CMPLNT_FR_DT = as.Date(df$CMPLNT_FR_DT,format = "%m/%d/%Y")
#df$year = format(df$CMPLNT_FR_DT,'%Y')
ui <- navbarPage(id="nav", windowTitle = "crime project", "Crime",
    tabPanel("Arrest",
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
    )
)
          
                         
                           
                            
                           



shinyServer <- function(input, output) {
  
  df$ARREST_DATE = as.Date(df$ARREST_DATE,format = "%Y-%m-%d")
  df$year = format(df$ARREST_DATE,'%Y')
  df$month = format(df$ARREST_DATE,'%m')
  df$YY_MM = paste(df$year, df$month, sep = '/')
  df["ARREST_BORO"][df["ARREST_BORO"] == "B"] <- "Bronx"
  df["ARREST_BORO"][df["ARREST_BORO"] == "K"] <- "Brooklyn"
  df["ARREST_BORO"][df["ARREST_BORO"] == "M"] <- "Manhattan"
  df["ARREST_BORO"][df["ARREST_BORO"] == "Q"] <- "Qweens"
  df["ARREST_BORO"][df["ARREST_BORO"] == "S"] <- "Staten Island"
  
  df["LAW_CAT_CD"][df["LAW_CAT_CD"] == "F"] <- "Felony"
  df["LAW_CAT_CD"][df["LAW_CAT_CD"] == "M"] <- "Misdemeanor"
  df["LAW_CAT_CD"][df["LAW_CAT_CD"] == "V"] <- "Violation"
  df["LAW_CAT_CD"][df["LAW_CAT_CD"] == "I"] <- "Infractions"
  
  
  
    
    
    
  
  
  
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
  
  
  
  
  
  
  
    
  
} 
shinyApp(ui = ui, server = shinyServer)