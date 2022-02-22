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
ui <- navbarPage(theme = shinytheme("darkly"), id="nav", windowTitle = "crime project", "Crime",
  tabPanel("Arrests",
    sidebarLayout(
      sidebarPanel(
        
        
        
        
        
        selectInput(inputId = "by", 
                    label = h5("Select By Year or By Month"), 
                    choices = list("Year" ,"Month"), selected = 1),
        
        
        selectInput(inputId = "year",
                    label = h5("Choose a specific year"), 
                    choices = c("2019","2020","2021")),
                    
    
        
        
      ),
      
      mainPanel(
        #plotlyOutput('Plotall1'),
        
        
        plotlyOutput('ARPlot1'),
        plotlyOutput('ARPlot2'),
        plotlyOutput('ARPlot3'),
        position = "right"
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
  
  
  
  
  
  by_chosen <- reactive({
    if ( "Year" %in% input$by){
      
      data <- df1_2019
      return( data ) 
    }
    
    
    
    
    
  })
  
  
  year_chosen <- reactive({
   
    if ( "2019" %in% input$year){
      df1_2019 = df[df$year=='2019',]
      #df1_2019$month = format(df1_2019$ARREST_DATE,'%m')
      data <- df1_2019
      return( data ) 
    }
    if ( "2020" %in% input$year){
      df1_2020 = df[df$year=='2020',]
      #df1_2020$month = format(df1_2020$ARREST_DATE,'%m')
      data <- df1_2020
      return( data ) 
    }
    if ( "2021" %in% input$year){
      df1_2021 = df[df$year=='2021',]
      #df1_2021$month = format(df1_2021$ARREST_DATE,'%m')
      data <- df1_2021
      return( data ) 
    }
 
  })
  
  output$ARPlot1 <- renderPlotly({
    data <- year_chosen() %>% group_by(ARREST_BORO,month)%>%
      summarise(count = n())
    
    ggplot(data, aes(x = month, y = count, group = ARREST_BORO, color = ARREST_BORO)) +
        geom_line()+
        scale_color_viridis(discrete = TRUE, option = "G") +
            #scale_x_date(breaks = "1 year", date_labels = "%b-%y") +
            labs(y = "Number of crimes Occurred", 
                 x = "month", 
                 title = paste("1. Overall NYC Arrests in", input$year)) +
            guides(color=guide_legend(title="Location")) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45),
                  axis.text.y = element_text(angle = 45),
                  plot.title = element_text(hjust = 5.5),panel.background = element_rect(fill = "white"))


    ggplotly(width = 750)
  })
  

  
  output$ARPlot2 <- renderPlotly({
    target <- c('Felony', 'Misdemeanor', 'Violation')
    data <- year_chosen() %>% 
      filter(LAW_CAT_CD %in% target) %>%
      group_by(LAW_CAT_CD,ARREST_BORO) %>%
      summarise(count = n())

    ggplot(data, aes(fill = LAW_CAT_CD, y = count, x = ARREST_BORO)) +
      geom_bar(position="stack", stat="identity") +
      #scale_color_viridis(discrete = TRUE, option = "G") +
      labs(y = "Number of crimes Occurred",
           x = "Location",
           title = paste("2. Level of Offense in Each Borough in", input$year)) +
      guides(fill=guide_legend(title="Level of Offense")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45),
            axis.text.y = element_text(angle = 45),
            plot.title = element_text(hjust = 5.5),panel.background = element_rect(fill = "white"))


    ggplotly(width = 750)
  })

  
  output$ARPlot3 <- renderPlotly({
    target <- c('Felony', 'Misdemeanor', 'Violation')
    data <- year_chosen() %>%
      filter(LAW_CAT_CD %in% target) %>%
      filter(PERP_RACE != 'AMERICAN INDIAN/ALASKAN NATIVE') %>%
      group_by(LAW_CAT_CD,PERP_RACE) %>%
      summarise(count = n())

    ggplot(data, aes(fill = LAW_CAT_CD, y = count, x = PERP_RACE)) +
      geom_bar(position="stack", stat="identity") +
      #scale_color_viridis(discrete = TRUE, option = "G") +
      labs(y = "Number of crimes Occurred",
           x = "Perpetrator’s Race",
           title = paste("3. Level of Offense in Each Race in", input$year)) +
      guides(fill=guide_legend(title="Level of Offense")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45),
            axis.text.y = element_text(angle = 45),
            plot.title = element_text(hjust = 5.5),panel.background = element_rect(fill = "white"))


    ggplotly(width = 750, height = 550)
  })
  
  
  
  
  
  
  
    
  
} 
shinyApp(ui = ui, server = shinyServer)