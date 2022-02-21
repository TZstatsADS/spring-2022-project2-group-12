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

getwd()
setwd('Downloads')
df <- read.csv("NYPD_Complaint_Data_Current__Year_To_Date_.csv")
#df$CMPLNT_FR_DT = as.Date(df$CMPLNT_FR_DT,format = "%m/%d/%Y")
#df$year = format(df$CMPLNT_FR_DT,'%Y')
unique(df$year)
ui <- navbarPage(
  tabPanel(
    "Complaints",
    sidebarLayout(
      sidebarPanel(
        
        
        
        
        
        
        
        
        selectInput(inputId = "year",
                    label = "Choose an year", 
                    choices = c("2019","2020","2021")),
        selectInput(inputId = "period",
                    label = "Choose a period", 
                    choices = c("overall","covid")),
        
        
      ),
      
      mainPanel(
        plotlyOutput("Plot1"),
        plotlyOutput("Plot2"),
        position = "right"
      )
    )
  )
)


shinyServer <- function(input, output) {
  
  df <- read.csv("NYPD_Complaint_Data_Current__Year_To_Date_.csv")
  
  
  
  
  df$CMPLNT_FR_DT = as.Date(df$CMPLNT_FR_DT,format = "%m/%d/%Y")
  df$year = format(df$CMPLNT_FR_DT,'%Y')
  df1_2019 = df[df$year=='2019',]
  df1_2019$CMPLNT_FR_DT = as.Date(df1_2019$CMPLNT_FR_DT,format = "%m/%d/%Y")
  df1_2019$month = format(df1_2019$CMPLNT_FR_DT,'%m')
  df1_2019_overall <- df1_2019%>% group_by(LAW_CAT_CD,month)%>%
    summarise(count = n())
  
  df1_2019_covid <- df1_2019%>% group_by(LAW_CAT_CD,month)%>%
    summarise(count = n())
  
  output$Plot1 <- renderPlotly({
    if(input$year=='2019'){
      if(input$period=='overall'){
      df1_2019 = df[df$year=='2019',]
      df1_2019$CMPLNT_FR_DT = as.Date(df1_2019$CMPLNT_FR_DT,format = "%m/%d/%Y")
      df1_2019$month = format(df1_2019$CMPLNT_FR_DT,'%m')
      df1_2019_overall <- df1_2019%>% group_by(LAW_CAT_CD,month)%>%
        summarise(count = n())
      ggplot(df1_2019_overall, aes(x = month, y = count, group = LAW_CAT_CD, color = LAW_CAT_CD)) +
        geom_line()+
        scale_color_viridis(discrete = TRUE, option = "G") +
        #scale_x_date(breaks = "1 year", date_labels = "%b-%y") +
        labs(y = "Number of crimes Occurred", x = "month", title = "Overall NYC Complaints in 2019") +
        guides(color=guide_legend(title="Location")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45),
              axis.text.y = element_text(angle = 45),
              plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
      
      ggplotly(width = 1000)
      } 
    }
    else if(input$year=='2020'){
      if(input$period=='overall'){
      df$CMPLNT_FR_DT = as.Date(df$CMPLNT_FR_DT,format = "%m/%d/%Y")
      df$year = format(df$CMPLNT_FR_DT,'%Y')
      df1_2020 = df[df$year=='2020',]
      df1_2020$CMPLNT_FR_DT = as.Date(df1_2020$CMPLNT_FR_DT,format = "%m/%d/%Y")
      df1_2020$month = format(df1_2020$CMPLNT_FR_DT,'%m')
      df1_2020_overall <- df1_2020%>% group_by(LAW_CAT_CD,month)%>%
        summarise(count = n())
      ggplot(df1_2020_overall, aes(x = month, y = count, group = LAW_CAT_CD, color = LAW_CAT_CD)) +
        geom_line()+
        scale_color_viridis(discrete = TRUE, option = "G") +
        #scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
        labs(y = "Number of crimes Occurred", x = "month", title = " Overall NYC Complaints in 2020d") +
        guides(color=guide_legend(title="Location")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45),
              axis.text.y = element_text(angle = 45),
              plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
      
      ggplotly(width = 1000)
      }
    }
    
    else if(input$year=='2021'){
      if(input$period=='overall'){
      df$CMPLNT_FR_DT = as.Date(df$CMPLNT_FR_DT,format = "%m/%d/%Y")
      df$year = format(df$CMPLNT_FR_DT,'%Y')
      df1_2021 = df[df$year=='2021',]
      df1_2021$CMPLNT_FR_DT = as.Date(df1_2021$CMPLNT_FR_DT,format = "%m/%d/%Y")
      df1_2021$month = format(df1_2021$CMPLNT_FR_DT,'%m')
      df1_2021_overall <- df1_2021%>% group_by(LAW_CAT_CD,month)%>%
        summarise(count = n())
      
      df1_2021_covid <- df1_2021%>% filter(CMPLNT_FR_DT >= "2020-03-01") %>% group_by(LAW_CAT_CD,month)%>%
        summarise(count = n())
      
      ggplot(df1_2021_overall, aes(x = month, y = count, group = LAW_CAT_CD, color = LAW_CAT_CD)) +
        geom_line()+
        scale_color_viridis(discrete = TRUE, option = "G") +
        #scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
        labs(y = "Number of crimes Occurred", x = "month", title = "Overall NYC Complaints in 2021") +
        guides(color=guide_legend(title="Location")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45),
              axis.text.y = element_text(angle = 45),
              plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
      
      ggplotly(width = 1000)
      }
    }
    
  })
  
  output$Plot2 <- renderPlotly({
    if(input$year=='2019'){
      if(input$period=='covid'){
        df1_2019 = df[df$year=='2019',]
        df1_2019$CMPLNT_FR_DT = as.Date(df1_2019$CMPLNT_FR_DT,format = "%m/%d/%Y")
        df1_2019$month = format(df1_2019$CMPLNT_FR_DT,'%m')
        df1_2019_covid <- df1_2019%>% group_by(LAW_CAT_CD,month)%>%
          summarise(count = n())
        ggplot(df1_2019_overall, aes(x = month, y = count, group = LAW_CAT_CD, color = LAW_CAT_CD)) +
          geom_line()+
          scale_color_viridis(discrete = TRUE, option = "G") +
          #scale_x_date(breaks = "1 year", date_labels = "%b-%y") +
          labs(y = "Number of crimes Occurred", x = "month", title = "NYC Complaints during Covid in 2019") +
          guides(color=guide_legend(title="Location")) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45),
                axis.text.y = element_text(angle = 45),
                plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
        
        ggplotly(width = 1000)
      } 
    }
    else if(input$year=='2020'){
      if(input$period=='covid'){
        df$CMPLNT_FR_DT = as.Date(df$CMPLNT_FR_DT,format = "%m/%d/%Y")
        df$year = format(df$CMPLNT_FR_DT,'%Y')
        df1_2020 = df[df$year=='2020',]
        df1_2020 = df1_2020[df1_2020$CMPLNT_FR_DT>="2020-03-01",]
        df1_2020$CMPLNT_FR_DT = as.Date(df1_2020$CMPLNT_FR_DT,format = "%m/%d/%Y")
        df1_2020$month = format(df1_2020$CMPLNT_FR_DT,'%m')
        df1_2020_covid <- df1_2020 %>% group_by(LAW_CAT_CD,month)%>%
          summarise(count = n())
        ggplot(df1_2020_covid, aes(x = month, y = count, group = LAW_CAT_CD, color = LAW_CAT_CD)) +
          geom_line()+
          scale_color_viridis(discrete = TRUE, option = "G") +
          #scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
          labs(y = "Number of crimes Occurred", x = "Date", title = "NYC Complaints during Covid in 2020") +
          guides(color=guide_legend(title="Location")) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45),
                axis.text.y = element_text(angle = 45),
                plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
        
        ggplotly(width = 1000)
      }
    }
    
    else if(input$year=='2021'){
      if(input$period=='covid'){
        df$CMPLNT_FR_DT = as.Date(df$CMPLNT_FR_DT,format = "%m/%d/%Y")
        df$year = format(df$CMPLNT_FR_DT,'%Y')
        df1_2021 = df[df$year=='2021',]
        df1_2021 = df1_2021[df1_2021$CMPLNT_FR_DT>="2020-03-01",]
        df1_2021$CMPLNT_FR_DT = as.Date(df1_2021$CMPLNT_FR_DT,format = "%m/%d/%Y")
        df1_2021$month = format(df1_2021$CMPLNT_FR_DT,'%m')
        
        
        df1_2021_covid <- df1_2021%>% filter(CMPLNT_FR_DT >= "2020-03-01") %>% group_by(LAW_CAT_CD,month)%>%
          summarise(count = n())
        
        
        ggplot(df1_2021_covid, aes(x = month, y = count, group = LAW_CAT_CD, color = LAW_CAT_CD)) +
          geom_line()+
          scale_color_viridis(discrete = TRUE, option = "G") +
          #scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
          labs(y = "Number of crimes Occurred", x = "Date", title = "NYC Complaints During Covid in 2021") +
          guides(color=guide_legend(title="Location")) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45),
                axis.text.y = element_text(angle = 45),
                plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"))
        
        ggplotly(width = 1000)
      }
    }
    
  })
  
} 
shinyApp(ui = ui, server = shinyServer)