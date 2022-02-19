#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(dplyr)
library(dbplyr)
library(dbplot)
library(DBI)
library(shiny)



shinyServer(function(input, output) {
  #Manipulate Data
  hate_data <- read.csv("../data/NYPD_Hate_Crimes.csv")
  covid_data <- read.csv("../data/cases-by-day.csv")
  tmp <- data.frame("CASE_COUNT"=rep(0,13),"BX_CASE_COUNT"=rep(0,13),"BK_CASE_COUNT"=rep(0,13),"MN_CASE_COUNT"=rep(0,13),"QN_CASE_COUNT"=rep(0,13),"SI_CASE_COUNT"=rep(0,13),"MM"=rep(0,13),"YY"=c(rep("2019",12),"2020"),"YY_MM"=c("2019/1", "2019/2", "2019/3", "2019/4", "2019/5", "2019/6", "2019/7", "2019/8", "2019/9", "2019/10", "2019/11", "2019/12", "2020/1"))
  covid_data <- rbind(tmp, covid_data[c("CASE_COUNT","BX_CASE_COUNT","BK_CASE_COUNT","MN_CASE_COUNT","QN_CASE_COUNT","SI_CASE_COUNT","MM","YY","YY_MM")])
  colnames(covid_data) <- c("CASE_COUNT", "BRONX", "KINGS","MANHATTAN","QUEENS","STATEN_ISLAND","MM","YY","YY_MM")
  covid_data <- covid_data %>%
    mutate(YY_MM = case_when(YY_MM =="2019/1"~"2019/01", YY_MM =="2019/2"~"2019/02",YY_MM =="2019/3"~"2019/03",YY_MM =="2019/4"~"2019/04",YY_MM =="2019/5"~"2019/05",YY_MM =="2019/6"~"2019/06",YY_MM =="2019/7"~"2019/07",YY_MM =="2019/8"~"2019/08",YY_MM =="2019/9"~"2019/09",YY_MM =="2019/10"~"2019/10",YY_MM =="2019/11"~"2019/11",YY_MM =="2019/12"~"2019/12",YY_MM =="2020/1"~"2020/01", YY_MM =="2020/2"~"2020/02",YY_MM =="2020/3"~"2020/03",YY_MM =="2020/4"~"2020/04",YY_MM =="2020/5"~"2020/05",YY_MM =="2020/6"~"2020/06",YY_MM =="2020/7"~"2020/07",YY_MM =="2020/8"~"2020/08",YY_MM =="2020/9"~"2020/09",YY_MM =="2020/10"~"2020/10",YY_MM =="2020/11"~"2020/11",YY_MM =="2020/12"~"2020/12",YY_MM =="2021/1"~"2021/01", YY_MM =="2021/2"~"2021/02",YY_MM =="2021/3"~"2021/03",YY_MM =="2021/4"~"2021/04",YY_MM =="2021/5"~"2021/05",YY_MM =="2021/6"~"2021/06",YY_MM =="2021/7"~"2021/07",YY_MM =="2021/8"~"2021/08",YY_MM =="2021/9"~"2021/09",YY_MM =="2021/10"~"2021/10",YY_MM =="2021/11"~"2021/11",YY_MM =="2021/12"~"2021/12"))
  covid_data$YY_MM <- factor(covid_data$YY_MM)
  
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
  
  covid_data_m <- covid_data %>%
    group_by(YY_MM) %>%
    summarise(sum(CASE_COUNT))
  colnames(covid_data_m) <- c("YY_MM", "cases")
  
  anti_data_m <- mod_data %>%
    group_by(YY_MM, Bias.Motive.Description) %>%
    summarise(count = n())
  
  anti_data_y <- mod_data %>%
    group_by(Complaint.Year.Number, Bias.Motive.Description) %>%
    summarise(count = n())
  
  covid_data_m_l<- covid_data %>%
    group_by(YY_MM) %>%
    summarise(sum(MANHATTAN),sum(BRONX),sum(QUEENS),sum(KINGS), sum(STATEN_ISLAND))
  data_m_loc <- mod_data[mod_data$Bias.Motive.Description=="ANTI ASIAN",] %>%
    group_by(YY_MM, County) %>%
    summarise(count=n())

  #Output Graph
    output$Plot1 <- renderPlot({
      if(input$option1 == 1) { 
        ggplot(data=anti_data_y)+
          geom_line(mapping = aes(x=Complaint.Year.Number,  y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
          labs(x="Year", y="Number of Crimes")+
          theme_classic() +
          theme(axis.text.x = element_text(face="bold", size=14),axis.text.y = element_text(face="bold", size=11))}
      else if(input$option1 == 2){
        ggplot(data=anti_data_m)+
          geom_line(mapping = aes(x=YY_MM,  y=count, group = Bias.Motive.Description, col=Bias.Motive.Description))+
          labs(xlabel="YY/MM", y="Number of Crimes")+
          theme_classic() +
          theme(axis.text.x = element_text(face="bold", size=11, angle = 90), axis.text.y = element_text(face="bold", size=11))
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
          geom_line(data=covid_data_m_l, mapping = aes(x=YY_MM,  y=`sum(MANHATTAN)`/1000,group=1),col="light blue")+
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
})
