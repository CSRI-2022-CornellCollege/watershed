library(shiny)
library(shinyWidgets)
library(tidyverse)
watershed_data <- read_csv("../Data/combined_data_clean2.csv")
rainfall_data <- read_csv("../Data/CR_airport_rainfall.csv")

# Define UI ----
ui <- fluidPage(
  
  navbarPage("Watershed Project", position="static-top",
             
             navbarMenu("Visualizations",
                        
                        tabPanel("Distributions",
                                 
                                 sidebarLayout(fluid=T,
                                               
                                               sidebarPanel(
                                                 selectInput("dist_watershed",
                                                             label = "Select Watershed",
                                                             choices = c("Indian Creek", "Bear", 
                                                                         "Blue", "Morgan", "Mud", 
                                                                         "North Bear", "Otter", 
                                                                         "Lime"),
                                                             selected = "Lime"
                                                             
                                                 ) #selectInput
                                                 
                                               ), #sidebarPanel
                                               
                                               mainPanel(
                                                 plotOutput("dist")
                                               ) #mainPanel
                                               
                                 ) #sidebarLayout
                                 
                                 ), #tabPanel
                        
                        tabPanel("Multiple Year Change",
                                 
                                 sidebarLayout(fluid=T,
                                               
                                               sidebarPanel(
                                                 selectInput("multyear_watershed",
                                                             label = "Select Watershed",
                                                             choices = c("Indian Creek", "Bear", 
                                                                         "Blue", "Morgan", "Mud", 
                                                                         "North Bear", "Otter", 
                                                                         "Lime"),
                                                             selected = "Lime"
                                                             
                                                 ), #selectInput
                                                 
                                                 selectInput("multyear_var",
                                                             label = "Select Chemical",
                                                             choices = c("DO", "Temp", "pH", "Cond",
                                                                         "Turb", "TSS", "DRP", "Cl",
                                                                         "NO3_N", "SO4", "E_coli"),
                                                             selected = "NO3_N"
                                                             
                                                 ), #selectInput
                                                 
                                                 sliderInput("multyear_range",
                                                             label="Date Range",
                                                             min=min(watershed_data$Date),
                                                             max=max(watershed_data$Date),
                                                             value=c(min(watershed_data$Date),
                                                                     max(watershed_data$Date))
                                                 ) #sliderInput
                                                 
                                               ), #sidebarPanel
                                               
                                               mainPanel(
                                                 plotOutput("multyear_graph")
                                               ) #mainPanel
                                               
                                 ) #sidebarLayout 
                                 
                        ), #tabPanel
                        
                        tabPanel("One Year Change",
                                 
                                 sidebarLayout(fluid=T,
                                               
                                               sidebarPanel(
                                                 
                                                 pickerInput("yearchange_watershed",
                                                                    label="Select Watershed(s)",
                                                                    choices = c("Indian Creek", "Bear", 
                                                                                "Blue", "Morgan", "Mud", 
                                                                                "North Bear", "Otter", 
                                                                                "Lime"),
                                                                    selected=c("Indian Creek", "Bear", 
                                                                               "Blue", "Morgan", "Mud", 
                                                                               "North Bear", "Otter", 
                                                                               "Lime"),
                                                                    multiple=T,
                                                                    options = list(`actions-box` = TRUE)
                                                                    ), #checkboxGroupInput
                                                 
                                                 selectInput("yearchange_year",
                                                              label="Select Year",
                                                              choices=c("2002", "2003", "2004", "2005",
                                                                        "2006", "2007", "2008", "2009",
                                                                        "2010", "2011", "2012", "2013",
                                                                        "2014", "2015", "2016", "2017",
                                                                        "2018", "2019", "2020", "2021"),
                                                             selected="2021"
                                                              ), #selectInput
                                                 selectInput("yearchange_var",
                                                             label = "Select Chemical",
                                                             choices = c("DO", "Temp", "pH", "Cond",
                                                                         "Turb", "TSS", "DRP", "Cl",
                                                                         "NO3_N", "SO4", "E_coli"),
                                                             selected = "NO3_N"
                                                             
                                                             ), #selectInput
                                                 
                                               ), #sidebarPanel
                                               
                                               mainPanel(
                                                 plotOutput("yearchange_graph")
                                               ) #mainPanel
                                               
                                 ) #sidebarLayout

                                 ) #tabPanel
                        
                        
                        ), #navbarMenu
             
             navbarMenu("Data",
                        
                        tabPanel("Table",
                                 
                                 sidebarLayout(fluid=T,
                                               
                                               sidebarPanel(
                                                 
                                                 
                                                 
                                               ), #sidebarPanel
                                               
                                               mainPanel(
                                                 
                                                 dataTableOutput("alldata")
                                                 
                                               ) #mainPanel
                                               
                                               ) #sidebarLayout
                                 
                                 ) #tabPanel
                        
                        ) #navbarMenu
             
             ) #navbarPage
  
) # fluidPage


# Define server logic ----
server <- function(input, output) {
  
  output$multyear_graph <- renderPlot({
    data <- switch(input$multyear_watershed,
                   "Indian Creek"=filter(watershed_data, Watershed=="Indian Creek") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                   
                   "Bear"=filter(watershed_data, Watershed=="Bear") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                            
                   "Blue"=filter(watershed_data, Watershed=="Blue") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                            
                   "Mud"=filter(watershed_data, Watershed=="Mud") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                            
                   "Morgan"=filter(watershed_data, Watershed=="Morgan") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                            
                   "North Bear"=filter(watershed_data, Watershed=="North Bear") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                            
                   "Otter"=filter(watershed_data, Watershed=="Otter") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                            
                   "Lime"=filter(watershed_data, Watershed=="Lime") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),

    ) #switch
    
    ggplot()+
      geom_point(aes(x=data$Date, y=data[[input$multyear_var]]))+
      xlab("Date")+
      ylab(input$multyear_var)+
      labs(title="Example Plot")+
      theme_minimal()
  }) #renderPlot
  
  output$alldata <- renderDataTable(watershed_data)
  
  output$dist <- renderPlot({
    
    data <- switch(input$dist_watershed,
                   "Indian Creek"=filter(watershed_data, Watershed=="Indian Creek"),
                   "Bear"=filter(watershed_data, Watershed=="Bear"),
                   "Blue"=filter(watershed_data, Watershed=="Blue"),
                   "Mud"=filter(watershed_data, Watershed=="Mud"),
                   "Morgan"=filter(watershed_data, Watershed=="Morgan"),
                   "North Bear"=filter(watershed_data, Watershed=="North Bear"),
                   "Otter"=filter(watershed_data, Watershed=="Otter"),
                   "Lime"=filter(watershed_data, Watershed=="Lime")
                   ) #switch
    
    data[,-(1:4)] %>%
      gather(key="key", value="value") %>%
      ggplot(aes(x=value))+
      geom_histogram()+
      facet_wrap(~key, scales="free")
    
  }) #renderPlot
  
  output$yearchange_graph <- renderPlot({
    
    to_include <- input$yearchange_watershed
    upper_date <- paste0(input$yearchange_year, "-08-07")
    lower_date <- paste0(input$yearchange_year, "-05-01")
    line_data <- watershed_data %>%
      filter(Date > lower_date & Date < upper_date) %>%
      filter(Watershed %in% to_include) %>%
      group_by(Watershed, Date) %>%
      summarize(avg=mean(eval(as.name(input$yearchange_var))))
    
    ggplot(line_data, aes(x=Date, y=avg, color=Watershed))+
      geom_line()+
      xlab("Date")+
      ylab(input$yearchange_var)+
      ggtitle(paste0(input$yearchange_var, " Summer ", input$yearchange_year))+
      theme_minimal()
    
  }) #renderPlot
  
} #server


# Run the app ----
shinyApp(ui=ui, server=server)