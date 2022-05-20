library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggpubr)
watershed_data <- read_csv("../Data/combined_data_clean2.csv")
rainfall_data <- read_csv("../Data/CR_airport_rainfall.csv")


# Pages

# Distributions of variables in a given watershed
distPage <- tabPanel("Distributions",
                     
                     sidebarLayout(fluid=T,
                                   
                                   sidebarPanel(
                                     
                                     h3("Distributions"),
                                     br(),
                                     
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
                     
              ) #tabPanel

# Scatterplot of a variable in a given watershed and date range
multyearchangePage <- tabPanel("Multiple Year Change",
                               
                               sidebarLayout(fluid=T,
                                             
                                             sidebarPanel(
                                               
                                               h3("Multiple Year Change"),
                                               br(),
                                               
                                               pickerInput("multyear_watershed",
                                                           label = "Select Watershed",
                                                           choices = c("Indian Creek", "Bear", 
                                                                       "Blue", "Morgan", "Mud", 
                                                                       "North Bear", "Otter", 
                                                                       "Lime"),
                                                           selected = c("Indian Creek", "Bear", 
                                                                        "Blue", "Morgan", "Mud", 
                                                                        "North Bear", "Otter", 
                                                                        "Lime")
                                                           
                                               ), #pickerInput
                                               
                                               selectInput("multyear_var",
                                                           label = "Select Variable",
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
                               
                    ) #tabPanel

# Line graph of a variable over a given year in one or more watersheds
oneyearchangePage <- tabPanel("One Year Change",
                              
                              sidebarLayout(fluid=T,
                                            
                                            sidebarPanel(
                                              
                                              h3("One Year Change"),
                                              br(),
                                              
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
                                                          label = "Select Variable",
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

compareyearsPage <- tabPanel("Compare Years",
                              
                              sidebarLayout(fluid=T,
                                            
                                            sidebarPanel(
                                              
                                              h3("Compare Years"),
                                              br(),
                                              
                                              selectInput("compareyears_watershed",
                                                          label="Select Watershed",
                                                          choices = c("Indian Creek", "Bear", 
                                                                      "Blue", "Morgan", "Mud", 
                                                                      "North Bear", "Otter", 
                                                                      "Lime"),
                                                          selected="Lime"
                                              ), #checkboxGroupInput
                                              
                                              pickerInput("compareyears_year",
                                                          label="Select Year(s)",
                                                          choices=c("2002", "2003", "2004", "2005",
                                                                    "2006", "2007", "2008", "2009",
                                                                    "2010", "2011", "2012", "2013",
                                                                    "2014", "2015", "2016", "2017",
                                                                    "2018", "2019", "2020", "2021"),
                                                          selected=c("2021", "2020"),
                                                          multiple=T,
                                                          options = list(`actions-box` = TRUE)
                                              ), #pickerInput
                                              selectInput("compareyears_var",
                                                          label = "Select Variable",
                                                          choices = c("DO", "Temp", "pH", "Cond",
                                                                      "Turb", "TSS", "DRP", "Cl",
                                                                      "NO3_N", "SO4", "E_coli"),
                                                          selected = "NO3_N"
                                                          
                                              ), #selectInput
                                              
                                            ), #sidebarPanel
                                            
                                            mainPanel(
                                              plotOutput("compareyears_graph")
                                            ) #mainPanel
                                            
                              ) #sidebarLayout
                              
) #tabPanel


sitePage <- tabPanel("Compare Sites",
                              
                              sidebarLayout(fluid=T,
                                            
                                            sidebarPanel(
                                              
                                              h3("Compare Sites"),
                                              br(),
                                              
                                              selectInput("site_watershed",
                                                          label="Select Watershed(s)",
                                                          choices = c("Indian Creek", "Lime"),
                                                          selected="Indian Creek"
                                              ), #selectInput
                                              
                                              selectInput("site_year",
                                                          label="Select Year",
                                                          choices=c("2002", "2003", "2004", "2005",
                                                                    "2006", "2007", "2008", "2009",
                                                                    "2010", "2011", "2012", "2013",
                                                                    "2014", "2015", "2016", "2017",
                                                                    "2018", "2019", "2020", "2021"),
                                                          selected="2011"
                                              ), #selectInput
                                              
                                              pickerInput("site_site",
                                                          label="Select Site(s)",
                                                          choices=c("ICLM", "ICS", "ICThom", "Dry", "ICN"),
                                                          selected=c("ICLM", "ICS", "ICThom", "Dry", "ICN"),
                                                          multiple=T,
                                                          options = list(`actions-box` = TRUE)
                                                          ),

                                              selectInput("site_var",
                                                          label = "Select Variable",
                                                          choices = c("DO", "Temp", "pH", "Cond",
                                                                      "Turb", "TSS", "DRP", "Cl",
                                                                      "NO3_N", "SO4", "E_coli"),
                                                          selected = "NO3_N"
                                                          
                                              ), #selectInput
                                              
                                            ), #sidebarPanel
                                            
                                            mainPanel(
                                              plotOutput("site_graph")
                                            ) #mainPanel
                                            
                              ) #sidebarLayout
                              
) #tabPanel


precipPage <- tabPanel("Precipitation Graph",
                     
                     sidebarLayout(fluid=T,
                                   
                                   sidebarPanel(
                                     
                                     h3("Precipitation Graphs"),
                                     br(),
                                     
                                     selectInput("precip_watershed",
                                                 label = "Select Watershed",
                                                 choices = c("Indian Creek", "Bear", 
                                                             "Blue", "Morgan", "Mud", 
                                                             "North Bear", "Otter", 
                                                             "Lime"),
                                                 selected = "Lime"
                                                 
                                     ), #selectInput
                                     
                                     selectInput("precip_year",
                                                 label="Select Year",
                                                 choices=c("2002", "2003", "2004", "2005",
                                                           "2006", "2007", "2008", "2009",
                                                           "2010", "2011", "2012", "2013",
                                                           "2014", "2015", "2016", "2017",
                                                           "2018", "2019", "2020", "2021"),
                                                 selected="2011"
                                     ), #selectInput
                                     
                                     selectInput("precip_var",
                                                 label = "Select Variable",
                                                 choices = c("DO", "Temp", "pH", "Cond",
                                                             "Turb", "TSS", "DRP", "Cl",
                                                             "NO3_N", "SO4", "E_coli"),
                                                 selected = "NO3_N"
                                                 
                                     ) #selectInput

                                   ), #sidebarPanel
                                   
                                   mainPanel(
                                     plotOutput("precip_graph")
                                   ) #mainPanel
                                   
                     ) #sidebarLayout
                     
) #tabPanel





# Table of all data
datatablePage <- tabPanel("Table",
                      
                      sidebarLayout(fluid=T,
                                    
                                    sidebarPanel(
                                      
                                      h3("Filters"),
                                      br(),
                                      
                                      pickerInput("table_watershed",
                                                  label="Select Watershed(s)",
                                                  choices=c("Indian Creek", "Bear", 
                                                            "Blue", "Morgan", "Mud", 
                                                            "North Bear", "Otter", 
                                                            "Lime"),
                                                  selected=c("Indian Creek", "Bear", 
                                                             "Blue", "Morgan", "Mud", 
                                                             "North Bear", "Otter", 
                                                             "Lime"),
                                                  multiple=T,
                                                  options = list(`actions-box` = TRUE)
                                                  ), #pickerInput
                                      
                                      pickerInput("table_site",
                                                  label = "Select Site(s)",
                                                  choices=unique(watershed_data$Site),
                                                  selected=unique(watershed_data$Site),
                                                  multiple=T,
                                                  options=list(`actions-box` = TRUE)
                                                  ), #pickerInput
                                      
                                      sliderInput("table_date",
                                                  label="Date Range",
                                                  min=min(watershed_data$Date),
                                                  max=max(watershed_data$Date),
                                                  value=c(min(watershed_data$Date),
                                                          max(watershed_data$Date))
                                      ) #sliderInput
                                      
                                    ), #sidebarPanel
                                    
                                    mainPanel(
                                      
                                      dataTableOutput("data_table")
                                      
                                    ) #mainPanel
                                    
                      ) #sidebarLayout
                      
) #tabPanel




# Structure

ui <- fluidPage(
  
  navbarPage("Watershed Project", position="static-top",
             
             navbarMenu("Visualizations",
                        
                        distPage,
                        
                        multyearchangePage,
                        
                        oneyearchangePage,
                        
                        compareyearsPage,
                        
                        sitePage,
                        
                        precipPage
                        
                        
                        ), #navbarMenu
             
             navbarMenu("Data",
                        
                        datatablePage
                        
                        ) #navbarMenu
             
             ) #navbarPage
  
) # fluidPage


# Server

server <- function(input, output, session) {
  
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
      ggtitle(paste0("Plot of ", input$multyear_var, " from ", input$multyear_range[1], " to ", input$multyear_range[2]))+
      theme_minimal()
  }) #renderPlot
  
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
      xlab("")+
      ylab("Count")+
      facet_wrap(~key, scales="free")
    
  }) #renderPlot
  
  output$yearchange_graph <- renderPlot({
    
    upper_date <- paste0(input$yearchange_year, "-08-07")
    lower_date <- paste0(input$yearchange_year, "-05-01")
    line_data <- watershed_data %>%
      filter(Date > lower_date & Date < upper_date) %>%
      filter(Watershed %in% input$yearchange_watershed) %>%
      group_by(Watershed, Date) %>%
      summarize(avg=mean(eval(as.name(input$yearchange_var))))
    
    ggplot(line_data, aes(x=Date, y=avg, color=Watershed))+
      geom_line()+
      geom_point()+
      xlab("Date")+
      ylab(input$yearchange_var)+
      ggtitle(paste0(input$yearchange_var, " Summer ", input$yearchange_year))+
      theme_minimal()
    
  }) #renderPlot
  
  
  output$compareyears_graph <- renderPlot({
    
    line_data <- watershed_data %>%
      mutate(year=substr(Date, 1, 4)) %>%
      mutate(day=as.Date(paste0("0000-", substr(Date, 6, 10)))) %>%
      filter(year %in% input$compareyears_year) %>%
      filter(Watershed==input$compareyears_watershed) %>%
      group_by(Watershed, year, day) %>%
      summarize(avg=mean(eval(as.name(input$compareyears_var))))
    
    ggplot(data=line_data, aes(x=day, y=avg, color=year))+
      geom_line()+
      geom_point()+
      xlab("Date")+
      ylab(input$compareyears_var)+
      ggtitle(paste0("Comparison of ", input$compareyears_var, " by year"))+
      labs(color="Year")+
      theme_minimal()
    
  }) #renderPlot
  
  
  # For sitePage
  observeEvent(input$site_year, {
    valid_sites <- unique(
      filter(watershed_data, Watershed==input$site_watershed & grepl(input$site_year, Date))$Site
    ) #unique
    updatePickerInput(session=session,
                      inputId="site_site",
                      label="Select Site",
                      choices=valid_sites,
                      selected=valid_sites,
                      options = list(`actions-box` = TRUE)
    ) #updatePickerInput
  }) #observeEvent
  
  # For sitePage
  observeEvent(input$site_watershed, {
    valid_sites <- unique(
      filter(watershed_data, Watershed==input$site_watershed & grepl(input$site_year, Date))$Site
    ) #unique
    updatePickerInput(session=session,
                      inputId="site_site",
                      label="Select Site",
                      choices=valid_sites,
                      selected=valid_sites,
                      options = list(`actions-box` = TRUE)
    ) #updatePickerInput
  }) #observeEvent
  
  output$site_graph <- renderPlot({
    
    upper_date <- paste0(input$site_year, "-08-07")
    lower_date <- paste0(input$site_year, "-05-01")
    watershed_data %>%
      filter(Watershed==input$site_watershed) %>%
      filter(Date < upper_date & Date > lower_date) %>%
      filter(Site %in% input$site_site) %>%
      group_by(Site, Date) %>%
      summarize(avg=mean(eval(as.name(input$site_var)))) %>%
      
      ggplot(aes(x=Date, y=avg, color=Site))+
      geom_line()+
      geom_point()+
      xlab("Date")+
      ylab(input$site_var)+
      ggtitle(paste0("Comparison of ", input$site_var, " by site"))+
      theme_minimal()
    
  }) #renderPlot
  
  
  output$precip_graph <- renderPlot({
    
    upper_date <- paste0(input$precip_year, "-08-07")
    lower_date <- paste0(input$precip_year, "-05-01")
    
    plot1 <- watershed_data %>%
      filter(Date > lower_date & Date < upper_date) %>%
      filter(Watershed==input$precip_watershed) %>%
      group_by(Date) %>%
      summarize(avg=mean(eval(as.name(input$precip_var)))) %>%
      ggplot(aes(x=Date, y=avg))+
      geom_line()+
      geom_point()+
      ylab(input$precip_var)+
      theme_minimal()
    
    plot2 <- rainfall_data %>%
      filter(Date > lower_date & Date < upper_date) %>%
      ggplot(aes(x=Date, y=Estimate))+
      geom_line()+
      geom_point()+
      ylab("Rainfall Estimate")+
      theme_minimal()
    
    ggarrange(plot1, plot2, ncol=1, nrow=2)
    
  }) #renderPlot
  
  
  new_data <- reactive({
    watershed_data %>%
      filter(Watershed %in% input$table_watershed) %>%
      filter(Site %in% input$table_site) %>%
      filter(Date > input$table_date[1] & Date < input$table_date[2])
    
  }) #reactive


  output$data_table <- renderDataTable(new_data())
  
  
  
  # ensure app closes properly
  session$onSessionEnded(function() {
    stopApp()
  })
  
} #server


# Run app
shinyApp(ui=ui, server=server)