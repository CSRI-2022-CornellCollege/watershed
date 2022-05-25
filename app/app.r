library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggpubr)
library(raster)
library(leaflet)
library(broom)
library(jcolors)

watershed_data <- read_csv("../Data/combined_data_clean2.csv")
rainfall_data <- read_csv("../Data/CR_airport_rainfall.csv")
watershed_shp <- shapefile("../Data/watershed_geo/watersheds.shp")
merged_watershed_shp <- shapefile("../Data/watershed_geo/merged_watersheds.shp")
sites <- shapefile("../Data/watershed_geo/sites.shp")

options(shiny.sanitize.errors = FALSE)

# Pages

# Interactive Map
mapPage <- tabPanel(div(class="navTab", "Map"),
                     
                     sidebarLayout(fluid=T,
                                   
                                   sidebarPanel(

                                     leafletOutput("map"),
                                     
                                     helpText("Zoom in to see sampling sites."),
                                     
                                     selectInput("map_var",
                                       label="Select Variable",
                                       choices=c("DO", "Temp", "pH", "Cond",
                                                 "Turb", "TSS", "DRP", "Cl",
                                                 "NO3_N", "SO4", "E_coli"),
                                       selected="NO3_N"
                                       
                                     ), #selectInput
                                     
                                     sliderInput("map_date",
                                                 label="Date Range",
                                                 min=min(watershed_data$Date),
                                                 max=max(watershed_data$Date),
                                                 value=c(min(watershed_data$Date),
                                                         max(watershed_data$Date))
                                     ) #sliderInput
                                     

                                   ), #sidebarPanel
                                   
                                   mainPanel(
                                     
                                     navbarPage("",
                                                
                                       tabPanel(div(class="navTab", "Plots"),
                                                fluidPage(
                                                  h2(strong(textOutput("map_title"), style="text-align: center;")),
                                                  br(),
                                                  column(6,
                                                         pickerInput("map_years_year",
                                                                     label="Select Year(s)",
                                                                     choices=c("2002", "2003", "2004", "2005",
                                                                               "2006", "2007", "2008", "2009",
                                                                               "2010", "2011", "2012", "2013",
                                                                               "2014", "2015", "2016", "2017",
                                                                               "2018", "2019", "2020", "2021"),
                                                                     selected=c("2021", "2020", "2019"),
                                                                     multiple=T,
                                                                     options = list(`actions-box` = TRUE)
                                                         ), #pickerInput
                                                         plotOutput("map_years_plot", height=250),
                                                         br(),
                                                         selectInput("map_sites_year",
                                                                     label="Select Year",
                                                                     choices=c("2002", "2003", "2004", "2005",
                                                                               "2006", "2007", "2008", "2009",
                                                                               "2010", "2011", "2012", "2013",
                                                                               "2014", "2015", "2016", "2017",
                                                                               "2018", "2019", "2020", "2021"),
                                                                     selected="2021",
                                                         ), #pickerInput
                                                         plotOutput("map_sites_plot", height=250)
                                                         ), #column
                                                  column(6,
                                                         sliderInput("map_change_date",
                                                                     label="Date Range",
                                                                     min=min(watershed_data$Date),
                                                                     max=max(watershed_data$Date),
                                                                     value=c(min(watershed_data$Date),
                                                                             max(watershed_data$Date))
                                                         ), #sliderInput
                                                         plotOutput("map_change_plot", height=250),
                                                         sliderInput("map_dist_date",
                                                                     label="Date Range",
                                                                     min=min(watershed_data$Date),
                                                                     max=max(watershed_data$Date),
                                                                     value=c(min(watershed_data$Date),
                                                                             max(watershed_data$Date))
                                                         ), #sliderInput
                                                         plotOutput("map_dist_plot", height=250)
                                                         ) #column
                                                  
                                                  
                                                ) #fluidPage
                                                ), #tabPanel
                                       
                                       tabPanel(div(class="navTab", "Overview"),
                                                h3("information about watershed goes here")
                                                ) #tabPanel
                                     )
                                     
                                     
                                     
                                   ) #mainPanel
                                   
                     ) #sidebarLayout
                     
) #tabPanel

# Distributions of variables in a given watershed
distPage <- tabPanel("Distributions",
                     
                     sidebarLayout(fluid=T,
                                   
                                   sidebarPanel(
                                     
                                     h3("Distributions"),
                                     br(),
                                     
                                     selectInput("dist_watershed",
                                                 label = "Select Watershed",
                                                 choices = c("Indian Creek", "Bear Creek", 
                                                             "Blue Creek", "Morgan Creek  Creek", "Mud Creek", 
                                                             "North Bear Creek", "Otter Creek", 
                                                             "Lime Creek"),
                                                 selected = "Lime Creek"
                                                 
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
                                                           choices = c("Indian Creek", "Bear Creek", 
                                                                       "Blue Creek", "Morgan Creek", "Mud Creek", 
                                                                       "North Bear Creek", "Otter Creek", 
                                                                       "Lime Creek"),
                                                           selected = c("Indian Creek", "Bear Creek", 
                                                                        "Blue Creek", "Morgan Creek", "Mud Creek", 
                                                                        "North Bear Creek", "Otter Creek", 
                                                                        "Lime Creek")
                                                           
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
                                                          choices = c("Indian Creek", "Bear Creek", 
                                                                      "Blue Creek", "Morgan Creek", "Mud Creek", 
                                                                      "North Bear Creek", "Otter Creek", 
                                                                      "Lime Creek"),
                                                          selected=c("Indian Creek", "Bear Creek", 
                                                                     "Blue Creek", "Morgan Creek", "Mud Creek", 
                                                                     "North Bear Creek", "Otter Creek", 
                                                                     "Lime Creek"),
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



sitePage <- tabPanel("Compare Sites",
                              
                              sidebarLayout(fluid=T,
                                            
                                            sidebarPanel(
                                              
                                              h3("Compare Sites"),
                                              br(),
                                              
                                              selectInput("site_watershed",
                                                          label="Select Watershed(s)",
                                                          choices = c("Indian Creek", "Lime Creek"),
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
                                                 choices = c("Indian Creek", "Bear Creek", 
                                                             "Blue Creek", "Morgan Creek", "Mud Creek", 
                                                             "North Bear Creek", "Otter Creek", 
                                                             "Lime Creek"),
                                                 selected = "Lime Creek"
                                                 
                                     ), #selectInput
                                     
                                     sliderInput("precip_year",
                                                 label="Select Year",
                                                 min=2002,
                                                 max=2021,
                                                 value=c(2002, 2021)
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
datatablePage <- tabPanel(div(class="navTab", "Data"),
                      
                      sidebarLayout(fluid=T,
                                    
                                    sidebarPanel(
                                      
                                      h3("Filters"),
                                      br(),
                                      
                                      pickerInput("table_watershed",
                                                  label="Select Watershed(s)",
                                                  choices=c("Indian Creek", "Bear Creek", 
                                                            "Blue Creek", "Morgan Creek", "Mud Creek", 
                                                            "North Bear Creek", "Otter Creek", 
                                                            "Lime Creek"),
                                                  selected=c("Indian Creek", "Bear Creek", 
                                                             "Blue Creek", "Morgan Creek", "Mud Creek", 
                                                             "North Bear Creek", "Otter Creek", 
                                                             "Lime Creek"),
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

ui <- fluidPage(theme="shiny.css",
  
  navbarPage("Watershed Project", position="static-top",
             
             mapPage,
             
             
             # navbarMenu(div(class="navTab", "Visualizations"),
             #            
             #            distPage,
             #            
             #            multyearchangePage,
             #            
             #            oneyearchangePage,
             #            
             #            sitePage,
             #            
             #            precipPage
             #            
             #            
             #            ), #navbarMenu
             
              datatablePage
                      
             ) #navbarPage
  
) # fluidPage


# Server

server <- function(input, output, session) {
  
  output$map_title <- renderText({ifelse(length(input$map_shape_click$id)>0, paste0(input$map_shape_click$id, " Watershed"), "Select a watershed to see visualizations")})
  
  watershed_shp_data <- merged_watershed_shp
  
  temp_data <- tidy(watershed_shp_data)
  min_lat <- min(temp_data$lat)
  max_lat <- max(temp_data$lat)
  min_long <- min(temp_data$long)
  max_long <- max(temp_data$long)
  center_lat <- (min_lat+max_lat)/2
  center_long <- (min_long+max_long)/2
  
  siteIcon <- makeIcon(
    iconUrl="https://resources.finalsite.net/images/f_auto,q_auto,t_image_size_2/v1620195943/brentwoodk12nyus/wivrch9gpckn4nvyconr/experiment-1295041_1280.png",
    iconWidth = 25, iconHeight = 24
  )
  
  
  output$map <- renderLeaflet({
    
    by_watershed <- watershed_data %>%
      filter(Date < input$map_date[2] & Date > input$map_date[1]) %>%
      group_by(Watershed) %>%
      summarize(value=mean(eval(as.name(input$map_var)), na.rm=T))
    watershed_shp_data$value <- by_watershed$value[match(watershed_shp_data$Watershed, by_watershed$Watershed)]
    
    palette <- colorNumeric("RdYlGn", watershed_shp_data$value, reverse=T)
    
    map <- leaflet(options = leafletOptions(zoomSnap = 0.25, 
                                                         zoomDelta = 0.25)) %>%
      setView(center_long, center_lat, 9.5) %>%
      setMaxBounds(min_long-0.25, min_lat-0.25, max_long+0.25, max_lat+0.25) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, options=providerTileOptions(minZoom=8.5)) %>%
      addPolygons(data=watershed_shp, color="black", fillColor="white", opacity=1, fillOpacity=0, weight=1, smoothFactor = 0.5) %>%
      addPolygons(data=watershed_shp_data, color = "#444444", weight = 1.5, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~palette(value),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label=paste0(watershed_shp_data$Watershed, " Watershed"),
                  layerId=~Watershed) %>%
      addLegend(position="topright", pal=palette, values=watershed_shp_data$value, title=input$map_var) %>%
      addMarkers(data=sites, label=sites$Site, icon=siteIcon, group="markers") %>%
      groupOptions("markers", zoomLevels=seq(9.5, 20, 0.25))
    
    map
    
    
  }) #renderLeaflet
  
  
  output$map_years_plot <- renderPlot({
    
    line_data <- watershed_data %>%
      mutate(year=substr(Date, 1, 4)) %>%
      mutate(day=as.Date(paste0("0000-", substr(Date, 6, 10)))) %>%
      filter(year %in% input$map_years_year) %>%
      filter(Watershed==input$map_shape_click$id) %>%
      group_by(Watershed, year, day) %>%
      summarize(avg=mean(eval(as.name(input$map_var))))
    
    ggplot(data=line_data, aes(x=day, y=avg, color=year))+
      geom_line()+
      geom_point()+
      xlab("Date")+
      ylab(input$map_var)+
      ggtitle(paste0("Comparison of ", input$map_var, " by year"))+
      labs(color="Year")+
      theme_minimal()
      
    
  }) #renderPlot
  
  
  output$map_sites_plot <- renderPlot({
    
    watershed_data %>%
      filter(substr(Date, 1, 4) %in% input$map_sites_year) %>%
      filter(Watershed==input$map_shape_click$id) %>%
      ggplot(aes(y=eval(as.name(input$map_var)), x=Date, color=Site))+
      geom_line()+
      geom_point()+
      xlab("Date")+
      ylab(input$map_var)+
      ggtitle(paste0("Comparison of ", input$map_var, " by site"))+
      theme_minimal()
    
  }) #renderPlot
  
  
  output$map_change_plot <- renderPlot({
    
    watershed_data %>%
      filter(Date < input$map_change_date[2] & Date > input$map_change_date[1]) %>%
      filter(Watershed==input$map_shape_click$id) %>%
      ggplot(aes(x=Date, y=eval(as.name(input$map_var))))+
      geom_point()+
      xlab("Date")+
      ylab(input$map_var)+
      ggtitle(paste0("Observed ", input$map_var))+
      theme_minimal()

  }) #renderPlot
  
  
  output$map_dist_plot <- renderPlot({
    
    watershed_data %>%
      filter(Date < input$map_dist_date[2] & Date > input$map_dist_date[1]) %>%
      filter(Watershed==input$map_shape_click$id) %>%
      ggplot(aes(x=eval(as.name(input$map_var))))+
      geom_histogram()+
      xlab("Count")+
      ylab(input$map_var)+
      ggtitle(paste0("Distribution of ", input$map_var))+
      theme_minimal()
    
  }) #renderPlot
  
  
  output$multyear_graph <- renderPlot({
    data <- switch(input$multyear_watershed,
                   "Indian Creek"=filter(watershed_data, Watershed=="Indian Creek") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                   
                   "Bear Creek"=filter(watershed_data, Watershed=="Bear Creek") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                            
                   "Blue Creek"=filter(watershed_data, Watershed=="Blue Creek") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                            
                   "Mud Creek"=filter(watershed_data, Watershed=="Mud Creek") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                            
                   "Morgan Creek"=filter(watershed_data, Watershed=="Morgan Creek") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                            
                   "North Bear Creek"=filter(watershed_data, Watershed=="North Bear Creek") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                            
                   "Otter Creek"=filter(watershed_data, Watershed=="Otter Creek") %>%
                     filter(Date < input$multyear_range[2] & Date > input$multyear_range[1]),
                            
                   "Lime Creek"=filter(watershed_data, Watershed=="Lime Creek") %>%
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
                   "Bear Creek"=filter(watershed_data, Watershed=="Bear Creek"),
                   "Blue Creek"=filter(watershed_data, Watershed=="Blue Creek"),
                   "Mud Creek"=filter(watershed_data, Watershed=="Mud Creek"),
                   "Morgan Creek"=filter(watershed_data, Watershed=="Morgan Creek"),
                   "North Bear Creek"=filter(watershed_data, Watershed=="North Bear Creek"),
                   "Otter Creek"=filter(watershed_data, Watershed=="Otter Creek"),
                   "Lime Creek"=filter(watershed_data, Watershed=="Lime Creek")
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
    
    plot1 <- watershed_data %>%
      mutate(Year = as.numeric(substr(Date, 1, 4))) %>%
      filter(Year > input$precip_year[1] & Year < input$precip_year[2]) %>%
      mutate(Year = as.Date(paste0(substr(Date, 1, 4), "-01-01"))) %>%
      filter(Watershed==input$precip_watershed) %>%
      group_by(Year) %>%
      summarize(avg=mean(eval(as.name(input$precip_var)))) %>%
      ggplot(aes(x=Year, y=avg))+
      geom_line()+
      geom_point()+
      ylab(input$precip_var)+
      theme_minimal()
    
    plot2 <- rainfall_data %>%
      mutate(Year = as.numeric(substr(Date, 1, 4))) %>%
      filter(Year > input$precip_year[1] & Year < input$precip_year[2]) %>%
      mutate(Year = as.Date(paste0(substr(Date, 1, 4), "-01-01"))) %>%
      group_by(Year) %>%
      summarize(avg=mean(Estimate)) %>%
      ggplot(aes(x=Year, y=avg))+
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