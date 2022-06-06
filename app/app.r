library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggpubr)
library(raster)
library(leaflet)
library(broom)
library(jcolors)
library(fmsb)

watershed_data <- read_csv("../Data/combined_data_clean2.csv")
rainfall_data <- read_csv("../Data/CR_airport_rainfall.csv")
watershed_shp <- shapefile("../Data/watershed_geo/watersheds.shp")
merged_watershed_shp <- shapefile("../Data/watershed_geo/merged_watersheds.shp")
sites <- shapefile("../Data/watershed_geo/sites.shp")

options(shiny.sanitize.errors = FALSE)

# Pages

bottomPanel <- fluidRow(
  tags$img(src="https://www.h-net.org/jobs/logo_view.php?id=59029&scale=0")
) #fluidRow

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
                                     
                                     navbarPage("", id="map_nav",
                                       
                                                
                                        # Overview of watershed data, some graphs and explanations are inlcuded
                                       tabPanel(div(class="navTab", "Overview"), value="overview_tab",
                                                fluidPage(
                                                  column(6,
                                                         h3(strong("About the Dashboard")),
                                                         br(),
                                                         p("Dr. Martin St. Clair, along with his students, has been collecting water samples from watersheds near Cedar Rapids, Iowa since 2002. These samples are tested for many different variables with an emphasis on different chemical concentrations in the water.", style="font-size: 20px;"),
                                                         br(),
                                                         p("The interactive map on the left displays information about the watersheds. You can select a watershed to see detailed information about that location. You may also change the variable of interest and the range of dates you would like to see. The graph on the right shows the change of a variable over the course of one year in all watersheds.", style="font-size: 20px;")
                                                         ), #column
                                                  column(6,
                                                         selectInput("overview_year",
                                                                     label="Select Year",
                                                                     choices=c("2002", "2003", "2004", "2005",
                                                                               "2006", "2007", "2008", "2009",
                                                                               "2010", "2011", "2012", "2013",
                                                                               "2014", "2015", "2016", "2017",
                                                                               "2018", "2019", "2020", "2021"),
                                                                     selected="2021",
                                                         ), #selectInput
                                                         plotOutput("overview_watersheds", height=250),
                                                         br(),
                                                         plotOutput("overview_spider_plot")
                                                         ) #column
                                                  
                                                ) #fluidPage
                                                
                                       ), #tabPanel
                                                
                                         
                                       # This page is for displaying graphs of a certain watershed when a user clicks on it in the interactive map       
                                       tabPanel(div(class="navTab", "Plots"), value="plots_tab",
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
                                                         plotOutput("map_spider_plot")
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
                                                         plotOutput("map_dist_plot", height=250)
                                                         ) #column
                                                  
                                                  
                                                ) #fluidPage
                                                ) #tabPanel
                                       

                                     ) #navbarPage
                                     
                                     
                                     
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
                                    
                                    div(class="table", mainPanel(
                                      
                                      dataTableOutput("data_table")
                                      
                                    ) #mainPanel
                                    ) #div
                                    
                      ) #sidebarLayout
                      
) #tabPanel




# Structure

ui <- fluidPage(theme="shiny.css",
  
  navbarPage("Watershed Project", position="static-top",
             
             mapPage,
             
              datatablePage
                      
             ), #navbarPage
  
) # fluidPage


# Server

server <- function(input, output, session) {
  
  #
  # Overview Page
  #
  
  output$overview_watersheds <- renderPlot({
    
    watershed_data %>%
      filter(substr(Date, 1, 4) == input$overview_year) %>%
      group_by(Watershed, Date) %>%
      summarize(value = mean(eval(as.name(input$map_var)))) %>%
      ggplot(aes(x=Date, y=value, color=Watershed))+
      geom_line()+
      geom_point()+
      ylab(input$map_var)+
      ggtitle(paste0("Comparison of ", input$map_var, " in ", input$overview_year, " by watershed"))+
      theme_minimal()
    
  }) #renderPlot
  
  
  
  
  #
  # Plots Page
  #
  
  # Header of plots page
  output$map_title <- renderText({ifelse(length(input$map_shape_click$id)>0, paste0(input$map_shape_click$id, " Watershed"), "Select a watershed to see visualizations")})
  
  # Getting max, min, and center long and lat for use in setting bounds of interactive map
  temp_data <- tidy(merged_watershed_shp)
  min_lat <- min(temp_data$lat)
  max_lat <- max(temp_data$lat)
  min_long <- min(temp_data$long)
  max_long <- max(temp_data$long)
  center_lat <- (min_lat+max_lat)/2
  center_long <- (min_long+max_long)/2
  
  # Icon for sampling site markers
  siteIcon <- makeIcon(
    iconUrl="https://resources.finalsite.net/images/f_auto,q_auto,t_image_size_2/v1620195943/brentwoodk12nyus/wivrch9gpckn4nvyconr/experiment-1295041_1280.png",
    iconWidth = 25, iconHeight = 24
  )
  
  
  # Change page on map_shape_click input
  observeEvent(input$map_shape_click, {
    
    updateNavbarPage(session=session,
                     inputId = "map_nav",
                     selected="overview_tab")
    req(input$map_shape_click$id)
    updateNavbarPage(session=session,
                     inputId = "map_nav",
                     selected="plots_tab")
      
  }) #observeEvent
  
  # Render leaflet map
  output$map <- renderLeaflet({
    
    # Get average values for variable in question
    by_watershed <- watershed_data %>%
      filter(Date < input$map_date[2] & Date > input$map_date[1]) %>%
      group_by(Watershed) %>%
      summarize(value=mean(eval(as.name(input$map_var)), na.rm=T))
    merged_watershed_shp$value <- by_watershed$value[match(merged_watershed_shp$Watershed, by_watershed$Watershed)]
    
    # One polygon for watershed currently selected to be drawn over rest of map, shows users current selection
    selected_watershed <- subset(merged_watershed_shp, merged_watershed_shp$Watershed==input$map_shape_click$id)
    
    # Color palette
    palette <- colorNumeric("RdYlGn", merged_watershed_shp$value, reverse=T)
    
    # Leaflet map
    map <- leaflet(options = leafletOptions(zoomSnap = 0.25, 
                                                         zoomDelta = 0.25)) %>%
      # View and bounds
      setView(center_long, center_lat, 9.5) %>%
      setMaxBounds(min_long-0.25, min_lat-0.25, max_long+0.25, max_lat+0.25) %>%
      
      # Using ESRI NatGeoWorldMap for background
      addProviderTiles(providers$Esri.NatGeoWorldMap, options=providerTileOptions(minZoom=8.5)) %>%
      
      # Adding lines for subwatersheds
      addPolygons(data=watershed_shp, color="black", fillColor="white", opacity=1, fillOpacity=0, weight=1, smoothFactor = 0.5) %>%
      
      # Adding polgyons for watersheds
      addPolygons(data=merged_watershed_shp, color = "#333333", weight = 1.5, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~palette(value),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label=paste0(merged_watershed_shp$Watershed, " Watershed"),
                  layerId=~Watershed) %>%
      
      # Adding polygon for watershed that the user has currently selected
      addPolygons(data=selected_watershed, color="white", opacity=1, fillOpacity=0, weight=5,highlightOptions = highlightOptions(color = "white", weight = 5,
                                                                                                                                 bringToFront = TRUE)) %>%
      
      # Add legend
      addLegend(position="topright", pal=palette, values=merged_watershed_shp$value, title=input$map_var) %>%
      
      # Add markers and set visibility according to zoom level
      addMarkers(data=sites, label=sites$Site, icon=siteIcon, group="markers") %>%
      groupOptions("markers", zoomLevels=seq(9.5, 20, 0.25))
    
    map
    
    
  }) #renderLeaflet
  
  # Plot comparing years for a given variable
  output$map_years_plot <- renderPlot({
    
    watershed_data %>%
      mutate(year=substr(Date, 1, 4)) %>%
      mutate(day=as.Date(paste0("0000-", substr(Date, 6, 10)))) %>%
      filter(year %in% input$map_years_year) %>%
      filter(Watershed==input$map_shape_click$id) %>%
      group_by(Watershed, year, day) %>%
      summarize(avg=mean(eval(as.name(input$map_var)))) %>%
      ggplot(aes(x=day, y=avg, color=year))+
      geom_line()+
      geom_point()+
      xlab("Date")+
      ylab(input$map_var)+
      ggtitle(paste0("Comparison of ", input$map_var, " by year"))+
      labs(color="Year")+
      theme_minimal()
      
    
  }) #renderPlot
  
  
  # Spider Plots
  
  #Setting min and max ranges for spider plot
  max_TSS <- quantile(watershed_data$TSS, p=0.9, na.rm=T)
  max_DRP <- quantile(watershed_data$DRP, p=0.9, na.rm=T)
  max_Cl <- quantile(watershed_data$Cl, p=0.9, na.rm=T)
  max_NO3_N <- quantile(watershed_data$NO3_N, p=0.9, na.rm=T)
  max_SO4 <- quantile(watershed_data$SO4, p=0.9, na.rm=T)
  max_E_coli <- quantile(watershed_data$E_coli, p=0.9, na.rm=T)
  
  max_values <- c(max_TSS, max_DRP, max_Cl, max_NO3_N, max_SO4, max_E_coli)
  
  min_values <- watershed_data %>%
    dplyr::select(10:15) %>%
    summarize_all(min, na.rm=T)
  
  #Safe levels for different variables to be displayed on the spider plot
  safe_levels <- as.data.frame(t(c(max_TSS, 0.1, 4, 10, max_SO4, max_E_coli)))
  colnames(safe_levels) <- c("TSS", "DRP", "Cl", "NO3_N", "SO4", "E_coli")
  
  #Rendering spider plot for watershed page
  output$map_spider_plot <- renderPlot({
    
    data <- watershed_data %>%
      filter(Watershed==input$map_shape_click$id) %>%
      filter(Date < input$map_date[2] & Date > input$map_date[1]) %>%
      dplyr::select(10:15) %>%
      summarize_all(mean, na.rm=T)
    
    data <- rbind(min_values, data)
    data <- rbind(max_values, data)
    data <- rbind(data, safe_levels)
    
    radarchart(data,
               cglty = 1,       # Grid line type
               cglcol = "gray", # Grid line color
               cglwd = 1,       # Line width of the grid
               pcol = c("blue", "red"),        # Color of the line
               plwd = 2,        # Width of the line
               plty = 1)
    
  }) #renderPlot
  
  
  #Rendering spider plot for overview page
  par(mar = c(4, 4, 0.1, 0.1))
  output$overview_spider_plot <- renderPlot({
    
    max_values <- rep(quantile(watershed_data[[input$map_var]], na.rm=T, p=0.9), 8)
    min_values <- rep(min(watershed_data[[input$map_var]], na.rm=T), 8)
    
    
    data_temp <- watershed_data %>%
      group_by(Watershed) %>%
      summarize(mean=mean(eval(as.name(input$map_var)), na.rm=T))

    data <- as.data.frame(t(data_temp[,-1]))
    colnames(data) <- data_temp$Watershed
    
    data <- rbind(min_values, data)
    data <- rbind(max_values, data)
    data <- rbind(data, rep(safe_levels[[input$map_var]], 8))

    radarchart(data,
               cglty = 1,       # Grid line type
               cglcol = "gray", # Grid line color
               cglwd = 1,       # Line width of the grid
               pcol = c("blue", "red"),        # Color of the line
               plwd = 2,        # Width of the line
               plty = 1)
    legend("topleft", legend=c("Actual Values", "Safe Levels"), col=c("Blue", "Red"), bty = "n", pch=20)

  }) #renderPlot
  
  
  # Precipitation Plot
  output$map_precip_plot <- renderPlot({
    
  })
  
  
  # Plot showing observations of variable over a given time
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
  
  # Plot showing distribution of variable in a given time
  output$map_dist_plot <- renderPlot({
    
    watershed_data %>%
      filter(Date < input$map_change_date[2] & Date > input$map_change_date[1]) %>%
      filter(Watershed==input$map_shape_click$id) %>%
      ggplot(aes(x=eval(as.name(input$map_var))))+
      geom_histogram()+
      xlab("Count")+
      ylab(input$map_var)+
      ggtitle(paste0("Distribution of ", input$map_var))+
      theme_minimal()
    
  }) #renderPlot
  
  
  
  
  #
  # Precipitation Page
  #
  
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