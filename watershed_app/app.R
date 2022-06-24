library(shiny)
library(shinyWidgets)
library(tidyverse)
library(raster)
library(leaflet)
library(broom)
library(jcolors)
library(fmsb)
library(rgdal)
library(lubridate)
library(ggiraph)
library(elementalist) # devtools::install_github("teunbrand/elementalist")
library(ggradar)

watershed_data <- read_csv("data/combined_data_clean3.csv")
rainfall_data <- read_csv("data/CR_airport_rainfall.csv")
watershed_rain_data <- read_csv("data/watershed_rain_data.csv")
watershed_shp <- shapefile("data/watershed_geo/watersheds.shp")
merged_watershed_shp <- shapefile("data/watershed_geo/merged_watersheds.shp")
sites <- shapefile("data/watershed_geo/sites.shp")

variables <- c("DO", "Temp", "pH", "Cond", "Turb", "TSS", "DRP", "Cl", "NO3_N", "SO4", "E_coli")
years <- c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
           "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")
watersheds <- c("Indian Creek", "Bear Creek", "Blue Creek", "Morgan Creek", "Mud Creek",
                "North Bear Creek", "Otter Creek", "Lime Creek")

#options(shiny.sanitize.errors = FALSE)

# Pages

bottomPanel <- fluidRow(
  tags$img(src="https://www.h-net.org/jobs/logo_view.php?id=59029&scale=0")
) #fluidRow




#
# Interactive Map
#
mapPage <- tabPanel(div(class="navTab", "Map"),
                     
                     sidebarLayout(fluid=T,
                                   
                                   sidebarPanel(
                                     
                                     # Map
                                     leafletOutput("map", height=500),
                                     
                                     helpText("Zoom in to see sampling sites. Select a watershed to see more information."),
                                     
                                     # Choose variable to display on map
                                     selectInput("map_var",
                                       label="Select Variable",
                                       choices=variables,
                                       selected="NO3_N"
                                       
                                     ), #selectInput
                                     
                                     # Choose date range for data on map
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
                                                         # Select year to display on graph
                                                         selectInput("overview_year",
                                                                     label="Select Year",
                                                                     choices=years,
                                                                     selected="2021",
                                                         ), #selectInput
                                                         
                                                         # Overview line graph
                                                         girafeOutput("overview_watersheds", height=250),
                                                         br(),
                                                         # Overview spider plot
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
                                                         # Choose years
                                                         pickerInput("map_years_year",
                                                                     label="Select Year(s)",
                                                                     choices=years,
                                                                     selected=c("2021", "2020", "2019"),
                                                                     multiple=T,
                                                                     options = list(`actions-box` = TRUE)
                                                         ), #pickerInput
                                                         # Years plot
                                                         girafeOutput("map_years_plot", height=350),
                                                         br(),
                                                         # Scatterplot
                                                         girafeOutput("map_change_plot", height=250),
                                                  ), #column
                                                  
                                                  column(6,
                                                         pickerInput("add_watershed_spider",
                                                                     label="Compare With Watershed(s)",
                                                                     choices=c(watersheds,"None"),
                                                                     selected="None",
                                                                     multiple=T,
                                                                     options = list(`actions-box` = TRUE)
                                                                     ), #pickerInput
                                                         # Spider plot
                                                         plotOutput("map_spider_plot", height=350),
                                                         br(),
                                                         # Histogram
                                                         girafeOutput("map_dist_plot", height=250)
                                                         ), #column
                                                  
                                                ) #fluidPage
                                                ) #tabPanel
                                       

                                     ) #navbarPage
                                     
                                     
                                     
                                   ) #mainPanel
                                   
                     ) #sidebarLayout
                     
) #tabPanel




#
# Precipitation Page
#
precipPage <- tabPanel(div(class="navTab", "Precipitation"),
                       
                       column(6,
                              leafletOutput("precip_map", height=650),
                              helpText("Zoom in to see sampling sites. Select a watershed to see more information."),
                              fluidRow(
                                column(6,
                                       # Choose years
                                       pickerInput("precip_year",
                                                   label="Select Year(s)",
                                                   choices=years,
                                                   selected=c("2021", "2020", "2019"),
                                                   multiple=T,
                                                   options = list(`actions-box` = TRUE)
                                       ) #pickerInput
                                       ), #column
                                column(6,
                                       selectInput("precip_var",
                                                   label="Select Variable",
                                                   choices=variables,
                                                   selected="NO3_N"
                                       ) #selectInput
                                       ) #column
                              ) #fluidRow
                              ),
                       column(6, style = "overflow-y:scroll;",
                              radioButtons("precip_interval",
                                           label="Select Time Interval",
                                           choiceNames=c("Days", "Weeks"),
                                           choiceValues=c("Date", "Week"),
                                           selected="Week"
                                           ),
                              plotOutput("precip_plot")
                              )
                       
) #tabPanel




#
# Water Quality Index
#
wqiPage <- tabPanel(div(class="navTab", "Water Quality Index"),
                    
                    column(4,
                           "Description of WQI goes here"
                    ), #column
                    column(8,
                           column(6,
                                  girafeOutput("DO_bar", height=250),
                                  br(),
                                  girafeOutput("NO3_N_bar", height=250),
                                  br(),
                                  girafeOutput("Turb_bar", height=250)
                                  ),
                           column(6,
                                  girafeOutput("E_coli_bar", height=250),
                                  br(),
                                  girafeOutput("DRP_bar", height=250)
                                  )
                    )
                    
) #tabPanel




#
# Table of all data
#
datatablePage <- tabPanel(div(class="navTab", "Data"),
                      
                      sidebarLayout(fluid=T,
                                    
                                    sidebarPanel(
                                      
                                      h3("Filters"),
                                      br(),
                                      
                                      pickerInput("table_watershed",
                                                  label="Select Watershed(s)",
                                                  choices=watersheds,
                                                  selected=watersheds,
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




#
# Structure
#
ui <- fluidPage(theme="shiny.css",
  
  navbarPage("Watershed Project", position="static-top",
             
             mapPage,
             
             precipPage,
             
             wqiPage,
             
             datatablePage
                      
             ), #navbarPage
  
) # fluidPage


# Server

server <- function(input, output, session) {
  
  #
  # Map Page- Overview Tab
  #
  
  output$overview_watersheds <- renderGirafe({
    
    graph <- watershed_data %>%
      filter(substr(Date, 1, 4) == input$overview_year) %>%
      group_by(Watershed, Date) %>%
      summarize(value = mean(eval(as.name(input$map_var)))) %>%
      ggplot(aes(x=Date, y=value, color=Watershed))+
      geom_line(size=1)+
      geom_point_interactive(aes(tooltip=value, data_id=value), size=2)+
      ylab(input$map_var)+
      ggtitle(paste0("Comparison of ", input$map_var, " in ", input$overview_year, " by watershed"))+
      theme_minimal(base_size = 20) +
      theme(plot.background  = element_rect(color="#523178", size=7))
    
    girafe(ggobj=graph, width_svg=11, height_svg=5, options = list(opts_sizing(rescale = TRUE, width = 1)))
    
  }) #renderPlot
  
  
  #Rendering spider plot for overview page
  output$overview_spider_plot <- renderPlot({
    
    max_values <- rep(quantile(watershed_data[[input$map_var]], na.rm=T, p=0.9), 8)
    min_values <- rep(min(watershed_data[[input$map_var]], na.rm=T), 8)
    
    
    data_temp <- watershed_data %>%
      filter(Date > input$map_date[1] & Date < input$map_date[2]) %>%
      group_by(Watershed) %>%
      summarize(mean=mean(eval(as.name(input$map_var)), na.rm=T))
    
    data <- as.data.frame(t(data_temp[,-1]))
    colnames(data) <- data_temp$Watershed
    
    data <- rbind(min_values, data)
    data <- rbind(max_values, data)
    
    radarchart(data,
               cglty = 1,       # Grid line type
               cglcol = "gray", # Grid line color
               cglwd = 1,       # Line width of the grid
               pcol = "blue",        # Color of the line
               plwd = 2,        # Width of the line
               plty = 1,
               title=paste0(input$map_var, " by watershed"))
    
  }) #renderPlot
  
  
  
  
  #
  # Map Page - Interactive Map
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
  
  
  
  
  #
  # Map Page - Plots Tab
  #
  
  # Plot comparing years for a given variable
  output$map_years_plot <- renderGirafe({
    
    graph <- watershed_data %>%
      mutate(year=substr(Date, 1, 4)) %>%
      mutate(day=as.Date(paste0("0000-", substr(Date, 6, 10)))) %>%
      filter(year %in% input$map_years_year) %>%
      filter(Watershed==input$map_shape_click$id) %>%
      group_by(Watershed, year, day) %>%
      summarize(avg=mean(eval(as.name(input$map_var)))) %>%
      ggplot(aes(x=day, y=avg, color=year))+
      geom_line(size=2)+
      geom_point_interactive(aes(tooltip=avg, data_id=avg), size=4)+
      xlab("Date")+
      ylab(input$map_var)+
      ggtitle(paste0("Comparison of ", input$map_var, " by year"))+
      labs(color="Year")+
      theme_minimal(base_size = 25) +
      theme(plot.background  = element_rect(color="#523178", size=7))
    
    girafe(ggobj=graph, width_svg=12, height_svg=7)
      
    
  }) #renderPlot
  
  
  
  # Spider Plot
  #Updating input
  observeEvent(input$map_shape_click, {
    
    updatePickerInput(session=session, "add_watershed_spider", label = NULL,
                      choices = c(watersheds[watersheds!=input$map_shape_click$id], "None"),
                      selected = "None")
    
  }) #observeEvent
  
  
  #Rendering spider plot for watershed page
  output$map_spider_plot <- renderPlot(bg="#BBBCBC", {
    
    watershed_data %>%
      dplyr::select(c(1, 5:15)) %>%
      filter(Temp < quantile(Temp, 0.9, na.rm=T)) %>%
      filter(pH < quantile(pH, 0.9, na.rm=T)) %>%
      filter(Cond < quantile(Cond, 0.9, na.rm=T)) %>%
      filter(Turb < quantile(Turb, 0.9, na.rm=T)) %>%
      filter(TSS < quantile(TSS, 0.9, na.rm=T)) %>%
      filter(DRP < quantile(DRP, 0.9, na.rm=T)) %>%
      filter(Cl < quantile(Cl, 0.9, na.rm=T)) %>%
      filter(NO3_N < quantile(NO3_N, 0.9, na.rm=T)) %>%
      filter(SO4 < quantile(SO4, 0.9, na.rm=T)) %>%
      filter(E_coli < quantile(E_coli, 0.9, na.rm=T)) %>%
      mutate_at(vars(-Watershed), scales::rescale) %>%
      group_by(Watershed) %>%
      summarise_at(-1, mean, na.rm=T) %>%
      filter(Watershed==input$map_shape_click$id | Watershed %in% input$add_watershed_spider) %>%
      ggradar(plot.extent.x.sf=1.1, plot.extent.y.sf=1.5, values.radar = "", group.line.width = 0.7, group.point.size = 3)+
      theme_minimal()+
      theme(plot.background  = element_rect(color="#523178", size=4), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_blank(), axis.ticks=element_blank())+
      scale_colour_discrete("Watershed")
    
  }) #renderPlot
  
  
  # Plot showing observations of variable over a given time
  output$map_change_plot <- renderGirafe({
    
    graph <- watershed_data %>%
      filter(Date < input$map_date[2] & Date > input$map_date[1]) %>%
      filter(Watershed==input$map_shape_click$id) %>%
      ggplot(aes(x=Date, y=eval(as.name(input$map_var))))+
      geom_point_interactive(aes(tooltip=eval(as.name(input$map_var)), data_id=eval(as.name(input$map_var))), size=4)+
      xlab("Date")+
      ylab(input$map_var)+
      ggtitle(paste0("Observed ", input$map_var))+
      theme_minimal(base_size = 20) +
      theme(plot.background  = element_rect(color="#523178", size=7))
    
    girafe(ggobj = graph, width_svg=11, height_svg=5)

  }) #renderPlot
  
  # Plot showing distribution of variable in a given time
  output$map_dist_plot <- renderGirafe({
    
    graph <- watershed_data %>%
      filter(Date < input$map_date[2] & Date > input$map_date[1]) %>%
      filter(Watershed==input$map_shape_click$id) %>%
      ggplot(aes(x=eval(as.name(input$map_var))))+
      geom_histogram_interactive(aes(tooltip=eval(as.name(input$map_var)), data_id=eval(as.name(input$map_var))), fill="#267326")+
      ylab("Count")+
      xlab(input$map_var)+
      ggtitle(paste0("Distribution of ", input$map_var))+
      theme_minimal(base_size = 20) +
      theme(plot.background  = element_rect(color="#523178", size=7))
    
    girafe(ggobj = graph, width_svg=11, height_svg=5)
    
  }) #renderPlot
  
  
  
  
  #
  # Precipitation Page
  #
  
  output$precip_map<- renderLeaflet({
    
    rain_data <- watershed_rain_data %>%
      filter(substr(Date, 1, 4) %in% input$precip_year)
    
    rain_data <- rain_data[,-1] %>%
      summarize_all(mean, na.rm=T) %>%
      t() %>%
      as.data.frame()
    rain_data$Watershed <- rownames(rain_data)
    
    p_by_watershed <- watershed_data %>%
      group_by(Watershed) %>%
      summarize(value=mean(NO3_N, na.rm=T))
    
    merged_watershed_shp$value <- rain_data$V1[match(merged_watershed_shp$Watershed, rain_data$Watershed)]
    
    # One polygon for watershed currently selected to be drawn over rest of map, shows users current selection
    p_selected_watershed <- subset(merged_watershed_shp, merged_watershed_shp$Watershed==input$precip_map_shape_click$id)
    
    # Color palette
    palette <- colorNumeric("Blues", merged_watershed_shp$value)
    
    # Leaflet map
    pmap <- leaflet(options = leafletOptions(zoomSnap = 0.25, 
                                            zoomDelta = 0.25)) %>%
      # View and bounds
      setView(center_long, center_lat, 10) %>%
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
      addPolygons(data=p_selected_watershed, color="white", opacity=1, fillOpacity=0, weight=5,highlightOptions = highlightOptions(color = "white", weight = 5,
                                                                                                                                 bringToFront = TRUE)) %>%
      # Add legend
      addLegend(position="topright", pal=palette, values=merged_watershed_shp$value, title="Precipitation (in)") %>%
      
      # Add markers and set visibility according to zoom level
      addMarkers(data=sites, label=sites$Site, icon=siteIcon, group="markers") %>%
      groupOptions("markers", zoomLevels=seq(10, 20, 0.25))
    
    pmap
    
  }) #renderLeaflet
  
  
  
  output$precip_plot <- renderPlot({
    
    if (is.null(input$precip_map_shape_click$id)) {
      wdata <- watershed_data %>%
        filter(substr(Date, 1, 4) %in% input$precip_year) %>%
        dplyr::select(Date, input$precip_var)
      rdata <- rainfall_data %>%
        filter(substr(Date, 1, 4) %in% input$precip_year)
    } else {
      wdata <- watershed_data %>%
        filter(Watershed==input$precip_map_shape_click$id) %>%
        filter(substr(Date, 1, 4) %in% input$precip_year) %>%
        dplyr::select(Date, input$precip_var)
      rdata <- watershed_rain_data %>%
        filter(substr(Date, 1, 4) %in% input$precip_year) %>%
        dplyr::select(Date, input$precip_map_shape_click$id)
    }
    
    data <- left_join(rdata, wdata, by="Date") %>%
      mutate(Year = substr(Date, 1, 4)) %>%
      mutate(Week=week(Date))
    year(data$Date) <- 0000
    names(data) <- c("Date", "Rain", "Value", "Year", "Week")
    data$Rain <- (data$Rain-min(data$Rain))/(max(data$Rain)-min(data$Rain))
    data$Value <- (data$Value-min(data$Value, na.rm=T))/(max(data$Value, na.rm=T)-min(data$Value, na.rm=T))
    data <- data %>%
      pivot_longer(cols=c("Rain", "Value"),
                   names_to="Type",
                   values_to="Value") %>%
      filter(Date < "0-08-15" & Date > "0-04-10")
    data$Type <- factor(data$Type, levels=c("Value", "Rain"))
    
    graph <- switch(input$precip_interval,
                    
      "Week" = data %>%
        group_by(Type, Week) %>%
        summarize_at(c("Value"), mean, na.rm=T) %>%
        ggplot(aes(x=Week, y=Value, fill=Type))+
        geom_area(position="identity", alpha=0.7)+
        xlab("Week")+
        ylab("")+
        scale_fill_manual(values=c("#339933", "#3366ff"))+
        theme_minimal() +
        theme(plot.background  = element_rect(color="#523178", size=4)),
      
      "Date" = data %>%
        group_by(Type, Date) %>%
        summarize_at(c("Value"), mean, na.rm=T) %>%
        ggplot(aes(x=Date, y=Value, fill=Type))+
        geom_col(alpha=0.7)+
        xlab("Date")+
        ylab("")+
        scale_fill_manual(values=c("#339933", "#3366ff"))+
        theme_minimal() +
        theme(plot.background  = element_rect(color="#523178", size=4))
    )
    
    graph
    
  }) #renderPlot
  
  
  
  #
  # Water Quality Index Page
  #
  
  # Dissolved Oxygen Graph
  output$DO_bar <- renderGirafe({
    graph <- watershed_data %>%
      group_by(Watershed) %>%
      summarize_at(c("DO"), median, na.rm=T) %>%
      ggplot(aes(x=Watershed, y=DO))+
      geom_col_interactive(aes(tooltip=DO, data_id=DO), fill="#00cc00")+
      geom_hline_interactive(aes(tooltip=5, data_id=5), yintercept = 5, color="red", size=2)+
      geom_text(aes(4,5,label = "Threshold (Higher is Better)", vjust = -1), color="red", size=6)+
      ggtitle("Dissolved Oxygen Levels by Watershed")+
      xlab("")+
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(angle = 15), plot.background  = element_rect(color="#523178", size=7))
    
    girafe(ggobj = graph, width_svg=11, height_svg=5)
  })
  
  #E. coli graph
  output$E_coli_bar <- renderGirafe({
    graph <- watershed_data %>%
      group_by(Watershed) %>%
      summarize_at(c("E_coli"), median, na.rm=T) %>%
      ggplot(aes(x=Watershed, y=E_coli))+
      geom_col_interactive(aes(tooltip=E_coli, data_id=E_coli), fill="#00cc00")+
      geom_hline_interactive(aes(tooltip=235, data_id=235), yintercept = 235, color="red", size=2)+
      geom_text(aes(4,235,label = "Threshold (Lower is Better)", vjust = -1), color="red", size=6)+
      ggtitle("E. coli Levels by Watershed")+
      xlab("")+
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(angle = 15), plot.background  = element_rect(color="#523178", size=7))
    
    girafe(ggobj = graph, width_svg=11, height_svg=5)
  })
  
  # Nitrate Graph
  output$NO3_N_bar <- renderGirafe({
    graph <- watershed_data %>%
      group_by(Watershed) %>%
      summarize_at(c("NO3_N"), median, na.rm=T) %>%
      ggplot(aes(x=Watershed, y=NO3_N))+
      geom_col_interactive(aes(tooltip=NO3_N, data_id=NO3_N), fill="#00cc00")+
      geom_hline_interactive(aes(tooltip=3.5, data_id=3.5), yintercept = 3.5, color="red", size=2)+
      geom_text(aes(4,3.5,label = "Threshold (Lower is Better)", vjust = -1), color="red", size=6)+
      ggtitle("Nitrate Levels by Watershed")+
      xlab("")+
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(angle = 15), plot.background  = element_rect(color="#523178", size=7))
    
    girafe(ggobj = graph, width_svg=11, height_svg=5)
  })
  
  # Phosphorus Graph
  output$DRP_bar <- renderGirafe({
    graph <- watershed_data %>%
      group_by(Watershed) %>%
      summarize_at(c("DRP"), median, na.rm=T) %>%
      ggplot(aes(x=Watershed, y=DRP))+
      geom_col_interactive(aes(tooltip=DRP, data_id=DRP), fill="#00cc00")+
      geom_hline_interactive(aes(tooltip=.18, data_id=.18), yintercept = .18, color="red", size=2)+
      geom_text(aes(4,.18,label = "Threshold (Lower is Better)", vjust = -1), color="red", size=6)+
      ggtitle("Phosphorus Levels by Watershed")+
      xlab("")+
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(angle = 15), plot.background  = element_rect(color="#523178", size=7))
    
    girafe(ggobj = graph, width_svg=11, height_svg=5)
  })
  
  # Turbidity Graph
  output$Turb_bar <- renderGirafe({
    graph <- watershed_data %>%
      group_by(Watershed) %>%
      summarize_at(c("Turb"), median, na.rm=T) %>%
      ggplot(aes(x=Watershed, y=Turb))+
      geom_col_interactive(aes(tooltip=Turb, data_id=Turb), fill="#00cc00")+
      geom_hline_interactive(aes(tooltip=25, data_id=25), yintercept = 25, color="red", size=2)+
      geom_text(aes(4,25,label = "Threshold (Lower is Better)", vjust = -1), color="red", size=6)+
      ggtitle("Turbidity by Watershed")+
      xlab("")+
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(angle = 15), plot.background  = element_rect(color="#523178", size=7))
    
    girafe(ggobj = graph, width_svg=11, height_svg=5)
  })
  

  
  #
  # Data table page
  #
  
  new_data <- reactive({
    watershed_data %>%
      filter(Watershed %in% input$table_watershed) %>%
      filter(Site %in% input$table_site) %>%
      filter(Date > input$table_date[1] & Date < input$table_date[2])
    
  }) #reactive
  
  output$data_table <- renderDataTable(new_data())

} #server


# Run app
shinyApp(ui=ui, server=server)