output$precip_map<- renderLeaflet({
  
  #get rain data for years selected by user
  rain_data <- watershed_rain_data %>%
    filter(substr(Date, 1, 4) %in% input$precip_year)
  
  # get means of rainfall and transpose the dataframe
  rain_data <- rain_data[,-1] %>%
    summarize_all(mean, na.rm=T) %>%
    t() %>%
    as.data.frame()
  
  #get columns names
  rain_data$Watershed <- rownames(rain_data)
  
  #get values for each waterhsed on map
  p_by_watershed <- watershed_data %>%
    group_by(Watershed) %>%
    summarize(value=mean(NO3_N, na.rm=T))
  
  #add values to shapefile in the correct place
  merged_watershed_shp$value <- rain_data$V1[match(merged_watershed_shp$Watershed, rain_data$Watershed)]
  
  # One polygon for watershed currently selected to be drawn over rest of map, shows users current selection
  p_selected_watershed <- subset(merged_watershed_shp, merged_watershed_shp$Watershed==input$precip_map_shape_click$id)
  
  # Color palette
  #palette <- colorNumeric("Blues", merged_watershed_shp$value)
  
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
                fillColor = "#99e6ff",
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                label=paste0(merged_watershed_shp$Watershed, " Watershed"),
                layerId=~Watershed) %>%
    
    # Adding polygon for watershed that the user has currently selected
    addPolygons(data=p_selected_watershed, color="white", opacity=1, fillOpacity=0, weight=5,highlightOptions = highlightOptions(color = "white", weight = 5,
                                                                                                                                 bringToFront = TRUE)) %>%
    # Add legend
    #addLegend(position="topright", pal=palette, values=merged_watershed_shp$value, title="Precipitation (in)") %>%
    
    # Add markers and set visibility according to zoom level
    addMarkers(data=sites, label=sites$Site, icon=siteIcon, group="markers") %>%
    groupOptions("markers", zoomLevels=seq(10, 20, 0.25))
  
  pmap
  
}) #renderLeaflet