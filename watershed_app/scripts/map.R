output$map <- renderLeaflet({
  
  #get average values for variable in question
  by_watershed <- watershed_data %>%
    filter(Date < input$map_date[2] & Date > input$map_date[1]) %>%
    group_by(Watershed) %>%
    summarize(value=mean(eval(as.name(input$map_var)), na.rm=T))
  
  #add values to shapefile in correct places
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