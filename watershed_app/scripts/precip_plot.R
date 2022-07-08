output$precip_plot <- renderPlot({
  
  #if no shape is clicked on map show data for all watersheds, but if a shape
  #is clicked show data for only that watershed
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
  
  #join rain data and watershed data by date
  data <- left_join(rdata, wdata, by="Date") %>%
    mutate(Year = substr(Date, 1, 4)) %>%
    mutate(Week=week(Date))
  year(data$Date) <- 0000
  names(data) <- c("Date", "Rain", "Value", "Year", "Week")
  data$Rain <- (data$Rain-min(data$Rain))/(max(data$Rain)-min(data$Rain)) #scale rain data
  data$Value <- (data$Value-min(data$Value, na.rm=T))/(max(data$Value, na.rm=T)-min(data$Value, na.rm=T)) #scale watershed data
  data <- data %>%
    pivot_longer(cols=c("Rain", "Value"),
                 names_to="Type",
                 values_to="Value") %>%
    filter(Date < "0-08-15" & Date > "0-04-10")
  data$Type <- factor(data$Type, levels=c("Value", "Rain"))
  
  graph <- switch(input$precip_interval,
                  
                  #graph showing weekly data              
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
                  
                  #graph showing daily data
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