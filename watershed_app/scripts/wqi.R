output$wqi <- renderGirafe({
  
  #a wqi for each watershed needs to be calculated, so a list of dataframes is
  #created where each element is a dataframe with the data from a single watershed
  data_vec <- list()
  for (i in 1:8){
    data <- watershed_data %>%
      mutate(P=DRP*0.3261) %>%
      filter(substr(Date, 1, 4) %in% input$wqi_year) %>%
      filter(Watershed==watersheds[i])
    data_vec[[i]] <- data
  }
  
  
  # F1 is calculated for each watershed in the list of dataframes
  F1 <- sapply(data_vec, getF1)
  
  # F2 is calculated for each watershed in the list of dataframes
  F2 <- sapply(data_vec, getF2)
  
  # F3 is calculated for each watershed in the list of dataframes
  F3 <- sapply(data_vec, getF3)
  
  # Index is calculated for each watershed in the list of dataframes
  index <- 100 - (sqrt(F1^2+F2^2+F3^2)/1.732)
  
  #column chart of WQI for each watershed
  graph <- data.frame(Watershed=watersheds, WQI=index) %>%
    ggplot(aes(y=reorder(Watershed, WQI), x=WQI))+
    geom_col_interactive(aes(tooltip=paste0(Watershed, ": ", format(WQI, digits=4)), data_id=WQI))+
    # coord_polar(theta="x", direction=1)+
    theme_minimal()+
    theme(plot.background  = element_rect(color="#523178", size=3.5))+
    xlab("")+
    ylab("")+
    ggtitle("Water Quality Index by Watershed")
  
  girafe(ggobj = graph)
  
})