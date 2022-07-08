output$precip_bar <- renderGirafe({
  
  graph <- watershed_rain_data %>%
    filter(substr(Date, 1, 4) %in% input$precip_year) %>%
    pivot_longer(cols=names(watershed_rain_data)[-1], names_to="Watershed", values_to="Rainfall") %>%
    group_by(Watershed) %>%
    summarize(Rainfall=mean(Rainfall)) %>%
    ggplot(aes(x=Watershed, y=Rainfall)) +
    geom_col_interactive(aes(tooltip=paste0("Average Daily Rainfall: ", format(Rainfall, digits=3), " in."), data_id=Rainfall), fill="#99e6ff")+
    ylab("Average Daily Rainfall (in.)")+
    ggtitle("Average Daily Precipitation per Watershed")+
    theme_minimal(base_size=35)+
    theme(axis.text.x = element_text(angle = 15), plot.background  = element_rect(color="#523178", size=10))
  girafe(ggobj = graph, width_svg=26, height_svg=9)
  
}) #renderGirafe