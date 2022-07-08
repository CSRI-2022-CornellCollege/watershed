output$DO_bar <- renderGirafe({
  graph <- watershed_data %>%
    filter(substr(Date, 1, 4) %in% input$wqi_year) %>%
    group_by(Watershed) %>%
    summarize_at(c("DO"), median, na.rm=T) %>%
    ggplot(aes(x=Watershed, y=DO))+
    geom_col_interactive(aes(tooltip=paste0(Watershed, ": ", format(DO, digits=4), " mg O2/L"), data_id=DO), fill="#00cc00")+
    
    #threshold value for DO
    geom_hline_interactive(aes(tooltip=5, data_id=5), yintercept = 5, color="red", size=2)+
    geom_text(aes(4,5,label = "Threshold (Higher is Better)", vjust = -1), color="red", size=6)+
    
    ggtitle("Dissolved Oxygen Levels by Watershed")+
    xlab("")+
    ylab("Dissolved Oxygen")+
    theme_minimal(base_size = 25) +
    theme(axis.text.x = element_text(angle = 15), plot.background  = element_rect(color="#523178", size=7))
  
  girafe(ggobj = graph, width_svg=11, height_svg=5)
})