output$DRP_bar <- renderGirafe({
  graph <- watershed_data %>%
    filter(substr(Date, 1, 4) %in% input$wqi_year) %>%
    group_by(Watershed) %>%
    mutate(P=0.3261*DRP) %>%
    summarize_at(c("P"), median, na.rm=T) %>%
    ggplot(aes(x=Watershed, y=P))+
    geom_col_interactive(aes(tooltip=paste0(Watershed, ": ", format(P, digits=3), " mg/L"), data_id=P), fill="#00cc00")+
    
    #threshold value for phosphorus
    geom_hline_interactive(aes(tooltip=.18, data_id=.18), yintercept = .18, color="red", size=2)+
    geom_text(aes(4,.18,label = "Threshold (Lower is Better)", vjust = 1.5), color="red", size=6)+
    
    ggtitle("Phosphorus Levels by Watershed")+
    xlab("")+
    ylab("Total Phosphorus")+
    theme_minimal(base_size = 25) +
    theme(axis.text.x = element_text(angle = 15), plot.background  = element_rect(color="#523178", size=7))
  
  girafe(ggobj = graph, width_svg=11, height_svg=5)
})