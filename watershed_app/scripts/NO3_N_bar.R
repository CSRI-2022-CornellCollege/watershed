output$NO3_N_bar <- renderGirafe({
  graph <- watershed_data %>%
    filter(substr(Date, 1, 4) %in% input$wqi_year) %>%
    group_by(Watershed) %>%
    summarize_at(c("NO3_N"), median, na.rm=T) %>%
    ggplot(aes(x=Watershed, y=NO3_N))+
    geom_col_interactive(aes(tooltip=paste0(Watershed, ": ", format(NO3_N, digits=4), " mg NO3-N/L"), data_id=NO3_N), fill="#00cc00")+
    
    #threshold value for nitrogen
    geom_hline_interactive(aes(tooltip=3.5, data_id=3.5), yintercept = 3.5, color="red", size=2)+
    geom_text(aes(4,3.5,label = "Threshold (Lower is Better)", vjust = 1.5), color="red", size=6)+
    
    ggtitle("Nitrate Levels by Watershed")+
    xlab("")+
    ylab("Total Nitrogen")+
    theme_minimal(base_size = 25) +
    theme(axis.text.x = element_text(angle = 15), plot.background  = element_rect(color="#523178", size=7))
  
  girafe(ggobj = graph, width_svg=11, height_svg=5)
})