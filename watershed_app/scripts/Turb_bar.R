output$Turb_bar <- renderGirafe({
  graph <- watershed_data %>%
    filter(substr(Date, 1, 4) %in% input$wqi_year) %>%
    group_by(Watershed) %>%
    summarize_at(c("Turb"), median, na.rm=T) %>%
    ggplot(aes(x=Watershed, y=Turb))+
    geom_col_interactive(aes(tooltip=paste0(Watershed, ": ", Turb, " NTU"), data_id=Turb), fill="#00cc00")+
    
    #threshold value for turbidity
    geom_hline_interactive(aes(tooltip=25, data_id=25), yintercept = 25, color="red", size=2)+
    geom_text(aes(4,25,label = "Threshold (Lower is Better)", vjust = 1.5), color="red", size=6)+
    
    ggtitle("Turbidity by Watershed")+
    xlab("")+
    ylab("Turbidity")+
    theme_minimal(base_size = 25) +
    theme(axis.text.x = element_text(angle = 15), plot.background  = element_rect(color="#523178", size=7))
  
  girafe(ggobj = graph, width_svg=11, height_svg=5)
})