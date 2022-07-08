output$E_coli_bar <- renderGirafe({
  graph <- watershed_data %>%
    filter(substr(Date, 1, 4) %in% input$wqi_year) %>%
    group_by(Watershed) %>%
    summarize_at(c("E_coli"), median, na.rm=T) %>%
    ggplot(aes(x=Watershed, y=E_coli))+
    geom_col_interactive(aes(tooltip=paste0(Watershed, ": ", E_coli, " CFU"), data_id=E_coli), fill="#00cc00")+
    
    #threshold value for E. coli
    geom_hline_interactive(aes(tooltip=235, data_id=235), yintercept = 235, color="red", size=2)+
    geom_text(aes(4,235,label = "Threshold (Lower is Better)", vjust = 1.5), color="red", size=6)+
    
    ggtitle("E. coli Levels by Watershed")+
    xlab("")+
    ylab("E. coli")+
    theme_minimal(base_size = 25) +
    theme(axis.text.x = element_text(angle = 15), plot.background  = element_rect(color="#523178", size=7))
  
  girafe(ggobj = graph, width_svg=11, height_svg=5)
})