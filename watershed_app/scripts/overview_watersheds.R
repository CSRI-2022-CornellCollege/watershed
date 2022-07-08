output$overview_watersheds <- renderGirafe({
  
  graph <- watershed_data %>%
    filter(substr(Date, 1, 4) == input$overview_year) %>%
    group_by(Watershed, Date) %>%
    summarize(value = mean(eval(as.name(input$map_var)))) %>%
    ggplot(aes(x=Date, y=value, color=Watershed))+
    geom_line(size=2)+
    geom_point_interactive(aes(tooltip=value, data_id=value), size=4)+
    ylab(getLabel(input$map_var))+
    ggtitle(paste0("Comparison of ", getLabel(input$map_var), " in ", input$overview_year, " by Watershed"))+
    theme_minimal(base_size = 25) +
    theme(plot.background  = element_rect(color="#523178", size=7))
  
  girafe(ggobj=graph, width_svg=18, height_svg=5, options = list(opts_sizing(rescale = TRUE, width = 1)))
  
}) #renderPlot