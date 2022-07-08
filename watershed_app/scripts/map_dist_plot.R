output$map_dist_plot <- renderGirafe({
  
  graph <- watershed_data %>%
    filter(Date < input$map_date[2] & Date > input$map_date[1]) %>%
    filter(Watershed==input$map_shape_click$id) %>%
    ggplot(aes(x=eval(as.name(input$map_var))))+
    geom_histogram_interactive(aes(tooltip=eval(as.name(input$map_var)), data_id=eval(as.name(input$map_var))), fill="#267326")+
    ylab("Count")+
    xlab(getLabel(input$map_var))+
    ggtitle(paste0("Distribution of ", getLabel(input$map_var)))+
    theme_minimal(base_size = 25) +
    theme(plot.background  = element_rect(color="#523178", size=7))
  
  girafe(ggobj = graph, width_svg=11, height_svg=5)
  
}) #renderPlot