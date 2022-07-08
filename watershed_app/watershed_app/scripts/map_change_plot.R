output$map_change_plot <- renderGirafe({
  
  graph <- watershed_data %>%
    filter(Date < input$map_date[2] & Date > input$map_date[1]) %>%
    filter(Watershed==input$map_shape_click$id) %>%
    ggplot(aes(x=Date, y=eval(as.name(input$map_var))))+
    geom_point_interactive(aes(tooltip=eval(as.name(input$map_var)), data_id=eval(as.name(input$map_var))), size=6)+
    xlab("Date")+
    ylab(getLabel(input$map_var))+
    ggtitle(paste0("Observed ", getLabel(input$map_var)))+
    theme_minimal(base_size = 25) +
    theme(plot.background  = element_rect(color="#523178", size=7))
  
  girafe(ggobj = graph, width_svg=11, height_svg=5)
  
}) #renderPlot