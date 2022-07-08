output$map_years_plot <- renderGirafe({
  
  graph <- watershed_data %>%
    mutate(year=substr(Date, 1, 4)) %>%
    mutate(day=as.Date(paste0("0000-", substr(Date, 6, 10)))) %>%
    filter(year %in% input$map_years_year) %>%
    filter(Watershed==input$map_shape_click$id) %>%
    group_by(Watershed, year, day) %>%
    summarize(avg=mean(eval(as.name(input$map_var)))) %>%
    ggplot(aes(x=day, y=avg, color=year))+
    geom_line(size=3)+
    geom_point_interactive(aes(tooltip=avg, data_id=avg), size=6)+
    xlab("Date")+
    ylab(getLabel(input$map_var))+
    ggtitle(paste0("Comparison of ", getLabel(input$map_var), " by year"))+
    labs(color="Year")+
    theme_minimal(base_size = 30) +
    theme(plot.background  = element_rect(color="#523178", size=7))
  
  girafe(ggobj=graph, width_svg=12, height_svg=7)
  
  
}) #renderPlot