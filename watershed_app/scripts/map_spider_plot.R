output$map_spider_plot <- renderPlot(bg="#BBBCBC", {
  
  #filter by 90th percentile
  watershed_data %>%
    dplyr::select(c(1, 5:15)) %>%
    filter(Temp < quantile(Temp, 0.9, na.rm=T)) %>%
    filter(pH < quantile(pH, 0.9, na.rm=T)) %>%
    filter(Cond < quantile(Cond, 0.9, na.rm=T)) %>%
    filter(Turb < quantile(Turb, 0.9, na.rm=T)) %>%
    filter(TSS < quantile(TSS, 0.9, na.rm=T)) %>%
    filter(DRP < quantile(DRP, 0.9, na.rm=T)) %>%
    filter(Cl < quantile(Cl, 0.9, na.rm=T)) %>%
    filter(NO3_N < quantile(NO3_N, 0.9, na.rm=T)) %>%
    filter(SO4 < quantile(SO4, 0.9, na.rm=T)) %>%
    filter(E_coli < quantile(E_coli, 0.9, na.rm=T)) %>%
    mutate_at(vars(-Watershed), scales::rescale) %>%
    group_by(Watershed) %>%
    summarise_at(-1, mean, na.rm=T) %>%
    filter(Watershed==input$map_shape_click$id | Watershed %in% input$add_watershed_spider) %>%
    ggradar(values.radar = "", group.line.width = 0.7, group.point.size = 3, gridline.mid.colour="grey")+
    theme_minimal()+
    theme(plot.background  = element_rect(color="#523178", size=3.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text=element_blank(), axis.ticks=element_blank())+
    scale_colour_discrete("Watershed")
  
}) #renderPlot