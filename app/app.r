library(shiny)
library(tidyverse)
watershed_data <- read_csv("../Data/combined_data_clean.csv")
rainfall_data <- read_csv("../Data/CR_airport_rainfall.csv")

# Define UI ----
ui <- fluidPage(
  titlePanel(h2("Watershed Project")),
  
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("watershed",
                  label = "Select Watershed",
                  choices = c("Indian Creek", "Bear", "Blue", "Morgan", "Mud", "North Bear", "Otter", "Lime", "McLoud Run"),
                  selected = "Indian Creek"
        
      ),
      
      selectInput("var",
                  label = "Select Chemical",
                  choices = c("DO", "Temp", "pH", "Cond", "Turb", "TSS", "DRP", "Cl", "NO3_N", "SO4", "E_coli"),
                  selected = "NO3_N"
                  
      ),
      
      sliderInput("range",
                  label="Date Range",
                  min=min(data$Date), max=max(data$Date), value=c(min(data$Date), max(data$Date))
                  )
    ),
    
    mainPanel(
      plotOutput("graph")
    )
    
  )
)


# Define server logic ----
server <- function(input, output) {
  
  output$graph <- renderPlot({
    data <- switch(input$watershed,
                   "Indian Creek"=filter(watershed_data, Watershed=="Indian Creek") %>%
                     filter(Date < input$range[2] & Date > input$range[1]),
                   
                   "Bear"=filter(watershed_data, Watershed=="Bear") %>%
                     filter(Date < input$range[2] & Date > input$range[1]),
                            
                   "Blue"=filter(watershed_data, Watershed=="Blue") %>%
                     filter(Date < input$range[2] & Date > input$range[1]),
                            
                   "Mud"=filter(watershed_data, Watershed=="Mud") %>%
                     filter(Date < input$range[2] & Date > input$range[1]),
                            
                   "Morgan"=filter(watershed_data, Watershed=="Morgan") %>%
                     filter(Date < input$range[2] & Date > input$range[1]),
                            
                   "North Bear"=filter(watershed_data, Watershed=="North Bear") %>%
                     filter(Date < input$range[2] & Date > input$range[1]),
                            
                   "Otter"=filter(watershed_data, Watershed=="Otter") %>%
                     filter(Date < input$range[2] & Date > input$range[1]),
                            
                   "Lime"=filter(watershed_data, Watershed=="Lime") %>%
                     filter(Date < input$range[2] & Date > input$range[1]),
                            
                   "McLoud Run"=filter(watershed_data, Watershed=="McLoud Run") %>%
                     filter(Date < input$range[2] & Date > input$range[1])
    )
    
    plot(y=data[[input$var]], x=data$Date, xlab="Date", ylab=input$var)
  })
}


# Run the app ----
shinyApp(ui=ui, server=server)