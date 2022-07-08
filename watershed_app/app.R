# Loading Libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(raster)
library(leaflet)
library(broom)
library(jcolors)
library(fmsb)
library(rgdal)
library(lubridate)
library(ggiraph)
library(elementalist) # devtools::install_github("teunbrand/elementalist")
library(ggradar)
library(shinyBS)

# Loading Data
watershed_data <- read_csv("data/combined_data_clean4.csv")
rainfall_data <- read_csv("data/CR_airport_rainfall.csv")
watershed_rain_data <- read_csv("data/watershed_rain_data.csv")
watershed_shp <- shapefile("data/watershed_geo/watersheds.shp")
merged_watershed_shp <- shapefile("data/watershed_geo/merged_watersheds.shp")
sites <- shapefile("data/watershed_geo/sites.shp")

# Creating lists for select inputs
variables <- c("Dissolved Oxygen (mg/L O2)"="DO", "Water Temperature (Degrees Celsius)"="Temp", "Acidity"="pH", "Conductivity (S/cm)"="Cond", "Turbidity (NTU)"="Turb", "Total Suspended Solids (mg/L"="TSS", "Dissolved Reactive Phosphorus (mg/L PO4)"="DRP", "Chloride (mg/L Cl)"="Cl", "Nitrate (mg/L NO3-N)"="NO3_N", "Sulfate (mg/L SO4)"="SO4", "E. coli (CFU/100 mL)"="E_coli")
years <- c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
           "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")
watersheds <- c("Indian Creek", "Bear Creek", "Blue Creek", "Morgan Creek", "Mud Creek",
                "North Bear Creek", "Otter Creek", "Lime Creek")


# Pages



#
# Interactive Map
#
mapPage <- tabPanel(div(class="navTab", "Map"),
                     
                     sidebarLayout(fluid=T,
                                   
                                   sidebarPanel(
                                     
                                     # Map
                                     leafletOutput("map", height=500),
                                     
                                     helpText("Zoom in to see sampling sites. Select a watershed to see more information."),
                                     
                                     # Choose variable to display on map
                                     selectInput("map_var",
                                       label="Select Variable",
                                       choices=variables,
                                       selected="NO3_N"
                                       
                                     ), #selectInput
                                     
                                     column(1),
                                     column(10,
                                            # Choose date range for data on map
                                            sliderInput("map_date", width="100%",
                                                        label="Date Range",
                                                        min=min(watershed_data$Date),
                                                        max=max(watershed_data$Date),
                                                        value=c(min(watershed_data$Date),
                                                                max(watershed_data$Date))
                                            ), #sliderInput
                                     
                                     ), #column
                                     column(1),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     

                                   ), #sidebarPanel
                                   
                                   mainPanel(
                                     
                                     navbarPage("", id="map_nav",
                                       
                                                
                                        # Overview of watershed data, some graphs and explanations are inlcuded
                                       tabPanel(div(class="navTab", "Overview"), value="overview_tab",
                                                fluidPage(
                                                  column(6,
                                                         h3(strong("About the Dashboard")),
                                                         br(),
                                                         p("A watershed is an area of land over which all water drains to a particular body of water, such as a creek or stream. The interactive map on the left displays information about the watersheds. This dashboard aims to visualize data about eight watersheds along the Cedar River. You can select a watershed to see detailed information about that location. You may also change the variable of interest and the range of dates you would like to see. Hover over graphs to see additional information.", style="font-size: 20px;"),
                                                         br()
                                                         ), #column
                                                  column(6,
                                                         # Overview spider plot
                                                         plotOutput("overview_spider_plot"),
                                                         bsPopover(id="overview_spider_plot", placement="left", title="About the Spider Plot", content="This spider plot shows means of the variable of interest across all watersheds. Points further from the center of the spider plot correspond to larger values. The middle dashed line represents the median value of the variable being displayed.")
                                                         ) #column
                                                  
                                                ), #fluidPage
                                                fluidRow(
                                                  column(10,
                                                         # Overview line graph
                                                         girafeOutput("overview_watersheds", height=290),
                                                         bsPopover(id="overview_watersheds", placement="top", title="About this Line Graph", content="This graph shows how a variable changes over the course of a summer in each watershed.")
                                                         ),
                                                  column(2,
                                                         # Select year to display on graph
                                                         selectInput("overview_year",
                                                                     label="Select Year",
                                                                     choices=years,
                                                                     selected="2021",
                                                         ), #selectInput
                                                         )
                                                ) #fluidRow
                                                
                                       ), #tabPanel
                                                
                                         
                                       # This page is for displaying graphs of a certain watershed when a user clicks on it in the interactive map       
                                       tabPanel(div(class="navTab", "Plots"), value="plots_tab",
                                                fluidPage(
                                                  h2(strong(textOutput("map_title"), style="text-align: left;")),
                                                  br(),
                                                  column(6,
                                                         # Choose years
                                                         pickerInput("map_years_year",
                                                                     label="Select Year(s)",
                                                                     choices=years,
                                                                     selected=c("2021", "2020", "2019"),
                                                                     multiple=T,
                                                                     options = list(`actions-box` = TRUE)
                                                         ), #pickerInput
                                                         # Years plot
                                                         girafeOutput("map_years_plot", height=350),
                                                         bsPopover(id="map_years_plot", placement="top", title="About this Line Graph", content="This graph compares trends of the variable of interest across years."),
                                                         br(),
                                                         # Scatterplot
                                                         girafeOutput("map_change_plot", height=250),
                                                         bsPopover(id="map_change_plot", placement="top", title="About this Scatterplot", content="This scatterplot shows observations over time.")
                                                  ), #column
                                                  
                                                  column(6,
                                                         pickerInput("add_watershed_spider",
                                                                     label="Compare With Watershed(s)",
                                                                     choices=c(watersheds,"None"),
                                                                     selected="None",
                                                                     multiple=T,
                                                                     options = list(`actions-box` = TRUE)
                                                                     ), #pickerInput
                                                         # Spider plot
                                                         plotOutput("map_spider_plot", height=350),
                                                         bsPopover(id="map_spider_plot", placement="top", title="About this Spider Plot", content="This spider plot shows a breakdown of the means of different variables in a given watershed. These variable are all scaled to be between 0 and 1. This allows variables of differing scales to be compared. The middle dashed line represents a value of 0.5"),
                                                         br(),
                                                         # Histogram
                                                         girafeOutput("map_dist_plot", height=250),
                                                         bsPopover(id="map_dist_plot", placement="top", title="About this Histogram", content="This histogram shows the distribution of the variable of interest.")
                                                         ), #column
                                                  
                                                ) #fluidPage
                                                ) #tabPanel
                                       

                                     ) #navbarPage
                                     
                                     
                                     
                                   ) #mainPanel
                                   
                     ) #sidebarLayout
                     
) #tabPanel




#
# Precipitation Page
#
precipPage <- tabPanel(div(class="navTab", "Precipitation"),
                       
                       column(6,
                              leafletOutput("precip_map", height=650),
                              helpText("Zoom in to see sampling sites. Select a watershed to see more information."),
                              fluidRow(
                                column(6,
                                       # Choose years
                                       pickerInput("precip_year",
                                                   label="Select Year(s)",
                                                   choices=years,
                                                   selected="2021",
                                                   multiple=T,
                                                   options = list(`actions-box` = TRUE)
                                       ) #pickerInput
                                       ), #column
                                column(6,
                                       selectInput("precip_var",
                                                   label="Select Variable",
                                                   choices=variables,
                                                   selected="NO3_N"
                                       ) #selectInput
                                       ) #column
                              ) #fluidRow
                              ),
                       column(6,
                              radioButtons("precip_interval",
                                           label="Select Time Interval",
                                           choiceNames=c("Days", "Weeks"),
                                           choiceValues=c("Date", "Week"),
                                           selected="Week"
                                           ),
                              plotOutput("precip_plot", height=300),
                              br(),
                              girafeOutput("precip_bar", height=350),
                              bsPopover(id="precip_plot", placement="bottom", title="About this Graph", content="This graph compares trends in precipitation to the value of the variable of interest over time. Precipitation and the variable of interest are both scaled to be between 0 and 1. Then the two variable are aggregated over the selected year(s) by day or by week and plotted over a few months."),
                              p("Precipitation data is made up of estimated historical rainfall data from the Iowa Environmental Mesonet (IEM) at Iowa State University. The data was obtained ", tags$a(href="https://mesonet.agron.iastate.edu/rainfall/", "here."), style="font-size: 20px;")
                              ) #column
                       
) #tabPanel




#
# Water Quality Index
#
wqiPage <- tabPanel(div(class="navTab", "Water Quality Index"),

                    column(4,
                           selectInput("wqi_year", width="50%",
                                                  label="Select Year",
                                                  choices=years,
                                                  selected="2021"
                           ), #selectInput
                           girafeOutput("wqi"),
                           bsPopover(id="wqi", placement="right", title="About the Water Quality Index", content="This graph shows the calculated water quality index for each watershed in the given year.", options = list(container = "body")),
                           br(),
                           p("The Water Quality Index is a system proposed by Chris Jones, a research engineer at the University of Iowa IIHR-Hydroscience and Engineering. It takes five components into account: Dissolved Oxygen, E. coli, Total Nitrogen, Total Phosphorus, and Turbidity.", style="font-size: 20px;"),
                           p("To find out more about the Water Quality Index, please visit ", tags$a(href="https://cjones.iihr.uiowa.edu/blog/2021/05/iowa-rivers-1-45-fair-marginal-ugly", "this link."), style="font-size: 20px;")
                    ), #column
                    column(8,
                           column(6,
                                  girafeOutput("DO_bar", height=250),
                                  bsPopover(id="DO_bar", placement="bottom", title="About this graph", content="This graph compares median dissolved oxygen values in a given summer to a threshold value for each watershed."),
                                  br(),
                                  girafeOutput("NO3_N_bar", height=250),
                                  bsPopover(id="NO3_N_bar", placement="top", title="About this graph", content="This graph compares median nitrate values in a given summer to a threshold value for each watershed. It should be noted that we are using nitrate-N rather here than total N. This will result in a higher calculated water quality index."),
                                  br(),
                                  girafeOutput("Turb_bar", height=250),
                                  bsPopover(id="Turb_bar", placement="top", title="About this graph", content="This graph compares median turbidity values in a given summer to a threshold value for each watershed.")
                                  ),
                           column(6,
                                  girafeOutput("E_coli_bar", height=250),
                                  bsPopover(id="E_coli_bar", placement="bottom", title="About this graph", content="This graph compares median E. coli counts in a given summer to a threshold value for each watershed."),
                                  br(),
                                  girafeOutput("DRP_bar", height=250),
                                  bsPopover(id="DRP_bar", placement="top", title="About this graph", content="This graph compares median phosphorus values in a given summer to a threshold value for each watershed. It should be noted that we are using dissolved reactive phosphorus here rather than total P. This will result in a higher calculated water quality index."),
                                  br(),
                                  br(),
                                  p("About the WQI", style="font-size: 20px;font-weight: bold;"),
                                  br(),
                                  p("The WQI used here was calculated by looking at whether a single sample did not meet each threshold, the number of samples that did not meet thresholds, and how far from the thresholds these samples were. The WQI is calculated for a single summer.", style="font-size: 20px;"),
                                  
                                  ) #column
                    ) #column
                    
) #tabPanel




#
# Table of all data
#
datatablePage <- tabPanel(div(class="navTab", "Data"),
                      
                      sidebarLayout(fluid=T,
                                    
                                    sidebarPanel(
                                      
                                      h3("Filters"),
                                      br(),
                                      
                                      pickerInput("table_watershed",
                                                  label="Select Watershed(s)",
                                                  choices=watersheds,
                                                  selected=watersheds,
                                                  multiple=T,
                                                  options = list(`actions-box` = TRUE)
                                                  ), #pickerInput
                                      
                                      pickerInput("table_site",
                                                  label = "Select Site(s)",
                                                  choices=unique(watershed_data$Site),
                                                  selected=unique(watershed_data$Site),
                                                  multiple=T,
                                                  options=list(`actions-box` = TRUE)
                                                  ), #pickerInput
                                      
                                      sliderInput("table_date",
                                                  label="Date Range",
                                                  min=min(watershed_data$Date),
                                                  max=max(watershed_data$Date),
                                                  value=c(min(watershed_data$Date),
                                                          max(watershed_data$Date))
                                      ) #sliderInput
                                      
                                    ), #sidebarPanel
                                    
                                    div(class="table", mainPanel(
                                      
                                      dataTableOutput("data_table")
                                      
                                    ) #mainPanel
                                    ) #div
                                    
                      ) #sidebarLayout
                      
) #tabPanel



aboutPage <- tabPanel(div(class="navTab", "About"),
                   
                # Allows inline equations to be used   
                tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$']]}
                });
                </script>
                ")),
                sidebarLayout(     
                sidebarPanel(
                  h3(tags$strong("About the Project"), style="text-align: center;"),
                  br(),
                  br(),
                       p("The Coe Water Quality Lab has been studying eastern Iowa surface waters for over twenty years.  Led by professor of chemistry and environmental studies Marty St. Clair, over eighty undergraduates have taken part in this effort to provide data to decision makers and citizens.  Find out more at the lab ", tags$a(href="https://cwql.weebly.com/", "website."), style="font-size: 20px;"),
                       br(),
                       p("For more information on the importance of the different parameters measured, see YSI’s ", tags$a(href="https://www.ysi.com/parameters", "website."), "To compare to values from Iowa, take a look at the Iowa Department of Natural Resource’s ", tags$a(href="http://publications.iowa.gov/23899/1/WFS-2017-02.pdf", "compilation"), " of data from 2000-2016.", style="font-size: 20px;"),
                  br(),
                  p("This dashoard was created by Brian Cochran during the Cornell College Summer Research Institute with Professor Tyler George. CSRI is an eight week summer program during which students work with faculty members on various projects. For more information about CSRI see Cornell College's ", tags$a(href="https://www.cornellcollege.edu/academic-affairs/faculty-development/internal/summer-research.shtml", "website."), style="font-size: 20px;"),
                  br(),
                  br(),
                  br()
                  ), #sidebarPanel
                mainPanel(
                       h3("How these Parameters were Measured", style="font-weight: bold;text-align: center;"),
                       br(),
                       br(),
                       column(5,
                              tags$b(tags$u("Sample Collection")),
                              tags$ul(tags$li("Surface water samples were collected by direct grab sampling or by bucket from bridges. (Containers were rinsed three times with stream water prior to collecting the final sample.) Samples to be analyzed for nitrate, chloride, sulfate, and TSS were collected in polyethylene bottles. Polyethylene sample bottles were washed in a laboratory dishwasher with distilled water rinse.  Samples to be analyzed for dissolved reactive phosphorus (DRP) were filtered in the field through a 0.45 μm nylon syringe tip filter into a 60 mL acid-washed brown glass bottle. Samples for", tags$em("E. coli"), "determination were collected by grab sampling in disposable sterile sample containers. Field blanks and duplicates were collected on each sampling trip. All samples were immediately stored in a cooler at 4 degrees Celsius until they could be transported back to the laboratory and refrigerated. Samples were typically analyzed within 24 hours of sampling.")),
                              br(),
                              tags$b(tags$u("Field Measurements")),
                              tags$ul(tags$li("Dissolved oxygen, water temperature, pH, conductivity, and turbidity were measured on-site for most surface-water samples collected. Dissolved oxygen, water temperature, pH, and conductivity have been measured with a YSI multi-parameter instrument since 2005; prior to that, individual parameter instruments were used. Turbidity was determined using a Hach 2100Q or 2100 P turbidimeter. Field instruments were calibrated on a daily basis. Dissolved oxygen values are reported in mg/L of", withMathJax("$O_{2}$"), ", turbidity is reported in NTU, and conductivity is reported in S/cm"))
                              ), #column
                       column(1),
                       column(5,
                              tags$b(tags$u("Laboratory Measurements")),
                              tags$ul(tags$li("Nitrate, sulfate, and chloride were measured using either a Dionex or Metrohm ion chromatograph using ", withMathJax("$Na_{2}CO_{3}/ NaHCO_{3}$"), "eluant. Nitrate is reported as mg/L of ", withMathJax("$NO_{3}^-$-$N$"), ", sulfate as mg/L of ", withMathJax("$SO_{4}^{-2}$"), ", and chloride as mg/L of ", withMathJax("$Cl^-$"), "."),
                                      tags$li("Dissolved reactive orthophosphate (DRP) has been analyzed using a Lachat QuikChem 8500 Series 2 flow injection analyzer running Lachat method 10-115-01-1-P since 2009. Prior to 2009, Hach method 8048 was used. All glassware used in sampling and analysis of DRP was acid washed in 1 M hydrochloric acid. Phosphate values were reported as mg/L of ", withMathJax("$PO_{4}^{3-}$"), ". "),
                                      tags$li("Total suspended solids were measured using standard gravimetric techniques with pre-weighed and -dried glass fiber filters, and reported as mg solid/liter."),
                                      tags$li(tags$em("E. coli"), "counts were determined using the IDEXX Colilert/Quanti-Tray 2000 most probable number technique since 2007. Results were reported as colony forming units (CFU) per 100 mL. Samples were typically diluted by 10X with sterile water to extend the countable range.")),
                              br(),
                              tags$b(tags$u("Sources")),
                              tags$ul(tags$li("Randall, G. W., & Mulla, D. J. (2001). Nitrate Nitrogen in Surface Waters as Influenced by Climatic Conditions and Agricultural Practices. J. Environ. Qual.,30, 337–344."),
                                      tags$li("Schilling, Keith E;Libra, Robert D, The Relationship of Nitrate Concentrations in Streams to Row Crop Land Use in Iowa. Journal of Environmental Quality; Nov/Dec 2000; 29, 6; ProQuest, pg. 1846"),
                                      tags$li("Dr. St. Clair, M., Cartwright, M., Edwards, C., Groenwold, N., McDermott, A., Olson, A., & Jake Tupper. (2021).Cedar River Tributary Study"),
                                      tags$li("Jones, C. (2021, May 27).Iowa Rivers 1 to 45: The Fair, the Marginal, and the Ugly,https://cjones.iihr.uiowa.edu/"),
                                      tags$li("Iowa State University IEM Rainfall"))
                              ), #column
                       column(1)
                       ) #mainPanel
                ) #sidebarLayout
                      
) #tabPanel




#
# Structure
#
ui <- fluidPage(theme="shiny.css",
  
  navbarPage("Watershed Dashboard", position="static-top",
             
             mapPage,
             
             precipPage,
             
             wqiPage,
             
             datatablePage,
             
             aboutPage
                      
             ), #navbarPage
  
) # fluidPage


# Server

server <- function(input, output, session) {
  
  getLabel <- function(str){
    
    return(switch(str,
           "DO"="Dissolved Oxygen",
           "Temp"="Water Temperature",
           "pH"="Acidity",
           "Cond"="Conductivity",
           "Turb"="Turbidity",
           "TSS"="Total Suspended Solids",
           "DRP"="Dissolved Reactive Phosphorus",
           "Cl"="Chloride",
           "NO3_N"="Nitrate",
           "SO4"="Sulfate",
           "E_coli"="E. coli"
           ))
  }
  
  #
  # Map Page- Overview Tab
  #
  
  # Graph which filters by year and gives a line graph comparing different watersheds over one summer
  source("scripts/overview_watersheds.R", local=T)
  
  
  
  #Rendering spider plot for overview page
  source("scripts/overview_spider_plot.R", local=T)
  
  
  
  #
  # Map Page - Interactive Map
  #
  
  # Header of plots page
  output$map_title <- renderText({ifelse(length(input$map_shape_click$id)>0, paste0(input$map_shape_click$id, " Watershed"), "Select a watershed to see visualizations")})
  
  # Getting max, min, and center long and lat for use in setting bounds of interactive map
  temp_data <- tidy(merged_watershed_shp)
  min_lat <- min(temp_data$lat)
  max_lat <- max(temp_data$lat)
  min_long <- min(temp_data$long)
  max_long <- max(temp_data$long)
  center_lat <- (min_lat+max_lat)/2
  center_long <- (min_long+max_long)/2
  
  # Icon for sampling site markers
  siteIcon <- makeIcon(
    iconUrl="https://resources.finalsite.net/images/f_auto,q_auto,t_image_size_2/v1620195943/brentwoodk12nyus/wivrch9gpckn4nvyconr/experiment-1295041_1280.png",
    iconWidth = 25, iconHeight = 24
  )
  
  
  # Change page on map_shape_click input
  observeEvent(input$map_shape_click, {
    
    updateNavbarPage(session=session,
                     inputId = "map_nav",
                     selected="overview_tab")
    req(input$map_shape_click$id)
    updateNavbarPage(session=session,
                     inputId = "map_nav",
                     selected="plots_tab")
      
  }) #observeEvent
  
  # Render leaflet map
  source("scripts/map.R", local=T)
  
  
  
  
  #
  # Map Page - Plots Tab
  #
  
  # Plot comparing years for a given variable
  source("scripts/map_years_plot.R", local=T)
  
  
  
  # Spider Plot
  #Updating input to not include currently selected watershed on map
  observeEvent(input$map_shape_click, {
    
    updatePickerInput(session=session, "add_watershed_spider", label = NULL,
                      choices = c(watersheds[watersheds!=input$map_shape_click$id], "None"),
                      selected = "None")
    
  }) #observeEvent
  
  
  #Rendering spider plot for watershed page
  source("scripts/map_spider_plot.R", local=T)
  
  
  # Plot showing observations of variable over a given time
  source("scripts/map_change_plot.R", local=T)
  
  # Plot showing distribution of variable in a given time
  source("scripts/map_dist_plot.R", local=T)
  
  
  
  
  #
  # Precipitation Page
  #
  
  
  # Precipitation Map
  source("scripts/precip_map.R", local=T)
  
  
  #Precipitation Bar Graph
  source("scripts/precip_bar.R", local=T)
  
  
  # Precipitation Plot
  source("scripts/precip_plot.R", local=T)
  
  
  
  #
  # Water Quality Index Page
  #
  
  #F1 measures whether any sample failed a test
  getF1 <- function(data){
    vars <- rep(FALSE, 5)
    vars[1] <- any(data$DO < 5, na.rm=T)
    vars[2] <- any(data$E_coli > 235, na.rm=T)
    vars[3] <- any(data$NO3_N > 3.5, na.rm=T)
    vars[4] <- any(data$P > 0.18, na.rm=T)
    vars[5] <- any(data$Turb > 25, na.rm=T)
    return((sum(vars)/5)*100)
  }
  
  #F2 measures how many samples failed tests
  getF2 <- function(data){
    DO_tests <- sum(!is.na(data$DO))
    DO_failed <- sum(data$DO < 5, na.rm=T)
    
    E_coli_tests <- sum(!is.na(data$E_coli))
    E_coli_failed <- sum(data$E_coli > 235, na.rm=T)
    
    NO3_N_tests <- sum(!is.na(data$NO3_N))
    NO3_N_failed <- sum(data$NO3_N > 3.5, na.rm=T)
    
    P_tests <- sum(!is.na(data$P))
    P_failed <- sum(data$P > 0.18, na.rm=T)
    
    Turb_tests <- sum(!is.na(data$Turb))
    Turb_failed <- sum(data$Turb > 25, na.rm=T)
    
    return(((DO_failed+E_coli_failed+NO3_N_failed+P_failed+Turb_failed)/(DO_tests+E_coli_tests+NO3_N_tests+P_tests+Turb_tests))*100)
  }
  
  #F3 measures how far off failed tests were from thresholds
  getF3 <- function(data){
    #departure is how far off failed tests were (standardized)
    DO_departure <- sum((5/data$DO[data$DO < 5 & !is.na(data$DO)])-1)
    E_coli_departure <- sum((data$E_coli[data$E_coli > 235 & !is.na(data$E_coli)]/235)-1)
    NO3_N_departure <- sum((data$NO3_N[data$NO3_N > 3.5 & !is.na(data$NO3_N)]/3.5)-1)
    P_departure <- sum((data$P[data$P > 0.18 & !is.na(data$P)]/0.18)-1)
    Turb_departure <- sum((data$Turb[data$Turb > 25 & !is.na(data$Turb)]/25)-1)
    departure <- sum(c(DO_departure, E_coli_departure, NO3_N_departure, P_departure, Turb_departure))
    
    #how many samples of each variable there were
    DO_tests <- sum(!is.na(data$DO))
    E_coli_tests <- sum(!is.na(data$E_coli))
    NO3_N_tests <- sum(!is.na(data$NO3_N))
    P_tests <- sum(!is.na(data$P))
    Turb_tests <- sum(!is.na(data$Turb))
    
    #nse is a statistic used to calculate F3
    nse <- departure/(DO_tests+E_coli_tests+NO3_N_tests+P_tests+Turb_tests)
    
    #this is the F3 formula
    return(nse/((0.01*nse)+0.01))
  }
  
  # WQI
  source("scripts/wqi.R", local=T)
  
  # Dissolved Oxygen Graph
  source("scripts/DO_bar.R", local=T)
  
  #E. coli graph
  source("scripts/E_coli_bar.R", local=T)
  
  # Nitrate Graph
  source("scripts/NO3_N_bar.R", local=T)
  
  # Phosphorus Graph
  source("scripts/DRP_bar.R", local=T)
  
  # Turbidity Graph
  source("scripts/Turb_bar.R", local=T)
  

  
  #
  # Data table page
  #
  
  #filter data by user selection
  new_data <- reactive({
    watershed_data %>%
      filter(Watershed %in% input$table_watershed) %>%
      filter(Site %in% input$table_site) %>%
      filter(Date > input$table_date[1] & Date < input$table_date[2])
    
  }) #reactive
  
  output$data_table <- renderDataTable(new_data())

} #server


# Run app
shinyApp(ui=ui, server=server)