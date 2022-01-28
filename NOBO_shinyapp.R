

# Create a googleKey.R in the same folder and register your Google Key with ggmap
#source('C:/Users/mwill/OneDrive - Harvard University/2022 - Winter/RShiny/Day05/Scripts/ggmap_demo.R')

### Loading Libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(ggmap)
library(sf)

library(tidyverse)
library(sf)
library(leaflet)
library(leaflegend)
library(htmltools)
library(htmlwidgets)
library(raster)
library(gstat)
library(spatial)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(rgdal)
library(RColorBrewer)
library(HatchedPolygons)
library(ggspatial)
library(stars)
library(ggpattern)
library(maps)
library(mapproj)



options(scipen=999)

# setwd("~/USDA_WLFW_NOBO")


### Loading Variable Sets



##Loading Boundaries
State_Boundaries <- st_read("cb_2018_us_state_500k.kml", quiet = TRUE)
State_Boundaries_GJSON <- st_read("US_State_Boundaries.json", quiet = TRUE)
State_Boundaries_Zip <- st_read("StateBoundariesZIP", quiet = T)


#loading counties
natl_priority_map<-readOGR("NOBO_Boundary_Aug2021_Dissolve_ST_Draft", layer="NOBO_Boundary_Aug2021_Dissolve_ST_Draft")


##Loading Goal Collection (GCT) Data
GCT_data <- read_csv("NOBODATA_ForLeaflet_Final.csv")


##Loading BIRD DATA RDS
bird_data <- readRDS("NOBO_route_level_trends.rds")

bird_data<-as(bird_data, "Spatial")  #this converts the object, bird_data, into a SpatialPointsDataFrame



## Converting Projection
bird_dat<-spTransform(bird_data, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


transf_natl_PA <- spTransform(natl_priority_map, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


##Joining Boundaries and GCT Data
GCT_and_Geographies <- st_as_sf(left_join(GCT_data, State_Boundaries_Zip, by =c("STATE" ="NAME")))

#read state abbr for later mapping
state_abbr <- data.frame(tolower(state.abb) %>%
                           cbind(tolower(state.name)))

colnames(state_abbr) <- c("state","state_name")







### Creating Labels for GCT Data + Bird Data


## Creating Label for States
GCT_and_Geographies$label <- 
  paste("<b>", "<big>", GCT_and_Geographies$STATE,"</b>", "</big>",
        "<br><i>", GCT_and_Geographies$REGION, "</i>",
        "<br>", "Values shown below reflect the combined value of", "<br>", "Original Program Goals and Framework Goals",
        "<br>",
        "<br>",
        "<b style='font-size:90%'> Top 3 Core Conservation Practices: </b>",
        "<br>",
        GCT_and_Geographies$TOP3CORE,
        "<br>",
        "<br>",
        "<b style='font-size:90%'>", "Financial Assistance:", "</b>","<br>",
        "$ ", prettyNum(GCT_and_Geographies$REQFIN_FRAME,big.mark=","),
        "<br>",
        "<br>",  "<b style='font-size:90%'>", "Total CP Coverage, Acres:", "</b>",
        "<br>", "Core:", prettyNum(GCT_and_Geographies$ACRE_FRAME_CORE, big.mark = ","),
        "<br>", "Supplemental:", prettyNum(GCT_and_Geographies$ACRE_FRAME_SUPP, big.mark = ","),
        "<br>", "Core and Supp Combined:", prettyNum(GCT_and_Geographies$ACRE_FRAME_CAS, big.mark = ","),
        "<br>",
        "<br>", "<b style='font-size:90%'>", "Total CP Coverage, Feet:", "</b>",
        "<br>", "Core:", prettyNum(GCT_and_Geographies$FT_FRAME_CORE, big.mark = ","),
        "<br>", "Supplemental:", prettyNum(GCT_and_Geographies$FT_FRAME_SUPP, big.mark = ","),
        "<br>", "Core and Supp Combined:", GCT_and_Geographies$FT_FRAME_CAS,
        "<br>",
        "<br>", "<b style='font-size:90%'>", "Total CP Coverage, Number of X:", "</b>",
        "<br>",GCT_and_Geographies$X_FRAME,
        "<br>",
        "<br>", "<b style='font-size:90%'>", "Number of Written Plans:", "</b>",
        "<br>",GCT_and_Geographies$WRITTEN_FRAME,
        "<br>",
        "<br>", "<b style='font-size:90%'>", "Number of Applied Plans:", "</b>",
        "<br>",GCT_and_Geographies$APPLIED_FRAME) %>% 
  lapply(htmltools::HTML)



# Creating Label for NOBO Trend Data
bird_dat$label <- 
  paste("NOBO Trend Data","<br>", "Abundance:", round(bird_dat$abund, digits=3),"<br>","Trend:", round(bird_dat$trend, digits=3),"</b>")%>%
  lapply(htmltools::HTML)




# Datasets ---------------------

temp_priority_data <- natl_priority_map %>%
  st_as_sf() %>%
  st_transform(crs = "WGS84")

sf::sf_use_s2(F)

# temp_priority_data$geometry <- temp_priority_data$geometry %>%
#   s2::s2_rebuild() %>%
#   sf::st_as_sfc()

bird_df <- bird_data
bird_df <- bird_df %>%
  st_as_sf() %>%
  st_transform(crs = "WGS84") %>%
  st_join(y = natl_priority_map %>%
            st_as_sf() %>%
            st_transform(crs = "WGS84")) %>%
  rename(priority_area = STATE) %>%
  mutate(priority_area = case_when(is.na(priority_area) == F ~ "Priority Area",
                                   is.na(priority_area) == T ~ "Non-Priority Area")) %>%
  as.data.frame() %>%
  mutate(STATE = substr(bird_df$strat, 4, nchar(bird_df$strat) - 3),
         REGION = ifelse(STATE %in% c("IL",
                                      "IN",
                                      "IA",
                                      "KS",
                                      "MN",
                                      "MO",
                                      "NE",
                                      "TX"), "Central", 
                  ifelse(STATE %in% c("DE",
                                      "MD",
                                      "NJ",
                                      "OH",
                                      "WV"), "Northeast", "Southeast")))

# gct_columns <- as.data.frame(colnames(GCT_and_Geographies))
# gct_columns
# 
# GCT_and_Geographies_clean <- GCT_and_Geographies %>%
#   mutate()


# create table of summary stats between priority and non-priority areas
bird_stats <- cbind(rbind("Non-Priority","Priority"),
                    rbind(bird_df %>%
                            group_by(priority_area) %>%
                            summarise(avg_trend = mean(trend),
                                      med_trend = median(trend),
                                      avg_abund = mean(abund),
                                      med_abund = median(abund),
                                      avg_prec_i = mean(prec_i),
                                      med_prec_i = median(prec_i)))
                    )



# Functions -------------------

## Plots -----

# create a function to plot data in bottom panel of shiny app ------------
depth_plot <- function(var_x, var_y, var_z = "REGION") {
  ggplot(bird_df) +
    geom_point(aes(.data[[var_x]], .data[[var_y]],
                   color = .data[[var_z]],
                   fill = .data[[var_z]]),
               alpha = 0.3, size = 3, shape = "circle") +
    # geom_boxplot(aes(.data[[var_x]], .data[[var_y]])) +
    coord_flip() +
    # labs(x = "", y = y_axis_lab, subtitle = subtitle) +
    theme(
      legend.position = "bottom", 
      axis.title = element_text(size = 16),
      axis.text.x = element_text(family = "Trebuchet MS", size = 12),
      panel.grid = element_blank())
      # labs( x = NULL, y = var_y))
}

GCT_and_Geographies_df <- as.data.frame(GCT_and_Geographies)

state_plot <- function(var_x, var_y, var_z = "REGION") {
  ggplot(GCT_and_Geographies_df) + 
    geom_col(aes(x = reorder(.data[[var_x]], 
                             -as.numeric(as.character(.data[[var_y]]))), 
                 y = as.numeric(as.character(.data[[var_y]])), 
                 fill = .data[[var_z]]),
             # labs(x = NULL, y = var_y),
             # theme(
             #   axis.text.x = element_text(family = "Trebuchet MS", size = 12, angle = 90)
             )
}

state_plot("STATE", "ACRE_PRO_SUPP")


# create leaflet -------------

# TREND
bins_trend <- seq(min(bird_dat$trend),
                  max(bird_dat$trend), by = .25)


numPal <- colorNumeric(brewer.pal(11, "RdYlGn"), bird_dat$trend)


#ABUND
bins_abund <- seq(min(bird_dat$abund),
                  max(bird_dat$abund), 
                  by = 10)


symbols_size <- function(numpal = numPal, colorvalues = bird_dat$trend, Color = "black",
                          strokewidth = .15, Values = bird_dat$abund,
                          Shape = "circle",
                          Opacity = 0.8,
                          basesize) {
  makeSizeIcons(
    pal = numpal, colorValues = colorvalues, color = Color, strokeWidth = strokewidth,
    values = Values,
    shape = Shape,
    opacity = Opacity,
    baseSize = basesize
  )
}



# setup Leaflet
nobo_and_gct_leaflet <- leaflet(options=leafletOptions(minZoom = 4)) %>%
  addProviderTiles("Esri.WorldImagery", group="Aerial",
                   tileOptions(opacity = .6)) %>% 
  addProviderTiles(providers$Stamen, group = "Map", 
                   options = tileOptions(opacity = .5)) %>%

  #set Max Bounds
  setMaxBounds(lng1=-140.791110603, 
               lat1= 15,
               lng2= -55.96466,
               lat2= 81.3577635769) %>%
  
  #create layer toggle  
  addLayersControl(
    baseGroups = c("Aerial", "Map"),
    overlayGroups = c("Points", "States", "Priority Counties"), 
    position = "topleft", 
    options = layersControlOptions(collapsed = T)
  ) 

# Shiny App ----------------------------
## UI ---------

ui <- fluidPage(
  titlePanel("Northern Bobwhite in the US"),
  mainPanel(
    leafletOutput("map",
                  height = 400,
                  width = '100%'),
    sliderInput(inputId = "size", label = "Marker Size", 2, 10, value = 2),
    navbarPage("Plots",
               tabPanel("Bird Data",
                 selectInput("select", label = h5("Continuous Variable"),
                             choices = colnames(bird_df),
                             selected = "trend"),
                 plotOutput("Plot")),
               tabPanel("State Data",
                        selectInput("stateselect", label = h5("Continuous Variable"),
                                    choices = colnames(GCT_and_Geographies), 
                                    selected = "ACRE_PRO_SUPP"),
                        plotOutput("statePlot"))
    )),
  # absolutePanel(
  #   id = "controls",
  #   top = 40, left = 40,
  #   selectInput("select", label = h3("Continuous Variable"),
  #               choices = colnames(bird_df))
  # ),
  sidebarPanel(
    # textInput('address', 'Enter address'),
    # actionButton('search', 'Search'),
    # textOutput('result1'),
    # textOutput('result2'),
    selectInput("state", label = h5("Select State"),
                choices = GCT_and_Geographies$STATE),
    htmlOutput("goals")
    
  )
)

## Server -----------

server <- function(input, output, session) {
  output$goals <- renderUI({
    x <- paste0(GCT_and_Geographies %>%
                  as.data.frame() %>%
                  filter(STATE == {input$state}) %>%
                  subset(select = "label") %>%
                  substr(7, nchar(GCT_and_Geographies %>%
                                    as.data.frame() %>%
                                    filter(STATE == {input$state}) %>%
                                    subset(select = "label")) - 2))
    HTML(x)
    })
  output$Plot <- renderPlot(depth_plot("priority_area", input$select))
  output$statePlot <- renderPlot(state_plot("STATE", input$stateselect))
    output$map <- renderLeaflet({
    nobo_and_gct_leaflet
  })
    observe({
      leafletProxy("map", data = bird_dat) %>%
        clearMarkers() %>%
        clearControls() %>%
      addMarkers(data = bird_dat,
                 icon = symbols_size(basesize = {input$size}),
                 label=paste("2002-2019:","Trend:", 
                             round(bird_dat$trend,digits=2),"%, Abundance: ", 
                             round(bird_dat$abund), sep = ""), 
                 popup=~label, 
                 group="Points") %>%
        
        #Add State Data 
        clearShapes() %>%
        addPolygons(data=GCT_and_Geographies,
                    highlightOptions = highlightOptions(fillColor="green", fillOpacity=.3),
                    weight=1, 
                    fillColor= "white",
                    color = "black",
                    fillOpacity=.05,
                    popup = paste0(GCT_and_Geographies$STATE, " - ", GCT_and_Geographies$REGION), 
                    group="States",
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "11px",
                      direction = "auto")) %>% 
        addPolygons(data = GCT_and_Geographies %>% filter(STATE == {input$state}),
                    fillColor = "purple",
                    fillOpacity = .3,
                    popup = paste0({input$state})
                    ) %>%
        addPolygons(data=transf_natl_PA, 
                    color="Black",
                    fillColor = "mediumpurple2", 
                    highlightOptions = highlightOptions(fillColor = "red", fillOpacity = .7),
                    popup = "Priority Area",
                    opacity = .5,
                    weight =1,
                    group="Priority Counties") %>% 
        
        addLegendSize(values = bird_dat$abund,
                      color = "black",
                      fillColor = "black",
                      title = "Abundance",
                      orientation = "vertical",
                      baseSize = {input$size},
                      shape = "circle") %>%
        addLegend(title = paste("Trend 2002-2019<br>(Avg Annual Change)"), 
                  colors = brewer.pal(9, "RdYlGn"),
                  labels = c("4 Percent Decrease","","","",
                             "No Change","","","","4 Percent Increase"), 
                  position = "topright")
        
    })
    
    # latlon <- reactive({
    #   input$search
    #   geocode(isolate(input$address),
    #           output = "latlona", # this returns a list of 3: latitude, longitude, and address
    #           source = "google")
    # })
    
    # output$result1 <- renderText({
    #   if (!is.na(latlon())) {
    #     paste('Address:', latlon()$address)
    #   } else {
    #     ''
    #   }
    # })
    # 
    # output$result2 <- renderText({
    #   if (!is.na(latlon())) {
    #     paste('Coordinates:', latlon()$lon, latlon()$lat)
    #   } else {
    #     ''
    #   }
    # })
    
    # observe({
    #   leafletProxy("map", data = latlon()) %>%
    #     addMarkers()
    # })
}


## Run App -----------

shinyApp(ui, server)

NOBO_App <- shinyApp(ui, server)



# ## Deploy App --------------
# library(rsconnect)
# rsconnect::deployApp()


