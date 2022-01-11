library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(ggmap)


# Create a googleKey.R in the same folder and register your Google Key with ggmap
#source('C:/Users/mwill/OneDrive - Harvard University/2022 - Winter/RShiny/Day05/Scripts/ggmap_demo.R')


# Datasets ---------------------

bird_df <- bird_data %>%
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

# Functions -------------------

## Plots -----

# create a function to plot data in bottom panel of shiny app ------------
depth_plot <- function(var_x, var_y, var_z = "REGION") {
  ggplot(bird_df) +
    geom_point(aes(.data[[var_x]], .data[[var_y]],
                   color = .data[[var_z]],
                   fill = .data[[var_z]]),
               alpha = 0.3, size = 3, shape = "circle") +
    coord_flip() +
    # labs(x = "", y = y_axis_lab, subtitle = subtitle) +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 16),
      axis.text.x = element_text(family = "Trebuchet MS", size = 12),
      panel.grid = element_blank())
}

state_plot <- function(var_x, var_y, var_z = "REGION") {
  ggplot(GCT_and_Geographies) +
    geom_col(aes(.data[[var_x]], .data[[var_y]], fill = .data[[var_z]]))
}



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
  
  #create layer toggle  
  addLayersControl(
    baseGroups = c("Aerial", "Map"),
    overlayGroups = c("Points", "States", "Priority Counties"), 
    position = "topleft", 
    options = layersControlOptions(collapsed = T)
  ) %>% 
  
  #Add State Data 
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
  
  # addMarkers(data = bird_dat,
  #            icon = symbols_size(basesize = 2),
  #            label=paste("2002-2019:","Trend:",
  #                        round(bird_dat$trend,digits=2),"%, Abundance: ",
  #                        round(bird_dat$abund), sep = ""),
  #            popup=~label,
  #            group="Points") %>%
  
  addPolygons(data=transf_natl_PA, 
              color="Black",
              fillColor = "mediumpurple2", 
              highlightOptions = highlightOptions(fillColor = "red", fillOpacity = .7),
              opacity = .5,
              weight =1,
              group="Priority Counties")%>%
  
  #set Max Bounds
  setMaxBounds(lng1=-140.791110603, 
               lat1= 10,
               lng2= -40.96466,
               lat2= 81.3577635769)
  
  
  # addLegendSize(values = bird_dat$abund,
  #               color = "black",
  #               fillColor = "black",
  #               title = "Abundance",
  #               orientation = "vertical",
  #               baseSize = 1.5,
  #               shape = "circle")



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
                             choices = colnames(bird_df)),
                 plotOutput("Plot")),
               tabPanel("State Data",
                        selectInput("stateselect", label = h5("Continuous Variable"),
                                    choices = colnames(GCT_and_Geographies)),
                        plotOutput("statePlot"))
    )),
  # absolutePanel(
  #   id = "controls",
  #   top = 40, left = 40,
  #   selectInput("select", label = h3("Continuous Variable"),
  #               choices = colnames(bird_df))
  # ),
  sidebarPanel(
    textInput('address', 'Enter address'),
    actionButton('search', 'Search'),
    textOutput('result1'),
    textOutput('result2'),
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
  output$Plot <- renderPlot(depth_plot("STATE", input$select))
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
    
    latlon <- reactive({
      input$search
      geocode(isolate(input$address),
              output = "latlona", # this returns a list of 3: latitude, longitude, and address
              source = "google")
    })
    
    output$result1 <- renderText({
      if (!is.na(latlon())) {
        paste('Address:', latlon()$address)
      } else {
        ''
      }
    })
    
    output$result2 <- renderText({
      if (!is.na(latlon())) {
        paste('Coordinates:', latlon()$lon, latlon()$lat)
      } else {
        ''
      }
    })
    
    observe({
      leafletProxy("map", data = latlon()) %>%
        addMarkers()
    })
}


## Run App -----------

shinyApp(ui, server)






