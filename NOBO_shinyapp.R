library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)

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

# create a function to plot data in bottom panel of shiny app
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

# test plot
depth_plot("STATE", "abund")


# Shiny App ----------------------------
## UI ---------

ui <- fluidPage(
  mainPanel(
    leafletOutput("map",
                  height = 400,
                  width = '100%'),
    selectInput("select", label = h5("Continuous Variable"),
                choices = colnames(bird_df)),
    plotOutput("Plot")),
  # absolutePanel(
  #   id = "controls",
  #   top = 40, left = 40,
  #   selectInput("select", label = h3("Continuous Variable"),
  #               choices = colnames(bird_df))
  # ),
  sidebarPanel(
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
    output$map <- renderLeaflet({
    nobo_and_gct_leaflet
  })
}


## Run App -----------

shinyApp(ui, server)






