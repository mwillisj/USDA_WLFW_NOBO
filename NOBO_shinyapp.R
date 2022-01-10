library(shiny)
library(leaflet)
library(leaflet.extras)

ui <- fluidPage(
  leafletOutput("map",
                height = 600,
                width = '100%'),
  absolutePanel(
    id = "controls",
    top = 40, left = 40
  ),
  sidebarPanel()
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    nobo_and_gct_leaflet
  })
}

shinyApp(ui, server)
