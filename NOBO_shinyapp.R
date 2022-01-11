library(shiny)
library(leaflet)
library(leaflet.extras)
library(qdapTools)

ui <- fluidPage(
  mainPanel(
    leafletOutput("map",
                  height = 600,
                  width = '100%')),
  absolutePanel(
    id = "controls",
    top = 40, left = 40
  ),
  sidebarPanel(
    selectInput("state", label = h3("Select State"), 
                choices = GCT_and_Geographies$STATE),
    hr(),
    htmlOutput("goals")
    
  )
)


server <- function(input, output, session) {
  output$goals <- renderUI({
    x <- paste0(GCT_and_Geographies %>%
                  as.data.frame() %>%
                  filter(STATE == {input$state}) %>%
                  subset(select = "label"))
    HTML(x)
    })
    output$map <- renderLeaflet({
    nobo_and_gct_leaflet
  })
}

shinyApp(ui, server)







