library(shiny)
library(leaflet)
library(leaflet.extras)

ui <- fluidPage(
  mainPanel(
    leafletOutput("map",
                  height = 400,
                  width = '100%')),
  # absolutePanel(
  #   id = "controls",
  #   top = 40, left = 40
  # ),
  sidebarPanel(
    selectInput("state", label = h3("Select State"), 
                choices = GCT_and_Geographies$STATE),
    htmlOutput("goals")
    
  )
)



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
    output$map <- renderLeaflet({
    nobo_and_gct_leaflet
  })
}

shinyApp(ui, server)






