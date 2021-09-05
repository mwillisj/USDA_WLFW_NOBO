library(shiny)
library(leaflet)

function(input, output, session){
  
  
  
  
  
  
  output$map<-renderLeaflet({
    leaflet() %>% addTiles()
    
    # nobo_and_gct_leaflet <- leaflet(options=leafletOptions(minZoom = 4)) %>%
    #   addProviderTiles(providers$CartoDB.Positron) %>%
    #   addProviderTiles("Esri.WorldImagery", group="Aerial") %>% 
    #   
    #   addMapPane("Pointpane", zIndex=480) %>%
    #   addMapPane("Prioritypane", zIndex=470) %>%
    #   addMapPane("Statespane", zIndex=460) %>%
    #   
    #   #create layer toggle  
    #   addLayersControl(
    #     baseGroups = c("Map", "Aerial"),
    #     overlayGroups = c("Points", "States", "Priority Counties"), 
    #     position = "topleft"
    #   ) %>% 
    #   
    #   #Add State Data 
    #   addPolygons(data=GCT_and_Geographies,
    #               highlightOptions = highlightOptions(fillColor="green", fillOpacity=.3),
    #               weight=1, 
    #               fillColor= "blanchedalmond",
    #               color = "black",
    #               fillOpacity=.3,
    #               label = paste0(GCT_and_Geographies$STATE, " - ", GCT_and_Geographies$REGION), 
    #               popup=~label,
    #               group="States",
    #               #options=pathOptions(pane="Statespane"),
    #               labelOptions = labelOptions(
    #                 style = list("font-weight" = "normal", padding = "3px 8px"),
    #                 textsize = "11px",
    #                 direction = "auto")) %>% 
    #   
    #   # Add NOBO trend Data
    #   addCircles(data=bird_dat, 
    #              color = ~pal(trend), 
    #              weight= ~abund/5,
    #              opacity=1,# fillOpacity = 0.6,
    #              label=paste("2002-2019:","Trend;", round(bird_dat$trend,digits=3),", Abundance", round(bird_dat$abund)), 
    #              popup=~label, 
    #              #options=pathOptions(pane="Pointspane"),
    #              group="Points") %>%
    #   
    #   addPolygons(data=transf_natl_PA, 
    #               # popup= GCT_and_Geographies$label[GCT_and_Geographies$STATE==indiana_priority_map$State],
    #               # color=priority$color, fillOpacity=0.8,
    #               #label=priority$Priority,
    #               #options=pathOptions(pane="Prioritypane"),
    #               color="Black",
    #               fillColor = "mediumpurple2", 
    #               opacity = .9,
    #               weight =1,
    #               group="Priority Counties")%>%
    #   
    #   #set Max Bounds
    #   setMaxBounds(lng1=-140.791110603, 
    #                lat1= 10,
    #                lng2= -40.96466,
    #                lat2= 81.3577635769) %>% 
    #   
    #   addLegend("bottomright", pal=pal ,title="Titlehere", values=c(-3,4), group="Points")
    # 
    # 
    # nobo_and_gct_leaflet
    # 
    
  })
  
  
  
  
}