
library(rgdal)


##Loading RDS
bird_data <- readRDS("NOBO_route_level_trends.rds")

bird_data<-as(bird_data, "Spatial")  #this converts the object, bird_data, into a SpatialPointsDataFrame

proj4string(bird_data)  #this tells you what the projection is

# proj4string(bird_data)<-"+proj=laea +lat_0=40 +lon_0=-95 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"  ## if proj4string() is NA, you can define a projection.  IT HAS TO BE THE PROJECTION THAT IT WAS SUPPOSED TO HAVE ORIGINALLY

## convert the projection
bird_dat<-spTransform(bird_data, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#bd<-as.data.frame(bird_dat)  # if you want to convert the spatial dataframe to a normal dataframe, you do this and it will put the coordinates in columns at the end
#write.csv(bd, file="C:/users/name.name/downloads/file.csv", row.names=F)  #if you want to write to csv



## ggplot
ggplot(bird_data, aes(color=lci_trend)) +
  geom_sf()





GCT_and_Geographies$label <- 
  paste("<b>", "<big>", GCT_and_Geographies$STATE,"</b>", "</big>",
        "<br>", GCT_and_Geographies$REGION,
        "<br>", "Values shown below reflect the combined value of", "<br>", "Original Program Goals and Framework Goals",
        "<br>",
        "<br>",
        "<b> Top 3 Core Conservation Practices: </b>",
        "<br>",
        GCT_and_Geographies$TOP3CORE,
        "<br>",
        "<br>",
        "<b>", "Financial Assistance:", "</b>","<br>",
        "$ ", prettyNum(GCT_and_Geographies$REQFIN_FRAME,big.mark=","),
        "<br>",
        "<br>",  "<b>", "Total CP Coverage, Acres:", "</b>",
        "<br>", "Core:", prettyNum(GCT_and_Geographies$ACRE_FRAME_CORE, big.mark = ","),
        "<br>", "Supplemental:", prettyNum(GCT_and_Geographies$ACRE_FRAME_SUPP, big.mark = ","),
        "<br>", "Core and Supp Combined:", prettyNum(GCT_and_Geographies$ACRE_FRAME_CAS, big.mark = ","),
        "<br>",
        "<br>", "<b>", "Total CP Coverage, Feet:", "</b>",
        "<br>", "Core:", prettyNum(GCT_and_Geographies$FT_FRAME_CORE, big.mark = ","),
        "<br>", "Supplemental:", prettyNum(GCT_and_Geographies$FT_FRAME_SUPP, big.mark = ","),
        "<br>", "Core and Supp Combined:", GCT_and_Geographies$FT_FRAME_CAS,
        "<br>",
        "<br>", "<b>", "Total CP Coverage, Number of X:", "</b>",
        "<br>",GCT_and_Geographies$X_FRAME,
        "<br>",
        "<br>", "<b>", "Number of Written Plans:", "</b>",
        "<br>",GCT_and_Geographies$WRITTEN_FRAME,
        "<br>",
        "<br>", "<b>", "Number of Applied Plans:", "</b>",
        "<br>",GCT_and_Geographies$APPLIED_FRAME)  
  
#lapply(htmltools::HTML)


#############################################################################

## read in shapefile of priority areas

#gsub("\\\\", "/", readClipboard())  # for changing slashes around

priority<-readOGR("C:/Users/sageg/Downloads/WLFW_Bobwhite", layer="WLFW_BWQ_Priority_Counties_only_2021")

proj4string(priority)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


#head(priority)
#plot(priority)

unique(priority$Priority)

priority$color<-ifelse(priority$Priority=="High", "red", ifelse(priority$Priority=="Medium", "orange", "yellow"))


list.files("C:/Users/sageg/Desktop", pattern="Bobwhite", include.dirs = TRUE)

natl_priority_map<-readOGR("C:/Users/sageg/Desktop/NOBO_Boundary_Aug2021_Dissolve_ST_Draft", layer="NOBO_Boundary_Aug2021_Dissolve_ST_Draft")
proj4string(natl_priority_map)
plot(natl_priority_map)
proj4string(natl_priority_map)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(natl_priority_map)

indiana_priority_map <-readOGR("C:/Users/sageg/Downloads/IN_WLFW_Priority_2022", layer="IN_WLFW_Priority_2022")
indiana_priority_map$State<-"Indiana"
plot(indiana_priority_map)
proj4string(indiana_priority_map)


##############################################################################################
## leaflet

leaflet() %>%
  addProviderTiles("CartoDB.Positron", group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group="Aerial") %>% 
  
  addLayersControl(
    baseGroups = c("Map", "Aerial"),
    overlayGroups = c("Points", "States", "Counties"), 
    position = "topleft"
  ) %>% 
  
  addPolygons(data=GCT_and_Geographies,
    highlightOptions = highlightOptions(fillOpacity = 1, fillColor="cornsilk"),
              weight=1, 
              fillColor= "darkseagreen",
              color = "black",
              fillOpacity=.5,
              label = paste0(GCT_and_Geographies$STATE, " - ", GCT_and_Geographies$REGION), 
              popup=~label,
              group="States",
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "11px",
                direction = "auto")) %>% 
  addCircles(data=bird_dat, 
             #lng = ~long, 
             #lat = ~lat, 
             color ="red", fillColor = "yellow", opacity=1, fillOpacity = 0.6,
             label=paste0("LCI Trend: ", round(bird_dat$lci_trend,digits=3)), 
             popup="", 
             group="Points") %>% 
  
  addPolygons(data=indiana_priority_map, 
              popup= GCT_and_Geographies$label[GCT_and_Geographies$STATE==indiana_priority_map$State],
             # color=priority$color, fillOpacity=0.8,
              #label=priority$Priority,
              group="Counties")

#plot()



transf_natl_PA <- spTransform(natl_priority_map, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(transf_natl_PA)
  
  
  
  
  






