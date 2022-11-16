source("../src/wgi_calculations.R")
library(dplyr)
library(leaflet)
library(sf)
library(rgdal)

wgi_21 <- d_calcs %>%
  filter(year == 2021) %>%
  inner_join(.,data[,c("countryname", "code")], by = "countryname")



# Geometry
shapes <- st_read("../data/countries_shp/countries.shp")
shapes <- merge(shapes, wgi_21, by.x = 'ISO3', by.y = 'code')
shapes <- shapes %>%
  group_by(countryname, year) %>%
  slice(1)

# Create a continuous palette function
pal <- colorNumeric(
  palette = c("red", "green"),
  domain = shapes$score_wgi_scale,
  na.color = "#808080")



#Fonction réalisant l'histogramme de répartition des Groupe d'aliment en fonction
# de leur score EF 
corruption_map <- function(input,output){
  
  output$mymap <- renderLeaflet({
    leaflet(shapes) %>% 
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lat = 10, lng = 0, zoom = 1) %>%
      addPolygons(fillColor = ~pal(shapes$score_wgi_scale),
                  fillOpacity = 0.7,
                  label = paste(shapes$countryname, shapes$score_wgi_scale,sep = ","),
                  stroke = TRUE,
                  color = 'black',
                  weight = 1,
                  opacity = 1) %>%
      addLegend(pal = pal, values = ~score_wgi_scale, opacity = 0.7, title = "WGI",
                position = "bottomright")
  
  })
  
}




