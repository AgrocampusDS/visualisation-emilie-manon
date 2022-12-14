library(dplyr)
library(leaflet)
library(sf)
library(rgdal)
source("./src/wgi_calculations.R")

wgi_21 <- d_calcs %>%
  filter(year == 2021) %>%
  inner_join(.,data[,c("countryname", "code")], by = "countryname")

code_wgi <- wgi_21$code
code_wgi <- code_wgi[!duplicated(code_wgi)]

# Geometry
shapes <- st_read("./data/countries_shp/TM_WORLD_BORDERS-0.3.shp")

# Gestion of mis matching codes
code_shapes <- shapes$ISO3
code_shapes <- code_shapes[!duplicated(code_shapes)]

wrong_codes <-  unique(wgi_21$code[which(!(wgi_21$code %in% shapes$ISO3))])

shapes_wrong_countries <- c("Andorra", "Democratic Republic of the Congo",
                      "Kosovo", "Romania", "Sudan", "Timor-Leste",
                      "West Bank and Gaza")

for (i in 1:length(wrong_codes)){
  new_code <- shapes[shapes$NAME == shapes_wrong_countries[i], ]$ISO3
  wgi_21$code[wgi_21$code == wrong_codes[i]] <- ifelse (length(new_code) > 0,
        rep(new_code,length(wgi_21$code[wgi_21$code == wrong_codes[i]])),
        wgi_21$code[wgi_21$code == wrong_codes[i]])
}


# Merging our data and countries geometry
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




