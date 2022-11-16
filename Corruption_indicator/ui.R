library(shiny)
library(shinythemes)
library(leaflet)


ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Corruption Indicator</a>'), id="nav",
             windowTitle = "Corruption Indicator",
             tabPanel(title = "Corruption map",
                      leafletOutput("mymap")),
             tabPanel(title = "Evolution by country"),
             tabPanel(title = "Last plot")
  )
)
