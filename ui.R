library(shiny)
library(shinythemes)
library(leaflet)


ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Corruption Indicator</a>'), id="nav",
             windowTitle = "Corruption Indicator",
             tabPanel(title = "World Corruption Overview",align = 'center',
                      textOutput("Title"),
                      tags$head(tags$style("#Title{color: black;
                                 font-size: 25px;
                                 font-style: bold;
                                 }"
                      )
                      ),
                      textOutput("Subtitle"),
                      tags$head(tags$style("#Sutitle{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                      )
                      ),
                      img(src="word_indicators.png",height="70%", width="70%", align="center")),
             tabPanel(title = "Corruption Evolution in the US",align = 'center',
                      img( src = "graph_evolution.png",height="80%", width="80%")
                      ),
             tabPanel(title = "Indicators Repartition in the US",align = 'center',
                        img(src="indicators_repartition.png",height="80%", width="80%", align="center")
                        ### the rest of your code
                      )
  )
)
