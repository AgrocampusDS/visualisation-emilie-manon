library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input,output) {
map.text<-function(output,input){
  output$Title <- renderText(expr = "World Overview of World wide governance Indicator (WGI) score in 2021")
  
  output$Subtitle <- renderText(expr = "WGI measures a country's corruption level. \nThe higher the score is, the less corrupted is a country.")
}
map.text(output,input)
})
