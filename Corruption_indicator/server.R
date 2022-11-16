library(shiny)
source("C:/Users/CamPc/Documents/ACO/3A/visualisation-emilie-manon/src/corruption_map.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    corruption_map(input,output)
})
