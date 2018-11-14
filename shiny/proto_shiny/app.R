library(pacman)
p_load(shiny, tidyverse)

p_load("stringr","ggplot2","ggExtra","lmerTest", "zoo","lubridate", "data.table")


ui = fluidPage(
  sliderInput(inputId = "num", #notice small d
              label = "Choose a number",
              value = 1920, min = 1920, max = 1948),
  plotOutput(outputId  = "hist")
)

server = function(input, output) {
  
output$hist = renderPlot({
  setwd("~/LONSEA_DB/shiny/shiny_test")
  da  = read.csv("")
  hist(rnorm(input$num)) #input$num gets its value from the num inputiD
})
}

shinyApp(ui = ui, server = server)



