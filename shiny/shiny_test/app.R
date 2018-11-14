library(pacman)
p_load(shiny)

ui = fluidPage(
  sliderInput(inputId = "num", #notice small d
              label = "Choose a number",
              value = 25, min = 1, max = 100),
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