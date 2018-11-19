
#loading/Installing packages
library(pacman)
p_load(shiny, tidyverse)
p_load("stringr","ggplot2","ggExtra","lmerTest", "zoo","lubridate", "data.table")


#not sure if working directory needs to be - I don't think so.
#setwd("~/LONSEA_DB/shiny/proto_shiny") #For


#reading 3 data sets - number of individuals for 3 different units.
total  = read.csv("count_df.csv")
fd = read.csv("count_first_division.csv")
ho = read.csv("count_higher_officials.csv")


#this controls the layout of the appp
ui = fluidPage(
  titlePanel("Lonsea App Prototype"),
  #selectInput("Unit", "Choose which unit you wanna see:", 
  #            choices = c("Higher Officials", "First Division", "Full Secretariat")),
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("range", "Range:",
                  min = 1920, max = 1947,
                  value = c(1920,1947)),
      selectInput("var", 
                  label = "Choose unit to display",
                  choices = c("Total Secretariat", "First Division", "Higher Officials"),
                  selected = "Total Secretariat")
    ), #endsidebarpanel
    
    mainPanel(
      plotOutput(outputId  = "hist")
    )#end mainpanel
  )# end sidebarlayou
  
  
  
)

#this creates a server the feeds information to the layout
server = function(input, output) {
  
output$hist = renderPlot({
  da <- switch(input$var, 
                 "Total Secretariat" = total,
                 "Higher Officials" = ho,
                 "First Division" = fd)
  
  
  
  da %>% 
    filter((X > input$range[1])  & (X < input$range[2])) %>% 
    ggplot(aes(x = X, y = Count, fill = Count)) +
    geom_col() +
    labs( title = "Distribution of Employees", x ="Year",y = "Number of Employees")
    
  

})
}

#runs the app
shinyApp(ui = ui, server = server)



