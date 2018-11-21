
#loading/Installing packages
library(pacman)
p_load(shiny, tidyverse)
p_load("stringr","ggplot2","ggExtra","lmerTest", "zoo","lubridate", "data.table")


#not sure if working directory needs to be - I don't think so.
#setwd("~/LONSEA_DB/shiny/proto_shiny") #For


#reading 3 data sets - number of individuals for 3 different units.
setwd("~/LONSEA_DB/shiny/proto_shiny")
df  = read.csv("lon_data.csv")


#this controls the layout of the appp
ui = fluidPage(
  titlePanel("Lonsea App Prototype"),
  #selectInput("Unit", "Choose which unit you wanna see:", 
  #            choices = c("Higher Officials", "First Division", "Full Secretariat")),
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("range", "Range:",
                  min = 1919, max = 1948,
                  value = c(1919,1948)),
      selectInput("var", 
                  label = "Choose unit to display",
                  choices = c("Total Secretariat", "First Division", "Higher Officials"),
                  selected = "Total Secretariat"),
      selectInput("nat", 
                  label = "Choose nationalities",
                  choices = c("All",levels(df$nationality)),
                  selected = "All")
    ), #endsidebarpanel
    
    mainPanel(
      plotOutput(outputId  = "hist")
    )#end mainpanel
  )# end sidebarlayou
  
  
  
)

#this creates a server the feeds information to the layout
server = function(input, output) {
  
  output$hist = renderPlot({
    
    #Filters the relevant part of the secretariat based on input$var
    if(input$var == "First Division"){
    df %>% 
        filter(canonical_fname == "LoN First Division") -> df
    }
    if(input$var == "Higher Officials"){
      df %>% 
      filter(str_detect(fname, "Section")| #including "Section"
               str_detect(fname, "Director")| #Including Directors
               str_detect(fname, "Chief") | #Including Chief
               str_detect(fname, "Expert")|
               str_detect(fname, "Head of") |
               str_detect(fname, "Section"),
             fname != "Secretary of Section",
             oname != "Treasury",
             oname != "Library",
             canonical_fname != "Second Division") -> df
    }
    
    
    if(input$nat != "All"){
      df %>% 
        filter(nationality == input$nat) -> df
    }
    
    
    mdl_df <- df %>% 
      select(pname, fname_code,begin_date, end_date) %>% 
      mutate(begin_date = as.Date(begin_date),
             end_date = as.Date(end_date))
    
    
    #the "By" argument specifies the precision of the time. alternative argument "1 month"
    result <- setDT(mdl_df)[
      .(mseq = seq(as.Date("1919-01-01"), as.Date("1948-12-30"), by = "year")), 
      on = .(begin_date <= mseq, end_date >= mseq), nomatch = 0L, allow.cartesian = TRUE][
        , dcast(unique(.SD), pname ~ end_date, toString, value.var = "fname_code")]
    
    result_nats <- left_join(result, select(df, pname,nationality)) %>% 
      filter(!duplicated(.))
    
    count = result_nats[,c(2:(length(result_nats)-1))]
    
    count = as.data.frame(colSums(count != ""))
    names(count) = "Count"
    
    rownames(count) = str_extract_all(rownames(count),pattern = "[0-9]{4}")  
    
    count = rownames_to_column(count)
    
    count %>% 
      filter((rowname > input$range[1])  & (rowname < input$range[2])) %>% 
      ggplot(aes(x = rowname, y = Count, fill = Count)) +
      geom_col() +
      labs( title = paste("Distribution of Employees,", "Nationality = ",input$nat), x ="Year",y = "Number of Employees")
    
    
    
  })
}

#runs the app
shinyApp(ui = ui, server = server)



