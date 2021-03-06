
#loading/Installing packages
library(pacman)
p_load(shiny, tidyverse)
p_load("stringr","ggplot2","ggExtra","lmerTest", "zoo","lubridate", "data.table")


#not sure if working directory needs to be - I don't think so.
setwd("~/LONSEA_DB/shiny/proto_shiny") #For
df  = read.csv("lon_data.csv")
df %>% 
  mutate(begin_date = as.Date(begin_date),
         end_date = as.Date(end_date),
         gender = replace(gender, gender == -1,0), #change the two -1 to males, their names are male
         gender =as.factor(gender),
         gender =recode(gender,"0"="Male","1"="Female")) -> df #the full data set


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
         canonical_fname != "Second Division") %>% 
  droplevels()-> higher_officials #data set containing only the higher officials

#this controls the layout of the appp


ui = fluidPage(
  
  navbarPage("Lonsea Shiny App", #creates multiple components within the app, a component/Page is created with "tabpanel()"
             tabPanel("Component 1: General features",  #this section defines the first page
                      
                      
                      titlePanel("General features"),
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          sliderInput("range", "Range:", #adds a slider for years
                                      min = 1919, max = 1948,
                                      value = c(1919,1948)),
                          
                          selectInput("var", #adds dropdown for unit to be displayed
                                      label = "Choose unit to display",
                                      choices = c("Total Secretariat", "First Division", "Higher Officials"),
                                      selected = "Total Secretariat"),
                          
                          checkboxGroupInput(inputId = "cbox_gender", label = "choose gender", #adds checkbox for gender
                                             choices = c("All",levels(df$gender)),
                                             selected = "All"),
                          
                          sliderInput("age_interval",label = "interval size for age distribution (i.e. the width of each bar" ,
                                             min = 0, max = 20, value = 1)
                          
                        ), #endsidebarpanel
                        
                        mainPanel(
                          
                          tabsetPanel(type = "tabs",
                                      tabPanel("Period histogram", plotOutput(outputId  = "hist")),
                                      tabPanel("Age distribution", plotOutput(outputId  = "hist2"))
                          ),
                                      
                          
                          fluidRow( #first row beneath the main plot, containing the following three inputs
                            column(3,
                                   h4("Nationality control"),
                                   
                                   checkboxGroupInput(inputId = "checkbox", label = "choose nationalities", #adds checkbox for nationalities
                                                      choices = c("All",levels(df$nationality)),
                                                      selected = "All")
                            ),
                            
                            column(4,
                                   h4("Higher offical control"),
                                   checkboxGroupInput(inputId = "cbox_higher_off", label = "choose higher officials oname (sections),
                                                      this only works if higher offcials is chosen in the 'unit to display'", #adds checkbox for higher officlas
                                                      choices = c("All",levels(higher_officials$oname)),
                                                      selected = "All")),
                            column(4,
                                   h4("Fname control"),
                                   checkboxGroupInput(inputId = "cbox_higher_fname", label = "choose higher officials fname,
                                  this only works if higher offcials is chosen in the 'unit to display'", #adds checkbox for higher officlas
                                                      choices = c("All",levels(higher_officials$fname)),
                                                      selected = "All"))
                          ) #end fluid row
                          
                        )#end mainpanel
                        
                          ) #end sidebar layout
                      ), #ends page 1
             
             tabPanel("Component 2: Still empty")
                        
                        
                      
                      
             
             
  
             ) #ends navbar page
  )# end fluid page



####################################################################### SERVER ############################################################

#this creates a server the feeds information to the layout

server = function(input, output) {
  
  dataframe <- reactive({
    
  
    
    if(input$var == "First Division"){ 
      df %>% 
        filter(canonical_fname == "LoN First Division") -> df
    }
    
    
    
    
    if(input$var == "Higher Officials"){
      df = higher_officials
      
      if(input$cbox_higher_off != "All"){ #furthermore, if the cbox_higher_off argument is different from "all", then a subset based on "oname" is filtered.
        df %>% 
          filter(oname %in% input$cbox_higher_off) -> df
      }
      if(input$cbox_higher_fname != "All"){ #furthermore, if the cbox_higher_off argument is different from "all", then a subset based on "oname" is filtered.
        df %>% 
          filter(fname %in% input$cbox_higher_fname) -> df
      }
    }
    
    
    if(input$checkbox != "All"){
      df %>% 
        filter(nationality %in% input$checkbox) -> df
    }
    
    if(input$cbox_gender != "All"){
      df %>% 
        filter(gender %in% input$cbox_gender) -> df
    }
    
    df
    
  })
  
    
    
    
  
  
    
    output$hist2 = renderPlot({ #updates the age histogram in tab 2
      
         dataframe() %>%  #filter people with born on year, and culculates their age at beginning of their contract
           filter(!is.na(born_on_year)) %>%
           mutate(age_contract_begin = begin_on_year - born_on_year)-> df2 #it calculates their age on the 1. of january the year they sign the contract.

      bar_width = seq(10,80, by = input$age_interval) 
      
      df2 %>%
         ggplot(aes(age_contract_begin)) +
         geom_histogram(breaks = bar_width,
                        aes(fill=..count..)) +
         labs( title = "For each contract, the age of the contract owner at the beginning of the contract")
    
  })
  
  

    
    
  
  
  output$hist = renderPlot({
    #print(dataframe())
    #Filters the relevant part of the secretariat based on input$var


     dataframe() %>%
      #df %>% 
       #select relevant variables for time table
       select(pname, fname_code,begin_date, end_date) -> res
     # the following function creates time table
     #the "By" argument specifies the precision of the time. alternative argument can be "1 month"
     #by chagning "pname" to "nationality" a nationality based time table can be created.
     setDT(res)[
       .(mseq = seq(as.Date("1919-01-01"), as.Date("1948-12-30"), by = "year")),
       on = .(begin_date <= mseq, end_date >= mseq), nomatch = 0L, allow.cartesian = TRUE][
         , dcast(unique(.SD), pname ~ end_date, toString, value.var = "fname_code")] %>%
       left_join( select(df, pname,nationality)) %>%
       filter(!duplicated(.)) %>%
       select(-nationality, - pname) -> result_nats
     
     
     count = as.data.frame(colSums(result_nats != "")) %>% #for each col (year), count number of non-empty cells
       rownames_to_column() %>% 
       mutate(rowname = str_extract_all(rowname,pattern = "[0-9]{4}"), #change the first row form long year format, to just the
              rowname = as.numeric(rowname))
     
     names(count) = c("rowname","Count")
     
     
     count %>%
       filter((rowname > input$range[1])  & (rowname < input$range[2])) %>%
       ggplot(aes(x = rowname, y = Count, fill = Count)) +
       geom_col() +
       labs( title = paste("Distribution of Employees,", "Nationality = ",input$checkbox), x ="Year",y = "Number of Employees")

    
  })
}


#runs the app
shinyApp(ui = ui, server = server)



