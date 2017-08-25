list.of.packages <- c("ggplot2","ggExtra","plyr","lme4", "nlme","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}
#"""
# add coded fnames (classes) to main data frame
# model individual career path for each member using a mixed linear model in foward model selection
#
#"""
lon.df <- read.csv("C:\\Users\\Adam\\Documents\\Historie projekt\\historie project\\LONSEA_DB\\fall17\\five_nationalities_data.csv", sep = ",", header = T, stringsAsFactors = FALSE, na.strings=c("","NA"))

#removing nas and turning variable into a factor
lon.df <- lon.df %>%
  filter(!is.na(nationality)) %>% 
  mutate(nationality = as.factor(nationality))

#removing members with begin date before 1919
lon.df <- lon.df %>% 
  filter(begin_on_year > 1918)

#removing double nationalities
lon.df <- lon.df %>% 
  filter(is.na(str_match(lon.df$nationality,",")))  #rows with "," indicating double nationality is removed
  
lon.df <- droplevels(lon.df)



plot.df <-lon.df %>% 
  select(nationality,begin_on_year) %>% 
  group_by(begin_on_year,nationality) %>% 
  summarise( count = n())
  
   
   plot.df %>% 
   ggplot(aes(begin_on_year, count, fill = count))+
   geom_col() +
  labs(x ="Year", y= "count of promotions and employments", title = "Begin_dates per year" )+
  facet_grid(nationality~.)
   
   plot.df %>% 
     ggplot(aes(begin_on_year, count, fill = count))+
     geom_col() +
     labs(x ="Year", y= "count of promotions and employments", title = "Begin_dates per year" )+
     facet_grid(nationality~., scales = "free")     
   
#plotting only first division
   plot.1division.df <-lon.df %>% 
     filter(canonical_fname == "First Division") %>% 
     select(nationality,begin_on_year) %>% 
     group_by(begin_on_year,nationality) %>% 
     summarise( count = n())

   plot.1division.df %>% 
     ggplot(aes(begin_on_year, count, fill = count))+
     geom_col() +
     labs(x ="Year", y= "count of promotions and employments", title = "FIRST DIVISON: Begin_dates per year" )+
     facet_grid(nationality~., scales = "free")     
   
#plotting densities

#calculating numbers of total begin_dates for each year
 totals <- plot.df %>% 
    group_by(begin_on_year) %>% 
    summarise(total = sum(count))
#creating a list of proportions in a very unelegent way
 p = 1:nrow(plot.df)

for (i in 1:nrow(plot.df)){
  for (j in 1:nrow(totals)){
    if (plot.df[i,1] == totals[j,1]){
      p[i] = plot.df[i,3] / totals[j,2]
      }
  }
  
}  
#adding the list of proportions to the df and plot it
plot.df$proportions <- as.numeric(p)

   plot.df %>% 
     ggplot(aes(x = begin_on_year,y = proportions, color = nationality))+
     geom_point()+
     geom_line() +
     labs(x = "Year", y ="proportions of begin_dates", title ="Whole Secretariat included")
   
#creating subsets of divisions
   proportion.1division.df <-lon.df %>% 
     filter(canonical_fname == "First Division") %>% 
     select(nationality,begin_on_year) %>% 
     group_by(begin_on_year,nationality) %>% 
     summarise( count = n())
   
   prop.1div.total <- proportion.1division %>% 
     group_by(begin_on_year) %>% 
     summarise(total = sum(count))
   
   p = 1:nrow(proportion.1division.df)
   
   for (i in 1:nrow(proportion.1division.df)){
     for (j in 1:nrow(prop.1div.total)){
       if (proportion.1division.df[i,1] == prop.1div.total[j,1]){
         p[i] = proportion.1division.df[i,3] / prop.1div.total[j,2]
       }
     }
     
   }  
   #adding the list of proportions to the df and plot it
   proportion.1division.df$proportions <- as.numeric(p)
   
   proportion.1division.df %>% 
     ggplot(aes(x = begin_on_year,y = proportions, color = nationality))+
     geom_point()+
     geom_line() +
     labs(x = "Year", y ="proportions of begin_dates", title ="1 division")
   
  
   #proportions for 2. division
   
   proportion.2division.df <-lon.df %>% 
     filter(canonical_fname == "Second Division") %>% 
     select(nationality,begin_on_year) %>% 
     group_by(begin_on_year,nationality) %>% 
     summarise( count = n())
   
   prop.2div.total <- proportion.2division.df %>% 
     group_by(begin_on_year) %>% 
     summarise(total = sum(count))
   
   p = 1:nrow(proportion.2division.df)
   
   for (i in 1:nrow(proportion.2division.df)){
     for (j in 1:nrow(prop.2div.total)){
       if (proportion.2division.df[i,1] == prop.2div.total[j,1]){
         p[i] = proportion.2division.df[i,3] / prop.2div.total[j,2]
       }
     }
     
   }  
   #adding the list of proportions to the df and plot it
   proportion.2division.df$proportions <- as.numeric(p)
   
   proportion.2division.df %>% 
     ggplot(aes(x = begin_on_year,y = proportions, color = nationality))+
     geom_point()+
     geom_line() +
     labs(x = "Year", y ="proportions of begin_dates", title ="Second division")
   
   #proportions for 3. division
   
   proportion.3division.df <-lon.df %>% 
     filter(canonical_fname == "Third Division") %>% 
     select(nationality,begin_on_year) %>% 
     group_by(begin_on_year,nationality) %>% 
     summarise( count = n())
   
   prop.3div.total <- proportion.3division.df %>% 
     group_by(begin_on_year) %>% 
     summarise(total = sum(count))
   
   p = 1:nrow(proportion.3division.df)
   
   for (i in 1:nrow(proportion.3division.df)){
     for (j in 1:nrow(prop.3div.total)){
       if (proportion.3division.df[i,1] == prop.3div.total[j,1]){
         p[i] = proportion.3division.df[i,3] / prop.3div.total[j,2]
       }
     }
     
   }  
   #adding the list of proportions to the df and plot it
   proportion.3division.df$proportions <- as.numeric(p)
   
   proportion.3division.df %>% 
     ggplot(aes(x = begin_on_year,y = proportions, color = nationality))+
     geom_point()+
     geom_line() +
     labs(x = "Year", y ="proportions of begin_dates", title ="Third division")
   