#---
#title: "R employement development function"
#author: "Adam Finnemann"
#date: "September 26, 2017"
#---
  

library(pacman)
p_load("stringr","ggplot2","ggExtra","lmerTest","tidyverse", "zoo","lubridate", "data.table")

employment <- function(df, interval = "1 month", y_axis = "Workers", x_axis = "Years", title1 = "LoN Workers") {
  
mdl_df <- df %>% 
  select(pname, fname_code,begin_date, end_date) %>% 
  mutate(begin_date = as.Date(begin_date),
         end_date = as.Date(end_date))



setDT(mdl_df)[
  # right join with sequence of monthly intervals
  .(mseq = seq(as.Date("1944-01-01"), length.out = 4L, by = interval)), 
  # using non-equi join conditions
  on = .(begin_date <= mseq, end_date >= mseq)][
    # reshape from wide to long format,
    # show rank (concatenate in case of multiple ranks)
    , dcast(unique(.SD), pname ~ end_date, toString, value.var = "fname_code")]

result <- setDT(mdl_df)[
  .(mseq = seq(as.Date("1919-01-01"), as.Date("1948-12-30"), by = interval)), 
  on = .(begin_date <= mseq, end_date >= mseq), nomatch = 0L, allow.cartesian = TRUE][
    , dcast(unique(.SD), pname ~ end_date, toString, value.var = "fname_code")]



left_out <- df$pname[!(df$pname %in% result$pname)] %>% 
  unique()


removed <- result[,-1]

time_section <- matrix(nrow = (ncol(removed)), ncol = 1)

for (i in 1:(nrow(time_section))){
  time_section[i] = sum(removed[,i, with = F] != "")
}


dates <- as.character(colnames(removed))

plot.df <- cbind(time_section,dates) %>% 
  as.data.frame() %>% 
  mutate(dates = as.Date(dates),
         V1 = as.numeric(as.character(V1))) %>% 
  rename(Section_Workers = V1)

plot <- plot.df %>% 
  ggplot(aes(dates, Section_Workers, color = Section_Workers)) +
  geom_col()+
  labs(x = x_axis, y =y_axis, titles = title1)+
  scale_x_date(date_breaks = "3 year", date_labels = "%Y")

return(plot)
}
