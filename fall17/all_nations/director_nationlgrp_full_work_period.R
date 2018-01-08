list.of.packages <- c("stringr","ggplot2","ggExtra","plyr","lme4", "nlme","tidyverse", "zoo","lubridate", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}

#Analyzing directors
setwd("~/Historie projekt/historie project/LONSEA_DB/fall17/all_nations")

da <- read.csv("reversedfcode_all.wo_temp_fcode0.w_GA_rank_intervals.csv", sep = ",", header = T, stringsAsFactors = FALSE, na.strings=c("","NA"))

directors <- da %>% 
  filter(!is.na(str_match(da$fname, "Director"))) %>% 
  select(pname) %>% 
  unique() %>% 
  as.list()

  
direc <- da %>% 
filter(da$pname %in% directors$pname) 

 direc<- direc %>% 
  mutate(nationality = as.factor(nationality),
         pname <- as.factor(pname))


#not sure if this is necessary, basically it just converts dates
direc$begin_year <- as.Date(with(direc, paste(begin_on_year,sep="-")), "%Y")
direc$end_year <- as.Date(with(direc, paste(end_on_year,sep="-")), "%Y")


mdl_df <- direc %>% 
  select(pname, fname_code,begin_year, end_year)
  
filter(!is.na(mdl_df$begin_year) & !is.na(mdl_df$end_year))

setDT(mdl_df)[
  # right join with sequence of monthly intervals
  .(mseq = seq(as.Date("1944-01-01"), length.out = 4L, by = "1 year")), 
  # using non-equi join conditions
  on = .(begin_year <= mseq, end_year >= mseq)][
    # reshape from wide to long format,
    # show rank (concatenate in case of multiple ranks)
    , dcast(unique(.SD), pname ~ end_year, toString, value.var = "fname_code")]

result <- setDT(mdl_df)[
  .(mseq = seq(as.Date("1919-01-01"), as.Date("1948-12-30"), by = "1 year")), 
  on = .(begin_year <= mseq, end_year >= mseq), nomatch = 0L, allow.cartesian = TRUE][
    , dcast(unique(.SD), pname ~ end_year, toString, value.var = "fname_code")]

#counting contract length

contract_length <- matrix(nrow = (nrow(result)), ncol = 1)

result <- as.data.frame(result)

for (i in 1:(nrow(contract_length))){
  contract_length[i] = sum(result[i,] != "")
}


names <- as.character(result[,1])
plot.df <- cbind(contract_length,names) %>% 
  as.data.frame() %>% 
  mutate(V1 = as.numeric(as.character(V1)),
         names = as.factor(names))
colnames(plot.df) <- c("Years","Name")

#gettig names and nationalities
nats <- da %>% 
filter(!duplicated(pname)) %>% 
  select(pname,nationality)

colnames(nats) <- c("Name","Nationality")

#merging nationalities and plot.df based on common names
test <- merge(nats, plot.df)

#Seperating into regions
seven_nat_direc <- test

seven_nat_direc$Nationality <-str_replace_all(seven_nat_direc$Nationality,"Dane", "Scandinavian")
seven_nat_direc$Nationality <-str_replace_all(seven_nat_direc$Nationality,"Swedish", "Scandinavian")
seven_nat_direc$Nationality <-str_replace_all(seven_nat_direc$Nationality,"Norwegian", "Scandinavian")
seven_nat_direc$Nationality <-str_replace_all(seven_nat_direc$Nationality,"British", "English")
seven_nat_direc$Nationality <-str_replace_all(seven_nat_direc$Nationality,"Canadian", "North American")
seven_nat_direc$Nationality <-str_replace_all(seven_nat_direc$Nationality,"US-American", "North American")
seven_nat_direc$Nationality <-str_replace_all(seven_nat_direc$Nationality,"Czech", "Others")
seven_nat_direc$Nationality <-str_replace_all(seven_nat_direc$Nationality,"Dutch", "Others")
seven_nat_direc$Nationality <-str_replace_all(seven_nat_direc$Nationality,"Greek", "Others")
seven_nat_direc$Nationality <-str_replace_all(seven_nat_direc$Nationality,"Italian,Italian", "Italian")
seven_nat_direc$Nationality <-str_replace_all(seven_nat_direc$Nationality,"Polish", "Others")
seven_nat_direc$Nationality <-str_replace_all(seven_nat_direc$Nationality,"Spanish", "Others")  

#plotting

seven_nat_direc %>% 
  ggplot(aes(Years, fill = Nationality)) +
  geom_bar(width = 1) +
  scale_x_continuous(breaks = seq(0,30, by = 2))+
  labs(x = "Length in years", y = "Number of work periods", title = "Directors: Distribution of total work periods")

seven_nat_direc %>% 
  filter(!Nationality == "Japanese") %>% 
  filter(!Nationality == "North American") %>% 
  filter(!Nationality == "Others") %>% 
  ggplot(aes(Years, color = Nationality)) +
  geom_density()+
  scale_x_continuous(breaks = seq(0,30, by = 2))+
  labs(x = "Length in years", y = "Number of work periods", title = "Directors: Distribution of total work periods")


result <- as.data.frame(result)

for (i in 2:(nrow(values)-1)){
  values[i-1] = sum(result[,i] != "")
}

da <- da1

dates <- seq(as.Date("1919-02-01"), as.Date("1948-04-01"), by = "1 month") %>% 
  as.character()


plot.df <- cbind(dates,values) %>% 
  as.data.frame() %>%
  mutate(V2 = as.numeric(as.character(V2)),
         dates = as.Date(dates)) %>% 
  filter(!is.na(V2))
colnames(plot.df) <- c("date","count")


plot.df %>% 
  ggplot(aes(x = date, y = count, color = count)) +
  geom_col()+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  labs(x = "Years", y = "Number of employees", title = " month total")



  
