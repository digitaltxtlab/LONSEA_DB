list.of.packages <- c("ggplot2","ggExtra","plyr","lme4", "nlme","tidyverse", "zoo","lubridate", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}

#Analyzing employment and contract length for first division for 5 nationaltiies


setwd("~/Historie projekt/historie project/LONSEA_DB/fall17/five_nations")
da <- read.csv("reversedfcode_nation5.wo_temp_fcode0.w_GA_rank_intervals.csv")
#looking at first division
da <- da %>% 
  filter(canonical_fname == "First Division")


da <- da %>% 
  filter(!is.na(da$begin_date) & !is.na(da$end_date))  

da$pname <- droplevels(da$pname)

mdl_df <- da %>% 
  select(pname, fname_code,begin_date, end_date) %>% 
  mutate(begin_date = as.Date(begin_date),
         end_date = as.Date(end_date))



setDT(mdl_df)[
  # right join with sequence of monthly intervals
  .(mseq = seq(as.Date("1944-01-01"), length.out = 4L, by = "1 month")), 
  # using non-equi join conditions
  on = .(begin_date <= mseq, end_date >= mseq)][
    # reshape from wide to long format,
    # show rank (concatenate in case of multiple ranks)
    , dcast(unique(.SD), pname ~ end_date, toString, value.var = "fname_code")]

result <- setDT(mdl_df)[
  .(mseq = seq(as.Date("1919-01-01"), as.Date("1948-12-30"), by = "1 month")), 
  on = .(begin_date <= mseq, end_date >= mseq), nomatch = 0L, allow.cartesian = TRUE][
    , dcast(unique(.SD), pname ~ end_date, toString, value.var = "fname_code")]


#calculating employment
values <- matrix(nrow = (ncol(result)-1), ncol = 1) #minus 1 due to a name row

result <- as.data.frame(result)

for (i in 2:(nrow(values)-1)){
  values[i-1] = sum(result[,i] != "")
}


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
  labs(x = "Years", y = "Number of employees", title = "First Division: Number of employees")



#Calculating contract length

contract_length <- matrix(nrow = (nrow(result)), ncol = 1) #minus 1 due to a name row

result <- as.data.frame(result)

for (i in 1:(nrow(contract_length))){
  contract_length[i] = sum(result[i,] != "")
}


names <- as.character(result[,1])

plot.df <- cbind(contract_length,names) %>% 
as.data.frame() %>% 
  mutate(V1 = as.numeric(as.character(V1)),
         names = as.factor(names))
  
colnames(plot.df) <- c("Months","Name")

#gettig names and nationalities
nats <- da %>% 
  filter(!duplicated(pname)) %>% 
  select(pname,nationality)

colnames(nats) <- c("Name","Nationality")

#merging nationalities and plot.df based on common names
plot_nat.df <- merge(nats, plot.df)


plot_nat.df %>% 
  ggplot(aes(x = Months, color = Months))+ 
  geom_density(aes(group = Nationality, color = Nationality)) +
  labs(x = "Months", y = "Density of contract lengths", title = "First Division: Contract Lengths For 5 Regions")+
  scale_x_continuous(breaks = seq(0,350, 12))

plot_nat.df %>% 
  ggplot(aes(x = Months, fill = Nationality,  color = Nationality))+ 
  geom_bar(width = 1)+
  labs(x = "Months", y = "Numbber of contract lengths", title = " All divisions: Contract Lengths For 5 Regions")+
  scale_x_continuous(breaks = seq(0,350, by = 12))

