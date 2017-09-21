


list.of.packages <- c("stringr","ggplot2","ggExtra","plyr","lme4", "nlme","tidyverse", "zoo","lubridate", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}

#Analyzing directors
setwd("~/Historie projekt/historie project/LONSEA_DB/fall17")

da <- read.csv("lon_data_w_GA.csv", sep = ",", header = T, stringsAsFactors = FALSE, na.strings=c("","NA"))

directors <- da %>% 
  filter(str_detect(da$fname, "Director")) %>% 
  select(pname) %>% 
  unique() %>% 
  as.list()

  
direc <- da %>% 
filter(da$pname %in% directors$pname) 

 direc<- direc %>% 
  mutate(nationality = as.factor(nationality))

 
 
  no_dub <- direc %>% 
  filter(!duplicated(pname)) 
summary(no_dub$nationality)  
  
x1 <- da$begin_on_year
x2 <- da$begin_on_month
x_1 <- x1 + x2/12# + x3/30
da <- da %>%
  mutate(entry_time = x_1)


da <- direc
# import data frame of rank names to ordinal scale
setwd("~/Historie projekt/historie project/LONSEA_DB")
ordinal_ranks <- read.csv("data/fname_u_with_class.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE)# changed original category csv due to encoding
# column names
colnames(ordinal_ranks) = c("u_id", "u_fname" , "u_fname_code")
# code every object in main
extract_rank <- rep(NA,nrow(da))

for(i in 1:nrow(ordinal_ranks)){
  #for each rank a logical vector indexing subject numbers are created
  idx <- ordinal_ranks$u_fname[i] == da$fname
  #true index are translated to oridnal rank numbers for each subject
  extract_rank[idx] <- ordinal_ranks$u_fname_code[i]
}
#ordinal rank numbers are added
da$fname_code <- extract_rank
da <- da %>%
  filter(begin_on_year > 1900) %>% 
  filter(fname_code != 0)

#reversing fname_code order
da1 <- da
for (i in 1:nrow(da)) {
  da1$fname_code[i] <- 10-da$fname_code[i]}
da <- da1 %>% 
  mutate(pname = as.factor(pname))

da <- da %>% 
  filter(begin_on_year > 1900 & end_on_year >1900)
da$begin_date <- as.Date(with(da, paste(begin_on_year, begin_on_month, begin_on_day,sep="-")), "%Y-%m-%d")
da$end_date <- as.Date(with(da, paste(end_on_year, end_on_month, end_on_day,sep="-")), "%Y-%m-%d")

da <- da %>% 
  filter(!is.na(da$begin_date) & !is.na(da$end_date))  

da$pname <- droplevels(da$pname)

da$begin_year <- as.Date(with(da, paste(begin_on_year,sep="-")), "%Y")
da$end_year <- as.Date(with(da, paste(end_on_year,sep="-")), "%Y")


mdl_df <- da %>% 
  select(pname, fname_code,begin_year, end_year)



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

contract_length <- matrix(nrow = (nrow(result)), ncol = 1) #minus 1 due to a name row

result <- as.data.frame(result)

for (i in 1:(nrow(contract_length))){
  contract_length[i] = sum(result[i,] != "")
}


names <- as.character(result[,1])
plot.df <- cbind(contract_length,names) %>% 
  as.data.frame() %>% 
  mutate(V1 = as.numeric(V1),
         names = as.factor(names))
colnames(plot.df) <- c("Years","Name")

#gettig names and nationalities
nats <- da %>% 
filter(!duplicated(pname)) %>% 
  select(pname,nationality)

colnames(nats) <- c("Name","Nationality")

#merging nationalities and plot.df based on common names
test <- merge(nats, plot.df)



test %>% ggplot(aes(x = Years, color = Years))+ 
  geom_density()

  
test %>% 
  filter(Nationality == "French"| Nationality == "Swiss"| Nationality == "English") %>% 
  ggplot(aes(x = Years, color = Years))+ 
  geom_density(aes(group = Nationality, color = Nationality))


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




  
