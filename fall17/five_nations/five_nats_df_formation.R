list.of.packages <- c("stringr","ggplot2","ggExtra","plyr","lme4", "nlme","tidyverse", "zoo","lubridate", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}

#lon_data.csv updating nations to five regions
setwd("~/Historie projekt/historie project/LONSEA_DB/fall17")
lon.df <- read.csv("lon_data_w_GA.csv", sep = ",", header = T, stringsAsFactors = FALSE, na.strings=c("","NA"))

#removing NAs and temporary collaborators
lon.df <- lon.df %>%
  filter(!is.na(nationality)) %>% 
  filter(!fname == "Temporary Collaborator") %>% 
  mutate(nationality = as.factor(nationality))


  
#doublelt same nationality and mispellings corrected
lon.df$nationality <- str_replace_all(lon.df$nationality,", Swiss", "Swiss")
lon.df$nationality <- str_replace_all(lon.df$nationality,"BritishSwiss", "British, Swiss")
lon.df$nationality <- str_replace_all(lon.df$nationality,"English, Dane English,", "English, Dane")
lon.df$nationality <- str_replace_all(lon.df$nationality,"Fench  French,", "French")
lon.df$nationality <- str_replace_all(lon.df$nationality,"LatvianSwiss", "Latvian, Swiss")
lon.df$nationality <- str_replace_all(lon.df$nationality,"AlbanianSwiss", "Albanian, Swiss")
lon.df$nationality <- str_replace_all(lon.df$nationality,"Dane, Dane", "Dane")
lon.df$nationality <- str_replace_all(lon.df$nationality,"French, BritishSwiss", "French, British, Swiss")
lon.df$nationality <- str_replace_all(lon.df$nationality,"SwissSwiss", "Swiss")
lon.df$nationality <- str_replace_all(lon.df$nationality,"British, British", "British")
lon.df$nationality <- str_replace_all(lon.df$nationality,"French, French", "French")
lon.df$nationality <- str_replace_all(lon.df$nationality,"ItalianSwiss", "Italian, Swiss")
lon.df$nationality <- str_replace_all(lon.df$nationality,"SwissSwissSwiss", "Swiss")
lon.df$nationality <- str_replace_all(lon.df$nationality,"RussianSwiss", "Russian, Swiss")
lon.df$nationality <- str_replace_all(lon.df$nationality,"Italian, Italian", "Italian")
lon.df$nationality <- str_replace_all(lon.df$nationality,"DutchSwiss", "Dutch, Swiss")
lon.df$nationality <- str_replace_all(lon.df$nationality,"SwissSwiss", "Swiss")
lon.df$nationality <- str_replace_all(lon.df$nationality,"FrenchSwiss", "French, Swiss")
lon.df <- lon.df %>% 
  mutate(nationality = as.factor(nationality))
summary(lon.df$nationality)


five_nat.df <- lon.df

five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Dane", "Scandinavian")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Dane", "Scandinavian")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Swedish", "Scandinavian")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Norwegian", "Scandinavian")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Brazilian\t", "South American")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Uruguayan", "South American")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Peruvian", "South American")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Argentinian", "South American")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Chilean", "South American")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Venezuelan\t", "South American")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Paraguayan", "South American")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Ecuadorian", "South American")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"Bolivian", "South American")
five_nat.df$nationality <-str_replace_all(five_nat.df$nationality,"English", "British")         
summary(as.factor(five_nat.df$nationality))

five_nat.df1 <- five_nat.df %>% 
  filter(nationality == "British"|nationality == "South American"| nationality == "Scandinavian"| nationality == "Swiss"| nationality == "French")

da <- five_nat.df1
#replacing missing days
da <- da %>%
  mutate(begin_on_day = replace(begin_on_day, is.na(begin_on_day), 1),
         end_on_day = replace(end_on_day, is.na(end_on_day), 1))

#adding entry times (Kristoffer left over)
x1 <- da$begin_on_year
x2 <- da$begin_on_month
x_1 <- x1 + x2/12# + x3/30
da <- da %>%
  mutate(entry_time = x_1)


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

#adding contract periods
da$begin_date <- as.Date(with(da, paste(begin_on_year, begin_on_month, begin_on_day,sep="-")), "%Y-%m-%d")
da$end_date <- as.Date(with(da, paste(end_on_year, end_on_month, end_on_day,sep="-")), "%Y-%m-%d")

#reversing fname_code order
da1 <- da
for (i in 1:nrow(da)) {
da1$fname_code[i] <- 10-da$fname_code[i]}
#write.csv(da1,"reversedfcode_nation5.wo_temp_fcode0.w_GA_rank_intervals.csv")

