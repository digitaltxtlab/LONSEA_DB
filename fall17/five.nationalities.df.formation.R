#lon_data.csv updating nations to five regions
setwd("~/Historie projekt/historie project/LONSEA_DB/fall17")


lon.df <- read.csv("lon_data_w_GA.csv", sep = ",", header = T, stringsAsFactors = FALSE, na.strings=c("","NA"))

#removing NAs and temporary collaborators
lon.df <- lon.df %>%
  filter(!is.na(nationality)) %>% 
  filter(!fname == "Temporary Collaborator")
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

#write.csv(five_nat.df1, file = "nation5.wo_temp.w_GA.csv")
