---
  title: "R markdown template""
author: "Adam Finnemann"
date: "September 24, 2017"
---
  
```{r setup, include=FALSE}

library(pacman)
p_load("stringr","ggplot2","ggExtra","lmerTest","tidyverse", "zoo","lubridate", "data.table")



setwd("~/Historie projekt/historie project/LONSEA_DB/fall17")

da <- read.csv("lon_data_w_GA.csv")

```



Correcting nationalities of directors, and aligning namings:
```{r}

#Nationalities assignments for Directors
indx <- da$pname == "Dr. Jur. Thanassis (Athanase) Aghnides"
da[indx,15] <- "Greek"

indx <- da$pname == "Erik Andreas Colban"
da[indx,15] <- "Norwegian"


indx <- da$pname == "Juan Antonio Buero"
da[indx,15] <- "Uruguayan"

indx<- da$pname == "Yotaro Sugimura"
da$nationality <- as.character(da$nationality)
da[indx,15] <- "Japanese"
da$nationality <- as.factor(da$nationality)

indx <- da$pname == "Prof. Inazo NitobÃ©"
da[indx,3] <- "Inazo Nitobe"
da[indx,4] <- 0
da[indx,15] <- "Japanese"
indx <- da$pname == "Inazo Nitobe"
da[indx,15] <- "Japanese"

```


Aligning nationalities asignments:

```{r}
da$nationality <- str_replace_all(da$nationality,", Swiss", "Swiss")
da$nationality <- str_replace_all(da$nationality,"BritishSwiss", "British, Swiss")
da$nationality <- str_replace_all(da$nationality,"English, Dane English,", "English, Dane")
da$nationality <- str_replace_all(da$nationality,"Fench  French,", "French")
da$nationality <- str_replace_all(da$nationality,"LatvianSwiss", "Latvian, Swiss")
da$nationality <- str_replace_all(da$nationality,"AlbanianSwiss", "Albanian, Swiss")
da$nationality <- str_replace_all(da$nationality,"Dane, Dane", "Dane")
da$nationality <- str_replace_all(da$nationality,"French, BritishSwiss", "French, British, Swiss")
da$nationality <- str_replace_all(da$nationality,"SwissSwiss", "Swiss")
da$nationality <- str_replace_all(da$nationality,"British, British", "British")
da$nationality <- str_replace_all(da$nationality,"French, French", "French")
da$nationality <- str_replace_all(da$nationality,"ItalianSwiss", "Italian, Swiss")
da$nationality <- str_replace_all(da$nationality,"SwissSwissSwiss", "Swiss")
da$nationality <- str_replace_all(da$nationality,"RussianSwiss", "Russian, Swiss")
da$nationality <- str_replace_all(da$nationality,"Italian, Italian", "Italian")
da$nationality <- str_replace_all(da$nationality,"DutchSwiss", "Dutch, Swiss")
da$nationality <- str_replace_all(da$nationality,"SwissSwiss", "Swiss")
da$nationality <- str_replace_all(da$nationality,"FrenchSwiss", "French, Swiss")
da <- da %>% 
  mutate(nationality = as.factor(nationality))



#write.csv(da, "lon_data_w_GA.csv")
