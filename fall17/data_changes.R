#Data enhancenment


list.of.packages <- c("stringr","ggplot2","ggExtra","plyr","lme4", "nlme","tidyverse", "zoo","lubridate", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}
setwd("~/Historie projekt/historie project/LONSEA_DB/fall17")

#da <- read.csv("lon_data_w_GA.csv")
da <- read.csv("five_nations/reversedfname_nation5.wo_temp_fcode0.w_GA_rank.csv")

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

#write.csv(da, "lon_data_w_GA.csv")
