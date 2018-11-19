# creating df counting number of individuals for the whole secretariat
setwd("~/LONSEA_DB/shiny/data")

da = read.csv("lon_data.csv")
tt = read.csv("timetable_year.csv", sep = ",") 

tt = tt[,-c(1,2,31)]

count = as.data.frame(colSums(tt != ""))
names(count) = "Count"

rownames(count) = str_extract_all(rownames(count),pattern = "[0-9]{4}")  

setwd("~/LONSEA_DB/shiny/proto_shiny")

write.csv(count,"count_df.csv")

#for first dvision
# creating df counting number of individuals for the whole secretariat
setwd("~/LONSEA_DB/shiny/data")

tt = read.csv("timetable_first_division.csv", sep = ",") 
tt[is.na(tt)] <- ""
tt = tt[,-c(1,2,31)]

count = as.data.frame(colSums(tt != ""))
names(count) = "Count"

rownames(count) = str_extract_all(rownames(count),pattern = "[0-9]{4}")  

setwd("~/LONSEA_DB/shiny/proto_shiny")

write.csv(count,"count_first_division.csv")

#for higher officials

setwd("~/LONSEA_DB/shiny/data")

tt = read.csv("timetable_higher_officials.csv", sep = ",") 
tt[is.na(tt)] <- ""
tt = tt[,-c(1,2,31)]

count = as.data.frame(colSums(tt != ""))
names(count) = "Count"

rownames(count) = str_extract_all(rownames(count),pattern = "[0-9]{4}")  

setwd("~/LONSEA_DB/shiny/proto_shiny")

write.csv(count,"count_higher_officials.csv")
