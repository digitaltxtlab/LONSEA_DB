#### testing
setwd("~/LONSEA_DB/shiny/data")

da = read.csv("lon_data.csv")
tt = read.csv("timetable_year.csv", sep = ",") 

tt = tt[,-c(1,2,31)]

count = as.data.frame(colSums(tt != ""))
names(count) = "Count"

rownames(count) = str_extract_all(rownames(count),pattern = "[0-9]{4}")  
