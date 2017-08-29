list.of.packages <- c("ggplot2","ggExtra","plyr","lme4", "nlme","tidyverse", "zoo","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}

setwd("~/Historie projekt/historie project/LONSEA_DB/fall17")

da <- read.csv("nation5.wo_temp.W_GA_rank.csv")

da <- da %>% 
  filter(begin_on_year > 1900 & end_on_year >1900)
da$begin_date <- as.Date(with(da, paste(begin_on_year, begin_on_month, begin_on_day,sep="-")), "%Y-%m-%d")
da$end_date <- as.Date(with(da, paste(end_on_year, end_on_month, end_on_day,sep="-")), "%Y-%m-%d")

da <- da %>% 
  filter(!is.na(da$begin_date) & !is.na(da$end_date))  
  
da$pname <- droplevels(da$pname)


mdl_df <- da %>% 
  select(pname, fname_code, begin_date,end_date)


start_date <- as.Date("1919-01-01")
end_date <- as.Date("1948-12-30")
dates <- seq.Date(start_date, end_date, by ="month")

test.df <- matrix(ncol =length(dates), nrow = nlevels(da$pname))
test.df <- as.data.frame(test.df)
colnames(test.df) <- dates
rownames(test.df) <- levels(da$pname)


for (name in 1:nlevels(mdl_df$pname)){ #go over all persons
  
  person_rows <- mdl_df %>% filter( mdl_df$pname == rownames(test.df)[name]) 
  
  for (date in 1:(length(dates)-1)) { #go over all dates minus the last one
    
    interval1 <- interval(ymd(colnames(test.df)[date]),ymd(colnames(test.df)[date+1]))  
    
    for (row in 1:nrow(person_rows)) {#finding employment intervals
      interval2 <- interval(ymd(person_rows$begin_date[row]),ymd(person_rows$end_date[row]))
    if (int_overlaps(interval1, interval2)){#checking if df period and work period overlap
      test.df[name,date] <- test_rows$fname_code[row]
      break
    }else{
      test.df[name,date] <- "N"
    }
    }  
    
  }

}

test_int1 <- interval(ymd(colnames(test.df)[1]),ymd(colnames(test.df)[1+1]))
test_int2 <- interval(ymd(test_rows$begin_date[1]),ymd(test_rows$end_date[1]))

int_overlaps(test_int1,test_int2)
as.Date(colnames(test.df)[1])


test_rows <- da %>% filter( da$pname == rownames(test.df)[1]) %>% 
  select(pname, begin_date,end_date)


