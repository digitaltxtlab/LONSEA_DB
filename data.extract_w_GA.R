## dynamically check, install and load libraries
list.of.packages <- c("stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}
#"""
## import LONSEA projects original data and remove objects that are not LoN and General Assembly 
# clean nationality and replace empty entries with NA
# export variables to historians
#"""
rm(list = ls())

setwd("~/Historie projekt/historie project/LONSEA_DB")
dat.df <- read.csv("data/sheet1.csv", stringsAsFactors = FALSE)
# features
varnames <- colnames(dat.df)
# reduce data set to LoN minus General Assembly
i <- 1:nrow(dat.df)# numerical index for generation of final data set
# LoN ids
lon_idx <- !is.na(str_match(dat.df$oname,'LoN'))
lon_id = dat.df$oname[lon_idx] 
i <- i[lon_idx]
# general assembly ids
#naga_idx <- is.na(str_match(lon_id,'General Assembly'))
#naga_id <- lon_id[naga_idx]
#i <- i[naga_idx]
# export data
dat2.df <- dat.df[i,]
# write.csv(dat2.df, file = "data/lon_data.csv")
# remove "IoN " in fname,canonical_fname,oname
for(ii in 4:6){
  dat2.df[,ii] <- str_replace(dat2.df[,ii],"LoN ","")
}
head(dat2.df)
#write.csv(dat2.df, file = "data/lon_data.csv")

# split nationality and transform to country name instead of nationality (for geo-mapping)
dat2.df$nationality <- str_replace(dat2.df$nationality,"\t","")
dat2.df$nationality <- str_replace(dat2.df$nationality,"New Zealander","New_Zealander")
dat2.df$nationality <- str_replace(dat2.df$nationality,"South African","South_African")
dat2.df$nationality <- str_replace(dat2.df$nationality,"Costa Rican","Costa_Rican")
dat2.df$nationality <- str_replace(dat2.df$nationality," ","")
# build list
nation.l <- dat2.df$nationality
# replace empty entries with NA
nation.l[which(nation.l == "")] = NA
# build matrix
nation.m <- matrix(data=NA,nrow=length(nation.l),ncol=3)
colnames(nation.m) <- c('nation_1','nation_2','nation_3')
for(i in 1:length(nation.l)){
  tmp <- unlist(strsplit(nation.l[i],",")) 
  for(ii in 1:length(tmp)){
    nation.m[i,ii] = tmp[ii]    
  }
}
# export as data frame
nation.df <- as.data.frame(nation.m)
head(nation.df)

# export
#write.csv(dat2.df, file = "data/lon_data_w_GA.csv")
#write.csv(unique(nation.l), file = 'data/nationalities.csv')
#write.csv(unique(dat2.df$fname), file = 'data/fname_u.csv')
#write.csv(unique(dat2.df$canonical_fname), file = 'data/canonincal_u.csv')
#write.csv(unique(dat2.df$oname), file = 'data/oname_u.csv')

rm(list=setdiff(ls(), "dat2.df"))
