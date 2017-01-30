#!/usr/bin/Rscript
rm(list = ls())
#"""
# add coded fnames (classes) to main data frame 
#"""
source("data.extract.R")
main.df <- dat2.df; rm(dat2.df)

# import 
class.df <- read.csv("data/fname_u_with_class.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE)# changed original category csv due to encoding
# column names 
colnames(class.df) = c("u_id", "u_fname" , "u_fname_code")  
# code every object in main
tmp <- rep(NA,nrow(main.df))
for(i in 1:nrow(class.df)){
  idx <- class.df$u_fname[i] == main.df$fname
  tmp[idx] <- class.df$u_fname_code[i]
}
# add column
main.df$fname_code <- tmp

### mdl 1: Average Career Path
## the average career score (fname_code) for each (unique) object (pname)

# number of months in organization
n_months <- function(y1,y2,m1, m2){
  y1m1 <- 12 - m1
  y2y1 <- y2 - y1
  if(y1 < y2){
    msum <- msum <- (12 - m1 + m2) + 12 * (y2y1 - 1)}else{
      msum = m2-m1
    }
  if(msum == 0){
    msum = 1
  }
  return(msum)
}
# calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


head(main.df)
pname_u <- unique(main.df$pname)
nationality_u <- rep(NA,length(pname_u))
# pre-allocate
pname_mat <- matrix(NA,length(pname_u),6)
# generate numeric scores
for(i in 1:length(pname_u)){
  
  
  idx <- pname_u[i] == main.df$pname
  df <- main.df[idx,]
  avg <- mean(df$fname_code)
  std <- sd(df$fname_code)
  if(is.na(std)){std = 0}
  med <- median(df$fname_code)
  gen <- getmode(df$gender) # gender based on mode of codes
  # calculate time spend in LoN
  n = nrow(df) 
  if(n > 1){
    y <- c(df$begin_on_year[1], df$end_on_year[n])
    m <- c(df$begin_on_month[1],df$end_on_month[n])
  }else{
    y <- c(df$begin_on_year[1], df$end_on_year[1]) 
    m <- c(df$begin_on_month[1], df$end_on_month[1])
  }
  # imputation rule: if both years are missing, then NA is upheld, if both months are missing months are assumed identical, if one  year or month is missing the existing value is used
  if(sum(is.na(m)) == 1){
    m[is.na(m)] = m[!is.na(m)]
  }
  if(sum(is.na(m)) == 2){
    m = c(1,1)
  }
  if(sum(is.na(y)) == 1){
    y[is.na(y)] = y[!is.na(y)]
  }
  if(sum(is.na(y)) == 2){
    dura = NA
    }else{
      dura <- n_months(y[1],y[2],m[1],m[2])
    }
  age_entry <- y[1] - mean(df$born_on_year, na.rm = T) 
  # update matrix
  pname_mat[i,1] <- avg
  pname_mat[i,2] <- std
  pname_mat[i,3] <- med
  pname_mat[i,4] <- dura
  pname_mat[i,5] <- age_entry
  pname_mat[i,6] <- gen
  # string vars
  nationality_u[i] <- getmode(unique(df$nationality))
}
# data frame with numerical variables for rank, duration in IoN, entry age, (most frequent) gender, and (most frequent) nationality
main_u.df <- data.frame(pname_u,pname_mat)
colnames(main_u.df) = c("name","avg_rank","sd_rank","median_rank","duration","entry_age","gender")
main_u.df$nationality <- nationality_u






