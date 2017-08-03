## dynamically check, install and load libraries
list.of.packages <- c("gender")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}
#"""
# add coded fnames (classes) to main data frame
# build matrix of numerical variables averaged on person (u: unique pname)
# add naitonality
#
#"""
rm(list = ls())
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
# build matrix
pname_u <- unique(main.df$pname)
nationality_u <- rep(NA,length(pname_u))
# pre-allocate
pname_mat <- matrix(NA,length(pname_u),7)
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
  pname_mat[i,7] <- mean(df$born_on_year, na.rm = T)# add year of birth
  # string vars
  nationality_u[i] <- getmode(unique(df$nationality))
}
# data frame with numerical variables for rank, duration in IoN, entry age, (most frequent) gender, and (most frequent) nationality
main_u.df <- data.frame(pname_u,pname_mat, stringsAsFactors = F)
colnames(main_u.df) = c("name","avg_rank","sd_rank","median_rank","duration","entry_age","gender","DoB")
main_u.df$nationality <- tolower(nationality_u)


# imputation of gender
gen <- pname_mat[,6]
gen[is.na(gen)] = -1
idx <- gen == -1 
nm <- main_u.df$name[idx]
db <- main_u.df$DoB[idx]
c1 <- nm[1:4]# esitmate probability from data
c2 <- c(nm[5:6], db[5:6])# esitmate probability from U.S. Social Security Administration
c2[1] <- 'Anastasia'
# c1
pr = c(sum(gen[!idx] == 0)/length(gen[!idx]), sum(gen[!idx] == 1)/length(gen[!idx]))
c1 = rbinom(length(c1), 1, pr[2])
# c2
tmp <- rep(NA,length(c2)/2)
for(i in 1:2){
  c2[i] <- strsplit(c2[i]," ")[[1]][1]
  tmp[i] <- round(1 - gender(c2[i], method = "ssa", years = as.numeric(c2[i+2]))$proportion_male)
}
c2 <- tmp
# update
gen[idx] <- c(c1,c2)
#Wgen[idx] <- rbinom(length(nm), 1, pr[2])# temporary solution
gen_char = rep(NA,length(gen))
  gen_char[gen == 0] <- 'male'
  gen_char[gen == 1] <- 'female'

main_u.df$gender <- as.factor(gen_char) 
main_u.df$DoB <- NULL # remove date of birth
# fill in nationality
main_u.df$nationality[main_u.df$nationality == ""] = 'unknown'
main_u.df$nationality <- as.factor(main_u.df$nationality)
# imputation of nationary, TODO

# update workspace and export
rm(list=setdiff(ls(), "main_u.df"))
# write.csv(main_u.df, file = 'data/lon_data_u.csv')
### models for demonstation
colnames(main_u.df)
mdl1 <- lm(avg_rank ~ duration, data = main_u.df)
summary(mdl1)
mdl2 <- lm(avg_rank ~ entry_age, data = main_u.df)
summary(mdl2)
mdl3 <- lm(avg_rank ~ gender, data = main_u.df)
summary(mdl3)
mdl4 <- lm(avg_rank ~ nationality, data = main_u.df)
summary(mdl4)

# multi var
mdl_1_2 <- lm(avg_rank ~ gender * duration, data = main_u.df)
summary(mdl_1_2)

# export data for Seaborn
write.table(main_u.df, file = "/home/kln/Documents/proj/lonsea/main_u.csv", append = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, fileEncoding = "utf-8")

