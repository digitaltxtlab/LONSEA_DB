#!/usr/bin/Rscript
rm(list = ls())
#"""
# add coded fnames (classes) to main data frame
# model individual career path for each member
#
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

# list with one matrix pr user with col1: year and col2: fname_code
careerpath <- function(df){
  uid <- unique(df$id)
  for(i in 1:length(uid)){
    idx <- uid[i] == df$id
    usr <- df[idx,]
    x <- c(usr$begin_on_year[1], usr$end_on_year)
    y <- c(usr$fname_code[1],usr$fname_code)
    xy = matrix(c(x,y),ncol=2)
    res[[i]] = xy
  }
  return(res)
}

# plot distribution of status on for each user with multiple instances
require(ggplot2)
require(ggExtra)
require(plyr)

x <- main.df$fname_code
y <- main.df$end_on_year + main.df$end_on_month/10
c = main.df$gender
# remove na
idx <- !is.na(y)
x <- x[idx]
y <- y[idx]
c <- c[idx]
idx <- (1900 < y) & (2000 > y)
status <- jitter(x[idx])
year <- y[idx]
gender <- as.factor(c[idx])

df <- data.frame(status,year,gender)
df$gender <- mapvalues(df$gender, from = c('-1','0','1'),to=c('unknown','male','female'))
# Scatter plot of x and y variables and color by groups
fig <- ggplot(df,aes(status, year, color = gender)) + geom_point() + 
    scale_color_brewer(palette="Greys") + 
    ggtitle('Status distribution')  + 
    labs(x="Status",y="Year")
   # Marginal density plot
fig2 <- ggMarginal(fig)
ggsave('figures/status_dist.png',fig2, width = 7, height = 6,
       units = 'in' ,dpi = 600)