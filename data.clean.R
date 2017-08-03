#!/usr/bin/Rscript
## dynamically check, install and load libraries
list.of.packages <- c("mice","VIM","lattice")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}
source("data.import.R")

# test sample
n = 100
idx <- sample(1:nrow(dat.df), n)
test.df <- dat.df[idx,]


# extract numerical data
dat.class.v <- unlist(dat.class.l)
num.idx <- dat.class.v == "integer"

#test.num.df <- test.df[,num.idx] 
dat.num.df <- dat.df[,num.idx] 

## missing feature values

#md.pattern(test.num.df)

dev.new()
aggr_plot <- aggr(dat.num.df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

cat ("Press [enter] to continue")
line <- readline()

# multiple entries pr. person reflect their organization history
idx <- dat.df$id[5] == dat.df$id 
print(dat.df[idx,])

## imputation
#tmp.dat <- mice(dat.num.df,m=5,maxit=5,meth='pmm',seed=500)
#summary(tmp.dat)

#completed.dat.df <- complete(tmp.dat,1)

#require(lattice)
#densityplot(tmp.dat)
i