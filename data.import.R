# import data
dat.df <- read.csv("data/sheet1.csv")
## extract identifiers
# person
writeLines('unique PERSON entities:')
print(length(unique(dat.df$pname)))
writeLines("\n")
# location
writeLines('unique LOCATION entities:')
print(length(unique(dat.df$nationality)))
writeLines("\n")
# features
varnames <- colnames(dat.df)
writeLines("Variable names:")
print(varnames)
writeLines("\n")

cat ("Press [enter] to continue")
line <- readline()

id_u.v <- unique(dat.df$id)
# object class
dat.class.l <- lapply(dat.df, class)

p_na <- function(x){sum(is.na(x))/length(x)*100}

for(i in 1:ncol(dat.df)){
  writeLines('--------------------')
  writeLines(paste("Feature name:", varnames[i]))
  print(head(dat.df[,i]))

  if(dat.class.l[i] == "integer"){
    tmp <- dat.df[,i]

    print(paste('Mean:', mean(tmp[!is.na(tmp)])))
    print(paste('SD:', sd(tmp[!is.na(tmp)])))
    print(paste('% missing',p_na(tmp)))
  }
  writeLines("\n")
  cat ("Press [enter] to continue")
  line <- readline()
}





