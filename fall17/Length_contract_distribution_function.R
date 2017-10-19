employment_distribution <- function(df, yseq = seq(0,2000, 250), group = "people"){ 
  result <- employment(df)
  
  contract_length <- matrix(nrow = (nrow(result)), ncol = 1)
  
  result <- as.data.frame(result)
  
  names <- as.character(result[,1])
  
  #removing pname column 
  contracts<- sapply(result[,-1],function(x) x != "") %>% 
    rowSums() %>% 
    as.data.frame() %>% 
    rename(contract_length = ".") %>% 
    mutate(names = as.factor(names))
  
  
  contracts %>% 
    ggplot(aes(contract_length, color = contract_length)) +
    geom_histogram( aes(fill = ..count..)) +
    scale_fill_gradient("count", low="orange", high="purple") +
    labs(x = "Work periods in years", y = "Number of people", title = paste("Distribution of work periods in LoN for",group)) +
    scale_x_continuous(breaks = seq(0,30,5))+
    scale_y_continuous(breaks = yseq)
}