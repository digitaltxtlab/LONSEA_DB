require('gender')
gender("madison", method = "demo", years = 1985)
test <- gender("madison", method = "demo", years = c(1900, 1985))

round(1 - test$proportion_female)
round(1 - gender("madison", method = "demo", years = c(1900, 1985))$proportion_female)
