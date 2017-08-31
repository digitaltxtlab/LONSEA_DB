list.of.packages <- c("ggplot2","ggExtra","lme4", "nlme","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}
setwd("~/Historie projekt/historie project/LONSEA_DB/fall17/five_nations")
da <- read.csv("reversedfcode_nation5.wo_temp_fcode0.w_GA_rank_intervals.csv")

da <- read.csv("reversedfname_nation5.wo_temp_fcode0.w_GA_rank.csv")

mdl_da <- da %>% 
  #  filter(canonical_fname == "First Division") %>% 
  select(fname_code,pname, entry_time, nationality) %>% 
  filter(!is.na(entry_time)) 

colnames(mdl_da) <- c("status","name","entry_time","nationality")

#we make contrasts testing of Scandinavia is significantly different from the others


contrasts(mdl_da$nationality) <- contr.treatment(levels(mdl_da$nationality), base = 3)


#as our baseline model we use the overall mean/intercept
baseline<-gls(status ~ 1, data = mdl_da, method = "ML")

#we let the intercept vary from person to person, i.e. specifying it as a random effect
random_intercept<-lme(status ~ 1, random = ~1|name, data = mdl_da, method = "ML")

#We comepare the baseline and the random_intercept model, and see that the random intercept
#fits the data significantly better
anova(baseline, random_intercept)

#we add entry_time as a predictor. 
entry_model <- update(random_intercept, .~. + entry_time)

anova(random_intercept,entry_model)
#adding entry time explains significantly more variance

#we add nationality as a predictor
nation_add_model <- update(entry_model, .~.+nationality)


add_interaction <- update(nation_add_model, .~. +nationality:entry_time)

anova(baseline, random_intercept, entry_model, nation_add_model, add_interaction)

summary(add_interaction)


division_da <- da %>% 
  select(fname_code,pname, entry_time, nationality,canonical_fname) %>% 
  filter(!is.na(entry_time)) %>% 
  filter((canonical_fname == "First Division" | canonical_fname == "Second Division" | canonical_fname =="Third Division")) 
division_da$canonical_fname <- droplevels(division_da$canonical_fname)
colnames(division_da) <- c("status","name","entry_time","nationality", "division")
#division_da <- rbind(division_da, other_canon)



#plotting results for each division
division_da %>% 
  filter(!name == "Raymond Ian Gordon Watt") %>% 
  ggplot(aes(entry_time,status, color = nationality)) +
  geom_point(alpha = 0.0001)+
  geom_jitter(height = 0.25)+
  geom_smooth(se = F, method = "lm", linetype = "solid", size = 2) +
  scale_y_continuous(breaks = round(seq(min(mdl_da$status), max(mdl_da$status), by = 1),1)) +
  facet_wrap(~division) +
  labs(x = "Entry times of rank changes and hirings", y = "Ranks", title ="Division Ranks for LoN Secretariat")
