list.of.packages <- c("ggplot2","ggExtra","lme4", "nlme","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}
setwd("~/Historie projekt/historie project/LONSEA_DB")
da <- read.csv("fall17/nation5.wo_temp.w_GA.csv")



da <- da %>% 
  filter(begin_on_year > 1900)

x1 <- da$begin_on_year
x2 <- da$begin_on_month
x_1 <- x1 + x2/12# + x3/30

da <- da %>% 
  mutate(entry_time = x_1)


# import data frame of rank names to ordinal scale
ordinal_ranks <- read.csv("data/fname_u_with_class.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE)# changed original category csv due to encoding
# column names
colnames(ordinal_ranks) = c("u_id", "u_fname" , "u_fname_code")
# code every object in main
extract_rank <- rep(NA,nrow(da))

for(i in 1:nrow(ordinal_ranks)){
  #for each rank a logical vector indexing subject numbers are created
  idx <- ordinal_ranks$u_fname[i] == da$fname
  #true index are translated to oridnal rank numbers for each subject
  extract_rank[idx] <- ordinal_ranks$u_fname_code[i]
}
#ordinal rank numbers are added
da$fname_code <- extract_rank

#write.csv(da, "nation5.wo_temp.W_GA_rank.csv")

mdl_da <- da %>% 
  select(fname_code,pname, entry_time, nationality) %>% 
  filter(!is.na(entry_time)) 
  
colnames(mdl_da) <- c("status","name","entry_time","nationality")



#Adam's mixed effects model

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

mdl_da %>% 
  filter(nationality == "British") %>% 
  summarise(count = mean(status))

mdl_da %>% 
  filter(nationality == "South American") %>% 
  summarise(count = mean(status))



#Plotting the graph
mdl_da %>% 
  ggplot(aes(entry_time,status, color = nationality)) +
  geom_point(alpha = 0.0001)+
  geom_jitter(height = 0.25)+
  geom_smooth(se = T, method = "lm", linetype = "solid", size = 2) +
  labs(x = "Year", y ="Status of new entries in LoN", titles ="Status over time for full LoN Secretariat")
  

#dividing data into three division

division_da <- da %>% 
  select(fname_code,pname, entry_time, nationality,canonical_fname) %>% 
  filter(!is.na(entry_time)) %>% 
  filter(!(canonical_fname == "First Division" | canonical_fname == "Second Division" | canonical_fname =="Third Division")) 
other_canon <- division_da
other_canon$canonical_fname <- "Other"

division_da <- da %>% 
  select(fname_code,pname, entry_time, nationality,canonical_fname) %>% 
  filter(!is.na(entry_time)) %>% 
  filter((canonical_fname == "First Division" | canonical_fname == "Second Division" | canonical_fname =="Third Division")) 

division_da <- rbind(division_da, other_canon)
division_da$canonical_fname <- droplevels(division_da$canonical_fname)
colnames(division_da) <- c("status","name","entry_time","nationality", "division")

#plotting results for each division
division_da %>% 
  ggplot(aes(entry_time,status, color = nationality)) +
  geom_point(alpha = 0.0001)+
  geom_jitter(height = 0.25)+
  geom_smooth(se = T, method = "lm", linetype = "solid", size = 2) +
  facet_wrap(~division, scales = "free")



#adding gender as predictiv variable

gender_add_model <- update(model1, .~. +gender)
summary(mdl_df) #summary shows the distribution of genders. NA: 7, Man: 3443, Woman: 3565
anova(model1,gender_add_model) 
#we see a significant effect of gender
summary(gender_add_model)
#we see a nearly significant effect of being a woman. They have 1.6 higher/worse rank 
#averages are calculated for each gender group
mdl_df %>% 
  group_by(gender) %>% 
  summarise(avg_status = mean(status))
#visualization of differences
mdl_df %>% 
  ggplot(aes(gender,status, color = gender))+
  geom_boxplot()
#Interesting here how little variance there are in woman. They aren't completely in the bottom.Furthermore,
#there are a couple of outliers
mdl_df %>% 
  filter(status < 5 & gender == "woman" & !duplicated(name)) %>% #counting womans per rank, for ranks "better" than 5
  mutate(stauts = as.factor(status)) %>% 
  group_by(status) %>% 
  summarise(overview = n())


nation_add_model <- update(gender_add_model, .~.+nation)
anova(gender_add_model, nation_add_model)
summary(nation_add_model)
#Initial rank of national workers is 2 points higher than international(they had worse ranks). Reflects that "mundane" workers
#were locals. 
#results is visualizd through boxplot beneath:
mdl_df %>% 
  ggplot(aes(nation,status)) +
  geom_boxplot()

anova(baseline,model1, gender_add_model,nation_add_model)
sum1 <- summary(nation_add_model)
sum2 <- summary(mdl3)
