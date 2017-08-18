list.of.packages <- c("ggplot2","ggExtra","plyr","lme4", "nlme","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for(i in 1:length(list.of.packages)){
  require(list.of.packages[i], character.only = TRUE)
}
#"""
# add coded fnames (classes) to main data frame
# model individual career path for each member using a mixed linear model in foward model selection
#
#"""
source("data.extract.R")
main.df <- dat2.df; rm(dat2.df)

# import data frame of rank names to ordinal scale
class.df <- read.csv("data/fname_u_with_class.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE)# changed original category csv due to encoding
# column names
colnames(class.df) = c("u_id", "u_fname" , "u_fname_code")
# code every object in main
tmp <- rep(NA,nrow(main.df))

for(i in 1:nrow(class.df)){
  #for each rank a logical vector indexing subject numbers are created
  idx <- class.df$u_fname[i] == main.df$fname
  #true index are translated to oridnal rank numbers for each subject
  tmp[idx] <- class.df$u_fname_code[i]
}
#ordinal rank numbers are added
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
fig2 <- ggMarginal(fig) #ggmarginal adds density plots at the margin - neat.
print(fig2)
#ggsave('figures/status_dist.png',fig2, width = 7, height = 6,
#      units = 'in' ,dpi = 600)

### clean main based on 'begin_on_year'
# remove na
idx_na_yr = !is.na(main.df['begin_on_year'])
main.df = main.df[idx_na_yr,]
idx_na_m =  !is.na(main.df['begin_on_month'])
main.df = main.df[idx_na_m,]
# remove errrors
idx1900 = main.df['begin_on_year'] > 1900
main.df = main.df[idx1900,]
idx12 = main.df['begin_on_month'] <= 12
main.df = main.df[idx12,]
### model
## fixed factors
# predictor 1: begin in year and month for for fname
x1 <- main.df$begin_on_year
x2 <- main.df$begin_on_month
x_1 <- x1 + x2/12# + x3/30
# predictor 2: gender
x_2 <- main.df$gender
x_2[is.na(x_2)] = -1
x_2 = as.factor(x_2)
levels(x_2) = c("NA", "man", "woman")

# predictor 3: nationality
x_3 <- main.df$nationality
x_3[!x_3 == "Swiss"] = "international"
x_3[x_3 == "Swiss"] = "national"
x_3 <- as.factor(x_3)
# predictor 4: age
x_4 <- main.df$born_on_year

## random factors
# person
x_r_1 = as.factor(main.df$pname)
## response
y_1 = main.df$fname_code

# model data frame
mdl_df = data.frame(y_1,x_1,x_2,x_3,x_r_1)
colnames(mdl_df) = c("status","entry_time","gender","nation","name")
head(mdl_df)

# model selection
mdl0 <- lmer(status ~ 1 + (1|name), data = mdl_df)
mdl1 <- lmer(status ~ entry_time + (1|name), data = mdl_df)
mdl2 <- lmer(status ~ entry_time + gender + (1|name), data = mdl_df)
mdl3 <- lmer(status ~ entry_time + gender + nation + (1|name), data = mdl_df)

print(anova(mdl0,mdl1,mdl2,mdl3))

print(summary(mdl3))

#Adam's mixed effects model


baseline<-lme(status ~ 1, random = ~1|name, data = mdl_df, method = "ML")

model1 <- update(baseline, .~. + entry_time)

anova(baseline, model1)
summary(model1)

mdl_df %>% 
  ggplot(aes(entry_time,status)) +
  geom_point()+
  geom_smooth() +
  geom_abline(slope = -0.010158, intercept = 25.1748, color = "red")
#shows that average status decreases over time: More people at the top over time. 


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
