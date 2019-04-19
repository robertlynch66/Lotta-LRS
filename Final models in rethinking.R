# load librarries
library(dplyr)
library(rethinking)
library(tidyr)
library(psych)
# path to the folder with the R data files
path <- "../data files/"
# read in person table
file<- "person_data.rds"
p <- readRDS(paste0(path, file))
file<- "children.rds"
children <- readRDS(paste0(path, file))
##lotta are less likely to reproduce
##But lotta are slightly older (average 1 year)
boxplot(age_1945~lotta, data=data)
# get the mean and std error
aggregate (age_1945~lotta, data=data,FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x))))

describeBy(data,list(data$age_1945,data$lotta)) 
##Older people are less likely to reproduce, but the lower probability of reproduction of lotta remains when correcting for age



library(dplyr)
#  Model 1
path <- "../data files/"
# read in person table
file<- "person_data.rds"
p <- readRDS(paste0(path, file))
p$age_1945 <- 1945-p$birthyear
p_full <- p %>% select ("id","age_1945","lotta","kids")
p$age_1945 <- 1945-p$birthyear
p_full <- p_full[complete.cases(p_full), ] 
# make reporoduced binomial variable
p$never_reproduced <- ifelse(p$kids>0,0,1)
#filter to ages between 18 to 45 in 1945 (when the ware ends)
p <- p %>% filter (age_1945 >17 & age_1945 <46)
# center age
p_full$age <- p_full$age_1945-mean(p_full$age_1945)

#Run regular binomial regression for lottas reproduced vs did not reproduce- Model 1
model <- glm(never_reproduced ~ lotta, family="binomial", data=p_full)
summary(model)
model <- glm(never_reproduced ~ education + lotta + age_1945 + lotta*age_1945, family="binomial", data=p_full)
summary(model)
#  "pikkulotta" )
#  Model 2a
#Already ran it
#  
#  Model 2b
#  # Lottas childless (included) time to repro model
library(dplyr)
library(rethinking)
library(tidyr)


# path to the folder with the R data files
path <- "../data files/"
# read in person table
file<- "person_data.rds"
p <- readRDS(paste0(path, file))
file<- "children.rds"
children <- readRDS(paste0(path, file))

p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708


p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p_full <- p
p<- p %>% drop_na(first_child_yob) # 60129
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid")
p <- p[complete.cases(p), ] # 47793
# 22878 started before war ends and 25558 started having kids after 1944 

# link children table 
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# ages--now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 

p$birth_plus_13 <- p$birthyear+13
p$lastapp <- ifelse (p$birthyear<1925, p$birthyear+45,1970)

## now make cut off when you want (e.g. age 50 or ages 13-50)

p$year <- mapply(seq, p$birth_plus_13, p$lastapp, SIMPLIFY = FALSE) 
#Creates a 
#sequence for each row,
#so if birth year is 1850 and death year 1900, the cell says 1850:1900.
#Simplify makes a matrix, but we want to keep a dataframe

#unnest creates a new row for each different value within a "cell" - 
#this is taken from the 'year' column created above
p_long <- unnest(p, year) #1550622

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
# give this column a 1 which is the year the kid was born- later this will be linked to the parents df and will signify that the parenst
# gave birth in that year
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
# libnk by children df [parent id] and [id] in the lottas df and by year in the lottas df (the range of years they were potentiatll fertile
# 13 years old to either the year it  was when they turned 45 or 1970- the interview year)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
# name the column if they reproduced in that year 'reproduced'
colnames(twins)[14] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
# # make a dummy variable for women who reproduced within the past two years before the end of the war (i.e. in 1943 or 1944)
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")

### maybe add another dummy variable for inds who had no kids



# # make a time to repro after 1945 variable
ttr <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945)
# preserve this varibale for birth intervals model
birth_ints <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945) %>% as.data.frame()

# get the sequential difference - birht interval by id's
birth_ints <- birth_ints %>%
  group_by(id) %>%
  mutate(Diff = time_to_repro - lag(time_to_repro)) %>% as.data.frame()

birth_ints$ttr_2 <- ifelse(is.na(birth_ints$Diff),birth_ints$time_to_repro,birth_ints$Diff)
birth_ints$Diff <- NULL


#####

ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro)) 

ttr_3 <- ttr %>% group_by (id) %>%
  dplyr::summarise(minimum = min(time_to_repro))




ttr <- ttr %>% left_join (ttr_2, by="id")
ttr <- ttr %>% left_join (ttr_3, by="id")

ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
ttr <- ttr %>% left_join (ttr_2, by="id")
rm(ttr_2)
# rejoin dummy coded birth within past 2 years to main table
ttr <- ttr %>% left_join (dummy, by="id")
ttr$repro_within_2_years[is.na(ttr$repro_within_2_years)] <- 0 


ttr$post_war_repro_rate <- ttr$maximum/ttr$kids_after_war
ttr$kids_before_war <- ttr$kids-ttr$kids_after_war
# remove duplicate ids for this data frame
ttr <- ttr[!duplicated(ttr[,c("id")]),]
ttr$age_sq <- ttr$age_1945*ttr$age_1945
# key line select ages
ttr <- ttr[which(ttr$age_1945>12 & ttr$age_1945<46),]

#put age in 1945 on the model prediction scale (i.e. the way it went into the mdel in rethinking)
ttr$age_1945_sc <- ttr$age_1945-min(ttr$age_1945)
ttr$age_1945_sc <- ttr$age_1945/max(ttr$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
ttr<-ttr %>% as.data.frame()  #31916

# read in person data again and get all the women who did not reproduce
#path<- (paste0("~/r_files/"))
path <- "../data files/"
# read in person table
file<- "person_data.rds"
d <- readRDS(paste0(path, file))

d <- d %>% filter (sex==0)
d <- d %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708

d$lotta<- as.numeric(d$lotta)
d$age_1945 <- 1945-d$birthyear
d <- d %>% filter(kids==0)
d$birth_cat <- 0
d$time_to_repro <- 25
d$lotta <- as.numeric(d$lotta)
d$repro_within_2_years <- 0
d$kids_after_war <- 0
d$reproduced <- 0
# 28992 started before war ends and 31137 started having kids after 1944 


d <- d %>% select ("id","kids_after_war","time_to_repro","lotta","birth_cat","age_1945",
                   "agriculture","repro_within_2_years","education","birthplaceid" )
d$reproduced <- 0
d$kids_after_war<- as.integer(d$kids_after_war)
d <- d[complete.cases(d), ] # 10172 no repros

# rbind the 2 data frames
ttr <- ttr %>% select("id","kids_after_war","time_to_repro","lotta","birth_cat","age_1945",
                      "agriculture","repro_within_2_years","education","birthplaceid")
ttr$reproduced <- 1
data <- rbind(d,ttr)# 41,946
data <- data %>% filter(age_1945>15 & age_1945<45) #37,750
data <- data%>% arrange(birthplaceid)
data$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(data$birthplaceid))!=0))

## check effect of non reproductives (tables s4)
data$never_reproduced <- ifelse(data$reproduced==0,1,0)
model <- glm(never_reproduced ~    lotta + age_1945, 
             family="binomial", data=data)
summary(model)
# coxph model fist - includes non reproductives for mean birth ints

# we should have 30691 individuals who are between the ages of 17 and 45
#data$time_to_repro <- as.integer(data$time_to_repro)
#map2stan formula
# run in rethinking
data_list <- list(
  kids_after_war = data$kids_after_war,
  time_to_repro = data$time_to_repro,
  lotta  = data$lotta,
  birth_cat = data$birth_cat,
  age = data$age_1945,
  agriculture = data$agriculture,
  repro_within_2_years = data$repro_within_2_years,
  education = data$education,
  birthplaceid_seq= data$birthplaceid_seq)


model <- map2stan(
  alist(
    time_to_repro ~ dpois(lambda),
    log(lambda) <- Intercept +
      a_birthplaceid[birthplaceid_seq] +
      b_lotta*lotta +
      b_age*age +
      b_birth_cat*birth_cat +
      b_education*education +
      b_agriculture*agriculture +
      b_repro_within_2_years*repro_within_2_years +
      b_lotta_X_age*lotta*age,
    
    a_birthplaceid[birthplaceid_seq] ~ dnorm (0, sigma),
    sigma ~ dcauchy (0,1),
    Intercept ~ dnorm(0,1),
    b_lotta ~ dnorm(0,1),
    b_age ~ dnorm(0,1),
    b_education ~ dnorm(0,1),
    b_birth_cat ~ dnorm(0,1),
    b_agriculture ~ dnorm(0,1),
    b_repro_within_2_years ~ dnorm(0,1),
    b_lotta_X_age ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  chains =4, cores=4,start=list(Intercept=mean(data$time_to_repro),b_age=0,
                                b_birth_cat=0,b_education=0,
                                b_agriculture=0,b_repro_within_2_years=0,
                                b_lotta_X_age=0))

path<- (paste0("results/"))
filename <- "Time_to_repro_w_censored_data.rds"

saveRDS(model, paste0(path, filename))




#model <- coxph(Surv(Lifespan,Censor==1) ~ Group+frailty(birthplaceid_seq), data=data)
#Censor==1; 1 is for those who reproduced; 0 is for those who did not
#frailty(Cage) is a random term (intercept). 

#coxph allows only 1 random term.
#coxme allows more random terms, coded as in lmer. For example:
  #coxme <- coxme(Surv(Lifespan,Censor==1) ~ Group+(1|County/State), data=data)  #For county nested in state (as random intercept).
## ## Model 2c - run with time to event analysis
# use the same data frame from 2b above (data)
library(survival)
data$age_1945 <- data$age_1945-min(data$age_1945)
data$age_1945 <- data$age_1945/max(data$age_1945)
data$reproduced <- ifelse(data$kids_after_war==0, 0, 1)
data$c <- Surv(data$time_to_repro,data$reproduced)
model <-coxph(c ~ lotta + birth_cat+age_1945+education+agriculture+repro_within_2_years+lotta:age_1945+
                frailty(birthplaceid_seq),data=data) # with kids after war to make reproduced plus repro with 2 years
plot(Surv(model))

data2 <- data[-which(data$kids_after_war == 0),]
model.no.cens <-coxph(c ~ lotta + birth_cat+age_1945+education+agriculture+repro_within_2_years+lotta:age_1945+
                frailty(birthplaceid_seq),data=data2)  
  
  #  Model 3 IBI's
# run the interbirth intervals to get at reproductive rate
# use birth_ints dataframe
#  # Lottas childless before war model
library(dplyr)
library(rethinking)
library(tidyr)


# path to the folder with the R data files
path <- "../data files/"
# read in person table
file<- "person_data.rds"
p <- readRDS(paste0(path, file))
file<- "children.rds"
children <- readRDS(paste0(path, file))

p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708


p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p_full <- p
p<- p %>% drop_na(first_child_yob) # 60129
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid")
p <- p[complete.cases(p), ] # 48223
# 22878 started before war ends and 25558 started having kids after 1944 

# link children table 
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# you can play around with the ages but now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 

p$birth_plus_13 <- p$birthyear+13
p$lastapp <- ifelse (p$birthyear<1925, p$birthyear+45,1970)

## now make cut off when you want (e.g. age 50 or ages 13-50)

p$year <- mapply(seq, p$birth_plus_13, p$lastapp, SIMPLIFY = FALSE) 
#Creates a 
#sequence for each row,
#so if birth year is 1850 and death year 1900, the cell says 1850:1900.
#Simplify makes a matrix, but we want to keep a dataframe

#unnest creates a new row for each different value within a "cell" - 
#this is taken from the 'year' column created above
p_long <- unnest(p, year) #1546317

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
# give this column a 1 which is the year the kid was born- later this will be linked to the parents df and will signify that the parenst
# gave birth in that year
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
# libnk by children df [parent id] and [id] in the lottas df and by year in the lottas df (the range of years they were potentiatll fertile
# 13 years old to either the year it  was when they turned 45 or 1970- the interview year)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
# name the column if they reproduced in that year 'reproduced'
colnames(twins)[14] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
# # make a dummy variable for women who reproduced within the past two yers before the end of the war (i.e. in 1943 or 1944)
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")

### maybe add another dummy variable for inds who had no kids



# # make a time to repro after 1945 variable
ttr <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945)
# preserve this varibale for birth intervals model
birth_ints <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945) %>% as.data.frame()

# get the sequential difference - birth interval by id's
birth_ints <- birth_ints %>%
  group_by(id) %>%
  mutate(Diff = time_to_repro - lag(time_to_repro)) %>% as.data.frame()

birth_ints$ttr_2 <- ifelse(is.na(birth_ints$Diff),birth_ints$time_to_repro,birth_ints$Diff)
birth_ints$Diff <- NULL


#####

ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro)) 

ttr_3 <- ttr %>% group_by (id) %>%
  dplyr::summarise(minimum = min(time_to_repro))




ttr <- ttr %>% left_join (ttr_2, by="id")
ttr <- ttr %>% left_join (ttr_3, by="id")

ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
ttr <- ttr %>% left_join (ttr_2, by="id")
rm(ttr_2)
# rejoin dummy coded birth within past 2 years to main table
ttr <- ttr %>% left_join (dummy, by="id")
ttr$repro_within_2_years[is.na(ttr$repro_within_2_years)] <- 0 


ttr$post_war_repro_rate <- ttr$maximum/ttr$kids_after_war
ttr$kids_before_war <- ttr$kids-ttr$kids_after_war
# remove duplicate ids for this data frame
ttr <- ttr[!duplicated(ttr[,c("id")]),]
ttr$age_sq <- ttr$age_1945*ttr$age_1945
# key line select ages
ttr <- ttr[which(ttr$age_1945>12 & ttr$age_1945<46),]

#put age in 1945 on the model prediction scale (i.e. the way it went into the model in rethinking)
ttr$age_1945_sc <- ttr$age_1945-min(ttr$age_1945)
ttr$age_1945_sc <- ttr$age_1945/max(ttr$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
ttr<-ttr %>% as.data.frame()  #31916

# read in person data again and get all the women who did not reproduce
path <- "../data files/"
# read in person table
file<- "person_data.rds"
d <- readRDS(paste0(path, file))

d <- d %>% filter (sex==0)
d <- d %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708

d$lotta<- as.numeric(d$lotta)
d$age_1945 <- 1945-d$birthyear
d <- d %>% filter(kids==0)
d$birth_cat <- 0
d$time_to_repro <- 25
d$lotta <- as.numeric(d$lotta)
d$repro_within_2_years <- 0
d$kids_after_war <- 0
d$ttr_2 <- 25
# 28992 started before war ends and 31137 started having kids after 1944 
# add kids-after_war to birth_ints


d <- d %>% select ("id","time_to_repro","lotta","birth_cat","age_1945","kids",
                   "agriculture","education","birthplaceid","ttr_2","repro_within_2_years","kids_after_war" )

d <- d[complete.cases(d), ] # 10030 no repros

# left join bitrth_ints (all repeated measures data) with ttr (first birth only) and add 'repro_within_2_years' to df
new <- ttr %>% select("id","kids_after_war","repro_within_2_years")
birth_ints <- birth_ints %>% left_join (new, by="id")
# change repro_within 2 years so it is only for the first birth
birth_ints <- birth_ints %>% 
  group_by(id) %>% 
  mutate(repro_within_2_years = ifelse(row_number()==1, repro_within_2_years, 0))%>% as.data.frame()
# rbind the 2 data frames
birth_ints <- birth_ints %>% select("id","time_to_repro","lotta","birth_cat","age_1945","kids",
                      "agriculture","education","birthplaceid","ttr_2","repro_within_2_years","kids_after_war") %>% as.data.frame()

data <- rbind(d,birth_ints)# 94,507
data <- data %>% filter(age_1945>15 & age_1945<45)
data <- data%>% arrange(birthplaceid)
data$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(data$birthplaceid))!=0))

data <- data%>% arrange(id)
data$id_seq <- cumsum(c(1,as.numeric(diff(data$id))!=0))

data$ttr_2<- as.integer(data$ttr_2)
#map2stan formula
# run in rethinking
data_list <- list(
  ttr = data$ttr_2,
  id = data$id_seq,
  lotta  = data$lotta,
  birth_cat = data$birth_cat,
  repro_cat = data$repro_within_2_years,
  age = data$age_1945,
  agriculture = data$agriculture,
  education = data$education,
  birthplaceid_seq= data$birthplaceid_seq)


model <- map2stan(
  alist(
    ttr ~ dpois(lambda),
    log(lambda) <- Intercept +
      #a_birthplaceid[birthplaceid_seq] +
      a_id[id]+
      b_lotta*lotta +
      b_age*age +
      b_birth_cat*birth_cat +
      b_repro_cat*repro_cat +
      b_education*education +
      b_agriculture*agriculture +
      b_lotta_X_age*lotta*age,
    
    a_id[id] ~ dnorm (0, sigma),
    sigma ~ dcauchy (0,1),
    Intercept ~ dnorm(0,1),
    b_lotta ~ dnorm(0,1),
    b_age ~ dnorm(0,1),
    b_education ~ dnorm(0,1),
    b_birth_cat ~ dnorm(0,1),
    b_repro_cat  ~ dnorm(0,1),
    b_agriculture ~ dnorm(0,1),
    b_lotta_X_age ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  chains =4, cores=4,start=list(Intercept=mean(data$ttr_2),b_age=0,
                                b_birth_cat=0,b_education=0, b_repro_cat=0,
                                b_agriculture=0,
                                b_lotta_X_age=0))

path<- (paste0("results/"))
filename <- "Birth_ints_w_censored_data_final.rds"

saveRDS(model, paste0(path, filename))

# try it in glmer
library(lme4)
 model <- glmer (ttr_2 ~ lotta + birth_cat+age_1945+education+agriculture+lotta:age_1945+
                   (1|id)+(1|birthplaceid),data=data, family="poisson")
 summary(model)

## survival model
## ## Model 2c - run with time to event analysis
 # use the same data frame from 2b above (data)
 library(survival)
 library(broom)
 data$reproduced <- ifelse(data$kids_after_war==0, 0, 1)
 
 
 data$c <- Surv(data$ttr_2,data$reproduced)
 data <- as.data.frame(data)
 model <-coxph(c ~ lotta + birth_cat+age_1945+education+agriculture+repro_within_2_years+lotta:age_1945+cluster(id),
                 data=data) # with kids after war to make reproduced plus repro with 2 years
 
 
 #alternative (interval censoring instead of right censoring)-interaction is not allowed (Simon wil help)
 data$ageatrepro <- ifelse(data$reproduced == 1, (data$ttr_2 + data$age_1945), 45)
 data$c <- Surv(time = data$age_1945, time2 = data$ageatrepro, event = data$reproduced, type = "interval")
 model <-survfit(c ~ lotta + birth_cat+age_1945+education+agriculture+repro_within_2_years+lotta:age_1945+cluster(id),
               data=data)
 summary(model)
######
 data$agegroup<-ifelse(data$age_1945<23, 1, ifelse(data$age_1945<30,2, 3)) #Make here the age groups of interest; here using three quantiles
 data$agelot<- paste(data$lotta,data$agegroup)
 model2 <-survfit(c ~ lotta + birth_cat+age_1945+education+agriculture+repro_within_2_years+lotta:age_1945+cluster(id),
               data=data) 

 data$agegroup<-ifelse(data$age_1945<21, 1, ifelse(data$age_1945<26,2, 3)) #Make here the age groups of interest; here using three quantiles
 data$agelot<- paste(data$lotta,data$agegroup)
 fit <- survfit(Surv(ttr_2, reproduced) ~ strata(agelot), data=data)
 # Mikhails survival code
 data <- as.data.frame(data)
 data$birthplaceid <- as.factor(data$birthplaceid)
 
 


m1 <- coxph(Surv(time_to_repro,reproduced) ~ strata(lotta), data=data)
m1<-survfit(m1)
plot(m1)
 
# subset data into young group only
young <- data[which (data$age_1945>17 &  data$age_1945<23),]

m2<-coxph(Surv(time_to_repro,reproduced) ~strata(lotta) + birth_cat+age_1945+education+agriculture+repro_within_2_years+
               frailty(birthplaceid),data=young)
m2<-survfit(m2)
plot(m2)
#surviveplot<-subset(data, Group=="lotta", select=BirdID:TimeTreat6MoS)
 
 
 # make ggplot
 library("survminer")
 require("survival")
 # 2 survival models 
 # m1 and m2 (with all other variables)
 
# Basic plots

z <-ggsurvplot(m2, data = young, size = 1,                 # change line size
  palette = 
    c("#E7B800", "#2E9FDF"),# custom color palettes
  conf.int = TRUE,          # Add confidence interval
  #pval = TRUE), 
  fun="event",             # Add p-value
  risk.table = TRUE,
  ylab = "Probability of reproducing",
  xlab = "Year",
 # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs = 
    c("Non-Lottas", "Lottas"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw())     # Change ggplot2 theme
  Z + scale_x_discrete(breaks=c("0","5","10"),
                   labels=c("Dose 0.5", "Dose 1", "Dose 2"))

ggsurv <- ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  data = lung,             # data used to fit survival curves.
  risk.table = TRUE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimates of survival curves.
  palette = c("#E7B800", "#2E9FDF"),
  xlim = c(0,500),         # present narrower X axis, but not affect
  # survival estimates.
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 100,     # break X axis in time intervals by 500.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.height = 0.25, # the height of the risk table
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  ncensor.plot.height = 0.25,
  conf.int.style = "step",  # customize style of confidence intervals
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = 
    c("Male", "Female")    # change legend labels.
)
ggsurv

#### new birth ints model run on cluster
#  Get Data
library(dplyr)
library(rethinking)
library(tidyr)


# path to the folder with the R data files
path<- (paste0("~/r_files/"))
path <- "../data files/"
# read in person table
file<- "person_data.rds"
p <- readRDS(paste0(path, file))
file<- "children.rds"
children <- readRDS(paste0(path, file))

p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708


p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p_full <- p
p<- p %>% drop_na(first_child_yob) # 60129
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid")
p <- p[complete.cases(p), ] # 48223
# 22878 started before war ends and 25558 started having kids after 1944 

# link children table 
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# you can play around with the ages but now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 

p$birth_plus_13 <- p$birthyear+13
p$lastapp <- ifelse (p$birthyear<1925, p$birthyear+45,1970)

## now make cut off when you want (e.g. age 50 or ages 13-50)

p$year <- mapply(seq, p$birth_plus_13, p$lastapp, SIMPLIFY = FALSE) 
#Creates a 
#sequence for each row,
#so if birth year is 1850 and death year 1900, the cell says 1850:1900.
#Simplify makes a matrix, but we want to keep a dataframe

#unnest creates a new row for each different value within a "cell" - 
#this is taken from the 'year' column created above
p_long <- unnest(p, year) #1546317

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
# give this column a 1 which is the year the kid was born- later this will be linked to the parents df and will signify that the parenst
# gave birth in that year
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
# libnk by children df [parent id] and [id] in the lottas df and by year in the lottas df (the range of years they were potentiatll fertile
# 13 years old to either the year it  was when they turned 45 or 1970- the interview year)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
# name the column if they reproduced in that year 'reproduced'
colnames(twins)[14] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
# # make a dummy variable for women who reproduced within the past two yers before the end of the war (i.e. in 1943 or 1944)
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")

### maybe add another dummy variable for inds who had no kids



# # make a time to repro after 1945 variable
ttr <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945)
# preserve this varibale for birth intervals model
birth_ints <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945) %>% as.data.frame()

# get the sequential difference - birth interval by id's
birth_ints <- birth_ints %>%
  group_by(id) %>%
  mutate(Diff = time_to_repro - lag(time_to_repro)) %>% as.data.frame()

birth_ints$ttr_2 <- ifelse(is.na(birth_ints$Diff),birth_ints$time_to_repro,birth_ints$Diff)
birth_ints$Diff <- NULL


#####

ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro)) 

ttr_3 <- ttr %>% group_by (id) %>%
  dplyr::summarise(minimum = min(time_to_repro))




ttr <- ttr %>% left_join (ttr_2, by="id")
ttr <- ttr %>% left_join (ttr_3, by="id")

ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
ttr <- ttr %>% left_join (ttr_2, by="id")
rm(ttr_2)
# rejoin dummy coded birth within past 2 years to main table
ttr <- ttr %>% left_join (dummy, by="id")
ttr$repro_within_2_years[is.na(ttr$repro_within_2_years)] <- 0 


ttr$post_war_repro_rate <- ttr$maximum/ttr$kids_after_war
ttr$kids_before_war <- ttr$kids-ttr$kids_after_war
# remove duplicate ids for this data frame
ttr <- ttr[!duplicated(ttr[,c("id")]),]
ttr$age_sq <- ttr$age_1945*ttr$age_1945
# key line select ages
ttr <- ttr[which(ttr$age_1945>12 & ttr$age_1945<46),]

#put age in 1945 on the model prediction scale (i.e. the way it went into the model in rethinking)
ttr$age_1945_sc <- ttr$age_1945-min(ttr$age_1945)
ttr$age_1945_sc <- ttr$age_1945/max(ttr$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
ttr<-ttr %>% as.data.frame()  #31607
ttr$post_war_repro_rate <- as.integer(ttr$post_war_repro_rate)
#map2stan formula
# run in rethinking
data_list <- list(
  pwrr = ttr$post_war_repro_rate,
  lotta  = ttr$lotta,
  birth_cat = ttr$birth_cat,
  repro_cat = ttr$repro_within_2_years,
  age = ttr$age_1945,
  agriculture = ttr$agriculture,
  education = ttr$education)


model <- map2stan(
  alist(
    pwrr ~ dpois(lambda),
    log(lambda) <- Intercept +
      #a_birthplaceid[birthplaceid_seq] +
      b_lotta*lotta +
      b_age*age +
      b_birth_cat*birth_cat +
      b_repro_cat*repro_cat +
      b_education*education +
      b_agriculture*agriculture +
      b_lotta_X_age*lotta*age,
    
    sigma ~ dcauchy (0,1),
    Intercept ~ dnorm(0,1),
    b_lotta ~ dnorm(0,1),
    b_age ~ dnorm(0,1),
    b_education ~ dnorm(0,1),
    b_birth_cat ~ dnorm(0,1),
    b_repro_cat  ~ dnorm(0,1),
    b_agriculture ~ dnorm(0,1),
    b_lotta_X_age ~ dnorm(0,1)
  ),
  data=data_list, iter=6000, warmup=2000, control=list(max_treedepth=20),
  chains =4, cores=4,start=list(Intercept=mean(ttr$post_war_repro_rate),b_age=0,
                                b_birth_cat=0,b_education=0, b_repro_cat=0,
                                b_agriculture=0,
                                b_lotta_X_age=0))

path<- (paste0("results/"))
filename <- "Post_war_repro_rate_repros_only.rds"

saveRDS(model, paste0(path, filename))



#### Kids after war all non-repros too
#  Get Data
library(dplyr)
library(rethinking)
library(tidyr)
#library(rstanarm)


# path to the folder with the R data files
path<- (paste0("~/r_files/"))


# read in the data used to create the model map2 stan object and the model
# read in person table and children table - up one directory
#path <- "../data files/"
#file2<- "person_data_old.rds"
p <- readRDS(paste0(path, file2))
# load children data
file3 <- "children.rds"
children<-readRDS(paste0(path,file3))
p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708


p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p_full <- p
p<- p %>% drop_na(first_child_yob) # 60129
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid")
p <- p[complete.cases(p), ] # 47793
# 22544 started before war ends and 25249 started having kids after 1944 

# link children table 
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# you can play around with the ages but now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 

p$birth_plus_13 <- p$birthyear+13
p$lastapp <- ifelse (p$birthyear<1925, p$birthyear+45,1970)

## now make cut off when you want (e.g. age 50 or ages 13-50)

p$year <- mapply(seq, p$birth_plus_13, p$lastapp, SIMPLIFY = FALSE) 
#Creates a 
#sequence for each row,
#so if birth year is 1850 and death year 1900, the cell says 1850:1900.
#Simplify makes a matrix, but we want to keep a dataframe

#unnest creates a new row for each different value within a "cell" - 
#this is taken from the 'year' column created above
p_long <- unnest(p, year) #1550622

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
# give this column a 1 which is the year the kid was born- later this will be linked to the parents df and will signify that the parenst
# gave birth in that year
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
# libnk by children df [parent id] and [id] in the lottas df and by year in the lottas df (the range of years they were potentiatll fertile
# 13 years old to either the year it  was when they turned 45 or 1970- the interview year)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
# name the column if they reproduced in that year 'reproduced'
colnames(twins)[14] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
# # make a dummy variable for women who reproduced within the past two yers before the end of the war (i.e. in 1943 or 1944)
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")

### maybe add another dummy variable for inds who had no kids



# # make a time to repro after 1945 variable
ttr <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945)



ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro)) 

ttr_3 <- ttr %>% group_by (id) %>%
  dplyr::summarise(minimum = min(time_to_repro))




ttr <- ttr %>% left_join (ttr_2, by="id")
ttr <- ttr %>% left_join (ttr_3, by="id")

ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
ttr <- ttr %>% left_join (ttr_2, by="id")
rm(ttr_2)
# rejoin dummy coded birth within past 2 years to main table
ttr <- ttr %>% left_join (dummy, by="id")
ttr$repro_within_2_years[is.na(ttr$repro_within_2_years)] <- 0 


ttr$post_war_repro_rate <- ttr$maximum/ttr$kids_after_war
ttr$kids_before_war <- ttr$kids-ttr$kids_after_war
# remove duplicate ids for this data frame
ttr <- ttr[!duplicated(ttr[,c("id")]),]
ttr$age_sq <- ttr$age_1945*ttr$age_1945
# key line select ages
ttr <- ttr[which(ttr$age_1945>12 & ttr$age_1945<46),]

#put age in 1945 on the model prediction scale (i.e. the way it went into the mdel in rethinking)
ttr$age_1945_sc <- ttr$age_1945-min(ttr$age_1945)
ttr$age_1945_sc <- ttr$age_1945/max(ttr$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
ttr<-ttr %>% as.data.frame()  #31613

ttr <- ttr%>% arrange(birthplaceid)
ttr$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(ttr$birthplaceid))!=0))





M3 <- readRDS("model results/Kids_after_war_mixed_model_noagesq.rds")


# add non repros
path <- "../data files/"
# read in person table
file<- "person_data.rds"
d <- readRDS(paste0(path, file))

d <- d %>% filter (sex==0)
d <- d %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708

d$lotta<- as.numeric(d$lotta)
d$age_1945 <- 1945-d$birthyear
d <- d %>% filter(kids==0)
d$birth_cat <- 0
d$time_to_repro <- 25
d$lotta <- as.numeric(d$lotta)
d$repro_within_2_years <- 0
d$kids_after_war <- 0
d$reproduced <- 0
# 28992 started before war ends and 31137 started having kids after 1944 


d <- d %>% select ("id","kids_after_war","time_to_repro","lotta","birth_cat","age_1945",
                   "agriculture","repro_within_2_years","education","birthplaceid" )
d$reproduced <- 0
d$kids_after_war<- as.integer(d$kids_after_war)
d <- d[complete.cases(d), ] # 10172 no repros

# rbind the 2 data frames
ttr <- ttr %>% select("id","kids_after_war","time_to_repro","lotta","birth_cat","age_1945",
                      "agriculture","repro_within_2_years","education","birthplaceid")
ttr$reproduced <- 1
data <- rbind(d,ttr)# 41,946
data <- data %>% filter(age_1945>15 & age_1945<45) #37,750
data <- data%>% arrange(birthplaceid)
data$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(data$birthplaceid))!=0))
data <- as.data.frame(data)
# run in rethinking
data_list <- list(
  kaw = data$kids_after_war,
  lotta  = data$lotta,
  birth_cat = data$birth_cat,
  repro_cat = data$repro_within_2_years,
  age = data$age_1945,
  agriculture = data$agriculture,
  education = data$education,
  birthplace_id = data$birthplaceid_seq)

model <- map2stan(
  alist(
    kaw ~ dpois(lambda),
    log(lambda) <- Intercept +
      a_birthplace_id[birthplace_id] +
      b_lotta*lotta +
      b_age*age +
      b_birth_cat*birth_cat +
      b_education*education +
      b_agriculture*agriculture +
      b_repro_within_2_years*repro_cat +
      b_lotta_X_age*lotta*age,
    
    a_birthplace_id[birthplace_id] ~ dnorm (0, sigma),
    sigma ~ dcauchy (0,1),
    Intercept ~ dnorm(0,1),
    b_lotta ~ dnorm(0,1),
    b_age ~ dnorm(0,1),
    b_education ~ dnorm(0,1),
    b_birth_cat ~ dnorm(0,1),
    b_agriculture ~ dnorm(0,1),
    b_repro_within_2_years ~ dnorm(0,1),
    b_lotta_X_age ~ dnorm(0,1)
  ),
  data=data_list, iter=6000, warmup=1500, control=list(max_treedepth=20),
  chains =4, cores=4,start=list(Intercept=mean(data$kids_after_war),b_age=0,
                                b_birth_cat=0,b_education=0,
                                b_agriculture=0,b_repro_within_2_years=0,
                                b_lotta_X_age=0))




path<- (paste0("results/"))
filename <- "Kids_after_war_includes_non_repros.rds"

saveRDS(model, paste0(path, filename))


### mean birth intervals includes non repros - this is for the survival model in Supp materials Table S4
path <- "../data files/"
# read in person table
file<- "person_data.rds"
p <- readRDS(paste0(path, file))
file<- "children.rds"
children <- readRDS(paste0(path, file))

p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708


p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p_full <- p
p<- p %>% drop_na(first_child_yob) # 60129
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid")
p <- p[complete.cases(p), ] # 48223
# 22878 started before war ends and 25558 started having kids after 1944 

# link children table 
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# you can play around with the ages but now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 

p$birth_plus_13 <- p$birthyear+13
p$lastapp <- ifelse (p$birthyear<1925, p$birthyear+45,1970)

## now make cut off when you want (e.g. age 50 or ages 13-50)

p$year <- mapply(seq, p$birth_plus_13, p$lastapp, SIMPLIFY = FALSE) 
#Creates a 
#sequence for each row,
#so if birth year is 1850 and death year 1900, the cell says 1850:1900.
#Simplify makes a matrix, but we want to keep a dataframe

#unnest creates a new row for each different value within a "cell" - 
#this is taken from the 'year' column created above
p_long <- unnest(p, year) #1546317

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
# give this column a 1 which is the year the kid was born- later this will be linked to the parents df and will signify that the parenst
# gave birth in that year
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
# libnk by children df [parent id] and [id] in the lottas df and by year in the lottas df (the range of years they were potentiatll fertile
# 13 years old to either the year it  was when they turned 45 or 1970- the interview year)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
# name the column if they reproduced in that year 'reproduced'
colnames(twins)[14] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
# # make a dummy variable for women who reproduced within the past two yers before the end of the war (i.e. in 1943 or 1944)
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")

### maybe add another dummy variable for inds who had no kids



# # make a time to repro after 1945 variable
ibi <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945)
# preserve this varibale for birth intervals model
birth_ints <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945) %>% as.data.frame()

# get the sequential difference - birth interval by id's
birth_ints <- birth_ints %>%
  group_by(id) %>%
  mutate(Diff = time_to_repro - lag(time_to_repro)) %>% as.data.frame()

birth_ints$ttr_2 <- ifelse(is.na(birth_ints$Diff),birth_ints$time_to_repro,birth_ints$Diff)
birth_ints$Diff <- NULL


#####

ibi_2 <- ibi  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro)) 

ibi_3 <- ibi %>% group_by (id) %>%
  dplyr::summarise(minimum = min(time_to_repro))




ibi <- ibi %>% left_join (ibi_2, by="id")
ibi <- ibi %>% left_join (ibi_3, by="id")

ibi_2 <- ibi  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
ibi <- ibi %>% left_join (ibi_2, by="id")
rm(ibi_2)
# rejoin dummy coded birth within past 2 years to main table
ibi <- ibi %>% left_join (dummy, by="id")
ibi$repro_within_2_years[is.na(ibi$repro_within_2_years)] <- 0 


ibi$post_war_repro_rate <- ibi$maximum/ibi$kids_after_war
ibi$kids_before_war <- ibi$kids-ibi$kids_after_war
# remove duplicate ids for this data frame
ibi <- ibi[!duplicated(ibi[,c("id")]),]
ibi$age_sq <- ibi$age_1945*ibi$age_1945
# key line select ages
ibi <- ibi[which(ibi$age_1945>12 & ibi$age_1945<46),]

#put age in 1945 on the model prediction scale (i.e. the way it went into the model in rethinking)
ibi$age_1945_sc <- ibi$age_1945-min(ibi$age_1945)
ibi$age_1945_sc <- ibi$age_1945/max(ibi$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
ibi<-ibi %>% as.data.frame()  #31607

# link to non repros
path <- "../data files/"
# read in person table
file<- "person_data.rds"
d <- readRDS(paste0(path, file))

d <- d %>% filter (sex==0)
d <- d %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708

d$lotta<- as.numeric(d$lotta)
d$age_1945 <- 1945-d$birthyear
d <- d %>% filter(kids==0)
d$birth_cat <- 0
d$post_war_repro_rate <- 25
d$lotta <- as.numeric(d$lotta)
d$repro_within_2_years <- 0
d$kids_after_war <- 0
d$reproduced <- 0
# 28992 started before war ends and 31137 started having kids after 1944 


d <- d %>% select ("id","kids_after_war","post_war_repro_rate","lotta","birth_cat","age_1945",
                   "agriculture","repro_within_2_years","education","birthplaceid" )
d$reproduced <- 0
d$kids_after_war<- as.integer(d$kids_after_war)
d <- d[complete.cases(d), ] # 10172 no repros

# rbind the 2 data frames
ibi <- ibi %>% select("id","kids_after_war","post_war_repro_rate","lotta","birth_cat","age_1945",
                      "agriculture","repro_within_2_years","education","birthplaceid")
ibi$reproduced <- 1
data <- rbind(d,ibi)# 41,946
data <- data %>% filter(age_1945>15 & age_1945<45) #37,750
data <- data%>% arrange(birthplaceid)
data$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(data$birthplaceid))!=0))

# run survival model for mean birth ints with censored data
library(survival)
data$age_1945 <- data$age_1945-min(data$age_1945)
data$age_1945 <- data$age_1945/max(data$age_1945)
data$reproduced <- ifelse(data$kids_after_war==0, 0, 1)
data$c <- Surv(data$post_war_repro_rate,data$reproduced)
model <-coxph(c ~ lotta + birth_cat+age_1945+education+agriculture+repro_within_2_years+lotta:age_1945+
                frailty(birthplaceid_seq),data=data) 


