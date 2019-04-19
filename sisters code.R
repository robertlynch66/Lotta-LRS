#  Get Data
library(dplyr)
library(rethinking)
library(tidyr)
#library(rstanarm)


# path to the folder with the R data files
path<- (paste0("~/r_files/"))


# read in the data used to create the model map2 stan object and the model
# read in person table and children table - up one directory
path <- "../data files/"
file2<- "person_data_old.rds"
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
p<- p %>% drop_na(lastapp)
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



# read in person table

d <- readRDS(paste0(path, file2))

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
data <- data %>% filter(age_1945>15 & age_1945<45) #37,613
data <- data%>% arrange(birthplaceid)
data$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(data$birthplaceid))!=0))
data <- as.data.frame(data)

# add mother id
path <- "../data files/"
file2<- "person_data.rds"
m <- readRDS(paste0(path, file2))
m <- m %>% select ('id','motherid')

data <- data %>% left_join(m, by=c('id'='id'))
data2 <- data %>% filter(reproduced==1)



a <- as.data.frame(table(data$motherid))
a <- a %>% filter(Freq>1)
names(a) <- c('motherid','sisters')
data$motherid <- as.factor(data$motherid)
# join it to p
# # use data for all and data2 for repros only
sis_only <- data %>% left_join(a, by=c('motherid'='motherid'))
sis_only <- sis_only %>% filter (sisters>1)


sis_only$age_1945s <- scale(sis_only$age_1945)
library(lme4)
model <- glmer(time_to_repro~ birth_cat + agriculture + repro_within_2_years + education + 
                  lotta*age_1945s + (1|motherid) , family=poisson, data=sis_only)
summary(model)
model2 <- glmer(kids_after_war~ birth_cat + agriculture + repro_within_2_years + education + 
                  lotta*age_1945s + (1|motherid) , family=poisson, data=sis_only)
summary(model2)
library(MuMIn)
AICc(model2)

### get birth intervals
path <- "../data files/"
# read in person table
file<- "person_data_old.rds"
p <- readRDS(paste0(path, file))
file<- "children.rds"
children <- readRDS(paste0(path, file))

p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708

p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
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

# add mother id
# add mother id
path <- "../data files/"
file2<- "person_data.rds"
m <- readRDS(paste0(path, file2))
m <- m %>% select ('id','motherid')

data <- ttr %>% left_join(m, by=c('id'='id'))


a <- as.data.frame(table(data$motherid))
a <- a %>% filter(Freq>1)
names(a) <- c('motherid','sisters')
data$motherid <- as.factor(data$motherid)
# join it to p
# # use data for all and data2 for repros only
sis_only <- data %>% left_join(a, by=c('motherid'='motherid'))
sis_only <- sis_only %>% filter (sisters>1)


sis_only$age_1945s <- scale(sis_only$age_1945)
library(lme4)
model <- glmer(post_war_repro_rate~ birth_cat + agriculture + repro_within_2_years + education + 
                 lotta*age_1945s + (1|motherid) , family=poisson, data=sis_only)
summary(model)


#### get lottas who had sisters who were not Lottas
#####################################################
#####################################################
#####################################################
#####################################################
#####################################################
#####################################################
#####################################################
#####################################################
library(dplyr)
library(rethinking)
library(tidyr)


# path to the folder with the R data files
#path<- (paste0("~/r_files/"))
library(dplyr)
#C:\Users\rofrly\Dropbox\Github\data files
path <- "../data files/"
# read in person table
file<- "person_data.rds"
p <- readRDS(paste0(path, file))
file<- "children.rds"
children <- readRDS(paste0(path, file))

p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#82031


p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p_full <- p
p<- p %>% drop_na(first_child_yob) # 60129
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid","motherid","fatherid")


p <- p[complete.cases(p), ] # 16530
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
colnames(twins)[16] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid","motherid","fatherid")



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
                   "agriculture","repro_within_2_years","education","birthplaceid", "fatherid",                       "motherid" )
d$reproduced <- 0
d$kids_after_war<- as.integer(d$kids_after_war)
d <- d[complete.cases(d), ] # 10172 no repros

# rbind the 2 data frames
ttr <- ttr %>% select("id","kids_after_war","time_to_repro","lotta","birth_cat","age_1945",
                      "agriculture","repro_within_2_years","education","birthplaceid","fatherid",                       "motherid" )
ttr$reproduced <- 1
data <- rbind(d,ttr)# 41,946
data <- data %>% filter(age_1945>15 & age_1945<45) #10,547
data <- data%>% arrange(birthplaceid)
data$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(data$birthplaceid))!=0))
# find full sisters
# put the mothers and 'fathers' in order
p <- arrange(data, motherid)


# get females only
#p <- p %>% filter (sex==0)
a <- as.data.frame(table(p$motherid))
a <- a %>% filter(Freq>1)
names(a) <- c('motherid','sisters')
p$motherid <- as.factor(p$motherid)
# join it to p
new <- p %>% left_join(a, by=c('motherid'='motherid'))

new <- new %>% filter (sisters >1 & sisters <3)
# remove lottas with NA
new <- new %>% filter (!is.na(lotta))


new <- arrange(new, motherid)
# do  all sisters
# make an empty vector
new$sequence <- rep(NA, length(new$id))

for(i in seq_along(new$id)){ #i stands in for a number - you could use any other letter if you wanted to
  # go along by row in order using the id column
  counter = new[i,] #sets counter to be ith row of dataframe
  # set the counter at the first row (row 1)
  sister = new[which(new$motherid == counter$motherid & new$id != counter$id),] #looks in dataframe for the person with the same mother AND a different ID
  
  if(isTRUE(counter$lotta == sister$lotta) == T){new$sequence[i] = 1} #if lotta status is the same, set sequence as 1 for that row
  else{new$sequence[i] = 0} #otherwise, 0
}

new <- new[which(new$sequence == 0),] #make new dataframe by only keeping cases where sequence is 0 


# now do the repros sisters only and age is under 20
p1 <- data %>% filter( age_1945>19)
# for IBI's (taken from below)
# link data to pwrr info
ttr <- ttr %>% select ('id','post_war_repro_rate')
p2 <- data %>% left_join(ttr, by=c("id"="id")) %>% filter (age_1945>19)
library(tidyr)
p2 <- p2  %>% drop_na(post_war_repro_rate)
p1 <- arrange(p1, motherid)
p2 <- arrange(p2, motherid)

# get females only
#p <- p %>% filter (sex==0)
a <- as.data.frame(table(p1$motherid))
a <- a %>% filter(Freq>1)
names(a) <- c('motherid','sisters')
p1$motherid <- as.factor(p1$motherid)
# join it to p
new2 <- p1 %>% left_join(a, by=c('motherid'='motherid'))

new2 <- new2 %>% filter (sisters >1 & sisters <3)
# remove lottas with NA
new2 <- new2 %>% filter (!is.na(lotta))


new2 <- arrange(new2, motherid)
new2$sequence <- rep(NA, length(new2$id))

for(i in seq_along(new2$id)){ #i stands in for a number - you could use any other letter if you wanted to
  # go along by row in order using the id column
  counter = new2[i,] #sets counter to be ith row of dataframe
  # set the counter at the first row (row 1)
  sister = new2[which(new2$motherid == counter$motherid & new2$id != counter$id),] #looks in dataframe for the person with the same mother AND a different ID
  
  if(isTRUE(counter$lotta == sister$lotta) == T){new2$sequence[i] = 1} #if lotta status is the same, set sequence as 1 for that row
  else{new2$sequence[i] = 0} #otherwise, 0
}

new2 <- new2[which(new2$sequence == 0),] #make new dataframe by only keeping cases where sequence is 0 

summary(new2)
# new id all sisater, new 2 is repro pairs only

# yeah new2 is 412 people - 206 sister pairs
se <- function(x) sqrt(var(x)/length(x))
new2$lotta<-as.factor(new2$lotta)
new3$lotta<-as.factor(new3$lotta)
# get cats
u20r <- new2 %>% filter(age_1945<19&reproduced==1)
u20r <- u20r[-c(21), ] 
u20 <- new2 %>% filter(age_1945<21)
o25 <- new2 %>% filter(age_1945>24)
o25r <- new2 %>% filter(age_1945>25&reproduced==1)
aggregate(kids_after_war ~lotta,data=o20,FUN = function(o20) c(mean = mean(o20), se = se(o20)))


# t tests


## Formula interface

t.test(kids_after_war ~ lotta, data = new2, var.equal = TRUE)
# 
model <-glm(kids_after_war~ birth_cat + agriculture + repro_within_2_years + education + lotta*age_1945 , data=new2)
model <-glm(kids_after_war~ lotta*age_1945 , data=new2)
model <-glm(time_to_repro~ birth_cat + agriculture +  education + lotta*age_1945 , data=new3)
model <-glm(time_to_repro~ lotta*age_1945 , data=new2)
summary(model)
#### next do sisters but mean birth ints
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
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid",'motherid')
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
colnames(twins)[15] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid",'motherid')
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
ttr<-ttr %>% as.data.frame()  #21763

p <- arrange(ttr, motherid)
a <- as.data.frame(table(p$motherid))
a <- a %>% filter(Freq>1)
names(a) <- c('motherid','sisters')

# join it to p
new <- ttr %>% left_join(a, by=c('motherid'='motherid'))

new <- new %>% filter (sisters >1 & sisters <3)
# remove lottas with NA
new <- new %>% filter (!is.na(lotta))
new <- arrange(new, motherid)
# make an empty vector
new$sequence <- rep(NA, length(new$id))

for(i in seq_along(new$id)){ #i stands in for a number - you could use any other letter if you wanted to
  # go along by row in order using the id column
  crr = new[i,] #sets crr to be ith row of dataframe
  # set the counter at the first row (row 1)
  sis = new[which(new$motherid == crr$motherid & new$id != crr$id),] #looks in dataframe for the person with the same mother AND a different ID
  
  if(isTRUE(crr$lotta == sis$lotta) == T){new$sequence[i] = 1} #if lotta status is the same, set sequence as 1 for that row
  else{new$sequence[i] = 0} #otherwise, 0
}

new2 <- new[which(new$sequence == 0),] #make new dataframe by only keeping cases where sequence is 0 
new3 <- new2 %>% filter (reproduced==1)
# yeah new2 is 412 people - 206 sister pairs
se <- function(x) sqrt(var(x)/length(x))
new2$lotta<-as.factor(new2$lotta)
new3$lotta<-as.factor(new3$lotta)

# filter ages
new2 <- new2 %>% filter(age_1945>16 &age_1945<41)
# get cats
u20 <- new2 %>% filter(age_1945<20)
#u20 <- new2 %>% filter(age_1945<21)
o20 <- new2 %>% filter(age_1945>19)
o20r <- new2 %>% filter(age_1945>19&reproduced==1)
aggregate(post_war_repro_rate ~lotta,data=o20,FUN = function(o20) c(mean = mean(o20), se = se(o20)))


# t tests
plot(post_war_repro_rate ~ lotta, data=o20)
## Traditional interface
with(u20r, t.test(time_to_repro[lotta == 0], time_to_repro[lotta == 1]))
## Formula interface
t.test(post_war_repro_rate ~ lotta, data = u20)
# }
# 
model <-glm(post_war_repro_rate~ birth_cat + agriculture + repro_within_2_years + education + lotta*age_1945 , data=new2)
