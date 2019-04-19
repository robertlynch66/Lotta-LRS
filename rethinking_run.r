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
file2<- "person_data.rds"
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
