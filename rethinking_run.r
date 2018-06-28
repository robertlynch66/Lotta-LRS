# Lottas childless before war model
library(dplyr)
library(rethinking)
library(tidyr)


# path to the folder with the R data files
path<- (paste0("~/r_files/"))
# read in person table
file<- "person_data.rds"
p <- readRDS(paste0(path, file))
file<- "children.rds"
children <- readRDS(paste0(path, file))

p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708
p<- p %>% drop_na(first_child_yob) # 60129
p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
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
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
colnames(twins)[14] <- "reproduced"
twins$reproduced[is.na(twins$reproduced)] <- 0


twins$age <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year",
                           "reproduced","age","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")
# # make a birth rate category
birthrate <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age-age_1945)

birthrate_2 <- birthrate  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro))
birthrate <- birthrate %>% left_join (birthrate_2, by="id")
birthrate_2 <- birthrate  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
birthrate <- birthrate %>% left_join (birthrate_2, by="id")
rm(birthrate_2)
# rejoin dummy coded birth within past 2 years to main table
birthrate <- birthrate %>% left_join (dummy, by="id")
birthrate$repro_within_2_years[is.na(birthrate$repro_within_2_years)] <- 0 


birthrate$post_war_repro_rate <- birthrate$maximum/birthrate$kids_after_war
birthrate$kids_before_war <- birthrate$kids-birthrate$kids_after_war
# remove duplicate ids for this data frame
birthrate <- birthrate[!duplicated(birthrate[,c("id")]),]
birthrate$age_sq <- birthrate$age_1945*birthrate$age_1945
# key line select ages
birthrate <- birthrate[which(birthrate$age_1945>12 & birthrate$age_1945<46),]
birthrate$age_1945 <- birthrate$age_1945-min(birthrate$age_1945)
birthrate$age_1945 <- birthrate$age_1945/max(birthrate$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
p<-birthrate %>% as.data.frame()  #31613

p <- p%>% arrange(birthplaceid)
p$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(p$birthplaceid))!=0))

# rescale some predictors

# get rid of women who are seen to have kids after the age of 50
p$ageatbirth <- p$age + p$time_to_repro
p <- p %>% filter (ageatbirth<51 & age>16)

# we should have 30691 individuals who are bewteen the ages of 17 and 45

#map2stan formula
# run in rethinking
data_list <- list(
  kids_after_war = p$kids_after_war,
  time_to_repro = p$time_to_repro,
  lotta  = p$lotta,
  birth_cat = p$birth_cat,
  age = p$age_1945,
  agriculture = p$agriculture,
  repro_within_2_years = p$repro_within_2_years,
  education = p$education,
  birthplaceid_seq= p$birthplaceid_seq)


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
    b_age_sq ~ dnorm(0,1),
    b_education ~ dnorm(0,1),
    b_birth_cat ~ dnorm(0,1),
    b_agriculture ~ dnorm(0,1),
    b_repro_within_2_years ~ dnorm(0,1),
    b_lotta_X_age ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  chains =1, cores=1,start=list(Intercept=mean(p$time_to_repro),b_age=0,
                                 b_age_sq=0,b_birth_cat=0,b_education=0,
                                 b_agriculture=0,b_repro_within_2_years=0,
                                 b_lotta_X_age=0))

path<- (paste0("results/"))
filename <- "Time_to_repro_new.rds"

saveRDS(model, paste0(path, filename))

#start=list( sigma_actor=c(1,1,1), sigma_block=c(1,1,1) ),
#constraints=list( sigma_actor="lower=0", sigma_block="lower=0" ),