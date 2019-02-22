path <- "C:/Users/rofrly/Dropbox/Github/data files/"
#path <- "C:/Users/robert/Dropbox/Github/data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
# load children data
file3 <- "children.rds"
children <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas_2/children.rds")
#children <- readRDS("/home/robert/Dropbox/Github/Lottas_2/children.rds")
library(dplyr)
library(tidyr)
library(rethinking)
library(lme4)
library(plyr)

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
"age_at_first_birth","age_1945","birth_cat","kids","professionid","birthplaceid")
p <- p[complete.cases(p), ] # 48436
# 22878 started before war ends and 25558 started having kids after 1944 




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
colnames(twins)[15] <- "reproduced"
twins$reproduced[is.na(twins$reproduced)] <- 0


twins$age <- twins$year-twins$birthyear
rm(p_long)
# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year",
                           "reproduced","age","age_1945","age_at_first_birth","birth_cat","kids","professionid","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid

# add a gave birth in 1943 or 1944 dummy variable
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
# jouin reproduced within past 2 years to main table
birthrate <- birthrate %>% left_join (dummy, by="id")

rm(dummy)
birthrate$repro_within_2_years[is.na(birthrate$repro_within_2_years)] <- 0

birthrate$post_war_repro_rate <- birthrate$maximum/birthrate$kids_after_war
birthrate$kids_before_war <- birthrate$kids-birthrate$kids_after_war
# remove duplicate ids for this data frame
birthrate <- birthrate[!duplicated(birthrate[,c("id")]),]
###  limit ages to 17 to 40 after the war #27490 obs
birthrate$age_sq <- birthrate$age_1945*birthrate$age_1945
birthrate <- birthrate[which(birthrate$age_1945>12 & birthrate$age_1945<46),] #32045

#rescale age_1945
birthrate$age_1945 <- birthrate$age_1945-min(birthrate$age_1945)

# Models
model1 <-glm(kids_after_war ~  lotta*age_1945+ birth_cat+education + agriculture+repro_within_2_years,  
            data = birthrate, family = poisson) 
summary(model1)
m1 <- drop1(model1)
m1


model2 <-glm(time_to_repro ~ lotta*age_1945+birth_cat+ education + agriculture+repro_within_2_years,  
            data = birthrate, family = poisson) 
summary(model2)
m1 <- drop1(model2)
m1

# with random effects
library(lme4)

model3 <-glmer(kids_after_war ~  lotta*age_1945+ birth_cat+education + agriculture+repro_within_2_years+
                 (1|professionid),  
               data = birthrate, family = "poisson",  
control = glmerControl(optimizer="nloptwrap", optCtrl=list(maxfun=100000)))
summary(model3)
m1 <- drop1(model3)
m1
model4 <-glmer(time_to_repro ~  lotta*age_1945+ birth_cat+education + agriculture+repro_within_2_years+
                 (1|birthplaceid),  
               data = birthrate, family = "poisson",  
               control = glmerControl(optimizer="nloptwrap", optCtrl=list(maxfun=100000)))
summary(model4)
m1 <- drop1(model4)
m1