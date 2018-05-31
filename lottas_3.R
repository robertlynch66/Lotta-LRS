path <- "C:/Users/rofrly/Dropbox/Working papers/R data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
library(dplyr)
library(tidyr)
library(lme4)
library(rethinking)
# convert booleans to numeric
p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
# deleted retarded rows
# p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))


p <- p %>% filter (sex==0)
p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708

# get all childless women in 1940 and those who never gave birth 
#(adding kids =0 make this analysis more conservative)
   #p <- p %>% filter (first_child_yob>1944 | ( is.na(first_child_yob) & kids==0 ))#44041
   #p <- p %>% filter (first_child_yob>1944 )#29533
p <- p %>% filter (first_child_yob>1944 | weddingyear>1944 )#35110
# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values -deletes
# 281 women from full data

p <- p %>% select ("id","lotta","birthyear","agriculture","education","martta","never_married")
p <- p[complete.cases(p), ]

# load children data
children <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas_2/children.rds")
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

# link p to children and add column for each kids birth year
# Year, Reproduced, Age


# possible DV'S kids, age at first birth and birth intervals
#read in the data, as you normally would



#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# you can play around with the ages but now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 
# drop rows with na's in the birth year column
p<- p %>% drop_na(birthyear)

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
p_long <- unnest(p, year)

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
colnames(twins)[11] <- "reproduced"
twins$reproduced[is.na(twins$reproduced)] <- 0

twins$war_year <- ifelse(twins$year>1938 & twins$year<1946 , 1,0)
twins$age <- twins$year-twins$birthyear



# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","martta","never_married","year",
                           "reproduced","war_year","age")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
no_twins <- twins[!duplicated(twins[,c("id","year")]),]


# so no_twins is no multiple births in same year, and twins includes them

# run time series analysis in glmer
model <-glmer(reproduced ~ lotta*age + education + agriculture + martta +
                (1|year), data = no_twins, family = binomial, 
        control = glmerControl(optimizer="nloptwrap", optCtrl=list(maxfun=100000)))
# compare AIC scores between models droppin a variable
m1 <- drop1(model)

summary(model)

# turn the formula into a map formula with glimmer

data(cars)
glimmer(reproduced ~ lotta + education + agriculture + martta + age + age*lotta +
          (1|year) +(1|id), data = twins, family = binomial)


model <- map2stan(alist(
  dist ~ dnorm( mu , sigma ),
  mu <- Intercept +
    b_speed*speed,
  Intercept ~ dnorm(0,10),
  b_speed ~ dnorm(0,10),
  sigma ~ dcauchy(0,2)
),data=cars)
  
#time series example in map2stan
intestinal_resistance ~ N( mu,sigma )
mu= f(time,antibiotics,patient )

# run in rethinking