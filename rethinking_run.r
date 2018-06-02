# Lottas childless before war model
library(dplyr)
library(rethinking)
library(tidyr)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
# read in person table
file<- "person_data.rds"
p <- readRDS(paste0(path, file))
# read in children table
#children <- readRDS("~/r_files/children.rds")

# convert booleans to numeric
#p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
#p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
# deleted retarded rows
# p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))


p <- p %>% filter (sex==0)
p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708

# get all childless women in 1940 and those who never gave birth 
#(adding kids =0 make this analysis more conservative)
#p <- p %>% filter (first_child_yob>1944 | ( is.na(first_child_yob) & kids==0 ))#44041
#p <- p %>% filter (first_child_yob>1944 )#29533




p <- p %>% filter (first_child_yob>1944 & age_1945>12 & age_1945<40 )#35110
# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values -deletes
# 281 women from full data

p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945")
p <- p[complete.cases(p), ]

#################Age at first birth model

p$age_sq <- p$age_1945*p$age_1945
#center the predictors 
p$age_c <- p$age_1945 - mean (p$age_1945)
p$age_sq_c <- p$age_sq - mean(p$age_sq)

#map2stan formula

# run in rethinking
data_list <- list(
  lotta  = p$lotta,
  age = p$age_c,
  age_sq = p$age_sq_c,
  agriculture = p$agriculture,
  education = p$education,
  age_at_first_birth= p$age_at_first_birth)

model <- map2stan(
  alist(
    age_at_first_birth ~ dpois(lambda),
    log(lambda) <- Intercept +
      b_lotta*lotta +
      b_age*age +
      b_age_sq*age_sq +
      b_education*education +
      b_agriculture*agriculture +
      b_lotta_X_age*lotta*age +
      b_lotta_X_age_sq*lotta*age_sq,
    Intercept ~ dnorm(0,10),
    b_lotta ~ dnorm(0,10),
    b_age ~ dnorm(0,10),
    b_age_sq ~ dnorm(0,10),
    b_education ~ dnorm(0,10),
    b_agriculture ~ dnorm(0,10),
    b_lotta_X_age ~ dnorm(0,10),
    b_lotta_X_age_sq ~ dnorm(0,10)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(b_lotta=0,b_age=0, b_education=0,b_agriculture=0, b_lotta_X_age=0,
             b_lotta_X_age_sq=0),
  chains =4, cores=4)

path<- (paste0("results/"))
filename <- "AAFB_lottas_age_and_age_sq.rds"

saveRDS(model, paste0(path, filename))
