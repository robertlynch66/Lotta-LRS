# Lottas childless before war model
library(dplyr)
library(rethinking)

# path to the folder with the R data files
path<- (paste0("~/r_files/"))

file<- "person_data.rds"
p <- readRDS(paste0(path, file))
# donkey
################################################################################################################
######################Model 2####################################################################
################################################################################################
################Women who got married before war or never married############
######################################################################################
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$servedduringwar_husband<- as.numeric(p$servedduringwar_husband)
# replace NAs with 0's for unmarrieds in husband served and husband injured cats
p$servedduringwar_husband[p$never_married==1]<- 0
p$injuredinwar_husband<- as.numeric(p$injuredinwar_husband)
p$injuredinwar_husband[p$never_married==1]<- 0
p$outbred2 <- ifelse(p$outbred==0 | is.na(p$outbred), 0, 1)
# add age in 1944
p$age <- 1944-p$birthyear
# kids over 18 by 1944
p$emancipated_kids <- ifelse(p$last_child_yob>1927 | is.na(p$last_child_yob),0,1)
p <- p %>% filter (sex==0)
# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values
p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))

# Women who married before 1945 or never married - exclude all the ones who got married after 1940
p <- p %>% filter (weddingyear<1940 |  never_married==1 | first_child_yob<1940 )
p <- p %>% filter (age>18)
# select complete cases for models
p <- p %>% select("lotta","age","sons","daughters","agriculture","returnedkarelia","outbred2",
                  "education","servedduringwar_husband","injuredinwar_husband","never_married")

p<- p[complete.cases(p),]

# bayesian analysis
data_list <- list(
  lotta  = p$lotta,
  age = p$age,
  sons = p$sons,
  daughters = p$daughters,
  agriculture = p$agriculture,
  education = p$education,
  returnedkarelia = p$returnedkarelia,
  outbred = p$outbred2,
  served = p$servedduringwar_husband,
  injured = p$injuredinwar_husband,
  never_married = p$never_married)

model <- map2stan(
  alist(
    lotta ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a +
      ba*age +
      bs*sons +
      bd*daughters +
      bag*agriculture +
      bed*education +
      brk*returnedkarelia +
      bo*outbred +
      bserv*served+
      binj*injured+
      bnm*never_married,
    
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,10),
    # priors for all slopes (b terms) in main model
    c(ba,bs,bd,bag,bed,brk,bo,bserv,binj,bnm) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(ba=0,bs=0,bd=0,bag=0,bed=0,bo=0,brk=0,bserv=0,binj=0,bnm=0), chains =4, cores=4)

path<- (paste0("results/"))
filename <- "lottas_married_or_had kids before_1940_or never married_2.rds"

saveRDS(model, paste0(path, filename))
