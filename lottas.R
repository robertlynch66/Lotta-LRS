################################################################################################################
######################Model 1####################################################################
################################################################################################
################Lottas who never married or were childless or single in 1939###########
######################################################################################
path <- "C:/Users/rofrly/Dropbox/Working papers/R data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
library(dplyr)
library("lme4")
library("MuMIn")
library("lsmeans")
# convert booleans to numeric
p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
# birth year must be earlier than 1920 for Katiha table and select females
p <- p %>% filter (birthyear<1920 & sex==0)
p$age <- 1944-p$birthyear

# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values
p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))

# get all childless women in 1940 and those who never gave birth 
#(adding kids =0 make this analysis more conservative)
p <- p %>% filter (first_child_yob>1939 | ( is.na(first_child_yob) & kids==0 ))


#  run the dredge code to predict lotta service and a seperate model to predict LRS
# select variables
p <- p %>% select("lotta","age","brothers","sisters","agriculture","never_married","education")

p1<- p[complete.cases(p),]

################## DREDGE MODEL RANK ####################
options(na.action = "na.fail") 

model<-glm(lotta ~  age +
            education + agriculture  + brothers+sisters+never_married, data=p1,
           family = binomial)
modelset<-dredge(model, rank = AICc, trace=FALSE) 
summary(modelset)


# Model summaries################
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
summary(model)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")
######################################################
# Rethinking code for Bayesian analysis###############
#####################################################
data_list <- list(
  lotta  = p1$lotta,
  age = p1$age,
  sons = p1$brothers,
  daughters = p1$sisters,
  agriculture = p1$agriculture,
  education = p1$education,
  never_married = p1$never_married)

model <- map2stan(
  alist(
    lotta ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a +
      ba*age +
      bb*brothers +
      bs*sisters +
      bag*agriculture +
      bed*education +
      bnm*never_married,
    
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,10),
    # priors for all slopes (b terms) in main model
    c(ba,bb,bs,bag,bed,bnm) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(ba=0,bs=0,bd=0,bag=0,bed=0,bserv=0,binj=0,bnm=0), chains =4, cores=4)

path<- (paste0("results/"))
filename <- "lottas_unmarried_and_childless_in_1940.rds"

saveRDS(model, paste0(path, filename))

################################################################################################################
######################Model 2####################################################################
#########################################################################################
#### Women who got married before war or never married#############################################################################
##########################################################################################
path <- "C:/Users/rofrly/Dropbox/Working papers/R data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
library(dplyr)
library("lme4")
library("MuMIn")
library("lsmeans")
# convert booleans to numeric
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

p2<- p[complete.cases(p),]

################## DREDGE MODEL RANK ####################
options(na.action = "na.fail") 

model<-glm(lotta ~  age +sons+daughters+ agriculture+education+outbred2+returnedkarelia+
             servedduringwar_husband+injuredinwar_husband+never_married, data=p,
           family = binomial)
modelset<-dredge(model, rank = AICc, trace=FALSE)
summary(modelset)


## Model Summaries
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
summary(model)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")

#####################################################################################
## Rethinking Bayes code for model 2 #################################
##################################################################
data_list <- list(
  lotta  = p2$lotta,
  age = p2$age,
  sons = p2$sons,
  daughters = p2$daughters,
  agriculture = p2$agriculture,
  education = p2$education,
  served = p2$servedduringwar_husband,
  injured = p2$injuredinwar_husband,
  returnedkarelia =p2$returnedkarelia,
  outbred = p2$outbred2,
  
  never_married = p2$never_married)

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
      bserv*served+
      binj*injured+
      bo*outbred +
      brk*returnedkarelia +
      bnm*never_married,
    
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,10),
    # priors for all slopes (b terms) in main model
    c(ba,bs,bd,bag,bed,bserv,binj,bnm,bo,brk) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(ba=0,bs=0,bd=0,bag=0,bed=0,bserv=0,binj=0,bnm=0,bo=0,brk=0), chains =4, cores=4)

path<- (paste0("results/"))
filename <- "lottas_married_or_had kids before_1940_or never married_2.rds"

saveRDS(model, paste0(path, filename))

################################################################################################################
######################Model 3####################################################################
## Run model with kids over 18 by 1944#############################################################
#####################################################################
path <- "C:/Users/rofrly/Dropbox/Working papers/R data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
library(dplyr)
library("lme4")
library("MuMIn")
library("lsmeans")
# convert booleans to numeric
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$servedduringwar_husband<- as.numeric(p$servedduringwar_husband)
# replace NAs with 0's for unmarrieds in husband served and husband injured cats
p$servedduringwar_husband[p$never_married==1]<- 0
p$injuredinwar_husband<- as.numeric(p$injuredinwar_husband)
p$injuredinwar_husband[p$never_married==1]<- 0
# add age in 1944
p$age <- 1944-p$birthyear
# kids over 18 by 1944
p$emancipated_kids <- ifelse(p$last_child_yob>1927 | is.na(p$last_child_yob),0,1)
p <- p %>% filter (sex==0)
# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values
p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))

# Women who married before 1945 or never married - exclude all the ones who got married after 1940
p <- p %>% filter (weddingyear<1940 |  never_married==1 | first_child_yob<1940 )
p <- p %>% filter (age>18 & emancipated_kids==1)

p <- p %>% select("lotta","age","sons","daughters","agriculture",
                         "education","servedduringwar_husband","injuredinwar_husband","never_married")

p3<- p[complete.cases(p),]

####Model Ranks Dredge#####################
options(na.action = "na.fail") 

model<-glm(lotta ~  age +sons+daughters+ agriculture+education+
             servedduringwar_husband+injuredinwar_husband+never_married, data=p3,
           family = binomial)
modelset<-dredge(model, rank = AICc, trace=FALSE)
summary(modelset)

#Model summaries
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
summary(model)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")


## Rethinking code for model 3 - emancipated kids ###############################
################################################################################
library(dplyr)
library(rethinking)

# path to the folder with the R data files
path<- (paste0("~/r_files/"))

file<- "person_data.rds"
p <- readRDS(paste0(path, file))

p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$servedduringwar_husband<- as.numeric(p$servedduringwar_husband)
# replace NAs with 0's for unmarrieds in husband served and husband injured cats
p$servedduringwar_husband[p$never_married==1]<- 0
p$injuredinwar_husband<- as.numeric(p$injuredinwar_husband)
p$injuredinwar_husband[p$never_married==1]<- 0
# add age in 1944
p$age <- 1944-p$birthyear
# kids over 18 by 1944
p$emancipated_kids <- ifelse(p$last_child_yob>1927 | is.na(p$last_child_yob),0,1)
p <- p %>% filter (sex==0)
# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values
p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))

# Women who married before 1945 or never married - exclude all the ones who got married after 1940
p <- p %>% filter (weddingyear<1940 |  never_married==1 | first_child_yob<1940 )
p <- p %>% filter (age>18 & emancipated_kids==1)

p <- p %>% select("lotta","age","sons","daughters","agriculture",
                  "education","servedduringwar_husband","injuredinwar_husband","never_married")

p<- p[complete.cases(p),]

print(nrow(p))
##########################################################
## Rethinking code for Bayesian analysis########################
#######################################################
data_list <- list(
  lotta  = p$lotta,
  age = p$age,
  sons = p$sons,
  daughters = p$daughters,
  agriculture = p$agriculture,
  education = p$education,
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
      bserv*served+
      binj*injured+
      bnm*never_married,
    
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,10),
    # priors for all slopes (b terms) in main model
    c(ba,bs,bd,bag,bed,bserv,binj,bnm) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(ba=0,bs=0,bd=0,bag=0,bed=0,bserv=0,binj=0,bnm=0), chains =4, cores=4)

path<- (paste0("results/"))
filename <- "lottas_emancipated_kids.rds"

saveRDS(model, paste0(path, filename))

############################################################################################
###########################################################################################
# get model predictions for absolute effects for each prediction

#Load the 3 models
mod_1 <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas/Models/Model_1.rds")
mod_2 <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas/Models/Model_2.rds")
mod_3 <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas/Models/Model_3.rds")
# Model 1
attach(p1)
lottas_1 <- tidyr::crossing(
  age = mean(age),
  #outbred = mean (outbred),# the "L" makes the value an integer, avoiding possible errors
  age = mean(age),
  brothers = 0L,
  sisters=mean(sisters),
  agriculture=mean(agriculture),
  education = mean(education),
  never_married = mean(never_married)) %>%
  as.data.frame()
detach(p1)

link_1 <- link(mod_1, data=lottas_1)
# get means
mu <- apply(link_1,2,mean)
#get PI's'
pi <- t(apply(link_1,2, PI))

mu
pi
#bullshit
# Model 2
attach(p2)
lottas_2 <- tidyr::crossing(
  age = mean(age),
  #outbred = mean (outbred),# the "L" makes the value an integer, avoiding possible errors
  sons = mean(sons),
  daughters=mean(daughters),
  agriculture=mean(agriculture),
  education=mean(education),
  served=mean(servedduringwar_husband),
  injured=mean(injuredinwar_husband),
  returnedkarelia = 1L,
  never_married = mean(never_married),
  outbred = mean(outbred2)) %>%
  as.data.frame()
detach(p2)

link_2 <- link(mod_2, data=lottas_2)
# get means
mu <- apply(link_2,2,mean)
#get PI's'
pi <- t(apply(link_2,2, PI))


# new changes
##################################
# change data to long form with age and year in seperate columns