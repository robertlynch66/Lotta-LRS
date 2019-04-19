# load libraries
library(dplyr)
library(rethinking)
library(tidyr)

# path to the folder with the R data files
path <- "../data files/"
# read in person table
file<- "data.rds"
data <- readRDS(paste0(path, file))
# remove non -reproductives from dataframe
data <- data %>% filter(reproduced==1)

#map2stan formula
#  MODEL 1 time to reproduction after the war
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
filename <- "Time_to_repro.rds"

saveRDS(model, paste0(path, filename))
##################################################################



#####################################################################
### Model 2 Mean IBI's after the war
# load libraries
library(dplyr)
library(rethinking)
library(tidyr)

# path to the folder with the R data files
path <- "../data files/"
# read in person table
file<- "data.rds"
data <- readRDS(paste0(path, file))

data <- data%>% arrange(birthplaceid)
data$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(data$birthplaceid))!=0))

data_list <- list(
  pwrr = data$post_war_repro_rate,
  lotta  = data$lotta,
  birth_cat = data$birth_cat,
  repro_cat = data$repro_within_2_years,
  age = data$age_1945,
  agriculture = data$agriculture,
  education = data$education)


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
##########################################################################


#############################################################################

### Model 3 Total reproduction after war
# load libraries
library(dplyr)
library(rethinking)
library(tidyr)

# path to the folder with the R data files
path <- "../data files/"
# read in person table with non reproductives
file<- "data2.rds"
data <- readRDS(paste0(path, file))

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
