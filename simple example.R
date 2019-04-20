# simple simulation example

library(dplyr)
library(rethinking)
library(tidyr)


# path to the folder with the R data files

# read in the data used to create the model map2 stan object and the model
# PUT in correct path to the sample data frame
path <- "../data files/"
#name the data frame - 'data_sample' is a random smaple of 200 rows to demonstrate code
file<- "data_sample.rds"
data <- readRDS(paste0(path, file))
# load children data

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
  data=data_list, iter=200, warmup=100, control=list(max_treedepth=20),
  chains =4, cores=4,start=list(Intercept=mean(data$kids_after_war),b_age=0,
                                b_birth_cat=0,b_education=0,
                                b_agriculture=0,b_repro_within_2_years=0,
                                b_lotta_X_age=0))


# get parametre estimates
precis(model)
## save results
path<- (paste0("results/"))
filename <- "Kids_after_war.rds"

saveRDS(model, paste0(path, filename))