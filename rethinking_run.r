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
children <- readRDS("~/r_files/children.rds")

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

children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

# link p to children and add column for each kids birth year

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
no_twins$age2 <- no_twins$age-13
no_twins$age_sq <- no_twins$age2*no_twins$age2

###############RUN in  rethinking##############################################
p<- no_twins
p<- p[complete.cases(p),]

# dump unnecessary data frames
rm(no_twins, children, p_long, twins)

print(nrow(p))
#renumber year to fit as random effect
p <- p %>% arrange(year)
p$year_seq <- cumsum(c(1,as.numeric(diff(p$year))!=0))
#map2stan formula

# run in rethinking
data_list <- list(
  lotta  = p$lotta,
  age = p$age2,
  age_sq = p$age_sq,
  agriculture = p$agriculture,
  education = p$education,
  reproduced= p$reproduced,
  year_seq=p$year_seq)

model <- map2stan(
  alist(
    reproduced ~ dbinom( 1 , p ),
    logit(p) <- Intercept +
      b_lotta*lotta +
      b_age*age +
      b_age_sq*age_sq +
      b_education*education +
      b_agriculture*agriculture +
      b_lotta_X_age*lotta*age +
      b_lotta_X_age_sq*lotta*age_sq +
      v_Intercept[year_seq],
    Intercept ~ dnorm(0,10),
    b_lotta ~ dnorm(0,10),
    b_age ~ dnorm(0,10),
    b_age_sq ~ dnorm(0,10),
    b_education ~ dnorm(0,10),
    b_agriculture ~ dnorm(0,10),
    b_lotta_X_age ~ dnorm(0,10),
    b_lotta_X_age_sq ~ dnorm(0,10),
    v_Intercept[year_seq] ~ dnorm(0,sigma_year),
    sigma_year ~ dcauchy(0,2)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(b_lotta=0,b_age=0, b_education=0,b_agriculture=0,b_martta=0, b_lotta_X_age=0,
             b_lotta_X_age_sq=0),
  chains =4, cores=4)

path<- (paste0("results/"))
filename <- "lottas_LRS_age_and_age_sq.rds"

saveRDS(model, paste0(path, filename))
