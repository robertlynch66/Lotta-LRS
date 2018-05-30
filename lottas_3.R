path <- "C:/Users/rofrly/Dropbox/Working papers/R data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
library(dplyr)
library(tidyr)
# convert booleans to numeric
p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)

# deleted retarded rows
# p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))


p <- p %>% filter (sex==0)
p$age_in_44 <- 1944-p$birthyear

# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values -deletes
# 281 women from full data

p <- p %>% select ("id","lotta","age_in_44","birthyear","agriculture","education","martta","never_married")


# load children data
children <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas_2/children.rds")
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)


# link p to children and add column for each kids birth year
# Year, Reproduced, Age


# possible DV'S kids, age at first birth and birth intervals
#read in the data, as you normally would



#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# you can play around with the ages but now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 
#p<- p%>% filter (birthyear<1931)
#p$year_age_13 <- as.numeric(p$birthyear+13)
#p$year_age_45 <- as.numeric(p$birthyear+45)
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

# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
no_twins <- twins[!duplicated(twins[,c("id","year")]),]


# so no_twins is no multiple births in same year, and twins includes them