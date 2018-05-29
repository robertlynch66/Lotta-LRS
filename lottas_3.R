path <- "C:/Users/rofrly/Dropbox/Working papers/R data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
library(dplyr)
library(tidyr)
# convert booleans to numeric
p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
# birth year must be earlier than 1920 for Katiha table and select females
p <- p %>% filter (sex==0)
p$age_in_44 <- 1944-p$birthyear

# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values -deletes
# 281 women from full data
p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))
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


data <- full_data %>% select ("per_capita_suicides","population","sanders",
                              "clinton","Trump","Clinton")
# tidy data - gather up the columns
data <- gather(data, key = Candidate, value = percent,
               sanders, clinton, Trump, Clinton)

# possible DV'S kids, age at first birth and birth intervals