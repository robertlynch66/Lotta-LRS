path <- "C:/Users/rofrly/Dropbox/Working papers/R data files/"
#path<-"/home/robert/Dropbox/Working papers/R data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
library(dplyr)
library(tidyr)
library(rethinking)
library(lme4)
library(plyr)
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
p <- p %>% filter (first_child_yob>1944 & age_1945>12 )#35110
# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values -deletes
# 281 women from full data

p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945")
p <- p[complete.cases(p), ]

p$age <- p$age_1945-min(p$age_1945) 
p$age_sq <- p$age*p$age
### try using age at first birth as DV
model <-glm(age_at_first_birth ~ lotta*age + lotta*age_sq+ education + agriculture,
              data = p, family = poisson)
# compare AIC scores between models dropping a variable
m1 <- drop1(model)

summary(model)
# load children data
children <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas_2/children.rds")
children <- readRDS("/home/robert/Dropbox/Github/Lottas_2/children.rds")
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
twins <- twins %>% select ("id","lotta","education","agriculture","year",
                           "reproduced","age","age_1945","age_at_first_birth")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
no_twins <- twins[!duplicated(twins[,c("id","year")]),]

## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
firstyearreproduced <- no_twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age-age_1945)

# now get only the time to the first repro event                                  
fyrepproduced2 <- firstyearreproduced  %>% group_by (id) %>%
   slice(which.min(time_to_repro))

model <-glm(time_to_repro ~ lotta*age_1945+ education + agriculture,  
            data = fyrepproduced2, family = poisson) 
summary(model)
# so no_twins is no multiple births in same year, and twins includes them
no_twins$age2 <- no_twins$age-13
no_twins$age_sq <- no_twins$age2*no_twins$age2
# run time series analysis in glmer
model <-glm(time_to_repro ~ lotta+  education + agriculture,  
                data = firstyearreproduced, family = poisson  , 
        control = glmerControl(optimizer="nloptwrap", optCtrl=list(maxfun=100000)))
# compare AIC scores between models dropping a variable
m1 <- drop1(model)

summary(model)

###############RUN in  rethinking

p<- no_twins
#renumber year to fit as random effect
p <- p %>% arrange(year)
p$year_seq <- cumsum(c(1,as.numeric(diff(p$year))!=0))
#map2stan formula

# run in rethinking
data_list <- list(
  lotta  = p$lotta,
  age = p$age2,
  agriculture = p$agriculture,
  education = p$education,
  reproduced= p$reproduced)

model <- map2stan(
  alist(
    reproduced ~ dbinom( 1 , p ),
    logit(p) <- Intercept +
      b_lotta*lotta +
      b_age*age +
      b_age_sq*age^2 +
      b_education*education +
      b_agriculture*agriculture +
      b_lotta_X_age*lotta_X_age +
      b_lotta_X_age_sq*age^2 +
      v_Intercept[year],
    Intercept ~ dnorm(0,10),
    b_lotta ~ dnorm(0,10),
    b_age ~ dnorm(0,10),
    b_education ~ dnorm(0,10),
    b_agriculture ~ dnorm(0,10),
    b_martta ~ dnorm(0,10),
    b_lotta_X_age ~ dnorm(0,10),
    v_Intercept[year] ~ dnorm(0,sigma_year),
    sigma_year ~ dcauchy(0,2)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(b_lotta=0,b_age=0, b_education=0,b_agriculture=0,b_martta=0, b_lotta_X_age=0),
  chains =4, cores=4)

path<- (paste0("results/"))
filename <- "lottas_LRS_age_and_age_sq.rds"

saveRDS(model, paste0(path, filename))



#### some basic graphs
#### get the means
#### of the reproduction across ages and age squared
d <- no_twins %>% filter (lotta==1) %>% mean (reproduced)

attach(no_twins)
aggdata <-aggregate(no_twins, by=list(lotta,age), 
                    FUN=mean, na.rm=TRUE)
print(no_twins)
detach(no_twins)

y <- no_twins %>% filter (lotta==0)
z <- no_twins %>% filter (lotta==1)

m <- aggregate(reproduced ~  age, data = y, mean)
colnames(m)[2] <- "not_lotta"
n <- aggregate(reproduced ~  age, data = z, mean)
colnames(n)[2] <- "lotta"
colnames(n)[1] <- "age2"
new <- cbind(m,n)
#temporarily disbale scientific notation
options("scipen"=0, "digits"=7)
new$diff <- 10*(new$lotta- new$not_lotta)
#new$diff <- sprintf(as.character(new$diff))
new <- new %>% select("age","diff")

ggplot(new, aes(age)) + 
  geom_line(aes(y = lotta, colour = "lotta")) + 
  geom_line(aes(y = not_lotta, colour = "not_lotta"))

p45_to_50 <- no_twins %>% filter (year>1944 & year <1951)

p <- aggregate(reproduced ~   lotta + year +age
                 , data = p45_to_50, FUN = function(x) c(mean = mean(x), n = length(x) ) )

aggregate (reproduced ~  lotta +age, data=p, FUN = function(x) c(mean = mean(x) ) )


## try to adjust age for year
p15 <- no_twins %>% filter (age_1945==15)
p16 <- no_twins %>% filter (age_1945==16)
p17 <- no_twins %>% filter (age_1945==17)
p18 <- no_twins %>% filter (age_1945==18)
p19 <- no_twins %>% filter (age_1945==19)
p20 <- no_twins %>% filter (age_1945==20)
p25 <- no_twins %>% filter (age_1945 >20 & age_1945 <25)
p30 <- no_twins %>% filter (age_1945 >24 & age_1945 <30)
p35 <- no_twins %>% filter (age_1945 >29 & age_1945 <35)
p40 <- no_twins %>% filter (age_1945 >34 & age_1945 <40)
p45 <- no_twins %>% filter (age_1945 > 39 & age_1945 <45)

# get the mean reproduction for all lottas vs non lottas across years of their lives
# for lottas who were 20,25,30,35 and 40 in 1945
# 15 year olds
pd <- position_dodge(0.3)

library(plyr)

# Run the functions length, mean, and sd on the value of "reproduced" for each group, 
# broken down by lotta + age
m15 <- ddply(p15, c("lotta", "age"), summarise,
               N    = length(reproduced),
               mean = mean(reproduced),
               sd   = sd(reproduced),
               se   = sd / sqrt(N)
)
# make lotta a factor
m15$lotta <- as.factor(m15$lotta)
# make a point plot with colors and 
plot15 <- ggplot(data = m15, aes(x=age, y=mean, colour=lotta, group=lotta)) +
  geom_point(position=pd, size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd)+
  scale_color_manual(values=c("darkblue", "darkgreen")) + 
  labs(x = "Age gave birth", y="mean reproduction", title="15 years old in 1945")
plot15


# 16
m16 <- ddply(p16, c("lotta", "age"), summarise,
             N    = length(reproduced),
             mean = mean(reproduced),
             sd   = sd(reproduced),
             se   = sd / sqrt(N)
)
# make lotta a factor
m16$lotta <- as.factor(m16$lotta)
# make a point plot with colors and 
plot16 <- ggplot(data = m16, aes(x=age, y=mean, colour=lotta, group=lotta)) +
  geom_point(position=pd, size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd)+
  scale_color_manual(values=c("darkblue", "darkgreen")) + 
  labs(x = "Age gave birth", y="mean reproduction", title="16 years old in 1945")
plot16
# 17
m17 <- ddply(p17, c("lotta", "age"), summarise,
             N    = length(reproduced),
             mean = mean(reproduced),
             sd   = sd(reproduced),
             se   = sd / sqrt(N)
)
# make lotta a factor
m17$lotta <- as.factor(m17$lotta)
# make a point plot with colors and 
plot17 <- ggplot(data = m17, aes(x=age, y=mean, colour=lotta, group=lotta)) +
  geom_point(position=pd, size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd)+
  scale_color_manual(values=c("darkblue", "darkgreen")) + 
  labs(x = "Age gave birth", y="mean reproduction", title="17 years old in 1945")
plot17
# 18
m18 <- ddply(p18, c("lotta", "age"), summarise,
             N    = length(reproduced),
             mean = mean(reproduced),
             sd   = sd(reproduced),
             se   = sd / sqrt(N)
)
# make lotta a factor
m18$lotta <- as.factor(m18$lotta)
# make a point plot with colors and 
plot18 <- ggplot(data = m18, aes(x=age, y=mean, colour=lotta, group=lotta)) +
  geom_point(position=pd, size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd)+
  scale_color_manual(values=c("darkblue", "darkgreen")) + 
  labs(x = "Age gave birth", y="mean reproduction", title="18 years old in 1945")
plot18
# 19
m19 <- ddply(p19, c("lotta", "age"), summarise,
             N    = length(reproduced),
             mean = mean(reproduced),
             sd   = sd(reproduced),
             se   = sd / sqrt(N)
)
# make lotta a factor
m19$lotta <- as.factor(m19$lotta)
# make a point plot with colors and 
plot19 <- ggplot(data = m19, aes(x=age, y=mean, colour=lotta, group=lotta)) +
  geom_point(position=pd, size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd)+
  scale_color_manual(values=c("darkblue", "darkgreen")) + 
  labs(x = "Age gave birth", y="mean reproduction", title="19 years old in 1945")
plot19
# 20 year olds first
m20 <- ddply(p20, c("lotta", "age"), summarise,
             N    = length(reproduced),
             mean = mean(reproduced),
             sd   = sd(reproduced),
             se   = sd / sqrt(N)
)
# make lotta a factor
m20$lotta <- as.factor(m20$lotta)
# make a point plot with colors and 
plot20 <- ggplot(data = m19, aes(x=age, y=mean, colour=lotta, group=lotta)) +
  geom_point(position=pd, size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd)+
  scale_color_manual(values=c("darkblue", "darkgreen")) + 
  labs(x = "Age gave birth", y="mean reproduction", title="20 years old in 1945")
plot20
# 25 year olds
m25 <- ddply(p25, c("lotta", "age"), summarise,
             N    = length(reproduced),
             mean = mean(reproduced),
             sd   = sd(reproduced),
             se   = sd / sqrt(N)
)
# make lotta a factor
m25$lotta <- as.factor(m25$lotta)
# make a point plot with colors and 
plot25 <- ggplot(data = m25, aes(x=age, y=mean, colour=lotta, group=lotta)) +
  geom_point(position=pd, size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd)+
  scale_color_manual(values=c("darkblue", "darkgreen")) + 
  labs(x = "Age gave birth", y="mean reproduction", title="21-25 years old in 1945")
plot25
# 30 year olds
m30 <- ddply(p30, c("lotta", "age"), summarise,
             N    = length(reproduced),
             mean = mean(reproduced),
             sd   = sd(reproduced),
             se   = sd / sqrt(N)
)
# make lotta a factor
m30$lotta <- as.factor(m30$lotta)
# make a point plot with colors and 
plot30 <- ggplot(data = m30, aes(x=age, y=mean, colour=lotta, group=lotta)) +
  geom_point(position=pd, size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd)+
  scale_color_manual(values=c("darkblue", "darkgreen")) + 
  labs(x = "Age gave birth", y="mean reproduction", title="26-30 years old in 1945")
plot30
# 35 year olds
m35 <- ddply(p35, c("lotta", "age"), summarise,
             N    = length(reproduced),
             mean = mean(reproduced),
             sd   = sd(reproduced),
             se   = sd / sqrt(N)
)
# make lotta a factor
m35$lotta <- as.factor(m35$lotta)
# make a point plot with colors and 
plot35 <- ggplot(data = m35, aes(x=age, y=mean, colour=lotta, group=lotta)) +
  geom_point(position=pd, size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd)+
  scale_color_manual(values=c("darkblue", "darkgreen")) + 
  labs(x = "Age gave birth", y="mean reproduction", title="31-35 years old in 1945")
plot35
# 40 year olds
m40 <- ddply(p40, c("lotta", "age"), summarise,
             N    = length(reproduced),
             mean = mean(reproduced),
             sd   = sd(reproduced),
             se   = sd / sqrt(N)
)
# make lotta a factor
m40$lotta <- as.factor(m40$lotta)
# make a point plot with colors and 
plot40 <- ggplot(data = m40, aes(x=age, y=mean, colour=lotta, group=lotta)) +
  geom_point(position=pd, size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd)+
  scale_color_manual(values=c("darkblue", "darkgreen")) + 
  labs(x = "Age gave birth", y="mean reproduction", title="36-40 years old in 1945")
plot40
# 45 year olds
m45 <- ddply(p45, c("lotta", "age"), summarise,
             N    = length(reproduced),
             mean = mean(reproduced),
             sd   = sd(reproduced),
             se   = sd / sqrt(N)
)
# make lotta a factor
m45$lotta <- as.factor(m45$lotta)
# make a point plot with colors and 
plot45 <- ggplot(data = m45, aes(x=age, y=mean, colour=lotta, group=lotta)) +
  geom_point(position=pd, size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd)+
  scale_color_manual(values=c("darkblue", "darkgreen")) + 
  labs(x = "Age gave birth", y="mean reproduction", title="Over 40 years old in 1945")
plot45

## plotting age at first birth
pd <- position_dodge(0.3)
p<- p %>% filter (age_1945>13 &age_1945<40)
aafb<- ddply(p, c("lotta", "age_1945"), summarise,
             N    = length(age_at_first_birth),
             mean = mean(age_at_first_birth),
             sd   = sd(age_at_first_birth),
             se   = sd / sqrt(N)
)
# make lotta a factor
aafb$lotta <- as.factor(aafb$lotta)
# make a point plot with colors and 
plot_aafb <- ggplot(data = aafb, aes(x=age_1945, y=mean, colour=lotta, group=lotta)) +
  geom_point(position=pd, size=1) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd)+
  scale_color_manual(values=c("darkblue", "darkgreen")) + 
  labs(x = "Age in 1945", y="Mean age at first birth", title="Age at first birth across ages")
plot_aafb



# try to add a predicted quadratic line to the above plot
model <-glm(age_at_first_birth ~ lotta*age + lotta*age_sq+ education + agriculture,
            data = p, family = poisson)

agevalues <- seq(0, 25, 1)
predictedcounts <- predict(model,list(age=agevalues, age_sq=agevalues^2, lotta=1,
                                      education=0,agriculture=0))

plot(Time, Counts, pch=16, xlab = "Time (s)", ylab = "Counts", cex.lab = 1.3, col = "blue")

lines(timevalues, predictedcounts, col = "darkgreen", lwd = 3)
pl <- p %>% filter (lotta==1)
pn <- p %>% filter (lotta==0)