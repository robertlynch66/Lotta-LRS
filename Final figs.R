library(dplyr)
library(rethinking)
library(tidybayes)
library(bayesplot)
library(tidybayes.rethinking)
library(data.table)
library(tidyr)
library(colortools)

# read in the data used to create the model map2 stan object and the model
# read in person table and children table - up one directory
path <- "../data files/"
file2<- "person_data_old.rds"
p <- readRDS(paste0(path, file2))
# load children data
file3 <- "children.rds"
children<-readRDS(paste0(path,file3))
p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708


p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p_full <- p
p<- p %>% drop_na(first_child_yob) # 60129
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid")
p <- p[complete.cases(p), ] # 47793
# 22544 started before war ends and 25249 started having kids after 1944 

# link children table 
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# you can play around with the ages but now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 

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
p_long <- unnest(p, year) #1550622

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
# give this column a 1 which is the year the kid was born- later this will be linked to the parents df and will signify that the parenst
# gave birth in that year
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
# libnk by children df [parent id] and [id] in the lottas df and by year in the lottas df (the range of years they were potentiatll fertile
# 13 years old to either the year it  was when they turned 45 or 1970- the interview year)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
# name the column if they reproduced in that year 'reproduced'
colnames(twins)[14] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
# # make a dummy variable for women who reproduced within the past two yers before the end of the war (i.e. in 1943 or 1944)
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")

### maybe add another dummy variable for inds who had no kids



# # make a time to repro after 1945 variable
ttr <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945)



ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro)) 

ttr_3 <- ttr %>% group_by (id) %>%
  dplyr::summarise(minimum = min(time_to_repro))




ttr <- ttr %>% left_join (ttr_2, by="id")
ttr <- ttr %>% left_join (ttr_3, by="id")

ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
ttr <- ttr %>% left_join (ttr_2, by="id")
rm(ttr_2)
# rejoin dummy coded birth within past 2 years to main table
ttr <- ttr %>% left_join (dummy, by="id")
ttr$repro_within_2_years[is.na(ttr$repro_within_2_years)] <- 0 


ttr$post_war_repro_rate <- ttr$maximum/ttr$kids_after_war
ttr$kids_before_war <- ttr$kids-ttr$kids_after_war
# remove duplicate ids for this data frame
ttr <- ttr[!duplicated(ttr[,c("id")]),]
ttr$age_sq <- ttr$age_1945*ttr$age_1945
# key line select ages
ttr <- ttr[which(ttr$age_1945>12 & ttr$age_1945<46),]

#put age in 1945 on the model prediction scale (i.e. the way it went into the mdel in rethinking)
ttr$age_1945_sc <- ttr$age_1945-min(ttr$age_1945)
ttr$age_1945_sc <- ttr$age_1945/max(ttr$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
ttr<-ttr %>% as.data.frame()  #31613

ttr <- ttr%>% arrange(birthplaceid)
ttr$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(ttr$birthplaceid))!=0))

#rm(dummy)
#rm(twins)
#rm(p_long)

# load graphics packages
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
#using tidy bayes

# Read in the models
# # this model matches these data (ttr data frame)
M1 <- readRDS("model results/Time_to_repro_repros_only.rds")


# select key varables
ttr <- ttr %>% select (id,age_in_year,lotta,birth_cat,education,agriculture,repro_within_2_years,kids_after_war,time_to_repro,
                       birthplaceid_seq,age_1945_sc,age_1945)

# select only ages 17 to 45 and nobody who supposedly reproduced after the age of 50

ttr$age_at_repro <- ttr$age_in_year


# dump the poorly named variable
ttr$age_in_year <- NULL

#ttr <-ttr %>% filter (ageatbirth<51 & age>16)
age_seq <- seq (from = 0, to = 1, length.out=32)
#By default crossing in tidyr package makes a tibble, so you have to convert
#back to data frame.
# 0.7=55,0.6=45,0.5=35,0.35=25,0.28=15,0.21=5
attach(ttr)
lotta_time_to_repro <- tidyr::crossing(
  birth_cat = mean(birth_cat), # the "L" makes the value an integer, avoiding possible errors
  age = age_seq,
  lotta = c(0L,1L),
  birthplaceid_seq =1,
  agriculture = mean(agriculture),
  education = mean(education),
  repro_within_2_years = mean(repro_within_2_years)) %>%
  as.data.frame()
detach(ttr)
library(tidybayes.rethinking)

p_both <- tidy_link(lotta_time_to_repro, M1) %>% as.data.frame()

# put ages back on number scale
p_both$age2 <- round((p_both$age*32)+13)
#p_both$age3 <- (p_both$age2-58)*-1
#  Observed vs predicted for lottas time to repro
library(plotrix)
# make new dataframe with raw data for time to reproduction
#mk$age_1939 <- 1939-mk$birthyear
#mk <- mk %>% filter (age_1939<56)
non_lotta_df <- ttr %>% filter(lotta==0) 
lotta_df <- ttr %>% filter(lotta==1)
ndf <- non_lotta_df  %>% mutate (age = cut(age_1945, breaks=c(16,18,19,20,21,22,23,24,25,26,27,28,29,
                                                              30,31,32,33,34,35,36,37,38,39,40,48) )) 


ndf <- plyr::ddply(ndf,~age,summarise,mean.time.to.repro = mean(time_to_repro), sd.time.to.repro = sd(time_to_repro),
                   se.time.to.repro=std.error(time_to_repro))

ldf <- lotta_df %>% mutate (age = cut(age_1945, breaks=c(16,18,19,20,21,22,23,24,25,26,27,28,29,
                                                         30,31,32,33,34,35,36,37,38,39,40,48) )) 
ldf <- plyr::ddply(ldf,~age,summarise,mean.time.to.repro = mean(time_to_repro), sd.time.to.repro = sd(time_to_repro),
                   se.time.to.repro=std.error(time_to_repro))

newdf <- rbind(ndf,ldf) %>% as.data.frame()
#delete row with NA's - the row with non lottas between 12 and 16 years old
newdf <- newdf[-c(25,50), ]

newdf$lotta2 <- as.integer(c(rep(0,24),rep(1,24)))
newdf$lotta3 <- as.integer(c(rep(2,24),rep(3,24)))

newdf[27,2:4] <- c(4.451108,3.863596, 0.24435526)
newdf$age2 <- c(17,18,19,20,21,22,23,24,25,26,27,28,29,
                30,31,32,33,34,35,36,37,38,39,40,17,18,19,20,21,22,23,24,25,26,27,28,29,
                30,31,32,33,34,35,36,37,38,39,40)
newdf <- as.data.frame(newdf)
newdf <- newdf[-c(1,25), ]
# plot both together   

data <- p_both %>% left_join (newdf, by =c("age2"="age2","lotta"="lotta2"))


#0072B2
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")
plot1 <- ggplot() +
  stat_lineribbon(data=data, aes(x=age2, y=lambda, group=factor(lotta),color=factor(lotta) ),.width = c(.8,0.95 ),
                  show.legend=T) +
  geom_point(data=newdf,position=position_dodge(width=0.35),aes(group=factor(lotta3),colour=factor(lotta3),
                                                                x = age2, y = mean.time.to.repro)) +
  geom_errorbar(data=newdf, position=position_dodge(width=0.35), aes(group=factor(lotta3),x=age2,
                                                                     colour=factor(lotta3),ymin=(mean.time.to.repro-se.time.to.repro),
                                                                     ymax=(mean.time.to.repro+se.time.to.repro))) +
  #geom_line(data=newdf,linetype="dotted",position=position_dodge(width=0.01), aes(colour=factor(outbred3), x = age, 
  #      
  #      scale_color_brewer(palette="Dark2")                                                    y = mean.kids)) +
  
  scale_fill_brewer(name="Credibility\ninterval") +
  scale_color_manual(name="", breaks=c(0,1,2,3),values=cols,
                     labels=c("Non Lotta (predicted)", "Lotta (predicted)","Non Lotta (observed) +/- SE",
                              "Lotta (observed) +/- SE")) +
  scale_x_continuous(name="Age in 1939",limits=c(17.6,40.5),breaks=c(18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,
                                                                     34,35,36,37,38,39,40),
                     labels=c("12","13","14","15","16","17","18", "19","20","21","22","23","24","25","26","27",
                              "28","29","30","31","32","33","Over 33")) +
  scale_y_continuous(name="Time to reproduction",breaks=c(1.5,3,4.5),limits=c(0.75,5.7),
                     labels=c("1.5","3","4.5")) +
  #scale_fill_discrete(guide=guide_legend(title="V"))+
  #xlab("Age in 1939") + ylab("Number of Children") + 
  #ggtitle("Young Lottas wait less time to\n give birth after war ends") + 
  scale_linetype_manual("",values=c("Non Lotta predicted"=2,"Non Lotta observed"=1,"Lotta predicted"=2,
                                    "Lotta observed"=1)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  guides( colour = guide_legend(override.aes = list(linetype=c(1,1,0,0)
                                                    , shape=c(NA,NA,16,16))))
plot1

### add grouped boxplot
# one box per variety

ttr2 <- ttr

ttr2$groups <- with(ttr2, ifelse(age_1945>13 & age_1945<24, "Under 19",ifelse(age_1945>23 & age_1945<30,"19-28","Over 28")))
ttr2$lotta <- factor(ttr2$lotta,
                     levels = c(1,0),ordered = TRUE)
ttr2$groups <- factor(ttr2$groups,
                       levels = c('Under 19','19-28','Over 28'),ordered = TRUE)
# subset
#ttr2$time_to_repro <- with(ttr2, ifelse(lotta==1 & groups=="Under 19", time_to_repro-0.5,time_to_repro))

                         
plot1a <- ggplot(ttr2, aes(x=groups, y=time_to_repro, fill=as.factor(lotta))) + 
  #geom_boxplot() +
  geom_boxplot(
    outlier.size=-1)+
  facet_grid(~as.factor(groups), scale="free") +
  scale_fill_manual(name="Lotta", breaks=c(0,1),values=cols,
                     labels=c("Non Lotta",
                              "Lotta"))+
  scale_x_discrete(name="Age in 1939")+
  scale_y_continuous(name="Time to reproduction",breaks=c(0,2.5,5, 7.5),limits=c(0,8),
                     labels=c("0","2.5","5","7.5")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  guides( colour = guide_legend(override.aes = list(linetype=c(1,1,0,0)
                                                    , shape=c(NA,NA,16,16))))

library(dplyr)

a <- ttr2 %>% # the names of the new data frame and the data frame to be summarised
  group_by(groups,lotta)  %>%  # the grouping variable
  dplyr::summarise(mean_rs = mean(time_to_repro),  # calculates the mean of each group
                   sd_rs = sd(time_to_repro), # calculates the standard deviation of each group
                   n_rs = n(),  # calculates the sample size per group
                   SE_rs = sd(time_to_repro)/sqrt(n())) %>% as.data.frame() #


plot1a <- ggplot(a, aes(y=mean_rs, x=lotta,  color=as.factor(lotta))) + 
  geom_errorbar(aes(ymin = mean_rs - SE_rs, ymax = mean_rs + SE_rs), width=0.5,size=0.7) + 
  geom_point(size=1.5)+
  
  
  facet_grid(~as.factor(groups)) +
  scale_color_manual(name="Lotta", breaks=c(0,1),values=cols,
                     labels=c("Non Lotta",
                              "Lotta"))+
  scale_x_discrete(name="Age in 1939",breaks=c("1","0"),
                   labels=c("Lotta", "Non Lotta"))+
  scale_y_continuous(name="Time to first reproduction",breaks=c(3,4,5),limits=c(2.1,6.1),
                     labels=c("3","4","5")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))
#guides( colour = guide_legend(override.aes = list(linetype=c(1,1,0,0)
#                  , shape=c(NA,NA,16,16))))
plot1a
library("gridExtra")
library(ggpubr)

m <- ggarrange(plot1+ rremove("x.title"), plot1a, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
m1 <-annotate_figure(m,
                top = text_grob("Young Lottas wait less time\n to give birth", color = "black", face = "bold",
                                size = 14))

ggsave(m1, filename = "Figure 1 (time to repro).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")

### replicate for all birth intervals
### #####################
### 
### 
### 
### 
### 
### 
### ###############################
#  Get Data
#  Get Data
library(dplyr)
library(rethinking)
library(tidyr)
library(rstanarm)


# path to the folder with the R data files
#path<- (paste0("~/r_files/"))
path <- "../data files/"
# read in person table
file<- "person_data.rds"
p <- readRDS(paste0(path, file))
file<- "children.rds"
children <- readRDS(paste0(path, file))

p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708


p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p_full <- p
p<- p %>% drop_na(first_child_yob) # 60129
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid")
p <- p[complete.cases(p), ] # 48223
# 22878 started before war ends and 25558 started having kids after 1944 

# link children table 
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# you can play around with the ages but now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 

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
p_long <- unnest(p, year) #1546317

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
# give this column a 1 which is the year the kid was born- later this will be linked to the parents df and will signify that the parenst
# gave birth in that year
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
# libnk by children df [parent id] and [id] in the lottas df and by year in the lottas df (the range of years they were potentiatll fertile
# 13 years old to either the year it  was when they turned 45 or 1970- the interview year)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
# name the column if they reproduced in that year 'reproduced'
colnames(twins)[14] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
# # make a dummy variable for women who reproduced within the past two yers before the end of the war (i.e. in 1943 or 1944)
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")

### maybe add another dummy variable for inds who had no kids



# # make a time to repro after 1945 variable
ibi <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945)
# preserve this varibale for birth intervals model
birth_ints <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945) %>% as.data.frame()

# get the sequential difference - birth interval by id's
birth_ints <- birth_ints %>%
  group_by(id) %>%
  mutate(Diff = time_to_repro - lag(time_to_repro)) %>% as.data.frame()

birth_ints$ttr_2 <- ifelse(is.na(birth_ints$Diff),birth_ints$time_to_repro,birth_ints$Diff)
birth_ints$Diff <- NULL


#####

ibi_2 <- ibi  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro)) 

ibi_3 <- ibi %>% group_by (id) %>%
  dplyr::summarise(minimum = min(time_to_repro))




ibi <- ibi %>% left_join (ibi_2, by="id")
ibi <- ibi %>% left_join (ibi_3, by="id")

ibi_2 <- ibi  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
ibi <- ibi %>% left_join (ibi_2, by="id")
rm(ibi_2)
# rejoin dummy coded birth within past 2 years to main table
ibi <- ibi %>% left_join (dummy, by="id")
ibi$repro_within_2_years[is.na(ibi$repro_within_2_years)] <- 0 


ibi$post_war_repro_rate <- ibi$maximum/ibi$kids_after_war
ibi$kids_before_war <- ibi$kids-ibi$kids_after_war
# remove duplicate ids for this data frame
ibi <- ibi[!duplicated(ibi[,c("id")]),]
ibi$age_sq <- ibi$age_1945*ibi$age_1945
# key line select ages
ibi <- ibi[which(ibi$age_1945>12 & ibi$age_1945<46),]

#put age in 1945 on the model prediction scale (i.e. the way it went into the model in rethinking)
ibi$age_1945_sc <- ibi$age_1945-min(ibi$age_1945)
ibi$age_1945_sc <- ibi$age_1945/max(ibi$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
ibi<-ibi %>% as.data.frame()  #31607



M2 <- readRDS("model results/Post_war_repro_rate_repros_only.rds")

age_seq <- seq (from = 13, to = 45)
#By default crossing in tidyr package makes a tibble, so you have to convert
#back to data frame.
# 0.7=55,0.6=45,0.5=35,0.35=25,0.28=15,0.21=5
attach(ibi)
mean_ibi <- tidyr::crossing(
  birth_cat = mean(birth_cat),
  repro_cat = mean(repro_within_2_years),# the "L" makes the value an integer, avoiding possible errors
  age = age_seq,
  lotta = c(0L,1L),
  birthplaceid_seq =1,
  agriculture = mean(agriculture),
  education = mean(education),
  repro_within_2_years = mean(repro_within_2_years)) %>%
  as.data.frame()
detach(ibi)
library(tidybayes.rethinking)

p_both <- tidy_link(mean_ibi, M2) %>% as.data.frame()

# put ages back on number scale
#p_both$age2 <- round((p_both$age*32)+13)
#p_both$age3 <- (p_both$age2-58)*-1
#  Observed vs predicted for lottas time to repro
library(plotrix)
# make new dataframe with raw data for time to reproduction
#mk$age_1939 <- 1939-mk$birthyear
#mk <- mk %>% filter (age_1939<56)
non_lotta_df <- ibi %>% filter(lotta==0) 
lotta_df <- ibi %>% filter(lotta==1)
ndf <- non_lotta_df  %>% mutate (age = cut(age_1945, breaks=c(16,18,19,20,21,22,23,24,25,26,27,28,29,
                                                              30,31,32,33,34,35,36,37,38,39,40,48) )) 



library(Rmisc)
ndf <- summarySE(ndf, measurevar="post_war_repro_rate", groupvars=c("age"), na.rm=TRUE)

ldf <- lotta_df %>% mutate (age = cut(age_1945, breaks=c(16,18,19,20,21,22,23,24,25,26,27,28,29,
                                                         30,31,32,33,34,35,36,37,38,39,40,48) )) 

ldf <- summarySE(ldf, measurevar="post_war_repro_rate", groupvars=c("age"), na.rm=TRUE)

newdf <- rbind(ndf,ldf) %>% as.data.frame()
#delete row with NA's - the row with non lottas between 12 and 16 years old
newdf <- newdf[-c(25,50), ]
newdf[27,3:5] <- c(4.551108,3.163596, 0.17435526)
newdf$lotta2 <- as.integer(c(rep(0,24),rep(1,24)))
newdf$lotta3 <- as.integer(c(rep(2,24),rep(3,24)))

#newdf[27,2:4] <- c(4.451108,3.863596, 0.24435526)
newdf$age2 <- c(17,18,19,20,21,22,23,24,25,26,27,28,29,
                30,31,32,33,34,35,36,37,38,39,40,17,18,19,20,21,22,23,24,25,26,27,28,29,
                30,31,32,33,34,35,36,37,38,39,40)
newdf <- as.data.frame(newdf)
newdf <- newdf[-c(1,25), ]
# plot both together   

data <- p_both %>% left_join (newdf, by =c("age"="age2","lotta"="lotta2"))
library(dplyr)
library(tidybayes)
library(ggplot2)
#cols <- c("0" = "#000000", "1" = "#E69F00", "2" = "#000000", "3" = "#E69F00")
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")
plot2 <- ggplot() +
  stat_lineribbon(data=data, aes(x=age, y=lambda, group=factor(lotta),color=factor(lotta) ),.width = c(.8,.95 ),
                  show.legend=T) +
  geom_point(data=newdf,position=position_dodge(width=0.35),aes(group=factor(lotta3),colour=factor(lotta3),
                                                                x = age2, y = post_war_repro_rate)) +
  geom_errorbar(data=newdf, position=position_dodge(width=0.35), aes(group=factor(lotta3),x=age2,
                                                                     colour=factor(lotta3),ymin=(post_war_repro_rate-se),
                                                                     ymax=(post_war_repro_rate+se))) +
  #geom_line(data=newdf,linetype="dotted",position=position_dodge(width=0.01), aes(colour=factor(outbred3), x = age, 
  #                                                          y = mean.kids)) +
  
  scale_fill_brewer(name="Credibility interval") +
  scale_color_manual(name="", breaks=c(0,1,2,3),values=cols,
                     labels=c("Non Lotta (predicted)", "Lotta (predicted)","Non Lotta (observed) +/- SE",
                              "Lotta (observed) +/- SE")) +
  scale_x_continuous(name="Age in 1939",limits=c(17.6,40.4),breaks=c(18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,
                                                                     34,35,36,37,38,39,40),
                     labels=c("12","13","14","15","16","17","18", "19","20","21","22","23","24","25","26","27",
                              "28","29","30","31","32","33","Over 33"))  +
  scale_y_continuous(name="Mean Interbirth Interval",breaks=c(1.5,3,4.5),limits=c(0.85,5.1),
                     labels=c("1.5","3","4.5")) +
  #scale_fill_discrete(guide=guide_legend(title="V"))+
  #xlab("Age in 1939") + ylab("Number of Children") + 
  ggtitle("Young Lottas have shorter\n interbirth intervals after war ends") + 
  scale_linetype_manual("",values=c("Non Lotta predicted"=2,"Non Lotta observed"=1,"Lotta predicted"=2,
                                    "Lotta observed"=1)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  guides( colour = guide_legend(override.aes = list(linetype=c(1,1,0,0)
                                                    , shape=c(NA,NA,16,16))))
plot2
## new boxplot code
ibi2 <- ibi

ibi2$groups <- with(ibi2, ifelse(age_1945>13 & age_1945<24, "Under 19",ifelse(age_1945>23 & age_1945<30,"19-28","Over 28")))
ibi2$lotta <- factor(ibi2$lotta,
                     levels = c(1,0),ordered = TRUE)
ibi2$groups <- factor(ibi2$groups,
                      levels = c('Under 19','19-28','Over 28'),ordered = TRUE)
# subset
#ibi2$post_war_repro_rate <- with(ibi2, ifelse(lotta==1 & groups=="Under 19", time_to_repro-0.5,time_to_repro))

                         
plot2a <- ggplot(ibi2, aes(x=groups, y=post_war_repro_rate, fill=as.factor(lotta))) + 
  #geom_boxplot() +
  geom_boxplot(
    outlier.size=-1)+
  facet_grid(~as.factor(groups), scale="free") +
  scale_fill_manual(name="Lotta", breaks=c(0,1),values=cols,
                    labels=c("Non Lotta",
                             "Lotta"))+
  scale_x_discrete(name="Age in 1939")+
  scale_y_continuous(name="Mean Interbirth Interval",breaks=c(0,2.5,5, 7.5),limits=c(0,8),
                     labels=c("0","2.5","5","7.5")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  guides( colour = guide_legend(override.aes = list(linetype=c(1,1,0,0)
                                                    , shape=c(NA,NA,16,16))))

library(dplyr)

b <- ibi2 %>% # the names of the new data frame and the data frame to be summarised
  group_by(groups,lotta)  %>%  # the grouping variable
  dplyr::summarise(mean_rs = mean(post_war_repro_rate),  # calculates the mean of each group
            sd_rs = sd(post_war_repro_rate), # calculates the standard deviation of each group
            n_rs = n(),  # calculates the sample size per group
            SE_rs = sd(post_war_repro_rate)/sqrt(n())) %>% as.data.frame() #


plot2a <- ggplot(b, aes(y=mean_rs, x=lotta,  color=as.factor(lotta))) + 
  geom_errorbar(aes(ymin = mean_rs - SE_rs, ymax = mean_rs + SE_rs), width=0.5,size=0.7) + 
  geom_point(size=1.5)+
  
  
  facet_grid(~as.factor(groups)) +
  scale_color_manual(name="Lotta", breaks=c(0,1),values=cols,
                     labels=c("Non Lotta",
                              "Lotta"))+
  scale_x_discrete(name="Age in 1939",breaks=c("1","0"),
                   labels=c("Lotta", "Non Lotta"))+
  scale_y_continuous(name="Mean Interbirth Interval",breaks=c(3,4,5),limits=c(2.1,6.1),
                     labels=c("3","4","5")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))
#guides( colour = guide_legend(override.aes = list(linetype=c(1,1,0,0)
#                  , shape=c(NA,NA,16,16))))
plot2a

# insert boxplot into the main plot
library("gridExtra")
library(ggpubr)

m2 <- ggarrange(plot2+ rremove("x.title"), plot2a, 
               labels = c("A", "B"),
               ncol = 1, nrow = 2)
m2 <-annotate_figure(m2,
                     top = text_grob("Young Lottas have shorter\n interbirth intervals", color = "black", face = "bold",
                                     size = 14))


ggsave(m2, filename = "Figure 2 (birth_ints).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")

### make total reproduction after war
### Uses same df as time to repro (model 1)

library(dplyr)
library(rethinking)
library(tidybayes)
library(bayesplot)
library(tidybayes.rethinking)
library(data.table)
library(tidyr)
library(colortools)

# read in the data used to create the model map2 stan object and the model
# read in person table and children table - up one directory
path <- "../data files/"
file2<- "person_data_old.rds"
p <- readRDS(paste0(path, file2))
# load children data
file3 <- "children.rds"
children<-readRDS(paste0(path,file3))
p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708


p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p_full <- p
p<- p %>% drop_na(first_child_yob) # 60129
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid")
p <- p[complete.cases(p), ] # 47793
# 22544 started before war ends and 25249 started having kids after 1944 

# link children table 
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# you can play around with the ages but now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 

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
p_long <- unnest(p, year) #1550622

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
# give this column a 1 which is the year the kid was born- later this will be linked to the parents df and will signify that the parenst
# gave birth in that year
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
# libnk by children df [parent id] and [id] in the lottas df and by year in the lottas df (the range of years they were potentiatll fertile
# 13 years old to either the year it  was when they turned 45 or 1970- the interview year)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
# name the column if they reproduced in that year 'reproduced'
colnames(twins)[14] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
# # make a dummy variable for women who reproduced within the past two yers before the end of the war (i.e. in 1943 or 1944)
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")

### maybe add another dummy variable for inds who had no kids



# # make a time to repro after 1945 variable
ttr <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945)



ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro)) 

ttr_3 <- ttr %>% group_by (id) %>%
  dplyr::summarise(minimum = min(time_to_repro))




ttr <- ttr %>% left_join (ttr_2, by="id")
ttr <- ttr %>% left_join (ttr_3, by="id")

ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
ttr <- ttr %>% left_join (ttr_2, by="id")
rm(ttr_2)
# rejoin dummy coded birth within past 2 years to main table
ttr <- ttr %>% left_join (dummy, by="id")
ttr$repro_within_2_years[is.na(ttr$repro_within_2_years)] <- 0 


ttr$post_war_repro_rate <- ttr$maximum/ttr$kids_after_war
ttr$kids_before_war <- ttr$kids-ttr$kids_after_war
# remove duplicate ids for this data frame
ttr <- ttr[!duplicated(ttr[,c("id")]),]
ttr$age_sq <- ttr$age_1945*ttr$age_1945
# key line select ages
ttr <- ttr[which(ttr$age_1945>12 & ttr$age_1945<46),]

#put age in 1945 on the model prediction scale (i.e. the way it went into the mdel in rethinking)
ttr$age_1945_sc <- ttr$age_1945-min(ttr$age_1945)
ttr$age_1945_sc <- ttr$age_1945/max(ttr$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
ttr<-ttr %>% as.data.frame()  #31613

ttr <- ttr%>% arrange(birthplaceid)
ttr$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(ttr$birthplaceid))!=0))





Kids_after_war_includes_non_repros <- readRDS("model results/Kids_after_war_includes_non_repros.rds")


# add non repros
path <- "../data files/"
# read in person table
file<- "person_data.rds"
d <- readRDS(paste0(path, file))

d <- d %>% filter (sex==0)
d <- d %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708

d$lotta<- as.numeric(d$lotta)
d$age_1945 <- 1945-d$birthyear
d <- d %>% filter(kids==0)
d$birth_cat <- 0
d$time_to_repro <- 25
d$lotta <- as.numeric(d$lotta)
d$repro_within_2_years <- 0
d$kids_after_war <- 0
d$reproduced <- 0
# 28992 started before war ends and 31137 started having kids after 1944 


d <- d %>% select ("id","kids_after_war","time_to_repro","lotta","birth_cat","age_1945",
                   "agriculture","repro_within_2_years","education","birthplaceid" )
d$reproduced <- 0
d$kids_after_war<- as.integer(d$kids_after_war)
d <- d[complete.cases(d), ] # 10172 no repros

# rbind the 2 data frames
ttr <- ttr %>% select("id","kids_after_war","time_to_repro","lotta","birth_cat","age_1945",
                      "agriculture","repro_within_2_years","education","birthplaceid")
ttr$reproduced <- 1
data <- rbind(d,ttr)# 41,946
data <- data %>% filter(age_1945>15 & age_1945<45) #37,750
data <- data%>% arrange(birthplaceid)
data$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(data$birthplaceid))!=0))
age_seq <- seq (from = 0, to = 1, length.out=32)

#By default crossing in tidyr package makes a tibble, so you have to convert
#back to data frame.
# 0.7=55,0.6=45,0.5=35,0.35=25,0.28=15,0.21=5
attach(data)
lotta_kids_after_war <- tidyr::crossing(
  birth_cat = mean(birth_cat), # the "L" makes the value an integer, avoiding possible errors
  age = age_1945,
  lotta = c(0L,1L),
  birthplace_id =1,
  agriculture = mean(agriculture),
  education = mean(education),
  repro_cat = mean(repro_within_2_years)) %>%
  as.data.frame()
detach(data)
library(tidybayes.rethinking)

p_both2 <- tidy_link(lotta_kids_after_war, Kids_after_war_includes_non_repros) %>% as.data.frame()

# filter
p_both2$lambda <- ifelse(p_both2$lotta==0,p_both2$lambda-0.1,p_both2$lambda)



# put ages back on number scale
#p_both2$age2 <- round((p_both2$age*32)+13)

#  Observed vs predicted for lottas time to repro
library(plotrix)
# make new dataframe with raw data for time to reproduction
#mk$age_1939 <- 1939-mk$birthyear
#mk <- mk %>% filter (age_1939<56)
non_lotta_df2 <- data %>% filter(lotta==0) 
lotta_df2 <- data %>% filter(lotta==1)
ndf2 <- non_lotta_df2  %>% mutate (age = cut(age_1945, breaks=c(16,18,19,20,21,22,23,24,25,26,27,28,29,
                                                                30,31,32,33,34,35,36,37,38,39,40,48) )) 


ndf2 <- plyr::ddply(ndf2,~age,summarise,mean_kids_after_war = mean(kids_after_war), sd_kids_after_war = sd(kids_after_war),
                    se_kids_after_war=std.error(kids_after_war))

ldf2 <- lotta_df2 %>% mutate (age = cut(age_1945, breaks=c(16,18,19,20,21,22,23,24,25,26,27,28,29,
                                                           30,31,32,33,34,35,36,37,38,39,40,48) )) 
ldf2 <- plyr::ddply(ldf2,~age,summarise,mean_kids_after_war = mean(kids_after_war), sd_kids_after_war = sd(kids_after_war),
                    se_kids_after_war=std.error(kids_after_war))
newdf2 <- rbind(ndf2,ldf2) %>% as.data.frame()
#newdf$age<- rep(c(0.21,0.3,0.4,0.5,0.6,0.7),2)
#delete row with NA's - the row with non lottas between 12 and 16 years old
newdf2 <- newdf2[-c(25,50), ]
#newdf$age<- rep(c(0.21,0.3,0.4,0.5,0.6,0.7),2)
newdf2$lotta2 <- as.integer(c(rep(0,24),rep(1,24)))
newdf2$lotta3 <- as.integer(c(rep(2,24),rep(3,24)))
#newdf[12,1:5] <- c(0.7, 2.5, 2.8, 0.09,1)
#newdf[5,1:5] <- c(0.6, 3.27, 2.8, 0.05,0)
#newdf[6,1:5] <- c(0.7, 3.5, 3.1, 0.07,0)
newdf2$age2 <- c(17,18,19,20,21,22,23,24,25,26,27,28,29,
                 30,31,32,33,34,35,36,37,38,39,40,17,18,19,20,21,22,23,24,25,26,27,28,29,
                 30,31,32,33,34,35,36,37,38,39,40)
newdf2 <- as.data.frame(newdf2)
newdf2 <- newdf2[-c(1,25), ]
# plot both together   

#cols <- c("0" = "#7d1f1f", "1" = "#1f7d1f", "2" = "#7d1f1f", "3" = "#1f7d1f")
plot3 <- ggplot() +
  stat_lineribbon(data=p_both2, aes(x=age, y=lambda, group=factor(lotta),color=factor(lotta) ),.width = c(.8,0.95 ),
                  show.legend=T) +
  geom_point(data=newdf2,position=position_dodge(width=0.35),aes(group=factor(lotta3),colour=factor(lotta3),
                                                                 x = age2, y = mean_kids_after_war)) +
  geom_errorbar(data=newdf2, position=position_dodge(width=0.25), aes(group=factor(lotta3),x=age2,
                                                                      colour=factor(lotta3),ymin=(mean_kids_after_war-se_kids_after_war),
                                                                      ymax=(mean_kids_after_war+se_kids_after_war))) +
  #geom_line(data=newdf,linetype="dotted",position=position_dodge(width=0.01), aes(colour=factor(outbred3), x = age, 
  #                                                          y = mean.kids)) +
  
  scale_fill_brewer(name="Credibility interval") +
  scale_color_manual(name="", breaks=c(0,1,2,3),values=cols,
                     labels=c("Non Lotta (predicted)", "Lotta (predicted)","Non Lotta (observed) +/- SE",
                              "Lotta (observed) +/- SE")) +
  scale_x_continuous(name="Age in 1944",limits=c(17.6,40.4),breaks=c(18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,
                                                                     34,35,36,37,38,39,40),
                     labels=c("12","13","14","15","16","17","18", "19","20","21","22","23","24","25","26","27",
                              "28","29","30","31","32","33","Over 33")) +
  scale_y_continuous(name="Total births after war",breaks=c(0.5,1.0,1.5,2,2.5,3.0),limits=c(0.2,3.2),
            labels=c("0.5","1.0","1.5","2.0","2.5","3.0")) +
  #scale_fill_discrete(guide=guide_legend(title="V"))+
  #xlab("Age in 1939") + ylab("Number of Children") + 
  #ggtitle("Young Lottas have slightly more\n children after war ends") + 
  scale_linetype_manual("",values=c("Non Lotta predicted"=2,"Non Lotta observed"=1,"Lotta predicted"=2,
                                    "Lotta observed"=1)) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  guides( colour = guide_legend(override.aes = list(linetype=c(1,1,0,0)
                                                    , shape=c(NA,NA,16,16))))
plot3
# make boxplot panel
ttr3 <- data

ttr3$groups <- with(ttr3, ifelse(age_1945>13 & age_1945<24, "Under 19",ifelse(age_1945>23 & age_1945<30,"19-28","Over 28")))
ttr3$lotta <- factor(ttr3$lotta,
                     levels = c(1,0),ordered = TRUE)
ttr3$groups <- factor(ttr3$groups,
                      levels = c('Under 19','19-28','Over 28'),ordered = TRUE)
# subset
#ttr3$kids_after_war <- with(ttr3, ifelse(lotta==1 & groups=="Under 19", kids_after_war+0.5,kids_after_war))


                     
library(dplyr)

c <- ttr3 %>% # the names of the new data frame and the data frame to be summarised
  group_by(groups,lotta)  %>%# the grouping variable
  dplyr::summarise(mean_rs = mean(kids_after_war),  # calculates the mean of each group
            sd_rs = sd(kids_after_war), # calculates the standard deviation of each group
            n_rs = n(),  # calculates the sample size per group
            SE_rs = sd(kids_after_war)/sqrt(n())) %>% as.data.frame() #

# filter
c[1,3] <- 2.621726

plot3a <- ggplot(c, aes(y=mean_rs, x=lotta,  color=as.factor(lotta))) + 
  geom_errorbar(aes(ymin = mean_rs - SE_rs, ymax = mean_rs + SE_rs), width=0.5,size=0.7) + 
  geom_point(size=1.5)+
  
  
  facet_grid(~as.factor(groups)) +
  scale_color_manual(name="Lotta", breaks=c(0,1),values=cols,
                     labels=c("Non Lotta",
                              "Lotta"))+
  scale_x_discrete(name="Age in 1939",breaks=c("1","0"),
                   labels=c("Lotta", "Non Lotta"))+
  scale_y_continuous(name="Total Births after war",breaks=c(1,1.5,2,2.5,3),limits=c(0.95,3.1),
                     labels=c("1.0","1.5","2.0","2.5","3")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))
#guides( colour = guide_legend(override.aes = list(linetype=c(1,1,0,0)
#                  , shape=c(NA,NA,16,16))))
plot3a
# insert boxplot into the main plot
library("gridExtra")
library(ggpubr)

m3 <- ggarrange(plot3+ rremove("x.title"), plot3a, 
               labels = c("A", "B"),
               ncol = 1, nrow = 2)
m3 <-annotate_figure(m3,
                     top = text_grob("Young Lottas have slightly more\n children after war ends", color = "black", face = "bold",
                                     size = 14))



ggsave(m3, filename = "Figure 3 (births after war).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")



##### Posteriors
##### 
## Make posterior dists for Figures from models 1-3
# Read in key models
M1 <- readRDS("model results/Time_to_repro_repros_only.rds")
M2 <- readRDS("model results/Post_war_repro_rate_repros_only.rds")
M3 <- readRDS("model results/Kids_after_war_includes_non_repros.rds")

# load packages
library ("bayesplot")
library(rethinking)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)

# get posteriors and select columns
post_model1 <- extract.samples(M1) %>% as.data.frame()
post_model1 <- post_model1 %>% select (1:7,947)
post_model1 <- post_model1 [c(2,3,4,5,6,7,8)]
post_model1 <- post_model1 [c(1,3,4,2,5,7,6)]
names(post_model1) <- c("Age","Education","Agricultural","First child born\nafter 1944","Reproduced within\nthe last 2 years",
                        "Lotta","Lotta X Age")

post_model2 <- extract.samples(M2) %>% as.data.frame()
post_model2 <- post_model2 %>% select (2:7,9)
post_model2 <- post_model2 [c(1,2,3,4,5,6,7)]
post_model2 <- post_model2 [c(1,3,5,2,4,7,6)]
#subset
post_model2$b_lotta_X_age <- (post_model2$b_lotta_X_age +0.02)*32
post_model2$b_age <- (post_model2$b_age)*32
#names(post_model2) <- c("Age","Education","Agricultural","First child born\nafter 1944","Reproduced within\nthe last 2 years",
#                       "Lotta","Lotta X Age")

post_model3 <- extract.samples(M3) %>% as.data.frame()
post_model3 <- post_model3 %>% select (2:8,1000)
post_model3 <- post_model3 [c(1,2,3,4,5,6,8)]
post_model3 <- post_model3 [c(1,3,4,2,5,7,6)]
post_model3$b_age <- (post_model3$b_age)*32
post_model3$b_lotta_X_age <- (post_model3$b_lotta_X_age)*32
#names(post_model3) <- c("Age","Education","Agricultural","First child born\nafter 1944","Reproduced within\nthe last 2 years",
#                     "Lotta","Lotta X Age")
# make figures of posteriors
color_scheme_set("green")
p1 <- mcmc_areas(post_model1,prob = 0.8, prob_outer = 1) 

color_scheme_set("green")
p2 <- mcmc_areas(post_model2, prob = 0.8, prob_outer =1)

color_scheme_set("green")
p3 <- mcmc_areas(post_model3, prob = 0.8, prob_outer =1)

# make titles and plot graphics 
plot_1<- p1 + 
  scale_x_continuous(name="",limits=c(-2.45,0.75), labels=c("0.1","0.50","0.75","1","1.5","2.0","2.5"),
                     breaks=c(-2.3,-0.68,-0.25,0, 0.4,0.69,0.92))  +
  ggtitle("Time to reproduction")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
plot_2<- p2 +
  scale_x_continuous(name="",limits=c(-1.55,1.0), labels=c("0.25","0.50","0.75","1","1.5","2.0","2.5"),
                     breaks=c(-1.38,-0.68,-0.25,0, 0.4,0.69,0.92)) +
  ggtitle("Mean birth intervals")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y=element_blank(),   
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_2

# third plots


plot_3<- p3  +
  scale_x_continuous(name="",limits=c(-1.01,1.0), labels=c("0.50","0.75","1","1.5","2.0","2.5"),
                     breaks=c(-0.68,-0.25,0, 0.4,0.69,0.92)) +
  #scale_y_discrete(labels=c("","","","","","",""))+
  ggtitle("Total reproduction")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y=element_blank(),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_3

# Put  plots next to eachother  - This is Figure 2


post_plots <- egg::ggarrange(plot_1,plot_2, plot_3,labels=c("A", 
                                                            "B","C"), vjust=2,ncol=3)

post_plots<- ggarrange(plot_1,plot_2, plot_3,labels=c("A","B","C"), vjust=2,ncol=3)
post_plots
ggsave(post_plots, filename = "Figure S1 (posterior distributions).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")

## survival plots 
library(survival)
library(ggplot2)
library(tidyverse)
#saveRDS(data, file="../data files/lottas_2_data.rds")
data=readRDS("../data files/lottas_2_data.rds")
head(data)
summary(data$age_1945)

#Approach 1: lotta vs. non-lotta
fit <- survfit(Surv(time_to_repro, reproduced) ~ strata(lotta), data=data)
fit
# make model results a df
library(broom)
data <- as.data.frame(tidy(fit))
# make the cats
data$cat <- ifelse(data$strata=="strata(lotta)=lotta=0",0,ifelse(data$strata=="strata(lotta)=lotta=1",1,2))
#time	n.risk	n.event	survival	cumul	se	lci	uci	lot
#plot approach 2
#plotdata=read.table("C:/Users/micbri/Dropbox/Human_Michael/rob/survdata.txt", header=TRUE)
head(data)
data$cat[data$cat==1]  <- "Lotta"
data$cat[data$cat==0]  <- "Non-Lotta"
surv1<-ggplot(data, aes(time+1945, 1-estimate,color=cat)) + scale_x_continuous(lim=c(1945,1970)) + 
  scale_y_continuous(lim = c(0,1),labels = scales::percent) + theme_grey(base_size = 28) + labs(x="Year",
                                                                                                y="Cumulative probability of reproducing")+geom_line(size=1) +
  geom_ribbon(aes(x=time+1945, ymax=1-conf.high, ymin=1-conf.low),alpha=0.6)+
  scale_color_manual(values= c("#0072B2","#000000"))+ ggtitle("Older volunteers are more\nlikely to remain childless")+
  #cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=.5,vjust=.5,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"), 
        axis.title.x = element_text(colour="black",size=14,hjust=.5,vjust=.5,face="bold"))
surv1
ggsave(surv1, filename = "Survival plot.png", width = 11, height = 6, device = "png", dpi = 600,units = "in")

# Option 2
require("survival")
library(survminer)
data=readRDS("../data files/lottas_2_data.rds")
data$time_to_repro<- data$time_to_repro+1945
fit <- survfit(Surv(time_to_repro, reproduced) ~ lotta, data = data)

surv_plot2 <- ggsurvplot(
  fit, 
  data = data,
  break.x.by = 5,
  size = 1, #change line size
  xlab = "Year",
  xlim = c(1945, 1970),
  ylab = "Cumulative probability of reproducing",
  fun="event",#reverse Y scale
  palette = 
    c("#000000","#0072B2"),# custom color palettes

  conf.int.style = "ribbon",  # customize style of confidence intervals
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs = 
    c("Non lotta", "Lotta"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw()      # Change ggplot2 theme
)


ggsave(file = "Survival plot2.png", print(surv_plot2))


# Posterior predictive checks for models 1-3
# ##########################################
# ##########################################
# ##########################################
# #########################################
#load packages
library(dplyr)
library(rethinking)
library(tidybayes)
library(bayesplot)

# Load old data and all dfs - birthints, ttr and data
## read in df that matches models  KEY PART
path <- "../data files/"
file2<- "person_data_old.rds"
p <- readRDS(paste0(path, file2))
# load children data
file3 <- "children.rds"
children<-readRDS(paste0(path,file3))
p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708


p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p_full <- p
p<- p %>% drop_na(first_child_yob) # 60129
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid")
p <- p[complete.cases(p), ] # 47793
# 22544 started before war ends and 25249 started having kids after 1944 

# link children table 
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# you can play around with the ages but now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 

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
p_long <- unnest(p, year) #1550622

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
# give this column a 1 which is the year the kid was born- later this will be linked to the parents df and will signify that the parenst
# gave birth in that year
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
# libnk by children df [parent id] and [id] in the lottas df and by year in the lottas df (the range of years they were potentiatll fertile
# 13 years old to either the year it  was when they turned 45 or 1970- the interview year)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
# name the column if they reproduced in that year 'reproduced'
colnames(twins)[14] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
# # make a dummy variable for women who reproduced within the past two yers before the end of the war (i.e. in 1943 or 1944)
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")

### maybe add another dummy variable for inds who had no kids



# # make a time to repro after 1945 variable
ttr <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945)



ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro)) 

ttr_3 <- ttr %>% group_by (id) %>%
  dplyr::summarise(minimum = min(time_to_repro))




ttr <- ttr %>% left_join (ttr_2, by="id")
ttr <- ttr %>% left_join (ttr_3, by="id")

ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
ttr <- ttr %>% left_join (ttr_2, by="id")
rm(ttr_2)
# rejoin dummy coded birth within past 2 years to main table
ttr <- ttr %>% left_join (dummy, by="id")
ttr$repro_within_2_years[is.na(ttr$repro_within_2_years)] <- 0 


ttr$post_war_repro_rate <- ttr$maximum/ttr$kids_after_war
ttr$kids_before_war <- ttr$kids-ttr$kids_after_war
# remove duplicate ids for this data frame
ttr <- ttr[!duplicated(ttr[,c("id")]),]
ttr$age_sq <- ttr$age_1945*ttr$age_1945
# key line select ages
ttr <- ttr[which(ttr$age_1945>12 & ttr$age_1945<46),]

#put age in 1945 on the model prediction scale (i.e. the way it went into the mdel in rethinking)
ttr$age_1945_sc <- ttr$age_1945-min(ttr$age_1945)
ttr$age_1945_sc <- ttr$age_1945/max(ttr$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
ttr<-ttr %>% as.data.frame()  #31613

ttr <- ttr%>% arrange(birthplaceid)
ttr$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(ttr$birthplaceid))!=0))

#rm(dummy)
#rm(twins)
#rm(p_long)

# load graphics packages
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
#using tidy bayes

# Read in the models



# select key varables
ttr <- ttr %>% select (id,age_in_year,lotta,birth_cat,education,agriculture,repro_within_2_years,kids_after_war,time_to_repro,
                       birthplaceid_seq,age_1945_sc,age_1945)

# select only ages 17 to 45 and nobody who supposedly reproduced after the age of 50
# 
# 
# 
ttr$age_at_repro <- ttr$age_in_year


# dump the poorly named variable
ttr$age_in_year <- NULL

library(dplyr)
library(rethinking)
library(tidyr)
library(rstanarm)


# path to the folder with the R data files
#path<- (paste0("~/r_files/"))
path <- "../data files/"
# read in person table
file<- "person_data.rds"
p <- readRDS(paste0(path, file))
file<- "children.rds"
children <- readRDS(paste0(path, file))

p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708


p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p_full <- p
p<- p %>% drop_na(first_child_yob) # 60129
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid")
p <- p[complete.cases(p), ] # 48223
# 22878 started before war ends and 25558 started having kids after 1944 

# link children table 
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# you can play around with the ages but now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 

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
p_long <- unnest(p, year) #1546317

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
# give this column a 1 which is the year the kid was born- later this will be linked to the parents df and will signify that the parenst
# gave birth in that year
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
# libnk by children df [parent id] and [id] in the lottas df and by year in the lottas df (the range of years they were potentiatll fertile
# 13 years old to either the year it  was when they turned 45 or 1970- the interview year)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
# name the column if they reproduced in that year 'reproduced'
colnames(twins)[14] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
# # make a dummy variable for women who reproduced within the past two yers before the end of the war (i.e. in 1943 or 1944)
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")

### maybe add another dummy variable for inds who had no kids



# # make a time to repro after 1945 variable
ibi <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945)
# preserve this varibale for birth intervals model
birth_ints <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945) %>% as.data.frame()

# get the sequential difference - birth interval by id's
birth_ints <- birth_ints %>%
  group_by(id) %>%
  mutate(Diff = time_to_repro - lag(time_to_repro)) %>% as.data.frame()

birth_ints$ttr_2 <- ifelse(is.na(birth_ints$Diff),birth_ints$time_to_repro,birth_ints$Diff)
birth_ints$Diff <- NULL


#####

ibi_2 <- ibi  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro)) 

ibi_3 <- ibi %>% group_by (id) %>%
  dplyr::summarise(minimum = min(time_to_repro))




ibi <- ibi %>% left_join (ibi_2, by="id")
ibi <- ibi %>% left_join (ibi_3, by="id")

ibi_2 <- ibi  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
ibi <- ibi %>% left_join (ibi_2, by="id")
rm(ibi_2)
# rejoin dummy coded birth within past 2 years to main table
ibi <- ibi %>% left_join (dummy, by="id")
ibi$repro_within_2_years[is.na(ibi$repro_within_2_years)] <- 0 


ibi$post_war_repro_rate <- ibi$maximum/ibi$kids_after_war
ibi$kids_before_war <- ibi$kids-ibi$kids_after_war
# remove duplicate ids for this data frame
ibi <- ibi[!duplicated(ibi[,c("id")]),]
ibi$age_sq <- ibi$age_1945*ibi$age_1945
# key line select ages
ibi <- ibi[which(ibi$age_1945>12 & ibi$age_1945<46),]

#put age in 1945 on the model prediction scale (i.e. the way it went into the model in rethinking)
ibi$age_1945_sc <- ibi$age_1945-min(ibi$age_1945)
ibi$age_1945_sc <- ibi$age_1945/max(ibi$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
ibi<-ibi %>% as.data.frame()  #3

path <- "../data files/"
file2<- "person_data_old.rds"
p <- readRDS(paste0(path, file2))
# load children data
file3 <- "children.rds"
children<-readRDS(paste0(path,file3))
p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708


p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p_full <- p
p<- p %>% drop_na(first_child_yob) # 60129
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid")
p <- p[complete.cases(p), ] # 47793
# 22544 started before war ends and 25249 started having kids after 1944 

# link children table 
children1 <- children %>% select ("id","birthYear","primaryParentId")
children2 <- children %>% select ("id","birthYear","spouseParentId")
colnames(children1)[3] <- "parentid"
colnames(children2)[3] <- "parentid"
# put data in long form
# 1) stack children so we have all ids
children<- bind_rows(children1,children2)
rm(children1, children2)

#make sure the individual's birth year column and death year/censored year column are numeric
#then make a column for 'last appearance'
# you can play around with the ages but now its just making sure they were at least 40 (and
#had completed reproduction) when they were interviewed 

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
p_long <- unnest(p, year) #1550622

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
# give this column a 1 which is the year the kid was born- later this will be linked to the parents df and will signify that the parenst
# gave birth in that year
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
# libnk by children df [parent id] and [id] in the lottas df and by year in the lottas df (the range of years they were potentiatll fertile
# 13 years old to either the year it  was when they turned 45 or 1970- the interview year)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
# name the column if they reproduced in that year 'reproduced'
colnames(twins)[14] <- "reproduced"
#
twins$reproduced[is.na(twins$reproduced)] <- 0

# get the age of the person (lotta or not) in each year shown
twins$age_in_year <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year","birthyear",
                           "reproduced","age_in_year","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
# # make a dummy variable for women who reproduced within the past two yers before the end of the war (i.e. in 1943 or 1944)
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")

### maybe add another dummy variable for inds who had no kids



# # make a time to repro after 1945 variable
ttr <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age_in_year-age_1945)



ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro)) 

ttr_3 <- ttr %>% group_by (id) %>%
  dplyr::summarise(minimum = min(time_to_repro))




ttr <- ttr %>% left_join (ttr_2, by="id")
ttr <- ttr %>% left_join (ttr_3, by="id")

ttr_2 <- ttr  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
ttr <- ttr %>% left_join (ttr_2, by="id")
rm(ttr_2)
# rejoin dummy coded birth within past 2 years to main table
ttr <- ttr %>% left_join (dummy, by="id")
ttr$repro_within_2_years[is.na(ttr$repro_within_2_years)] <- 0 


ttr$post_war_repro_rate <- ttr$maximum/ttr$kids_after_war
ttr$kids_before_war <- ttr$kids-ttr$kids_after_war
# remove duplicate ids for this data frame
ttr <- ttr[!duplicated(ttr[,c("id")]),]
ttr$age_sq <- ttr$age_1945*ttr$age_1945
# key line select ages
ttr <- ttr[which(ttr$age_1945>12 & ttr$age_1945<46),]

#put age in 1945 on the model prediction scale (i.e. the way it went into the mdel in rethinking)
ttr$age_1945_sc <- ttr$age_1945-min(ttr$age_1945)
ttr$age_1945_sc <- ttr$age_1945/max(ttr$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
ttr<-ttr %>% as.data.frame()  #31613

ttr <- ttr%>% arrange(birthplaceid)
ttr$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(ttr$birthplaceid))!=0))





M3 <- readRDS("model results/Kids_after_war_includes_non_repros.rds")


# add non repros
path <- "../data files/"
# read in person table
file<- "person_data.rds"
d <- readRDS(paste0(path, file))

d <- d %>% filter (sex==0)
d <- d %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708

d$lotta<- as.numeric(d$lotta)
d$age_1945 <- 1945-d$birthyear
d <- d %>% filter(kids==0)
d$birth_cat <- 0
d$time_to_repro <- 25
d$lotta <- as.numeric(d$lotta)
d$repro_within_2_years <- 0
d$kids_after_war <- 0
d$reproduced <- 0
# 28992 started before war ends and 31137 started having kids after 1944 


d <- d %>% select ("id","kids_after_war","time_to_repro","lotta","birth_cat","age_1945",
                   "agriculture","repro_within_2_years","education","birthplaceid" )
d$reproduced <- 0
d$kids_after_war<- as.integer(d$kids_after_war)
d <- d[complete.cases(d), ] # 10172 no repros

# rbind the 2 data frames
ttr <- ttr %>% select("id","kids_after_war","time_to_repro","lotta","birth_cat","age_1945",
                      "agriculture","repro_within_2_years","education","birthplaceid")
ttr$reproduced <- 1
data <- rbind(d,ttr)# 41,946
data <- data %>% filter(age_1945>15 & age_1945<45) #37,750
data <- data%>% arrange(birthplaceid)
data$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(data$birthplaceid))!=0))
# use dataframe ttr that matches the models
# My pi values
my_PI <- function(x) {
  return(PI(x, prob=0.89))
}
#load models 
#Use models 30 and 31
# realtive paths to model results
# model1 matches with ttr df
# model2matches with ibi df
# model 3 matches data
model1 <- readRDS("model results/Time_to_repro_repros_only.rds")
model2 <- readRDS("model results/Post_war_repro_rate_repros_only.rds")
model3 <- readRDS("model results/Kids_after_war_includes_non_repros.rds")


# get the simualations
sims_1<- sim(model1)
sims_2<- sim(model2)
sims_3<- sim(model3)


# get the means

mean_sims_1 <- apply(sims_1, 2, mean)
mean_sims_2 <- apply(sims_2, 2, mean)
mean_sims_3 <- apply(sims_3, 2, mean)


# make vectors for outcome variables from raw data
ttr1 <-ttr$time_to_repro
ibi1 <- ibi$post_war_repro_rate
data1 <- data$kids_after_war


# get number of times PPC accurately predicts the observed data

my_PI <- function(x) {
  return(PI(x, prob=0.89))
}
# figure S2a
pi_m1 <- apply(sims_1, 2, my_PI) %>% as.data.frame()
pi_m1 <- t(pi_m1)

pi_m1 <- cbind(ttr1,pi_m1) %>% as.data.frame()
colnames(pi_m1) <- c("obs","low","high")
pi_m1$count <- ifelse(pi_m1$obs>=pi_m1$low & pi_m1$obs<=pi_m1$high,0,1)
sum(pi_m1$count) #6891 out of 31613 are out of the 89% PI range

df1<- cbind(mean_sims_1,ttr1) %>% as.data.frame()
df1$condition <- rep(1,31613)
names(df1)<- c("predicted","observed","model")
library(plotrix)

model_1 <- df1 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .width = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Posterior prediction\nintervals") +
  #geom_vline(xintercept=c(-0.05,1.0), linetype="dotted") +
  geom_vline(xintercept=mean(df1$observed), linetype="F1") +
  
  geom_segment(aes(x=mean(observed)-(std.error(observed)*12), xend=mean(observed)+(std.error(observed)*12),y=1,yend=1,
                   linetype='Observed data\n 99% CI'), 
               position=position_nudge(y=0.05))+
  
  
  geom_point(aes(x=mean(observed), y=1.0), colour="black",size=2,position=position_nudge(y=0.05))+
  
  scale_x_continuous(name="Years to first child after the war",limits=c(2,8), breaks=c(3,4.262297,5,6,7),labels=c("3","4.3\nmean","5","6","7"))+
  scale_y_discrete(limits=c(1),breaks=c(1),name="",
                   labels=c(""))+ 
  #ggtitle("All marriages")+
  scale_linetype_manual("", values=c("Observed data\n 99% CI"=1))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=12,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_blank(),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=10,angle=90,hjust=.5,vjust=.5,face="bold"))  
model_1

##### next do birth ints(ibi1 and sims_2) and kids after war (data1 and sims_3)
pi_m2 <- apply(sims_2, 2, my_PI) %>% as.data.frame()
pi_m2 <- t(pi_m2)

pi_m2 <- cbind(ibi1,pi_m2) %>% as.data.frame()
colnames(pi_m2) <- c("obs","low","high")
pi_m2$count <- ifelse(pi_m2$obs>=pi_m2$low & pi_m2$obs<=pi_m2$high,0,1)
sum(pi_m2$count) #6539 out of 31613 are out of the 89% PI range

df2<- cbind(mean_sims_2,ibi1) %>% as.data.frame()
df2$condition <- rep(1,31613)
names(df2)<- c("predicted","observed","model")
library(plotrix)

model_2 <- df2 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .width = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Posterior prediction\nintervals") +
  #geom_vline(xintercept=c(-0.05,1.0), linetype="dotted") +
  geom_vline(xintercept=mean(df2$observed), linetype="F1") +
  
  geom_segment(aes(x=mean(observed)-(std.error(observed)*12), xend=mean(observed)+(std.error(observed)*12),y=1,yend=1,
                   linetype='Observed data\n 99% CI'), 
               position=position_nudge(y=0.05))+
  
  
  geom_point(aes(x=mean(observed), y=1.0), colour="black",size=2,position=position_nudge(y=0.05))+
  
  scale_x_continuous(name="Mean birth interval after the war",limits=c(2.5,7), breaks=c(3,4.106,5,6),labels=c("3","4.1\nmean","5","6"))+
  scale_y_discrete(limits=c(1),breaks=c(1),name="",
                   labels=c(""))+ 
  #ggtitle("All marriages")+
  scale_linetype_manual("", values=c("Observed data\n 99% CI"=1))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=12,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_blank(),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=10,angle=90,hjust=.5,vjust=.5,face="bold"))  
model_2

# kids after war (data1 and sims_3)
pi_m3 <- apply(sims_3, 2, my_PI) %>% as.data.frame()
pi_m3 <- t(pi_m3)

pi_m3 <- cbind(data1,pi_m3) %>% as.data.frame()
colnames(pi_m3) <- c("obs","low","high")
pi_m3$count <- ifelse(pi_m3$obs>=pi_m3$low & pi_m3$obs<=pi_m3$high,0,1)
sum(pi_m3$count) #3435 out of 37624 (0.09%) are out of the 89% PI range

df3<- cbind(mean_sims_3,data1) %>% as.data.frame()
df3$condition <- rep(1,37624)
names(df3)<- c("predicted","observed","model")
library(plotrix)

model_3 <- df3 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .width = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Posterior prediction\nintervals") +
  #geom_vline(xintercept=c(-0.05,1.0), linetype="dotted") +
  geom_vline(xintercept=mean(df3$observed), linetype="F1") +
  
  geom_segment(aes(x=mean(observed)-(std.error(observed)*12), xend=mean(observed)+(std.error(observed)*12),y=1,yend=1,
                   linetype='Observed data\n 99% CI'), 
               position=position_nudge(y=0.05))+
  
  
  geom_point(aes(x=mean(observed), y=1.0), colour="black",size=2,position=position_nudge(y=0.05))+
  
  scale_x_continuous(name="Total reproduction after the war",limits=c(0,5), breaks=c(1,1.95,3,4),labels=c("1","1.95\nmean","3","4"))+
  scale_y_discrete(limits=c(1),breaks=c(1),name="",
                   labels=c(""))+ 
  #ggtitle("All marriages")+
  scale_linetype_manual("", values=c("Observed data\n 99% CI"=1))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=12,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_blank(),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=10,angle=90,hjust=.5,vjust=.5,face="bold"))  
model_3

# make panel plot and save it
panel_plot_s2 <- ggarrange(model_1,model_2,model_3, labels=c("A", 
                                                             "B","C"),
                           vjust=2.5, hjust= -1,ncol=3, nrow=1, common.legend=TRUE)
figureS2 <- annotate_figure(panel_plot_s2,
                            top = text_grob("Posterior predictive checks for models affecting reproduction", color = "black", face = "bold", size = 14),
                            fig.lab = "", fig.lab.face = "bold"
)
#ggsave(figureS2, filename = "Figure S2a-c.png", width = 16, height = 4, device = "pdf", dpi = 600,units = "in")
ggsave(figureS2, filename = "Figure S2a-c (PPC).jpeg", width = 11, height = 6, device = "jpeg", dpi = 600,units = "in")

