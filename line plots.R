library(dplyr)
library(rethinking)
library(tidybayes)
library(bayesplot)
library(tidybayes.rethinking)
library(data.table)
library(tidyr)
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

M2 <- readRDS("model results/Kids_after_war_mixed_model_noagesq.rds")
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

#ttr <-ttr %>% filter (ageatbirth<51 & age>16)
age_seq <- seq (from = 0, to = 1, length.out=32)
#By default crossing in tidyr package makes a tibble, so you have to convert
#back to data frame.
# 0.7=55,0.6=45,0.5=35,0.35=25,0.28=15,0.21=5
attach(ttr)
lotta_time_to_repro <- tidyr::crossing(
  birth_cat = mean(birth_cat), # the "L" makes the value an integer, avoiding possible errors
  age = age_seq,
  #age_sq= age*age,
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

# plot both together   

data <- p_both %>% left_join (newdf, by =c("age2"="age2","lotta"="lotta2"))


cols <- c("0" = "darkblue", "1" = "darkgreen", "2" = "darkblue", "3" = "darkgreen")
plot1 <- ggplot() +
  stat_lineribbon(data=data, aes(x=age2, y=lambda, group=factor(lotta),color=factor(lotta) ),.width = c(.8 ),
                  show.legend=T) +
  geom_point(data=newdf,position=position_dodge(width=0.35),aes(group=factor(lotta3),colour=factor(lotta3),
                                                                x = age2, y = mean.time.to.repro)) +
  geom_errorbar(data=newdf, position=position_dodge(width=0.35), aes(group=factor(lotta3),x=age2,
                                                                     colour=factor(lotta3),ymin=(mean.time.to.repro-se.time.to.repro),
                                                                     ymax=(mean.time.to.repro+se.time.to.repro))) +
  #geom_line(data=newdf,linetype="dotted",position=position_dodge(width=0.01), aes(colour=factor(outbred3), x = age, 
  #                                                          y = mean.kids)) +
  
  scale_fill_brewer(name="Credibility interval") +
  scale_color_manual(name="", breaks=c(0,1,2,3),values=cols,
                     labels=c("Non Lotta (predicted)", "Lotta (predicted)","Non Lotta (observed) +/- SE",
                              "Lotta (observed) +/- SE")) +
  scale_x_continuous(name="Age in 1944",limits=c(17.5,40.5),breaks=c(18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,
                                                                 34,35,36,37,38,39,40),
                     labels=c("18","19","20","21","22","23","24", "25","26","27","28","29","30","31","32","33",
                              "34","35","36","37","38","39","Over 40"))  +
  scale_y_continuous(name="Time to reproduction (years)",breaks=c(1.5,3,4.5),limits=c(0,6.0),
                     labels=c("1.5","3","4.5")) +
  #scale_fill_discrete(guide=guide_legend(title="V"))+
  #xlab("Age in 1939") + ylab("Number of Children") + 
  ggtitle("Young Lottas wait less time to\n give birth after war ends") + 
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




ggsave(plot1, filename = "Figure 2 (time to repro).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")

### next make the number of kids after the war figure#####################

 
age_seq <- seq (from = 0, to = 1, length.out=32)
#By default crossing in tidyr package makes a tibble, so you have to convert
#back to data frame.
# 0.7=55,0.6=45,0.5=35,0.35=25,0.28=15,0.21=5
attach(ttr)
lotta_kids_after_war <- tidyr::crossing(
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

p_both2 <- tidy_link(lotta_kids_after_war, M2) %>% as.data.frame()

# put ages back on number scale
p_both2$age2 <- round((p_both$age*32)+13)

#  Observed vs predicted for lottas time to repro
library(plotrix)
# make new dataframe with raw data for time to reproduction
#mk$age_1939 <- 1939-mk$birthyear
#mk <- mk %>% filter (age_1939<56)
non_lotta_df2 <- ttr %>% filter(lotta==0) 
lotta_df2 <- ttr %>% filter(lotta==1)
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

# plot both together   


cols <- c("0" = "darkblue", "1" = "darkgreen", "2" = "darkblue", "3" = "darkgreen")
plot2 <- ggplot() +
  stat_lineribbon(data=p_both2, aes(x=age2, y=lambda, group=factor(lotta),color=factor(lotta) ),.width = c(.8 ),
                  show.legend=T) +
  geom_point(data=newdf2,position=position_dodge(width=0.03),aes(group=factor(lotta3),colour=factor(lotta3),
                                                                x = age2, y = mean_kids_after_war)) +
  geom_errorbar(data=newdf2, position=position_dodge(width=0.03), aes(group=factor(lotta3),x=age2,
                                                                     colour=factor(lotta3),ymin=(mean_kids_after_war-se_kids_after_war),
                                                                     ymax=(mean_kids_after_war+se_kids_after_war))) +
  #geom_line(data=newdf,linetype="dotted",position=position_dodge(width=0.01), aes(colour=factor(outbred3), x = age, 
  #                                                          y = mean.kids)) +
  
  scale_fill_brewer(name="Credibility interval") +
  scale_color_manual(name="", breaks=c(0,1,2,3),values=cols,
                     labels=c("Non Lotta (predicted)", "Lotta (predicted)","Non Lotta (observed) +/- SE",
                              "Lotta (observed) +/- SE")) +
  scale_x_continuous(name="Age in 1944",limits=c(17.5,40.5),breaks=c(18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,
                                                                 34,35,36,37,38,39,40),
                     labels=c("18","19","20","21","22","23","24", "25","26","27","28","29","30","31","32","33",
                              "34","35","36","37","38","39","Over 40"))   +
  scale_y_continuous(name="Total births after war",breaks=c(1.5,2.5,3.5,4.5,5.5,6.5),limits=c(1.0,4.0),
                     labels=c("1.5","2.5","3.5","4.5","5.5","6.5")) +
  #scale_fill_discrete(guide=guide_legend(title="V"))+
  #xlab("Age in 1939") + ylab("Number of Children") + 
  ggtitle("Young Lottas have more children\n after war ends") + 
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

ggsave(plot2, filename = "Figure 3 (births after war).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")


################ Survival analysis code
library(survival)
# get the time to the event or time censored
data$cens <- surv(time,event)

#coxph(time_to_repro, cens) ~ age_1945 +  ....))

# models to run
#1)  reproduced binomial regression
# use p_full
p_full <- p_full %>% select ("id","lotta","kids")
p_full <- p_full[complete.cases(p_full), ] 
# make reporoduced binomial variable
p_full$reproduced <- ifelse(p_full$kids>0,1,0)
#Run regular binomial regression for lottas reproduced vs did not reproduce- Model 1
model <- glm(reproduced ~ lotta, family="binomial", data=p_full)


# run the time to repro with censored data (for inds who never reproduced) in regular glmm and in rethinking- model 2
# 
# run the interbirth intervals to get at reproductive rate
# 1) restructure data
# ID   IBI
# 1     2
# 1     3
# 1     2
# 2     1
# 2     3
# 2     1

# then run repated measures mixed model in glmm and rethinking- id is randome effect for repeated DV
glmer (IBI ~ lotta + lotta*age +....+ (1|ID))