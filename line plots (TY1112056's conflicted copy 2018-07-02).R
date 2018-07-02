library(dplyr)
library(rethinking)
library(tidybayes)
library(bayesplot)
library(tidybayes.rethinking)
library(data.table)

# read in the dataused to create the model map2 stan object and the model
# read in person tabel and children table
p <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas_2/person_data.rds")
children <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas_2/children.rds")

p <- p %>% filter (sex==0)
p <- p %>% filter (birthyear < 1940 & age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))#80708
p<- p %>% drop_na(first_child_yob) # 60129
p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$age_1945 <- 1945-p$birthyear
p$birth_cat <- ifelse(p$first_child_yob<1944, 0, 1)
# 28992 started before war ends and 31137 started having kids after 1944 


p <- p %>% select ("id","lotta","birthyear","agriculture","education",
                   "age_at_first_birth","age_1945","birth_cat","kids","birthplaceid")
p <- p[complete.cases(p), ] # 47793
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
p_long <- unnest(p, year) #1550622

# Now all women are censored either at age 45 or at the year of their interview

#  NEXT link their kids year of birth to their 'year' by id=parentid
children <- children %>% select ("birthYear","parentid")
children$id <- 1
children<- children %>% drop_na(birthYear)
children<- children %>% drop_na(parentid)
twins <- p_long %>% left_join (children, by=c("id"="parentid","year"="birthYear"))
colnames(twins)[14] <- "reproduced"
twins$reproduced[is.na(twins$reproduced)] <- 0


twins$age <- twins$year-twins$birthyear

# select data frame columns
twins <- twins %>% select ("id","lotta","education","agriculture","year",
                           "reproduced","age","age_1945","age_at_first_birth","birth_cat","kids","birthplaceid")
# find duplicate data
#dupes<-children[which(duplicated(children[,c('parentid','birthYear')])==T),]

# make p_long_3 no duplicates for year and id
#no_twins <- twins[!duplicated(twins[,c("id","year")]),]

### here are the key lines
## now choose the lowest year within each id category that has the first year where
# reproduce = 1

# this makes a years to reproduction after 1944 variable- basically this is the time 
# that women waited after the war to have a kid
dummy <- twins  %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year > 1942 & year<1945) %>% mutate (repro_within_2_years=1)
dummy <- dummy[!duplicated(dummy[,c("id")]),]
dummy <- dummy %>% select ("id","repro_within_2_years")
# # make a birth rate category
birthrate <- twins %>% arrange(id) %>% group_by (id) %>%
  filter (reproduced==1 & year>1944) %>% mutate (time_to_repro=age-age_1945)

birthrate_2 <- birthrate  %>% group_by (id) %>%
  dplyr::summarise(maximum= max(time_to_repro))
birthrate <- birthrate %>% left_join (birthrate_2, by="id")
birthrate_2 <- birthrate  %>% group_by (id) %>%
  dplyr::summarise(kids_after_war= n())
birthrate <- birthrate %>% left_join (birthrate_2, by="id")
rm(birthrate_2)
# rejoin dummy coded birth within past 2 years to main table
birthrate <- birthrate %>% left_join (dummy, by="id")
birthrate$repro_within_2_years[is.na(birthrate$repro_within_2_years)] <- 0 


birthrate$post_war_repro_rate <- birthrate$maximum/birthrate$kids_after_war
birthrate$kids_before_war <- birthrate$kids-birthrate$kids_after_war
# remove duplicate ids for this data frame
birthrate <- birthrate[!duplicated(birthrate[,c("id")]),]
birthrate$age_sq <- birthrate$age_1945*birthrate$age_1945
# key line select ages
birthrate <- birthrate[which(birthrate$age_1945>12 & birthrate$age_1945<46),]
birthrate$age_1945 <- birthrate$age_1945-min(birthrate$age_1945)
birthrate$age_1945 <- birthrate$age_1945/max(birthrate$age_1945)
#p$age_sq = p$age_sq - min(p$age_sq)
#p$age_sq = p$age_sq/max(p$age_sq)
p<-birthrate %>% as.data.frame()  #31613

p <- p%>% arrange(birthplaceid)
p$birthplaceid_seq <- cumsum(c(1,as.numeric(diff(p$birthplaceid))!=0))


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
#residual_kids <- readRDS("C:/Users/rofrly/Dropbox/model results/residual_kids.rds")
M1 <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas_2/model results/Time_to_repro_mixed_model_noagesq.rds")
M2 <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas_2/model results/Kids_after_war_mixed_model_noagesq.rds")
p <- p %>% select (age,lotta,birth_cat,education,agriculture,repro_within_2_years,kids_after_war,time_to_repro,birthplaceid_seq)

# select only ages 17to 45 and nobody who supposedly reproduced after the age of 50
p$ageatbirth <- p$age + p$time_to_repro
p <- p %>% filter (ageatbirth<51 & age>16)
age_seq <- seq (from = 0, to = 1, length.out=32)
#By default crossing in tidyr package makes a tibble, so you have to convert
#back to data frame.
# 0.7=55,0.6=45,0.5=35,0.35=25,0.28=15,0.21=5
attach(p)
lotta_time_to_repro <- tidyr::crossing(
  birth_cat = mean(birth_cat), # the "L" makes the value an integer, avoiding possible errors
  age = age_seq,
  age_sq= age*age,
  lotta = c(0L,1L),
  birthplaceid_seq =1,
  agriculture = mean(agriculture),
  education = mean(education),
  repro_within_2_years = mean(repro_within_2_years)) %>%
  as.data.frame()
detach(p)
library(tidybayes.rethinking)

p_both <- tidy_link(lotta_time_to_repro, Time_to_repro_new2) %>% as.data.frame()

# put ages back on number scale
p_both$age2 <- round((p_both$age*32)+13)

#  Observed vs predicted for lottas time to repro
library(plotrix)
# make new dataframe with raw data for time to reproduction
#mk$age_1939 <- 1939-mk$birthyear
#mk <- mk %>% filter (age_1939<56)
non_lotta_df <- p %>% filter(lotta==0) 
lotta_df <- p %>% filter(lotta==1)
ndf <- non_lotta_df  %>% mutate (age = cut(age, breaks=c(16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
                                                         35,36,37,38,39,48) )) %>% group_by(age) %>% 
  summarise(mean.time.to.repro = mean(time_to_repro), sd.time.to.repro = sd(time_to_repro),
            se.time.to.repro=std.error(time_to_repro))
ldf <- lotta_df %>% mutate (age = cut(age,breaks=c(16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
                                                   35,36,37,38,39,48) )) %>% group_by(age) %>% 
  summarise(mean.time.to.repro = mean(time_to_repro), sd.time.to.repro = sd(time_to_repro),
            se.time.to.repro=std.error(time_to_repro))
newdf <- rbind(ndf,ldf)
#delete row with NA's - the row with non lottas between 12 and 16 years old
#newdf <- newdf[-c(7), ]
#newdf$age<- rep(c(0.21,0.3,0.4,0.5,0.6,0.7),2)
newdf$lotta2 <- as.integer(c(rep(0,24),rep(1,24)))
newdf$lotta3 <- as.integer(c(rep(2,24),rep(3,24)))
#newdf[12,1:5] <- c(0.7, 2.5, 2.8, 0.09,1)
#newdf[5,1:5] <- c(0.6, 3.27, 2.8, 0.05,0)
#newdf[6,1:5] <- c(0.7, 3.5, 3.1, 0.07,0)
newdf$age2 <- c(17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
                35,36,37,38,39,40)
newdf <- as.data.frame(newdf)

# plot both together   

#data <- p_both %>% left_join (newdf, by =c("age2"="age2","lotta"="lotta2"))


cols <- c("0" = "darkblue", "1" = "darkgreen", "2" = "darkblue", "3" = "darkgreen")
plot1 <- ggplot() +
  stat_lineribbon(data=p_both2, aes(x=age2, y=lambda, group=factor(lotta),color=factor(lotta) ),.prob = c(.8 ),
                  show.legend=T) +
  geom_point(data=newdf2,position=position_dodge(width=0.01),aes(group=factor(lotta3),colour=factor(lotta3),
                                                                x = age2, y = mean.time.to.repro)) +
  geom_errorbar(data=newdf2, position=position_dodge(width=0.01), aes(group=factor(lotta3),x=age2,
                                                                     colour=factor(lotta3),ymin=(mean.time.to.repro-se.time.to.repro),
                                                                     ymax=(mean.time.to.repro+se.time.to.repro))) +
  #geom_line(data=newdf,linetype="dotted",position=position_dodge(width=0.01), aes(colour=factor(outbred3), x = age, 
  #                                                          y = mean.kids)) +
  
  scale_fill_brewer(name="Credibility interval") +
  scale_color_manual(name="", breaks=c(0,1,2,3),values=cols,
                     labels=c("Non Lotta (predicted)", "Lotta (predicted)","Non Lotta (observed) +/- SE",
                              "Lotta (observed) +/- SE")) +
  scale_x_continuous(name="Age in 1944",limits=c(16,42),breaks=c(17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
                                                                 35,36,37,38,39,40),
                     labels=c("17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34",
                              "35","36","37","38","39","Over 40"))  +
  scale_y_continuous(name="Time to reproduction (years)",breaks=c(2.5,5,7.5),limits=c(0,6.0),
                     labels=c("2.5","5","7.5")) +
  #scale_fill_discrete(guide=guide_legend(title="V"))+
  #xlab("Age in 1939") + ylab("Number of Children") + 
  ggtitle("Young Lottas wait less time to\n give birth after war ends") + 
  scale_linetype_manual("",values=c("Non Lotta predicted"=2,"Non Lotta observed"=1,"Lotta predicted"=2,
                                           "Lotta observed"=1)) +
  
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=18),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  guides( colour = guide_legend(override.aes = list(linetype=c(1,1,0,0)
                                                    , shape=c(NA,NA,16,16))))






### next make the number of kids after the war figure#####################

 
age_seq <- seq (from = 0, to = 1, length.out=32)
#By default crossing in tidyr package makes a tibble, so you have to convert
#back to data frame.
# 0.7=55,0.6=45,0.5=35,0.35=25,0.28=15,0.21=5
attach(p)
lotta_kids_after_war <- tidyr::crossing(
  birth_cat = mean(birth_cat), # the "L" makes the value an integer, avoiding possible errors
  age = age_seq,
  lotta = c(0L,1L),
  birthplaceid_seq =1,
  agriculture = mean(agriculture),
  education = mean(education),
  repro_within_2_years = mean(repro_within_2_years)) %>%
  as.data.frame()
detach(p)
library(tidybayes.rethinking)

p_both2 <- tidy_link(lotta_kids_after_war, Kids_after_war_new) %>% as.data.frame()

# put ages back on number scale
p_both2$age2 <- round((p_both2$age*32)+13)

#  Observed vs predicted for lottas time to repro
library(plotrix)
# make new dataframe with raw data for time to reproduction
#mk$age_1939 <- 1939-mk$birthyear
#mk <- mk %>% filter (age_1939<56)
non_lotta_df2 <- p %>% filter(lotta==0) 
lotta_df2 <- p %>% filter(lotta==1)
ndf2 <- non_lotta_df2  %>% mutate (age = cut(age, breaks=c(16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
                                                           35,36,37,38,39,48) )) %>% group_by(age) %>% 
  summarise(mean_kids_after_war = mean(kids_after_war), sd_kids_after_war = sd(kids_after_war),
            se_kids_after_war=std.error(kids_after_war))
ldf2 <- lotta_df2 %>% mutate (age = cut(age,breaks=c(16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
                                                     35,36,37,38,39,48))) %>% group_by(age) %>% 
  summarise(mean_kids_after_war = mean(kids_after_war), sd_kids_after_war = sd(kids_after_war),
            se_kids_after_war=std.error(kids_after_war))
newdf2 <- rbind(ndf2,ldf2)
#newdf$age<- rep(c(0.21,0.3,0.4,0.5,0.6,0.7),2)
newdf2$lotta2 <- as.integer(c(rep(0,24),rep(1,24)))
newdf2$lotta3 <- as.integer(c(rep(2,24),rep(3,24)))
#newdf[12,1:5] <- c(0.7, 2.5, 2.8, 0.09,1)
#newdf[5,1:5] <- c(0.6, 3.27, 2.8, 0.05,0)
#newdf[6,1:5] <- c(0.7, 3.5, 3.1, 0.07,0)
newdf2$age2 <- c(17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
                35,36,37,38,39,40)
newdf2 <- as.data.frame(newdf2)

# plot both together   

#data2 <- p_both2 %>% left_join (newdf2, by =c("age2"="age2","lotta"="lotta2"))


#cols <- c("0" = "black", "1" = "grey", "2" = "black", "3" = "grey")
plot2 <- ggplot() +
  stat_lineribbon(data=p_both2, aes(x=age2, y=lambda, group=factor(lotta),color=factor(lotta) ),.prob = c(.8 ),
                  show.legend=T) +
  geom_point(data=newdf2,position=position_dodge(width=0.01),aes(group=factor(lotta3),colour=factor(lotta3),
                                                                x = age2, y = mean_kids_after_war)) +
  geom_errorbar(data=newdf2, position=position_dodge(width=0.01), aes(group=factor(lotta3),x=age2,
                                                                     colour=factor(lotta3),ymin=(mean_kids_after_war-se_kids_after_war),
                                                                     ymax=(mean_kids_after_war+se_kids_after_war))) +
  #geom_line(data=newdf,linetype="dotted",position=position_dodge(width=0.01), aes(colour=factor(outbred3), x = age, 
  #                                                          y = mean.kids)) +
  
  scale_fill_brewer(name="Credibility interval") +
  scale_color_manual(name="", breaks=c(0,1,2,3),values=cols,
                     labels=c("Non Lotta (observed) +/- SE",
                              "Lotta (observed) +/- SE")) +
  scale_x_continuous(name="Age in 1944",limits=c(19,42),breaks=c(19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
                                                                 35,36,37,38,39,40),
                     labels=c("19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34",
                              "35","36","37","38","39","Over 40"))   +
scale_y_continuous(name="Total children after war",breaks=c(1,2,3,4),limits=c(0,6.0),
                   labels=c("1","2","3","4")) +
  #scale_fill_discrete(guide=guide_legend(title="V"))+
  #xlab("Age in 1939") + ylab("Number of Children") + 
  ggtitle("Young Lottas have more children\n after war ends") + 
  scale_linetype_manual("",values=c("Non Lotta predicted"=2,"Non Lotta observed"=1,"Lotta predicted"=2,
                                    "Lotta observed"=1)) +
  
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=18),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  guides( colour = guide_legend(override.aes = list(linetype=c(1,1,0,0)
                                                    , shape=c(NA,NA,16,16))))