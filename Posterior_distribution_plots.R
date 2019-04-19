
# from desktop

M1 <- readRDS("model results/time_to_repro_repros_only.rds")
M2<- readRDS("model results/time_to_repro_repros_only.rds")
M1 <- readRDS("model results/time_to_repro_repros_only.rds")
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

#################start here !!!!
post_model1 <- post_model1 %>% select (1:7,947)
post_model1 <- post_model1 [c(2,3,4,5,6,7,8)]

post_model2 <- extract.samples(M2) %>% as.data.frame()
post_model2 <- post_model2 %>% select (1:7,947)
post_model2 <- post_model2 [c(2,3,4,5,6,7,8)]


# make figures of posteriors
color_scheme_set("green")
p1 <- mcmc_areas(post_model1,prob = 0.8, prob_outer = 1) 

color_scheme_set("blue")
p2 <- mcmc_areas(post_model2, prob = 0.8, prob_outer =1)

# make titles and plot graphics 
plot_1<- p1 + scale_y_discrete(limits=c("Lotta","Lotta X Age","Reproduced within\nthe last 2 years","Agricultural","Education",
                                        "First child born\nafter 1944",
                                        "Age")) +
  scale_x_continuous(name="Odds ratio",limits=c(-2.45,0.75), labels=c("0.13","0.45","0.67","1","1.5"),
                     breaks=c(-2,-0.8,-0.4, 0, 0.4)) +
  ggtitle("Time to reproduction after 1944")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
plot_2<- p2 + scale_y_discrete(limits=c("Lotta","Lotta X Age","Reproduced within\nthe last 2 years","Agricultural","Education",
                                        "First child born\nafter 1944",
                                        "Age")) +
  scale_x_continuous(name="Odds ratio",limits=c(-0.5,0.5), labels=c("0.67","1","1.5"),
                     breaks=c(-0.4, 0, 0.4)) +
  ggtitle("Total children birthed after 1944")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_2
# Put  plots next to eachother  - This is Figure 2
m_plots <- ggarrange(plot_1,plot_2, labels=c("Time to reproduction after 1944", 
                                            "Total children birthed after 1944"),
vjust=2.5, hjust= -2,ncol=2, nrow=1, common.legend=TRUE)

