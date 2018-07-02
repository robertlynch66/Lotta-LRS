cols <- c("0" = "darkblue", "1" = "darkgreen")
plot1 <- ggplot() +
  stat_lineribbon(data=p_both, aes(x=age2, y=lambda, group=factor(lotta),color=factor(lotta) ),.prob = c(.8 ),
                  show.legend=T)+
  
  scale_fill_brewer(name="Credibility interval") +
  scale_color_manual(name="", breaks=c(0,1),values=cols,
                     labels=c("Non Lotta (predicted)", "Lotta (predicted)")) +
  scale_x_continuous(name="Age in 1944",limits=c(16,42),breaks=c(17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
                                                                 35,36,37,38,39,40),
                     labels=c("17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34",
                              "35","36","37","38","39","Over 40"))  +
  scale_y_continuous(name="Time to reproduction (years)",breaks=c(1.5,2.5,3.5,4.5),limits=c(0,6.0),
                     labels=c("1.5","2.5","3.5","4.5")) +
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
                                                    , shape=c(NA,NA,16,16)))))




  
  +
    geom_point(data=newdf,position=position_dodge(width=0.01),aes(group=factor(lotta3),colour=factor(lotta3),
                                                                  x = age2, y = mean.time.to.repro)) +
    geom_errorbar(data=newdf, position=position_dodge(width=0.01), aes(group=factor(lotta3),x=age2,
                                                                       colour=factor(lotta3),ymin=(mean.time.to.repro-se.time.to.repro),
                                                                       ymax=(mean.time.to.repro+se.time.to.repro))) +
    #geom_line(data=newdf,linetype="dotted",position=position_dodge(width=0.01), aes(colour=factor(outbred3), x = age, 
    #                                                          y = mean.kids)) +

