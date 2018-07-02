cols <- c("0" = "darkblue", "1" = "darkgreen", "2" = "darkblue", "3" = "darkgreen")
plot1 <- ggplot() +
  #stat_lineribbon(data=p_both, aes(x=age2, y=lambda, group=factor(lotta),color=factor(lotta) ),.prob = c(.8 ),
   #               show.legend=T) +
  geom_point(data=newdf2,position=position_dodge(width=0.01),aes(group=factor(lotta3),colour=factor(lotta3),
                                                                 x = age2, y = mean_kids_after_war)) +
  geom_errorbar(data=newdf2, position=position_dodge(width=0.01), aes(group=factor(lotta3),x=age2,
                                                                      colour=factor(lotta3),ymin=(mean_kids_after_war-se_kids_after_war),
                                                                      ymax=(mean_kids_after_war+se_kids_after_war))) +
  #geom_line(data=newdf,linetype="dotted",position=position_dodge(width=0.01), aes(colour=factor(outbred3), x = age, 
  #                                                          y = mean.kids)) +
  
  #scale_fill_brewer(name="Credibility interval") +
  scale_color_manual(name="", breaks=c(0,1,2,3),values=cols,
                     labels=c("Non Lotta (predicted)", "Lotta (predicted)","Non Lotta (observed) +/- SE",
                              "Lotta (observed) +/- SE")) +
  scale_x_continuous(name="Age in 1944",limits=c(16,42),breaks=c(17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
                                                                 35,36,37,38,39,40),
                     labels=c("17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34",
                              "35","36","37","38","39","Over 40"))  +
  scale_y_continuous(name="Years to reproduction after war",breaks=c(2.5,5,7.5),limits=c(0,6.0),
                     labels=c("2.5","5","7.5")) +
  #scale_fill_discrete(guide=guide_legend(title="V"))+
  #xlab("Age in 1939") + ylab("Number of Children") + 
  ggtitle("Young Lottas wait less time to\n give birth after war ends") + 
  #scale_linetype_manual("",values=c("Non Lotta predicted"=2,"Non Lotta observed"=1,"Lotta predicted"=2,
    #                                "Lotta observed"=1)) +
  
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=18),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))#+
  #guides( colour = guide_legend(override.aes = list(linetype=c(1,1,0,0)
   #                                                 , shape=c(NA,NA,16,16))))
   #                                                 
   plot2 <- ggplot() +
#stat_lineribbon(data=p_both2, aes(x=age2, y=lambda, group=factor(lotta),color=factor(lotta) ),.prob = c(.8 ),
             #   show.legend=T) +
  geom_point(data=newdf2,position=position_dodge(width=0.01),aes(group=factor(lotta3),colour=factor(lotta3),
                                                                 x = age2, y = mean_kids_after_war)) +
  geom_errorbar(data=newdf2, position=position_dodge(width=0.01), aes(group=factor(lotta3),x=age2,
                                                                     colour=factor(lotta3),ymin=(mean_kids_after_war-se_kids_after_war),
                                                                   ymax=(mean_kids_after_war+se_kids_after_war))) +

  
  #scale_fill_brewer(name="Credibility interval") +
  scale_color_manual(name="", breaks=c(0,1,2,3),values=cols,
                     labels=c("Non Lotta (predicted)", "Lotta (predicted)")) +
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
