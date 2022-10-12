#
plotEventsObsMod <- function(data,eventsObs,eventsMod,gage,thresh) {

data <- data[,c("Date","obs","mod")]

# fill in missing times with NaN
dates <- seq(min(data$Date),max(data$Date),by="hour")
dates1 <- dates[!dates %in% data$Date]
if (length(dates1)>0) {
  data <- rbind(data,data.frame(Date=dates1, obs=NA, mod=NA))
  data <- data[order(data$Date),]
}

peaksObs <- subset(data,Date %in% eventsObs$peak)
peaksMod <- subset(data,Date %in% eventsMod$peak)
startsObs <- subset(data,Date %in% eventsObs$start)
startsMod <- subset(data,Date %in% eventsMod$start)
endsObs <- subset(data,Date %in% eventsObs$end)
endsMod <- subset(data,Date %in% eventsMod$end)

colors <- c(obs="black", mod="red")
require(ggplot2)
require(scales)
gg1 <- ggplot(data,aes(x=Date)) +
       geom_line(aes(y=obs,color="obs",group=1),size=0.5) + 
       geom_line(aes(y=mod,color="mod",group=1),size=0.5) + 
       geom_point(data=peaksObs,aes(Date,obs),color="blue",shape=2,size=2) +
       geom_point(data=startsObs,aes(Date,obs),color="blue",shape=0,size=1) +
       geom_point(data=endsMod,aes(Date,mod),color="purple",shape=1,size=1) +
       geom_point(data=peaksMod,aes(Date,mod),color="purple",shape=2,size=2) +
       geom_point(data=startsMod,aes(Date,mod),color="purple",shape=0,size=1) +
       geom_point(data=endsObs,aes(Date,obs),color="blue",shape=1,size=1) +
       geom_hline(yintercept=thresh,linetype="dashed",color="darkgrey",size=0.5) +
       scale_color_manual(name="",values = colors) +
       scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y-%m-%d")) +
       labs(main=gage,x="",y="streamflow (cms)") +
       guides(color=guide_legend(ncol=2)) +
       theme(text=element_text(size=14),legend.position=c(0.5, 0.99),
         legend.key = element_rect(color = "transparent", fill = "transparent"),
         legend.background = element_rect(fill = "transparent"), # get rid of legend bg
         legend.box.background = element_rect(color="transparent",fill = "transparent"),
         legend.title=element_blank())

ggsave(paste0("../figs/events_",gage,".png"),plot=gg1,height=3.5,width=16,units="in",dpi=300)
}
