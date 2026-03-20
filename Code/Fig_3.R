# Fig. 3
n_strat$x<-rep(c("7","4","6","5","1","2","3"),10)
n_strat$y<-c(rep("a",7),rep("d",7),rep("b",7),rep("e",7),rep("c",7),
                 rep("a",7),rep("d",7),rep("b",7),rep("e",7),rep("c",7))
names_taxa<-c("Large ungulates","Medium ungulates","Small ungulates","Primates","Rodents","Reptiles","Other")
names_strat<-c("Bow & arrows","Gun","Snares","Dogs","Others")
palette_strat<-rep(c("olivedrab","gold2","grey30","brown1","pink3"),7)
par(mar=c(9.5,5,0.5,0.5),xpd="",mfrow=c(1,2))
day<-n_strat[1:35,]
night<-n_strat[36:70,]

barplot(proportions(day$Freq)~ day$y + day$x,las=2,
        beside=TRUE,space=c(0,0.5),ylim=c(0,0.5),xlim=c(0,38),
        xlab="",ylab="Proportion of individuals",
        col=palette_strat,names.arg=names_taxa,border=NA,
        cex.names=1.2,cex.lab=1.3,cex.axis=1.2)

barplot(proportions(night$Freq)~ night$y + night$x,las=2,
        beside=TRUE,space=c(0,0.5),ylim=c(0,0.5),xlim=c(0,38),
        xlab="",ylab="Proportion of individuals",
        col=palette_strat,names.arg=names_taxa,border=NA,
        cex.names=1.2,cex.lab=1.3,cex.axis=1.2)
legend(x=23.5,y=0.53,fill=palette_strat,legend=names_strat,
       cex=1.2,bty="n",border=NA,title="Strategy",title.cex=1.4,x.intersp = 0.5)