par(mfrow=c(2,2),mar=c(4,5,3,1))
lab_month<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#Pregnant ungulates
p_preg_ung<-barplot(preg_month_ung$x[1:12],names.arg =preg_month_ung$Group.1[1:12],ylim=c(0,0.5),xaxt="n",las=1,cex.axis=1.5,borde=NA,col="lightskyblue2",main="Ungulates",cex.main=2)
axis(side=1,at=p_preg_ung,labels=rep("",12),line=0.5,tck=-0.03,cex=1.5)
mtext("Proportion pregnant",side=2,las=0,line=3.5,cex=1.5)
#Pregnant primates
p_preg_prim<-barplot(preg_month_prim$x[1:12],names.arg =preg_month_prim$Group.1[1:12],ylim=c(0,0.5),xaxt="n",yaxt="n",las=1,cex.axis=1.5,borde=NA,col="pink3",,main="Primates",cex.main=2)
axis(side=1,at=p_preg_ung,labels=rep("",12),line=0.5,tck=-0.03)
#Lactating ungulates
p_lact_ung<-barplot(lact_month_ung$x[1:12],names.arg =lact_month_ung$Group.1[1:12],ylim=c(0,0.5),xaxt="n",las=1,cex.axis=1.5,borde=NA,col="lightskyblue2")
axis(side=1,at=p_preg_ung,labels=lab_month,line=0.5,tck=-0.03,las=2,cex.axis=1.5)
mtext("Proportion lactating",side=2,las=0,line=3.5,cex=1.5)

#Lactating primates
p_lact_prim<-barplot(lact_month_prim$x[1:12],names.arg =lact_month_prim$Group.1[1:12],ylim=c(0,0.5),xaxt="n",yaxt="n",las=1,cex.axis=1.5,borde=NA,col="pink3")
axis(side=1,at=p_preg_ung,labels=lab_month,line=0.5,tck=-0.03,las=2,cex.axis=1.5)

