# Ungulates
par(mar=c(4,8.5,1,1),xpd=FALSE,cex=1,mfrow=c(2,2))
plot(y=seq(0,9,1),x=coeff_ung[,2],pch=16,col="darkred",
     cex=2,cex.axis=1.3,cex.lab=1.5,xlim=c(-4,4),ylim=c(-0.6,9.6),yaxt="n",ylab="",bty="n",xlab="Coefficients",type="n")
abline(v=0,col="grey40",lwd=2)
points(y=seq(0,9,1),x=coeff_ung[,2],pch=16,col="darkred",cex=2)
segments(y0=seq(0,9,1),y1=seq(0,9,1),
         x0=coeff_ung[,3],
         x1=coeff_ung[,4],
         col="darkred",lwd=5)
axis(side=2,at=seq(0,9,1),labels = coeff_ung[,1],cex.axis=1.4,las=1)

# Primates
plot(y=seq(0,9,1),x=coeff_ung[,2],pch=16,col="orange",
     cex=2,cex.axis=1.3,cex.lab=1.5,xlim=c(-4,5),ylim=c(-0.6,9.6),yaxt="n",ylab="",bty="n",xlab="Coefficients",type="n")
abline(v=0,col="grey40",lwd=2)
points(y=seq(0,9,1),x=c(coeff_prim[1:6,2],0,coeff_prim[7:9,2]),pch=16,col=c(rep("orange",6),"transparent",rep("orange",3)),cex=2)
segments(y0=seq(0,9,1),y1=seq(0,9,1),
         x0=c(coeff_prim[1:6,3],0,coeff_prim[7:9,3]),
         x1=c(coeff_prim[1:6,4],0,coeff_prim[7:9,4]),
         col=c(rep("orange",6),"transparent",rep("orange",3)),lwd=5)
axis(side=2,at=seq(0,9,1),labels = coeff_ung[,1],cex.axis=1.4,las=1)

# Rodents
plot(y=seq(0,8,1),x=coeff_rod[,2],pch=16,col="steelblue",
     cex=2,cex.axis=1.3,cex.lab=1.5,xlim=c(-4.5,8.8),ylim=c(-0.6,8.6),yaxt="n",ylab="",bty="n",xlab="Coefficients",type="n")
abline(v=0,col="grey40",lwd=2)
points(y=seq(0,8,1),x=coeff_rod[,2],pch=16,col="steelblue",cex=2)
segments(y0=seq(0,8,1),y1=seq(0,8,1),
         x0=coeff_rod[,3],
         x1=coeff_rod[,4],
         col="steelblue",lwd=5)
axis(side=2,at=seq(0,8,1),labels = coeff_rod[,1],cex.axis=1.4,las=1)

# Reptiles
plot(y=seq(0,8,1),x=coeff_rep[,2],pch=16,col="darkgreen",
     cex=2,cex.axis=1.3,cex.lab=1.5,xlim=c(-4,5),ylim=c(-0.6,8.6),yaxt="n",ylab="",bty="n",xlab="Coefficients",type="n")
abline(v=0,col="grey40",lwd=2)
points(y=seq(0,8,1),x=coeff_rep[,2],pch=16,col="darkgreen",cex=2)
segments(y0=seq(0,8,1),y1=seq(0,8,1),
         x0=coeff_rep[,3],
         x1=coeff_rep[,4],
         col="darkgreen",lwd=5)
axis(side=2,at=seq(0,8,1),labels = coeff_rep[,1],cex.axis=1.4,las=1)

