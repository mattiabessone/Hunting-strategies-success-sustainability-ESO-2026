##### Plot ungualtes by species
# Small
par(mar=c(4,7,1,1),xpd=FALSE,cex=1.5,mfrow=c(1,3))
plot(y=seq(0,9,1),x=coeff_ung_small[,2],pch=16,col="darkred",
     cex=2,cex.axis=1.3,cex.lab=1.5,xlim=c(-5,5),ylim=c(-0.6,9.6),yaxt="n",ylab="",bty="n",xlab="Coefficients",type="n")
abline(v=0,col="grey40",lwd=2)
points(y=seq(0,9,1),x=coeff_ung_small[,2],pch=16,col="darkred",cex=2)
segments(y0=seq(0,9,1),y1=seq(0,9,1),
         x0=coeff_ung_small[,3],
         x1=coeff_ung_small[,4],
         col="darkred",lwd=5)
axis(side=2,at=seq(0,9,1),labels = coeff_ung_small[,1],cex.axis=1,las=1)
# Medium
plot(y=seq(0,9,1),x=coeff_ung_medium[,2],pch=16,col="darkred",
     cex=2,cex.axis=1.3,cex.lab=1.5,xlim=c(-5,5),ylim=c(-0.6,9.6),yaxt="n",ylab="",bty="n",xlab="Coefficients",type="n")
abline(v=0,col="grey40",lwd=2)
points(y=seq(0,9,1),x=coeff_ung_medium[,2],pch=16,col="darkred",cex=2)
segments(y0=seq(0,9,1),y1=seq(0,9,1),
         x0=coeff_ung_medium[,3],
         x1=coeff_ung_medium[,4],
         col="darkred",lwd=5)
axis(side=2,at=seq(0,9,1),labels = coeff_ung_medium[,1],cex.axis=1,las=1)

# Large
plot(y=seq(0,9,1),x=coeff_ung_large[,2],pch=16,col="darkred",
     cex=2,cex.axis=1.3,cex.lab=1.5,xlim=c(-5,5),ylim=c(-0.6,9.6),yaxt="n",ylab="",bty="n",xlab="Coefficients",type="n")
abline(v=0,col="grey40",lwd=2)
points(y=seq(0,9,1),x=coeff_ung_large[,2],pch=16,col="darkred",cex=2)
segments(y0=seq(0,9,1),y1=seq(0,9,1),
         x0=coeff_ung_large[,3],
         x1=coeff_ung_large[,4],
         col="darkred",lwd=5)
axis(side=2,at=seq(0,9,1),labels = coeff_ung_large[,1],cex.axis=1,las=1)
