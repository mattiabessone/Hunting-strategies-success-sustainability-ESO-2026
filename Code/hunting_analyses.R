setwd("~/GitHub/PPP-Wildlife-Hunting-Analyses")
results<-read.csv("Data/results_all.csv")

library(RColorBrewer)
library(stringr)
#### RESULTS ####
#Descriptives
results$Genre<-ifelse(results$Genre=="pangolins" | results$Genre=="bird"| results$Genre=="carnivore"| results$Genre=="autre","Others",results$Genre)
results$sexe<-ifelse(results$sexe=="male",1,ifelse(results$sexe=="femelle",0,-1))
results$en_allaitement<-ifelse(results$en_allaitement=="oui",1,ifelse(results$en_allaitement=="non",0,-1))
results$enceinte<-ifelse(results$enceinte=="oui",1,ifelse(results$enceinte=="non",0,-1))
#Rename taxa & strategy
library(stringr)
levels(as.factor(results$Genre))
results$Genre<-str_replace_all(results$Genre,"ungulate_small","Ungulates small")
results$Genre<-str_replace_all(results$Genre,"ungulate_medium","Ungulates medium")
results$Genre<-str_replace_all(results$Genre,"ungulate_large","Ungulates large")
results$Genre<-str_replace_all(results$Genre,"primates","Primates")
results$Genre<-str_replace_all(results$Genre,"hogs","Ungulates large")
results$Genre<-str_replace_all(results$Genre,"rodent","Rodents")
results$Genre<-str_replace_all(results$Genre,"reptiles","Reptiles")
results$Genre<-str_replace_all(results$Genre,"others","Others")
levels(as.factor(results$Genre))
order_genre<-c(7,4,6,5,3,2,1) # plotting order

results$strategie<-str_replace_all(results$strategie,"arc&fleches","Bow & arrows")
results$strategie<-str_replace_all(results$strategie,"fusil","Gun")
results$strategie<-str_replace_all(results$strategie,"chiens","Dogs")
results$strategie<-str_replace_all(results$strategie,"machette","Other")
results$strategie<-str_replace_all(results$strategie,"pieges","Snares")
results$strategie<-str_replace_all(results$strategie,"filets","Other")
results$strategie<-str_replace_all(results$strategie,"autres","Other")
levels(as.factor(results$strategie))
order_strategie<-c(1,5,2,4,6,3) # plotting order
#Remove rows with no animal recorded (Genre = "Aucun")
results<-subset(results,results$espece!="Aucun")
# total animals hunted = 1,671
length(results[,1])
# mean body mass of hunted species = 5.80
mean(results$poids_kg,na.rm=TRUE)
sd(results$poids_kg,na.rm=TRUE)
MBM<-as.data.frame(matrix(NA,nrow=6,ncol=2))
MBM[1,2]<-mean(subset(results$poids_kg,results$village=="bekombo"),na.rm=TRUE)
MBM[2,2]<-mean(subset(results$poids_kg,results$village=="ipope"),na.rm=TRUE)
MBM[3,2]<-mean(subset(results$poids_kg,results$village=="lompole"),na.rm=TRUE) 
MBM[4,2]<-mean(subset(results$poids_kg,results$village=="mbongo"),na.rm=TRUE)
MBM[5,2]<-mean(subset(results$poids_kg,results$village=="mbungusani"),na.rm=TRUE)
MBM[6,2]<-mean(subset(results$poids_kg,results$village=="nganda"),na.rm=TRUE)
MBM[,1]<-c("V1","V2","V3","V4","V5","V6")
MBM
par(mar=c(4.5,4.5,1,1))
barplot(MBM$V2,col="steelblue",border=NA,names.arg = MBM$V1,las=2,cex.names=1.2,cex.lab=1.5,cex.axis=1.2,ylab="MBM (kg)",xlab="Village",las=1)

# mean bodymass by taxa
MBM_taxa<-as.data.frame(aggregate(results$poids_kg,by=list(results$Genre),mean,na.rm=TRUE))
MBM_taxa[,3]<-as.data.frame(aggregate(results$poids_kg,by=list(results$Genre),sd,na.rm=TRUE))[,2]

# rodents:ungulates = 0.422
length(subset(results$Genre,results$Genre=="Rodents")) / length(subset(results$Genre,results$Genre=="Ungulates small" | results$Genre=="Ungulates large" | results$Genre=="Ungulates medium" ))
t<-as.data.frame(table(subset(results$Genre,results$Genre=="Rodents" | results$Genre=="Ungulates small" | results$Genre=="Ungulates large" | results$Genre=="Ungulates medium"),subset(results$village,results$Genre=="Rodents" | results$Genre=="Ungulates small" | results$Genre=="Ungulates large" | results$Genre=="Ungulates medium" )))
rod_ung<-as.data.frame(matrix(NA,nrow=6,ncol=2))
rod_ung[1,2]<-t[1,3]  / (t[2,3]+t[3,3]+t[4,3])     # Bekombo = 0.279
rod_ung[2,2]<-t[5,3]  / (t[6,3]+t[7,3]+t[8,3])     # Ipope = 0.611
rod_ung[3,2]<-t[9,3]  / (t[9,3]+t[10,3]+t[11,3])   # Lompole = 0.391
rod_ung[4,2]<-t[13,3] / (t[14,3]+t[15,3]+t[16,3])  # Mbongo = 0.263
rod_ung[5,2]<-t[17,3] / (t[18,3]+t[19,3]+t[20,3])  # Mbungusani = 0.297
rod_ung[6,2]<-t[21,3] / (t[22,3]+t[23,3]+t[24,3])  # Nganda = 0.711
rod_ung[,1]<-c("V1","V2","V3","V4","V5","V6")
barplot(rod_ung$V2,col="steelblue",border=NA,names.arg = rod_ung$V1,las=1,,cex.names=1.2,cex.lab=1.5,cex.axis=1.2,ylab="rodents:ungulates",xlab="Village")

# animals by taxa: ungulates small  = 24%!
n_taxa<-as.data.frame(table(results$Genre))
n_taxa<-n_taxa[order(order_genre),]
par(mar=c(8,5,1,0.5))
barplot(n_taxa$Freq,names.arg = n_taxa$Var1,las=2,col="steelblue",border=NA,ylab="Number of individuals",cex.names=0.9)
# animals by strategy
sub<-subset(results,results$Source=="Q4")
n_strat<-as.data.frame(table(sub$Genre,sub$strategie))
n_strat$x<-rep(c("7","4","6","5","1","2","3"),5)
n_strat$y<-c(rep("a",7),rep("d",7),rep("b",7),rep("e",7),rep("c",7))
names_taxa<-c("Large ungulates","Medium ungulates","Small ungulates","Primates","Rodents","Reptiles","Other")
names_strat<-c("Bow & arrows","Gun","Snares","Dogs","Others")
palette_strat<-rep(c("olivedrab","gold2","grey30","brown1","pink3"),7)
par(mar=c(9.5,5,0.5,0.5),xpd="")
barplot(proportions(n_strat$Freq)~ n_strat$y + n_strat$x,las=2,
        beside=TRUE,space=c(0,0.5),ylim=c(0,0.25),xlim=c(0,38),
        xlab="",ylab="Proportion of individuals",
        col=palette_strat,names.arg=names_taxa,border=NA,
        cex.names=1.2,cex.lab=1.5,cex.axis=1.2)
legend(x=25,y=0.26,fill=palette_strat,legend=names_strat,
       cex=1.2,bty="n",border=NA,title="Strategy",title.cex=1.5,x.intersp = 0.5)

# animals by time of day
n_time<-as.data.frame(table(sub$Genre,sub$jour_nuit))
n_time$x<-rep(c("7","4","6","5","1","2","3"),2)
barplot(proportions(n_time$Freq) ~ n_time$Var2 + n_time$x,las=2,
        beside=TRUE,space=c(0,0.5),
        xlab="",ylab="Proportion of individuals",
        col=c("gold3","grey20"),
        ylim=c(0,0.25),cex.names=1.2,cex.axis=1.2,cex.lab=1.5,border=NA,
        names.arg=names_taxa)
legend(x=13.5,y=0.26,fill=c("gold3","grey20"),legend=c("Day", "Night"),
       cex=1.2,bty="n",border=NA,title="Time",title.cex=1.5,x.intersp = 0.5)

# animals by village
palette2<-brewer.pal(8, "Set2")
n_village<-as.data.frame(table(results$Genre,results$village))
par(mar=c(6,5,5,0.5),xpd=TRUE)
barplot(n_village$Freq ~ n_village$Var1 + n_village$Var2,las=2,xlab="",ylab="Number of individuals",col=palette2,border="grey",cex.names=0.8)
legend(x=0,y=530,fill=palette2,legend=n_village$Var1[1:8],cex=0.7,bty="n",title="Taxon",title.cex=1.1,border=NA,ncol=2)
# total biomass hunted = 9934.5kg
sum(results$poids_kg,na.rm=TRUE)
# biomass by taxa: red duikers  = 32%!
biomass_taxa<-aggregate(results$poids_kg,by=list(results$Genre),sum)
biomass_taxa<-biomass_taxa[order(-biomass_taxa$x), ]
par(mar=c(8,5,1,0.5))
barplot(proportions(biomass_taxa$x),names.arg = biomass_taxa$Group.1,las=2,cex.names=0.8,ylab="Proportion biomass",col="indianred3",border=NA,ylim=c(0,0.35))
# biomass by strategy
biomass_strat<-aggregate(results$poids_kg,by=list(results$strategie),sum)
biomass_strat<-biomass_strat[-c(1),]
biomass_strat<-biomass_strat[order(-biomass_strat$x), ]
par(mar=c(7,5,1,0.5))
barplot(proportions(biomass_strat$x),names.arg = biomass_strat$Group.1,las=2,col="indianred3",border=NA,ylim=c(0,0.4),ylab="Proportion biomass")
# biomass by time of day
biomass_time<-aggregate(results$poids_kg,by=list(results$jour_nuit),sum)
biomass_time<-biomass_time[-c(1),]
biomass_time<-biomass_time[order(-biomass_time$x), ]
par(mar=c(4,5,1,0.5))
barplot(proportions(biomass_time$x),las=1,col=c("gold3","grey20"),border=NA,xlab="Hunting",ylab="Proportion biomass",names.arg=c("Day","Night"),cex.names = 0.9)
# biomass by village
biomass_village<-aggregate(results$poids_kg,by=list(results$village),sum,na.rm=TRUE)
par(mar=c(8,5,1,0.5))
barplot(biomass_village$x,names.arg = unique(biomass_village$Group.1),las=2,col="steelblue3",border=NA,cex.names=0.9,ylab="Biomass hunted (kg)")
#biomass by time and strategies
biomass_tstra<-aggregate(results$poids_kg,by=list(results$jour_nuit,results$strategie),sum)
biomass_tstra<-biomass_tstra[-c(1),]
par(mar=c(10,5,3,0.5),xpd=TRUE)
barplot(proportions(biomass_tstra$x) ~ biomass_tstra$Group.1 + biomass_tstra$Group.2,names.arg = unique(biomass_tstra$Group.2),las=2,xlab="",ylab="Proportion biomass",col=c("gold3","grey20"),border=NA,cex.names=1.5,cex.axis=1.2,cex.lab=1.5)
legend(x=2.5,y=0.36,legend=c("Day","Night"),fill=c("gold3","grey20"),bty="n",cex=1.1,title = "Hunting",ncol=2,title.cex=1.5,border=NA)

#biomass by species and strategies
ifelse(results$strategie=="","Other",results$strategie)
sub<-subset(results,results$strategie!="")
biomass_sstra<-aggregate(sub$poids_kg,by=list(sub$Genre,sub$strategie),sum)
biomass_sstra<-biomass_sstra[-c(23:29),]

barplot(proportions(biomass_sstra$x) ~ biomass_sstra$Group.1 + biomass_sstra$Group.2,names.arg = unique(biomass_sstra$Group.2),las=2,xlab="",ylab="Proportion biomass",col=palette2,border=NA,,cex.names=1.5,cex.axis=1.2,cex.lab=1.5)
legend(x=1.1,y=0.4,fill=palette2,legend=unique(biomass_sstra$Group.1),cex=1.15,bty="n",border=NA,title="Taxon",title.cex=1.5,ncol=2,x.intersp=0.5)

#biomass by month
biomass_month<-aggregate(sub$poids_kg,by=list(sub$mois),sum)
biomass_month$Group.1<-as.numeric(biomass_month$Group.1)
biomass_month <- biomass_month[order(biomass_month$Group.1,decreasing = FALSE),]
barplot(proportions(biomass_month$x) ~ biomass_month$Group.1,names.arg = unique(biomass_month$Group.1),las=2,xlab="Month",ylab="Proportion biomass",col="grey40",border=NA,cex.names=1.5,cex.axis=1.2,cex.lab=1.5)


 # consumed biomass = 2039.8kg
sum(results$poids_kg)

# consumed biomass = 2039.8kg
sum(results$kg_conso)
# sold biomass = 7006.4kg
sum(results$kg_vente)
sum(results$kg_vente)/sum(results$poids_kg)
# gifted biomass = 461.2kg
sum(results$kg_don)
# biomass payment = 653.8kg
sum(results$kg_paim)
# biomass waste = 30.8kg
sum(results$kg_jeter)

# Plot proportions
biomass_destination<-as.data.frame(matrix(NA,nrow = 5,ncol=2))
biomass_destination[1,1]<-sum(results$kg_vente)
biomass_destination[2,1]<-sum(results$kg_conso)
biomass_destination[3,1]<-sum(results$kg_don)
biomass_destination[4,1]<-sum(results$kg_paim)
biomass_destination[5,1]<-sum(results$kg_jeter)
biomass_destination[1,2]<-"Sold"
biomass_destination[2,2]<-"Consumed"
biomass_destination[3,2]<-"Gifted"
biomass_destination[4,2]<-"Payment"
biomass_destination[5,2]<-"Wasted"
barplot(proportions(biomass_destination$V1),names.arg = biomass_destination$V2,las=2,cex.names=0.9,col="steelblue",border=NA,ylim=c(0,0.62),ylab="Proportion biomass")

## Check if species composition differ between consumed and sold
biomass_sold<-aggregate(results$kg_vente,by=list(results$Genre),sum)
biomass_consumed<-aggregate(results$kg_conso,by=list(results$Genre),sum)
test<-as.data.frame(matrix(NA,nrow=14,ncol=3))
test[,1]<-append(biomass_sold[,1],biomass_consumed[,1])
test[,2]<-append(proportions(biomass_sold[,2]),proportions(biomass_consumed[,2]))
test[1:7,3]<-"Sold"
test[8:14,3]<-"Consumed"
test$V4<-rep(order_genre,2)
par(mar=c(5,5,2,0.5),xpd=TRUE)
barplot(test$V2 ~ test$V3 + test$V4,col=palette2,,xlab="",ylab="Proportion",las=2,cex.names=1,border=NA,ylim=c(0,0.7),names.arg=names_taxa)
legend(x=0,y=0.75,legend=c("Consumed","Sold"),
       fill=palette2,bty="n",cex=0.9,title = "Destination",ncol=1,title.cex=1.1,border=NA)

df_test<-as.data.frame(cbind(test$V2[1:7]*sum(results$kg_vente),test$V2[8:14]))
par(mar=c(4,5,7,0.5),xpd=TRUE)
barplot(test$V2 ~ test$V4 + test$V3,col=palette2,,xlab="Destination",ylab="Proportion taxa",las=1,cex.names=1.5,border=NA,cex.lab=1.5)
legend(x=0.5,y=1.5,legend=unique(names_taxa),
       fill=palette2,bty="n",cex=1.1,title = "Destination",ncol=2,title.cex=1.5,border=NA)
chisq.test(df_test$V1,p=df_test$V2)

# supporting figure
test$V2<-append(biomass_sold[,2]/sum(results$poids_kg),biomass_consumed[,2]/sum(results$poids_kg))
test$V2<-ifelse(test$V3=="Sold",(test$V2)* -1,test$V2)
sold<-test[1:7,]
consumed<-test[8:14,]
dev.off()
par(mar=c(5,12,3,2))
barplot(sold$V2,beside=TRUE,col="steelblue",xlab="Proportion biomass",ylab="",las=1,cex.axis=1.5,cex.names=1.5,border=NA,cex.lab=2,horiz=TRUE,xlim=c(-0.3,0.3),names=sold$V1,xaxt="n")
barplot(consumed$V2,beside=TRUE,col="red2",xlab="",ylab="",horiz=TRUE,add=TRUE,border=NA,xaxt="n",yaxt="n")
axis(side=1,at=seq(-0.3,0.3,0.1),labels=c(0.3,0.2,0.1,"0.0",0.1,0.2,0.3),cex.axis=1.5)
abline(v=0,col="grey30",lwd=2)
mtext("Sold",side=3,line=1,adj=0.20,cex=2)
mtext("Consumed",side=3,line=1,adj=0.95,cex=2)

# Check villages that sell the most
results$prop_sold<-results$kg_vente/results$poids_kg
sold_vill<-aggregate(results$prop_sold,by=list(results$village),mean,na.rm=TRUE)
par(mar=c(6,5,4,0.5),xpd=TRUE)
barplot(sold_vill$x,las=2,names.arg = c("Bekombo","Ipope","Lompole","Mbongo","Mbungusani","Nganda"),ylab="% Trade",,cex.names=1.2,cex.lab=1.5,cex.axis=1.2,col="steelblue",border=NA,ylim=c(0,0.5))

#boxplot(results$prop_sold~results$village,las=2)
#Price/kg by different taxa
price_kg<-as.data.frame(aggregate(results$prix_vente_kg,by=list(results$Genre),mean,na.rm=TRUE))
price_kg$sd<-as.data.frame(aggregate(results$prix_vente_kg,by=list(results$Genre),sd,na.rm=TRUE))[,2]
par(mar=c(7,5,2,0.5),xpd=TRUE)
barplot(price_kg$x,las=2,ylab = "Price/Kg (CDF)",names.arg = unique(price_kg$Group.1),cex.names=0.8,col="steelblue",border=NA,cex.axis=0.9)

# correlation between MBM and price
shapiro.test(log(MBM_taxa$x)) # normal
shapiro.test(log(price_kg$x)) # normal
cor.test(log(MBM_taxa$x), log(price_kg$x),method="pearson")

# let's try with full DB

shapiro.test(log(results$poids_kg)) # not normal
shapiro.test(log(results$prix_vente_kg)) # not normal
cor.test(results$poids_kg, results$prix_vente_kg,method="spearman",exact=FALSE)
plot(results$poids_kg,results$prix_vente_kg,ylim=c(0,40000))


# Check if large ungualtes have significantly higher price
shapiro.test(subset(results$prix_vente_kg,results$Genre=="Ungulates large"))
shapiro.test(subset(results$prix_vente_kg,results$Genre!="Ungulates large"))

#ungulate large vs all other
wilcox.test(subset(results$prix_vente_kg,results$Genre=="Ungulates large"),subset(results$prix_vente_kg,results$Genre!="Ungulates large"),alternative="greater")
#ungulate large vs medium
wilcox.test(subset(results$prix_vente_kg,results$Genre=="Ungulates large"),subset(results$prix_vente_kg,results$Genre=="Ungulates medium"),alternative="greater")
#ungulate large vs small
wilcox.test(subset(results$prix_vente_kg,results$Genre=="Ungulates large"),subset(results$prix_vente_kg,results$Genre=="Ungulates small"),alternative="greater")
#ungulate large vs primates
wilcox.test(subset(results$prix_vente_kg,results$Genre=="Ungulates large"),subset(results$prix_vente_kg,results$Genre=="Primates"),alternative="greater")
#ungulate large vs rodents
wilcox.test(subset(results$prix_vente_kg,results$Genre=="Ungulates large"),subset(results$prix_vente_kg,results$Genre=="Rodents"),alternative="greater")
#ungulate large vs other
wilcox.test(subset(results$prix_vente_kg,results$Genre=="Ungulates large"),subset(results$prix_vente_kg,results$Genre=="Others"),alternative="greater")

# Calculate proportion lactating by species
lact_sub<-subset(results,results$en_allaitement>-1 )
lact_prop<-as.data.frame(aggregate(lact_sub$en_allaitement,by=list(lact_sub$Genre),mean))
barplot(lact_prop$x,names.arg = unique(lact_prop$Group.1),las=2,cex.names=0.8,ylab="Proportion lactating",col="lightgoldenrod3",border=NA)

# nursing ungulates
x<-subset(results$en_allaitement,results$en_allaitement>-1 & (results$Genre=="Ungulates medium" | results$Genre=="Ungulates large" |results$Genre=="Ungulates small"),drop=FALSE)
sum(x)/length(x)
# nursing primates
z<-subset(results$en_allaitement,results$en_allaitement>-1 & (results$Genre=="Primates"),drop=FALSE)
sum(z)/length(z)

# pregnant ungulates 
x<-subset(results$enceinte,results$enceinte>-1 & (results$Genre=="Ungulates medium" | results$Genre=="Ungulates large" |results$Genre=="Ungulates small"),drop=FALSE)
sum(x)/length(x)
# pregnant primates 
z<-subset(results$enceinte,results$enceinte>-1 & (results$Genre=="Primates"),drop=FALSE)
sum(z)/length(z)

#nursing together
x<-subset(results$en_allaitement,results$en_allaitement>-1 & (results$Genre=="Ungulates medium" | results$Genre=="Ungulates large" |results$Genre=="Ungulates small"|results$Genre=="Primates"),drop=FALSE)
sum(x)/length(x)

#pregnant together
x<-subset(results$enceinte,results$enceinte>-1 & (results$Genre=="Ungulates medium" | results$Genre=="Ungulates large" |results$Genre=="Ungulates small"|results$Genre=="Primates"),drop=FALSE)
sum(x)/length(x)

#Look at monthly trends
lact_sub_ung<-subset(results,results$en_allaitement>-1 & (results$Genre=="Ungulates medium" | results$Genre=="Ungulates large" |results$Genre=="Ungulates small"),drop=FALSE)
lact_month_ung<-as.data.frame(aggregate(lact_sub_ung$en_allaitement,by=list(as.numeric(lact_sub_ung$mois)),mean))
lact_month_ung[is.na(lact_month_ung)] <- 0

par(mar=c(3,4,1,1))
lab_month<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
x_at<-barplot(lact_month_ung$x[1:12])

#Lactating ungulates
barplot(lact_month_ung$x[1:12],names.arg =lact_month_ung$Group.1[1:12],xaxt="n",las=1,cex.axis=1.5,borde=NA,col="lightskyblue2")
axis(side=1,at=x_at,labels=lab_month,las=2,cex=1.5)

# Calculate proportion pregnant by species
preg_sub<-subset(results,results$enceinte>-1)
preg_prop<-as.data.frame(aggregate(preg_sub$enceinte,by=list(preg_sub$Genre),mean))
barplot(preg_prop$x,names.arg = unique(preg_prop$Group.1),las=2,cex.names=0.8,ylab="Proportion pregnant",col="plum4",border=NA)

# Calculate sex ratio by species
sex_sub<-subset(results,results$sexe>-1)
sex_prop<-as.data.frame(aggregate(sex_sub$sexe,by=list(sex_sub$Genre),mean))
barplot(sex_prop$x,names.arg = unique(sex_prop$Group.1),las=2,cex.names=0.8,col="bisque3",border=NA,ylab="Proportion of males")

####SORTIES #####
trips<-read.csv("Data/CPUE_sorties.csv")
trips$CPUE_distance<-as.numeric(trips$CPUE_distance)
trips$CPUE_duree<-as.numeric(trips$CPUE_duree)
library(stringr)

trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"arc&fleches","Bow & arrows")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"fusil","Gun")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"chiens","Dogs")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"machette","Machete")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"pieges","Snares")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"filet","Other")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"autre","Other")
levels(as.factor(trips$strategie_primaire))

mean(trips$CPUE_duree) #8.64 kg/day

trips_all<-read.csv("Data/hunting__trips_data.csv")
trips_all$n<-rep(1,by=length(trips_all[,1]))
trips_all$strategie_primaire<-ifelse(trips_all$strategie_primaire=="filet","autre",trips_all$strategie_primaire)
trips_all$strategie_primaire<-ifelse(trips_all$strategie_primaire=="machette","autre",trips_all$strategie_primaire)

trips1<-subset(trips_all,trips_all$strategie_primaire!="")

str_all<-aggregate(trips1$n,by=list(trips1$strategie_primaire,trips1$periode_chasse),sum,na.rm=TRUE,drop=FALSE)
str_all[is.na(str_all)] <- 0
str_all<-str_all[-c(1:6),]

str_month<-aggregate(trips1$n,by=list(trips1$strategie_primaire,trips1$mois),sum,na.rm=TRUE,drop=FALSE)
str_month[is.na(str_month)] <- 0
t_mth<-as.vector(table(trips_all$mois))
str_month$y<-rep(c("a","e","d","b","c"),12)
Z<-c(rep(t_mth[1],5),rep(t_mth[2],5),rep(t_mth[3],5),rep(t_mth[4],5),rep(t_mth[5],5),rep(t_mth[6],5)
     ,rep(t_mth[7],5),rep(t_mth[8],5),rep(t_mth[9],5),rep(t_mth[10],5),rep(t_mth[11],5),rep(t_mth[12],5))
#Plot
names_taxa<-c("Large ungulates","Medium ungulates","Small ungulates","Primates","Rodents","Reptiles","Other")
names_strat<-c("Bow & arrows","Gun","Snares","Dogs","Others")
palette_time<-rep(c("gold3","lightblue3","grey20"),12)
palette_strat<-rep(c("olivedrab","gold2","grey30","brown1","pink3"),7)


par(mar=c(4,5,2,8),xpd=TRUE)
palette_strat<-rep(c("olivedrab","gold2","grey30","brown1","pink3"),7)
barplot(str_month$x/Z ~ str_month$y + str_month$Group.2,beside=FALSE,
        col=palette_strat,border=NA,las=1,cex.axis=1.2,cex.names=1.5, cex.lab=1.5,xlab="Month",ylab="Hunting trips (n)")
legend(x=14.5,y=1,legend=names_strat,
       fill=palette_strat,bty="n",cex=1.2,title = "Strategy",ncol=1,title.cex=1.5,border=NA)





trips2<-subset(trips_all,trips_all$periode_chasse!="")
time_month<-aggregate(trips2$n,by=list(trips2$periode_chasse,trips2$mois),sum,na.rm=TRUE,drop=FALSE)
time_month[is.na(time_month)] <- 0
Y<-aggregate(trips2$n,by=list(trips2$mois),sum,na.rm=TRUE,drop=FALSE)[,2]
time_month$y<-c(rep(Y[1],3),rep(Y[2],3),rep(Y[3],3),rep(Y[4],3),rep(Y[5],3),rep(Y[6],3),
                rep(Y[7],3),rep(Y[8],3),rep(Y[9],3),rep(Y[10],3),rep(Y[11],3),rep(Y[12],3))
palette_time<-rep(c("gold3","lightblue3","grey20"),12)
par(mar=c(4,5,2,8),xpd=TRUE)
barplot(time_month$x/time_month$y ~ time_month$Group.1 + time_month$Group.2,beside=FALSE,
        col=palette_time,border=NA,las=1,cex.axis=1.2,cex.names=1.5, cex.lab=1.5,xlab="Month",ylab="Hunting trips (n)")
legend(x=14.5,y=1,legend=c("Day","Long trip","Night"),
       fill=palette_time,bty="n",cex=1.2,title = "Period",ncol=1,title.cex=1.5,border=NA)
# Check if proportion of night hunting is higher during duiker nursing period (Jun-Oct)
# create column wit 0 = day and 1 = night
trips2$night_freq<-ifelse(trips2$periode_chasse=="nuit",1,0)
trips2$BandA_freq<-ifelse(trips2$strategie_primaire=="arc&fleches",1,0)
trips2$snares_freq<-ifelse(trips2$strategie_primaire=="pieges",1,0)

night_lact<-matrix(NA,ncol=3,nrow=12)
night_lact[,1]<-seq(1,12,1)
night_lact[,2]<-subset(time_month$x,time_month$Group.1=="nuit")/subset(time_month$y,time_month$Group.1=="nuit")
night_lact[,3]<-lact_month_ung[,2]
plot(night_lact[,2],night_lact[,3],xlab="Proportion night hunting",ylab="Proportion nursing",pch=1,cex=2,col="steelblue4",lwd=2)
cor.test(night_lact[,2],night_lact[,3])
shapiro.test(night_lact[6:10,2])
shapiro.test(night_lact[c(1:5,11,12),2])
wilcox.test(night_lact[6:10,2],night_lact[c(1:5,11,12),2],alternative = "greater")
# use entire database
night_h<-subset(trips2,trips2$mois==1|trips2$mois==2|trips2$mois==3|trips2$mois==4|trips2$mois==5|trips2$mois==10|trips2$mois==11|trips2$mois==12)
night_l<-subset(trips2,trips2$mois==6|trips2$mois==7|trips2$mois==8|trips2$mois==9|trips2$mois==10)
#binomial test to test if frequency of night hunting is higher during duiker lactating period
binom.test(sum(night_l$night_freq),length(night_l$night_freq),mean(night_h$night_freq),alternative="greater")
# during nursing season, night hunting proportion is higher

# Do the same with bow & arrow
prop_BandA<-subset(str_month$x,str_month$Group.1=="arc&fleches")/unique(Z)
cor.test(prop_BandA,night_lact[,3])
test<-lm(night_lact[,3]~prop_BandA)
summary(test)
plot(prop_BandA,night_lact[,3],xlab="Proportion bow & arrows",ylab="Proportion nursing",pch=1,cex=2,col="steelblue4",lwd=2)
abline(test,col="red",lwd=2)
shapiro.test(prop_BandA[6:10])
shapiro.test(prop_BandA[c(1:5,11,12)])
t.test(prop_BandA[6:10],prop_BandA[c(1:5,11,12)],alternative = "greater")
wilcox.test(prop_BandA[6:10],prop_BandA[c(1:5,11,12)],alternative = "greater")
# boxplot
boxplot(prop_BandA[6:10],prop_BandA[c(1:5,11,12)])

# use entire database
#binomial test to test if frequency of night hunting is higher during duiker lactating period
binom.test(sum(night_l$BandA_freq),length(night_l$BandA_freq),mean(night_h$BandA_freq),alternative="greater")

# And snares
prop_snare<-subset(str_month$x,str_month$Group.1=="pieges")/unique(Z)
shapiro.test(prop_snare[6:10])
shapiro.test(prop_snare[c(1:5,11,12)])
t.test(prop_snare[6:10],prop_snare[c(1:5,11,12)],alternative = "less")
wilcox.test(prop_snare[6:10],prop_snare[c(1:5,11,12)],alternative = "less")
# use entire database
#binomial test to test if frequency of night hunting is higher during duiker lactating period
binom.test(sum(night_l$snares_freq),length(night_l$snares_freq),mean(night_h$snares_freq),alternative="less")
library(RColorBrewer)
palette<-brewer.pal(6,"YlOrRd")
par(mar=c(6.5,4,1,1))
CPUE_by_method<-as.data.frame(aggregate(trips$CPUE_duree,by=list(trips$strategie_primaire),mean))
CPUE_by_method[,3]<-as.data.frame(aggregate(trips$CPUE_duree,by=list(trips$strategie_primaire),sd))[,2]
names(CPUE_by_method)<-c("taxon","CPUE(kg/day)","SD")
CPUE_by_method<-CPUE_by_method[order(CPUE_by_method$`CPUE(kg/day)`),]
CPUE_by_method
par(mar=c(6.5,4,1,1))
barplot(CPUE_by_method$`CPUE(kg/day)`,names.arg = CPUE_by_method[,1],las=2,cex.names=0.8,ylab="CPUE (day)",col=palette,border = "grey70")

# Check if difference ibetween dogs and others is significant
shapiro.test(subset(trips$CPUE_duree,trips$strategie_primaire=="Dogs"))
shapiro.test(subset(trips$CPUE_duree,trips$strategie_primaire!="Dogs"))
wilcox.test(subset(trips$CPUE_duree,trips$strategie_primaire=="Dogs"),subset(trips$CPUE_duree,trips$strategie_primaire!="Dogs"),alternative="greater")
wilcox.test(subset(trips$CPUE_duree,trips$strategie_primaire=="Gun"),subset(trips$CPUE_duree,trips$strategie_primaire!="Gun"),alternative="greater")


#And by duree
CPUE_by_taxon<-as.data.frame(matrix(NA,nrow=9,ncol=2))
CPUE_by_taxon[,1]<-c("Ungulates small","Ungulates medium","Ungulates large","Hogs","Primates","Rodent","Reptiles","Birds","Other")
CPUE_by_taxon[1,2]<-mean(trips$CPUE_duree_petitsongules)
CPUE_by_taxon[2,2]<-mean(trips$CPUE_duree_ongulesmoyens)
CPUE_by_taxon[3,2]<-mean(trips$CPUE_duree_grandsongules)
CPUE_by_taxon[4,2]<-mean(trips$CPUE_duree_sangliers)
CPUE_by_taxon[5,2]<-mean(trips$CPUE_duree_primates)
CPUE_by_taxon[6,2]<-mean(trips$CPUE_duree_rongeurs)
CPUE_by_taxon[7,2]<-mean(trips$CPUE_duree_reptiles)
CPUE_by_taxon[8,2]<-mean(trips$CPUE_duree_oiseaux)
CPUE_by_taxon[9,2]<-mean(trips$CPUE_duree_autres)
names(CPUE_by_taxon)<-c("taxon","CPUE(kg/day)")
CPUE_by_taxon<-CPUE_by_taxon[order(CPUE_by_taxon$`CPUE(kg/day)`),]
CPUE_by_taxon
palette<-brewer.pal(9,"Blues")
barplot(CPUE_by_taxon$`CPUE(kg/day)`,names.arg = CPUE_by_taxon$taxon,las=2,cex.names=0.8,ylab="CPUE (kg/day)",col=palette,border="grey70")

CPUE_by_taxon_time<-as.data.frame(matrix(NA,nrow=27,ncol=3))
CPUE_by_taxon_time[,1]<-c(rep("Ungulates small",3),rep("Ungulates medium",3),rep("Ungulates large",3),rep("Hogs",3),rep("Primates",3),rep("Rodent",3),rep("Reptiles",3),rep("Birds",3),rep("Other",3))
CPUE_by_taxon_time[,2]<-rep(c("Day","Day & night","Night"),9)
CPUE_by_taxon_time[1:3,3]<-aggregate(trips$CPUE_duree_petitsongules,by=list(trips$periode_chasse),mean)[,2]
CPUE_by_taxon_time[4:6,3]<-aggregate(trips$CPUE_duree_ongulesmoyens,by=list(trips$periode_chasse),mean)[,2]
CPUE_by_taxon_time[7:9,3]<-aggregate(trips$CPUE_duree_grandsongules,by=list(trips$periode_chasse),mean)[,2]
CPUE_by_taxon_time[10:12,3]<-aggregate(trips$CPUE_duree_sangliers,by=list(trips$periode_chasse),mean)[,2]
CPUE_by_taxon_time[13:15,3]<-aggregate(trips$CPUE_duree_primates,by=list(trips$periode_chasse),mean)[,2]
CPUE_by_taxon_time[16:18,3]<-aggregate(trips$CPUE_duree_rongeurs,by=list(trips$periode_chasse),mean)[,2]
CPUE_by_taxon_time[19:21,3]<-aggregate(trips$CPUE_duree_reptiles,by=list(trips$periode_chasse),mean)[,2]
CPUE_by_taxon_time[22:24,3]<-aggregate(trips$CPUE_duree_oiseaux,by=list(trips$periode_chasse),mean)[,2]
CPUE_by_taxon_time[25:27,3]<-aggregate(trips$CPUE_duree_autres,by=list(trips$periode_chasse),mean)[,2]
barplot(data=CPUE_by_taxon_time, as.numeric(V3) ~ as.factor(V2) + as.factor(V1), col=c("gold3","lightblue","grey20"),border=NA,las=2,cex.names=0.8,xlab="",ylab="CPUE (kg/day)",beside=TRUE) 
legend(x=0.5,y=3.9,fill=c("gold3","lightblue","grey20"),legend=c("Day","Day&night","Night"),cex=0.8,bty="n",border=NA,title="Hunting",title.cex=1.1)

######## MODEL ######
library(stringr)
data_glm<-trips
data_glm$id_chasseur<-as.factor(data_glm$id_chasseur)
data_glm$activite_1<-as.factor(data_glm$activite_1)
data_glm$mois<-as.factor(data_glm$mois)
data_glm$strategie_primaire<-as.factor(data_glm$strategie_primaire)
data_glm$periode_chasse<-as.factor(data_glm$periode_chasse)
data_glm$type_chasse<-as.factor(data_glm$type_chasse)
data_glm$succes<-as.factor(data_glm$succes)
data_glm$temps_chasseur<-scale(data_glm$temps_chasseur)
data_glm$age_chasseur<-scale(data_glm$age_chasseur)
data_glm$duree<-scale(data_glm$duree)
data_glm$depenses_totales<-scale(data_glm$depenses_totales)
data_glm$distance_lieu_chasse_hr<-scale(data_glm$distance_lieu_chasse_hr)
data_glm$nombre_pieges<-ifelse(is.na(data_glm$nombre_pieges),0,data_glm$nombre_pieges)
# code hunting frequency into 3 categories - low / moderate / high
levels(as.factor(data_glm$frequence_chasse))
data_glm$frequence_chasse<-as.numeric(data_glm$frequence_chasse)
#data_glm$frequence_chasse_cat<-ifelse(data_glm$frequence_chasse<0.354,"low",
#                                      ifelse(data_glm$frequence_chasse>0.566,"high","moderate"))
# code hunting frequency into 2 categories - low / high (cutpoint = 0.5)
data_glm$frequence_chasse_cat<-ifelse(data_glm$frequence_chasse<0.501,"low","high")

# lump dogs with "other" strategy
data_glm$strategie_primaire<-str_replace_all(trips$strategie_primaire,"Dogs","Other")

#Then start the analyses
m1<-glm(data=data_glm, succes ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = binomial(link="logit"))
summary(m1)

library(DHARMa)
testDispersion(m1) # looks good!
#let's do an additional test
sim_m1 <- simulateResiduals(fittedModel = m1, plot = T) #no issues found
# check collinearity
library(car)
vif(m1) # Great! all values <1.2 (we'd worry if >5)

#Create a matrix to store coefficients and confidence intervals for plotting
coefficients_df<-as.data.frame(matrix(NA,nrow=(12*5)-3,ncol=5))
names(coefficients_df)<-c("model","V","coeff","Lci","Uci")
coefficients_df[1:12,1]<-"All species"
coefficients_df[1:12,2]<-c("intercept","hunter","time_hunting","frequency_low","distance",
                           "expenses","strategy_gun","strategy_other","strategy_snares","night&day","night","individual")
coefficients_df[1:12,3]<-as.vector(m1$coefficients)
coefficients_df[1:12,4]<-confint(m1)[,1]
coefficients_df[1:12,5]<-confint(m1)[,2]

# Hard to interprete - let's try the bernoulli glm by species

##### Ungulates ####

data<-subset(data_glm,data_glm$kg_petitsongules>0 | data_glm$kg_tot==0)
m_succes_ungulates<-glm(data=data, succes ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = binomial(link="logit"))
summary(m_succes_ungulates)

data<-subset(data_glm,data_glm$kg_ongulesmoyens>0 | data_glm$kg_tot==0)
m_succes_ungulatem<-glm(data=data, succes ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = binomial(link="logit"))
summary(m_succes_ungulatem)

data<-subset(data_glm,data_glm$kg_grandsongules>0 | data_glm$kg_tot==0)
m_succes_ungulateb<-glm(data=data, succes ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = binomial(link="logit"))
summary(m_succes_ungulateb)

data<-subset(data_glm,data_glm$kg_sangliers>0 | data_glm$kg_tot==0)
m_succes_hogs<-glm(data=data, succes ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = binomial(link="logit"))
summary(m_succes_hogs)

data<-subset(data_glm,data_glm$kg_grandsongules>0 | data_glm$kg_petitsongules>0 |data_glm$kg_ongulesmoyens>0 |data_glm$kg_sangliers>0 | data_glm$kg_tot==0)
m_succes_ungulates_all<-glm(data=data, succes ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = binomial(link="logit"))
summary(m_succes_ungulates_all)
coefficients_df[13:24,2]<-c("intercept","hunter","time_hunting","frequency_low","distance",
                           "expenses","strategy_gun","strategy_other","strategy_snares","night&day","night","individual")
coefficients_df[13:24,1]<-"Ungulates"
coefficients_df[13:24,3]<-m_succes_ungulates_all$coefficients
coefficients_df[13:24,4]<-confint(m_succes_ungulates_all)[,1]
coefficients_df[13:24,5]<-confint(m_succes_ungulates_all)[,2]

# Check model
testDispersion(m_succes_ungulates_all) # looks good!
#let's do an additional test
simulateResiduals(fittedModel = m_succes_ungulates_all, plot = T) #no issues found
# check collinearity
vif(m_succes_ungulates_all) # Great! all values <1.2 (we'd worry if >5)

##### Primates ####

data<-subset(data_glm,data_glm$kg_primates>0 | data_glm$kg_tot==0)
# Take out hunting strategy = snare (code as "other")
data$strategie_primaire<-ifelse(data$strategie_primaire=="Snares","Other",data$strategie_primaire)
m_succes_primates<-glm(data=data, succes ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = binomial(link="logit"))
summary(m_succes_primates)
coefficients_df[25:35,1]<-"Primates"
coefficients_df[25:35,2]<-c("intercept","hunter","time_hunting","frequency_low","distance",
                                "expenses","strategy_gun","strategy_other","night&day","night","individual")

coefficients_df[25:35,3]<-as.vector(m_succes_primates$coefficients)
coefficients_df[25:35,4]<-confint(m_succes_primates)[,1]
coefficients_df[25:35,5]<-confint(m_succes_primates)[,2]

# Check model
testDispersion(m_succes_primates) # looks good!
#let's do an additional test
simulateResiduals(fittedModel = m_succes_primates, plot = T) #no issues found
# check collinearity
vif(m_succes_primates) # Great! all values <1.2 (we'd worry if >5)

##### Rodents ####
data<-subset(data_glm,data_glm$kg_rongeurs>0 | data_glm$kg_tot==0)
# Take out hunting strategy = gun (code as "other")
data$strategie_primaire<-ifelse(data$strategie_primaire=="Gun","Other",data$strategie_primaire)
m_succes_rodents<-glm(data=data, succes ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = binomial(link="logit"))
m_succes_rodents2<-glm(data=data, succes ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse + nombre_pieges, family = binomial(link="logit"))
summary(m_succes_rodents)
summary(m_succes_rodents2)
coefficients_df[36:46,1]<-"Rodents"
coefficients_df[36:46,2]<-c("intercept","hunter","time_hunting","frequency_low","distance",
                                "expenses","strategy_other","strategy_snares","night&day","night","individual")
coefficients_df[36:46,3]<-as.vector(m_succes_rodents$coefficients)
coefficients_df[36:46,4]<-confint(m_succes_rodents)[,1]
coefficients_df[36:46,5]<-confint(m_succes_rodents)[,2]

# Check model
testDispersion(m_succes_rodents) # looks good!
#let's do an additional test
simulateResiduals(fittedModel = m_succes_rodents, plot = T) #no issues found
# check collinearity
vif(m_succes_rodents) # Great! all values <1.2 (we'd worry if >5)

##### Reptiles ####
data<-subset(data_glm,data_glm$kg_reptiles>0 | data_glm$kg_tot==0)
# Take out hunting strategy = gun (code as "other")
data$strategie_primaire<-ifelse(data$strategie_primaire=="Gun","Other",data$strategie_primaire)
m_succes_reptiles<-glm(data=data, succes ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = binomial(link="logit"))
summary(m_succes_reptiles)
coefficients_df[47:57,1]<-"Reptiles"
coefficients_df[47:57,2]<-c("intercept","hunter","time_hunting","frequency_low","distance",
                                "expenses","strategy_other","strategy_snares","night&day","night","individual")

coefficients_df[47:57,3]<-as.vector(m_succes_reptiles$coefficients)
coefficients_df[47:57,4]<-confint(m_succes_reptiles)[,1]
coefficients_df[47:57,5]<-confint(m_succes_reptiles)[,2]
# Check model
testDispersion(m_succes_reptiles) # looks good!
#let's do an additional test
simulateResiduals(fittedModel = m_succes_reptiles, plot = T) #no issues found
# check collinearity
vif(m_succes_reptiles) # Great! all values <1.4 (we'd worry if >5)


#####Plotting####
# For plotting, let's get rid of all intercepts
coefficients_df<-coefficients_df[-c(1,13,25,36,47),]
par(mar=c(4,8,1,1),xpd=FALSE)
plot(y=seq(0,10,1),x=subset(coefficients_df$coeff,coefficients_df$model=="Ungulates"),pch=16,col="darkred",
     cex=0.8,xlim=c(-4,4),ylim=c(-0.6,10.6),yaxt="n",ylab="",bty="n",xlab="Coefficients")
abline(v=0,col="grey40",lwd=2)
points(y=seq(0,10,1),x=subset(coefficients_df$coeff,coefficients_df$model=="Ungulates"),pch=16,col="darkred",cex=1)
segments(y0=seq(0,10,1),y1=seq(0,10,1),
         x0=subset(coefficients_df[,4],coefficients_df$model=="Ungulates"),
         x1=subset(coefficients_df[,5],coefficients_df$model=="Ungulates"),
         col="darkred",lwd=2)
axis(side=2,at=seq(0,10,1),labels = coefficients_df$V[1:11],cex.axis=0.8,las=1)

plot(y=seq(0,9,1),x=coefficients_df[23:32,3],pch=16,col="darkred",
     cex=0.8,xlim=c(-4,4),ylim=c(-0.6,9.6),yaxt="n",ylab="",bty="n",xlab="Coefficients",type="n")
abline(v=0,col="grey40",lwd=2)
points(y=seq(0,9,1),x=coefficients_df[23:32,3],pch=16,col="orange",cex=1)
segments(y0=seq(0,9,1),y1=seq(0,9,1),
         x0=coefficients_df[23:32,4],
         x1=coefficients_df[23:32,5],
         col="orange",lwd=2)
axis(side=2,at=seq(0,9,1),labels = coefficients_df[23:32,2],cex.axis=0.8,las=1)

plot(y=seq(0,9,1),x=subset(coefficients_df$coeff,coefficients_df$model=="Rodents"),pch=16,col="darkred",
     cex=0.8,xlim=c(-4,4),ylim=c(-0.6,9.6),yaxt="n",ylab="",bty="n",xlab="Coefficients",type="n")
abline(v=0,col="grey40",lwd=2)
points(y=seq(0,9,1),x=subset(coefficients_df$coeff,coefficients_df$model=="Rodents"),pch=16,col="steelblue",cex=1)
segments(y0=seq(0,9,1),y1=seq(0,9,1),
         x0=subset(coefficients_df[,4],coefficients_df$model=="Rodents"),
         x1=subset(coefficients_df[,5],coefficients_df$model=="Rodents"),
         col="steelblue",lwd=2)
axis(side=2,at=seq(0,9,1),labels = coefficients_df[33:42,2],cex.axis=0.8,las=1)

plot(y=seq(0,9,1),x=subset(coefficients_df$coeff,coefficients_df$model=="Reptiles"),pch=16,col="darkred",
     cex=0.8,xlim=c(-5,5),ylim=c(-0.6,10.6),yaxt="n",ylab="",bty="n",xlab="Coefficients",type="n")
abline(v=0,col="grey40",lwd=2)
points(y=seq(0,9,1),x=subset(coefficients_df$coeff,coefficients_df$model=="Reptiles"),pch=16,col="darkgreen",cex=1)
segments(y0=seq(0,9,1),y1=seq(0,9,1),
         x0=subset(coefficients_df[,4],coefficients_df$model=="Reptiles"),
         x1=subset(coefficients_df[,5],coefficients_df$model=="Reptiles"),
         col="darkgreen",lwd=2)
axis(side=2,at=seq(0,9,1),labels = coefficients_df[33:42,2],cex.axis=0.8,las=1)

# And now the Gamma glm
# Here we use CPUE duree in days - not perfect, but at least correct
data<-subset(data_glm,data_glm$CPUE_duree>0)
m3<-glm(data=data, CPUE_duree ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = Gamma(link="log"))
summary(m3)

data<-subset(data_glm,data_glm$CPUE_duree_petitsongules>0)
m_CPUE_ungulates<-glm(data=data, CPUE_duree_petitsongules ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = Gamma(link="log"),epsilon=1e7)
summary(m_CPUE_ungulates)

data<-subset(data_glm,data_glm$CPUE_duree_ongulesmoyens>0)
m_CPUE_ungulatem<-glm(data=data, CPUE_duree_ongulesmoyens ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = Gamma(link="log"))
summary(m_CPUE_ungulatem)

data<-subset(data_glm,data_glm$CPUE_duree_grandsongules>0)
m_CPUE_ungulateb<-glm(data=data, CPUE_duree_grandsongules ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = Gamma(link="log"),epsilon=1e7)
summary(m_CPUE_ungulateb)

data<-subset(data_glm,data_glm$CPUE_duree_sangliers>0)
m_CPUE_hogs<-glm(data=data, CPUE_duree_sangliers ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = Gamma(link="log"))
summary(m_CPUE_hogs)

data_glm$CPUE_duree_ongules_all<-(data_glm$kg_petitsongules+data_glm$kg_grandsongules+data_glm$kg_ongulesmoyens+data_glm$kg_sangliers)/data_glm$duree
data<-subset(data_glm,data_glm$CPUE_duree_ongules_all>0)
m_CPUE_ungulates_all<-glm(data=data, CPUE_duree_ongules_all ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = Gamma(link="log"))
summary(m_CPUE_ungulates_all)

data<-subset(data_glm,data_glm$CPUE_duree_primates>0)
m_CPUE_primates<-glm(data=data, CPUE_duree_primates ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, family = Gamma(link="log"))
summary(m_CPUE_primates)

data<-subset(data_glm,data_glm$CPUE_duree_rongeurs>0)
m_CPUE_rodents<-glm(data=data, CPUE_duree_rongeurs ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + nombre_pieges + periode_chasse, family = Gamma(link="log"))
summary(m_CPUE_rodents)

data<-subset(data_glm,data_glm$CPUE_duree_reptiles>0)
m_CPUE_reptiles<-glm(data=data, CPUE_duree_reptiles ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr 
                     + depenses_totales + strategie_primaire  + periode_chasse + type_chasse, family = Gamma(link="log"),epsilon=1e7)
summary(m_CPUE_reptiles)

#Finally we try strategy-specific models - success first
levels(as.factor(data_glm$strategie_primaire))
data<-subset(data_glm,data_glm$strategie_primaire=="Bow & arrows")
m_succes_bow<-glm(data=data, succes ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + periode_chasse + type_chasse, family = binomial(link="logit"))
summary(m_succes_bow)

data<-subset(data_glm,data_glm$strategie_primaire=="Gun")
m_succes_gun<-glm(data=data, succes ~ activite_1 + temps_chasseur + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + periode_chasse + type_chasse, family = binomial(link="logit"))
summary(m_succes_gun)

data<-subset(data_glm,data_glm$strategie_primaire=="Snares")
m_succes_snare<-glm(data=data, succes ~ activite_1 + temps_chasseur + nombre_pieges + frequence_chasse_cat + distance_lieu_chasse_hr + depenses_totales + periode_chasse + type_chasse, family = binomial(link="logit"))
summary(m_succes_snare)
str(data_glm)
