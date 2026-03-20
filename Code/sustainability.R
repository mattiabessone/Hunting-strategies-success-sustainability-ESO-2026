# Extracted biomass per area per year
sum(results$poids_kg[results$village=="1"|results$village=="3"|results$village=="5"]) / 272.5

# Rodent:ungualte index
length(subset(results$Genre,results$Genre=="Rodents")) / length(subset(results$Genre,results$Genre=="Ungulates small" | results$Genre=="Ungulates large" | results$Genre=="Ungulates medium" ))
# by village
t<-as.data.frame(table(subset(results$Genre,results$Genre=="Rodents" | results$Genre=="Ungulates small" | results$Genre=="Ungulates large" | results$Genre=="Ungulates medium"),subset(results$village,results$Genre=="Rodents" | results$Genre=="Ungulates small" | results$Genre=="Ungulates large" | results$Genre=="Ungulates medium" )))
rod_ung<-as.data.frame(matrix(NA,nrow=6,ncol=2))
rod_ung[1,2]<-t[1,3]  / (t[2,3]+t[3,3]+t[4,3]) # village 1 
rod_ung[2,2]<-t[5,3]  / (t[6,3]+t[7,3]+t[8,3]) # village 2
rod_ung[3,2]<-t[9,3]  / (t[9,3]+t[10,3]+t[11,3]) # village 3
rod_ung[4,2]<-t[13,3] / (t[14,3]+t[15,3]+t[16,3]) # village 4
rod_ung[5,2]<-t[17,3] / (t[18,3]+t[19,3]+t[20,3]) # village 5
rod_ung[6,2]<-t[21,3] / (t[22,3]+t[23,3]+t[24,3]) # village 6
rod_ung[,1]<-c("V1","V2","V3","V4","V5","V6")
rod_ung

# Mean body mass (MBM)
# mean body mass of hunted species = 5.80
mean(results$poids_kg,na.rm=TRUE)
sd(results$poids_kg,na.rm=TRUE)
MBM<-as.data.frame(matrix(NA,nrow=6,ncol=2))
# By village
MBM[1,2]<-mean(results$poids_kg[results$village==1],na.rm=TRUE)
MBM[2,2]<-mean(results$poids_kg[results$village==2],na.rm=TRUE)
MBM[3,2]<-mean(results$poids_kg[results$village==3],na.rm=TRUE) 
MBM[4,2]<-mean(results$poids_kg[results$village==4],na.rm=TRUE)
MBM[5,2]<-mean(results$poids_kg[results$village==5],na.rm=TRUE)
MBM[6,2]<-mean(results$poids_kg[results$village==6],na.rm=TRUE)
MBM[,1]<-c("V1","V2","V3","V4","V5","V6")
MBM

# Proportion of sale
sum(results$kg_vente)/sum(results$poids_kg)

# Reproductive indices
# Calculate proportions of lactating adult female for which reproductive state was recorded
lact_sub<-subset(results,results$en_allaitement>-1 & results$age=="adulte")
# Ungulates
lact_sub_ung<-subset(results,results$en_allaitement>-1 & (results$Genre=="Ungulates medium" | results$Genre=="Ungulates large" |results$Genre=="Ungulates small"),drop=FALSE)
lact_month_ung<-as.data.frame(aggregate(lact_sub_ung$en_allaitement,by=list(as.numeric(lact_sub_ung$mois)),mean))
lact_month_ung[is.na(lact_month_ung)] <- 0
# Primates
lact_sub_prim<-subset(results,results$en_allaitement>-1 & (results$Genre=="Primates"),drop=FALSE)
lact_month_prim<-as.data.frame(aggregate(lact_sub_prim$en_allaitement,by=list(as.numeric(lact_sub_prim$mois)),mean))
lact_month_prim[is.na(lact_month_prim)] <- 0
rm(lact_sub_ung,lact_sub_prim)
# Calculate proportions of pregnant adult female for which reproductive state was recorded
preg_sub<-subset(results,results$enceinte>-1 & results$age=="adulte")
# Ungulates
preg_sub_ung<-subset(results,results$enceinte>-1 & (results$Genre=="Ungulates medium" | results$Genre=="Ungulates large" |results$Genre=="Ungulates small"),drop=FALSE)
preg_month_ung<-as.data.frame(aggregate(preg_sub_ung$enceinte,by=list(as.numeric(preg_sub_ung$mois)),mean))
preg_month_ung[is.na(preg_month_ung)] <- 0
# Primates
preg_sub_prim<-subset(results,results$enceinte>-1 & (results$Genre=="Primates"),drop=FALSE)
preg_month_prim<-as.data.frame(aggregate(preg_sub_prim$enceinte,by=list(as.numeric(preg_sub_prim$mois)),mean))
preg_month_prim[is.na(preg_month_prim)] <- 0
rm(preg_sub_ung,preg_sub_prim)

# Plot Fig. 2
source("Code/Fig_2.R")
