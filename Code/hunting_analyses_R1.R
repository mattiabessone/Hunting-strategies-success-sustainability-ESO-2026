setwd("~/GitHub/PPP-Wildlife-Hunting-Analyses")
# Additional analyses - Major revisions
results<-read.csv("Data/results_all.csv")
trips<-read.csv("Data/CPUE_sorties.csv")
trips_all<-read.csv("Data/hunting__trips_data.csv")
#### Organise data ####
library(stringr)
#### Results
results$Genre<-ifelse(results$Genre=="pangolins" | results$Genre=="bird"| results$Genre=="carnivore"| results$Genre=="autre","Others",results$Genre)
results$sexe<-ifelse(results$sexe=="male",1,ifelse(results$sexe=="femelle",0,-1))
results$en_allaitement<-ifelse(results$en_allaitement=="oui",1,ifelse(results$en_allaitement=="non",0,-1))
results$enceinte<-ifelse(results$enceinte=="oui",1,ifelse(results$enceinte=="non",0,-1))
# Rename taxa & strategy
levels(as.factor(results$Genre))
results$Genre<-str_replace_all(results$Genre,"ungulate_small","Ungulates small")
results$Genre<-str_replace_all(results$Genre,"ungulate_medium","Ungulates medium")
results$Genre<-str_replace_all(results$Genre,"ungulate_large","Ungulates large")
results$Genre<-str_replace_all(results$Genre,"primates","Primates")
results$Genre<-str_replace_all(results$Genre,"hogs","Ungulates large")
results$Genre<-str_replace_all(results$Genre,"rodent","Rodents")
results$Genre<-str_replace_all(results$Genre,"reptiles","Reptiles")
results$Genre<-str_replace_all(results$Genre,"others","Others")
ifelse(results$strategie=="","Other",results$strategie)
results$strategie<-str_replace_all(results$strategie,"arc&fleches","Bow & arrows")
results$strategie<-str_replace_all(results$strategie,"fusil","Gun")
results$strategie<-str_replace_all(results$strategie,"chiens","Dogs")
results$strategie<-str_replace_all(results$strategie,"machette","Other")
results$strategie<-str_replace_all(results$strategie,"pieges","Snares")
results$strategie<-str_replace_all(results$strategie,"filets","Other")
results$strategie<-str_replace_all(results$strategie,"autres","Other")
# Remove rows with no animal recorded (Genre = "Aucun")
results<-subset(results,results$espece!="Aucun")
#### Trips
trips$CPUE_distance<-as.numeric(trips$CPUE_distance)
trips$CPUE_duree<-as.numeric(trips$CPUE_duree)
# Recode as primary strategy
# Interviews
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"arc&fleches","Bow & arrows")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"fusil","Gun")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"chiens","Dogs")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"machette","Machete")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"pieges","Snares")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"filet","Other")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"autre","Other")
# Focal points
trips_all$n<-rep(1,by=length(trips_all[,1]))
trips_all$strategie_primaire<-ifelse(trips_all$strategie_primaire=="filet","autre",trips_all$strategie_primaire)
trips_all$strategie_primaire<-ifelse(trips_all$strategie_primaire=="machette","autre",trips_all$strategie_primaire)
trips1<-subset(trips_all,trips_all$strategie_primaire!="")

#### Comment 2&3 ####
# Re-define strategy 
d<-trips1
d$periode_chasse<-ifelse(d$periode_chasse=="les_deux" & d$strategie_primaire=="arc&fleches" ,"nuit",
                         ifelse(d$periode_chasse=="les_deux" & d$strategie_primaire=="pieges","jour",
                                ifelse(d$periode_chasse=="les_deux" & d$strategie_primaire=="fusil","jour",
                                       ifelse(d$periode_chasse=="les_deux" & d$strategie_primaire=="chiens","jour",
                                              ifelse(d$periode_chasse=="les_deux" & d$strategie_primaire=="autres","nuit",d$periode_chasse)))))
d$strat_revised<-paste(d$strategie_primaire,d$type_chasse,d$periode_chasse,sep="_")

unique(d$strat_revised)

# Allocate entries without time of day
d$strat_revised<-ifelse(d$strat_revised=="pieges_individuelle_","pieges_individuelle_jour",
                 ifelse(d$strat_revised=="pieges_collective_","pieges_collective_jour",
                 ifelse(d$strat_revised=="fusil_individuelle_","fusil_individuelle_jour",
                 ifelse(d$strat_revised=="fusil_collective_","fusil_collective_jour",
                 ifelse(d$strat_revised=="arc&fleches_collective_","arc&fleches_collective_nuit",
                 ifelse(d$strat_revised=="arc&fleches_individuelle_","arc&fleches_individuelle_jour",
                 ifelse(d$strat_revised=="pieges_NA_jour","pieges_individuelle_jour",d$strat_revised)))))))
levels(as.factor(d$strat_revised))
# Tabulate
strat_revised<-as.data.frame(table(d$strat_revised)) # 16 stategies
strat_revised<-strat_revised[order(-strat_revised$Freq),]
strat_revised[,3]<-round(proportions(strat_revised$Freq)*100,2)
strat_revised

write.csv(strat_revised,"Results/strategy_revised.csv")

# Ignore composite strategy except in the case of bow and arrow
d$strat_revised2<-ifelse(d$strategie_primaire=="arc&fleches",d$strat_revised,d$strategie_primaire)
d$strat_revised2<-ifelse(d$strat_revised2=="arc&fleches_collective_jour","arc&fleches_individuelle_jour",d$strat_revised2)
strat_revised2<-as.data.frame(table(d$strat_revised2))
strat_revised2[,3]<-round(proportions(strat_revised2$Freq)*100,2)
strat_revised2
write.csv(strat_revised2,"Results/strategy_revised2.csv")
#### Calculate CPUE for most common strategies
# In this case we only use data from interviews we are sure of outcome
d<-trips
d$periode_chasse<-ifelse(d$periode_chasse=="les_deux" & d$strategie_primaire=="Bow & arrows" ,"nuit",
                         ifelse(d$periode_chasse=="les_deux" & d$strategie_primaire=="Snares","jour",
                                ifelse(d$periode_chasse=="les_deux" & d$strategie_primaire=="Gun","jour",
                                       ifelse(d$periode_chasse=="les_deux" & d$strategie_primaire=="Dogs","jour",
                                              ifelse(d$periode_chasse=="les_deux" & d$strategie_primaire=="Other","nuit",d$periode_chasse)))))
d$strat_revised<-ifelse(d$strategie_primaire=="Bow & arrows" & d$type_chasse=="individuelle" & d$periode_chasse =="jour","Bow&arrow_day_individual",
                            ifelse(d$strategie_primaire=="Bow & arrows" & d$type_chasse=="individuelle" & d$periode_chasse =="nuit","Bow&arrow_night_individual",
                                   ifelse(d$strategie_primaire=="Bow & arrows" & d$type_chasse=="collective" & d$periode_chasse =="nuit","Bow&arrow_night_collective",d$strategie_primaire)))
d$strat_revised<-ifelse(d$strat_revised=="Bow & arrows","Bow&arrow_day_individual",d$strat_revised)
d$strat_revised<-ifelse(d$strat_revised=="Bow&arrow_night_individual" & d$CPUE_duree_primates>0,"Bow&arrow_day_individual",d$strat_revised)                         



for(i in 1:nrow(d)) d$CPUE_duree_other[i]<-sum(d$CPUE_duree_oiseaux[i],d$CPUE_duree_autres[i])
CPUE_revised<-as.data.frame(table(d$strat_revised)) # 16 stategies
CPUE_revised

CPUE_revised[,3]<-aggregate(d$CPUE_duree,by=list(d$strat_revised),mean)[,2]
CPUE_revised[,4]<-aggregate(d$CPUE_duree_petitsongules,by=list(d$strat_revised),mean)[,2]
CPUE_revised[,5]<-aggregate(d$CPUE_duree_ongulesmoyens,by=list(d$strat_revised),mean)[,2]
CPUE_revised[,6]<-aggregate(d$CPUE_duree_grandsongules,by=list(d$strat_revised),mean)[,2]
CPUE_revised[,7]<-aggregate(d$CPUE_duree_primates,by=list(d$strat_revised),mean)[,2]
CPUE_revised[,8]<-aggregate(d$CPUE_duree_rongeurs,by=list(d$strat_revised),mean)[,2]
CPUE_revised[,9]<-aggregate(d$CPUE_duree_reptiles,by=list(d$strat_revised),mean)[,2]
CPUE_revised[,10]<-aggregate(d$CPUE_duree_other,by=list(d$strat_revised),mean)[,2]

CPUE_revised[,11]<-aggregate(d$CPUE_duree,by=list(d$strat_revised),sd)[,2]
CPUE_revised[,12]<-aggregate(d$CPUE_duree_petitsongules,by=list(d$strat_revised),sd)[,2]
CPUE_revised[,13]<-aggregate(d$CPUE_duree_ongulesmoyens,by=list(d$strat_revised),sd)[,2]
CPUE_revised[,14]<-aggregate(d$CPUE_duree_grandsongules,by=list(d$strat_revised),sd)[,2]
CPUE_revised[,15]<-aggregate(d$CPUE_duree_primates,by=list(d$strat_revised),sd)[,2]
CPUE_revised[,16]<-aggregate(d$CPUE_duree_rongeurs,by=list(d$strat_revised),sd)[,2]
CPUE_revised[,17]<-aggregate(d$CPUE_duree_reptiles,by=list(d$strat_revised),sd)[,2]
CPUE_revised[,18]<-aggregate(d$CPUE_duree_other,by=list(d$strat_revised),sd)[,2]

CPUE_revised<-CPUE_revised[order(-CPUE_revised$Freq),]
names(CPUE_revised)<-c("Strategy","n","CPUE_all","small_ung","med_ung","large_ung","primates","rodents","reptiles","other",
                       "CPUE_all_sd","small_ung_sd","med_ung_sd","large_ung_sd","primates_sd","rodents_sd","reptiles_sd","other_sd")
CPUE_revised

write.csv(CPUE_revised,"Results/CPUE_strat.csv")

d$strat_revised2<-ifelse(d$strat_revised=="Bow&arrow_night_collective" | d$strat_revised=="Bow&arrow_night_individual","Bow&arrow_night",d$strat_revised)

mean(d$CPUE_duree[d$strat_revised2=="Bow&arrow_night"])
mean(d$CPUE_duree[d$strat_revised=="Bow&arrow_day_individual"])
mean(d$CPUE_duree[d$strat_revised=="Bow&arrow_night_individual"])
mean(d$CPUE_duree[d$strat_revised=="Bow&arrow_night_collective"])

wilcox.test(d$CPUE_duree[d$strat_revised2=="Bow&arrow_night"],d$CPUE_duree[d$strat_revised2=="Bow&arrow_day_individual"])
wilcox.test(d$CPUE_duree[d$strat_revised=="Bow&arrow_night_individual"],d$CPUE_duree[d$strat_revised=="Bow&arrow_day_individual"])
wilcox.test(d$CPUE_duree[d$strat_revised=="Bow&arrow_night_collective"],d$CPUE_duree[d$strat_revised=="Bow&arrow_day_individual"])

wilcox.test(d$CPUE_duree[d$strat_revised=="Bow&arrow_night_collective"],d$CPUE_duree[d$strat_revised=="Snares"])
wilcox.test(d$CPUE_duree[d$strat_revised=="Bow&arrow_night_collective"],d$CPUE_duree[d$strat_revised=="Gun"])

wilcox.test(d$CPUE_duree[d$strat_revised=="Gun"],d$CPUE_duree[d$strat_revised=="Bow&arrow_night_individual"])
wilcox.test(d$CPUE_duree[d$strat_revised=="Gun"],d$CPUE_duree[d$strat_revised=="Bow&arrow_day_individual"])
wilcox.test(d$CPUE_duree[d$strat_revised=="Gun"],d$CPUE_duree[d$strat_revised=="Snares"])
wilcox.test(d$CPUE_duree[d$strat_revised=="Gun"],d$CPUE_duree[d$strat_revised=="Other"])

wilcox.test(d$CPUE_duree[d$strat_revised=="Dogs"],d$CPUE_duree[d$strat_revised=="Gun"])
wilcox.test(d$CPUE_duree[d$strat_revised=="Dogs"],d$CPUE_duree[d$strat_revised=="Snares"])
wilcox.test(d$CPUE_duree[d$strat_revised=="Dogs"],d$CPUE_duree[d$strat_revised=="Bow&arrow_night_collective"])
wilcox.test(d$CPUE_duree[d$strat_revised=="Dogs"],d$CPUE_duree[d$strat_revised=="Bow&arrow_night_individual"])
wilcox.test(d$CPUE_duree[d$strat_revised=="Dogs"],d$CPUE_duree[d$strat_revised=="Bow&arrow_day_individual"])
wilcox.test(d$CPUE_duree[d$strat_revised=="Dogs"],d$CPUE_duree[d$strat_revised=="Other"])

#### Comment 5 ####
# Targeted vs opportunistic hunts 

#### Comment 8 ####
# Hunter profiles
hunters<-read.csv("Data/hunters_general.csv")
hunters<-hunters[,c(5,7,8,12,13,16,19,25,35,39,45,47,49,56,58,60,66,67,69,77,79,91,97,99,113)+1]
str(hunters)
hunters$bow<-paste(hunters$temps_strategie_arcfleches,hunters$type_chasse_arcfleches,sep="_")
hunters$gun<-paste(hunters$temps_strategie_fusil,hunters$type_chasse_fusil,sep="_")
hunters$snare<-paste(hunters$temps_strategie_pieges,hunters$type_chasse_pieges,sep="_")
hunters$mach<-paste(hunters$temps_strategie_machette,hunters$type_chasse_machette,sep="_")
hunters$dog<-paste(hunters$temps_strategie_chiens,hunters$type_chasse_chiens,sep="_")

hunters$hunted_for<-ifelse(hunters$temps_chasseur<6,1,
                    ifelse(hunters$temps_chasseur>30,4,
                    ifelse(hunters$temps_chasseur>5 & hunters$temps_chasseur<16,2,3)))

table(hunters$hunted_for)
table(hunters$village)
proportions(table(hunters$bow,hunters$hunted_for),margin=1)
hunters$time_arcfleches<-ifelse(hunters$temps_strategie_arcfleches=="journuit","nuit",hunters$temps_strategie_arcfleches)
table(hunters$time_arcfleches,hunters$hunted_for)
proportions(table(hunters$time_arcfleches,hunters$hunted_for),margin=2)

hunters$type_arcfleches<-ifelse(hunters$type_chasse_arcfleches=="individuelle_collective","collective",hunters$type_chasse_arcfleches)
table(hunters$type_arcfleches,hunters$hunted_for)
proportions(table(hunters$type_arcfleches,hunters$hunted_for),margin=2)

hunters$bowarrow<-paste(hunters$time_arcfleches,hunters$type_arcfleches,sep="_")
hunters$bowarrow<-ifelse(hunters$bowarrow=="jour_collective","jour_individuelle",hunters$bowarrow)
table(hunters$bowarrow,hunters$hunted_for)
proportions(table(hunters$bowarrow,hunters$hunted_for),margin=2)

table(hunters$bowarrow,hunters$niveau_etudes)
proportions(table(hunters$bowarrow,hunters$niveau_etudes),margin=2)

hunters$act<-ifelse(hunters$activite_1=="commerce" | hunters$activite_1=="salarie","salarie",hunters$activite_1)
hunters$act<-ifelse(hunters$activite_1=="peche","autre",hunters$act)
table(hunters$bowarrow,hunters$act)
proportions(table(hunters$bowarrow,hunters$act),margin=2)

hunter_age<-read.csv("Data/profil_chas2.csv")
hunter_age$experience<-ifelse(hunter_age$temps_chasseur<6,1,
                              ifelse(hunter_age$temps_chasseur>30,4,
                                     ifelse(hunter_age$temps_chasseur>5 & hunter_age$temps_chasseur<16,2,3)))

table_hunt_age<-aggregate(hunter_age$Age,by=list(hunter_age$niveau_etudes),median)
table_hunt_age$min<-aggregate(hunter_age$Age,by=list(hunter_age$niveau_etudes),min)[,2]
table_hunt_age$max<-aggregate(hunter_age$Age,by=list(hunter_age$niveau_etudes),max)[,2]

temp<-aggregate(hunter_age$Age,by=list(hunter_age$experience),median)
temp$min<-aggregate(hunter_age$Age,by=list(hunter_age$experience),min)[,2]
temp$max<-aggregate(hunter_age$Age,by=list(hunter_age$experience),max)[,2]

table_hunt_age<-rbind(table_hunt_age,temp)

temp<-aggregate(hunter_age$Age,by=list(hunter_age$activite_1),median)
temp$min<-aggregate(hunter_age$Age,by=list(hunter_age$activite_1),min)[,2]
temp$max<-aggregate(hunter_age$Age,by=list(hunter_age$activite_1),max)[,2]

table_hunt_age<-rbind(table_hunt_age,temp)
table_hunt_age
rm(temp)
write.csv(table_hunt_age,"Results/table_hunter_age.csv")

#### Comment 14 ####
# proportion trade per speceis
trade_species<-aggregate(results$kg_vente,by=list(results$espece),sum)
write.csv(trade_species,"Results/traded_species.csv")          

#### Comment 15 ####
# Economic efficiency
for(i in 1:nrow(d)){
  d$depenses_tot[i] = sum(d[i,23:27])
}
lm1<-lm(d$Kg_Tot_Ani ~ d$depenses_tot)
summary(lm1)
plot(d$Kg_Tot_Ani ~ d$depenses_tot)
abline(lm1,lwd=1.5)

lm2<-lm(d$Kg_Tot_Ani ~ d$depenses_tot*d$strat_revised)
summary(lm2)

offtake_strat<-aggregate(d$Kg_Tot_Ani,by=list(d$strat_revised),mean,na.rm=TRUE)
expenses_strat<-aggregate(d$depenses_tot,by=list(d$strat_revised),mean,na.rm=TRUE)
par(mar=c(8,3,2,1))
barplot(offtake_strat$x,names.arg = offtake_strat$Group.1,las=2,cex.names = 0.6)
barplot(expenses_strat$x,names.arg = expenses_strat$Group.1,las=2,cex.names = 0.6)

plot(offtake_strat$x ~ expenses_strat$x)

d$strat_revised2<-paste(d$strategie_primaire,d$periode_chasse,sep="_")
levels(as.factor(d$strat_revised2))
d$strat_revised2<-ifelse(d$strat_revised2=="fusil_","fusil_jour",
                  ifelse(d$strat_revised2=="pieges_","pieges_jour",
                  ifelse(d$strat_revised2=="arc&fleches_","arc&fleches_nuit",d$strat_revised2)))
levels(as.factor(d$strat_revised2))

offtake_strat2<-aggregate(d$Kg_Tot_Ani,by=list(d$strat_revised2),mean,na.rm=TRUE)
expenses_strat2<-aggregate(d$depenses_tot,by=list(d$strat_revised2),mean,na.rm=TRUE)

barplot(offtake_strat2$x,names.arg = offtake_strat2$Group.1,las=2,cex.names = 0.6)
barplot(expenses_strat2$x,names.arg = expenses_strat2$Group.1,las=2,cex.names = 0.6)

offtake_strat3<-aggregate(d$Kg_Tot_Ani,by=list(d$strategie_primaire),mean,na.rm=TRUE)
expenses_strat3<-aggregate(d$depenses_tot,by=list(d$strategie_primaire),mean,na.rm=TRUE)
aggregate(d$depenses_tot,by=list(d$strategie_primaire),sd,na.rm=TRUE)

barplot(offtake_strat3$x,names.arg = offtake_strat3$Group.1,las=2,cex.names = 0.6)
barplot(expenses_strat3$x,names.arg = expenses_strat3$Group.1,las=2,cex.names = 0.6)

wilcox.test(d$depenses_tot[d$strategie_primaire=="pieges"],
            d$depenses_tot[d$strategie_primaire=="fusil"],alternative="g")
wilcox.test(d$depenses_tot[d$strategie_primaire=="pieges"],
            d$depenses_tot[d$strategie_primaire=="arc&fleches"],alternative="g")

# Look specifically at bow and arrow
aggregate(d$CPUE_kg_all_duree,by=list(d$strat_revised),mean)
aggregate(d$CPUE_kg_all_duree,by=list(d$strat_revised),mean)

wilcox.test(d$CPUE_kg_all_duree[d$strat_revised=="arc&fleches_individuelle_jour"],
            d$CPUE_kg_all_duree[d$strat_revised2=="arc&fleches_nuit"])
wilcox.test(d$CPUE_kg_all_duree[d$strat_revised=="arc&fleches_individuelle_nuit"],
            d$CPUE_kg_all_duree[d$strat_revised=="arc&fleches_collective_nuit"])



# look at trade
results$prop_sold<-results$kg_vente/results$poids_kg
trade_strat<-aggregate(results$prop_sold,by=list(results$strategie),mean)
trade_strat
results$revised_strat<-paste(results$strategie,results$jour_nuit,sep="_")
levels(as.factor(d$strat_revised2))
trade_strat2<-aggregate(results$prop_sold,by=list(results$revised_strat),mean)
trade_strat2

#### Comment 16 ####
# hunter biomass/trade
trips1$hunted_for<-ifelse(trips1$temps_chasseur<6,1,
                           ifelse(trips1$temps_chasseur>30,4,
                                  ifelse(trips1$temps_chasseur>5 & trips1$temps_chasseur<16,2,3)))

prolific_h<-aggregate(trips1$Kg_Tot_Ani,by=list(trips1$unique_hunter_ID),sum)
prolific_h<-prolific_h[order(-prolific_h$x),]
prolific_h
plot(hist(prolific_h$x[2:nrow(prolific_h)]),col="tomato2",xlab="Extracted biomass (kg)",las=1,main="")

prolific_h_mean<-aggregate(trips1$Kg_Tot_Ani,by=list(trips1$unique_hunter_ID),mean)
prolific_h_mean<-prolific_h_mean[order(-prolific_h_mean$x),]
prolific_h_mean
plot(hist(prolific_h_mean$x[2:nrow(prolific_h_mean)]),col="tomato2",xlab="Extracted biomass (kg) / trip",las=1,main="")


aggregate(trips1$Kg_Tot_Ani,by=list(trips1$hunted_for),mean)
aggregate(trips1$Kg_Tot_Ani,by=list(trips1$niveau_etudes),mean)
aggregate(trips1$Kg_Tot_Ani,by=list(trips1$niveau_etudes),mean)
aggregate(trips1$Kg_Tot_Ani,by=list(trips1$activite_1),mean)
aggregate(trips1$Kg_Tot_Ani,by=list(trips1$periode_chasse),mean)

wilcox.test(trips1$Kg_Tot_Ani[trips1$periode_chasse=="nuit"],trips1$Kg_Tot_Ani[trips1$periode_chasse=="jour"])

prolific_h_strat<-subset(trips1,trips1$unique_hunter_ID=="mbungusani_1" |
                           trips1$unique_hunter_ID=="mbungusani_2" |
                           trips1$unique_hunter_ID=="mbungusani_3" |
                           trips1$unique_hunter_ID=="mbungusani_21" |
                           trips1$unique_hunter_ID=="mbongo_6" |
                           trips1$unique_hunter_ID=="ipope_1" |
                           trips1$unique_hunter_ID=="ipope_3" |
                           trips1$unique_hunter_ID=="bekombo_10" |
                           trips1$unique_hunter_ID=="bekombo_6" |
                           trips1$unique_hunter_ID=="lompole_24")

top_hunters_mean<-aggregate(prolific_h_strat$Kg_Tot_Ani,by=list(prolific_h_strat$unique_hunter_ID,prolific_h_strat$strategie_primaire),mean)
top_hunters_tot<-aggregate(prolific_h_strat$Kg_Tot_Ani,by=list(prolific_h_strat$unique_hunter_ID,prolific_h_strat$strategie_primaire),sum)
top_hunters_tot<-top_hunters_tot[order(top_hunters_tot$Group.1),]
top_hunters_mean<-top_hunters_mean[order(top_hunters_mean$Group.1),]
top_hunters_mean
sub_trade<-subset(results,results$Source=="Q4")
prolific_h_trade<-subset(sub_trade,sub_trade$unique_hunter_ID=="mbungusani_1" |
                           sub_trade$unique_hunter_ID=="mbungusani_2" |
                           sub_trade$unique_hunter_ID=="mbungusani_3" |
                           sub_trade$unique_hunter_ID=="mbungusani_21" |
                           sub_trade$unique_hunter_ID=="mbongo_6" |
                           sub_trade$unique_hunter_ID=="ipope_1" |
                           sub_trade$unique_hunter_ID=="ipope_3" |
                           sub_trade$unique_hunter_ID=="bekombo_10" |
                           sub_trade$unique_hunter_ID=="bekombo_6" |
                           sub_trade$unique_hunter_ID=="lompole_24")
other_h_trade<-subset(sub_trade,sub_trade$unique_hunter_ID!="mbungusani_1" |
                           sub_trade$unique_hunter_ID!="mbungusani_2" |
                           sub_trade$unique_hunter_ID!="mbungusani_3" |
                           sub_trade$unique_hunter_ID!="mbungusani_21" |
                           sub_trade$unique_hunter_ID!="mbongo_6" |
                           sub_trade$unique_hunter_ID!="ipope_1" |
                           sub_trade$unique_hunter_ID!="ipope_3" |
                           sub_trade$unique_hunter_ID!="bekombo_10" |
                           sub_trade$unique_hunter_ID!="bekombo_6" |
                           sub_trade$unique_hunter_ID!="lompole_24")
mean(prolific_h_trade$prop_sold)
mean(other_h_trade$prop_sold)
mean(other_h_trade$kg_vente)
mean(prolific_h_trade$kg_vente)

wilcox.test(prolific_h_trade$prop_sold,other_h_trade$prop_sold,alternative = "greater")
wilcox.test(prolific_h_trade$kg_vente,other_h_trade$kg_vente,alternative = "greater")

sum(prolific_h_trade$Dest_vente)
sum(other_h_trade$Dest_vente)

### Does hunting success increae with numebr of people?#
library(mgcv)
m1<-gam(trips$succes ~ s(trips$nombre_personnes),family=binomial)

