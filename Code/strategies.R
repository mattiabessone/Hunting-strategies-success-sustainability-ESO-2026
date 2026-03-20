# Frequency of use
strat_revised<-as.data.frame(table(trips1$strat_revised)) # 16 stategies
strat_revised<-strat_revised[order(-strat_revised$Freq),]
strat_revised[,3]<-round(proportions(strat_revised$Freq)*100,2)
strat_revised
# Frequency of use considering only 8 stategy
strat_revised2<-as.data.frame(table(trips1$strat_revised2))
strat_revised2[,3]<-round(proportions(strat_revised2$Freq)*100,2)
strat_revised2
# Frequency hunting tools
table(trips1$strategie_primaire)
proportions(table(trips1$strategie_primaire))
# Frequency night vs day hunting
table(trips1$periode_chasse)
proportions(table(trips1$periode_chasse))
# Frequency night vs day hunting by strategy
table(trips1$periode_chasse,trips1$strategie_primaire)
proportions(table(trips1$periode_chasse,trips1$strategie_primaire),margin=2)
# Check if proportion of night hunting is higher during duiker nursing period (Jun-Oct)
night_h<-subset(trips2,trips2$mois==1|trips2$mois==2|trips2$mois==3|trips2$mois==4|trips2$mois==5|trips2$mois==10|trips2$mois==11|trips2$mois==12)
night_l<-subset(trips2,trips2$mois==6|trips2$mois==7|trips2$mois==8|trips2$mois==9|trips2$mois==10)
#binomial test to test if frequency of night hunting is higher during duiker lactating period
binom.test(sum(night_l$night_freq),length(night_l$night_freq),mean(night_h$night_freq),alternative="greater")
# during nursing season, night hunting proportion is higher

# Do the same with bow & arrow
prop_BandA<-subset(str_month$x,str_month$Group.1=="arc&fleches")/unique(Z)
shapiro.test(prop_BandA[6:10])
shapiro.test(prop_BandA[c(1:5,11,12)])
#binomial test to test if frequency of night hunting is higher during duiker lactating period
binom.test(sum(night_l$BandA_freq),length(night_l$BandA_freq),mean(night_h$BandA_freq),alternative="greater")

# And snares
prop_snare<-subset(str_month$x,str_month$Group.1=="pieges")/unique(Z)
shapiro.test(prop_snare[6:10])
shapiro.test(prop_snare[c(1:5,11,12)])
# Binomial test to test if frequency of night hunting is higher during duiker lactating period
binom.test(sum(night_l$snares_freq),length(night_l$snares_freq),mean(night_h$snares_freq),alternative="less")

# CPUE by method
aggregate(trips$CPUE_duree,by=list(trips$strat_revised),mean)
aggregate(trips$CPUE_duree,by=list(trips$strat_revised),sd)
# Check if difference in efficiency between strategies
wilcox.test(trips$CPUE_duree[trips$strat_revised2=="Bow&arrow_night"],trips$CPUE_duree[trips$strat_revised2=="Bow&arrow_day_individual"])
wilcox.test(trips$CPUE_duree[trips$strat_revised=="Bow&arrow_night_individual"],trips$CPUE_duree[trips$strat_revised=="Bow&arrow_day_individual"])
wilcox.test(trips$CPUE_duree[trips$strat_revised=="Bow&arrow_night_collective"],trips$CPUE_duree[trips$strat_revised=="Bow&arrow_day_individual"])

wilcox.test(trips$CPUE_duree[trips$strat_revised=="Bow&arrow_night_collective"],trips$CPUE_duree[trips$strat_revised=="Snares"])
wilcox.test(trips$CPUE_duree[trips$strat_revised=="Bow&arrow_night_collective"],trips$CPUE_duree[trips$strat_revised=="Gun"])

wilcox.test(trips$CPUE_duree[trips$strat_revised=="Gun"],trips$CPUE_duree[trips$strat_revised=="Bow&arrow_night_individual"])
wilcox.test(trips$CPUE_duree[trips$strat_revised=="Gun"],trips$CPUE_duree[trips$strat_revised=="Bow&arrow_day_individual"])
wilcox.test(trips$CPUE_duree[trips$strat_revised=="Gun"],trips$CPUE_duree[trips$strat_revised=="Snares"])
wilcox.test(trips$CPUE_duree[trips$strat_revised=="Gun"],trips$CPUE_duree[trips$strat_revised=="Other"])

wilcox.test(trips$CPUE_duree[trips$strat_revised=="Dogs"],trips$CPUE_duree[trips$strat_revised=="Gun"])
wilcox.test(trips$CPUE_duree[trips$strat_revised=="Dogs"],trips$CPUE_duree[trips$strat_revised=="Snares"])
wilcox.test(trips$CPUE_duree[trips$strat_revised=="Dogs"],trips$CPUE_duree[trips$strat_revised=="Bow&arrow_night_collective"])
wilcox.test(trips$CPUE_duree[trips$strat_revised=="Dogs"],trips$CPUE_duree[trips$strat_revised=="Bow&arrow_night_individual"])
wilcox.test(trips$CPUE_duree[trips$strat_revised=="Dogs"],trips$CPUE_duree[trips$strat_revised=="Bow&arrow_day_individual"],exact = F)
wilcox.test(trips$CPUE_duree[trips$strat_revised=="Dogs"],trips$CPUE_duree[trips$strat_revised=="Other"],exact = F)

# Strategy by taxa
sub<-subset(results,results$Source=="Q4")
n_strat<-as.data.frame(table(sub$Genre,sub$strategie,sub$jour_nuit))
n_strat

# Plot Fig. 3
source("Code/Fig_3.R")
