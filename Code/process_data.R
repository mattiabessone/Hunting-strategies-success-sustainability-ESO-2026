# Organise data for hunter profiles
hunters$act<-ifelse(hunters$activite_1=="commerce" | hunters$activite_1=="salarie","salarie",hunters$activite_1)
hunters$act<-ifelse(hunters$activite_1=="peche","autre",hunters$act)
hunters$bow<-paste(hunters$temps_strategie_arcfleches,hunters$type_chasse_arcfleches,sep="_")
hunters$gun<-paste(hunters$temps_strategie_fusil,hunters$type_chasse_fusil,sep="_")
hunters$snare<-paste(hunters$temps_strategie_pieges,hunters$type_chasse_pieges,sep="_")
hunters$mach<-paste(hunters$temps_strategie_machette,hunters$type_chasse_machette,sep="_")
hunters$dog<-paste(hunters$temps_strategie_chiens,hunters$type_chasse_chiens,sep="_")
hunters$type_arcfleches<-ifelse(hunters$type_chasse_arcfleches=="individuelle_collective","collective",hunters$type_chasse_arcfleches)
hunters$hunted_for<-ifelse(hunters$temps_chasseur<6,1,
                           ifelse(hunters$temps_chasseur>30,4,
                                  ifelse(hunters$temps_chasseur>5 & hunters$temps_chasseur<16,2,3)))
hunters$time_arcfleches<-ifelse(hunters$temps_strategie_arcfleches=="journuit","nuit",hunters$temps_strategie_arcfleches)
hunters$bowarrow<-paste(hunters$time_arcfleches,hunters$type_arcfleches,sep="_")
hunters$bowarrow<-ifelse(hunters$bowarrow=="jour_collective","jour_individuelle",hunters$bowarrow)

# Recode hunted species
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
# Calculate proporiton of sold meat
results$prop_sold<-results$kg_vente/results$poids_kg
# Remove rows with no animal recorded (Genre = "Aucun")
results<-subset(results,results$espece!="Aucun")
#### Trips
trips$CPUE_distance<-as.numeric(trips$CPUE_distance)
trips$CPUE_duree<-as.numeric(trips$CPUE_duree)
# Recode as primary strategy
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"arc&fleches","Bow & arrows")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"fusil","Gun")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"chiens","Dogs")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"machette","Machete")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"pieges","Snares")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"filet","Other")
trips$strategie_primaire<-str_replace_all(trips$strategie_primaire,"autre","Other")
# Redefine strategy
trips$periode_chasse<-ifelse(trips$periode_chasse=="les_deux" & trips$strategie_primaire=="Bow & arrows" ,"nuit",
                         ifelse(trips$periode_chasse=="les_deux" & trips$strategie_primaire=="Snares","jour",
                                ifelse(trips$periode_chasse=="les_deux" & trips$strategie_primaire=="Gun","jour",
                                       ifelse(trips$periode_chasse=="les_deux" & trips$strategie_primaire=="Dogs","jour",
                                              ifelse(trips$periode_chasse=="les_deux" & trips$strategie_primaire=="Other","nuit",trips$periode_chasse)))))
trips$strat_revised<-ifelse(trips$strategie_primaire=="Bow & arrows" & trips$type_chasse=="individuelle" & trips$periode_chasse =="jour","Bow&arrow_day_individual",
                        ifelse(trips$strategie_primaire=="Bow & arrows" & trips$type_chasse=="individuelle" & trips$periode_chasse =="nuit","Bow&arrow_night_individual",
                               ifelse(trips$strategie_primaire=="Bow & arrows" & trips$type_chasse=="collective" & trips$periode_chasse =="nuit","Bow&arrow_night_collective",trips$strategie_primaire)))
trips$strat_revised<-ifelse(trips$strat_revised=="Bow & arrows","Bow&arrow_day_individual",trips$strat_revised)
trips$strat_revised<-ifelse(trips$strat_revised=="Bow&arrow_night_individual" & trips$CPUE_duree_primates>0,"Bow&arrow_day_individual",trips$strat_revised)                         

trips$strat_revised2<-ifelse(trips$strat_revised=="Bow&arrow_night_collective" | trips$strat_revised=="Bow&arrow_night_individual","Bow&arrow_night",trips$strat_revised)

# Focal points
trips_all$n<-rep(1,by=length(trips_all[,1]))
trips_all$strategie_primaire<-ifelse(trips_all$strategie_primaire=="filet","autre",trips_all$strategie_primaire)
trips_all$strategie_primaire<-ifelse(trips_all$strategie_primaire=="machette","autre",trips_all$strategie_primaire)
trips1<-subset(trips_all,trips_all$strategie_primaire!="")

# Re-define strategy 
trips1$periode_chasse<-ifelse(trips1$periode_chasse=="les_deux" & trips1$strategie_primaire=="arc&fleches" ,"nuit",
                         ifelse(trips1$periode_chasse=="les_deux" & trips1$strategie_primaire=="pieges","jour",
                                ifelse(trips1$periode_chasse=="les_deux" & trips1$strategie_primaire=="fusil","jour",
                                       ifelse(trips1$periode_chasse=="les_deux" & trips1$strategie_primaire=="chiens","jour",
                                              ifelse(trips1$periode_chasse=="les_deux" & trips1$strategie_primaire=="autres","nuit",trips1$periode_chasse)))))
trips1$strat_revised<-paste(trips1$strategie_primaire,trips1$type_chasse,trips1$periode_chasse,sep="_")

unique(trips1$strat_revised)

# Allocate entries without time of day
trips1$strat_revised<-ifelse(trips1$strat_revised=="pieges_individuelle_","pieges_individuelle_jour",
                        ifelse(trips1$strat_revised=="pieges_collective_","pieges_collective_jour",
                               ifelse(trips1$strat_revised=="fusil_individuelle_","fusil_individuelle_jour",
                                      ifelse(trips1$strat_revised=="fusil_collective_","fusil_collective_jour",
                                             ifelse(trips1$strat_revised=="arc&fleches_collective_","arc&fleches_collective_nuit",
                                                    ifelse(trips1$strat_revised=="arc&fleches_individuelle_","arc&fleches_individuelle_jour",
                                                           ifelse(trips1$strat_revised=="pieges_NA_jour","pieges_individuelle_jour",trips1$strat_revised)))))))
unique(trips1$strat_revised)
# Ignore composite strategy except in the case of bow and arrow
trips1$strat_revised2<-ifelse(trips1$strategie_primaire=="arc&fleches",trips1$strat_revised,trips1$strategie_primaire)
trips1$strat_revised2<-ifelse(trips1$strat_revised2=="arc&fleches_collective_jour","arc&fleches_individuelle_nuit",trips1$strat_revised2)

