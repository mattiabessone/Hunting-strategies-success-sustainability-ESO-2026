#### Descriptives
# n hunters
length(hunters[,1])
# n recorded animals
length(results[,1])
# n hunting trips
length(trips_all[,1])
#total biomass hunted
sum(results$poids_kg,na.rm=TRUE)

#### Assess profile of collaborating hunters
# Look into education
table(hunters$niveau_etudes)
proportions(table(hunters$niveau_etudes))
# Experience
table(hunters$hunted_for)
proportions(table(hunters$hunted_for))
# Primary activity
proportions(table(hunters$activite_1))

# Strategy used by each of the category above
table(hunters$bowarrow,hunters$niveau_etudes)
proportions(table(hunters$bowarrow,hunters$niveau_etudes),margin=2)
table(hunters$bowarrow,hunters$hunted_for)
proportions(table(hunters$bowarrow,hunters$hunted_for),margin=2)
table(hunters$bowarrow,hunters$act)
proportions(table(hunters$bowarrow,hunters$act),margin=2)

# Age
hunters$experience<-ifelse(hunters$temps_chasseur<6,1,
                              ifelse(hunters$temps_chasseur>30,4,
                                     ifelse(hunters$temps_chasseur>5 & hunters$temps_chasseur<16,2,3)))

table_hunt_age<-aggregate(hunters$Age,by=list(hunters$niveau_etudes),median)
table_hunt_age$min<-aggregate(hunters$Age,by=list(hunters$niveau_etudes),min)[,2]
table_hunt_age$max<-aggregate(hunters$Age,by=list(hunters$niveau_etudes),max)[,2]

temp<-aggregate(hunters$Age,by=list(hunters$experience),median)
temp$min<-aggregate(hunters$Age,by=list(hunters$experience),min)[,2]
temp$max<-aggregate(hunters$Age,by=list(hunters$experience),max)[,2]

table_hunt_age<-rbind(table_hunt_age,temp)

temp<-aggregate(hunters$Age,by=list(hunters$activite_1),median)
temp$min<-aggregate(hunters$Age,by=list(hunters$activite_1),min)[,2]
temp$max<-aggregate(hunters$Age,by=list(hunters$activite_1),max)[,2]

table_hunt_age<-rbind(table_hunt_age,temp)
table_hunt_age
rm(temp)
