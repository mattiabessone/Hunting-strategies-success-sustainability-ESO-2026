# Prey by taxa
table(results$Genre)
# Prey by species
table(results$espece)

# Biomass by taxa
aggregate(results$poids_kg,by=list(results$Genre),sum)
proportions(aggregate(results$poids_kg,by=list(results$Genre),sum)$x)
# Biomass by species
aggregate(results$poids_kg,by=list(results$espece),sum)
proportions(aggregate(results$poids_kg,by=list(results$espece),sum)$x)

# Biomass/hunter/year
sum(results$poids_kg)/nrow(hunters)
# Biomass/hunter/trip
sum(results$poids_kg)/nrow(trips_all)

# 10 most prolific hunters
prolific_h<-aggregate(trips1$Kg_Tot_Ani,by=list(trips1$unique_hunter_ID),sum)
prolific_h<-prolific_h[order(-prolific_h$x),]
sum(prolific_h$x[2:11]) # we skip the first line as this is the total for unidentified huenters
sum(prolific_h$x[2:11])/sum(results$poids_kg)

# Most prolific hunters extraction by strategy
prolific_h_strat<-subset(trips1,trips1$unique_hunter_ID=="5_1" |
                           trips1$unique_hunter_ID=="5_2" |
                           trips1$unique_hunter_ID=="5_3" |
                           trips1$unique_hunter_ID=="5_21" |
                           trips1$unique_hunter_ID=="4_6" |
                           trips1$unique_hunter_ID=="2_1" |
                           trips1$unique_hunter_ID=="2_3" |
                           trips1$unique_hunter_ID=="1_10" |
                           trips1$unique_hunter_ID=="1_6" |
                           trips1$unique_hunter_ID=="3_24")
aggregate(prolific_h_strat$Kg_Tot_Ani,by=list(prolific_h_strat$strat_revised),sum)

### Meat destination
# Number of animals fully or partially traded
nrow(results[results$kg_vente>0,])
# consumed biomass = 2039.8kg
sum(results$kg_conso)
# sold biomass = 7006.4kg
sum(results$kg_vente)
# gifted biomass = 461.2kg
sum(results$kg_don)
# biomass payment = 653.8kg
sum(results$kg_paim)
# biomass waste = 30.8kg
sum(results$kg_jeter)

# Most prolific hunters trade by strategy
sub_trade<-subset(results,results$Source=="Q4")
prolific_h_trade<-subset(sub_trade,sub_trade$unique_hunter_ID=="5_1" |
                           sub_trade$unique_hunter_ID=="5_2" |
                           sub_trade$unique_hunter_ID=="5_3" |
                           sub_trade$unique_hunter_ID=="5_21" |
                           sub_trade$unique_hunter_ID=="4_6" |
                           sub_trade$unique_hunter_ID=="2_1" |
                           sub_trade$unique_hunter_ID=="2_3" |
                           sub_trade$unique_hunter_ID=="1_10" |
                           sub_trade$unique_hunter_ID=="1_6" |
                           sub_trade$unique_hunter_ID=="1_24")
other_h_trade<-subset(sub_trade,sub_trade$unique_hunter_ID!="5_1" |
                        sub_trade$unique_hunter_ID!="5_2" |
                        sub_trade$unique_hunter_ID!="5_3" |
                        sub_trade$unique_hunter_ID!="5_21" |
                        sub_trade$unique_hunter_ID!="4_6" |
                        sub_trade$unique_hunter_ID!="2_1" |
                        sub_trade$unique_hunter_ID!="2_3" |
                        sub_trade$unique_hunter_ID!="1_10" |
                        sub_trade$unique_hunter_ID!="1_6" |
                        sub_trade$unique_hunter_ID!="3_24")
mean(prolific_h_trade$prop_sold)
mean(other_h_trade$prop_sold)
# Test differences between prolific vs other
wilcox.test(prolific_h_trade$prop_sold,other_h_trade$prop_sold,alternative = "greater")

## Check if species composition differ between consumed and sold
biomass_sold<-aggregate(results$kg_vente,by=list(results$Genre),sum)
biomass_consumed<-aggregate(results$kg_conso,by=list(results$Genre),sum)
test<-as.data.frame(matrix(NA,nrow=14,ncol=3))
test[,1]<-append(biomass_sold[,1],biomass_consumed[,1])
test[,2]<-append(proportions(biomass_sold[,2]),proportions(biomass_consumed[,2]))
test[1:7,3]<-"Sold"
test[8:14,3]<-"Consumed"
composition_test<-as.data.frame(cbind(test$V1[1:7],test$V2[1:7]*sum(results$kg_vente),test$V2[8:14]))
chisq.test(as.numeric(composition_test$V2),p=as.numeric(composition_test$V3))

## Correlation between MBM and price
shapiro.test(log(results$poids_kg)) # not normal
shapiro.test(log(results$prix_vente_kg)) # not normal
cor.test(results$poids_kg, results$prix_vente_kg,method="spearman",exact=FALSE)
