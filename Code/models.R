data_glm<-trips<-read.csv("Data/CPUE_sorties.csv")
# Recode as primary strategy
data_glm$strategie_primaire<-str_replace_all(data_glm$strategie_primaire,"arc&fleches","Bow & arrows")
data_glm$strategie_primaire<-str_replace_all(data_glm$strategie_primaire,"fusil","Gun")
data_glm$strategie_primaire<-str_replace_all(data_glm$strategie_primaire,"chiens","Dogs")
data_glm$strategie_primaire<-str_replace_all(data_glm$strategie_primaire,"machette","Machete")
data_glm$strategie_primaire<-str_replace_all(data_glm$strategie_primaire,"pieges","Snares")
data_glm$strategie_primaire<-str_replace_all(data_glm$strategie_primaire,"filet","Other")
data_glm$strategie_primaire<-str_replace_all(data_glm$strategie_primaire,"autre","Other")
# Organise data
data_glm$CPUE_distance<-as.numeric(data_glm$CPUE_distance)
data_glm$CPUE_duree<-as.numeric(data_glm$CPUE_duree)
data_glm$id_chasseur<-as.factor(data_glm$id_chasseur)
data_glm$activite_1<-as.factor(data_glm$activite_1)
data_glm$mois<-as.factor(data_glm$mois)
data_glm$strategie_primaire<-as.factor(data_glm$strategie_primaire)
# lump dogs with "other" strategy
data_glm$strategie_primaire<-str_replace_all(data_glm$strategie_primaire,"Dogs","Other")
data_glm$periode_chasse<-as.factor(data_glm$periode_chasse)
data_glm$type_chasse<-as.factor(data_glm$type_chasse)
data_glm$succes<-as.factor(data_glm$succes)
data_glm$temps_chasseur<-scale(data_glm$temps_chasseur)
data_glm$age_chasseur<-scale(data_glm$age_chasseur)
data_glm$duree<-data_glm$duree
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

# Hard to interpret - let's try the bernoulli glm by species

##### Ungulates ####
# Using frequency only improves model of 3.3 points - AIC - basically indistinguishable
data<-subset(data_glm,data_glm$kg_grandsongules>0 | data_glm$kg_petitsongules>0 |data_glm$kg_ongulesmoyens>0 |data_glm$kg_sangliers>0 | data_glm$kg_tot==0)
data$strategie_primaire<-as.factor(data$strategie_primaire)
data$strategie_primaire<-relevel(data$strategie_primaire,ref="Other")
m_succes_ungulates_all<-glm(data=data, succes ~ activite_1 + temps_chasseur + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, offset = duree, family = binomial(link="logit"))

summary(m_succes_ungulates_all)

coeff_ung<-as.data.frame(matrix(NA,nrow=11,ncol=6))
coeff_ung[,1]<-c("intercept","Hunter","Experience","Distance",
                            "Expenses","Bow & arrows","Gun","Snares","Multiple days","Night","Individual")
coeff_ung[,2]<-m_succes_ungulates_all$coefficients
coeff_ung[,3]<-confint(m_succes_ungulates_all)[,1]
coeff_ung[,4]<-confint(m_succes_ungulates_all)[,2]
coeff_ung[,5]<-as.vector(summary(m_succes_ungulates_all)$coefficient[,2])
coeff_ung[,6]<-as.vector(summary(m_succes_ungulates_all)$coefficient[,4])
coeff_ung<-coeff_ung[-1,]
# Check model
testDispersion(m_succes_ungulates_all) # looks good!
#let's do an additional test
simulateResiduals(fittedModel = m_succes_ungulates_all, plot = T) #no issues found
# check collinearity
vif(m_succes_ungulates_all) # Great! all values <1.2 (we'd worry if >5)

##### Primates ####
data<-subset(data_glm,data_glm$kg_primates>0 | data_glm$kg_tot==0)
# Take out hunting strategy = snare (code as "other")
data$strategie_primaire<-str_replace_all(data$strategie_primaire,"Snares","Other")
data$strategie_primaire<-as.factor(data$strategie_primaire)
data$strategie_primaire<-relevel(data$strategie_primaire,ref="Other")
m_succes_primates<-glm(data=data, succes ~ activite_1 + temps_chasseur + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, offset = duree, family = binomial(link="logit"))
# Using frequency only improves model of 2 points - AIC - basically indistinguishable
summary(m_succes_primates)
coeff_prim<-as.data.frame(matrix(NA,nrow=10,ncol=6))
coeff_prim[,1]<-c("intercept","Hunter","Experience","Distance",
                 "Expenses","Bow and arrows","Gun","Multiple days","Night","Individual")
coeff_prim[,2]<-m_succes_primates$coefficients
coeff_prim[,3]<-confint(m_succes_primates)[,1]
coeff_prim[,4]<-confint(m_succes_primates)[,2]
coeff_prim[,5]<-as.vector(summary(m_succes_primates)$coefficient[,2])
coeff_prim[,6]<-as.vector(summary(m_succes_primates)$coefficient[,4])
coeff_prim<-coeff_prim[-1,]
# Check model
testDispersion(m_succes_primates) # looks good!
#let's do an additional test
simulateResiduals(fittedModel = m_succes_primates, plot = T) #no issues found
# check collinearity
vif(m_succes_primates) # Great! all values <1.2 (we'd worry if >5)

##### Rodents ####
data<-subset(data_glm,data_glm$kg_rongeurs>0 | data_glm$kg_tot==0)
# Take out hunting strategy = gun (code as "other")
data$strategie_primaire<-str_replace_all(data$strategie_primaire,"Gun","Other")
data$strategie_primaire<-as.factor(data$strategie_primaire)
data$strategie_primaire<-relevel(data$strategie_primaire,ref="Other")
m_succes_rodents<-glm(data=data, succes ~ activite_1 + temps_chasseur+ distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, offset = duree, family = binomial(link="logit"))
summary(m_succes_rodents)

coeff_rod<-as.data.frame(matrix(NA,nrow=10,ncol=6))
coeff_rod[,1]<-c("intercept","Hunter","Experience","Distance",
                  "Expenses","Bow & arrows","Snares","Multiple days","Night","Individual")
coeff_rod[,2]<-m_succes_rodents$coefficients
coeff_rod[,3]<-confint(m_succes_rodents)[,1]
coeff_rod[,4]<-confint(m_succes_rodents)[,2]
coeff_rod[,5]<-as.vector(summary(m_succes_rodents)$coefficient[,2])
coeff_rod[,6]<-as.vector(summary(m_succes_rodents)$coefficient[,4])
coeff_rod<-coeff_rod[-1,]
# Check model
testDispersion(m_succes_rodents) # looks good!
#let's do an additional test
simulateResiduals(fittedModel = m_succes_rodents, plot = T) #no issues found
# check collinearity
vif(m_succes_rodents) # Great! all values <1.2 (we'd worry if >5)

##### Reptiles ####
data<-subset(data_glm,data_glm$kg_reptiles>0 | data_glm$kg_tot==0)
# Take out hunting strategy = gun (code as "other")
data$strategie_primaire<-str_replace_all(data$strategie_primaire,"Gun","Other")
data$strategie_primaire<-as.factor(data$strategie_primaire)
data$strategie_primaire<-relevel(data$strategie_primaire,ref="Other")
m_succes_reptiles<-glm(data=data, succes ~ activite_1 + temps_chasseur + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse, offset = duree,  family = binomial(link="logit"))
summary(m_succes_reptiles)

coeff_rep<-as.data.frame(matrix(NA,nrow=10,ncol=6))
coeff_rep[,1]<-c("intercept","Hunter","Experience","Distance",
                 "Expenses","Bow & arrows","Snares","Multiple days","Night","Individual")
coeff_rep[,2]<-m_succes_reptiles$coefficients
coeff_rep[,3]<-confint(m_succes_reptiles)[,1]
coeff_rep[,4]<-confint(m_succes_reptiles)[,2]
coeff_rep[,5]<-as.vector(summary(m_succes_reptiles)$coefficient[,2])
coeff_rep[,6]<-as.vector(summary(m_succes_reptiles)$coefficient[,4])
coeff_rep<-coeff_rep[-1,]
# Check model
testDispersion(m_succes_reptiles) # looks good!
#let's do an additional test
simulateResiduals(fittedModel = m_succes_reptiles, plot = T) #no issues found
# check collinearity
vif(m_succes_reptiles) # Great! all values <1.4 (we'd worry if >5)

# Plot results
source("Code/Fig_4.R")

#### REVISION 1 #####
###### Ungulates ####
# Small
m_succes_ungulates_small<-glm(data=subset(data_glm,data_glm$kg_petitsongules>0 | data_glm$kg_tot==0),
                              succes ~ activite_1 + temps_chasseur + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse,
                              offset = duree, family = binomial(link="logit"))

summary(m_succes_ungulates_small)

coeff_ung_small<-as.data.frame(matrix(NA,nrow=11,ncol=6))
coeff_ung_small[,1]<-c("intercept","Hunter","Experience","Distance",
                 "Expenses","Bow & arrows","Gun","Snares","Multiple days","Night","Individual")
coeff_ung_small[,2]<-m_succes_ungulates_small$coefficients
coeff_ung_small[,3]<-confint(m_succes_ungulates_small)[,1]
coeff_ung_small[,4]<-confint(m_succes_ungulates_small)[,2]
coeff_ung_small[,5]<-as.vector(summary(m_succes_ungulates_small)$coefficient[,2])
coeff_ung_small[,6]<-as.vector(summary(m_succes_ungulates_small)$coefficient[,4])
coeff_ung_small<-coeff_ung_small[-1,]
# Check model
testDispersion(m_succes_ungulates_small) # looks good!
#let's do an additional test
simulateResiduals(fittedModel = m_succes_ungulates_small, plot = T) #no issues found
# check collinearity
vif(m_succes_ungulates_small) # Great! all values <1.2 (we'd worry if >5)

# Medium
m_succes_ungulates_medium<-glm(data=subset(data_glm,data_glm$kg_ongulesmoyens>0 | data_glm$kg_tot==0),
                              succes ~ activite_1 + temps_chasseur + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse,
                              offset = duree, family = binomial(link="logit"))

summary(m_succes_ungulates_medium)

coeff_ung_medium<-as.data.frame(matrix(NA,nrow=11,ncol=6))
coeff_ung_medium[,1]<-c("intercept","Hunter","Experience","Distance",
                       "Expenses","Bow & arrows","Gun","Snares","Multiple days","Night","Individual")
coeff_ung_medium[,2]<-m_succes_ungulates_medium$coefficients
coeff_ung_medium[,3]<-confint(m_succes_ungulates_medium)[,1]
coeff_ung_medium[,4]<-confint(m_succes_ungulates_medium)[,2]
coeff_ung_medium[,5]<-as.vector(summary(m_succes_ungulates_medium)$coefficient[,2])
coeff_ung_medium[,6]<-as.vector(summary(m_succes_ungulates_medium)$coefficient[,4])
coeff_ung_medium<-coeff_ung_medium[-1,]
# Check model
testDispersion(m_succes_ungulates_medium) # looks good!
#let's do an additional test
simulateResiduals(fittedModel = m_succes_ungulates_medium, plot = T) #no issues found
# check collinearity
vif(m_succes_ungulates_medium) # Great! all values <1.4 (we'd worry if >5)

# Large
m_succes_ungulates_large<-glm(data=subset(data_glm,data_glm$kg_grandsongules>0 | data_glm$kg_sangliers>0 | data_glm$kg_tot==0),
                              succes ~ activite_1 + temps_chasseur + distance_lieu_chasse_hr + depenses_totales + strategie_primaire + periode_chasse + type_chasse,
                              offset = duree, family = binomial(link="logit"))

summary(m_succes_ungulates_large)

coeff_ung_large<-as.data.frame(matrix(NA,nrow=11,ncol=6))
coeff_ung_large[,1]<-c("intercept","Hunter","Experience","Distance",
                       "Expenses","Bow & arrows","Gun","Snares","Multiple days","Night","Individual")
coeff_ung_large[,2]<-m_succes_ungulates_large$coefficients
coeff_ung_large[,3]<-confint(m_succes_ungulates_large)[,1]
coeff_ung_large[,4]<-confint(m_succes_ungulates_large)[,2]
coeff_ung_large[,5]<-as.vector(summary(m_succes_ungulates_large)$coefficient[,2])
coeff_ung_large[,6]<-as.vector(summary(m_succes_ungulates_large)$coefficient[,4])
coeff_ung_large<-coeff_ung_large[-1,]
# Check model
testDispersion(m_succes_ungulates_large) # looks good!
#let's do an additional test
simulateResiduals(fittedModel = m_succes_ungulates_large, plot = T) #no issues found
# check collinearity
vif(m_succes_ungulates_large) # Great! all values <1.3 (we'd worry if >5)

# Plot results
source("Code/Supp_Fig_4.R")

