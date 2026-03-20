##### Baraka, Bessone et al. (2026). Ecologial Solutions and Evidence####
# Load libraries
library(RColorBrewer)
library(stringr)
library(lme4)
library(car)
library(DHARMa)
# Load data
results<-read.csv("Data/results_all.csv")
trips<-read.csv("Data/CPUE_sorties.csv")
trips_all<-read.csv("Data/hunting__trips_data.csv")
hunters<-read.csv("Data/hunters_data.csv")

#### 1) Process data - recode categories of prey and strategy ####
source("Code/process_data.R")

#### 2) Hunter profiles ####
source("Code/hunter_profile.R")

#### 3) Species composition and biomass ####
source("Code/offtake.R")

#### 4) Sustainability indexes ####
source("Code/sustainability.R")

#### 5) Hunting strategies ####
source("Code/strategies.R")

#### 6) Models ####
source("Code/models.R")
