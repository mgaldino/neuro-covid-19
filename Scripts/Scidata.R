###################################
# R Code for reliability analysis #
# used in the Nature Comms        #
# paper.                          #
###################################

#1. OPEN FILE, LIBRARIES:



# Load library
library(ufs)
library(psych)
library(MBESS) # OpenMx
library(dplyr)
library(data.table)
library(readr)
# open file (where data is)
library(here)

ICSMP <- read_csv("Dados/ICSMP_cleaned_data.csv")

# 2. SELECT AND PREPARE SCALES (get variables, reverse items and average scales)

# Select variables for each scale:
phy_con <- c("contact1", "contact2rev", "contact3","contact4", "contact5") #Physical contact (or social distancing). Item 2 is reversed
phy_hyg <- c("hygiene1", "hygiene2", "hygiene3","hygiene4", "hygiene5") #Physical hygiene
pol_sup <- c("psupport1", "psupport2", "psupport3","psupport4", "psupport5") #COVID-19 policy support
col_nar <- c("cnarc1", "cnarc2", "cnarc3") #collective (or national) narcissism
nat_ide <- c("nidentity1", "nidentity2") #national identity
con_the <- c("ctheory1", "ctheory2", "ctheory3", "ctheory4") #conspiracy theory
mor_ide <- c("moralid1", "moralid2", "moralid3", "moralid4rev", "moralid5", "moralid6", "moralid7rev", "moralid8", "moralid9", "moralid10") #moral identity. Items 4 and 7 are reversed
mor_cop <- c("mcoop1", "mcoop2", "mcoop3", "mcoop4", "mcoop5", "mcoop6", "mcoop7") #moral as cooperation
nar_tra <- c("narc1", "narc2", "narc3", "narc4", "narc5", "narc6") #narcissism trait
ope_min <- c("omind1rev", "omind2", "omind3", "omind4", "omind5rev", "omind6rev") #open mindedness. Items 1, 5 and 6 are reveresed
soc_bel <- c("sbelong1", "sbelong2", "sbelong3", "sbelong4") #social belonging
opt_tra <- c("optim1", "optim2") #optmism trait
sel_con <- c("slfcont1", "slfcont2", "slfcont3rev", "slfcont4rev") #self-control trait. Items 3 and 4 are reversed.

mainvar <- c("phy_con", "phy_hyg", "pol_sup") #main variables

#Correct reversed items
ICSMP <- ICSMP %>% mutate(contact2rev = 10 - contact2,
                          moralid4rev = 10 - moralid4,
                          moralid7rev = 10 - moralid7,
                          omind1rev = 10 - omind1,
                          omind5rev = 10 - omind5,
                          omind6rev = 10 - omind6,
                          slfcont3rev = 10 - slfcont3,
                          slfcont4rev = 10 - slfcont4)

#Average main scales
ICSMP$phy_con_avg <- rowMeans(ICSMP[,phy_con]) #Physical contact average
ICSMP$phy_hyg_avg <- rowMeans(ICSMP[,phy_hyg]) #Physical hygiene average
ICSMP$pol_sup_avg <- rowMeans(ICSMP[,pol_sup]) #COVID-19 policy support average
ICSMP$col_nar_avg <- rowMeans(ICSMP[,col_nar]) #collective (or national) narcissism average
ICSMP$nat_ide_avg <- rowMeans(ICSMP[,nat_ide]) #national identity average
ICSMP$con_the_avg <- rowMeans(ICSMP[,con_the]) #conspiracy theory average
ICSMP$mor_ide_avg <- rowMeans(ICSMP[,mor_ide]) #moral identity average
ICSMP$mor_cop_avg <- rowMeans(ICSMP[,mor_cop]) #moral as cooperation average
ICSMP$nar_tra_avg <- rowMeans(ICSMP[,nar_tra]) #narcissism trait average
ICSMP$ope_min_avg <- rowMeans(ICSMP[,ope_min]) #open mindedness average
ICSMP$soc_bel_avg <- rowMeans(ICSMP[,soc_bel]) #social belonging average
ICSMP$opt_tra_avg <- rowMeans(ICSMP[,opt_tra]) #optmism trait average
ICSMP$sel_con_avg <- rowMeans(ICSMP[,sel_con]) #self-control trait average

#Vector with country names based on $ISO
countries <- names(table(ICSMP$ISO3))

#saving dataset
saveRDS(ICSMP, file = "Dados/icsmp.rds")

#3. FUNCTIONS FOR GET RELIABILITY VALUES:

#Function for alpha (raw alpha):
GetAlp <- function(x, y){
  tryCatch(alpha(ICSMP[ICSMP$ISO3 == x, y])$total$raw_alpha, error = function(e){NA})
}
#Function for omega (total, one factor model): 
GetOme <- function(x, y){ 
  tryCatch(omega(ICSMP[ICSMP$ISO3 == x, y], nfactors = 1, plot = F)$omega.tot, error = function(e){NA})
}
#Function for guttman (glb)
GetGlb <- function(x, y){
  tryCatch(guttman(ICSMP[ICSMP$ISO3 == x, y])$glb, error = function(e){NA})
}
#Function for Variance explained by one factor:
GetVar <- function(x, y){
  tryCatch(fa(ICSMP[ICSMP$ISO3 == x, y])$Vaccounted[2], error = function(e){NA})
} 

#Funcion to get reliability values and create table:
Getval <- function(y){
  a <- data.frame(sapply(countries, GetAlp, y))
  o <- data.frame(sapply(countries, GetOme, y))
  g <- data.frame(sapply(countries, GetGlb, y))
  v <- data.frame(sapply(countries, GetVar, y))
  aogv <- cbind(a, o, g, v)
  names(aogv) <- c("alpha", "omega", "glb", "variance")
  return(aogv)
}

#4. GEL ALPHA, OMEGA, CRONBACH, GUTTMAN FOR MAIN SCALES: 

#physical contact: 
phy_con_rel <- Getval(phy_con)
#physical hygiente:
phy_hyg_rel <- Getval(phy_hyg)
#policy support:
pol_sup_rel <- Getval(pol_sup)

#get correlation between main scales:
corr.test(ICSMP[,c("phy_con_avg","phy_hyg_avg","pol_sup_avg")])
