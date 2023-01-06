########################################################################################################################
# R Project Master 1 
# Comparative Analysis of the determinants of Economic growth in WAEMU and BRICS Countries from 2005 to 2018
# 
# Performed by :
#   
# -   GOMEZ Jean-Baptiste Boris
# -   MOUSSA DJIBO Nabil
# -   LAKSIBI Safae
########################################################################################################################

# Library required

library(foreign)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(plm)
library(stargazer)
library(car)
library(lmtest)

# Importing and Preparing Data
setwd("C:/Users/gomez/OneDrive/Documents/r STUDIO MASTER1/Project_R_M1/Databases")


################################################# WAEMU DATABASES ######################################################
######### Public expenses #############

data_DP <- read_excel("Depenses_publiques.xls", sheet = "Data", skip = 3) 
data_DP <- data_DP[,-3:-24]
names(data_DP)[1:2]<-c("Country_Name","Country_Code")
row_data_DP_UEMOA<- c("Bénin","Burkina Faso","Côte d'Ivoire","Guinée-Bissau","Mali","Niger","Sénégal","Togo")
data_DP_UEMOA<- data_DP[data_DP$Country_Name %in% row_data_DP_UEMOA,]
BenData_uemoa = data_DP_UEMOA[1,]
Depubdata <- BenData_uemoa %>% pivot_longer(c(3:ncol(data_DP_UEMOA)),names_to = "Year", values_to = "DEPUB") #gather(Year, DEPUB, 4:10)
tempdf = data.frame()
i=2
while (i<= nrow(data_DP_UEMOA) ) {
  tempdf<-data_DP_UEMOA[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_DP_UEMOA)),names_to = "Year", values_to = "DEPUB")
  Depubdata <- rbind.data.frame(Depubdata,tempdf)
  i<-i+1
}
# View(Depubdata)

######### External Debts ###########

data_DEBTEXT <- read_excel("Dette_exterieur.xls", sheet = "Data", skip = 3) 
data_DEBTEXT <- data_DEBTEXT[,-3:-24]
names(data_DEBTEXT)[1:2]<-c("Country_Name","Country_Code")
row_data_DEBTEXT_UEMOA<- c("Bénin","Burkina Faso","Côte d'Ivoire","Guinée-Bissau","Mali","Niger","Sénégal","Togo")
data_DEBTEXT_UEMOA<- data_DEBTEXT[data_DEBTEXT$Country_Name %in% row_data_DEBTEXT_UEMOA,]
BenData_uemoa = data_DEBTEXT_UEMOA[1,]
Dettdata <- BenData_uemoa %>% pivot_longer(c(3:ncol(data_DEBTEXT_UEMOA)),names_to = "Year", values_to = "DEBTEXT") 
tempdf = data.frame()
i=2
while (i<= nrow(data_DEBTEXT_UEMOA) ) {
  tempdf<-data_DEBTEXT_UEMOA[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_DEBTEXT_UEMOA)),names_to = "Year", values_to = "DEBTEXT")
  Dettdata <- rbind.data.frame(Dettdata,tempdf)
  i<-i+1
}
# View(Dettdata)

######### Gross savings ##############

data_Epar <- read_excel("epargne_brute.xls", sheet = "Data", skip = 3) 
data_Epar <- data_Epar[,-3:-24]
names(data_Epar)[1:2]<-c("Country_Name","Country_Code")
row_data_Epar_UEMOA<- c("Bénin","Burkina Faso","Côte d'Ivoire","Guinée-Bissau","Mali","Niger","Sénégal","Togo")
data_Epar_UEMOA<- data_Epar[data_Epar$Country_Name %in% row_data_Epar_UEMOA,]
BenData_uemoa = data_Epar_UEMOA[1,]
Epardata <- BenData_uemoa %>% pivot_longer(c(3:ncol(data_Epar_UEMOA)),names_to = "Year", values_to = "EPARBRUT") 
tempdf = data.frame()
i=2
while (i<= nrow(data_Epar_UEMOA) ) {
  tempdf<-data_Epar_UEMOA[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_Epar_UEMOA)),names_to = "Year", values_to = "EPARBRUT")
  Epardata <- rbind.data.frame(Epardata,tempdf)
  i<-i+1
}
# View(Epardata)

######### GDP per capita ##############

data_GDPH <- read_excel("GDPperHabitat.xls", sheet = "Data", skip = 3) 
data_GDPH <- data_GDPH[,-3:-24]
names(data_GDPH)[1:2]<-c("Country_Name","Country_Code")
row_data_GDPH_UEMOA<- c("Bénin","Burkina Faso","Côte d'Ivoire","Guinée-Bissau","Mali","Niger","Sénégal","Togo")
data_GDPH_UEMOA<- data_GDPH[data_GDPH$Country_Name %in% row_data_GDPH_UEMOA,]
BenData_uemoa = data_GDPH_UEMOA[1,]
GDPHdata <- BenData_uemoa %>% pivot_longer(c(3:ncol(data_GDPH_UEMOA)),names_to = "Year", values_to = "GDP_Habitat") 
tempdf = data.frame()
i=2
while (i<= nrow(data_GDPH_UEMOA) ) {
  tempdf<-data_GDPH_UEMOA[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_GDPH_UEMOA)),names_to = "Year", values_to = "GDP_Habitat")
  GDPHdata <- rbind.data.frame(GDPHdata,tempdf)
  i<-i+1
}
# View(GDPHdata)

######### Inflation in % GDP ##############

data_INFLA <- read_excel("inflation_deflateur_PIB.xls", sheet = "Data", skip = 3) 
data_INFLA <- data_INFLA[,-3:-24]
names(data_INFLA)[1:2]<-c("Country_Name","Country_Code")
row_data_INFLA_UEMOA<- c("Bénin","Burkina Faso","Côte d'Ivoire","Guinée-Bissau","Mali","Niger","Sénégal","Togo")
data_INFLA_UEMOA<- data_INFLA[data_INFLA$Country_Name %in% row_data_INFLA_UEMOA,]
BenData_uemoa = data_INFLA_UEMOA[1,]
INFLAdata <- BenData_uemoa %>% pivot_longer(c(3:ncol(data_INFLA_UEMOA)),names_to = "Year", values_to = "Inflation") 
tempdf = data.frame()
i=2
while (i<= nrow(data_INFLA_UEMOA) ) {
  tempdf<-data_INFLA_UEMOA[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_INFLA_UEMOA)),names_to = "Year", values_to = "Inflation")
  INFLAdata <- rbind.data.frame(INFLAdata,tempdf)
  i<-i+1
}
# View(INFLAdata)

######### Private investment ################

data_INVPR <- read_excel("Investissement_privée(FBCF).xls", sheet = "Data", skip = 3) 
data_INVPR <- data_INVPR[,-3:-24]
names(data_INVPR)[1:2]<-c("Country_Name","Country_Code")
row_data_INVPR_UEMOA<- c("Bénin","Burkina Faso","Côte d'Ivoire","Guinée-Bissau","Mali","Niger","Sénégal","Togo")
data_INVPR_UEMOA<- data_INVPR[data_INVPR$Country_Name %in% row_data_INVPR_UEMOA,]
BenData_uemoa = data_INVPR_UEMOA[1,]
INVPRdata <- BenData_uemoa %>% pivot_longer(c(3:ncol(data_INVPR_UEMOA)),names_to = "Year", values_to = "INPRIV") 
tempdf = data.frame()
i=2
while (i<= nrow(data_INVPR_UEMOA) ) {
  tempdf<-data_INVPR_UEMOA[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_INVPR_UEMOA)),names_to = "Year", values_to = "INPRIV")
  INVPRdata <- rbind.data.frame(INVPRdata,tempdf)
  i<-i+1
}
# View(INVPRdata)

######### Labour force ###############

data_POPACT <- read_excel("population_active.xls", sheet = "Data", skip = 3) 
data_POPACT <- data_POPACT[,-3:-24]
names(data_POPACT)[1:2]<-c("Country_Name","Country_Code")
row_data_POPACT_UEMOA<- c("Bénin","Burkina Faso","Côte d'Ivoire","Guinée-Bissau","Mali","Niger","Sénégal","Togo")
data_POPACT_UEMOA<- data_POPACT[data_POPACT$Country_Name %in% row_data_POPACT_UEMOA,]
BenData_uemoa = data_POPACT_UEMOA[1,]
POPACTdata <- BenData_uemoa %>% pivot_longer(c(3:ncol(data_POPACT_UEMOA)),names_to = "Year", values_to = "POP_ACT") 
tempdf = data.frame()
i=2
while (i<= nrow(data_POPACT_UEMOA) ) {
  tempdf<-data_POPACT_UEMOA[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_POPACT_UEMOA)),names_to = "Year", values_to = "POP_ACT")
  POPACTdata <- rbind.data.frame(POPACTdata,tempdf)
  i<-i+1
}
# View(POPACTdata)

######### Export of goods and services in GDP % ################

data_EXPIB <- read_excel("Exportation_de_bien_et_services_pourcentage_PIB.xls", sheet = "Data", skip = 3) 
data_EXPIB <- data_EXPIB[,-3:-24]
names(data_EXPIB)[1:2]<-c("Country_Name","Country_Code")
row_data_EXPIB_UEMOA<- c("Bénin","Burkina Faso","Côte d'Ivoire","Guinée-Bissau","Mali","Niger","Sénégal","Togo")
data_EXPIB_UEMOA<- data_EXPIB[data_EXPIB$Country_Name %in% row_data_EXPIB_UEMOA,]
BenData_uemoa = data_EXPIB_UEMOA[1,]
EXPIBdata <- BenData_uemoa %>% pivot_longer(c(3:ncol(data_EXPIB_UEMOA)),names_to = "Year", values_to = "EXP_PIB") 
tempdf = data.frame()
i=2
while (i<= nrow(data_EXPIB_UEMOA) ) {
  tempdf<-data_EXPIB_UEMOA[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_EXPIB_UEMOA)),names_to = "Year", values_to = "EXP_PIB")
  EXPIBdata <- rbind.data.frame(EXPIBdata,tempdf)
  i<-i+1
}
# View(EXPIBdata)

######### importation_des_biens_et_services_pourcentage_PIB ##############

data_IMPIB <- read_excel("importation_des_biens_et_services_pourcentage_PIB.xls", sheet = "Data", skip = 3) 
data_IMPIB <- data_IMPIB[,-3:-24]
names(data_IMPIB)[1:2]<-c("Country_Name","Country_Code")
row_data_IMPIB_UEMOA<- c("Bénin","Burkina Faso","Côte d'Ivoire","Guinée-Bissau","Mali","Niger","Sénégal","Togo")
data_IMPIB_UEMOA<- data_IMPIB[data_IMPIB$Country_Name %in% row_data_IMPIB_UEMOA,]
BenData_uemoa = data_IMPIB_UEMOA[1,]
IMPIBdata <- BenData_uemoa %>% pivot_longer(c(3:ncol(data_IMPIB_UEMOA)),names_to = "Year", values_to = "IMP_PIB") 
tempdf = data.frame()
i=2
while (i<= nrow(data_IMPIB_UEMOA) ) {
  tempdf<-data_IMPIB_UEMOA[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_IMPIB_UEMOA)),names_to = "Year", values_to = "IMP_PIB")
  IMPIBdata <- rbind.data.frame(IMPIBdata,tempdf)
  i<-i+1
}
 # View(IMPIBdata)

############## The degree of openness #####################

OCdata <- cbind.data.frame(IMPIBdata, EXPIBdata[,ncol(EXPIBdata)])
OCdata[is.na(OCdata)] <- 0 # replace na  0 
OCdata$OUVCM <- (OCdata$IMP_PIB + OCdata$EXP_PIB)/2
# View(OCdata)

# Merging dataset

Data_UEMOA <- cbind.data.frame(GDPHdata, POPACTdata[,ncol(POPACTdata)], INVPRdata[,ncol(INVPRdata)],
                               Epardata[,ncol(Epardata)], Dettdata[,ncol(Dettdata)], Depubdata[,ncol(Depubdata)],
                               OCdata["OUVCM"], INFLAdata[,ncol(INFLAdata)])

# View(Data_UEMOA)

################################################# BRICS DATABASES ##################################################################

######### Public expenses #############

data_DP <- read_excel("Depenses_publiques.xls", sheet = "Data", skip = 3) 
data_DP <- data_DP[,-3:-24]
names(data_DP)[1:2]<-c("Country_Name","Country_Code")
row_data_DP_BRICS<- c("Brésil","Fédération de Russie","Inde","Chine","Afrique du Sud")
data_DP_BRICS<- data_DP[data_DP$Country_Name %in% row_data_DP_BRICS,]
BenData_BRICS = data_DP_BRICS[1,]
Depubdata <- BenData_BRICS %>% pivot_longer(c(3:ncol(data_DP_BRICS)),names_to = "Year", values_to = "DEPUB") #gather(Year, DEPUB, 4:10)
tempdf = data.frame()
i=2
while (i<= nrow(data_DP_BRICS) ) {
  tempdf<-data_DP_BRICS[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_DP_BRICS)),names_to = "Year", values_to = "DEPUB")
  Depubdata <- rbind.data.frame(Depubdata,tempdf)
  i<-i+1
}
# View(Depubdata)

######### External Debts ###########

data_DEBTEXT <- read_excel("Dette_exterieur.xls", sheet = "Data", skip = 3) 
data_DEBTEXT <- data_DEBTEXT[,-3:-24]
names(data_DEBTEXT)[1:2]<-c("Country_Name","Country_Code")
row_data_DEBTEXT_BRICS<- c("Brésil","Fédération de Russie","Inde","Chine","Afrique du Sud")
data_DEBTEXT_BRICS<- data_DEBTEXT[data_DEBTEXT$Country_Name %in% row_data_DEBTEXT_BRICS,]
BenData_BRICS = data_DEBTEXT_BRICS[1,]
Dettdata <- BenData_BRICS %>% pivot_longer(c(3:ncol(data_DEBTEXT_BRICS)),names_to = "Year", values_to = "DEBTEXT") 
tempdf = data.frame()
i=2
while (i<= nrow(data_DEBTEXT_BRICS) ) {
  tempdf<-data_DEBTEXT_BRICS[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_DEBTEXT_BRICS)),names_to = "Year", values_to = "DEBTEXT")
  Dettdata <- rbind.data.frame(Dettdata,tempdf)
  i<-i+1
}
# View(Dettdata)

######### Gross savings ##############

data_Epar <- read_excel("epargne_brute.xls", sheet = "Data", skip = 3) 
data_Epar <- data_Epar[,-3:-24]
names(data_Epar)[1:2]<-c("Country_Name","Country_Code")
row_data_Epar_BRICS<- c("Brésil","Fédération de Russie","Inde","Chine","Afrique du Sud")
data_Epar_BRICS<- data_Epar[data_Epar$Country_Name %in% row_data_Epar_BRICS,]
BenData_BRICS = data_Epar_BRICS[1,]
Epardata <- BenData_BRICS %>% pivot_longer(c(3:ncol(data_Epar_BRICS)),names_to = "Year", values_to = "EPARBRUT") 
tempdf = data.frame()
i=2
while (i<= nrow(data_Epar_BRICS) ) {
  tempdf<-data_Epar_BRICS[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_Epar_BRICS)),names_to = "Year", values_to = "EPARBRUT")
  Epardata <- rbind.data.frame(Epardata,tempdf)
  i<-i+1
}

# View(Epardata)

######### GDP per capita ##############

data_GDPH <- read_excel("GDPperHabitat.xls", sheet = "Data", skip = 3) 
data_GDPH <- data_GDPH[,-3:-24]
names(data_GDPH)[1:2]<-c("Country_Name","Country_Code")
row_data_GDPH_BRICS<- c("Brésil","Fédération de Russie","Inde","Chine","Afrique du Sud")
data_GDPH_BRICS<- data_GDPH[data_GDPH$Country_Name %in% row_data_GDPH_BRICS,]
BenData_BRICS = data_GDPH_BRICS[1,]
GDPHdata <- BenData_BRICS %>% pivot_longer(c(3:ncol(data_GDPH_BRICS)),names_to = "Year", values_to = "GDP_Habitat") 
tempdf = data.frame()
i=2
while (i<= nrow(data_GDPH_BRICS) ) {
  tempdf<-data_GDPH_BRICS[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_GDPH_BRICS)),names_to = "Year", values_to = "GDP_Habitat")
  GDPHdata <- rbind.data.frame(GDPHdata,tempdf)
  i<-i+1
}
# View(GDPHdata)

######### Inflation in % GDP ##############

data_INFLA <- read_excel("inflation_deflateur_PIB.xls", sheet = "Data", skip = 3) 
data_INFLA <- data_INFLA[,-3:-24]
names(data_INFLA)[1:2]<-c("Country_Name","Country_Code")
row_data_INFLA_BRICS<- c("Brésil","Fédération de Russie","Inde","Chine","Afrique du Sud")
data_INFLA_BRICS<- data_INFLA[data_INFLA$Country_Name %in% row_data_INFLA_BRICS,]
BenData_BRICS = data_INFLA_BRICS[1,]
INFLAdata <- BenData_BRICS %>% pivot_longer(c(3:ncol(data_INFLA_BRICS)),names_to = "Year", values_to = "Inflation") 
tempdf = data.frame()
i=2
while (i<= nrow(data_INFLA_BRICS) ) {
  tempdf<-data_INFLA_BRICS[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_INFLA_BRICS)),names_to = "Year", values_to = "Inflation")
  INFLAdata <- rbind.data.frame(INFLAdata,tempdf)
  i<-i+1
}
# View(INFLAdata)

######### Private investment ################

data_INVPR <- read_excel("Investissement_privée(FBCF).xls", sheet = "Data", skip = 3) 
data_INVPR <- data_INVPR[,-3:-24]
names(data_INVPR)[1:2]<-c("Country_Name","Country_Code")
row_data_INVPR_BRICS<- c("Brésil","Fédération de Russie","Inde","Chine","Afrique du Sud")
data_INVPR_BRICS<- data_INVPR[data_INVPR$Country_Name %in% row_data_INVPR_BRICS,]
BenData_BRICS = data_INVPR_BRICS[1,]
INVPRdata <- BenData_BRICS %>% pivot_longer(c(3:ncol(data_INVPR_BRICS)),names_to = "Year", values_to = "INPRIV") 
tempdf = data.frame()
i=2
while (i<= nrow(data_INVPR_BRICS) ) {
  tempdf<-data_INVPR_BRICS[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_INVPR_BRICS)),names_to = "Year", values_to = "INPRIV")
  INVPRdata <- rbind.data.frame(INVPRdata,tempdf)
  i<-i+1
}
# View(INVPRdata)

######### Labour force ###############

data_POPACT <- read_excel("population_active.xls", sheet = "Data", skip = 3) 
data_POPACT <- data_POPACT[,-3:-24]
names(data_POPACT)[1:2]<-c("Country_Name","Country_Code")
row_data_POPACT_BRICS<- c("Brésil","Fédération de Russie","Inde","Chine","Afrique du Sud")
data_POPACT_BRICS<- data_POPACT[data_POPACT$Country_Name %in% row_data_POPACT_BRICS,]
BenData_BRICS = data_POPACT_BRICS[1,]
POPACTdata <- BenData_BRICS %>% pivot_longer(c(3:ncol(data_POPACT_BRICS)),names_to = "Year", values_to = "POP_ACT") 
tempdf = data.frame()
i=2
while (i<= nrow(data_POPACT_BRICS) ) {
  tempdf<-data_POPACT_BRICS[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_POPACT_BRICS)),names_to = "Year", values_to = "POP_ACT")
  POPACTdata <- rbind.data.frame(POPACTdata,tempdf)
  i<-i+1
}
# View(POPACTdata)

######### Export of goods and services in GDP % ################

data_EXPIB <- read_excel("Exportation_de_bien_et_services_pourcentage_PIB.xls", sheet = "Data", skip = 3) 
data_EXPIB <- data_EXPIB[,-3:-24]
names(data_EXPIB)[1:2]<-c("Country_Name","Country_Code")
row_data_EXPIB_BRICS<- c("Brésil","Fédération de Russie","Inde","Chine","Afrique du Sud")
data_EXPIB_BRICS<- data_EXPIB[data_EXPIB$Country_Name %in% row_data_EXPIB_BRICS,]
BenData_BRICS = data_EXPIB_BRICS[1,]
EXPIBdata <- BenData_BRICS %>% pivot_longer(c(3:ncol(data_EXPIB_BRICS)),names_to = "Year", values_to = "EXP_PIB") 
tempdf = data.frame()
i=2
while (i<= nrow(data_EXPIB_BRICS) ) {
  tempdf<-data_EXPIB_BRICS[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_EXPIB_BRICS)),names_to = "Year", values_to = "EXP_PIB")
  EXPIBdata <- rbind.data.frame(EXPIBdata,tempdf)
  i<-i+1
}
# View(EXPIBdata)

######### Import of goods and services in GDP % ################

data_IMPIB <- read_excel("importation_des_biens_et_services_pourcentage_PIB.xls", sheet = "Data", skip = 3) 
data_IMPIB <- data_IMPIB[,-3:-24]
names(data_IMPIB)[1:2]<-c("Country_Name","Country_Code")
row_data_IMPIB_BRICS<- c("Brésil","Fédération de Russie","Inde","Chine","Afrique du Sud")
data_IMPIB_BRICS<- data_IMPIB[data_IMPIB$Country_Name %in% row_data_IMPIB_BRICS,]
BenData_BRICS = data_IMPIB_BRICS[1,]
IMPIBdata <- BenData_BRICS %>% pivot_longer(c(3:ncol(data_IMPIB_BRICS)),names_to = "Year", values_to = "IMP_PIB") 
tempdf = data.frame()
i=2
while (i<= nrow(data_IMPIB_BRICS) ) {
  tempdf<-data_IMPIB_BRICS[i,]
  tempdf<-tempdf %>% pivot_longer(c(3:ncol(data_IMPIB_BRICS)),names_to = "Year", values_to = "IMP_PIB")
  IMPIBdata <- rbind.data.frame(IMPIBdata,tempdf)
  i<-i+1
}
# View(IMPIBdata)

############## The degree of openness #####################

OCdata <- cbind.data.frame(IMPIBdata, EXPIBdata[,ncol(EXPIBdata)])
OCdata[is.na(OCdata)] <- 0 # replace na  0 
OCdata$OUVCM <- (OCdata$IMP_PIB + OCdata$EXP_PIB)/2
# View(OCdata)

# Merging databases

Data_BRICS <- cbind.data.frame(GDPHdata, POPACTdata[,ncol(POPACTdata)], INVPRdata[,ncol(INVPRdata)],
                               Epardata[,ncol(Epardata)], Dettdata[,ncol(Dettdata)], Depubdata[,ncol(Depubdata)],
                               OCdata["OUVCM"], INFLAdata[,ncol(INFLAdata)])

# View(Data_BRICS)

########################################################### START ANALYSIS ###########################################################

# Balanced panel data (2005 - 2018 without CIV) and without 2010 because Guinée-Bissau at negative value of brut saving irrelevant for log 

Data_UEMOA <- Data_UEMOA[Data_UEMOA$Country_Code != "CIV",]
Data_UEMOA <- Data_UEMOA[Data_UEMOA$Year %in% c(2005,2006,2007,2008,2009,2011,2012,2013,2014,2015,2016,2017,2018),]
Data_UEMOA <- Data_UEMOA %>% mutate_at(c('GDP_Habitat', 'POP_ACT', 'INPRIV', 'EPARBRUT', 'DEBTEXT', 'DEPUB', 'OUVCM', 'Inflation'), as.numeric)

Data_UEMOA$Code = as.factor(ifelse(Data_UEMOA$Country_Name == "Bénin",1,
                                   ifelse(Data_UEMOA$Country_Name == "Burkina Faso", 2,
                                          ifelse(Data_UEMOA$Country_Name == "Guinée-Bissau", 3,
                                                 ifelse(Data_UEMOA$Country_Name == "Mali", 4,
                                                        ifelse(Data_UEMOA$Country_Name == "Niger", 5,
                                                               ifelse(Data_UEMOA$Country_Name == "Sénégal", 6,7
                                                                            )))))))
Data_UEMOA %>% relocate(c("Code","Year"), .before = Country_Name) -> Data_UEMOA
# View(Data_UEMOA)



Data_BRICS <- Data_BRICS[Data_BRICS$Country_Code != "CIV",]
Data_BRICS <- Data_BRICS[Data_BRICS$Year %in% c(2005,2006,2007,2008,2009,2011,2012,2013,2014,2015,2016,2017,2018),]
Data_BRICS <- Data_BRICS %>% mutate_at(c('GDP_Habitat', 'POP_ACT', 'INPRIV', 'EPARBRUT', 'DEBTEXT', 'DEPUB', 'OUVCM', 'Inflation'), as.numeric)

Data_BRICS$Code = as.factor(ifelse(Data_BRICS$Country_Name == "Afrique du Sud",1,
                                   ifelse(Data_BRICS$Country_Name == "Brésil", 2,
                                          ifelse(Data_BRICS$Country_Name == "Chine", 3,
                                                 ifelse(Data_BRICS$Country_Name == "Fédération de Russie", 4,5
                                                       )))))
Data_BRICS %>% relocate(c("Code","Year"), .before = Country_Name) -> Data_BRICS
# View(Data_BRICS)

######################################### Variable identifies the cross sectional unit = Code and the time unit = Year

########################### Check if is balanced or Unbalanced Panel Data and Obtain characteristics of Data ############################################# 
Data_UEMOA <- pdata.frame(Data_UEMOA, index = c("Country_Name", "Year"))
Data_UEMOA  %>%  is.pbalanced()

Data_BRICS <- pdata.frame(Data_BRICS, index = c("Country_Name", "Year"))
Data_BRICS  %>%  is.pbalanced()

pdim(Data_UEMOA)
pdim(Data_BRICS)

################################### Comparison of economic growth with GDP per capita between the two groups of countries by year
df1 <- Data_UEMOA %>%
  group_by(Year) %>%
  summarise_at(vars(GDP_Habitat), list(Mean_GDPH = mean))

df2 <- Data_BRICS %>%
  group_by(Year) %>%
  summarise_at(vars(GDP_Habitat), list(Mean_GDPH = mean))

df1$Group_Name <- rep("WAEMU", times = nrow(df1))
df2$Group_Name <- rep("BRICS", times = nrow(df2))
Data_graph <- rbind(df1,df2)

ggplotly(ggplot(Data_graph,aes(x = Year, y =Mean_GDPH, fill = Group_Name)) + 
           geom_bar(stat = "identity", position = "dodge") + 
           scale_y_log10(expand = expansion(mult = c(0, 0.10))) + # for disabled the space of 10% on x-axis
           scale_fill_manual(values=c("blue", "darkgreen")) +
           theme(panel.border = element_rect(linetype = "dashed", fill = NA)) +
           labs(x="Year",y="Mean GDP per Capita", caption = "Data source: World Bank data (2022).")+
           theme(legend.background = element_rect(fill="lightgray", 
                                                  size=0.3, linetype="solid"))+
           theme(legend.title = element_text(face = "bold"))+
           theme(plot.title = element_text(size = 10, face = "bold")) +
           ggtitle("Evolution of average GDP per capita (in dollars) in WAEMU and BRICS countries between 2005 and 2018"))

# We performed a test for Stationarity of the logarithmic variables and a descriptive analysis of stationary variables.
# Since inflation and trade openness are percentage variables, they will not be evaluated in logarithm.
##### WAEMU
Data_UEMOA$GDP_Habitat <- log(Data_UEMOA$GDP_Habitat)
Data_UEMOA$POP_ACT <- log(Data_UEMOA$POP_ACT)
Data_UEMOA$INPRIV <- log(Data_UEMOA$INPRIV)
Data_UEMOA$EPARBRUT <- log(Data_UEMOA$EPARBRUT)
Data_UEMOA$DEBTEXT <- log(Data_UEMOA$DEBTEXT)
Data_UEMOA$DEPUB <- log(Data_UEMOA$DEPUB)

##### BRICS
Data_BRICS$GDP_Habitat <- log(Data_BRICS$GDP_Habitat)
Data_BRICS$POP_ACT <- log(Data_BRICS$POP_ACT)
Data_BRICS$INPRIV <- log(Data_BRICS$INPRIV)
Data_BRICS$EPARBRUT <- log(Data_BRICS$EPARBRUT)
Data_BRICS$DEBTEXT <- log(Data_BRICS$DEBTEXT)
Data_BRICS$DEPUB <- log(Data_BRICS$DEPUB)


############################ Stationarity Test #########################################################################
# H0: There is unit root in variable or Variable is non stationarity
# significative level 5%
################ Unit root test ########################################################################################

################################### Im-Pesaran-Shin Unit-Root Test #####################################################

##### WAEMU
GDP_Habitat_IPS <- data.frame(split(Data_UEMOA$GDP_Habitat, Data_UEMOA$Country_Name))
purtest(GDP_Habitat_IPS, pmax = 2, exo = "intercept", test = "ips") #non-stationary p-value = 0.1388

POP_ACT_IPS <- data.frame(split(Data_UEMOA$POP_ACT, Data_UEMOA$Country_Name))
purtest(POP_ACT_IPS, pmax = 2, exo = "intercept", test = "ips") #non-stationary p-value = 1

INPRIV_IPS <- data.frame(split(Data_UEMOA$INPRIV, Data_UEMOA$Country_Name))
purtest(INPRIV_IPS, pmax = 2, exo = "intercept", test = "ips") #non-stationary p-value = 0.2489

EPARBRUT_IPS <- data.frame(split(Data_UEMOA$EPARBRUT, Data_UEMOA$Country_Name))
purtest(EPARBRUT_IPS, pmax = 2, exo = "intercept", test = "ips") #non-stationary p-value = 0.3887

DEBTEXT_IPS <- data.frame(split(Data_UEMOA$DEBTEXT, Data_UEMOA$Country_Name))
purtest(DEBTEXT_IPS, pmax = 2, exo = "intercept", test = "ips") #non-stationary p-value = 0.9195

DEPUB_IPS <- data.frame(split(Data_UEMOA$DEPUB, Data_UEMOA$Country_Name))
purtest(DEPUB_IPS, pmax = 2, exo = "intercept", test = "ips") #non-stationary p-value = 0.2446

OUVCM_IPS <- data.frame(split(Data_UEMOA$OUVCM, Data_UEMOA$Country_Name))
purtest(OUVCM_IPS, pmax = 2, exo = "intercept", test = "ips") #stationary p-value = 0.0004958

Inflation_IPS <- data.frame(split(Data_UEMOA$Inflation, Data_UEMOA$Country_Name))
purtest(Inflation_IPS, pmax = 2, exo = "intercept", test = "ips") #stationary p-value = 2.71e-05

# WAEMU Non-Stationary variables : 'GDP_Habitat','POP_ACT', 'INPRIV', 'EPARBRUT', 'DEPUB' and 'DEBTEXT'

##### BRICS

GDP_Habitat_IPS <- data.frame(split(Data_BRICS$GDP_Habitat, Data_BRICS$Country_Name))
purtest(GDP_Habitat_IPS, pmax = 2, exo = "trend", test = "ips") #non-stationary p-value = 0.6065

POP_ACT_IPS <- data.frame(split(Data_BRICS$POP_ACT, Data_BRICS$Country_Name))
purtest(POP_ACT_IPS, pmax = 2, exo = "trend", test = "ips") #non-stationary p-value = 0.2817

INPRIV_IPS <- data.frame(split(Data_BRICS$INPRIV, Data_BRICS$Country_Name))
purtest(INPRIV_IPS, pmax = 2, exo = "trend", test = "ips") #non-stationary p-value = 0.474

EPARBRUT_IPS <- data.frame(split(Data_BRICS$EPARBRUT, Data_BRICS$Country_Name))
purtest(EPARBRUT_IPS, pmax = 2, exo = "trend", test = "ips") #non-stationary p-value = 0.4467

DEBTEXT_IPS <- data.frame(split(Data_BRICS$DEBTEXT, Data_BRICS$Country_Name))
purtest(DEBTEXT_IPS, pmax = 2, exo = "trend", test = "ips") #non-stationary p-value = 0.9233

DEPUB_IPS <- data.frame(split(Data_BRICS$DEPUB, Data_BRICS$Country_Name))
purtest(DEPUB_IPS, pmax = 2, exo = "trend", test = "ips") #non-stationary p-value = 0.8208

OUVCM_IPS <- data.frame(split(Data_BRICS$OUVCM, Data_BRICS$Country_Name))
purtest(OUVCM_IPS, pmax = 2, exo = "trend", test = "ips") #stationary p-value = 0.0104

Inflation_IPS <- data.frame(split(Data_BRICS$Inflation, Data_BRICS$Country_Name))
purtest(Inflation_IPS, pmax = 2, exo = "trend", test = "ips") #stationary p-value = 6e-04

# BRICS Non-Stationary variables : 'GDP_Habitat','POP_ACT', 'INPRIV', 'EPARBRUT', 'DEPUB' and 'DEBTEXT'

# Since the degree of trade openness (OUVCM) and inflation (Inflation) are stationary in the BRICS and WAEMU data, 
# we will remove them from our model to keep only non-stationary variables in our model.

Data_UEMOA <- subset(Data_UEMOA, select = -c(OUVCM,Inflation))
Data_BRICS <- subset(Data_BRICS, select = -c(OUVCM,Inflation))

# WAEMU Descriptive analysis
stargazer(Data_UEMOA, title="Descriptive Statistic ", type = "text")

scatterplot(GDP_Habitat ~ Year | Country_Name, boxplot = FALSE,
            smooth = TRUE, reg.line = TRUE, data = Data_UEMOA)

scatterplot(POP_ACT ~ Year | Country_Name, boxplot = FALSE,
            smooth = TRUE, reg.line = TRUE, data = Data_UEMOA)

scatterplot(INPRIV ~ Year | Country_Name, boxplot = FALSE,
            smooth = TRUE, reg.line = TRUE, data = Data_UEMOA)

scatterplot(EPARBRUT ~ Year | Country_Name, boxplot = FALSE,
            smooth = TRUE, reg.line = TRUE, data = Data_UEMOA)

scatterplot(DEBTEXT ~ Year | Country_Name, boxplot = FALSE,
            smooth = TRUE, reg.line = TRUE, data = Data_UEMOA)

scatterplot(DEPUB ~ Year | Country_Name, boxplot = FALSE,
            smooth = TRUE, reg.line = TRUE, data = Data_UEMOA)

# BRICS Descriptive analysis
stargazer(Data_BRICS, title="Descriptive Statistic ", type = "text")

scatterplot(GDP_Habitat ~ Year | Country_Name, boxplot = FALSE,
            smooth = TRUE, reg.line = TRUE, data = Data_BRICS)

scatterplot(POP_ACT ~ Year | Country_Name, boxplot = FALSE,
            smooth = TRUE, reg.line = TRUE, data = Data_BRICS)

scatterplot(INPRIV ~ Year | Country_Name, boxplot = FALSE,
            smooth = TRUE, reg.line = TRUE, data = Data_BRICS)

scatterplot(EPARBRUT ~ Year | Country_Name, boxplot = FALSE,
            smooth = TRUE, reg.line = TRUE, data = Data_BRICS)

scatterplot(DEBTEXT ~ Year | Country_Name, boxplot = FALSE,
            smooth = TRUE, reg.line = TRUE, data = Data_UEMOA)

scatterplot(DEPUB ~ Year | Country_Name, boxplot = FALSE,
            smooth = TRUE, reg.line = TRUE, data = Data_BRICS)


# We are going to regress economic growth per capita on the private investment,labor force, gross savings,public 
# expenditure and public debts of the country

################################## WAEMU ################################

# Model estimation using the lm() function
reg1 <- lm(data = Data_UEMOA, GDP_Habitat ~ factor(Code) + POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT)
summary(reg1)

# Using plm, we can estimate the within model, Numerically equivalent to fixed effects
within_reg1 = plm(GDP_Habitat ~ POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT, data=Data_UEMOA, model="within")
summary(within_reg1)
#Estimate the within model with two effects individuals and times
within_reg1.tways = plm(GDP_Habitat ~ POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT, data=Data_UEMOA, model="within", effect="twoways")
summary(within_reg1.tways)
#Estimate the pooling model
pooling_reg1 = plm(GDP_Habitat ~ POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT, data=Data_UEMOA, model="pooling")
summary(pooling_reg1)
# We can also estimate the first difference model:
fd_reg1 = plm(GDP_Habitat ~ POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT, data=Data_UEMOA, model="fd")
summary(fd_reg1)

# Before estimating the random effects model, we will check for the presence or absence of individual or 
# time effects in our model using Fisher's test; we can also do this using the Breush-Pagan test, but it is the same thing.

# The assumption of no individuals and times effects
pFtest(within_reg1.tways, pooling_reg1) # p-value < 2.2e-16 hypothesis strongly rejected

# The assumption of no times effects, but assuming that there are individuals effects
pFtest(within_reg1.tways, within_reg1) # p-value = 1.915e-05 hypothesis strongly rejected

# So the best within model is the model with two effects : individuals and times effects 

#We can also estimate the random effect model with two effects
re_reg1 = plm(GDP_Habitat ~ POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT, data=Data_UEMOA, effect="twoways", model="random")
summary(re_reg1)





########################################### Model Summary WAEMU ##############################################################
stargazer(reg1, fd_reg1, pooling_reg1, within_reg1.tways, re_reg1,
          dep.var.caption="",dep.var.labels="GDP per Capita",
          model.numbers = FALSE,
          column.labels = c("OLS","First difference","Pooling","Within twoways","GLS"),
          omit.table.layout = "n", star.cutoffs = NA,keep.stat=c("rsq","n"),no.space=TRUE,
          header=FALSE,
          keep=c("POP_ACT","INPRIV","EPARBRUT","DEPUB","DEBTEXT"),
          title="Summary of estimated models", type="text"
)

############################################Implementing a Hausman test for WAEMU ##############################################################
phtest(within_reg1.tways,re_reg1)

#Under the null hypothesis of no correlation between the regressors and the individual effects cov(alpha_i,x_it)=0,
# the statistics is distributed as a Chi-2 with fives degrees of freedom.This hypothesis, with a p-value of 3.268e-16
# is rejected at the 5% confidence level.
# We therefore return the model with the fixed effects. In this case, the Within estimator with two effects is BLUE and better than the GLS estimator which is not consistent.

################################## BRICS ################################

# Model estimation using the lm() function
reg2 <- lm(data = Data_BRICS, GDP_Habitat ~ factor(Code) + POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT)
summary(reg2)
# Using plm, we can estimate the within model, Numerically equivalent to fixed effects
within_reg2 = plm(GDP_Habitat ~ POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT, data=Data_BRICS, model="within")
summary(within_reg2)
#Estimate the within model with two effects individuals and times
within_reg2.tways = plm(GDP_Habitat ~ POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT, data=Data_BRICS, model="within", effect="twoways")
summary(within_reg2.tways)
#Estimate the pooling model
pooling_reg2 = plm(GDP_Habitat ~ POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT, data=Data_BRICS, model="pooling")
summary(pooling_reg2)
# We can also estimate the first difference model:
fd_reg2 = plm(GDP_Habitat ~ POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT, data=Data_BRICS, model="fd")
summary(fd_reg2)

# Before estimating the random effects model, we will check for the presence or absence of individual or 
# time effects in our model using Fisher's test; we can also do this using the Breush-Pagan test, but it is the same thing.

# The assumption of no individuals and times effects
pFtest(within_reg2.tways, pooling_reg2) # p-value = 7.678e-05 hypothesis strongly rejected

# The assumption of no times effects, but assuming that there are individuals effects
pFtest(within_reg2.tways, within_reg2) # p-value = 0.7135 hypothesis is not rejected

# So the best within model is the model with only individuals effects. 
within_reg2.ind = plm(GDP_Habitat ~ POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT, data=Data_BRICS, model="within", effect="individual")
summary(within_reg2.ind)

#We can also estimate the random effect model
# we can't use swar default random.method of error components model because the numbers of parameters included intercept is greater than
# the number of individus so we use the amemiya random method with 3 degree of freedom which means that the unbiased version of 
# the estimation of the error components is used.

re_reg2 = plm(GDP_Habitat ~ POP_ACT + INPRIV + EPARBRUT + DEPUB + DEBTEXT, data=Data_BRICS, model="random", effect="individual",
              random.method = "amemiya", random.dfcor = 3)
summary(re_reg2)


################################################## Model Summary BRICS ##############################################################

stargazer(reg2, fd_reg2, pooling_reg2, within_reg2.ind, re_reg2, 
          dep.var.caption="",dep.var.labels="GDP per Capita",
          model.numbers = FALSE,
          column.labels = c("OLS","First difference","Pooling","Within individuals", "GLS"),
          omit.table.layout = "n", star.cutoffs = NA,keep.stat=c("rsq","n"),no.space=TRUE,
          header=FALSE,
          keep=c("POP_ACT","INPRIV","EPARBRUT","DEPUB","DEBTEXT"),
          title="Summary of estimated models", type="text"
)

################################################### Implementing a Hausman test for BRICS ##############################################################
phtest(within_reg2.ind,re_reg2)

#Under the null hypothesis of no correlation between the regressors and the individual effects cov(alpha_i,x_it)=0,
# the statistics is distributed as a Chi-2 with fives degrees of freedom because 5 parameters without alpha_i.This hypothesis, with a p-value of 0.7752
# is not rejected at the 5% confidence level.
# We therefore return the random effects/Error Components Model.

############################# Heteroscedasticity  Breush-Pagan Test  ###############################################################################
# H0 : There is Homoscedasticity
bptest(formula = within_reg1.tways , studentize = F) #p-value = 0.0001135  rejected

bptest(formula = re_reg2 , studentize = F) #p-value = 0.1905 not rejected

############################# Durbin- Watson Test for serial correlation in panel models #############################################################
#H0 : There is no Autocorrelation in error term
pdwtest(re_reg2) # p-value = 8.188e-12 rejected

# To confirm the Durbin-Watson Test for serial correlation we perform the Breusch-Godfrey/Wooldridge test for serial correlation in panel models
# Breusch-Godfrey/Wooldridge test for serial correlation in panel models. Test based on the selected fixed effects model.
#H0 : There is no Autocorrelation or serial correlation in error term
pbgtest(within_reg2.ind) #p-value = 0.0003539 rejeted

#################################################### END CODE ####################################################################################

