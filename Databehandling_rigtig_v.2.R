library("tidyverse")
library("ggplot2")
library("stringr")
library("foreign")
library("dplyr")
library("readxl")
library("mapproj")
library("geosphere")
library("sp")
library("stargazer")
library("gridExtra")
library("mapDK")
library("ggrepel") #Indllser relevante pakker 

setwd("/Users/LasseJensen/Documents/Statskundskab/Kandidat/11. semester - speciale/Data") #Sætter sti

load("data_samlet_udenFV19.RData") #Load hele datasættet

### INDLÆSER ALLE DATASÆT

valgdata_0115 <- read_xlsx("Valgdata_2001_2015.xlsx") #Valgresultater fra 2001-2015
  
valgdata_2019 <- read_xlsx("FV_2019.xlsx") %>%
  filter(Resultattype == "AfstemningsomrÂde") #Tager kun data om valgdistrikter med

geo_data <- read_xlsx("Geografiske_stamdata.xlsx") %>%
  rename(Sted = `Valgsted navn`) #Omdøber mhp. merge

asylcentre <- read_xlsx("Asyl_FV2001-2019.xlsx") %>%
  filter(FV != 2019) #Sorterer 2019 fra for nu, indtil valgdata for 2019 foreligger

sim_kontrol <- read_xlsx("SIM_nøgletal.xlsx") %>%
  dplyr::select(-("2019")) #Sletter 2019 indtil 2019 foreligger

View(sim_kontrol)
#valgdatasamlet <- left_join(valgdata_2019, geo_data, by = "Sted")

### MERGER
  
if(!require("devtools")) install.packages("devtools")
devtools::install_github("sebastianbarfort/mapDK", force = T) #Installerer Danmarkskort

valgdistr_geo <- polling %>% #"polling" kommer fra mapDK og er alle valgdistrikter optegnet geografisk
  unite(mlregn, c("KommuneNum","AfstemKod"), remove = F) %>% #Samler KommueNum og AfstemKod, da de udgør valgID
  mutate(ValgstedId = str_replace(mlregn, "_", "0")) %>% #Indsætter 0 ml. kommunenr. og afstemnkode for at få valgID
  select(-mlregn) #Sletter mellemregningsvariabel

valg0115_geo <- left_join(valgdistr_geo, valgdata_0115, by = "ValgstedId") #Samler valgdata med geografisk data om valgdistrikter

### Måler afstande
kunlonglat <- valgdistr_geo %>% #Kan kun måle afstande med in input fra dataframe, der har to kolonner
  transmute(long, lat) #Fjerner derfor alt andet end long og lat

#Deler asylcenterdata op i 5; én dataframe for hvert valg. 

asylcentre01 <- asylcentre %>%
  filter(FV == 2001) %>%
  transmute(long, lat) %>%
  transform(long = as.numeric(long),
            lat = as.numeric(lat))

asylcentre05 <- asylcentre %>%
  filter(FV == 2005) %>%
  transmute(long, lat) %>%
  transform(long = as.numeric(long),
            lat = as.numeric(lat))

asylcentre07 <- asylcentre %>%
  filter(FV == 2007) %>%
  transmute(long, lat) %>%
  transform(long = as.numeric(long),
            lat = as.numeric(lat))

asylcentre11 <- asylcentre %>%
  filter(FV == 2011) %>%
  transmute(long, lat) %>%
  transform(long = as.numeric(long),
            lat = as.numeric(lat))

asylcentre15 <- asylcentre %>%
  filter(FV == 2015) %>%
  transmute(long, lat) %>%
  transform(long = as.numeric(long),
            lat = as.numeric(lat))

dist_asyl_01 <- distm(kunlonglat, asylcentre01, fun = distHaversine) #Måler distance mellem alle valgdistrikter og asylcenter i 2001
dist_asyl_05 <- distm(kunlonglat, asylcentre05, fun = distHaversine) #... 2005
dist_asyl_07 <- distm(kunlonglat, asylcentre07, fun = distHaversine) #... 2007
dist_asyl_11 <- distm(kunlonglat, asylcentre11, fun = distHaversine) #... 2011
dist_asyl_15 <- distm(kunlonglat, asylcentre15, fun = distHaversine) #... 2015

dist_asyl_01 <- (dist_asyl_01[,1:58]/(1000)) %>%
  as_tibble()
dist_asyl_05 <- (dist_asyl_05[,1:19]/(1000)) %>%
  as_tibble()
dist_asyl_07 <- (dist_asyl_07[,1:14]/(1000)) %>%
  as_tibble()
dist_asyl_11 <- (dist_asyl_11[,1:14]/(1000)) %>%
  as_tibble()
dist_asyl_15 <- (dist_asyl_15[,1:48]/(1000)) %>%
  as_tibble() #Dividerer alle distancer med 1000 for at konvertere til meter og lave til tibble

FV2001 <- valg0115_geo %>%
  select(-contains(c("FV2015"))) %>%
  select(-contains(c("FV2005"))) %>%
  select(-contains(c("FV2007"))) %>%
  select(-contains(c("FV2011"))) # Sletter alle andre valg end FV2001

FV2005 <- valg0115_geo %>%
  select(-contains(c("FV2001"))) %>%
  select(-contains(c("FV2011"))) %>%
  select(-contains(c("FV2007"))) %>%
  select(-contains(c("FV2015"))) # Sletter alle andre valg end FV2005

FV2007 <- valg0115_geo %>%
  select(-contains(c("FV2001"))) %>%
  select(-contains(c("FV2005"))) %>%
  select(-contains(c("FV2011"))) %>%
  select(-contains(c("FV2015"))) # Sletter alle andre valg end FV2007

FV2011 <- valg0115_geo %>%
  select(-contains(c("FV2001"))) %>%
  select(-contains(c("FV2005"))) %>%
  select(-contains(c("FV2007"))) %>%
  select(-contains(c("FV2015"))) # Sletter alle andre valg end FV2011

FV2015 <- valg0115_geo %>%
  select(-contains(c("FV2001"))) %>%
  select(-contains(c("FV2005"))) %>%
  select(-contains(c("FV2007"))) %>%
  select(-contains(c("FV2011"))) # Sletter alle andre valg end FV2011

fvasyl2001 <- bind_cols(dist_asyl_01, FV2001)
fvasyl2005 <- bind_cols(dist_asyl_05, FV2005)
fvasyl2007 <- bind_cols(dist_asyl_07, FV2007)
fvasyl2011 <- bind_cols(dist_asyl_11, FV2011)
fvasyl2015 <- bind_cols(dist_asyl_15, FV2015) #Sammensætter data igen for hvert folketingsvalg

fvasyl2001 <- fvasyl2001 %>% #2001
  mutate(`FV2001 - kortestedist` = apply(fvasyl2001[, 1:58], 1, min), #Laver ny variabel, der er den korteste distance 
         `FV2001 - gnsdist` = apply(fvasyl2001[, 1:58], 1, mean),
         `FV2001 - radius5` = rowSums(fvasyl2001[, 1:58] < 5),
         `FV2001 - radius10` = rowSums(fvasyl2001[, 1:58] < 10),
         `FV2001 - radius20` = rowSums(fvasyl2001[, 1:58] < 20)) %>% 
  select(-(1:58)) %>% #Sletter vektor for hver enkel asylceter
  group_by(ValgstedId) %>% #Grupperer indenfor valgsteder
  slice(which.min(`FV2001 - kortestedist`)) #Beholder kun observation inden for hvert valgdistrikt med kortest distance til nærmeste valgdistrikt

fvasyl2005 <- fvasyl2005 %>% #2005
  mutate(`FV2005 - kortestedist` = apply(fvasyl2005[, 1:19], 1, min), 
         `FV2005 - gnsdist` = apply(fvasyl2005[, 1:19], 1, mean),
         `FV2005 - radius5` = rowSums(fvasyl2005[, 1:19] < 5),
         `FV2005 - radius10` = rowSums(fvasyl2005[, 1:19] < 10),
         `FV2005 - radius20` = rowSums(fvasyl2005[, 1:19] < 20)) %>% 
  select(-(1:19)) %>% 
  group_by(ValgstedId) %>% 
  slice(which.min(`FV2005 - kortestedist`))

fvasyl2007 <- fvasyl2007 %>% #2007
  mutate(`FV2007 - kortestedist` = apply(fvasyl2007[, 1:14], 1, min), 
         `FV2007 - gnsdist` = apply(fvasyl2007[, 1:14], 1, mean),
         `FV2007 - radius5` = rowSums(fvasyl2007[, 1:14] < 5),
         `FV2007 - radius10` = rowSums(fvasyl2007[, 1:14] < 10),
         `FV2007 - radius20` = rowSums(fvasyl2007[, 1:14] < 20)) %>% 
  select(-(1:14)) %>% 
  group_by(ValgstedId) %>% 
  slice(which.min(`FV2007 - kortestedist`))

fvasyl2011 <- fvasyl2011 %>% #2011
  mutate(`FV2011 - kortestedist` = apply(fvasyl2011[, 1:14], 1, min), 
         `FV2011 - gnsdist` = apply(fvasyl2011[, 1:14], 1, mean),
         `FV2011 - radius5` = rowSums(fvasyl2011[, 1:14] < 5),
         `FV2011 - radius10` = rowSums(fvasyl2011[, 1:14] < 10),
         `FV2011 - radius20` = rowSums(fvasyl2011[, 1:14] < 20)) %>% 
  select(-(1:14)) %>% 
  group_by(ValgstedId) %>% 
  slice(which.min(`FV2011 - kortestedist`))

fvasyl2015 <- fvasyl2015 %>% #2015
  mutate(`FV2015 - kortestedist` = apply(fvasyl2015[, 1:48], 1, min), #Korteste distance
         `FV2015 - gnsdist` = apply(fvasyl2015[, 1:48], 1, mean), #Gennemsnitlige
         `FV2015 - radius5` = rowSums(fvasyl2015[, 1:48] < 5), #Antal centre inden for en radius af 5 km.
         `FV2015 - radius10` = rowSums(fvasyl2015[, 1:48] < 10), #... 10 km.
         `FV2015 - radius20` = rowSums(fvasyl2015[, 1:48] < 20)) %>% #... 20 km.
  select(-(1:48)) %>% 
  group_by(ValgstedId) %>% 
  slice(which.min(`FV2015 - kortestedist`))

geo_data$ValgstedId <- as.numeric(geo_data$ValgstedId)
geo_data$KredsNr <- as.numeric(geo_data$KredsNr)
geo_data$StorKredsNr <- as.numeric(geo_data$StorKredsNr)
geo_data$LandsdelsNr <- as.numeric(geo_data$LandsdelsNr) #Ændrer alle fire identifikationsvariable til samme type som valgstedsdata  

fvasyl_samlet <- fvasyl2015 %>% 
  left_join(.,fvasyl2011, by = c("ValgstedId", "Gruppe")) %>%
  select(-contains(".y")) %>%
  left_join(.,fvasyl2007, by = c("ValgstedId", "Gruppe")) %>%
  select(-contains(".y")) %>%
  left_join(.,fvasyl2005, by = c("ValgstedId", "Gruppe")) %>%
  select(-contains(".y")) %>%
  left_join(.,fvasyl2001, by = c("ValgstedId", "Gruppe")) %>%
  select(-contains(".y")) %>%
  select(-ends_with(".x.x")) %>%
  select(-c("long", "lat", "group", "AfstemKod",
            "id", "KommuneNum", "KommuneNav", "OpstilNum",
            "OpstilNav", "KredsNr", "StorKredsNr", "LandsdelsNr")) %>%
  gather(., variabel, vaerdi, 15:134) %>%
  mutate(FV = str_sub(variabel, 3, 6), #Ny variabel med character nr. 3-6 (begge inkl.) for at lave aar
         parti = str_sub(variabel, 9)) %>%
  select(-(variabel)) %>%
  spread(., parti, vaerdi) %>%
  arrange(ValgstedId, FV) %>%
  rename(KredsNr = KredsNr.x,
         StorKredsNr = StorKredsNr.x,
         LandsdelsNr = LandsdelsNr.x,
         long = long.x,
         lat = lat.x,
         group = group.x,
         AfstemKod = AfstemKod.x,
         id = id.x,
         KommuneNum = KommuneNum.x,
         KommuneNav = KommuneNav.x,
         OpstilNum = OpstilNum.x,
         OpstilNav = OpstilNav.x) %>%
  transform(ValgstedId = as.numeric(ValgstedId)) %>%
  left_join(geo_data, by = c("ValgstedId", "KredsNr", "StorKredsNr", "LandsdelsNr")) %>% #Merger valgdata med geografiske data
  as_tibble() #Laver dataframe til en tibble for håndtering i tidyverse

parti_stemmekolonne <- c(17, 19:22, 25, 27:32, 36:37, 39) #Identicerer partivariable

for(col in names(fvasyl_samlet)[parti_stemmekolonne]) {
  fvasyl_samlet[paste0(col, "_pct")] = round(100*fvasyl_samlet[col]/fvasyl_samlet$X.Afgivne.stemmer,2)
} #Omregner til pct. 

### BAGGRUNDSVARIABLE

sim_kontrol_lang <- gather(sim_kontrol, "aar", "vaerdi", 4:8) %>%
  spread("Variabel", "vaerdi") %>% #Gør data tidy
  rename(KommuneNum = Kom.nr,
         FV = aar) #Loader baggrundsvariable. 

sim_kontrol_lang$FV <- as.numeric(sim_kontrol_lang$FV) #Laver FV numeric

### MERGE BAGGRUNDSVARIABLE MED DATASÆT

fvasyl_samlet$KommuneNum <- as.numeric(fvasyl_samlet$KommuneNum)
fvasyl_samlet$FV <- as.numeric(fvasyl_samlet$FV)

data <- left_join(fvasyl_samlet, sim_kontrol_lang, by = c("KommuneNum", "FV"), suffix = c("SLET1", "SLET2")) #Samler datasæt

saveRDS(data, file = "data_samlet_udenFV19.rds") 
save(data, file = "data_samlet_udenFV19.RData") #Gemmer

### Laver blokvariabel og valgdeltagelse

data <- data %>%
  mutate(valgdeltagelse = round(X.Afgivne.stemmer/data$X.Stemmeberettigede*100, 2))

data$blaa <- rowSums(data[,c("X.Dansk.Folkeparti_pct", "X.Fremskridtspartiet_pct")], na.rm=TRUE) #Højrenationale partier 
data$roed <- rowSums(data[,c("X.Enhedslisten_pct", "X.Socialistisk.Folkeparti_pct",
                             "X.Socialdemokratiet_pct", "X.Radikale.Venstre_pct")], na.rm=TRUE) #Røde blok

### Laver data om flytninger

flytning <- read_xlsx("tilogfraflytning.xlsx")

flytning_lang <- gather(flytning, "FV", "nettotilflytning", 2:14) %>%
  filter(FV == 2007 | FV == 2011 | FV == 2015) %>%
  transform(KommuneNum = as.numeric(KommuneNum))

data_flytning <- data %>%
  dplyr::select(KommuneNum, ValgstedId, KommuneNav, FV, X.kortestedist, X.Dansk.Folkeparti_pct,
         blaa, roed, valgdeltagelse, X.radius10, X.radius20, X.radius5,
         andel_ejerbolig, andel_videregudd, areal_km2, bef_tæt,
         befandel_by, ikke_vestlige, indb_tal, ledige_pr.100,
         serviceudg, socioøkonomi, tyveri_indbrud) 

flytning_lang$KommuneNum <- as.numeric(flytning_lang$KommuneNum)
flytning_lang$FV <- as.numeric(flytning_lang$FV)

data_flytning$KommuneNum <- as.numeric(data_flytning$KommuneNum)
data_flytning$FV <- as.numeric(data_flytning$FV)

data_flytning <- data_flytning %>%
  left_join(flytning_lang, by = c("KommuneNum", "FV")) %>%
  filter(FV == 2007 | FV == 2011 | FV == 2015) ### DONE

### DATASÆT TIL BRUG FOR NETTOTILFLYTNING

flytning <- read_xlsx("tilogfraflytning.xlsx")

flytning_lang <- gather(flytning, "FV", "nettotilflytning", 2:14) %>%
  filter(FV == 2007 | FV == 2011 | FV == 2015) %>%
  transform(KommuneNum = as.numeric(KommuneNum))

data_flytning <- data %>%
  dplyr::select(KommuneNum, ValgstedId, KommuneNav, FV, X.kortestedist, X.Dansk.Folkeparti_pct,
         blaa, roed, valgdeltagelse, X.radius10, X.radius20, X.radius5,
         andel_ejerbolig, andel_videregudd, areal_km2, bef_tæt,
         befandel_by, ikke_vestlige, indb_tal, ledige_pr.100,
         serviceudg, socioøkonomi, tyveri_indbrud) 

flytning_lang$KommuneNum <- as.numeric(flytning_lang$KommuneNum)
flytning_lang$FV <- as.numeric(flytning_lang$FV)

data_flytning$KommuneNum <- as.numeric(data_flytning$KommuneNum)
data_flytning$FV <- as.numeric(data_flytning$FV)

data_flytning_2 <- data_flytning %>%
  filter(FV == 2007 | FV == 2011 | FV == 2015) %>%
  group_by(KommuneNum, FV) %>%
  dplyr::summarize(gnsdistance = mean(X.kortestedist, na.rm = T)) %>%
  mutate(treatment5 = if_else(gnsdistance <= 5, 1, 0),
         treatment10 = if_else(gnsdistance <= 10, 1, 0),
         treatment15 = if_else(gnsdistance <= 15, 1, 0),
         treatment20 = if_else(gnsdistance <= 20, 1, 0),
         treatment30 = if_else(gnsdistance <= 30, 1, 0),
         treatment50 = if_else(gnsdistance <= 50, 1, 0), #Forskellige afgrænsninger
         treatment1 = if_else(gnsdistance <= 1, 1, 0)) %>%
  left_join(flytning_lang, by = c("KommuneNum", "FV")) #Samler

data_flytning_2$treatment5 <- as.factor(data_flytning_2$treatment5)
data_flytning_2$treatment10 <- as.factor(data_flytning_2$treatment10)

### SLUT
