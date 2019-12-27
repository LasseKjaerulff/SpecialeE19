library("broom")
library("sandwich")
library("ggpubr") 
library("stargazer") 
library("ggplot2") #Loader relevante pakker

### OVERSIGT FIGURER ### 

figur1 #Figur 1
udv_antalcentre # Figur 2
grid.arrange(p_valgkredse_kommuner, danskfolkeparti, ncol = 2) #Figur 3
asyl_beftaet_FV #Figur 4
hypotese1_trendfigur2 #Figur 5
enosplot #Figur 6
distancefordeling_simpel #Figur 7
grid.arrange(flytteplot5km, flytteplot10km, ncol = 2,
             left = "Nettotilflytning",
             top = grid::textGrob("Til- og fraflytning fra kommuner med forskellig afstand til nærmeste asylcenter",x=0,hjust=0)) #Figur 8
enosplot_valg #Figur 9
enosplot_int #Figur 10
enosplot_int_krimi #Figur 11

######################################
############## TABELLER ############## 
######################################

### Tabel 5: Deskriptiv statistik

deskriptivstat <- data %>%
  dplyr::select(X.Dansk.Folkeparti_pct, roed, blaa, valgdeltagelse, X.kortestedist, X.radius10,
                X.radius20, X.radius5, bef_tæt, ikke_vestlige, ledige_pr.100, tyveri_indbrud, FV, andel_videregudd, 
                andel_ejerbolig, serviceudg) %>%
  mutate(ledige_pr.100 = as.numeric(ledige_pr.100),
         tyveri_indbrud = as.numeric(tyveri_indbrud),
         valgdeltagelse = as.numeric(valgdeltagelse),
         X.kortestedist = as.numeric(X.kortestedist),
         bef_tæt = as.numeric(bef_tæt),
         ikke_vestlige = as.numeric(ikke_vestlige),
         ledige_pr.100 = as.numeric(ledige_pr.100),
         andel_videregudd = as.numeric(andel_videregudd),
         andel_ejerbolig = as.numeric(andel_ejerbolig),
         serviceudgifter = as.numeric(serviceudg)) %>%
  filter(FV != 2019) %>%
  as.data.frame()


stargazer::stargazer(deskriptivstat, type = "html", out = "deskriptiv statistik.txt", digits = 1,
                     #covariate.labels = c("Dansk Folkeparti (valgkredse, %)", "Vaerdipolitisk hoejreorienterede (valgkredse, &)",
                     #"Valgdeltagelse (valgkredse, %)", "Distance (valgkredse", "Antal centre (radius 10 km, valgkredse)",
                     #"Antal centre (radius 20 km, valgkredse)", "Antal centre (radius 5 km, valgkredse)",
                     #"Befolkningstaethed (kommune, indb/km2)", "Andel ikke-vestlige indvandrere (kommune)",
                     #"Ledige pr. 100 (kommune)", "Anmeldte tyverier/indbrud pr. 1000 (kommune)", 
                     #"Andel videregaaende uddanelse (kommune)",
                     #"Andel i ejerboliger", "Serviceudgifter (kommune)"),
                     omit.summary.stat = "n",
                     omit = "FV") #Tabel 5

### TABEL 6: Hypotese 1

m1 <- lm(blaa ~ X.kortestedist, data = data)
m2 <- lm(blaa ~ X.kortestedist + factor(KommuneNr) + factor(FV), data = data)
m3 <- lm(blaa ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
           bef_tæt + as.numeric(serviceudg) + factor(KommuneNr), data = data)

m3_1 <- lm(blaa ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
             bef_tæt + as.numeric(serviceudg) + factor(FV), data = data)

m3_2 <- lm(blaa ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
             bef_tæt + as.numeric(serviceudg) + factor(KommuneNr) + factor(FV), data = data)

m4 <- lm(blaa ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
           bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
           as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data)

m4_df <- lm(X.Dansk.Folkeparti_pct ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
              as.numeric(bef_tæt) + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
              as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data)

clustervcovm1 <- tidy(coeftest(m1,vcovHC(m1,cluster="group")))
clustervcovm2 <- tidy(coeftest(m2,vcovHC(m2,cluster="group")))
clustervcovm3 <- tidy(coeftest(m3,vcovHC(m3,cluster="group")))
clustervcovm3_1 <- tidy(coeftest(m3_1,vcovHC(m3_1,cluster="group")))
clustervcovm3_2 <- tidy(coeftest(m3_2,vcovHC(m3_2,cluster="group")))
clustervcovm4 <- tidy(coeftest(m4,vcovHC(m4,cluster="group")))
clustervcovm4_df <- tidy(coeftest(m4_df,vcovHC(m4_df,cluster="group")))

stargazer::stargazer(m1,m2,m3,m3_1,m3_2,m4, omit = c("FV", "KommuneNr"), type = "html",
                     se=list(clustervcovm1$std.error, clustervcovm2$std.error, clustervcovm3$std.error, clustervcovm3_1$std.error,
                             clustervcovm3_2$std.error, clustervcovm4$std.error),
                     out = "Hypotese 1.doc",
                     df = F,
                     digits = 2,
                     dep.var.labels = c("Hoejrenationale"),
                     dep.var.caption = "<em> Afhaenging variabel: Opbakningen til hoejrenationale partier/DF</em>",
                     covariate.labels = c("Distance", "Andel i ejerbolig", "Andel videreg. udd", "Befolkningstaethed", "Serviceudgifter pr. indb.",
                                          "Ledige pr. 100", "Andel ikke-vestlige", "Tyveri og indbrud pr. 1000", "Konstant"),
                     notes.label = "Note: Klyngerobuste standardfejl i parentes (kommuneniveau)",
                     omit.stat = c("rsq", "f", "ser"),
                     add.lines = list(c("Kommune FE", "", "Y", "Y", "", "Y", "Y"),
                                      c("Tids FE", "", "Y","","Y", "Y", "Y"))) #Tabel 6

### TABEL 7: Hypotese 2

vd_m1 <- lm(valgdeltagelse ~ X.kortestedist, data = data)
vd_m2 <- lm(valgdeltagelse ~ X.kortestedist + factor(KommuneNr) + factor(FV), data = data)
vd_m3 <- lm(valgdeltagelse ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
              bef_tæt + as.numeric(serviceudg) + factor(KommuneNr), data = data)

vd_m3_1 <- lm(valgdeltagelse ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                bef_tæt + as.numeric(serviceudg) + factor(FV), data = data)

vd_m3_2 <- lm(valgdeltagelse ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                bef_tæt + as.numeric(serviceudg) + factor(KommuneNr) + factor(FV), data = data)

vd_m4 <- lm(valgdeltagelse ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
              bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
              as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data)

vd1_vcov <- tidy(coeftest(vd_m1,vcovHC(vd_m1,cluster="group")))
vd2_vcov <- tidy(coeftest(vd_m2,vcovHC(vd_m2, cluster="group")))
vd3_vcov <- tidy(coeftest(vd_m3,vcovHC(vd_m3,cluster="group")))
vd3_1_vcov <- tidy(coeftest(vd_m3_1,vcovHC(vd_m3_1,cluster="group")))
vd3_2_vcov <- tidy(coeftest(vd_m3_2,vcovHC(vd_m3_2,cluster="group")))
vd4_vcov <- tidy(coeftest(vd_m4,vcovHC(vd_m4,cluster="group")))

stargazer::stargazer(vd_m1,vd_m2,vd_m3,vd_m3_1,vd_m3_2,vd_m4, omit = c("FV", "KommuneNr"), type = "html",
                     se=list(vd1_vcov$std.error, vd2_vcov$std.error, vd3_vcov$std.error, vd3_1_vcov$std.error,
                             vd3_2_vcov$std.error, vd4_vcov$std.error),
                     out = "Hypotese 2.doc",
                     dep.var.labels = c(""),
                     df = F,
                     digits = 2,
                     dep.var.caption = "Valgdeltagelse",
                     covariate.labels = c("Distance", "Andel i ejerbolig", "Andel videreg. udd", "Befolkningstaethed", "Serviceudgifter pr. indb.",
                                          "Ledige pr. 100", "Andel ikke-vestlige", "Tyveri og indbrud pr. 1000", "Konstant"),
                     notes.label = "Note: Klyngerobuste standardfejl i parentes (kommuneniveau)",
                     omit.stat = c("rsq", "f", "ser"),
                     add.lines = list(c("Kommune FE", "", "Y", "Y", "", "Y", "Y"),
                                      c("Tids FE", "", "Y","","Y", "Y", "Y"))) #Tabel 7

###Tabel 8: Interaktion ml. Distance og befolkningstæthed

hyp3_beftaet <- lm(X.Dansk.Folkeparti_pct ~ X.kortestedist*as.numeric(bef_tæt) + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd)
                   + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                     as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data)

stargazer::stargazer(m4_df, hyp3_beftaet, type = "html", omit = c("FV", "KommuneNr"),
                     out = "hypotese3_befolkningstaethed.doc") #Tabel 8

### Tabel 9: Interaktion ml. Distance og kriminalitet


hyp4_krimi <- lm(X.Dansk.Folkeparti_pct ~ X.kortestedist*as.numeric(tyveri_indbrud) + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd)
                 + as.numeric(serviceudg) + as.numeric(bef_tæt) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) 
                 + factor(KommuneNr) + factor(FV), data = data)

stargazer::stargazer(m4_df, hyp4_krimi, type = "html", omit = c("FV", "KommuneNr"),
                     out = "hypotese4_kriminalitet.doc") #Tabel 9




#####################################
############## FIGURER ############## 
#####################################

### Figur 1: Asylcentre i Jammerbugt kommune
asylcentre01_viz <- asylcentre01 %>%
  dplyr::rename(lon = long)

asylcentre05_viz <- asylcentre05 %>%
  dplyr::rename(lon = long)

asylcentre07_viz <- asylcentre07 %>%
  dplyr::rename(lon = long)

asylcentre11_viz <- asylcentre11 %>%
  dplyr::rename(lon = long)

asylcentre15_viz <- asylcentre15 %>%
  dplyr::rename(lon = long) #Laver asylcenter klar til at blive plottet

figur1 <- pointDK(asylcentre15_viz[c(15,16),], detail = "polling", point.colour = "blue3", sub.plot = "jammerbugt") + 
  #labs(title = "Asylcentre i 2015 i Jammerbugtkredsen",
  #caption = "Kilde: Se afsnit om datagrundlag") + 
  theme(plot.title = element_text(color = "black", size = 12, face = "plain"))

### Figur 2: Udvikling i antallet af asylcentre

antalasylcentre <- data.frame("Aar" = c("2001", "2005", "2007", "2011", "2015"), 
                              "Antal" = c(nrow(asylcentre01), nrow(asylcentre05), 
                                          nrow(asylcentre07), nrow(asylcentre11), 
                                          nrow(asylcentre15)))

antalasylcentre$Antal <- as.numeric(antalasylcentre$Antal)
antalasylcentre$Aar <- as.numeric(antalasylcentre$Aar)
antalasylcentre

udv_antalcentre <- ggplot(antalasylcentre, aes(x = Aar, y = Antal)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(limits=c("2001","2005","2007", "2011", "2015")) +
  xlab("Folketingsvalg") +
  ylab("Antal åbne asylcentre ved tidspunktet for afholdelse af Folketingsvalget") +
  labs(title = "Udviklingen i antallet af asylcentre i perioden 2001-2015",
       subtitle = "Observeret i hvert Folketingsår",
       caption = "Kilde: Dansk Røde Kors, CVR-registret, Kristeligt Dagblad og Udlændingestyrelsen (se afsnit om datagrundlag)") +
  theme_minimal()

udv_antalcentre #Figur 2

### Figur 3: Boxplots

p_valgkredse_kommuner <- data %>%
  group_by(KommuneNum, FV) %>%
  dplyr::summarise(dist = min(X.kortestedist)) %>% #Tager den mindste distance indenfor hvert kommune og FV 
  ggplot(aes(x = factor(FV), y=dist)) + #gøres for at undgå, at kommuner med mange valgkredse vægter mere 
  geom_boxplot(aes(group = FV)) +
  labs(title = "Fordeling af distance til nærmeste asylcenter fordelt på Folketingsvalg",
       subtitle = "Hver observation er en kommune",
       caption = "Egne beregninger. Se afsnit om data") +
  ylab("Distance til nærmeste asylcenter") +
  xlab("Folketingsvalg") +
  ylim(0, 150) +
  theme_minimal()

danskfolkeparti <- data %>%
  ggplot(aes(x = factor(FV), y=X.Dansk.Folkeparti_pct)) + 
  geom_boxplot(aes(group = FV)) +
  labs(title = "Fordeling af Dansk Folkepartis andel af de samlede stemmer",
       subtitle = "Hver observation er en valgkreds",
       caption = "Kilde: Den Danske Valgdatabase") +
  ylab("Opbakning til Dansk Folkeparti (%)") +
  xlab("Folketingsvalg") +
  theme_minimal()

grid.arrange(p_valgkredse_kommuner, danskfolkeparti, ncol = 2) #Figur 3

### Figur 4: Befolkningstæthed betinget af storby/andre

asyl_beftaet_FV <- data %>%
  mutate(beftaet_fordeling = if_else(KommuneNum == 101 | KommuneNum == 147 | KommuneNum == 461 | 
                                       KommuneNum == 751 | KommuneNum == 851, "storbyer", 
                                     "andre byer")) %>%
  filter(beftaet_fordeling != "NA",
         FV == 2007 | FV == 2011 | FV == 2015) %>%
  ggplot(., aes(x = X.kortestedist, fill = factor(beftaet_fordeling, levels = c("storbyer", "andre byer")))) +
  geom_density(alpha = 0.7, bw = 2) +
  scale_fill_brewer(palette = "Set1",
                    name = "Valgdistrikter placeret i") +
  theme_bw() +
  xlim(0, 35) +
  #facet_wrap(. ~ FV) +
  facet_grid(rows = vars(FV)) +
  xlab("Distance til nærmeste asylcenter (kilometer)") +
  ylab("Densitet") +
  labs(title = "Fordeling af distance til nærmeste asylcenter for forskellige niveauer af befolkningstæthed",
       subtitle = "Opsplittet på Folketingsvalg",
       caption = "Storbyer: København, Frederiksberg, Odense, og Aarhus\nGrafen dækker 75% af observationerne")

asyl_beftaet_FV #Figur 4

### Figur 5: Trends 

test <- data %>%
  dplyr::select(group, KommuneNav, X.kortestedist, ValgstedId, FV, blaa, X.Dansk.Folkeparti_pct, valgdeltagelse)

trends_hyp1 <- test %>%
  mutate(treatment10 = if_else(ValgstedId %in% trendstreat2015, 1, 0)) %>%
  dplyr::group_by(treatment10, FV) %>%
  dplyr::summarize(DF = mean(X.Dansk.Folkeparti_pct, na.rm = T))

hypotese1_trendfigur <- ggplot(data = trends_hyp1) +
  geom_line(aes(x = as.factor(FV), y = DF, group=treatment10, col = factor(treatment10))) +
  geom_point(aes(x = as.factor(FV), y = DF, group=treatment10, col = factor(treatment10))) +
  scale_color_brewer(palette = "Set1",
                     name  ="Afstand til nærmeste\nasylcenter i 2015",
                     labels=c("Over 10 km.", "Under 10 km.")) +
  labs(title = "Trends for valgkredse med og uden asylcenter i nærområdet 2015",
       subtitle = "Gennemsnitlig opbakning til Dansk Folkeparti for hvert år for de to grupper") +
  xlab("Folketingsvalg") +
  ylab("Opbakning til Dansk Folkeparti (%)") +
  scale_x_discrete(breaks = c("2001", "2005", "2007", "2011", "2015")) +
  theme_minimal()

trends_hyp1_test <- test %>%
  mutate(treatment10 = if_else(ValgstedId %in% trendstreat2015, 1, 0))

trends_hyp1_test$FV <- as.factor(trends_hyp1_test$FV)
trends_hyp1_test$X.Dansk.Folkeparti_pct <- as.numeric(trends_hyp1_test$X.Dansk.Folkeparti_pct)
trends_hyp1_test$treatment10 <- as.factor(trends_hyp1_test$treatment10)

hypotese1_trendfigur2 <- ggline(trends_hyp1_test, x = "FV", y = "X.Dansk.Folkeparti_pct", 
                                add = c("mean_ci"),
                                color = "treatment10") +
  scale_color_brewer(palette = "Set1",
                     name  ="Afstand til nærmeste\nasylcenter",
                     labels=c("Over 10 km.", "Under 10 km.")) +
  labs(title = "Trends for valgkredse med og uden asylcenter i nærområdet 2015",
       caption = "95% konfidensintervaller",
       subtitle = "Gennemsnitlig opbakning til Dansk Folkeparti for hvert år for henholdsvis treatment- og kontrolgruppen")+
  xlab("Folketingsvalg") +
  ylab("Opbakning til Dansk Folkeparti (%)") +
  theme_minimal()
#theme(legend.position = c(0.25, 0.81),
legend.background = element_rect(fill="grey",
                                 size=1, 
                                 colour ="grey")

hypotese1_trendfigur2 #Figur 5

### Figur 6: Binært treatment
data_dummytreat <- data %>%
  dplyr::select(ValgstedId, FV, KommuneNr, X.Dansk.Folkeparti_pct, X.kortestedist, andel_ejerbolig, andel_videregudd,
                bef_tæt, serviceudg, ledige_pr.100, ikke_vestlige, tyveri_indbrud) %>%
  mutate(treatment02 = ifelse(X.kortestedist < 0.2, 1, 0),
         treatment04 = ifelse(X.kortestedist < 0.4, 1, 0),
         treatment06 = ifelse(X.kortestedist < 0.6, 1, 0),
         treatment08 = ifelse(X.kortestedist < 0.8, 1, 0),
         treatment1 = ifelse(X.kortestedist < 1, 1, 0),
         treatment2 = ifelse(X.kortestedist < 2, 1, 0),
         treatment5 = ifelse(X.kortestedist < 5, 1, 0),
         treatment10 = ifelse(X.kortestedist < 10, 1, 0),
         treatment20 = ifelse(X.kortestedist < 20, 1, 0),
         treatment40 = ifelse(X.kortestedist < 40, 1, 0),
         andel_ejerbolig = as.numeric(andel_ejerbolig),
         andel_videregudd = as.numeric(andel_videregudd),
         bef_tæt = as.numeric(bef_tæt),
         serviceudg = as.numeric(serviceudg),
         ledige_pr.100 = as.numeric(ledige_pr.100),
         ikke_vestlige = as.numeric(ikke_vestlige),
         tyveri_indbrud = as.numeric(tyveri_indbrud)) %>%
  na.omit()

dummy_02 <- lm(X.Dansk.Folkeparti_pct ~ factor(treatment02) + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                 bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                 as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat)

dummy_04 <- lm(X.Dansk.Folkeparti_pct ~ treatment04 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                 bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                 as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat)

dummy_06 <- lm(X.Dansk.Folkeparti_pct ~ treatment06 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                 bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                 as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat)

dummy_08 <- lm(X.Dansk.Folkeparti_pct ~ treatment08 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                 bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                 as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat)

dummy_1 <- lm(X.Dansk.Folkeparti_pct ~ treatment1 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat)

dummy_2 <- lm(X.Dansk.Folkeparti_pct ~ treatment2 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat)

dummy_5 <- lm(X.Dansk.Folkeparti_pct ~ treatment5 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat)

dummy_10 <- lm(X.Dansk.Folkeparti_pct ~ treatment10 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                 bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                 as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat)

dummy_20 <- lm(X.Dansk.Folkeparti_pct ~ treatment20 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                 bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                 as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat)

dummy_40 <- lm(X.Dansk.Folkeparti_pct ~ treatment40 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                 bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                 as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat)

dummy <- lm(X.Dansk.Folkeparti_pct ~ treatment + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
              bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
              as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat)

summary(dummy)

d02_vcov <- tidy(coeftest(dummy_02,vcovHC(dummy_02,cluster="group")))[2, ]
d04_vcov <- tidy(coeftest(dummy_04,vcovHC(dummy_04, cluster="group")))[2, ]
d06_vcov <- tidy(coeftest(dummy_06,vcovHC(dummy_06,cluster="group")))[2, ]
d08_vcov <- tidy(coeftest(dummy_08,vcovHC(dummy_08,cluster="group")))[2, ]
d1_vcov <- tidy(coeftest(dummy_1,vcovHC(dummy_1,cluster="group")))[2, ]
d2_vcov <- tidy(coeftest(dummy_2,vcovHC(dummy_2,cluster="group")))[2, ]
d5_vcov <- tidy(coeftest(dummy_5,vcovHC(dummy_5,cluster="group")))[2, ]
d10_vcov <- tidy(coeftest(dummy_10,vcovHC(dummy_10,cluster="group")))[2, ]
d20_vcov <- tidy(coeftest(dummy_20,vcovHC(dummy_20,cluster="group")))[2, ]
d40_vcov <- tidy(coeftest(dummy_40,vcovHC(dummy_40,cluster="group")))[2, ]

df_vcov <- bind_rows(d02_vcov,d04_vcov,d06_vcov, d08_vcov, d1_vcov, 
                     d2_vcov, d5_vcov, d10_vcov, d20_vcov, d40_vcov)

df_vcov$term[df_vcov$term == "treatment02"] <- "200 meter"
df_vcov$term[df_vcov$term == "treatment04"] <- "400 meter"
df_vcov$term[df_vcov$term == "treatment06"] <- "600 meter"
df_vcov$term[df_vcov$term == "treatment08"] <- "800 meter"
df_vcov$term[df_vcov$term == "treatment1"] <- "1 km."
df_vcov$term[df_vcov$term == "treatment2"] <- "2 km."
df_vcov$term[df_vcov$term == "treatment5"] <- "5 km."
df_vcov$term[df_vcov$term == "treatment10"] <- "10 km."
df_vcov$term[df_vcov$term == "treatment20"] <- "20 km."
df_vcov$term[df_vcov$term == "treatment40"] <- "40 km."

enosplot_konf <- ggplot(data = df_vcov) +
  geom_pointrange(mapping = aes(x = term, y = estimate,
                                ymin = estimate - 3.219*std.error,
                                ymax = estimate + 3.219*std.error)) +
  geom_pointrange(mapping = aes(x = term, y = estimate,
                                ymin = estimate - 1.98*std.error,
                                ymax = estimate + 1.98*std.error), color = "grey") +
  scale_x_discrete(limits=c("200 meter", "400 meter","600 meter", "800 meter", "1 km.", "2 km.", 
                            "5 km.", "10 km.", "20 km.", "40 km.")) +
  labs(y = "Effekten af asylcentre på opbakningen til DF (%-point)",
       x = "Treatmentgruppens distance til nærmeste asylcenter",
       title = "Effekten af asylcentre på opbakningen til Dansk Folkeparti ved forskellige definitioner af treatment",
       caption = "99,9% konfidensintervaller\nGrå: 95% konfidensintervaller") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_minimal()

enosplot <- ggplot(data = df_vcov) +
  geom_pointrange(mapping = aes(x = term, y = estimate,
                                ymin = estimate - 1.98*std.error,
                                ymax = estimate + 1.98*std.error)) +
  scale_x_discrete(limits=c("200 meter", "400 meter","600 meter", "800 meter", "1 km.", "2 km.", 
                            "5 km.", "10 km.", "20 km.", "40 km.")) +
  labs(y = "Effekten af asylcentre på opbakningen til DF (%-point)",
       x = "Treatmentgruppens distance til nærmeste asylcenter",
       title = "Effekten af asylcentre på opbakningen til Dansk Folkeparti ved forskellige definitioner af treatment",
       caption = "95% konfidensintervaller") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_minimal()

enosplot

### Figur 7: Fordeling af distance
distancedata <- data %>%
  dplyr::select(FV, X.kortestedist, KommuneNav, ValgstedId, id) %>%
  mutate(X.kortestedist = as.numeric(X.kortestedist)) %>%
  filter(FV == 2007 | FV == 2011 | FV == 2015) %>%
  na.omit()

distancefordeling_simpel <- ggplot(data = distancedata, aes(x = X.kortestedist)) +
  geom_histogram(binwidth = 0.5, alpha = 0.8) +
  facet_grid(rows = vars(FV)) +
  geom_vline(aes(xintercept = 10), linetype = "dashed") +
  scale_x_continuous(breaks = seq(0,170,10)) +
  theme_minimal() +
  labs(y = "Antal observationer",
       x = "Distance til nærmeste asylcenter (kilometer)",
       title = "Fordelingen distance til nærmeste asylcenter inden for hvert Folketingsvalgår",
       subtitle = "Hver observation er en valgkreds",
       caption  = "Stiplede linjer ved 10 kilometer")


distancefordeling_simpel #Figur 7

### Figur 8: Nettotilflytning

flytteplot5km <- ggerrorplot(data_flytning_2, x = "FV", y = "nettotilflytning", 
                               desc_stat = "mean_ci",
                               color = "treatment5",
                               add = "mean",
                               position = position_dodge(0.3)) +
    scale_color_brewer(palette = "Set1",
                       name  ="Afstand til nærmeste\nasylcenter",
                       labels=c("Under 5 km.", "Over 5 km.")) +
    #labs(title = "Nettotilflytning til kommuner, hvor valgdistrikterne i gennemsnit er placeret inden for 5 km. til nærmeste asylcenter",
         #subtitle = "95% konfidensintervaller",
         #caption = "Kilde: Danmarks Statistik og eget datasæt om asylcentre") +
    xlab("Folketingsvalg") +
    ylab("") +
  theme_minimal() +
  theme(legend.position="bottom")
  
flytteplot10km <- ggerrorplot(data_flytning_2, x = "FV", y = "nettotilflytning", 
                                desc_stat = "mean_ci",
                                color = "treatment10",
                                add = "mean",
                                position = position_dodge(0.3)) +
    scale_color_brewer(palette = "Set1",
                       name  ="Afstand til nærmeste\nasylcenter",
                       labels=c("Under 10 km.", "Over 10 km.")) +
    #labs(title = "Nettotilflytning til kommuner, hvor valgdistrikterne i gennemsnit er placeret inden for 10 km. til nærmeste asylcenter",
        # subtitle = "95% konfidensintervaller",
        # caption = "Kilde: Danmarks Statistik og eget datasæt om asylcentre") +
    xlab("Folketingsvalg") +
    ylab("") +
    theme_minimal() +
  theme(legend.position="bottom")
  

  
flytteplot5km #Treatment = 5 km.
flytteplot10km #Treatment = 10 km.

flyt_tilopgave <- grid.arrange(flytteplot5km, flytteplot10km, ncol = 2,
             left = "Nettotilflytning",
             top = grid::textGrob("Til- og fraflytning fra kommuner med forskellig afstand til nærmeste asylcenter",x=0,hjust=0))

### Figur 9: Binært treatment valgdeltagelse
data_dummytreat_vd <- data %>%
  dplyr::select(ValgstedId, FV, KommuneNr, X.Dansk.Folkeparti_pct, X.kortestedist, andel_ejerbolig, andel_videregudd,
                bef_tæt, serviceudg, ledige_pr.100, ikke_vestlige, tyveri_indbrud, valgdeltagelse) %>%
  mutate(treatment02 = ifelse(X.kortestedist < 0.2, 1, 0),
         treatment04 = ifelse(X.kortestedist < 0.4, 1, 0),
         treatment06 = ifelse(X.kortestedist < 0.6, 1, 0),
         treatment08 = ifelse(X.kortestedist < 0.8, 1, 0),
         treatment1 = ifelse(X.kortestedist < 1, 1, 0),
         treatment2 = ifelse(X.kortestedist < 2, 1, 0),
         treatment5 = ifelse(X.kortestedist < 5, 1, 0),
         treatment10 = ifelse(X.kortestedist < 10, 1, 0),
         treatment20 = ifelse(X.kortestedist < 20, 1, 0),
         treatment40 = ifelse(X.kortestedist < 40, 1, 0),
         andel_ejerbolig = as.numeric(andel_ejerbolig),
         andel_videregudd = as.numeric(andel_videregudd),
         bef_tæt = as.numeric(bef_tæt),
         serviceudg = as.numeric(serviceudg),
         ledige_pr.100 = as.numeric(ledige_pr.100),
         ikke_vestlige = as.numeric(ikke_vestlige),
         tyveri_indbrud = as.numeric(tyveri_indbrud),
         valgdeltagelse = as.numeric(valgdeltagelse)) %>%
  na.omit()

vd_dummy_02 <- lm(valgdeltagelse ~ treatment02 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                    bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                    as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat_vd)

vd_dummy_04 <- lm(valgdeltagelse ~ treatment04 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                    bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                    as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat_vd)

vd_dummy_06 <- lm(valgdeltagelse ~ treatment06 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                    bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                    as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat_vd)

vd_dummy_08 <- lm(valgdeltagelse ~ treatment08 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                    bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                    as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat_vd)

vd_dummy_1 <- lm(valgdeltagelse ~ treatment1 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                   bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                   as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat_vd)

vd_dummy_2 <- lm(valgdeltagelse ~ treatment2 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                   bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                   as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat_vd)

vd_dummy_5 <- lm(valgdeltagelse ~ treatment5 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                   bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                   as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat_vd)

vd_dummy_10 <- lm(valgdeltagelse ~ treatment10 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                    bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                    as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat_vd)

vd_dummy_20 <- lm(valgdeltagelse ~ treatment20 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                    bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                    as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat_vd)

vd_dummy_40 <- lm(valgdeltagelse ~ treatment40 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                    bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                    as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = data_dummytreat_vd)

d02_vdtidy <- tidy(vd_dummy_02)[2, ]
d04_vdtidy <- tidy(vd_dummy_04)[2, ]
d06_vdtidy <- tidy(vd_dummy_06)[2, ]
d08_vdtidy <- tidy(vd_dummy_08)[2, ]
d1_vdtidy <- tidy(vd_dummy_1)[2, ]
d2_vdtidy <- tidy(vd_dummy_2)[2, ]
d5_vdtidy <- tidy(vd_dummy_5)[2, ]
d10_vdtidy <- tidy(vd_dummy_10)[2, ]
d20_vdtidy <- tidy(vd_dummy_20)[2, ]
d40_vdtidy <- tidy(vd_dummy_40)[2, ]


tidy_dummytreat_vd <- bind_rows(d02_vdtidy,d04_vdtidy,d06_vdtidy,d08_vdtidy,d1_vdtidy,
                                d2_vdtidy,d5_vdtidy,d10_vdtidy,d20_vdtidy,d40_vdtidy)

d02_vdvcov <- tidy(coeftest(vd_dummy_02,vcovHC(vd_dummy_02,cluster="group")))[2, ]
d04_vdvcov <- tidy(coeftest(vd_dummy_04,vcovHC(vd_dummy_04, cluster="group")))[2, ]
d06_vdvcov <- tidy(coeftest(vd_dummy_06,vcovHC(vd_dummy_06,cluster="group")))[2, ]
d08_vdvcov <- tidy(coeftest(vd_dummy_08,vcovHC(vd_dummy_08,cluster="group")))[2, ]
d1_vdvcov <- tidy(coeftest(vd_dummy_1,vcovHC(vd_dummy_1,cluster="group")))[2, ]
d2_vdvcov <- tidy(coeftest(vd_dummy_2,vcovHC(vd_dummy_2,cluster="group")))[2, ]
d5_vdvcov <- tidy(coeftest(vd_dummy_5,vcovHC(vd_dummy_5,cluster="group")))[2, ]
d10_vdvcov <- tidy(coeftest(vd_dummy_10,vcovHC(vd_dummy_10,cluster="group")))[2, ]
d20_vdvcov <- tidy(coeftest(vd_dummy_20,vcovHC(vd_dummy_20,cluster="group")))[2, ]
d40_vdvcov <- tidy(coeftest(vd_dummy_40,vcovHC(vd_dummy_40,cluster="group")))[2, ]

valgdel_vcov <- bind_rows(d02_vdvcov,d04_vdvcov,d06_vdvcov, d08_vdvcov, d1_vdvcov, 
                          d2_vdvcov, d5_vdvcov, d10_vdvcov, d20_vdvcov, d40_vdvcov)

valgdel_vcov$term[valgdel_vcov$term == "treatment02"] <- "200 meter"
valgdel_vcov$term[valgdel_vcov$term == "treatment04"] <- "400 meter"
valgdel_vcov$term[valgdel_vcov$term == "treatment06"] <- "600 meter"
valgdel_vcov$term[valgdel_vcov$term == "treatment08"] <- "800 meter"
valgdel_vcov$term[valgdel_vcov$term == "treatment1"] <- "1 km."
valgdel_vcov$term[valgdel_vcov$term == "treatment2"] <- "2 km."
valgdel_vcov$term[valgdel_vcov$term == "treatment5"] <- "5 km."
valgdel_vcov$term[valgdel_vcov$term == "treatment10"] <- "10 km."
valgdel_vcov$term[valgdel_vcov$term == "treatment20"] <- "20 km."
valgdel_vcov$term[valgdel_vcov$term == "treatment40"] <- "40 km."

enosplot_valg <- ggplot(data = valgdel_vcov) +
  geom_pointrange(mapping = aes(x = term, y = estimate,
                                ymin = estimate - 1.96*std.error,
                                ymax = estimate + 1.96*std.error), color = "black") +
  scale_x_discrete(limits=c("200 meter", "400 meter","600 meter", "800 meter", "1 km.", "2 km.", 
                            "5 km.", "10 km.", "20 km.", "40 km.")) +
  labs(y = "Effekten af asylcentre på valgdeltagelsen",
       x = "Treatmentgruppens distance til nærmeste asylcenter",
       title = "Effekten af asylcentre på valgdeltagelsen ved forskellige definitioner af treatment",
       caption = "Sort: 95% konfidensintervaller") +
  scale_color_brewer(palette = "Set1") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_minimal()

enosplot_valg

### Figur 10: Befolkningstæthed

data_hyp3 <- data %>%
  dplyr::select(X.Dansk.Folkeparti_pct, bef_tæt, X.kortestedist, andel_ejerbolig, andel_videregudd, 
                serviceudg, ledige_pr.100, ikke_vestlige, tyveri_indbrud, KommuneNr, FV) %>%
  mutate(andel_ejerbolig = as.numeric(andel_ejerbolig),
         andel_videregudd = as.numeric(andel_videregudd),
         bef_taet = as.numeric(bef_tæt),
         serviceudg = as.numeric(serviceudg),
         ledige_pr.100 = as.numeric(ledige_pr.100),
         ikke_vestlige = as.numeric(ikke_vestlige),
         tyveri_indbrud = as.numeric(tyveri_indbrud),
         bef_tæt_kat = ifelse(bef_taet <= 65, 1,
                              ifelse(bef_taet > 65 & bef_taet <= 220, 2, 3)),
         bef_taet_kvartiler = ntile(bef_taet, 10),
         X.kortestedist1 = X.kortestedist,
         X.kortestedist2 = X.kortestedist) %>%
  na.omit() %>%
  mutate(predvaerdier = predict(hyp3_beftaet))

data_hyp3 %>%
  group_by(bef_tæt_kat) %>%
  dplyr::summarise(mean(predvaerdier))

m4_df_bef_tæt_int_1 <- lm(X.Dansk.Folkeparti_pct ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                            as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                            as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), 
                          data = subset(data_hyp3, bef_tæt_kat == 1))

m4_df_bef_tæt_int_2 <- lm(X.Dansk.Folkeparti_pct ~ X.kortestedist1 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                            + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                            as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), 
                          data = subset(data_hyp3, bef_tæt_kat == 2))

m4_df_bef_tæt_int_3 <- lm(X.Dansk.Folkeparti_pct ~ X.kortestedist2 + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                            + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                            as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), 
                          data = subset(data_hyp3, bef_tæt_kat == 3))


int1_vdvcov <- tidy(coeftest(m4_df_bef_tæt_int_1,vcovHC(m4_df_bef_tæt_int_1,cluster="group")))[2, ]
int2_vdvcov <- tidy(coeftest(m4_df_bef_tæt_int_2,vcovHC(m4_df_bef_tæt_int_2, cluster="group")))[2, ]
int3_vdvcov <- tidy(coeftest(m4_df_bef_tæt_int_3,vcovHC(m4_df_bef_tæt_int_3,cluster="group")))[2, ]

int_tidy <- bind_rows(int1_vdvcov, int2_vdvcov, int3_vdvcov)

int_tidy$term[int_tidy$term == "X.kortestedist"] <- "Befolkningstæthed under 65 indbyggere pr. km2"
int_tidy$term[int_tidy$term == "X.kortestedist1"] <- "Befolkningstæthed mellem 65 og 220 indbyggere pr. km2"
int_tidy$term[int_tidy$term == "X.kortestedist2"] <- "Befolkningstæthed over 220 indbyggere"

enosplot_int <- ggplot(data = int_tidy) +
  #geom_pointrange(mapping = aes(x = term, y = estimate,
  # ymin = estimate - 3.219*std.error,
  #ymax = estimate + 3.219*std.error)) +
  geom_pointrange(mapping = aes(x = term, y = estimate,
                                ymin = estimate - 1.96*std.error,
                                ymax = estimate + 1.96*std.error), color = "black") +
  #geom_pointrange(mapping = aes(x = term, y = estimate,
  #ymin = estimate - 1.98*std.error,
  #ymax = estimate + 1.98*std.error), color = "blue") +
  scale_x_discrete(limits=c("Befolkningstæthed under 65 indbyggere pr. km2", 
                            "Befolkningstæthed mellem 65 og 220 indbyggere pr. km2",
                            "Befolkningstæthed over 220 indbyggere")) +
  labs(y = "Effekten af asylcentre på opbakningen til Dansk Folkeparti",
       x = "Afgrænsning af sample ift. befolkningstæthed",
       title = "Effekten af asylcentre på opbakningen til Dansk Folkeparti ved forskellige grader af befolkningstæthed",
       subtitle = "Koefficienten og konfidensintervaller for koefficienter ved forskellige samples",
       caption = "Sort: 95 % konfidensintervaller") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_minimal()

enosplot_int #Figur 10

### Figur 11: Betinget af kriminalitetsrate

data_hyp4 <- data %>%
  dplyr::select(X.Dansk.Folkeparti_pct, bef_tæt, X.kortestedist, andel_ejerbolig, andel_videregudd, 
                serviceudg, ledige_pr.100, ikke_vestlige, tyveri_indbrud, KommuneNr, FV) %>%
  mutate(andel_ejerbolig = as.numeric(andel_ejerbolig),
         andel_videregudd = as.numeric(andel_videregudd),
         bef_taet = as.numeric(bef_tæt),
         serviceudg = as.numeric(serviceudg),
         ledige_pr.100 = as.numeric(ledige_pr.100),
         ikke_vestlige = as.numeric(ikke_vestlige),
         tyveri_indbrud = as.numeric(tyveri_indbrud),
         tyveri_indbrud_kat = ifelse(tyveri_indbrud <= 41, 1,
                                     ifelse(tyveri_indbrud > 41 & tyveri_indbrud <= 64.5, 2, 3)),
         krimi_kvartiler = ntile(tyveri_indbrud, 10),
         X.kortestedist = as.numeric(X.kortestedist),
         X.kortestedist2 = as.numeric(X.kortestedist),
         X.kortestedist3 = as.numeric(X.kortestedist)) %>%
  na.omit()

m4_df_krimi_lav <- lm(X.Dansk.Folkeparti_pct ~ X.kortestedist + as.numeric(andel_ejerbolig) + tyveri_indbrud +
                        as.numeric(andel_videregudd) + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + 
                        as.numeric(ikke_vestlige) + factor(KommuneNr) + factor(FV), 
                      data = subset(data_hyp4, tyveri_indbrud_kat == 1))

m4_df_krimi_ml <- lm(X.Dansk.Folkeparti_pct ~ X.kortestedist2 + as.numeric(andel_ejerbolig) + tyveri_indbrud +
                       as.numeric(andel_videregudd) + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + 
                       as.numeric(ikke_vestlige) + factor(KommuneNr) + factor(FV), 
                     data = subset(data_hyp4, tyveri_indbrud_kat == 2))

m4_df_krimi_hoej <- lm(X.Dansk.Folkeparti_pct ~ X.kortestedist3 + as.numeric(andel_ejerbolig) + tyveri_indbrud +
                         as.numeric(andel_videregudd) + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + 
                         as.numeric(ikke_vestlige) + factor(KommuneNr) + factor(FV), 
                       data = subset(data_hyp4, tyveri_indbrud_kat == 3))

intkrimi1_vdvcov <- tidy(coeftest(m4_df_krimi_lav,vcovHC(m4_df_krimi_lav,cluster="group")))[2, ]
intkrimi2_vdvcov <- tidy(coeftest(m4_df_krimi_ml,vcovHC(m4_df_krimi_ml, cluster="group")))[2, ]
intkrimi3_vdvcov <- tidy(coeftest(m4_df_krimi_hoej,vcovHC(m4_df_krimi_hoej,cluster="group")))[2, ]

intkrimi_tidy <- bind_rows(intkrimi1_vdvcov, intkrimi2_vdvcov, intkrimi3_vdvcov)

intkrimi_tidy$term[intkrimi_tidy$term == "X.kortestedist"] <- "Tyveri og indbrud: Under 41 tilfælde pr. 1000"
intkrimi_tidy$term[intkrimi_tidy$term == "X.kortestedist2"] <- "Tyveri og indbrud: Mellem 41 og 64,5 tilfælde pr. 1000"
intkrimi_tidy$term[intkrimi_tidy$term == "X.kortestedist3"] <- "Tyveri og indbrud: Over 64,5 tilfælde pr. 1000"

enosplot_int_krimi <- ggplot(data = intkrimi_tidy) +
  #geom_pointrange(mapping = aes(x = term, y = estimate,
  #ymin = estimate - 3.219*std.error,
  #ymax = estimate + 3.219*std.error)) +
  geom_pointrange(mapping = aes(x = term, y = estimate,
                                ymin = estimate - 1.96*std.error,
                                ymax = estimate + 1.96*std.error), color = "black") +
  scale_x_discrete(limits=c("Tyveri og indbrud: Under 41 tilfælde pr. 1000", 
                            "Tyveri og indbrud: Mellem 41 og 64,5 tilfælde pr. 1000",
                            "Tyveri og indbrud: Over 64,5 tilfælde pr. 1000")) +
  labs(y = "Effekten af asylcentre på opbakningen til Dansk Folkeparti (%)",
       x = "Afgrænsning af sample ift. kriminalitetsniveau",
       title = "Effekten af asylcentre på opbakningen til Dansk Folkeparti ved forskellige grader af kriminalitet",
       subtitle = "Koefficient og konfidensintervaller ved forskellige samples",
       caption = "Sort: 95% konfidensintervaller") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_minimal()

enosplot_int_krimi #Figur 11

#######################################
############## APPENDIKS ############## 
#######################################

### Appendiks: Alternative specifikationer af modeller 

m1_0715 <- lm(blaa ~ X.kortestedist, data = subset(data, FV == 2007 | FV == 2011 | FV == 2015))
m2_0715 <- lm(blaa ~ X.kortestedist + factor(KommuneNr) + factor(FV), data = subset(data, FV == 2007 | FV == 2011 | FV == 2015))
m3_0715 <- lm(blaa ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                bef_tæt + as.numeric(serviceudg) + factor(KommuneNr) + factor(FV), data = subset(data, FV == 2007 | FV == 2011 | FV == 2015))

m4_0715 <- lm(blaa ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = subset(data, FV == 2007 | FV == 2011 | FV == 2015))

m4_df_0715 <- lm(blaa ~ X.kortestedist + as.numeric(andel_ejerbolig) + as.numeric(andel_videregudd) + 
                   bef_tæt + as.numeric(serviceudg) + as.numeric(ledige_pr.100) + as.numeric(ikke_vestlige) + 
                   as.numeric(tyveri_indbrud) + factor(KommuneNr) + factor(FV), data = subset(data, FV == 2007 | FV == 2011 | FV == 2015))


stargazer::stargazer(m1_0715,m2_0715,m3_0715,m4_0715,m4_df_0715, omit = c("FV", "KommuneNr"),
                     type = "html", out = "kun2007_2015.doc")

### Appendiks: Opbakningen til Dansk Folkeparti med binært treatment
stargazer::stargazer(dummy_02,dummy_04,dummy_06,dummy_08,dummy_1,dummy_2,
                     dummy_5,dummy_10,dummy_20,dummy_40, omit = c("FV", "KommuneNr"),
                     type = "html", out = "enosplot_appendiks.doc")

### Appendiks: Valgdeltagelsen med binært treatment
stargazer::stargazer(vd_dummy_02,vd_dummy_04,vd_dummy_06,vd_dummy_08,vd_dummy_1,vd_dummy_2,
                     vd_dummy_5, vd_dummy_10, vd_dummy_20, vd_dummy_40, omit = c("FV", "KommuneNr"), type = "html",
                     out = "valgdeltagelse_dummy.doc")

### Appendiks: Forudsætningstests
par(mfrow = c(2, 2))

plot(m4_df) #Forudsætninger hypotese 1
plot(vd_m4) #Forudsætninger hypotese 2
plot(hyp3_beftaet) #Forudsætninger hypotese 3
plot(hyp4_krimi) #Forudsætninger hypotese 4