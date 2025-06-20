setwd("/Users/danigiovanardi/Desktop/INAIL_OPENDATA/dati_processati")
load("dati_semestrali.Rdata")

library(dplyr)
library(sqldf)
library(sf)
library(tibble)

dati_semestrali_2019_2023<-dati_semestrali ##1° clone
reg2019 <- st_read("Reg01012019_WGS84")

glimpse(dati_semestrali)
str(dati_semestrali)

glimpse(dati_semestrali_2019_2023)

dati_semestrali_2019_2023_<- dati_semestrali_2019_2023 %>% #per rinominare la viaribile giorni indennizati
  dplyr::rename(GG=GiorniIndennizzati)
##
glimpse(dati_semestrali_2019_2023_)
##
#dati_semestrali_2019_2023<-dati_semestrali_2019_2023_##2° clone

#dati_semestrali_2019_2023<-dati_semestrali
#dati_semestrali_2019_2023<-dati_semestrali_2019_2023 %>%
 # filter(ModalitaAccadimento=="N",Genere=="F")


dati_semestrali_2019_2023_<- sqldf("SELECT *,
                           CASE 
                         WHEN Regione='DatiConCadenzaInfortuniAbruzzo'  THEN 'ABRUZZO'
                         WHEN Regione='DatiConCadenzaInfortuniBasilicata'  THEN 'BASILICATA' 
                         WHEN Regione='DatiConCadenzaInfortuniCalabria'  THEN 'CALABRIA' 
                         WHEN Regione='DatiConCadenzaInfortuniCampania'  THEN 'CAMPANIA' 
                         WHEN Regione='DatiConCadenzaInfortuniEmiliaRomagna'  THEN 'EMILIA ROMAGNA' 
                         WHEN Regione='DatiConCadenzaInfortuniFriuliVeneziaGiulia'  THEN 'FRIULI VENEZIA GIULIA' 
                         WHEN Regione='DatiConCadenzaInfortuniLazio'  THEN 'LAZIO'
                         WHEN Regione='DatiConCadenzaInfortuniLiguria'  THEN 'LIGURIA'
                         WHEN Regione='DatiConCadenzaInfortuniLombardia'  THEN 'LOMBARDIA' 
                         WHEN Regione='DatiConCadenzaInfortuniMarche'  THEN 'MARCHE' 
                         WHEN Regione='DatiConCadenzaInfortuniMolise'  THEN 'MOLISE' 
                         WHEN Regione='DatiConCadenzaInfortuniPiemonte'  THEN 'PIEMONTE' 
                         WHEN Regione='DatiConCadenzaInfortuniPuglia'  THEN 'PUGLIA' 
                         WHEN Regione='DatiConCadenzaInfortuniSardegna'  THEN 'SARDEGNA' 
                         WHEN Regione='DatiConCadenzaInfortuniSicilia'  THEN 'SICILIA' 
                         WHEN Regione='DatiConCadenzaInfortuniToscana'  THEN 'TOSCANA' 
                         WHEN Regione='DatiConCadenzaInfortuniTrentinoAltoAdige'  THEN 'TRENTINO' 
                         WHEN Regione='DatiConCadenzaInfortuniUmbria'  THEN 'UMBRIA' 
                         WHEN Regione='DatiConCadenzaInfortuniValledAosta'  THEN 'VALLE D AOSTA' 
                         WHEN Regione='DatiConCadenzaInfortuniVeneto'  THEN 'VENETO' 
                         
                                                         ELSE ''
                           END 
                     as REGIONE_1
                     from dati_semestrali_2019_2023_")


dati_semestrali_2019_2023_<- sqldf("SELECT *,
                           CASE 
                         WHEN REGIONE_1='PIEMONTE'  THEN '1' 
                         WHEN REGIONE_1='VALLE D AOSTA'  THEN '2'
                         WHEN REGIONE_1='LOMBARDIA'  THEN '3'
                         WHEN REGIONE_1='TRENTINO' THEN '4'
                         WHEN REGIONE_1='VENETO'  THEN '5'
                         WHEN REGIONE_1='FRIULI VENEZIA GIULIA'  THEN '6'
                         WHEN REGIONE_1='LIGURIA'  THEN '7'
                         WHEN REGIONE_1='EMILIA ROMAGNA'  THEN '8'
                         WHEN REGIONE_1='TOSCANA'  THEN '9'
                         WHEN REGIONE_1='UMBRIA'  THEN '10'
                         WHEN REGIONE_1='MARCHE'  THEN '11'
                         WHEN REGIONE_1='LAZIO'  THEN '12'
                         WHEN REGIONE_1='ABRUZZO'  THEN '13'
                         WHEN REGIONE_1='MOLISE'  THEN '14'
                         WHEN REGIONE_1='CAMPANIA'  THEN '15'
                         WHEN REGIONE_1='PUGLIA'  THEN '16'
                         WHEN REGIONE_1='BASILICATA'  THEN '17'
                         WHEN REGIONE_1='CALABRIA'  THEN '18'
                         WHEN REGIONE_1='SICILIA'  THEN '19'
                         WHEN REGIONE_1='SARDEGNA'  THEN '20'
                                                        ELSE ''
                           END 
                     as COD_REG
                     from dati_semestrali_2019_2023_")


dati_semestrali_2019_2023_<- sqldf("SELECT *,
                           CASE 
                         WHEN Eta <= 14 THEN '14'
                         WHEN Eta>= 15 and Eta <= 19 THEN '15-19'
                         WHEN Eta>= 20 and Eta <= 24 THEN '20-24'
                         WHEN Eta>= 25 and Eta <= 29 THEN '25-29'
                         WHEN Eta>= 30 and Eta <= 34 THEN '30-34'
                         WHEN Eta>= 35 and Eta <= 39 THEN '35-39'
                         WHEN Eta>= 40 and Eta <= 44 THEN '40-44'
                         WHEN Eta>= 45 and Eta <= 49 THEN '45-49'
                         WHEN Eta>= 50 and Eta <= 54 THEN '50-54'
                         WHEN Eta>= 55 and Eta <= 59 THEN '55-59'
                         WHEN Eta>= 60 and Eta <= 64 THEN '60-64'
                         WHEN Eta>= 65 and Eta <= 69 THEN '65-69'
                         WHEN Eta>= 70 and Eta <= 74 THEN '70-74'
                         WHEN Eta>= 75               THEN 'da 75'
                        
                                                             ELSE ''
                           END 
                     as ETA_f
                     from dati_semestrali_2019_2023_")
str(dati_semestrali_2019_2023_)

library(stringr)
dati_semestrali_2019_2023_<- dati_semestrali_2019_2023_ %>%
    mutate(ATECO=ifelse(SettoreAttivitaEconomica=="ND","ND",str_sub(SettoreAttivitaEconomica,1,1)))

table(dati_semestrali_2019_2023_$COD_REG)
table(dati_semestrali_2019_2023_$REGIONE_1)

dati_semestrali_2019_2023_<- sqldf("SELECT *,
                           CASE 
                         WHEN ATECO='ND'  THEN 'ND'
                         WHEN ATECO='A'  THEN 'A - AGRICOLTURA' 
                         WHEN ATECO='B'  THEN 'B - ESTRAZIONE DI MINERALI'
                         WHEN ATECO='C'  THEN 'C - ATTIVITA  MANIFATTURIERE'
                         WHEN ATECO='D'  THEN 'D - FORNITURA DI ENERGIE'
                         WHEN ATECO='E'  THEN 'E - FORNITURA DI ACQUA'
                         WHEN ATECO='F'  THEN 'F - COSTRUZIONI'
                         WHEN ATECO='G'  THEN 'G - COMMERCIO ALL INGROSSO E DETT.- RIPARAZIONI VEICOLI'
                         WHEN ATECO='H'  THEN 'H - TRASPORTO E MAGAZZINAGGIO'
                         WHEN ATECO='I'  THEN 'I - ATTIVITA  DEI SERVIZI DI ALLOGGIO E DI RISTORAZIONE'
                         WHEN ATECO='J'  THEN 'J - SERVIZI DI INFORMAZIONE E COMUNICAZIONE'
                         WHEN ATECO='K'  THEN 'K - ATTIVITA  FINANZIARIE E ASSICURATIVE'
                         WHEN ATECO='L'  THEN 'L - ATTIVITA  IMMOBILIARI'
                         WHEN ATECO='M'  THEN 'M - ATTIVITA  PROFESSIONALI, SCIENTIFICHE E TECNICHE'
                         WHEN ATECO='N'  THEN 'N - NOLEGGIO, AGENZIE DI VIAGGIO'
                         WHEN ATECO='O'  THEN 'O - AMMINISTRAZIONE PUBBLICA E DIFESA'
                         WHEN ATECO='P'  THEN 'P - ISTRUZIONE'
                         WHEN ATECO='Q'  THEN 'Q - SANITA  E ASSISTENZA SOCIALE'
                         WHEN ATECO='R'  THEN 'R - ATTIVITA  ARTISTICHE, SPORTIVE, DIVERTIMENTO'
                         WHEN ATECO='S'  THEN 'S - ALTRE ATTIVITA  DI SERVIZI'
                         WHEN ATECO='T'  THEN 'T - ATTIVITA  DI FAMIGLIE E CONVIVENZE'
                         WHEN ATECO='U'  THEN 'U - ORGANIZZAZIONI ED ORGANISMI EXTRATERRITORIALI'
                         
                         ELSE ''
                           END 
                     as   ATECO_1
                     from dati_semestrali_2019_2023_")

str(dati_semestrali_2019_2023_)

##rivedere i nomi delle variabili##


##codificare nuove variabili
table(dati_semestrali_2019_2023_$ModalitaAccadimento)
dati_semestrali_2019_2023_<- sqldf("SELECT *,
                           CASE 
                         WHEN ModalitaAccadimento='N'  THEN 'IN OCCASIONE'
                         WHEN ModalitaAccadimento='S'  THEN 'IN ITINERE'
                                                  ELSE ''
                           END 
                     as   MODALITA
                     from dati_semestrali_2019_2023_")
str(dati_semestrali_2019_2023_)

##check per verifiche dati-->controllo già i totali delle tabeklle di OPenData
table(dati_semestrali_2019_2023_$ModalitaAccadimento)
table(dati_semestrali_2019_2023_$MODALITA)
prop.table(table(dati_semestrali_2019_2023_$MODALITA))
prop.table(table(dati_semestrali_2019_2023_$REGIONE_1))*100


glimpse(dati_semestrali_2019_2023_)


#############
#############
####cloni DB

dati_semestrali_2019_2023<-dati_semestrali_2019_2023_ ##seconda copia

#############
#############


#####alcuni primi controlli sulla base dati ottrenuta incorciando anche con tabelle dati OPD ed eventuali incroci con dati ed elaborazioni diverse
prop.table(table(dati_semestrali_2019_2023$ATECO_1))
##-->
marco<-dati_semestrali_2019_2023 %>%
  filter(ATECO!="ND")
glimpse(marco)

prop.table(table(marco$ATECO))
#####

##inizio 

###automazione per statistiche di base-->rivedere il disegno di ricerca o quesito
tabella_1<-table(dati_semestrali_2019_2023$ETA_f,dati_semestrali_2019_2023$Genere)
str(tabella_1)
tabella_1
tabella_1_freq<-prop.table(tabella_1,2)

tabella_2<-table(dati_semestrali_2019_2023$ETA_f,dati_semestrali_2019_2023$REGIONE_1)
str(tabella_2)
tabella_2
tabella_2_freq<-prop.table(tabella_2,2)

tabella_3<-table(dati_semestrali_2019_2023$ETA_f,dati_semestrali_2019_2023$ATECO_1)
str(tabella_3)
tabella_3
tabella_3_freq<-prop.table(tabella_3,2)

tabella_4<-table(dati_semestrali_2019_2023$REGIONE_1,dati_semestrali_2019_2023$Anno)
str(tabella_4)
tabella_4_freq<-prop.table(tabella_4,2)

library(openxlsx)
tabella_1_2_3_4<-list(dist_1=tabella_1,dist_2=tabella_2,dist_3=tabella_3,dist_4=tabella_4)
                        
tabella_1_2_3_4<-write.xlsx(tabella_1_2_3_4,"tabella_1_2_3_4.xlsx")



################
################
##tabella composita con filtri sulla base dati, comaptibile con foglio excel
table(dati_semestrali_2019_2023$ATECO_1)

tabella_5_male<-dati_semestrali_2019_2023 %>%
  filter(Genere=="M",ATECO_1=="N - NOLEGGIO, AGENZIE DI VIAGGIO")

tabella_5_male<-prop.table(table(tabella_5_male$ETA_f,tabella_5_male$Anno),2)
#tabella_5_male<-data.frame(tabella_5_male)##-->il data frame serve per creare il DB (pivot)


tabella_5_female<-dati_semestrali_2019_2023 %>%
  filter(Genere=="F",ATECO_1=="N - NOLEGGIO, AGENZIE DI VIAGGIO")
tabella_5_female<-prop.table(table(tabella_5_female$ETA_f,tabella_5_female$Anno),2)
#tabella_5_female<-data.frame(tabella_5_female)
    #tabella_5_genere<-rbind(tabella_5_male,tabella_5_female)
 #   tabella_5_genere
    
    tabella_6_male<-dati_semestrali_2019_2023 %>%
      filter(Genere=="M",ATECO_1=="N - NOLEGGIO, AGENZIE DI VIAGGIO")
    tabella_6_male<-table(tabella_6_male$ETA_f,tabella_6_male$Anno)
    #tabella_6_male<-data.frame(tabella_6_male)
    
    tabella_6_female<-dati_semestrali_2019_2023 %>%
      filter(Genere=="F",ATECO_1=="N - NOLEGGIO, AGENZIE DI VIAGGIO")
    tabella_6_female<-table(tabella_6_female$ETA_f,tabella_6_female$Anno)
    #tabella_6_female<-data.frame(tabella_6_female)
    
    tabella_1_2_3_4_5_6<-list(dist_1=tabella_1,dist_2=tabella_2,dist_3=tabella_3,dist_4=tabella_4,
                              dist_5_male=tabella_5_male,
                              dist_5_female=tabella_5_female,
                              dist_6_male=tabella_6_male,
                              dist_6_female=tabella_6_female)
    tabella_1_2_3_4_5_6<-write.xlsx(tabella_1_2_3_4_5_6,"tabella_1_2_3_4_5_6.xlsx")
    
    ################
    ################
    ##unico file
    #tabella_6_genere<-cbind(tabella_6_male,tabella_6_female)
    #tabella_6_genere<-data.frame(tabella_6_genere)
    
    #tabella_7_genere<-cbind(tabella_6_genere,tabella_5_genere)
    #tabella_7_genere<-data.frame(tabella_7_genere)
    
    
    
#tabella_4<-table()

###
#tabella_1_2_3_4_5_6<-list(dist_1=tabella_1,dist_2=tabella_2,dist_3=tabella_3,dist_4=tabella_4,dist_5=tabella_5_genere,                                                    dist_6=tabella_6_genere)
#tabella_1_2_3_4_5_6<-write.xlsx(tabella_1_2_3_4_5_6,"tabella_1_2_3_4_5_6.xlsx")
    ###########
    ###########
    ###########
    ###########
    ###########
    ###########
    ###########
    ###########
    ###########
###plot per eda analysis per genere e totale##ciclo for per automatizzare le tabelle al variare delle modalità della variabile

    lista_INF_male<-vector(mode="list",length(levels(factor(dati_semestrali_2019_2023$ATECO_1))))##QUESTO CODICE SE NON TROVA DTERMINAZIONI NONE SEGUE; NO DA NULL
    nomi<-levels(factor(dati_semestrali_2019_2023$ATECO_1));nomi<-substr(nomi,1,15)
    names(lista_INF_male)<-nomi
    #lista<-vector(mode="list",length(levels(factor(data_orig$ATECO)))) %>%
    #data_orig<-filter(data_orig,ATECO!=c("ND"))
    for (i in 1:length(levels(factor(dati_semestrali_2019_2023$ATECO_1)))) {
      lista_INF_male[[i]]<- dati_semestrali_2019_2023%>%
                filter(ATECO_1==levels(factor(dati_semestrali_2019_2023$ATECO_1))[i])%>%
                filter(ATECO!="ND",Anno==2023,Genere=="M") %>%
        group_by(Anno,ETA_f)%>%
        tally()%>%
        mutate(percent=n/sum(n))
    }
    lista_INF_ATECO_1_male<-export(lista_INF_male,
                              file="lista_INF_ATECO_1_male.xlsx",overwrigth=TRUE)##IMPLEMENTARE LE SCHEDE)
    
str(dati_semestrali_2019_2023)
    #female
    lista_INF_female<-vector(mode="list",length(levels(factor(dati_semestrali_2019_2023$ATECO_1))))##QUESTO CODICE SE NON TROVA DTERMINAZIONI NONE SEGUE; NO DA NULL
    nomi<-levels(factor(dati_semestrali_2019_2023$ATECO_1));nomi<-substr(nomi,1,15)
    names(lista_INF_female)<-nomi
    #lista<-vector(mode="list",length(levels(factor(data_orig$ATECO)))) %>%
    #data_orig<-filter(data_orig,ATECO!=c("ND"))
    for (i in 1:length(levels(factor(dati_semestrali_2019_2023$ATECO_1)))) {
      lista_INF_female[[i]]<- dati_semestrali_2019_2023%>%
        filter(ATECO_1==levels(factor(dati_semestrali_2019_2023$ATECO_1))[i])%>%
        filter(ATECO!="ND",Anno==2023,Genere=="F") %>%
        group_by(Anno,ETA_f)%>%
        tally()%>%
        mutate(percent=n/sum(n))
    }
    lista_INF_ATECO_1_female<-export(lista_INF_female,
                                   file="lista_INF_ATECO_1_female.xlsx",overwrigth=TRUE)##IMPLEMENTARE LE SCHEDE)
    
    
    ###########
    ###########
    ###########
    
    
    
    ###########
    ###########
    ###########
    ###prova con mutate
    lista_INF<-vector(mode="list",length(levels(factor(dati_semestrali_2019_2023$ATECO_1))))##QUESTO CODICE SE NON TROVA DTERMINAZIONI NONE SEGUE; NO DA NULL
    nomi<-levels(factor(dati_semestrali_2019_2023$ATECO_1));nomi<-substr(nomi,1,15)
    names(lista_INF)<-nomi
    #lista<-vector(mode="list",length(levels(factor(data_orig$ATECO)))) %>%
    #data_orig<-filter(data_orig,ATECO!=c("ND"))
    for (i in 1:length(levels(factor(dati_semestrali_2019_2023$ATECO_1)))) {
      lista_INF[[i]]<- dati_semestrali_2019_2023%>%
        filter(ATECO_1==levels(factor(dati_semestrali_2019_2023$ATECO_1))[i])%>%
        filter(ATECO!="ND",Anno==2023) %>%
        group_by(Anno,ETA_f)%>%
        tally()%>%
        mutate(percent=n/sum(n))
    }
    lista_INF_ATECO_1<-export(lista_INF,
                              file="lista_INF_ATECO_1.xlsx",overwrigth=TRUE)##IMPLEMENTARE LE SCHEDE)
    
    ###########
    ###########
    ###########
    
    
    ###########
    ###########
    ###########
    ###prova con select (aLTERNATIVE TAKE)
    lista_INF<-vector(mode="list",length(levels(factor(dati_semestrali_2019_2023$ATECO_1))))##QUESTO CODICE SE NON TROVA DTERMINAZIONI NONE SEGUE; NO DA NULL
    nomi<-levels(factor(dati_semestrali_2019_2023$ATECO_1));nomi<-substr(nomi,1,15)
    names(lista_INF)<-nomi
    #lista<-vector(mode="list",length(levels(factor(data_orig$ATECO)))) %>%
    #data_orig<-filter(data_orig,ATECO!=c("ND"))
    for (i in 1:length(nomi)) {
      lista_INF_female[[i]] <- dati_semestrali_2019_2023 %>%
        filter(ATECO_1 == nomi[i]) %>%
        filter(ATECO != "ND", Anno == 2023, Genere == "F") %>%
        group_by(Anno, ETA_f) %>%
        tally() %>%
        mutate(percent = n / sum(n))
    }
    
    lista_INF_ATECO_1_v1<-write.xlsx(lista_INF,
                              file="lista_INF_ATECO_1_v1.xlsx",overwrigth=TRUE)##IMPLEMENTARE LE SCHEDE)
    
    
    ###########
    ###########
    ###########
    ###########
    ###########
    ###########
    ###########FARE CON ATECO E ALTRA VARIABILE DRIVER
    
    
    lista_INF_male<-vector(mode="list",length(levels(factor(dati_semestrali_2019_2023$ATECO_1))))##QUESTO CODICE SE NON TROVA DTERMINAZIONI NONE SEGUE; NO DA NULL
    nomi<-levels(factor(dati_semestrali_2019_2023$ATECO_1));nomi<-substr(nomi,1,15)
    names(lista_INF_male)<-nomi
    #lista<-vector(mode="list",length(levels(factor(data_orig$ATECO)))) %>%
    #data_orig<-filter(data_orig,ATECO!=c("ND"))
    for (i in 1:length(levels(factor(dati_semestrali_2019_2023$ATECO_1)))) {
      lista_INF_male[[i]]<- dati_semestrali_2019_2023%>%
        filter(ATECO_1==levels(factor(dati_semestrali_2019_2023$ATECO_1))[i])%>%
        filter(ATECO!="ND",Anno==2023,Genere=="M") %>%
        group_by(Anno,ETA_f)%>%
        tally()%>%
        mutate(percent=n/sum(n))
    }
    lista_INF_ATECO_1_male<-export(lista_INF_male,
                                   file="lista_INF_ATECO_1_male.xlsx",overwrigth=TRUE)##IMPLEMENTARE LE SCHEDE)
    
    
    #female
    lista_INF_female<-vector(mode="list",length(levels(factor(dati_semestrali_2019_2023$ATECO_1))))##QUESTO CODICE SE NON TROVA DTERMINAZIONI NONE SEGUE; NO DA NULL
    nomi<-levels(factor(dati_semestrali_2019_2023$ATECO_1));nomi<-substr(nomi,1,15)
    names(lista_INF_female)<-nomi
    #lista<-vector(mode="list",length(levels(factor(data_orig$ATECO)))) %>%
    #data_orig<-filter(data_orig,ATECO!=c("ND"))
    for (i in 1:length(levels(factor(dati_semestrali_2019_2023$ATECO_1)))) {
      lista_INF_female[[i]]<- dati_semestrali_2019_2023%>%
        filter(ATECO_1==levels(factor(dati_semestrali_2019_2023$ATECO_1))[i])%>%
        filter(ATECO!="ND",Anno==2023,Genere=="F") %>%
        group_by(Anno,ETA_f)%>%
   table
    }
    lista_INF_ATECO_1_female<-export(lista_INF_female,
                                     file="lista_INF_ATECO_1_female.xlsx",overwrigth=TRUE)##IMPLEMENTARE LE SCHEDE)
    
    #####eda per regione
    lista_INF_male<-vector(mode="list",length(levels(factor(dati_semestrali_2019_2023$REGIONE_1))))##QUESTO CODICE SE NON TROVA DTERMINAZIONI NONE SEGUE; NO DA NULL
    nomi<-levels(factor(dati_semestrali_2019_2023$REGIONE_1));nomi<-substr(nomi,1,15)
    names(lista_INF_male)<-nomi
    #lista<-vector(mode="list",length(levels(factor(data_orig$ATECO)))) %>%
    #data_orig<-filter(data_orig,ATECO!=c("ND"))
    for (i in 1:length(levels(factor(dati_semestrali_2019_2023$REGIONE_1)))) {
      lista_INF_male[[i]]<- dati_semestrali_2019_2023%>%
        filter(REGIONE_1==levels(factor(dati_semestrali_2019_2023$REGIONE_1))[i])%>%
        filter(ATECO!="ND",Anno==2023,Genere=="M") %>%
        group_by(Anno,ETA_f)%>%
        tally()%>%
        mutate(percent=n/sum(n))
    }
    lista_INF_REGIONE_1_male<-export(lista_INF_male,
                                   file="lista_INF_REGIONE_1_male.xlsx",overwrigth=TRUE)##IMPLEMENTARE LE SCHEDE)
    
    str(dati_semestrali_2019_2023)
    #female
    lista_INF_female<-vector(mode="list",length(levels(factor(dati_semestrali_2019_2023$REGIONE_1))))##QUESTO CODICE SE NON TROVA DTERMINAZIONI NONE SEGUE; NO DA NULL
    nomi<-levels(factor(dati_semestrali_2019_2023$REGIONE_1));nomi<-substr(nomi,1,15)
    names(lista_INF_female)<-nomi
    #lista<-vector(mode="list",length(levels(factor(data_orig$ATECO)))) %>%
    #data_orig<-filter(data_orig,ATECO!=c("ND"))
    for (i in 1:length(levels(factor(dati_semestrali_2019_2023$REGIONE_1)))) {
      lista_INF_female[[i]]<- dati_semestrali_2019_2023%>%
        filter(REGIONE_1==levels(factor(dati_semestrali_2019_2023$REGIONE_1))[i])%>%
        filter(ATECO!="ND",Anno==2023,Genere=="F") %>%
        group_by(Anno,ETA_f)%>%
        tally()%>%
        mutate(percent=n/sum(n))
    }
    lista_INF_REGIONE_1_female<-export(lista_INF_female,
                                     file="lista_INF_REGIONE_1_female.xlsx",overwrigth=TRUE)##IMPLEMENTARE LE SCHEDE)
    
    
    ###########
    ###########
    ###########
    
    
    
    ###########
    ###########
    ###########
    ###prova con mutate
    lista_INF<-vector(mode="list",length(levels(factor(dati_semestrali_2019_2023$REGIONE_1))))##QUESTO CODICE SE NON TROVA DTERMINAZIONI NONE SEGUE; NO DA NULL
    nomi<-levels(factor(dati_semestrali_2019_2023$REGIONE_1));nomi<-substr(nomi,1,15)
    names(lista_INF)<-nomi
    #lista<-vector(mode="list",length(levels(factor(data_orig$ATECO)))) %>%
    #data_orig<-filter(data_orig,ATECO!=c("ND"))
    for (i in 1:length(levels(factor(dati_semestrali_2019_2023$REGIONE_1)))) {
      lista_INF[[i]]<- dati_semestrali_2019_2023%>%
        filter(REGIONE_1==levels(factor(dati_semestrali_2019_2023$REGIONE_1))[i])%>%
        filter(ATECO!="ND",Anno==2023) %>%
        group_by(Anno,ETA_f)%>%
        tally()%>%
        mutate(percent=n/sum(n))
    }
    lista_INF_REGIONE_1<-export(lista_INF,
                              file="lista_INF_REGIONE_1.xlsx",overwrigth=TRUE)##IMPLEMENTARE LE SCHEDE)
    
    ###########
    ###########
    ###########
    
    
    ###########
    ###########
  
    
    ###########

      
    
    
    
    
    
    
    
    
    
########creazione tabelle per statistiche EDA






###########
###########
###########
###########
###########
###########
###primi plot geografici
io<-dati_semestrali_2019_2023 %>%
  #filter(REGIONE_1=="ABRUZZO",Genere=="M",Eta==55)%>%
  #filter(Anno==2021)%>%
  group_by(COD_REG)%>%
  summarize(FREQ=n())
    
#mutate(Prob_mort=round(FREQ_mort/FREQ_compl*1000,2))
#colnames(io)<-c("COD_REG","FREQ_compl","FREQ_mort","Prob_mort")
io$COD_REG<-as.numeric(io$COD_REG) 
str(io)

map_REG<-right_join(reg2019,io, by = "COD_REG") %>% ##mappa regionale valori assoluti##
  ggplot(aes(fill = FREQ)) + 
  ggtitle("Infortuni sul lavoro
Italia quinquennio 2019-2023") +
  geom_sf(col = "gray") +
  theme_void() +
  #scale_fill_distiller(direction = 1) 
  scale_fill_distiller(palette = "RdYlBu", direction =-1, name = "Infortuni Denunciati")+
  #theme(legend.title=element_blank()) +  # etichette
  geom_sf_text(aes(label = FREQ),size = 4,col="black")+
  labs(fill = "FREQ")
map_REG<-map_REG+theme(legend.position = c(1,0.95), legend.justification = c(0, 1),axis.text.x=element_blank(),legend.text = element_text(size=1),legend.key.width = unit(1, 'cm'))+
  theme_void()#https://ggplot2-book.org/themes.html#sec-themes

map_0<-map_REG
map_0


##altre mappe sulle freequenze
###primi plot geografici
io<-dati_semestrali_2019_2023 %>%
  group_by(COD_REG)%>%
  tally()%>%
  mutate(percent=(n/sum(n)))#filter(REGIONE_1=="ABRUZZO",Genere=="M",Eta==55)%>%
  #filter(Anno==2021)%>%
  #group_by(COD_REG)%>%
  #summarize(FREQ=n())
#mutate(Prob_mort=round(FREQ_mort/FREQ_compl*1000,2))
#colnames(io)<-c("COD_REG","FREQ_compl","FREQ_mort","Prob_mort")
io$COD_REG<-as.numeric(io$COD_REG) 
str(io)
map_REG<-right_join(reg2019,io, by = "COD_REG") %>% ##mappa regionale valori assoluti##
  ggplot(aes(fill = percent)) + 
  ggtitle("Infortuni sul lavoro
Italia quinquennio 2019-2023") +
  geom_sf(col = "gray") +
  theme_void() +
  #scale_fill_distiller(direction = 1) 
  scale_fill_distiller(palette = "RdYlBu", direction =-1, name = "Infortuni Denunciati")+
  #theme(legend.title=element_blank()) +  # etichette
  geom_sf_text(aes(label = paste0(
    round(percent*100,1),"%")),size = 4,col="black")+
  labs(fill = "Frequenze")
map_REG<-map_REG+theme(legend.position = c(1,0.95), legend.justification = c(0, 1),axis.text.x=element_blank(),legend.text = element_text(size=1),legend.key.width = unit(1, 'cm'))+
  theme_void()#https://ggplot2-book.org/themes.html#sec-themes

map_0<-map_REG
map_0

###
###
###########
###########
###########
###########
###########
###########
##PASSAGGI AI GIORNI
glimpse(dati_semestrali_2019_2023)
###########
###########
###########
###primi plot geografici
#perimetri
table(dati_semestrali_2019_2023$ATECO_1)
table(dati_semestrali_2019_2023$ModalitaAccadimento)
table(dati_semestrali_2019_2023$MODALITA)
###SENZA FILTRO O PERIMETRO

par_Mod<-c("IN ITINERE")
io<-dati_semestrali_2019_2023 %>%
  group_by(COD_REG,REGIONE_1)%>%
  #filter(Anno==2023)%>%
  #filter(Genere==par_Genere,ATECO_1==par_ATECO_1,Indennizzo==par_Ind,MODALITA==par_Mod) %>%
  #filter(Genere==par_Genere,ATECO_1==par_ATECO_1,Indennizzo==par_Ind,MODALITA==par_Mod) %>%
  filter(MODALITA==par_Mod) %>%
  summarize(FREQ=n(),GIORNI=sum(GG), GIORNI_MEDI=mean(GG))
#tally()%>%
#mutate(percent=(n/sum(n)))#filter(REGIONE_1=="ABRUZZO",Genere=="M",Eta==55)%>%

#group_by(COD_REG)%>%
#summarize(FREQ=n())
#mutate(Prob_mort=round(FREQ_mort/FREQ_compl*1000,2))
#colnames(io)<-c("COD_REG","FREQ_compl","FREQ_mort","Prob_mort")
io$COD_REG<-as.numeric(io$COD_REG) 
str(io)
map_REG<-right_join(reg2019,io, by = "COD_REG") %>% ##mappa regionale valori assoluti##
  ggplot(aes(fill = GIORNI_MEDI)) + 
  ggtitle(paste0("Giorni Medi Indennizzati 
sul lavoro in Italia nel quinquennio 2019-2023",":",
                 par_Genere,",",par_ATECO_1,",",par_Ind)) +
  geom_sf(col = "gray") +
  theme_void() +
  #scale_fill_distiller(direction = 1) 
  scale_fill_distiller(palette = "RdYlBu", direction =-1, name = "Giorni Indennizzati Medi")+
  #theme(legend.title=element_blank()) +  # etichette
  geom_sf_text(aes(label = 
                     round(GIORNI_MEDI,1)),size = 3,col="black")+
  labs(fill = "GIORNI_MEDI")
map_REG<-map_REG+theme(legend.position = c(1,0.95), legend.justification = c(0, 1),axis.text.x=element_blank(),legend.text = element_text(size=1),legend.key.width = unit(1, 'cm'))+
  theme_void()#https://ggplot2-book.org/themes.html#sec-themes

map_0<-map_REG
map_0
tabella_0_map<-io
tabella_0_map
tabella_0_map<-write.xlsx(tabella_0_map,"tabella_0_map.xlsx")
####
####

###con parametrio

table(dati_semestrali_2019_2023$ATECO_1)
par_Genere<-c("M")
par_ATECO_1<-c("P - ISTRUZIONE")
par_Ind<-c("TE")
par_Mod<-c("IN ITINERE")

io<-dati_semestrali_2019_2023 %>%
  group_by(COD_REG,REGIONE_1)%>%
  #filter(Anno==2023)%>%
  #filter(Genere==par_Genere,ATECO_1==par_ATECO_1,Indennizzo==par_Ind,MODALITA==par_Mod) %>%
  filter(Genere==par_Genere,ATECO_1==par_ATECO_1,Indennizzo==par_Ind,MODALITA==par_Mod) %>%
  #filter(Genere==par_Genere,ATECO_1==par_ATECO_1,MODALITA==par_Mod) %>%
  summarize(FREQ=n(),GIORNI=sum(GG), GIORNI_MEDI=mean(GG))
  #tally()%>%
  #mutate(percent=(n/sum(n)))#filter(REGIONE_1=="ABRUZZO",Genere=="M",Eta==55)%>%

#group_by(COD_REG)%>%
#summarize(FREQ=n())
#mutate(Prob_mort=round(FREQ_mort/FREQ_compl*1000,2))
#colnames(io)<-c("COD_REG","FREQ_compl","FREQ_mort","Prob_mort")
io$COD_REG<-as.numeric(io$COD_REG) 
str(io)
map_REG<-right_join(reg2019,io, by = "COD_REG") %>% ##mappa regionale valori assoluti##
  ggplot(aes(fill = GIORNI_MEDI)) + 
  ggtitle(paste0("Giorni Medi Indennizzati 
sul lavoro in Italia nel quinquennio 2019-2023",":",
                 par_Genere,",",par_ATECO_1,",",par_Ind)) +
  geom_sf(col = "gray") +
  theme_void() +
  #scale_fill_distiller(direction = 1) 
  scale_fill_distiller(palette = "RdYlBu", direction =-1, name = "Giorni Indennizzati Medi")+
  #theme(legend.title=element_blank()) +  # etichette
  geom_sf_text(aes(label = 
   round(GIORNI_MEDI,1)),size = 3,col="black")+
  labs(fill = "GIORNI_MEDI")
map_REG<-map_REG+theme(legend.position = c(1,0.95), legend.justification = c(0, 1),axis.text.x=element_blank(),legend.text = element_text(size=1),legend.key.width = unit(1, 'cm'))+
  theme_void()#https://ggplot2-book.org/themes.html#sec-themes

map_0<-map_REG
map_0
tabella_0_map<-io
tabella_0_map
tabella_0_map<-write.xlsx(tabella_0_map,"tabella_0_map.xlsx")

#####
#####





##preparazione dati per analisi statistitica CDA

dati_semestrali_2019_2023<-dati_semestrali_2019_2023 %>%
  mutate(VAR_STAT=ifelse(GG<25,0,1))
str(dati_semestrali_2019_2023)
table(dati_semestrali_2019_2023$VAR_STAT)


###
plot(table(dati_semestrali_2019_2023$VAR_STAT))
