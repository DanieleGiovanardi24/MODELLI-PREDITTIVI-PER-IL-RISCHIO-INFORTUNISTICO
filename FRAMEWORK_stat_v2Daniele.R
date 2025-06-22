setwd("/Users/danigiovanardi/Desktop/INAIL_OPENDATA/dati_processati")
library(dplyr)
library(sqldf)
library(sf)
library(tibble)

##file di check poiint
load("/Users/danigiovanardi/Desktop/INAIL_OPENDATA/dati_processati/dati_semestrali_2019_2023.RData")
reg2019 <- st_read("Reg01012019_WGS84")

glimpse(dati_semestrali_2019_2023)
str(dati_semestrali_2019_2023)

dati_semestrali_2019_2023_stat<-dati_semestrali_2019_2023 %>%
  mutate(VAR_STAT=ifelse(GG>40,1,0))
prop.table(table(dati_semestrali_2019_2023_stat$VAR_STAT))


####analisi modelli
##studio perimetro premodllizzazione
data_model_1<-dati_semestrali_2019_2023_stat %>%
  filter(Genere=="M",MODALITA=="IN OCCASIONE")
model_1<-glm(VAR_STAT ~ETA_f, 
             family = binomial(link = logit),
             data = data_model_1)
summary(model_1)
model_1_coeff<-model_1[["coefficients"]]
model_1_coeff_male<-t(model_1_coeff)
t(model_1_coeff_male)

data_model_1<-dati_semestrali_2019_2023_stat %>%
  filter(Genere=="F",MODALITA=="IN OCCASIONE")
model_1<-glm(VAR_STAT ~ETA_f, 
             family = binomial(link = logit),
             data = data_model_1)
summary(model_1)
model_1_coeff<-model_1[["coefficients"]]
model_1_coeff_female<-t(model_1_coeff)
t(model_1_coeff_female)

io<-cbind(t(model_1_coeff_female),t(model_1_coeff_male))
colnames(io)<-c("female","male")
io_net<-io[-1,]

boxplot(io_net)#posizione
par(mfrow=c(2,2))
boxplot(main="Coeff.GLM",io_net,xlab="Gender")
boxplot(main="Coeff.GLM",io_net,xlab="Gender")
hist(main="Coefficients Distribution female",io_net[,1])
hist(main="Coefficients Distribution male",io_net[,2])
boxplot(main="Coeff.GLM",io_net,xlab="Gender")
io_net<-data.frame(io_net)
plot(io_net)

###MODELLO 2 PER GENERE DISCRIMINANTE
##studio perimetro premodllizzazione
data_model_1<-dati_semestrali_2019_2023_stat %>%
  filter(MODALITA=="IN OCCASIONE",ATECO=="G")
model_1<-glm(VAR_STAT ~Genere+ETA_f, 
             family = binomial(link = logit),
             data = data_model_1)
summary(model_1)
model_1_coeff<-model_1[["coefficients"]]
model_1_coeff<-t(model_1_coeff)
t(model_1_coeff)


#######
#######
#######






####PARAMETRIZZAZIONE
par_Genere<-c("M")

par_ATECO_1<-c("C - ATTIVITA  MANIFATTURIERE")
par_IND<-c("TE")
par_MOD<-c("IN OCCASIONE")

##modellizzazione


graph_model_1<-dati_semestrali_2019_2023_stat %>% #caso in cui occorre sempre richiamare il ggplot per usare i filtri
  filter(ATECO_1==par_ATECO_1,Indennizzo==par_IND,MODALITA==par_MOD)%>%
  ggplot()+
  geom_bar(mapping = aes(x=ETA_f))+
  facet_wrap(~Genere)+
  facet_grid(Genere~Indennizzo)

  graph_model_1


data_model_1<-dati_semestrali_2019_2023_stat %>%
  filter(ATECO_1==par_ATECO_1,Indennizzo==par_IND,MODALITA==par_MOD)

  model_1<-glm(VAR_STAT ~ ETA_f, 
    family = binomial(link = logit),
    data = data_model_1)

summary(model_1)
plot(model_1)  
predict(model_1)

glimpse(data_model_1)
  


summary(model_1)
plot(model_1)

par_Genere<-c("F")
par_ATECO_1<-c("A - AGRICOLTURA")
par_Ind<-c("TE")
par_Mod<-c("N")

data_model<-dati_semestrali_2019_2023 %>%
  filter(Genere==par_Genere,ATECO_1==par_ATECO_1,Indennizzo==par_Ind,ModalitaAccadimento==par_Mod,GG>30)

plot(data_model)

model <- lm(GG ~ Genere,
            data = data_model)

plot(data_model$Eta,data_model$GiorniIndennizzati)

summary(model)

################################################################################
####################CODICE DANIELE##############################################
################################################################################

# -------------------
# MODELLO MASCHI
# -------------------
data_model_male <- dati_semestrali_2019_2023_stat %>%
  filter(Genere == "M", MODALITA == "IN OCCASIONE")
model_male <- glm(VAR_STAT ~ ETA_f,
                  family = binomial(link = logit),
                  data = data_model_male)

# -------------------
# MODELLO FEMMINE
# -------------------
data_model_female <- dati_semestrali_2019_2023_stat %>%
  filter(Genere == "F", MODALITA == "IN OCCASIONE")
model_female <- glm(VAR_STAT ~ ETA_f,
                    family = binomial(link = logit),
                    data = data_model_female)

# -------------------
# MODELLO 2 CON GENERE + ETA
# -------------------
data_model_genere <- dati_semestrali_2019_2023_stat %>%
  filter(MODALITA == "IN OCCASIONE", ATECO == "G")
model_genere <- glm(VAR_STAT ~ Genere + ETA_f,
                    family = binomial(link = logit),
                    data = data_model_genere)

# -------------------
# MATRICE DI CONFUSIONE + SPECIFICITÀ
# -------------------
calcola_confusion_specificity <- function(model) {
  pred_prob <- predict(model, type = "response")
  pred_class <- ifelse(pred_prob >= 0.140713  , 1, 0)
  true_class <- model$data$VAR_STAT
  
  conf_matrix <- table(Predetto = pred_class, Osservato = true_class)
  
  # Controlla che ci siano le celle "0" e "1"
  TN <- ifelse("0" %in% rownames(conf_matrix) & "0" %in% colnames(conf_matrix),
               conf_matrix["0", "0"], 0)
  FP <- ifelse("1" %in% rownames(conf_matrix) & "0" %in% colnames(conf_matrix),
               conf_matrix["1", "0"], 0)
  specificity <- TN / (TN + FP)
  
  cat("\n===== MATRICE DI CONFUSIONE =====\n")
  print(conf_matrix)
  cat("Specificità:", round(specificity, 4), "\n")
}

# Applica funzione ai modelli
cat("\n### MASCHI ###\n")
calcola_confusion_specificity(model_male)

cat("\n### FEMMINE ###\n")
calcola_confusion_specificity(model_female)

cat("\n### GENERE + ETA ###\n")
calcola_confusion_specificity(model_genere)

calcola_metriche <- function(model, data) {
  # Predizioni probabilistiche
  pred_probs <- predict(model, type = "response")
  
  # Threshold 0.5 per classificazione binaria
  pred_class <- ifelse(pred_probs > 0.140713 , 1, 0)
  
  # Valori osservati reali
  actual <- data$VAR_STAT
  
  # Confusione (forzo livelli 0/1 per evitare errori)
  cm <- table(factor(pred_class, levels = c(0,1)),
              factor(actual, levels = c(0,1)))
  
  # Estraggo elementi
  TN <- cm["0", "0"]
  FP <- cm["1", "0"]
  FN <- cm["0", "1"]
  TP <- cm["1", "1"]
  
  # Metriche
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- TP / (TP + FN)  # recall
  specificity <- TN / (TN + FP)
  precision <- TP / (TP + FP)
  f1_score <- ifelse((precision + sensitivity) > 0,
                     (2 * precision * sensitivity) / (precision + sensitivity),
                     0)
  
  # Stampo
  cat("\n===== MATRICE DI CONFUSIONE =====\n")
  print(cm)
  cat(sprintf("\nAccuracy: %.4f", accuracy))
  cat(sprintf("\nSensitivity (Recall): %.4f", sensitivity))
  cat(sprintf("\nSpecificity: %.4f", specificity))
  cat(sprintf("\nPrecision: %.4f", precision))
  cat(sprintf("\nF1 Score: %.4f\n", f1_score))
}

calcola_metriche(model_male, data_model_male)
calcola_metriche(model_female, data_model_female)
calcola_metriche(model_genere, data_model_genere)

########## CORVA ROC #########################

# Installiamo pacchetti se non ci sono
if (!require(pROC)) install.packages("pROC")
library(pROC)

# Funzione per curva ROC e AUC
calcola_roc_auc <- function(model, data, modello_nome = "Modello") {
  # Predizioni probabilistiche
  pred_probs <- predict(model, type = "response")
  
  # Valori osservati reali
  actual <- data$VAR_STAT
  
  # Calcolo ROC
  roc_obj <- roc(actual, pred_probs)
  
  # Plot
  plot(roc_obj, main = paste("ROC Curve -", modello_nome), col = "blue")
  auc_value <- auc(roc_obj)
  
  # Mostra AUC
  cat(paste("\nAUC per", modello_nome, ":", round(auc_value, 4), "\n"))
  
  return(roc_obj)
}

# Eseguiamo per ciascun modello
roc_male <- calcola_roc_auc(model_male, data_model_male, "Male")
roc_female <- calcola_roc_auc(model_female, data_model_female, "Female")
roc_genere <- calcola_roc_auc(model_genere, data_model_genere, "Genere + ETA")

#############################################################################################
########################################PROVA################################################
#############################################################################################
library(dplyr)
library(pROC)
library(caret)

# 1️⃣ Prepara variabile binaria (GG > 40 come soglia)
dati <- dati_semestrali_2019_2023 %>%
  mutate(VAR_STAT = ifelse(GG > 40, 1, 0)) %>%
  filter(!is.na(Genere), !is.na(ETA_f), !is.na(ATECO_1), !is.na(REGIONE_1))

cat(sprintf("Totale record dopo pulizia: %d\n", nrow(dati)))

# 2️⃣ Dividi train/test (80/20)
set.seed(123)
train_idx <- sample(1:nrow(dati), 0.8 * nrow(dati))
train_data <- dati[train_idx, ]
test_data <- dati[-train_idx, ]

cat(sprintf("Train set: %d righe | Test set: %d righe\n", nrow(train_data), nrow(test_data)))

# 3️⃣ Costruisci modello GLM migliorato
model <- glm(VAR_STAT ~ Genere + ETA_f + ATECO_1 + REGIONE_1,
             family = binomial(link = "logit"),
             data = train_data)

summary(model)

# 4️⃣ Predizioni probabilistiche sul test set
pred_probs <- predict(model, newdata = test_data, type = "response")
cat(sprintf("Lunghezza pred_probs: %d (atteso %d)\n", length(pred_probs), nrow(test_data)))

if (length(pred_probs) != nrow(test_data)) {
  stop("Errore: predizioni e dati reali hanno lunghezze diverse!")
}

# 5️⃣ Calcolo ROC e AUC
roc_obj <- roc(test_data$VAR_STAT, pred_probs)
auc_value <- auc(roc_obj)
cat(sprintf("\nAUC: %.4f\n", auc_value))

plot(roc_obj, main = sprintf("ROC Curve (AUC = %.4f)", auc_value))

# 6️⃣ Trova threshold ottimale (estrai numerico, non data.frame)
opt_coords <- coords(roc_obj, "best", ret = c("threshold"), best.method = "youden")
opt_thresh <- as.numeric(opt_coords)
cat(sprintf("\nOptimal threshold: %.4f\n", opt_thresh))

# 7️⃣ Classificazione binaria usando threshold ottimale
pred_class <- ifelse(pred_probs > opt_thresh, 1, 0)

# Controllo lunghezze
cat(sprintf("Lunghezza pred_class: %d (atteso %d)\n", length(pred_class), nrow(test_data)))

# 8️⃣ Conversione a fattori coerenti
pred_factor <- factor(pred_class, levels = c(0, 1))
actual_factor <- factor(test_data$VAR_STAT, levels = c(0, 1))

if (length(pred_factor) != length(actual_factor)) {
  stop("Errore: predizioni e dati osservati hanno lunghezze diverse!")
}

# 9️⃣ Matrice di confusione
cm <- confusionMatrix(pred_factor, actual_factor, positive = "1")
print(cm)

# 🔟 Metriche principali
cat(sprintf("\nAccuracy: %.4f", cm$overall["Accuracy"]))
cat(sprintf("\nSensitivity (Recall): %.4f", cm$byClass["Sensitivity"]))
cat(sprintf("\nSpecificity: %.4f", cm$byClass["Specificity"]))
cat(sprintf("\nPrecision (PPV): %.4f", cm$byClass["Pos Pred Value"]))
cat(sprintf("\nF1 Score: %.4f\n", cm$byClass["F1"]))

#######################################################################################
#############TASK1: Validazione incrociata dei calcoli ################################
######################################################################################
prop.table(table(dati$VAR_STAT))
nrow(dati) == (nrow(train_data) + nrow(test_data))
colSums(is.na(dati))

##Verifica se gli indici generati sono coerenti
# Salvo prima versione
set.seed(123)
train_idx_1 <- sample(1:nrow(dati), 0.8 * nrow(dati))

# Rilancio per controllo
set.seed(123)
train_idx_2 <- sample(1:nrow(dati), 0.8 * nrow(dati))

# Verifica uguaglianza
identical(train_idx_1, train_idx_2)  # Deve restituire TRUE

##Test sulle predizioni
# Predizioni sul training set
pred_train <- predict(model)

# Check 1: lunghezza predizioni
length(pred_train) == nrow(train_data)  # Deve essere TRUE

# Check 2: range probabilità (devono stare tra 0 e 1)
range(predict(model, type = "response"))  # Deve stare in [0,1]

##Coerenza tra metrica e modello
# A) Coefficienti, errori, deviance
summary(model)

# B) AUC e curva ROC
print(auc_value)
plot(roc_obj)

# C) Confusion Matrix e metriche
print(cm)

# Oppure, in dettaglio:
cat(sprintf("\nAccuracy: %.4f", cm$overall["Accuracy"]))
cat(sprintf("\nSensitivity (Recall): %.4f", cm$byClass["Sensitivity"]))
cat(sprintf("\nSpecificity: %.4f", cm$byClass["Specificity"]))
cat(sprintf("\nPrecision (PPV): %.4f", cm$byClass["Pos Pred Value"]))
cat(sprintf("\nF1 Score: %.4f\n", cm$byClass["F1"]))

###############################################################################
####################TASK2: Validazione Temporale###############################
###############################################################################

# 📦 Caricamento librerie necessarie
library(dplyr)
library(pROC)
library(caret)

# 1️⃣ Prepara la variabile binaria (soglia: GG > 40)
dati <- dati_semestrali_2019_2023 %>%
  mutate(VAR_STAT = ifelse(GG > 40, 1, 0)) %>%
  filter(
    !is.na(Genere),
    !is.na(ETA_f),
    !is.na(ATECO_1),
    !is.na(REGIONE_1),
    !is.na(Anno)
  )

cat(sprintf("✅ Totale record dopo pulizia: %d\n", nrow(dati)))

# 2️⃣ Suddivisione in training (2019-2022) e validation (2023)
train_data <- dati %>% filter(Anno %in% c(2019, 2020, 2021, 2022))
test_data  <- dati %>% filter(Anno == 2023)

cat(sprintf("🧪 Training set: %d righe\n", nrow(train_data)))
cat(sprintf("🧪 Validation set: %d righe\n", nrow(test_data)))

# 3️⃣ Costruzione modello GLM
model <- glm(VAR_STAT ~ Genere + ETA_f + ATECO_1 + REGIONE_1,
             family = binomial(link = "logit"),
             data = train_data)

summary(model)

# 4️⃣ Predizioni probabilistiche sulla validation (2023)
pred_probs <- predict(model, newdata = test_data, type = "response")

# Verifica lunghezza
if (length(pred_probs) != nrow(test_data)) {
  stop("❌ ERRORE: predizioni e dati reali hanno lunghezze diverse!")
}
cat(sprintf("✅ Lunghezza pred_probs: %d\n", length(pred_probs)))

# 5️⃣ Calcolo ROC e AUC
roc_obj <- roc(test_data$VAR_STAT, pred_probs)
auc_value <- auc(roc_obj)
cat(sprintf("📈 AUC sulla validation (2023): %.4f\n", auc_value))

# 6️⃣ Soglia ottimale (metodo di Youden)
opt_coords <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")
opt_thresh <- as.numeric(opt_coords)
cat(sprintf("🔍 Soglia ottimale: %.4f\n", opt_thresh))

# 7️⃣ Classificazione binaria secondo soglia ottimale
pred_class <- ifelse(pred_probs > opt_thresh, 1, 0)

# 8️⃣ Conversione a fattori coerenti
pred_factor <- factor(pred_class, levels = c(0, 1))
actual_factor <- factor(test_data$VAR_STAT, levels = c(0, 1))

# 9️⃣ Matrice di confusione e metriche
cm <- confusionMatrix(pred_factor, actual_factor, positive = "1")
print(cm)

# 🔟 Stampa metriche principali
cat(sprintf("\n📊 Accuracy: %.4f", cm$overall["Accuracy"]))
cat(sprintf("\n📊 Sensitivity (Recall): %.4f", cm$byClass["Sensitivity"]))
cat(sprintf("\n📊 Specificity: %.4f", cm$byClass["Specificity"]))
cat(sprintf("\n📊 Precision (PPV): %.4f", cm$byClass["Pos Pred Value"]))
cat(sprintf("\n📊 F1 Score: %.4f\n", cm$byClass["F1"]))

# 📉 Tracciamento della curva ROC
plot(roc_obj, main = sprintf("ROC Curve (AUC = %.4f)", auc_value))


###############################################################################
####################TASK3: confronto tra l’età continua e le classi quinquennali##########
###############################################################################

library(dplyr)
library(caret)
library(pROC)

# Crea variabile binaria e filtra dati
dati <- dati_semestrali_2019_2023 %>%
  mutate(VAR_STAT = ifelse(GG > 40, 1, 0)) %>%
  filter(!is.na(Genere), !is.na(Eta), !is.na(ETA_f), !is.na(ATECO_1), !is.na(REGIONE_1))


set.seed(123)
train_idx <- sample(1:nrow(dati), 0.8 * nrow(dati))
train_data <- dati[train_idx, ]
test_data <- dati[-train_idx, ]

#MODELLO ETA CONTINUA 
model_eta <- glm(VAR_STAT ~ Genere + Eta + ATECO_1 + REGIONE_1,
                 family = binomial(link = "logit"),
                 data = train_data)

summary(model_eta)

# Predizioni + ROC
pred_eta <- predict(model_eta, newdata = test_data, type = "response")
roc_eta <- roc(test_data$VAR_STAT, pred_eta)
auc_eta <- auc(roc_eta)
cat(sprintf("AUC modello con Eta continua: %.4f\n", auc_eta))

#MODELLO CLASSI DI ETA
model_etaf <- glm(VAR_STAT ~ Genere + ETA_f + ATECO_1 + REGIONE_1,
                  family = binomial(link = "logit"),
                  data = train_data)

summary(model_etaf)

# Predizioni + ROC
pred_etaf <- predict(model_etaf, newdata = test_data, type = "response")
roc_etaf <- roc(test_data$VAR_STAT, pred_etaf)
auc_etaf <- auc(roc_etaf)
cat(sprintf("AUC modello con ETA_f classi: %.4f\n", auc_etaf))

#CONFRONTA AUC 
plot(roc_eta, col = "blue", main = "Confronto ROC: Eta vs ETA_f")
plot(roc_etaf, col = "red", add = TRUE)
legend("bottomright", legend = c("Eta continua", "ETA_f classi"),
       col = c("blue", "red"), lty = 1)
