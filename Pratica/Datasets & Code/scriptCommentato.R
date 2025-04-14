# Script commentanto per l'analisi dei dati IAT ---- 
library(implicitMeasures) # caricare il pacchetto necessario 

# Importare il data set ----- 
# (se necessario impostare la working directory corretta)

data = read.table("IATdata.dat", header=TRUE, sep = "\t")
head(data)

# check dei partecipanti ----- 
table(data$subject)

# il soggetto 9999999 era un soggetto "finto" va tolto 

data = data[!data$subject %in% 9999999, ]

# blocchi dello IAT ----- 
table(data$blockcode)  # quante osservazioni per ogni blocco

unique(data$blockcode) # semplice lista dei blocchi 

# trial dello IAT -------
table(data$trialcode) # quante osservazioni per ogni trial

unique(data$trialcode) # semplice lista dei trial

# Pulire il data per il calcolo del D score attraverso la funzione 
# clean_iat()
# ?clean_iat() per leggere la documentazione

data_clean = clean_iat(
  data,               # nome del data set
  sbj_id = "subject", # colonna con gli ID dei soggetti
  block_id = "blockcode", # Colonna con le etichette dei blocchi
  mapA_practice = "PracticeWhitegood", 
  mapA_test = "TestWhitegood",
  mapB_practice = "PracticeWhitebad",
  mapB_test = "TestWhitebad",
  latency_id = "latency",   # colonna delle latenze
  accuracy_id = "correct",  # colonna delle accuratezze
  trial_id = "trialcode",   # colonna con le etichette dei trial
  trial_eliminate = c("reminder", "reminder1"), # trial da eliminare
  demo_id = "blockcode",    # colonna con le etichette dei blocchi
  trial_demo = "demografica" # etichette dei trial demografica
)

# data_clean contiene tre elementi diversi: 

names(data_clean)

# A noi interessa il primo, data_keep
iat = data_clean[[1]]
str(iat)
head(iat)

# si può calcolare il D score usando la funzione compute_iat()

d3 = compute_iat(iat,  # data set con i dati IAT
                 Dscore = "d3") # algoritmo D score

head(d3)

# Qualche rappresentazione grafica ----
# distribuzione dei punteggi 
d_density(d3, 
          graph = "violin", 
          col_point = "blue", 
          include_stats = T)

d_density(d3, 
           graph = "histogram", 
           col_fill = "yellow", 
           include_stats = T)

# distribuzione dei singoli soggetti 

d_point(d3,
        point_size = 1.3, 
        col_point = "pink", 
        x_values = FALSE)

d_point(d3,
        point_size = 1.3, 
        col_point = "pink", 
        order_sbj = "D-decreasing", 
        x_values = FALSE)

# più algoritmi insieme ---- 

dscores = multi_dscore(iat,  # data set pulito
                       ds = "error-inflation") # quali algoritmi

# grafici dei diversi D
dscores$graph


# data set con i diversi D

multi_data = dscores$dscores
head(multi_data)


# dati demografici ----- 
demo_raw = data_clean[[3]] # data set con le informazioni demografiche ricavato 
                          # da clean_iat()
str(demo_raw)

# selezione delle colonne utili ----

demo_raw = demo_raw[, c("participant", "trialcode",
                        "response")]
str(demo_raw)

# reshape del dataset -----
demo <- reshape(demo_raw, 
                timevar = "trialcode", 
                idvar = "participant", 
                direction = "wide")
head(demo)
colnames(demo) <- gsub("response.", '', # toglie "response" dai nomi delle colonne
                       colnames(demo)) 
demo[,  c(3, 6:7)] <- apply(demo[,  c(3, 6:7)], # transforma in numeri le variabili giuste
                            2, as.integer)
head(demo)

# unire i dati demografici ai d score

d3complete = merge(d3, # data set con i D score 
                   demo, # data set con le info. demografiche
                   by = "participant") # id della variabole per unire
head(d3complete[17:29])

# calcolo delle correlazioni 
correlazioni <- data.frame(cor(d3complete[, 
                                          c("dscore_d3", "pol1", "pol2")]))
correlazioni <- round(correlazioni, 2)
correlazioni[upper.tri(correlazioni, diag = TRUE)] <- ""
correlazioni
# non c'è un test di significatività e si può ottene in due modi 
# modo 1: 
cor.test(~ pol1 + dscore_d3, # dscore e atteggiamento
         data = d3complete)
cor.test(~ pol2 + dscore_d3, # dscore e orientamento politico 
         data = d3complete)
cor.test(~ pol1 + pol2,      # orientamento politico e atteggiamento
         data = d3complete)
# modo 2:
# bisogna installare il pacchetto sjPlot: 
# install.packages("sjPlot")
library(sjPlot)
tab_corr(cor(d3complete[, 
                        c("dscore_d3", "pol1", "pol2")]), 
         title = "Correlazioni", 
         show.p = TRUE, 
         digits = 2, 
         triangle = "lower")


# si possono correlare tra loro i vari D score ------
tab_corr(cor(multi_data[,-1]), 
         title = "Correlazioni dei D score", 
         show.p = TRUE, 
         digits = 2, 
         triangle = "lower")

# si può anche investigare se i Dscore ottenuti con correzioni dei tempi
# di risposta diversi correlano in modo divero

multi_dother = multi_dscore(iat, 
                            ds = "built-in")
other_d = multi_dother[[1]]

all_scores = merge(multi_data, 
                   other_d, 
                   by = "participant")
tab_corr(cor(all_scores[,-1]), 
         title = "Correlazioni di tutti i D score", 
         show.p = TRUE, 
         digits = 2, 
         triangle = "lower")
