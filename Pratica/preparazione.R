data = read.table("CioccoRazzaBuilt.dat", 
           header=TRUE, sep = "\t")
table(data$blockcode)
table(data$trialcode)
# get rid of the chocolate evaluation

data = data[!grepl("pref_", data[["trialcode"]]), ]
data = data[!grepl("int", data[["trialcode"]]), ]
data = data[!grepl("dieta", data[["trialcode"]]), ]
data = data[!grepl("fame", data[["trialcode"]]), ]


# data set with inly the plain race IAT block code, demographic data 

small = data[data$blockcode %in% c("consenso", "demografica", 
                                   "Plainbadgood", "PlainPracticeWhitebad", 
                                   "PlainPracticeWhitegood", 
                                   "PlainTestWhitebad", "PlainTestWhitegood",
                                   "PlainWhiteBlack", "PlainWhiteBlack2nd"), ]




# save demographic data 

demo <- data[data$blockcode %in% "demografica", -c(1:3, 5, 6)]
demo <- demo[!grepl("int", demo[["trialcode"]]), -c(2, 5:7)]
demo$trialcode <- as.character(demo$trialcode)
demoW <- reshape(demo, timevar = "trialcode", idvar = "subject", 
                 direction = "wide")

colnames(demoW) <- gsub("response.", '', colnames(demoW))

demoW[, c(2:8)] <- apply(demoW[, c(2:8)], 2, as.character)

demoW[,  c(3, 7:8)] <- apply(demoW[,  c(3, 7:8)], 2, as.integer)


# same thing on the small dataset
# questi son i dati deellademografica da salvare a parte per l'eserciatzine con 
# shinyApp
demo <- small[small$blockcode %in% "demografica", -c(1:3, 5, 6)]
demo <- demo[!grepl("int", demo[["trialcode"]]), -c(2, 5:7)]
demo$trialcode <- as.character(demo$trialcode)
demoW <- reshape(demo, timevar = "trialcode", idvar = "subject", 
                 direction = "wide")

colnames(demoW) <- gsub("response.", '', colnames(demoW))

demoW[, c(2:8)] <- apply(demoW[, c(2:8)], 2, as.character)

demoW[,  c(3, 7:8)] <- apply(demoW[,  c(3, 7:8)], 2, as.integer)

write.table(demoW,"Demgrafica.csv", row.names = FALSE, sep = ",")

# isoolo i dati per la shiny app 

table(small$blockcode)
small$blockcode = gsub("Plain", "", small$blockcode)

write.table(small, "shinyAppdata.csv", col.names = TRUE, 
            row.names = FALSE, sep = ",")

# import data 

data = read.table("IATdata.dat", header=TRUE, sep = "\t")
table(data$blockcode)

library(implicitMeasures)
p =clean_iat(
  data,
  sbj_id = "subject",
  block_id = "blockcode",
  mapA_practice = "PracticeWhitegood",
  mapA_test = "TestWhitegood",
  mapB_practice = "PracticeWhitebad",
  mapB_test = "PracticeWhitebad",
  latency_id = "latency",
  accuracy_id = "correct",
  trial_id = "trialcode",
  trial_eliminate = c("reminder", "reminder1"),
  demo_id = "blockcode",
  trial_demo = "demografica"
)
