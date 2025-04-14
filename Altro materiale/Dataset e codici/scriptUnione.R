getwd()
setwd("C:/Users/ottae/Documents/GitHub/seminarioImplicite/Altro materiale/Dataset e codici/IAT compilati")

files = list.files("C:/Users/ottae/Documents/GitHub/seminarioImplicite/Altro materiale/Dataset e codici/IAT compilati", 
                   pattern=".dat")
files
files = files[grepl("dati", files)]

data = list()

for(i in 1:length(files)) {
  data[[i]] = read.table(files[i], header = TRUE, sep = "\t")
}

all_data = NULL
temp = NULL

for (i in 1:length(data)){
  temp = data[[i]]
  all_data = rbind(all_data, temp)
}
write.table(all_data, 
            file =  "all_data.dat", sep = "\t")
library(implicitMeasures)
table(all_data$blockcode)

iat_clean = clean_iat(all_data, 
                      sbj_id = "subject", 
                      block_id = "blockcode", 
                      mapA_practice = "compatibletest1", 
                      mapA_test = "compatibletest2", 
                      mapB_practice = "incompatibletest1", 
                      mapB_test = "incompatibletest2", 
                      latency_id = "latency", 
                      accuracy_id = "correct", 
                      demo_id = "blockcode", 
                      trial_demo = c("demografica", "preferenza"))
iat = iat_clean[[1]]

dscore = compute_iat(iat, Dscore = "d1")
descript_d(dscore)

demo_long = iat_clean[[3]]
demo_long = demo_long[, c("participant", "trialcode", "response")]
demo <- reshape(demo_long,
                timevar = "trialcode",
                idvar = "participant",
                direction = "wide")
colnames(demo) = gsub("response.", "", colnames(demo))
demo[, c(3, (ncol(demo)-2):ncol(demo))] <- apply(demo[, c(3, (ncol(demo)-2):ncol(demo))], 2, as.integer)
