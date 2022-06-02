# Analize IAT data with the implicitMeasures package
library(implicitMeasures)

data = read.table("IATdata.dat", header=TRUE, sep = "\t") # import data
                                                          # set to your own directory
                                                          # change the file name

# explre data

head(data)
str(data)

table(data$blockcode)
table(data$trialcode)

# clean data

data_clean = clean_iat(
  data, # data set name
  sbj_id = "subject", # column of sbj IDs
  block_id = "blockcode", # column of the block labels
  mapA_practice = "PracticeWhitegood", 
  mapA_test = "TestWhitegood",
  mapB_practice = "PracticeWhitebad",
  mapB_test = "TestWhitebad",
  latency_id = "latency", # column with latency
  accuracy_id = "correct", # column with accuracy
  trial_id = "trialcode", # column with trial labels
  trial_eliminate = c("reminder", "reminder1"), # trials to get rid of
  demo_id = "blockcode", # column of the block labels
  trial_demo = "demografica" # label of the demographic trials
)

# isolate IAT data

iat = data_clean[[1]]

# compute D score

d3 = compute_iat(iat, Dscore = "d3")

# plot 

d_density(d3, graph = "violin")

d_point(d3)

# compute multiple D scores at a time

dscores = multi_dscore(iat, 
                       ds = "error-inflation")

head(dscores[[1]])

dscores$graph

# demograophic infos

demo_raw = data_clean[[3]]
str(demo_raw)

demo_raw = demo_raw[, c("participant", "trialcode",
                        "response")]
# reshape the data set

demo <- reshape(demo_raw, 
                timevar = "trialcode", 
                idvar = "participant", 
                direction = "wide")
colnames(demo) <- gsub("response.", '', colnames(demo))
demo[,  c(3, 6:7)] <- apply(demo[,  c(3, 6:7)], 2, as.integer)
str(demo)

# merge with d3

d3complete = merge(d3, demo, 
                   by = "participant")

# compute correlations
correlations <- data.frame(cor(d3complete[, 
                                          c("dscore_d3", "pol1", "pol2")]))
correlations <- round(correlations, 2)
correlations[upper.tri(correlations, diag = TRUE)] <- ""