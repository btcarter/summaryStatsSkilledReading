# preamble
#   author: Benjamin Carter
#   date: 2019-07-01
#   project: Skilled Reading Project
#   objective: predict ACT score based on eye tracking measures of reading

#### ENVIRONMENT ####
STUDY="~/Box/LukeLab/SkilledReadingStudy" # path to study directory
ANAL="~/Dropbox/Lab data & Papers/analyses/predictACT" # path to analysis directory
WORK="~/Box/LukeLab/SkilledReadingStudy/results/predictACT" # path to working directory
ET_DATA="~/Box/LukeLab/SkilledReadingStudy/data/eyeTrackingData/allRuns.csv" # path for eye tracking dataframe
SUMM_DATA="~/Box/LukeLab/SkilledReadingStudy/data/demographics/participantDemographicsWithETStats.csv" # path for summary dataframe
OUT_NAME="results.txt" # destination file name for output
list.of.packages <- c("stats","lme4","lmerTest") # list of packages to use in analysis

# load libraries
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]                           # compare the list to the installed packages list and add missing packages to new list
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)                                          # install missing packages
lapply(list.of.packages,library,character.only = TRUE)                                                                # load packages


#### FUNCTIONS ####

#   function to select participants with ACT score
makeParticipantList <- function(OCULO.DF,SUMM.DF) {
  participants <- SUMM.DF[["recording_session_label"]][!is.na(SUMM.DF[["actScore"]])]
  return(participants)
}

#   add ACT score to ET_DATA and then select only participants with an ACT
addACT <- function(OCULO.DF,SUMM.DF,LIST,PARTICIPANT) {
  for (participant in LIST) {
    score <- SUMM.DF[["actScore"]][SUMM.DF[["recording_session_label"]]==participant]
    regressions <- SUMM.DF[["REGRESSIONS"]][SUMM.DF[["recording_session_label"]]==participant]
    OCULO.DF[["actScore"]][OCULO.DF[["RECORDING_SESSION_LABEL"]]==participant] <- score
    OCULO.DF[["REGRESSIONS"]][OCULO.DF[["RECORDING_SESSION_LABEL"]]==participant] <- regressions
  }
  modelDF <- OCULO.DF[!is.na(OCULO.DF[["actScore"]])==TRUE & OCULO.DF[["RECORDING_SESSION_LABEL"]]!=PARTICIPANT]
  testDF <- OCULO.DF[OCULO.DF[["RECORDING_SESSION_LABEL"]]==PARTICIPANT]
  return_list <- list(modelDF,testDF)
  return(return_list)
}

#   function to make a model.
model <- function(INPUT){
  score = lmer(actScore ~ CURRENT_FIX_DURATION + NEXT_SAC_AMPLITUDE + REGRESSIONS + (1|RUN) + (1|RECORDING_SESSION_LABEL), data=INPUT)
  return(score)
}

#   function to predict ACT based on model
predictACT <- function(PARTICIPANT_DATA,MODEL){
  act <- predict(PARTICIPANT_DATA,MODEL)
  return(act)
}


#   function to evaluate predictions
evalPrediction <- function(true,predicted){
  R_sq=cor(true,predicted,use = "pairwise.complete.obs")^2
  return(r_sq)
}

#   function to execute above functions and output data?
order66 <- function(INPUT){
  #all the things
}
  
#### EXECUTE ####

# read in dataframes
ET_DATA <- read.csv(ET_DATA,header=TRUE,sep=",",na.strings = ".")
SUMM_DATA <- read.csv(SUMM_DATA,header=TRUE,sep=",",na.strings = "NA")

# make predictions
participants <- makeParticipantList(ET_DATA,SUMM_DATA)
for (person in participants) {
  data <- addACT(ET_DATA,SUMM_DATA,participants,person)
  
}


# evaluate predictions

# save output?