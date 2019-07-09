# preamble
#   author: Benjamin Carter
#   date: 2019-07-01
#   project: Skilled Reading Project
#   objective: predict ACT score based on eye tracking measures of reading

# environment
STUDY="/Box/LukeLab/SkilledReadingStudy" # path to study directory
ANAL="/Dropbox/Lab data & Papers/analyses/predictACT" # path to analysis directory
WORK="~/Box/LukeLab/SkilledReadingStudy/results/predictACT" # path to working directory
ET_DATA="~/Box/LukeLab/SkilledReadingStudy/data/eyeTrackingData/allRuns.csv" # path for eye tracking dataframe
SUMM_DATA="~/Box/LukeLab/SkilledReadingStudy/data/demographics/participantDemographicsWithETStats.csv" # path for summary dataframe
OUT_NAME="results.txt" # destination file name for output
list.of.packages <- c("stats","lme4","lmerTest") # list of packages to use in analysis

# load libraries
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]                           # compare the list to the installed packages list and add missing packages to new list
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)                                          # install missing packages
lapply(list.of.packages,library,character.only = TRUE)                                                                # load packages


# FUNCTIONS

#   function to select participants with ACT score
makeParticipantList <- function(OCULO.DF,SUMM.DF) {
  participants <- SUMM.DF[["recording_session_label"]][!is.na(SUMM.DF[["actScore"]])]
  return(participants)
}

#   add ACT score to ET_DATA and then select only participants with an ACT
addACT <- function(OCULO.DF,SUMM.DF,LIST,PARTICIPANT) {
  for (participant in LIST) {
    score <- SUMM.DF[["actScore"]][SUMM.DF[["recording_session_label"]]==participant]
    gender <- SUMM.DF[["male.female"]][SUMM.DF[["recording_session_label"]]==participant]
    race <- SUMM.DF[["ethnicity"]][SUMM.DF[["recording_session_label"]]==participant]
    age <-  SUMM.DF[["scanAge"]][SUMM.DF[["recording_session_label"]]==participant]
    OCULO.DF[["actScore"]][OCULO.DF[["RECORDING_SESSION_LABEL"]]==participant] <- score
    OCULO.DF[["gender"]][OCULO.DF[["RECORDING_SESSION_LABEL"]]==participant] <- gender
    OCULO.DF[["race"]][OCULO.DF[["RECORDING_SESSION_LABEL"]]==participant] <- race
    OCULO.DF[["age"]][OCULO.DF[["RECORDING_SESSION_LABEL"]]==participant] <- age
  }
  modelDF <- OCULO.DF[!is.na(OCULO.DF[["actScore"]])==TRUE & OCULO.DF[["RECORDING_SESSION_LABEL"]]!=PARTICIPANT]
  testDF <- OCULO.DF[OCULO.DF[["RECORDING_SESSION_LABEL"]]==PARTICIPANT]
  return_list <- list(modelDF,testDF)
  return(return_list)
}

#   function to make a model. Using gender, race, and age instead of recording_session_label because the label will be different for the predicted subject and is arbitrary.
model <- function(INPUT){
  score = lmer(actScore ~ CURRENT_FIX_DURATION + NEXT_SAC_AMPLITUDE + REGRESSIONS + (1|RUN) + (1|gender) + (1|race) + (1|age), data=INPUT)
  #score = lmer(actScore ~ CURRENT_FIX_DURATION + NEXT_SAC_AMPLITUDE + REGRESSIONS + (1|RUN) + (1|RECORDING_SESSION_LABEL), data=INPUT)
  return(score)
}

#   function to predict ACT based on model
predictACT <- function(PARTICIPANT_DATA,MODEL){
  act <- predict(PARTICIPANT_DATA,MODEL)
  return(act)
}


#   function to evaluate predictions
evalPrediction <- function(true,predicted){
  
}

  
# EXECUTE

#   read in dataframes
ET_DATA <- read.csv(ET_DATA,header=TRUE,sep=",",na.strings = ".")
SUMM_DATA <- read.csv(SUMM_DATA,header=TRUE,sep=",",na.strings = "NA")

LIST <- makeParticipantList(ET_DATA,SUMM_DATA) #debug
BigList <- addACT(ET_DATA,SUMM_DATA,LIST,"Luke_Reading_S19") #debug
# save output?