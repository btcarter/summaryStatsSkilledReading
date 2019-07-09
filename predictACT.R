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
list.of.packages <- c("lme4","lmerTest") # list of packages to use in analysis

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
addACT <- function(OCULO.DF,SUMM.DF,LIST,PARTICIPANT){
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
  modelDF <- OCULO.DF[!is.na(OCULO.DF[["actScore"]]) and OCULO.DF[["RECORDING_SESSION_LABEL"]]!=PARTICIPANT]
  testDF <- OCULO.DF[OCULO.DF[["RECORDING_SESSION_LABEL"]]==PARTICIPANT]
  return_list <- list(modelDF,testDF)
  return(return_list)
}

#   function to make a model. Using gender, race, and age instead of recording_session_label because the label will be different for the predicted subject and is arbitrary.
model <- function(INPUT){
  score = lmer(actScore ~ CURRENT_FIX_DURATION + NEXT_SAC_AMPLITUDE + REGRESSIONS + (1|RUN) + (1|gender) + (1|race) + (1|age), data=INPUT)
  return(score)
}

#   function to predict ACT based on model
predictACT <- function(PARTICIPANT_DATA,MODEL){
  INTERCEPT = 
  BETA_DUR = 
  BETA_AMP =
  BETA_REG = 
  BETA_RUN1 =
  BETA_RUN2 = 
  BETA_RUN3 =
  BETA_gender = 
  BETA_race = 
  BETA_age =
  act = INTERCEPT + BETA_DUR*something + BETA_AMP*somethings
  return(act)
}


#   function to evaluate predictions
evalPrediction <- function()

  
# EXECUTE

#   read in dataframes
ET_DATA <- read.csv(ET_DATA,header=TRUE,sep=",",na.strings = ".")
SUMM_DATA <- read.csv(SUMM_DATA,header=TRUE,sep=",",na.strings = "NA")

# save output?