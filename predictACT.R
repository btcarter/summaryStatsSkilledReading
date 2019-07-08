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


# functions

#   function to select participants with ACT score
makeParticipantList <- function(OCULO.DF,SUMM.DF) {
  participants <- SUMM.DF[["recording_session_label"]][!is.na(SUMM.DF[["actScore"]])]
  return(participants)
}

#   add ACT score to ET_DATA
addACT <- function(OCULO.DF,SUMM.DF,LIST){
  for (participant in LIST) {
    score <- SUMM.DF[["actScore"]][SUMM.DF[["recording_session_label"]]==participant]
    OCULO.DF[["actScore"]][OCULO.DF[["RECORDING_SESSION_LABEL"]]==participant] <- score
  }
  return(OCULO.DF[!is.na(OCULO.DF[["actScore"]])])
}

#   function to make a model
model <- function(INPUT){
  score = lmer(actScore ~ CURRENT_FIX_DURATION + NEXT_SAC_AMPLITUDE + (1|RUN) + (1|RECORDING_SESSION_LABEL), data=INPUT)
  return(score)
}
#   function to make predictions based on model

#   function to evaluate predictions
  
# execute

#   read in dataframes
ET_DATA <- read.csv(ET_DATA,header=TRUE,sep=",",na.strings = ".")
SUMM_DATA <- read.csv(SUMM_DATA,header=TRUE,sep=",",na.strings = "NA")

# output