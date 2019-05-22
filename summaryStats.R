# Author: Benjamin Carter
# Date: 2019-05-06
# Project: Skilled Reading Project
# This script will compute summary statistics for participants in the Skilled Reading Project

#### ENVIRONMENT ####
list.of.packages <- c("psych","stats")                                                                             # list of packages this script needs to run
# data
DEMO = "~/Box/LukeLab/SkilledReadingStudy/data/demographics/participantDemographics.csv"
EYES = "~/Box/LukeLab/SkilledReadingStudy/data/eyeTrackingData/allRuns.csv"
# list of variables to summarize
variableList=c("CURRENT_FIX_DURATION","NEXT_SAC_AVG_VELOCITY","NEXT_SAC_DURATION","CURRENT_FIX_PUPIL")

#### COMMANDS ####
# load packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]                           # compare the list to the installed packages list and add missing packages to new list
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)                                          # install missing packages
lapply(list.of.packages,library,character.only = TRUE)                                                                # load packages

# read in data
DEMO <- read.csv(DEMO, header=TRUE,sep=",",na.strings = ".")
EYES <- read.csv(EYES, header =TRUE,sep=",",na.strings = ".")

# compute summary statistics for eye tracking data and put it in the DEMO dataframe
#   function to compute the mean of the input variable for each participant
makeMeans <- function(variable,demographicDF,etDF){
  vName=as.name(paste(as.character(variable),"_µ",sep=""))
  agged <- aggregate(etDF[[variable]], list(etDF$RECORDING_SESSION_LABEL),FUN = "mean", na.rm = TRUE)
  demographicDF[[vName]] <- agged[["x"]][demographicDF[["recording_session_label"]] == agged[["Group.1"]]]
  return(demographicDF)
}

#   function to compute standard deviation of the input variable for each participant
makeSigma <- function(variable,demographicDF,etDF){
  vName=as.name(paste(as.character(variable),"_σ",sep=""))
  agged <- aggregate(etDF[[variable]], list(etDF$RECORDING_SESSION_LABEL),FUN = "SD", na.rm = TRUE)
  demographicDF[[vName]] <- agged[["x"]][demographicDF[["recording_session_label"]] == agged[["Group.1"]]]
  return(demographicDF)
}

#   function for number of leftward or upward saccades
regressions <- function(demographicDF,etDF){
  participants <- levels(demographicDF[["recording_session_label"]])
  for (participant in participants) {
    demographicDF[["REGRESSIONS"]][demographicDF[["recording_session_label"]] == participant] <-
      length(etDF[["RECORDING_SESSION_LABEL"]][etDF[["RECORDING_SESSION_LABEL"]] == participant & etDF[["NEXT_SAC_DIRECTION"]] == "LEFT"]) +
      length(etDF[["RECORDING_SESSION_LABEL"]][etDF[["RECORDING_SESSION_LABEL"]] == participant & etDF[["NEXT_SAC_DIRECTION"]] == "UP"])
  }
  return(demographicDF)
}

#   function for number of fixations
countFixations <- function(demographicDF,etDF){
  participants <- levels(demographicDF[["recording_session_label"]])
  for (participant in participants) {
    demographicDF[["totalFixations"]][demographicDF[["recording_session_label"]] == participant] <-
      length(etDF[["RECORDING_SESSION_LABEL"]][etDF[["RECORDING_SESSION_LABEL"]] == participant])
    }
  return(demographicDF)
}

#### DO THE MATHS I TOLD YOU ####
# means and stanDevs
for (I in variableList) {
  DEMO <- makeMeans(I,DEMO,EYES)
  DEMO <- makeSigma(I,DEMO,EYES)
}

# other statistics
DEMO <- regressions(DEMO,EYES)
DEMO <- countFixations(DEMO,EYES)
