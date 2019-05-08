# Author: Benjamin Carter
# Date: 2019-05-06
# Project: Skilled Reading Project
# This script will compute summary statistics for participants in the Skilled Reading Project

#### ENVIRONMENT ####
list.of.packages <- c("psych")                                                                             # list of packages this script needs to run
# data
DEMO = "~/Box/LukeLab/SkilledReadingStudy/data/demographics/participantDemographics.csv"
EYES = "~/Box/LukeLab/SkilledReadingStudy/data/eyeTrackingData/allRuns.csv"

#### COMMANDS ####
# load packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]                           # compare the list to the installed packages list and add missing packages to new list
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)                                          # install missing packages
lapply(list.of.packages,library,character.only = TRUE)                                                                # load packages

# read in data
DEMO <- read.csv(DEMO, header=TRUE,sep=",")
EYES <- read.csv(EYES, header =TRUE,sep=",")

# compute summary statistics for eye tracking data and put it in the DEMO dataframe
summarize <- function(variable){
  for (participant in DEMO$recording_session_label) {
    name <- paste(variable,"_µ")
    DEMO$name[DEMO$recording_session_label == participant ] <-  
  }
}