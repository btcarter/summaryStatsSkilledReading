# Author: Benjamin Carter
# Date: 2019-05-07
# Project: Skilled Reading Project
# Purpose: concatenate the ET runs into one dataframe for the Skilled Reading Project
# Warning
#   1. All input fixation reports must have the same variables, in the same order. The script assumes this is the case and will not check for it.

#### ENVIRONMENT ####
RUN1.1 = "~/Box/LukeLab/SkilledReadingStudy/data/eyeTrackingData/Run1.1-44.txt"
RUN2.1 = "~/Box/LukeLab/SkilledReadingStudy/data/eyeTrackingData/Run2.1-44.txt"
RUN3.1 = "~/Box/LukeLab/SkilledReadingStudy/data/eyeTrackingData/Run3.1-44.txt"
RUN1.2 = "~/Box/LukeLab/SkilledReadingStudy/data/eyeTrackingData/Run1.45-56.txt"
RUN2.2 = "~/Box/LukeLab/SkilledReadingStudy/data/eyeTrackingData/Run2.45-56.txt"
RUN3.2 = "~/Box/LukeLab/SkilledReadingStudy/data/eyeTrackingData/Run3.45-56.txt"
LIST <- c(RUN1.1,RUN2.1,RUN3.1,RUN1.2,RUN2.2,RUN3.2)
CSV="~/Box/LukeLab/SkilledReadingStudy/data/eyeTrackingData/allRuns.csv"

#### COMMANDS ####
READING <- data.frame()
for (i in LIST) {
  runNum = gsub("~\\/Box\\/LukeLab\\/SkilledReadingStudy\\/data\\/eyeTrackingData\\/Run(\\d+).\\d-\\d+.txt","\\1",i) # extract run number from filename
  i <- read.table(i,header=TRUE,sep="\t")                     # read in table
  i$RUN <- runNum                                             # add run variable
  READING<-rbind(READING,i)                                   # bind table to dataframe
}

# fix broken participant labels
READING <- READING[READING$RECORDING_SESSION_LABEL != "ignoreme",]                                                  # select only columns with real participant IDs
READING$RECORDING_SESSION_LABEL <- gsub("[a-z]*[0]?(\\d+).?","Luke_Reading_S\\1",READING$RECORDING_SESSION_LABEL)     # make recording session labels look like the dicom file names

# create output
write.csv(READING,file = CSV,row.names = FALSE,append = FALSE)
