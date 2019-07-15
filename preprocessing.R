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
KEEP=c("Luke_Reading_S1", "Luke_Reading_S22", "Luke_Reading_S33", "Luke_Reading_S52", "Luke_Reading_S11", "Luke_Reading_S23", "Luke_Reading_S36", "Luke_Reading_S54", "Luke_Reading_S12", "Luke_Reading_S24", "Luke_Reading_S37", "Luke_Reading_S55", "Luke_Reading_S13", "Luke_Reading_S25", "Luke_Reading_S38", "Luke_Reading_S56", "Luke_Reading_S14", "Luke_Reading_S26", "Luke_Reading_S4",  "Luke_Reading_S6", "Luke_Reading_S16", "Luke_Reading_S27", "Luke_Reading_S45", "Luke_Reading_S7", "Luke_Reading_S17", "Luke_Reading_S28", "Luke_Reading_S47", "Luke_Reading_S8", "Luke_Reading_S18", "Luke_Reading_S29", "Luke_Reading_S48", "Luke_Reading_S9", "Luke_Reading_S19", "Luke_Reading_S3",  "Luke_Reading_S5", "Luke_Reading_S2",  "Luke_Reading_S30", "Luke_Reading_S50", "Luke_Reading_S20", "Luke_Reading_S31", "Luke_Reading_S51") # participants to remove from the analysis

#### COMMANDS ####
READING <- data.frame()
for (i in LIST) {
  runNum = gsub("~\\/Box\\/LukeLab\\/SkilledReadingStudy\\/data\\/eyeTrackingData\\/Run(\\d+).\\d+-\\d+.txt","\\1",i) # extract run number from filename
  i <- read.table(i,header=TRUE,sep="\t")                     # read in table
  i$RUN <- runNum                                             # add run variable
  READING<-rbind(READING,i)                                   # bind table to dataframe
}

# fix broken participant labels
READING <- READING[READING$RECORDING_SESSION_LABEL != "ignoreme",]                                                  # select only columns with real participant IDs
READING$RECORDING_SESSION_LABEL <- gsub("[a-z]*[0]?(\\d+).?","Luke_Reading_S\\1",READING$RECORDING_SESSION_LABEL)     # make recording session labels look like the dicom file names

# remove bad participants
READING <- READING[ READING$RECORDING_SESSION_LABEL %in% KEEP, ]

# create output
write.csv(READING,file = CSV,row.names = FALSE,append = FALSE)
