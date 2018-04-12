#Test script for cell recorded from Igor file CS180312.pxp
#Values for SpikeFreq and CurrentStep exported as .csv
#Test = CS180312
#Test1= CS180319 Cell 1
#Test2 = CS180321 Cell 1
#Test3 = CS180321 Cell 2 ##EXCLUDE?! UNSTABLE?! 
#Test4 = CS180326 Cell 1 ##UNSTABLE

#Set working directory
setwd("~/Documents/DPhil/Data/Bath 5-HT/R/Test")

#Remove scientific notations
options(scipen=999)

#Loading packages
library(plyr)
library(ggplot2)
library(tidyverse)

#Loading data from csv files
rawSpikeFreq <- read.csv("Spikefreq.csv")
rawCurrentStep <- read.csv("Currentstep.csv")

#Writing raw data into data frame, omitting time allowed for 5-HT bath to run through
cleanData <- data.frame(matrix(0,ncol=3))
colnames(cleanData) <- c("spikeFreq","currentStep","serotonin")
cleanData[,3] <- factor(c("before","after"))
#Defining intervals
bathSwitch <- round(nrow(rawSpikeFreq)*.4,0)
serotoninEffect <- round(nrow(rawSpikeFreq)*0.5,0)
endPoint <- serotoninEffect+bathSwitch
# endofData <- ifelse(bathSwitch%%2==0, bathSwitch*2+1, bathSwitch*2)
#Spike frequency
cleanData[1:bathSwitch,1] <- rawSpikeFreq[1:bathSwitch,1]
cleanData[(bathSwitch+1):(bathSwitch*2+1),1] <- rawSpikeFreq[serotoninEffect:endPoint,1]
#Current Step (1e12 to pA, rounded to integer)
cleanData[1:bathSwitch,2] <- round(rawCurrentStep[1:bathSwitch,1]*1e12,0)
cleanData[(bathSwitch+1):(bathSwitch*2+1),2] <- round(rawCurrentStep[serotoninEffect:endPoint,1]*1e12,0)
#Before or after 5HT as factor level
cleanData[1:bathSwitch,3] <- "before"
cleanData[(bathSwitch+1):(bathSwitch*2+1),3] <- "after"

#Obtaining summary statistics
cleanDataSum <- summarySE(cleanData, measurevar="spikeFreq", groupvars=c("currentStep","serotonin"))

#ANOVA
anova <- aov(cleanData$spikeFreq ~ cleanData$currentStep * cleanData$serotonin)
summary(anova)

#Plotting with summary stats
ggplot(data=cleanDataSum, aes(x=currentStep, y=spikeFreq, colour=serotonin)) + 
  geom_errorbar(aes(ymin=spikeFreq-se, ymax=spikeFreq+se), colour="grey", width=2) +
  geom_line() +
  geom_point() +
  xlab("Current step, pA") +
  ylab("Spike Frequency") +
  scale_colour_hue(name="",
                   breaks=c("before", "after"),
                   labels=c("Before 5HT", "After 5HT"))