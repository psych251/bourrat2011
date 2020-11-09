mydata <- read.csv("Desktop/bourrat2011/bourrat2011_replication_pilotB.csv")

# Load libraries
library(foreign) # for reading spss formatted data
library(tidyr)
library(dplyr)
library(stringr) # useful for some string manipulation
library(ggplot2)

# Rename data
names(mydata) <- sub("^FL_11_DO$", "condition", names(mydata))
names(mydata) <- sub("^Q1_1$", "wallet_control", names(mydata))
names(mydata) <- sub("^Q2_1$", "resume_control", names(mydata))
names(mydata) <- sub("^Q8_1$", "wallet_experimental", names(mydata))
names(mydata) <- sub("^Q9_1$", "resume_experimental", names(mydata))

colnames(mydata)

#Main statistical test - Wilcoxon 

mydata %>% select(wallet_control:condition)

testdata <- as.data.frame(apply(mydata[,18:21], MARGIN=2, FUN=as.integer))

wilcox.test(x=testdata$wallet_control, y=testdata$wallet_experimental, na.rm = TRUE)
wilcox.test(x=testdata$resume_control, y=testdata$resume_experimental, na.rm = TRUE)

#Descriptive Stats

wallet.control <- strtoi(c("2", "6"))
resume.control <- strtoi(c("1", "4"))
wallet.experimental <- strtoi(c("7","1"))
resume.experimental <- strtoi(c("8","2"))

wallet.control.mean <- mean(wallet.control)
resume.control.mean <- mean(resume.control)
wallet.experimental.mean <- mean(wallet.experimental)
resume.experimental.mean <- mean(resume.experimental)

wallet.control.median <- median(wallet.control)
resume.control.median <- median(resume.control)
wallet.experimental.median <- median(wallet.experimental)
resume.experimental.median <- median(resume.experimental)
