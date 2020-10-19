mydata <- read.csv("Desktop/bourrat_pilot.csv")

library(foreign) # for reading spss formatted data
library(tidyr)
library(dplyr)
library(stringr) # useful for some string manipulation
library(ggplot2)

## mydata %>%
 ## select(StartDate, EndDate, Status, IPAddress, Progress, Duration, Finished, Recorded Date, ResponseId, RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage) %>%
##  mutate() 
## trying to delete columns i dont need but cant figure it out

mydata %>%
  rename(
    FL_11_DO = Condition,
    Wallet_control = Q1_1
    Resume_control = Q2_1
    Wallet_experimental = Q8_1
    Resume_experimental = Q9_1
  )

names(mydata) <- sub("^FL_11_DO$", "condition", names(mydata))
names(mydata) <- sub("^Q1_1$", "wallet_control", names(mydata))
names(mydata) <- sub("^Q2_1$", "resume_control", names(mydata))
names(mydata) <- sub("^Q8_1$", "wallet_experimental", names(mydata))
names(mydata) <- sub("^Q9_1$", "resume_experimental", names(mydata))

colnames(mydata)

mydata %>% select(wallet_control:condition)

wilcox.test(wallet_control, wallet_experimental = NULL) #not sure how to execute this
wilcox.test(resume_control, resume_experimental = NULL)

