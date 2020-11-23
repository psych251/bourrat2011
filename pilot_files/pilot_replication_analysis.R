library(readr)
mydata <- read_csv("https://raw.githubusercontent.com/psych251/bourrat2011/master/Bourrat%202011%20replication_finaldata.csv?token=ARBJXF45N45KIVWL6CQPEM27WWE2C")
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

wilcox.test(x=testdata$wallet_control, y=testdata$wallet_experimental, alternative=c("less"), na.rm = TRUE)
wilcox.test(x=testdata$resume_control, y=testdata$resume_experimental, alternative=c("less"), na.rm = TRUE)

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

# wallet_control median = 2
# wallet_experimental median = 3
# resume_control median = 3
# resume_experimental median = 4

#Plot Data
#Histogram - counts 
ggplot(mydata, aes(x = wallet)) + 
  geom_histogram(
    binwidth = 1,
    center = 1,
  ) +
  facet_wrap(~condition) +
  scale_x_continuous(
    breaks=seq(1,10,1)
  )

ggplot(mydata, aes(x = resume)) + 
  geom_histogram(
    binwidth = 1,
    center = 1,
  ) +
  facet_wrap(~condition) +
  scale_x_continuous(
    breaks=seq(1,10,1)
  )

#means of each condition 

mydata %>%
  group_by(condition) %>%
  summarise(mean_wallet=mean(wallet),
            se_wallet=sd(wallet)/sqrt(n())) %>%
  ggplot(aes(x=condition,
             y=mean_wallet)) +
  geom_errorbar(position=position_dodge(),
                width= .25, 
                aes(ymin=mean_wallet-se_wallet,
                    ymax=mean_wallet+se_wallet)) +
  geom_bar(stat="identity", fill="darkolivegreen4") 

mydata %>%
  group_by(condition) %>%
  summarise(mean_resume=mean(resume),
            se_resume=sd(resume)/sqrt(n())) %>%
  ggplot(aes(x=condition,
             y=mean_resume)) +
  geom_errorbar(position=position_dodge(),
                width= .25, 
                aes(ymin=mean_resume-se_resume,
                    ymax=mean_resume+se_resume)) +
  geom_bar(stat="identity", fill="plum4")


mydata %>%
  group_by(condition) %>%
  summarise(mean_wallet=mean(wallet),
            se_wallet=sd(wallet)/sqrt(n())) %>%
  ggplot(aes(x=condition,
             y=mean_wallet)) +
  geom_errorbar(position=position_dodge(),
                width= .25, 
                aes(ymin=mean_wallet-se_wallet,
                    ymax=mean_wallet+se_wallet)) +
  geom_bar(stat="identity", fill="darkolivegreen4")

mydata %>%
  group_by(condition) %>%
  summarise(mean_resume=mean(resume),
            se_resume=sd(resume)/sqrt(n())) %>%
  ggplot(aes(x=condition,
             y=mean_resume)) +
  geom_errorbar(position=position_dodge(),
                width= .25, 
                aes(ymin=mean_resume-se_resume,
                    ymax=mean_resume+se_resume)) +
  geom_bar(stat="identity", fill="plum4")

#mean plots: original vs. replication (copied from report, idk if something is wrong with it)
knitr::include_graphics("https://github.com/psych251/bourrat2011/blob/master/bourrat2011_mainplot.png?raw=TRUE)

test <- mydata %>% 
  select(c("condition", "resume", "wallet")) %>% 
  pivot_longer(cols=c("resume", "wallet"), names_to="vignette", values_to="result")

test$vignette = factor(test$vignette, levels=c("wallet", "resume"), ordered=TRUE)
test$condition = factor(test$condition, levels=c("ExperimentalBlock","ControlBlock"), ordered=TRUE)

test %>% 
  group_by(condition, vignette) %>% 
  summarize(mean=mean(result), se=sd(result)/sqrt(n())) %>%
  ggplot(aes(x=vignette, y=mean, fill=condition)) + 
  geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(position=position_dodge(0.9),
                width= .5, 
                aes(ymin=mean-se,
                    ymax=mean+se)) +
  ylim(1,6) + 
  scale_fill_manual(values=c("plum3", "plum4"))