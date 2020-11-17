# load packages 
library(readr)
library(tidyverse)
library(scales)
library(ggpubr)

# read in data
currRegistry <- read_csv("data/currentRegistry.csv")
snapshot <- read_csv("data/cleanedData.csv")
snapshotOrg <- read_csv("data/snapshotW.csv")
benefits <- read_csv("data/benefits2.csv")
p3 <- read_csv("data/P3.csv")
focusGroup <- read_csv("data/focusGroup.csv")
comments <- read_csv("data/comments2.csv")

# fix formatting and update column names

# current registry dates
currRegistry$Date <- as.Date(currRegistry$Date, "%m/%d/%Y")

# age
snapshot$AGE <- factor(snapshot$AGE)
snapshot$AGE <- relevel(snapshot$AGE, "Less than 20 years old")
levels(snapshot$AGE)

# role type
snapshot$ROLE <- factor(snapshot$ROLE)
levels(snapshot$ROLE)[levels(snapshot$ROLE)=="Other (Please describe)"] <- "Other"
snapshot$ROLE <- factor(snapshot$ROLE, levels = c("Director/Owner", "Program administrator", 
                                                  "Lead teacher", "Assistant teacher", "Other"))
levels(snapshot$ROLE)

# years in field
snapshot$YRS_in_FIELD <- factor(snapshot$YRS_in_FIELD, levels = c("Less than 5 years", "5 - 10 years", 
                                                                  "11 - 15 years", "16 - 20 years",
                                                                  "More than 25 years"))
levels(snapshot$YRS_in_FIELD)

# years in current position
snapshot$YRS_in_JOB <- factor(snapshot$YRS_in_JOB, levels = c("Less than 3 years", 
                                                              "3 - 5 years", "6 - 10 years", "11 - 15 years", 
                                                              "16 - 20 years", "More than 20 years"))
levels(snapshot$YRS_in_JOB)

# benefits
snapshot$BENEFITS_ <- factor(snapshot$BENEFITS_)
snapshot$BENEFITS_ <- relevel(snapshot$BENEFITS_, "Yes")
levels(snapshot$BENEFITS_)

names(snapshotOrg)[27] <- "Benefits"
snapshotOrg$Benefits <- factor(snapshotOrg$Benefits)
snapshotOrg$Benefits <- relevel(snapshotOrg$Benefits, "Yes")
levels(snapshotOrg$Benefits)

benefits$Benefit <- factor(benefits$Benefit, level = c("PTO", "SupportTraining", "Health",
                                                       "Retirement", "Membership", "Childcare", "Other"))

# highest level of education
names(snapshot)[17] <- "HighestEd"
snapshot$HighestEd <- factor(snapshot$HighestEd, 
                             levels = c("Did not complete/have not yet completed high school", 
                                        "High school or high school equivalency (HiSET or GED)",
                                        "CDA (Child Development Associate) Credential",
                                        "Associate's degree", "Bachelor's degree", 
                                        "Master's degree", "Doctorate"))
levels(snapshot$HighestEd)[levels(snapshot$HighestEd)=="Did not complete/have not yet completed high school"] <- "Less than high school"
levels(snapshot$HighestEd)[levels(snapshot$HighestEd)=="High school or high school equivalency (HiSET or GED)"] <- "High school"
levels(snapshot$HighestEd)[levels(snapshot$HighestEd)=="CDA (Child Development Associate) Credential"] <- "CDA"
levels(snapshot$HighestEd)[levels(snapshot$HighestEd)=="Associate's degree"] <- "Associates"
levels(snapshot$HighestEd)[levels(snapshot$HighestEd)=="Bachelor's degree"] <- "Bachelor's"
levels(snapshot$HighestEd)[levels(snapshot$HighestEd)=="Masters's degree"] <- "Master's"
levels(snapshot$HighestEd)

# p3
p3$Semester <- factor(p3$Semester, 
                      levels = c("Fall 2015", "Spg 2016", "Sum 2016", "Fall 2016", "Spg 2017", 
                                 "Sum 2017", "Fall 2017", "Spg 2018", "Sum 2018", "Fall 2018"))

# likelihood of remaining in field
names(snapshotOrg)[30] <- "StayInField"
snapshotOrg$StayInField <- factor(snapshotOrg$StayInField, 
                                  levels = c("Extremely likely", "Somewhat likely", "Unsure", 
                                             "Somewhat unlikely", "Extremely unlikely"))

# likelihood of remaining in current position
names(snapshotOrg)[31] <- "StayInCurrentJob"
snapshotOrg$StayInCurrentJob <- factor(snapshotOrg$StayInCurrentJob, 
                                       levels = c("Extremely likely", "Somewhat likely", 
                                                  "Unsure", "Somewhat unlikely", "Extremely unlikely"))

# last wage increase
names(snapshot)[13] <- "LastWageIncrease"
snapshot$LastWageIncrease <- factor(snapshot$LastWageIncrease, 
                                    levels = c("Within the past year", "1 - 3 years ago", 
                                               "3 - 5 years ago", "More than 5 years ago",
                                               "I have not had an increase in wage since I started in my current position"))
levels(snapshot$LastWageIncrease)[levels(snapshot$LastWageIncrease)=="I have not had an increase in wage since I started in my current position"] <- "Never"

names(snapshotOrg)[26] <- "LastWageIncrease"
snapshotOrg$LastWageIncrease <- factor(snapshotOrg$LastWageIncrease, 
                                       levels = c("Within the past year", "1 - 3 years ago", 
                                                  "3 - 5 years ago", "More than 5 years ago",
                                                  "I have not had an increase in wage since I started in my current position"))
levels(snapshotOrg$LastWageIncrease)[levels(snapshotOrg$LastWageIncrease)=="I have not had an increase in wage since I started in my current position"] <- "Never"

# comments
comments$Theme <- factor(comments$Theme, level = c("Education", "Scholarship", "Pay", "Joy", "Registry", "Benefits"))

# focus group
focusGroup$AgeRange <- factor(focusGroup$AgeRange)
fgCareerLevel <- focusGroup[c(1, 9, 10)]
fgCareerLevel[5, 2] <- 1.1
fgCareerLevel[5, 3] <- 9.1
fgCareerLevel[7, 2] <- 0.9
fgCareerLevel[7, 3] <- 8.9

# simplify levels for graphs

# simplified age
snapshot$simpAge <- snapshot$AGE
for(i in 1:nrow(snapshot)){
  if(is.na(snapshot$AGE[i])){
    snapshot$simpAge[i] <- NA}
  else if(snapshot$AGE[i] == "Less than 20 years old"){
    snapshot$simpAge[i] <- "20 - 25 years old"}
}
levels(snapshot$simpAge)[2] <- "25 years or less"
snapshot$simpAge <- factor(snapshot$simpAge)

# simplified role
snapshot$simpRole <- snapshot$ROLE
for(i in 1:nrow(snapshot)){
  if(is.na(snapshot$ROLE[i])){
    snapshot$simpRole[i] <- NA}
  else if(snapshot$ROLE[i] == "Other"){
    snapshot$simpRole[i] <- NA}
  #else if(snapshot$ROLE[i] == "Program administrator"){
  #snapshot$simpRole[i] <- NA}
}
snapshot$simpRole <- factor(snapshot$simpRole)

# simplified highest education
snapshot$simpEdu <- snapshot$HighestEd
for(i in 1:nrow(snapshot)){
  if(is.na(snapshot$HighestEd[i])){
    snapshot$simpEdu[i] <- NA}
  else if(snapshot$HighestEd[i] == "Less than high school"){
    snapshot$simpEdu[i] <- NA}
  else if(snapshot$HighestEd[i] == "CDA"){
    snapshot$simpEdu[i] <- "High school"}
  else if(snapshot$HighestEd[i] == "Doctorate"){
    snapshot$simpEdu[i] <- NA}
}
snapshot$simpEdu <- factor(snapshot$simpEdu)
levels(snapshot$simpEdu)[4] <- "Master's"

# fig 1
# current registry
# line graph
fig1 <- ggplot(data = currRegistry[5:35,], aes(x = Date, y = Current)) +
  theme_minimal() + labs(x = "Date", y = "Count") +
  geom_line(color="steelblue", size = 1.1) + geom_point(color = "steelblue", size = 2) + 
  ylim(c(0, 4000)) + scale_x_date(breaks = pretty_breaks(16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

fig1
ggsave(filename="figures/fig1.png", plot=fig1, device="png", units="in", dpi=500)

# fig 2
# age 
# bar graph
fig2 <- ggplot(data = snapshot, mapping = aes(x = AGE)) + geom_bar(fill="steelblue") + labs(x = "Age", y = "Count") + 
  theme_minimal() + ylim(c(0, 225)) + 
  scale_x_discrete(labels = c("Under 20", "20 - 25", "26 - 30", "31 - 40", "41 - 50", 
                              "51 - 60", "Over 60", "Not reported")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

fig2
ggsave(filename="figures/fig2.png", plot=fig2, device="png", units="in", dpi=500)

# not using
# age 
# pie graph
ageDF <- as.data.frame(table(snapshot$AGE))
names(ageDF)[1] <- "Age"
names(ageDF)[2] <- "Count"
agePercents <- round((ageDF$Count/sum(ageDF$Count)*100), digits = 1)
agePercents=paste0(agePercents, "%")

ggplot(ageDF, aes(x = "", y = Count, fill = Age)) + geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  geom_text(aes(x = 1.6, y = Count, label = agePercents), position = position_stack(vjust = 0.6)) +
  labs(x = NULL, y = NULL) + theme_classic() + 
  theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
        legend.title = element_text(size = 16), legend.text = element_text(size = 12))+
  scale_fill_brewer(palette="Paired")

# fig 3
# role type
# bar graph
fig3 <- ggplot(data = snapshot, mapping = aes(x = ROLE)) + geom_bar(fill="steelblue") + labs(x = "Role type", y = "Count") +
  theme_minimal() + ylim(c(0, 350)) +
  scale_x_discrete(labels = c("Director/Owner", "Program Administrator", "Lead teacher", "Assistant teacher",  "Other", 
                              "Not reported")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

fig3
ggsave(filename="figures/fig3.png", plot=fig3, device="png", units="in", dpi=500)

# fig 4
# role type by age 
# bar graph
fig4 <- ggplot(data = snapshot[!is.na(snapshot$ROLE),]) + 
  geom_bar(mapping = aes(x = AGE, fill = fct_rev(ROLE)), position = "stack") + scale_fill_brewer(palette = "Paired") +  
  labs(x = "Age", y = "Count", fill = "Role type") + theme_minimal() + ylim(c(0, 225)) +
  scale_x_discrete(labels = c("Under 20", "20 - 25", "26 - 30", "31 - 40", "41 - 50", 
                              "51 - 60", "Over 60")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12), legend.position=c(0.2, 0.8),
        legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  guides(fill = guide_legend(reverse = TRUE))

fig4
ggsave(filename="figures/fig4.png", plot=fig4, device="png", units="in", dpi=500)

# fig 5
# years in field 
# pie graph
yearsInField <- c("10 years or less", "11 - 20 years", "More than 20 years")
yearsFieldCount <- c(374, 229, 236)
yearsFieldDF <- data.frame(yearsInField, yearsFieldCount)
yearsFieldPercents <- round((yearsFieldDF$yearsFieldCount/sum(yearsFieldDF$yearsFieldCount)*100), digits = 1)
yearsFieldPercents=paste0(yearsFieldPercents, "%")

fig5 <- ggplot(yearsFieldDF, aes(x = "", y = yearsFieldCount, fill = yearsInField)) + geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  geom_text(aes(x = 1, y = yearsFieldCount, label = yearsFieldPercents), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Years in field") + theme_classic() + 
  theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
        legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  scale_fill_brewer(palette = "Paired")

fig5
ggsave(filename="figures/fig5.png", plot=fig5, device="png", units="in", dpi=500)

# not using
# years in field 
# bar graph
ggplot(data = snapshot, mapping = aes(x = YRS_in_FIELD)) + geom_bar(fill="steelblue") + 
  labs(x = "Years employed in the field", y = "Count") + ylim(c(0, 200)) +
  theme_minimal() + 
  scale_x_discrete(labels = c("Less than 5", "5 - 10", "11 - 15", "16 - 20", "More than 25", "Not reported")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

# fig 6
# years in position
# pie graph
yearsInJob <- c("5 years or less", "6 - 10 years", "11 - 20 years", "More than 20 years")
yearsJobCount <- c(477, 134, 125, 102)
yearsJobDF <- data.frame(yearsInJob, yearsJobCount)
yearsJobDF$yearsInJob <- factor(yearsJobDF$yearsInJob, levels = c("5 years or less", "6 - 10 years", "11 - 20 years", "More than 20 years"))

yearsJobPercents <- round((yearsJobDF$yearsJobCount/sum(yearsJobDF$yearsJobCount)*100), digits = 1)
yearsJobPercents=paste0(yearsJobPercents, "%")

fig6 <- ggplot(yearsJobDF, aes(x = "", y = yearsJobCount, fill = yearsInJob)) + geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  geom_text(aes(x = 1, y = yearsJobCount, label = yearsJobPercents), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Years in current \n position") + theme_classic() + 
  theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
        legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  scale_fill_brewer(palette="Paired")

fig6
ggsave(filename="figures/fig6.png", plot=fig6, device="png", units="in", dpi=500)

# not using
# years in position 
# bar graph
ggplot(data = snapshot, mapping = aes(x = YRS_in_JOB)) + geom_bar(fill="steelblue") + 
  labs(x = "Years employed in current position", y = "Count") + theme_minimal() + 
  scale_x_discrete(labels = c("Less than 3", "3 - 5", "6 - 10", "11 - 15", "16 - 20", "More than 20", "Not reported")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

# not using
# years in field by years experience
# bar graph
ggplot(data = snapshot[!is.na(snapshot$YRS_in_FIELD),]) + 
  geom_bar(mapping = aes(x = YRS_in_FIELD, fill = YRS_in_JOB), position = "dodge") + scale_fill_brewer(palette="Paired")

# fig 7
# benefits offered
# bar graph
fig7 <- ggplot(data = benefits, mapping = aes(x = Benefit)) + geom_bar(fill="steelblue") + theme_minimal() + 
  labs(x = "Benefits offered", y = "Count") + ylim(c(0, 450)) +
  scale_x_discrete(labels = c("Paid time off", "Financial support \n for training", "Health insurance", 
                              "Retirement", "Professional \n membership", "Child care", "Other")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

fig7
ggsave(filename="figures/fig7.png", plot=fig7, device="png", units="in", dpi=500)

# fig 8
# highest education
# bar graph
fig8 <- ggplot(data = snapshot, mapping = aes(x = HighestEd)) + geom_bar(fill="steelblue") + 
  labs(x = "Highest level of education", y = "Count") + ylim(c(0, 300)) + theme_minimal() + 
  scale_x_discrete(labels = c("Less than \n high school", "High school", "CDA", "Associates", "Bachelor's",
                              "Master's", "Doctorate", "Not reported")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

fig8
ggsave(filename="figures/fig8.png", plot=fig8, device="png", units="in", dpi=500)

# fig 9
# p-3
# bar and line graph
fig9 <- ggplot(p3) + 
  geom_col(aes(x = Semester, y = Recipients*2000), size = 1, color = "steelblue", fill = "steelblue") +
  geom_line(aes(x = Semester, y = Amount), size = 1.1, color="red", group = 1) + theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~./2000, name = "Number of recipients"), 
                     name = "Funding awarded", labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

fig9
ggsave(filename="figures/fig9.png", plot=fig9, device="png", units="in", dpi=500)

# fig 10
# likelihood of remaining in current position
# bar graph
fig10 <- ggplot(data = snapshotOrg, mapping = aes(x = StayInCurrentJob)) + geom_bar(fill="steelblue") + 
  labs(x = "Likelihood of remaining in current position", y = "Count") + theme_minimal() + 
  scale_x_discrete(labels = c("Extremely likely", "Somewhat likely", "Unsure", "Somewhat unlikely", 
                              "Extremely unlikely", "Not reported")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

fig10
ggsave(filename="figures/fig10.png", plot=fig10, device="png", units="in", dpi=500)

# not using
# wage increase by liklihood current position
# bar graph
ggplot(data = snapshotOrg) + 
  geom_bar(mapping = aes(x = LastWageIncrease, fill = StayInCurrentJob), position = "dodge") + 
  labs(x = "", y = "") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12)) +
  ggtitle("Likelihood of remaining in current job over \n the next three years by date of last wage increase") +
  ylim(c(0, 225)) + 
  scale_fill_discrete(name = "Remain in current job", 
                      labels = c("Extremely likely", "Somewhat likely", "Unsure", "Somewhat unlikely", 
                                 "Extremely unlikely", "Not reported")) +
  scale_x_discrete(labels = c("Within the past year", "1 - 3 years ago", "3 - 5 years ago", 
                              "More than 5 years ago", "Never", "Not reported")) 

# not using
# benefits by liklihood current position
# bar graph
ggplot(data = snapshotOrg) + 
  geom_bar(mapping = aes(x = StayInCurrentJob, fill = Benefits), position = "dodge") + 
  labs(x = "Reported likelihood of remaining in current position for the next three years", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 12), 
                          plot.title = element_text(size = 20), axis.title=element_text(size = 16)) +
  scale_fill_discrete(labels = c("Yes", "No", "Not reported")) +
  scale_x_discrete(labels = c("Extremely likely", "Somewhat likely", "Unsure", "Somewhat unlikely", 
                              "Extremely unlikely", "Not reported"))

# fig 11
# likelihood of remaining in field
# bar graph
fig11 <- ggplot(data = snapshotOrg, mapping = aes(x = StayInField)) + geom_bar(fill="steelblue") + 
  labs(x = "Likelihood of remaining in the field", y = "Count") + theme_minimal() + 
  scale_x_discrete(labels = c("Extremely likely", "Somewhat likely", "Unsure", "Somewhat unlikely", 
                              "Extremely unlikely", "Not reported")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

fig11
ggsave(filename="figures/fig11.png", plot=fig11, device="png", units="in", dpi=500)

# fig 12
# comment themes
# bar graph
fig12 <- ggplot(data = comments, mapping = aes(x = Theme)) + geom_bar(fill="steelblue") + theme_minimal() + 
  labs(x = "Comment theme", y = "Count") + ylim(c(0, 45)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

fig12
ggsave(filename="figures/fig12.png", plot=fig12, device="png", units="in", dpi=500)

# not using
# comment themes
# pie graph
comments <- c("Education", "Scholarship", "Pay", "Joy", "Benefits", "Registry")
commentsCount <- c(42, 34, 31, 10, 7, 7)
commentsDF <- data.frame(comments, commentsCount)
commentsPercents <- round((commentsDF$commentsCount/sum(commentsDF$commentsCount)*100), digits = 1)
commentsPercents=paste0(commentsPercents, "%")

ggplot(commentsDF, aes(x = "", y = commentsCount, fill = comments)) + geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  geom_text(aes(x = 1.65, y = commentsCount, label = commentsPercents), position = position_stack(vjust = 0.6)) +
  labs(x = NULL, y = NULL, fill = "Comment themes") + theme_classic() + 
  theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
        legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  scale_fill_brewer(palette="Paired")

# fig 13
# focus group ages
# bar graph
fig13 <- ggplot(data = focusGroup, mapping = aes(x = AgeRange)) + geom_bar(fill="steelblue") + 
  labs(x = "Age", y = "Count") +  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

fig13
ggsave(filename="figures/fig13.png", plot=fig13, device="png", units="in", dpi=500)

# fig 14
# education by age
# bar graph
fig14 <- ggplot(data = snapshot[!is.na(snapshot$simpEdu),], aes(x = simpAge, fill = simpEdu)) + 
  geom_bar(width = 0.75, position = "dodge") + 
  labs(fill = "Highest level \n of education", y = "Count", x = "Age") + 
  theme_minimal() + scale_fill_brewer(palette="Paired") +
  scale_x_discrete(labels = c("25 or younger", "26 - 30", "31 - 40", "41 - 50", "51 - 60", "Over 60")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12), 
        legend.position="bottom", legend.direction = "horizontal",
        legend.title = element_text(size = 12), legend.text = element_text(size = 10))

fig14
ggsave(filename="figures/fig14.png", plot=fig14, device="png", units="in", dpi=500)

# fig 15
# education by role
# bar graph
fig15 <- ggplot(data = snapshot[!is.na(snapshot$simpRole),], aes(x = simpRole, fill = simpEdu)) + 
  geom_bar(width = 1, position = "dodge") + scale_fill_brewer(palette="Paired") +
  labs(fill = "Highest level \n of education", y = "Count", x = "Role type") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12), 
        legend.position="bottom", legend.direction = "horizontal",
        legend.title = element_text(size = 12), legend.text = element_text(size = 10))

fig15
ggsave(filename="figures/fig15.png", plot=fig15, device="png", units="in", dpi=500)

# fig 16
# focus group career level 
# paired line graph
fig16 <- ggplot(fgCareerLevel) + 
  geom_segment(aes(x = 1, xend = 2, y = PriorCareerLevel, yend = CareerLevel), color="steelblue", size = 0.75) + 
  geom_point(aes(x = 1, y = PriorCareerLevel), color = "steelblue", size = 1.25) +
  geom_point(aes(x = 2, y = CareerLevel), color = "steelblue", size = 1.25) + theme_bw() + 
  scale_x_discrete(breaks = c("1", "2"), limits = c(1, 2),
                   labels = c("Before receiving \n financial assistance", 
                              "After receiving \n financial assistance")) +
  theme_minimal() +
  scale_y_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) + 
  labs(x = "", y = "Career path level") +
  theme(axis.text=element_text(size = 10), 
        plot.title = element_text(size = 12), axis.title=element_text(size = 12))

fig16
ggsave(filename="figures/fig16.png", plot=fig16, device="png", units="in", dpi=500)

# load tables packages
library(flextable)
library(officer)
library(webshot2)

# load tables data
wagesRole <- read_csv("data/wagesRole.csv")
roles <- read_csv("data/roles.csv")
wagesProgram <- read_csv("data/wagesProgram.csv")

# table 1
# wages by role
table1 <- flextable(wagesRole)
table1 <- theme_zebra(table1)
table1 <- autofit(table1)
table1 <- border(table1, border.bottom = fp_border(width = 1, color = "steel blue"),
                 border.top = fp_border(width = 1, color = "steel blue"),
                 border.left = fp_border(width = 1, color = "steel blue"),
                 border.right = fp_border(width = 1, color = "steel blue"),
                 part = "header")
table1 <- border(table1, border.bottom = fp_border(width = 1, color = "steel blue"), 
                 border.left = fp_border(width = 1, color = "steel blue"),
                 border.right = fp_border(width = 1, color = "steel blue"),
                 part = "body")
table1

save_as_image(x = table1, path = "figures/table1.png", webshot = "webshot2")

# table 2
# wages by program
table2 <- flextable(wagesProgram)
table2 <- theme_zebra(table2)
table2 <- autofit(table2)
table2 <- border(table2, border.bottom = fp_border(width = 1, color = "steel blue"),
                 border.top = fp_border(width = 1, color = "steel blue"),
                 border.left = fp_border(width = 1, color = "steel blue"),
                 border.right = fp_border(width = 1, color = "steel blue"),
                 part = "header")
table2 <- border(table2, border.bottom = fp_border(width = 1, color = "steel blue"), 
                 border.left = fp_border(width = 1, color = "steel blue"),
                 border.right = fp_border(width = 1, color = "steel blue"),
                 part = "body")
table2

save_as_image(x = table2, path = "figures/table2.png", webshot = "webshot2")

# table 3
# role types
table3 <- flextable(roles)
table3 <- theme_zebra(table3)
table3 <- autofit(table3)
table3 <- merge_v(table3)
table3 <- border(table3, border.bottom = fp_border(width = 1, color = "steel blue"),
                 border.top = fp_border(width = 1, color = "steel blue"),
                 border.left = fp_border(width = 1, color = "steel blue"),
                 border.right = fp_border(width = 1, color = "steel blue"),
                 part = "header")
table3 <- border(table3, border.bottom = fp_border(width = 1, color = "steel blue"), 
                 border.left = fp_border(width = 1, color = "steel blue"),
                 border.right = fp_border(width = 1, color = "steel blue"),
                 part = "body")
table3

save_as_image(x = table3, path = "figures/table3.png", webshot = "webshot2")
