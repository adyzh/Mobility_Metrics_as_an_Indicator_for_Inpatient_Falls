###### Use Data Prep From MatchingExampleCode.R #####
wfirstamp <- rbind(FallNMatch, FallYMatch)
wfirstamp <- subset(wfirstamp, LOScalc >= 7 & LOScalc < 22)

# Check to see if Match gives same data set, results should be 1923 Non-Fall 63 Fall
by.IDPost <- wfirstamp[match(unique(wfirstamp$PAT_ENC_CSN_ID), wfirstamp$PAT_ENC_CSN_ID),]
table(by.IDPost$FallInjuryYN)

#### Danger Zone ####
### Necessary Variables: Assign if patient is in a given danger zone on a given day
wfirstamp$inLowDZ <- ifelse(wfirstamp$IMP.AMPAC_MOB_RAW.ALL >= 8 & wfirstamp$IMP.AMPAC_MOB_RAW.ALL <= 12, TRUE, FALSE)
wfirstamp$inMidDZ <- ifelse(wfirstamp$IMP.AMPAC_MOB_RAW.ALL >= 12 & wfirstamp$IMP.AMPAC_MOB_RAW.ALL <= 16, TRUE, FALSE)
wfirstamp$inHighDZ <- ifelse(wfirstamp$IMP.AMPAC_MOB_RAW.ALL >= 16 & wfirstamp$IMP.AMPAC_MOB_RAW.ALL <= 20, TRUE, FALSE)

# Get total number of days in each zone as well as proportion of days
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% mutate(DaysinLowDZ = sum(inLowDZ)) %>%  mutate(DaysinMidDZ = sum(inMidDZ)) %>%  mutate(DaysinHighDZ = sum(inHighDZ))
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% mutate(PropinLowDZ = DaysinLowDZ / LOScalc) %>%  mutate(PropinMidDZ = DaysinMidDZ / LOScalc) %>%  mutate(PropinHighDZ = DaysinHighDZ / LOScalc)

# Get One Instance of Each Patient
by.IDPost <- wfirstamp[match(unique(wfirstamp$PAT_ENC_CSN_ID), wfirstamp$PAT_ENC_CSN_ID),]

# Compare Fallers and Non-Fallers for each different zone, you will have to switch out DaysinMidDZ / PropinMidDZ to DaysinLowDZ etc.
t.test(DaysinMidDZ~FallInjuryYN, data = by.IDPost)
t.test(PropinMidDZ~FallInjuryYN, data = by.IDPost)
NoFall <- subset(by.IDPost, FallInjuryYN == 0)
Fall <- subset(by.IDPost, FallInjuryYN == 1)
suppressWarnings(ks.test(NoFall$DaysinMidDZ, Fall$DaysinMidDZ))
suppressWarnings(ks.test(NoFall$PropinMidDZ, Fall$PropinMidDZ))

# See Distribution
tapply(by.IDPost$PropinMidDZ, by.IDPost$FallInjuryYN, summary)


# Plot difference
ggplot(by.IDPost, aes(x=as.factor(FallInjuryYN), y=PropinMidDZ, color=as.factor(FallInjuryYN))) + geom_boxplot() + scale_color_manual(labels = c("Non-Fall", "Fall"), values = c("blue", "red")) + labs(title = "Proportion of LOS in 'Danger Zone' (Raw AM-PAC 12-16)", x="", y="Days in Danger Zone / Length of Stay" , color="") + theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12))


#### Run the Same for JHHLM ####
### Necessary Variables: Assign if patient is in a given mobility level on a given day
wfirstamp$high_jhhlm <- ifelse(wfirstamp$IMP.JHHLM.max >= 7, 1, 0)
wfirstamp$mid_jhhlm <- ifelse(wfirstamp$IMP.JHHLM.max < 7 & wfirstamp$IMP.JHHLM.max > 4, 1, 0)
wfirstamp$low_jhhlm <- ifelse(wfirstamp$IMP.JHHLM.max < 5, 1, 0)

# Get total number of days at each level as well as proportion of days
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% mutate(daysHighMob = sum(na.omit(high_jhhlm)))  %>% mutate(daysMidMob = sum(na.omit(mid_jhhlm)))  %>% mutate(daysLowMob = sum(na.omit(low_jhhlm)))
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% mutate(propHighMob = daysHighMob / LOScalc) %>% mutate(propMidMob = daysMidMob / LOScalc) %>% mutate(propLowMob = daysLowMob / LOScalc)

by.IDPost <- wfirstamp[match(unique(wfirstamp$PAT_ENC_CSN_ID), wfirstamp$PAT_ENC_CSN_ID),]

t.test(LOScalc~FallInjuryYN, data = by.IDPost)
t.test(propLowMob~FallInjuryYN, data = by.IDPost)
NoFall <- subset(by.IDPost, FallInjuryYN == 0)
Fall <- subset(by.IDPost, FallInjuryYN == 1)
suppressWarnings(ks.test(NoFall$LOScalc, Fall$LOScalc))
suppressWarnings(ks.test(NoFall$propHighMob, Fall$propHighMob))

tapply(by.IDPost$propLowMob, by.IDPost$FallInjuryYN, summary)

# Individual Plots - Don't really need if no difference
ggplot(by.IDPost, aes(x=as.factor(FallInjuryYN), y=daysHighMob, color=as.factor(FallInjuryYN))) + geom_boxplot() + scale_color_manual(labels = c("Non-Fall", "Fall"), values = c("blue", "red")) + labs(title = "Days Spent in JH-HLM 7/8", x="", y="Days in JH-HLM 7/8" , color="") + theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12))
ggplot(by.IDPost, aes(x=as.factor(FallInjuryYN), y=propLowMob, color=as.factor(FallInjuryYN))) + geom_boxplot() + scale_color_manual(labels = c("Non-Fall", "Fall"), values = c("blue", "red")) + labs(title = "Proportion of LOS in JH-HLM <= 4", x="", y="Days in JH-HLM <= 4 / Number of Scores" , color="") + theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12))

ggplot(by.IDPost, aes(x=as.factor(FallInjuryYN), y=LOScalc, color=as.factor(FallInjuryYN))) + geom_boxplot() + scale_color_manual(labels = c("Non-Fall", "Fall"), values = c("blue", "red")) + labs(title = "Length of Stay Matched Population", x="", y="Length of Stay" , color="") + theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12))

#### Stacking barplots ####
## Plotting population median ontop of eachother
by.IDPost <- by.IDPost %>% group_by(FallInjuryYN) %>% mutate(medLowMob = median(propLowMob)) %>% mutate(avgLowMob = mean(propLowMob)) %>% mutate(medMidMob = median(propMidMob)) %>% mutate(avgMidMob = mean(propMidMob)) %>% mutate(medHighMob = median(propHighMob)) %>% mutate(avgHighMob = mean(propHighMob))

## Aggregating population by fall together and seeing results there
NoFall <- subset(by.IDPost, FallInjuryYN == 0)
Fall <- subset(by.IDPost, FallInjuryYN == 1)
FDays <- sum(Fall$daysLowMob, Fall$daysMidMob, Fall$daysHighMob)
NFDays <- sum(NoFall$daysLowMob, NoFall$daysMidMob, NoFall$daysHighMob)
by.IDPost$totPopDays <- ifelse(by.IDPost$FallInjuryYN == 1, FDays, NFDays)

by.IDPost <- by.IDPost %>% group_by(FallInjuryYN) %>% mutate(totalPropLowMob = sum(daysLowMob) / totPopDays) %>% mutate(totalPropMidMob = sum(daysMidMob) / totPopDays) %>% mutate(totalPropHighMob = sum(daysHighMob) / totPopDays)
table(by.IDPost$totalPropHighMob, by.IDPost$FallInjuryYN)
table(by.IDPost$totalPropMidMob, by.IDPost$FallInjuryYN)
table(by.IDPost$totalPropLowMob, by.IDPost$FallInjuryYN)


#Hard coding for graph
mobLevels <- c('Low Mobility', 'Middle Mobility', 'High Mobility')#, 'Low Mobility', 'Middle Mobility', 'High Mobility')
fallLevel <- c('Injurious Faller\n(n = 72)', 'Injurious Faller\n(n = 72)', 'Injurious Faller\n(n = 72)', 'Non-Faller\n(n = 1273)', 'Non-Faller\n(n = 1273)', 'Non-Faller\n(n = 1273)')
AvgProp <- c(28, 38, 34, 19, 41, 39)
MedProp <- c(0.19375, 0.333333333333333, 0.25, 0, 0.375, 0.333333333333333)
AggProp <- c(0.268939393939394, 0.393939393939394, 0.337121212121212, 0.193363539445629, 0.410047974413646, 0.396588486140725)
bp <- data.frame(mobLevels, fallLevel, AvgProp, MedProp, AggProp)
bp$mobLevels <- factor(bp$mobLevels, levels = rev(mobLevels))

### Plotting
#Average of Prop
#version 1
# ggplot(bp, aes(x=fallLevel, y=AvgProp,fill=mobLevels)) + geom_bar(position='stack', stat='identity', color="black") + 
#   scale_fill_manual(values = c('#CCCCCC', '#666666', '#333333')) + 
#   theme(plot.title = element_text(face="bold", hjust = 0.5), axis.text.x = element_text(face="bold", size = 12), legend.title=element_blank(), legend.text = element_text(face = "bold", size=10), legend.background = element_rect(color = "black"), legend.position=c(1.125,0.85), plot.margin = unit(c(.5,9,0.5,0.5), "lines") ) +
#   labs(title = (("Population Average Proportion of Hospital Stay Pre-Fall \nat Mobility Levels by Fall Status")), x="", y=bquote(bold("Average Proportion of Pre-Fall Spent at Mobility"))) +
#   theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(color="black")) +
#   coord_cartesian(ylim= c(0,1), clip="off") +
#   annotate("label", x = Inf, y = 0.6, label = " Injurious Faller\nHigh Mobility: 0.340\nMiddle Mobility: 0.383\nLow Mobility: 0.276\n\n Non-Faller\nHigh Mobility: 0.391\nMiddle Mobility: 0.412\nLow Mobility: 0.192", hjust = -0.1, size = 3.5, fontface = "bold") +
#   annotate("label", x = Inf, y = 0.3, label = "P-Values of Difference \n           in Means\nHigh Mobility: 0.1958\nMiddle Mobility: 0.4338\nLow Mobility: 0.0272", hjust = -0.1, size = 3.5, fontface = "bold")

# version 2
ggplot(bp, aes(x=fallLevel, y=AvgProp,fill=mobLevels)) + geom_bar(position='stack', stat='identity', color="black") + 
  scale_fill_manual(values = c('#CCCCCC', '#666666', '#333333')) + 
  theme(plot.title = element_text(face="bold", hjust = 0.5), axis.text.x = element_text(face="bold", size = 12), legend.title=element_blank(), legend.text = element_text(face = "bold", size=10), legend.background = element_rect(color = "black"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines") ) +
  labs(title = (("Population Average Percentage of Hospital Stay Pre-Fall \nat Mobility Levels by Fall Status")), x="", y=bquote(bold("Average Percentage of Pre-Fall Spent at Mobility"))) +
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(color="black")) +
  coord_cartesian(ylim= c(0,100), clip="off") +
  annotate("text", x = 1, y=83,label = "34%", color="black", size=5, fontface="bold") +
  annotate("text", x = 1, y=47,label = "38%", color="white", size=5, fontface="bold") +
  annotate("text", x = 1, y=14,label = "28%*", color="white", size=5, fontface="bold") +
  annotate("text", x = 2, y=79.5,label = "39%", color="black", size=5, fontface="bold") +
  annotate("text", x = 2, y=39.5,label = "41%", color="white", size=5, fontface="bold") +
  annotate("text", x = 2, y=9.5,label = "19%*", color="white", size=5, fontface="bold") +
  annotate("text", x = Inf, y = -Inf, label = "*p-value < 0.05", hjust = 0, size = 4, fontface = "bold")


#Median of Prop
ggplot(bp, aes(x=fallLevel, y=MedProp,fill=mobLevels)) + geom_bar(position='stack', stat='identity', color="black") + 
  scale_fill_manual(values = c('green', 'yellow', 'red')) + theme(legend.title=element_blank()) +
  labs(title = "Population Median Proportion of Hospital Stay Pre-Fall \nat Mobility Levels by Fall Status", x="", y="Median Proportion")

#Aggregate Prop
ggplot(bp, aes(x=fallLevel, y=AggProp,fill=mobLevels)) + geom_bar(position='stack', stat='identity', color="black") + 
  scale_fill_manual(values = c('green', 'yellow', 'red')) + theme(legend.title=element_blank()) +
  labs(title = "Population Aggregate Proportion of Hospital Stay Pre-Fall \nat Mobility Levels by Fall Status", x="", y="Aggregate Proportion")
