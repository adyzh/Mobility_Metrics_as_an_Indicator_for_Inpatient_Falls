###### New Matching Technique Code ######

# Install and load in packages
install.packages('MatchIt')
library(tidyverse)
library(lubridate)
library(MatchIt)

# Load in Dataset
## removed

### Data Prep ###
# Remove missing scores
wfirstamp <- subset(finalLongFalls_JHFRAT_MOBCSVsubset, !is.na(IMP.AMPAC_MOB_TSCORE.ALL))
# Subset by LOS of [7, 21) days
wfirstamp <- subset(wfirstamp, LOScalc >= 7 & LOScalc < 22)

# Add value for first AM-PAC score and lowest AM-PAC score in first 48 hours per patient
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% 
  mutate(first_amp = first(IMP.AMPAC_MOB_TSCORE.ALL))

wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% 
  mutate(low_amp48 = min(first(na.omit(IMP.AMPAC_MOB_TSCORE.ALL)), 
                         nth(na.omit(IMP.AMPAC_MOB_TSCORE.ALL), 2)))


# Truncate admit date value
wfirstamp$ADMIT_DATE.truncated <- format(as.POSIXct(wfirstamp$ADMIT_DATE,format='%m/%d/%Y %H:%M'),
                                         format='%m/%d/%Y')

# Calculate days since admission per patient
wfirstamp$Days_since_admission <- 
  as.numeric(difftime(mdy(wfirstamp$RECORDED_TIME.day), 
                      as.Date(mdy(wfirstamp$ADMIT_DATE.truncated, tz = "US/Eastern")), units = 'day'))

# Convert surgery to binary variable
wfirstamp$Surgery_binary <- ifelse(wfirstamp$ADMFORSURGERY=='Yes',1, 0)

# Calculate normalized days per patient
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% 
  mutate(normalized_days = (Days_since_admission-min(Days_since_admission))/(max(Days_since_admission)-min(Days_since_admission)))

# Convert JHFRAT risk category to numeric variable
wfirstamp$FallRiskNum <- recode(wfirstamp$FallRiskCat, "Low Risk" = 1, "Moderate Risk" = 2, "High Risk" = 3)

# Get highest JH-FRAT score in 48 hrs per patient
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% 
  mutate(JHFRAT_48max = max(first(na.omit(FallRiskNum)), nth(na.omit(FallRiskNum), 2)))

## Bin comorbidities by quintiles of population
# Get one instance of each patient
by.ID <- wfirstamp[match(unique(wfirstamp$PAT_ENC_CSN_ID), wfirstamp$PAT_ENC_CSN_ID),]
quantile(by.ID$JHHS_Comorbidity_Count, probs = seq(0, 1, 1/5))
# Values are currently hardcoded but could be updated to use code from quintiles
wfirstamp$Comorb.BIN <- ifelse(wfirstamp$JHHS_Comorbidity_Count <= 2, 1, 
                               ifelse(wfirstamp$JHHS_Comorbidity_Count > 2 & wfirstamp$JHHS_Comorbidity_Count <= 4, 2, 
                                      ifelse(wfirstamp$JHHS_Comorbidity_Count > 4 & wfirstamp$JHHS_Comorbidity_Count <= 5, 3,
                                             ifelse(wfirstamp$JHHS_Comorbidity_Count > 5 & wfirstamp$JHHS_Comorbidity_Count <= 6, 4,
                                                    ifelse(wfirstamp$JHHS_Comorbidity_Count > 6 & wfirstamp$JHHS_Comorbidity_Count <= 14, 5, -1)))))

# Calculate number of ICU days per patient
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% mutate(ICUdays = sum(UNIT.ICU))
wfirstamp$ICU.bool <- ifelse(wfirstamp$ICUdays > 0, 1, 0)

# Calculate number of usable scores per patient
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% mutate(num_scores = as.double(n()))

# Calculate days to fall for injurious fall patients
wfirstamp$DaysBeforeFall <- 
  as.numeric(as.Date(wfirstamp$FALL_DAY.date, format = '%m/%d/%Y') - as.Date(wfirstamp$DATE, format = '%m/%d/%Y'))

wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% mutate(DaysToFall = max(DaysBeforeFall))


# Calculate highest JH-FRAT score in first 48 hours per patient
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% mutate(JHFRAT_48max = max(first(na.omit(JHFRATTotal)), nth(na.omit(JHFRATTotal), 2)))

# Calculate min,mean,med,max AM-PAC and JH-HLM for each patient
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% 
  mutate(max_amp = max(IMP.AMPAC_MOB_TSCORE.ALL)) %>% 
  mutate(min_amp = min(IMP.AMPAC_MOB_TSCORE.ALL)) %>% 
  mutate(avg_amp = mean(IMP.AMPAC_MOB_TSCORE.ALL, na.rm = TRUE)) %>% 
  mutate(med_amp = median(IMP.AMPAC_MOB_TSCORE.ALL, na.rm = TRUE))

wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% 
  mutate(max_hlm = max(IMP.JHHLM.max)) %>% 
  mutate(min_hlm = min(IMP.JHHLM.max)) %>% 
  mutate(avg_hlm = mean(IMP.JHHLM.max, na.rm = TRUE)) %>% 
  mutate(med_hlm = median(IMP.JHHLM.max, na.rm = TRUE))

# Impute all JH-FRAT categories and total by LOCF
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% fill(JHFRATTotal) %>% 
  fill(JHFRATTotal, .direction = "up") %>%fill(JHFRATAge) %>% fill(JHFRATAge, .direction = "up") %>% 
  fill(JHFRATFallHx) %>% fill(JHFRATFallHx, .direction = "up") %>% fill(JHFRATEquip) %>% 
  fill(JHFRATEquip, .direction = "up") %>% fill(JHFRATBowel) %>% 
  fill(JHFRATBowel, .direction = "up") %>% fill(JHFRATCog) %>% fill(JHFRATCog, .direction = "up") %>%
  fill(JHFRATMeds) %>% fill(JHFRATMeds, .direction = "up") %>% fill(JHFRATMobility) %>% 
  fill(JHFRATMobility, .direction = "up")

# Remove any patients who never received a JHFRAT
wfirstamp <- subset(wfirstamp, !is.na(JHFRATTotal))

# Calculate JH-FRAT without mobility component
wfirstamp$newJHFRAT <- wfirstamp$JHFRATTotal - wfirstamp$JHFRATMobility

# Highest JH-FRAT wihtout mobility in first 48 hours
wfirstamp <- wfirstamp %>% group_by(PAT_ENC_CSN_ID) %>% 
  mutate(newJHFRAT_max48 = max(first(na.omit(newJHFRAT)), nth(na.omit(newJHFRAT), 2)))

wfirstamp <- subset(wfirstamp, !is.na(newJHFRAT_max48))


# Bin this value by quintiles based on distribution of population
by.ID <- wfirstamp[match(unique(wfirstamp$PAT_ENC_CSN_ID), wfirstamp$PAT_ENC_CSN_ID),]
quantile(by.ID$newJHFRAT_max48, probs = seq(0, 1, 1/5))
# Values are currently hardcoded but could be updated to use code from quintiles
wfirstamp$newJHFRAT.BIN <- ifelse(wfirstamp$newJHFRAT_max48 <= 5, 1, 
                                  ifelse(wfirstamp$newJHFRAT_max48 > 5 & wfirstamp$newJHFRAT_max48 <= 7, 2, 
                                         ifelse(wfirstamp$newJHFRAT_max48 > 7 & wfirstamp$newJHFRAT_max48 <= 9, 3,
                                                ifelse(wfirstamp$newJHFRAT_max48 > 9 & wfirstamp$newJHFRAT_max48 <= 11, 4,
                                                       ifelse(wfirstamp$newJHFRAT_max48 > 11 & wfirstamp$newJHFRAT_max48 <= 27, 5, -1)))))

### MATCHING:
# Separate Non-Fallers
nonfall <- subset(wfirstamp, FallInjuryYN == 0)

## Get instance of day before fall for fallers
# Subset to get all days of fallers with day of fall removed 
fallers <- subset(wfirstamp, FallInjuryYN == 1 & DayOfFALL.bool == 0)

# Calculate which day since fall
fallers <- fallers %>% group_by(PAT_ENC_CSN_ID) %>% 
  mutate(DaysSinceFall = as.numeric(as.Date(as.character(DATE), format="%m/%d/%Y") - as.Date(as.character(FALL_DAY1), format="%m/%d/%Y")))

# Remove days following the fall
fallers <- subset(fallers, DaysSinceFall < 0)

# Calculate number of days total prior to fall per patient
fallers <- fallers %>% group_by(PAT_ENC_CSN_ID) %>% mutate(DaysPreFall = n())

# Select instance of day before fall, remove any injurious fall patients 
# that only have 1 score prior to fall
fallers <- subset(fallers, DaysSinceFall == -1 & DaysPreFall > 1)

# Combine one instance day before fall of injurious fallers and all instances of non-fallers
matches <- rbind(fallers, nonfall)

# Match control (non-fall) to treatment (injurious fall)
m.out <- matchit(FallInjuryYN ~ Days_since_admission + low_amp48 + Surgery_binary + 
                   newJHFRAT.BIN + Comorb.BIN, data = matches, method = "exact", estimand = "ATE") 

# display match results
summary(m.out, un=FALSE)

# Extract matches
Matches <- match.data(m.out)

# Extract full data from matches
tmp <- subset(wfirstamp, wfirstamp$PAT_ENC_CSN_ID %in% Matches$PAT_ENC_CSN_ID)
tmp$control <- Matches$subclass[match(tmp$PAT_ENC_CSN_ID, Matches$PAT_ENC_CSN_ID)]

# Verification: View matched patients with two or more matches
# tmp2 <- Matches %>% group_by(PAT_ENC_CSN_ID) %>% filter(n() > 1)
# tmp2 <- subset(tmp2, select = c('PAT_ENC_CSN_ID', 'Days_since_admission', 'subclass'))

# Randomly select non-fallers who are matched more than once and remove all but one instance
set.seed(1216)
Matches <- Matches %>% group_by(PAT_ENC_CSN_ID) %>% slice_sample(n=1)

# Truncate the day by matched date
tmp$MatchDay <- Matches$Days_since_admission[match(tmp$PAT_ENC_CSN_ID, Matches$PAT_ENC_CSN_ID)]
# Remove controls with more days than their matched case
tmp <- subset(tmp, Days_since_admission <= MatchDay)

# Get one instance of each patient
by.ID <- tmp[match(unique(tmp$PAT_ENC_CSN_ID), tmp$PAT_ENC_CSN_ID),]

# Should be 1779 non-fallers and 72 non-fallers
table(by.ID$FallInjuryYN)

# remove matched fallers who dont have same number of scores as their match
tmp <- tmp %>% group_by(PAT_ENC_CSN_ID) %>% mutate(NumDays = n())
by.ID <- tmp[match(unique(tmp$PAT_ENC_CSN_ID), tmp$PAT_ENC_CSN_ID),]
tmp2 <- subset(by.ID, FallInjuryYN == 1)
tmp$matchNumDays <- tmp2$NumDays[match(tmp$control, tmp2$control)]
tmp <- subset(tmp, NumDays >= matchNumDays)

# Get one instance of each patient
by.ID <- tmp[match(unique(tmp$PAT_ENC_CSN_ID), tmp$PAT_ENC_CSN_ID),]

# Should be 1273 non-fallers and 72 non-fallers
table(by.ID$FallInjuryYN)

# Separate if need be
FallNMatch <- subset(tmp, tmp$FallInjuryYN == 0)
FallYMatch <- subset(tmp, tmp$FallInjuryYN == 1)

# Export dataframe as csv
write.csv(tmp, "MatchedDF-TruncatedMatchDay.csv", row.names = FALSE)
