rm( list=ls() )			# Clear memory of R

#import data
library(stringr)
library(dplyr)

#1 read in vaccination activities between 01 May 2016 and 01 November 2019
AJAY <- read.csv()
     
AJAY$ActivityDateFrom <- as.Date(AJAY$Date.Activity.Start, format = "%m/%d/%Y")

#filter AJAY dataframe to create desired database
# - dates after "2019-11-01"
AJAYdat <- subset(AJAY, AJAY$VaccineType=="mOPV2" &
                  (AJAY$ActivityStatus=="Confirmed"|
                     AJAY$ActivityStatus=="Done") &
                    AJAY$ActivityDateFrom < as.Date("2019-11-01"),
            select = c("Admin0", "Admin1", "Admin2", "ActivityDateFrom",
                       "ActivityStatus", "VaccineType" , "CalculatedTargetPopulation"))
# remove duplicates
mOPV2 <- unique(AJAYdat)
mOPV2$CalculatedDosages <- as.numeric(mOPV2$CalculatedTargetPopulation)

#create by mOPV2 campaign by admin 1 and 0
# check for unique observations
dim(mOPV2)
moPV_byadmin1 <- mOPV2
moPV_byadmin1$SIA.Sub.Activity.Code <- ""
moPV_byadmin1 <- moPV_byadmin1 %>% 
  mutate(SIA.Sub.Activity.Code = paste("SIA-code-", 
                                       group_indices(moPV_byadmin1, Admin0, ActivityDateFrom, CalculatedDosages),
                                       sep = ""))
# remove admn 1 and 2 levels
mOPV_byadmin0 <- unique(subset(moPV_byadmin1, select = c("Admin0", "ActivityDateFrom", "CalculatedDosages", "SIA.Sub.Activity.Code")))

#see dimensions of 1 and 2
# check this all makes sense
dim(moPV_byadmin1)
dim(mOPV_byadmin0)

#2 load VDPV2 events between 01 May 2016 and 01 November 2019

#2A Load reported VDPV2 AFP cases - from POLIS
dat2 <- read.csv()
VDPV_case <- subset(dat2, select = c("AnonymousEPID", "Place.Admin.0",
                                    "Place.Admin.1", "Place.Admin.2", 
                                    "Paralysis.Onset.Date",
                                    "ClassificationVdpv", "SurveillanceTypeName",
                                    "CalculatedAgeInMonth",
                                    "Doses.Total"))
VDPV_case$Paralysis.Onset.Date <- as.Date(VDPV_case$Paralysis.Onset.Date, format = "%d/%m/%Y")

names <- c("Specimen.EPID", "Admin0OfficialName",
           "Admin1OfficialName",
           "Admin2OfficialName" ,
           "date",
           "VDPVclassification",
           "surveillancetypename", 
           "CalculatedAgeInMonth",
           "Doses.Total")
names(VDPV_case) <- names


#2B Load all VDPV2 isolates - WHO data including lineages
dat <- read.csv()
VDPV_ajay <- subset(dat, dat$VDPV2==TRUE)
VDPV_ajay$date <- as.Date(VDPV_ajay$date, format = "%Y/%m/%d")
VDPV_ajay$Admin0OfficialName <- toupper(VDPV_ajay$Admin0OfficialName)
table(VDPV_ajay$VDPV2, useNA = "a")

########match AFP cases with isolates (to add in the age and number of doses for cases)
# corresponding age and nnumber of OPV doses
library(plyr)
joint <- as.data.frame(matrix(nrow = nrow(VDPV_ajay), ncol = ncol(VDPV_ajay)+4))
colnames(joint) <- c(colnames(VDPV_ajay), colnames(VDPV_case[8:9]), "country_2", "surveillancetypename2")
joint[,c(1:ncol(VDPV_ajay))] <- VDPV_ajay
joint$surveillancetypename2 <- "a"
joint$country_2 <- "a"

###add in age and doses of AFP cases
VDPV_case2 <- VDPV_case
for (i in 1:nrow(joint)) {
  p <- match_df(VDPV_case2, VDPV_ajay[i,], on =  c("date","Admin0OfficialName", "Admin1OfficialName"))
  if(nrow(p)==1) {
  joint$surveillancetypename2[i] <- as.character(p$surveillancetypename)
  joint$CalculatedAgeInMonth[i] <- p$CalculatedAgeInMonth
  joint$Doses.Total[i] <- p$Doses.Total
  joint$country_2[i] <- as.character(p$Admin0OfficialName)
  VDPV_case2 <- VDPV_case2[-which(rownames(VDPV_case2)==rownames(p)),]
  }
  else if(nrow(p)>1) {
    p <- p[1,]
    joint$surveillancetypename2[i] <- as.character(p$surveillancetypename)
    joint$CalculatedAgeInMonth[i] <- p$CalculatedAgeInMonth
    joint$Doses.Total[i] <- p$Doses.Total
    joint$country_2[i] <- as.character(p$Admin0OfficialName)
    VDPV_case2 <- VDPV_case2[-which(rownames(VDPV_case2)==rownames(p)),]
} }


#** create VDPV events dataset, out of joint dataframe (VDPV isolates and cases),
# ** removing immune deficient cases
VDPV_events <- subset(joint, ((VDPVclassification=="Immune Deficient")==FALSE),
                      select = c("Admin0OfficialName",
                                 "Admin1OfficialName",
                                 "Admin2OfficialName",
                                 "surveillancetypename",
                                 "date",
                                 "VDPVclassification",        
                                 "VdpvNtChanges",
                                 "SampleId",   
                                 "EmergenceGroup",
                                 "Year",
                                 "CalculatedAgeInMonth",
                                 "Doses.Total"))


#####set years since the switch
VDPV_events$years_since_switch[VDPV_events$date>="2016-05-01" & VDPV_events$date < "2017-05-01"] <- 1
VDPV_events$years_since_switch[VDPV_events$date>="2017-05-01" & VDPV_events$date < "2018-05-01"] <- 2
VDPV_events$years_since_switch[VDPV_events$date>="2018-05-01" & VDPV_events$date < "2019-05-01"] <- 3
VDPV_events$years_since_switch[VDPV_events$date>="2019-05-01" & VDPV_events$date < "2019-12-02"] <- 4

#tidy global enviroment, remove un-needed 
mOPV_by_admin1 <- moPV_byadmin1
j <- c("mOPV_byadmin0", "mOPV_by_admin1", "VDPV_events", "SL2", "path_data","path_output", "path_work")
rm(list= ls()[!(ls() %in% j)])


#Calculate estimated seeding date
#Set mutation rate (1.14*10^-2/365.25) * number of nucleotides in VP1 genome (365.25 days in the year)
Nick_rate <- (1.14*10^-2*906)/365.25

#Estimate seeding date for each isolate
VDPV_events$emergence_date1 <- rep(as.Date("0000-01-01"), nrow(VDPV_events))
VDPV_events$emergence_date_max1 <- rep(as.Date("0000-01-01"), nrow(VDPV_events))
VDPV_events$emergence_date_min1 <- rep(as.Date("0000-01-01"), nrow(VDPV_events))
VDPV_events$probability_after_switch <- rep(0)

#say first 1 changes happen instantaneously, then calculate based on n-1 mutations and mutation rate 
VDPV_events
for (i in 1:nrow(VDPV_events)) {
  n <- VDPV_events$VdpvNtChanges[i]
  if(is.na(n)==FALSE) {
    k <- as.Date(VDPV_events$date[i] - qgamma(0.5, (n-1), Nick_rate)). # date of detection - estimated age of virus (days) (median)
    VDPV_events$emergence_date1[i] <- k
    
    k2 <- as.Date(VDPV_events$date[i] - qgamma(0.025, (n-1), Nick_rate)) # lower
    VDPV_events$emergence_date_max1[i] <- k2
    
    k3 <- as.Date(VDPV_events$date[i] - qgamma(0.975, (n-1), Nick_rate)) # upper
    VDPV_events$emergence_date_min1[i] <- k3
    
    l <- as.numeric(VDPV_events$date[i] - as.Date("2016-05-01")). # diff time between detection and switch
    probs <- pgamma(q = l, (n-1), Nick_rate).  # proportion that is <= detection and switch = prob virus seeded before switch for each isolate
    VDPV_events$probability_after_switch[i] <- probs
  }  
  else {
    VDPV_events$emergence_date1[i] <- NA
    VDPV_events$emergence_date_min1[i] <- NA
    VDPV_events$emergence_date_max1[i] <- NA
    VDPV_events$probability_after_switch[i] <- NA }
  
}

####Estimate  seeding date for emergence clusters
#create group seeding date variable
VDPV_events$emergence_date_group <- VDPV_events$emergence_date1
VDPV_events$emergence_date_max_group <- VDPV_events$emergence_date_max1
VDPV_events$emergence_date_min_group <- VDPV_events$emergence_date_min1
VDPV_events$probability_after_switch_group <- VDPV_events$probability_after_switch

#frequency of emergence clusters
library(plyr)
y <- count(VDPV_events, 'EmergenceGroup') # more than one isolate
cluster <- y$EmergenceGroup[y$freq>1]
cluster <- cluster[-which(is.na(cluster))] # part of group and have estimated emergence date (should be all)

#estimate seeding date from resampling up to the first **3** detected isolates (vary part of sensitivity analysis)
for (j in 1:length(cluster)) {
  group <- cluster[j]
  virus_w_dates <- which(VDPV_events$EmergenceGroup == group & (is.na(VDPV_events$emergence_date1)==FALSE))
  virus_all <- which(VDPV_events$EmergenceGroup == group)
  #how many virus_w-dates do we select from
  if(length(virus_w_dates)  >= 3) {
    number <- 3
  } else if (length(virus_w_dates)  < 3) {
    number <- length(virus_w_dates) 
  } 
  k <- rep(as.Date("1000-01-01"), 10000)
  for (i in 1:10000) { # sample!
    virus_w_dates_order <- virus_w_dates[order(VDPV_events$date[virus_w_dates], decreasing = FALSE)]
    random <- sample(1:number, 1)
    n <- VDPV_events$VdpvNtChanges[virus_w_dates_order[random]]
    k[i] <- as.Date(VDPV_events$date[virus_w_dates_order[random]] - rgamma(1, 
                                                                           (n-1), 
                                                                           Nick_rate))  # chose random number 
  }
  p <- as.Date(k) # big vector of 10,000 dates
  VDPV_events$emergence_date_max_group[virus_all] <- quantile(p, probs = 0.975, type = 1)
  VDPV_events$emergence_date_min_group[virus_all] <- quantile(p, probs = 0.025, type = 1)
  VDPV_events$emergence_date_group[virus_all] <- quantile(p, probs = 0.5, type = 1) # median
  mydis <- ecdf(p) # use empirical dist function 
  prob = 1-mydis(16922)
  #16922 is date of switch (with orgin "1970-01-01"), probability is CDF that seeding was after switch    
  VDPV_events$probability_after_switch_group[virus_all] <- prob 
}

####### Estimate outbreak source (compare to mOPV2 campaigns)
####chose assumption

library(lubridate)
VDPV_events$mOPV <-  NA #= mOPV2 in admin 1 region during interval  
VDPV_events$mOPV_correspond <- rep(0, nrow(VDPV_events))
VDPV_events$mOPV_date <- as.Date(NA)
VDPV_events$number_mOPV2_correspond <- 0
VDPV_events$diff_time <- NA

for (i in 1:nrow(VDPV_events)) {
  admin1 <-VDPV_events$Admin1OfficialName[i] # check admn1
  admin0 <-VDPV_events$Admin0OfficialName[i]# check admn0
  region <-VDPV_events$Region[i] # check admn1
  
  #time dif between date of detection and maximum estimated seeding date
  maxseed_to_detect <- difftime(ymd(VDPV_events$date[i]), ymd(VDPV_events$emergence_date_max_group[i])) 
  z <- maxseed_to_detect > 0
  if (TRUE %in% z) { 
    #if detection date is after seeding interval, int is the 95% CI for estimated seeding 
  int <- interval(ymd(VDPV_events$emergence_date_min_group[i]), ymd(VDPV_events$emergence_date_max_group[i])) 
  } else {
    #if detection date is during seeding interval, int is the minumum seeding date to date of detection 
    #(as emergence cannot of been seeded after it was detected)
  int <- interval(ymd(VDPV_events$emergence_date_min_group[i]), ymd(VDPV_events$date[i])) 
  }
  #Q1 : were there campaigns in that admin1 in interval?
  question1 <- mOPV_by_admin1$Admin1 == as.character(admin1) & ymd(mOPV_by_admin1$ActivityDateFrom) %within% int
  #Q2 : were there campaigns in that admin0 in interval?
  question2 <- mOPV_by_admin1$Admin0 == as.character(admin0) & ymd(mOPV_by_admin1$ActivityDateFrom) %within% int

          #if Q1 = yes  
          if (TRUE %in% question1)   {
          selected <- which(question1==TRUE)
          VDPV_events$mOPV[i] <- "In same admin 1"
          #how many campaigns were in interval in region?
          VDPV_events$number_mOPV2_correspond[i] <- length(selected)
          
          #find closest one (to mean seeding date)
          time_diff <- abs(difftime(mOPV_by_admin1$ActivityDateFrom[selected], VDPV_events$emergence_date_group[i]))
          camp <- which(time_diff == min(time_diff)) #closest to mean seeding date but before detection date
          select <- selected[camp[1]]
          #assign this campaign to vaccine virus  
          VDPV_events$mOPV_date[i] <- as.Date(mOPV_by_admin1$ActivityDateFrom[select])
          VDPV_events$mOPV_correspond[i] <- as.character(mOPV_by_admin1$SIA.Sub.Activity.Code[select])
          VDPV_events$diff_time[i] <- difftime(VDPV_events$mOPV_date[i], VDPV_events$emergence_date_group[i]) 
  
          
          #else if Q2 = yes 
  } else if (TRUE %in% question2) {   
    selected <- which(question2==TRUE)
    VDPV_events$mOPV[i] <- "In same admin 0"
    VDPV_events$number_mOPV2_correspond[i] <- length(selected)
    #how many campaigns were in interval in region?
    #find closest one (to mean seeding date)
    time_diff <- abs(difftime(mOPV_by_admin1$ActivityDateFrom[selected], VDPV_events$emergence_date_group[i]))
    camp <- which(time_diff == min(time_diff)) #closest to mean seeding date but before detection
    select <- selected[camp[1]]
    #assign this campaign to vaccine virus  
    VDPV_events$mOPV_correspond[i] <- as.character(mOPV_by_admin1$SIA.Sub.Activity.Code[select])
    VDPV_events$mOPV_date[i] <- as.Date(mOPV_by_admin1$ActivityDateFrom[select])
    VDPV_events$diff_time[i] <- difftime(VDPV_events$mOPV_date[i], VDPV_events$emergence_date_group[i]) 
    } 
   }

#

#Make sure there is a single source for all isolates in the same emegernce cluster
for (j in 1:length(unique(VDPV_events$EmergenceGroup[!is.na(VDPV_events$EmergenceGroup==TRUE)]))) {
  group <- unique(VDPV_events$EmergenceGroup[!is.na(VDPV_events$EmergenceGroup==TRUE)])[j]
  virus <- which(VDPV_events$EmergenceGroup == group)
  order_by_nt <- virus[order(VDPV_events$date[virus])]
  first <- order_by_nt[1]
  VDPV_events$mOPV_date[virus] <- unique((VDPV_events$mOPV_date[first]))
  VDPV_events$mOPV_correspond[virus] <- unique((VDPV_events$mOPV_correspond[first]))
  VDPV_events$number_mOPV2_correspond[virus] <- unique((VDPV_events$number_mOPV2_correspond[first]))
  VDPV_events$mOPV[virus] <-  unique((VDPV_events$mOPV[first]))
}


VDPV_events$diff_time <- difftime(VDPV_events$mOPV_date, VDPV_events$emergence_date_group, units = "days")
#####################################################################
#match VDPVs to mOPVs

####review VDPVs with multiple mOPVs


###make second mOPV correspondtab ignoring those likely pre-switch (*Not used in analysis*)
VDPV_events$mOPV_correspond2 <- VDPV_events$mOPV_correspond
for (i in 1:nrow(VDPV_events)) {
  if(!is.na(VDPV_events$probability_after_switch_group[i]) & VDPV_events$probability_after_switch_group[i]<0.5) {
    VDPV_events$mOPV_correspond2[i] <- 0
  }
  }

table(VDPV_events$number_mOPV2_correspond, VDPV_events$mOPV)
j <- which(VDPV_events$number_mOPV2_correspond>0 & VDPV_events$mOPV == "In same admin 0")
VDPV_events
unique(VDPV_events$EmergenceGroup)


###Look for mOPV2 in neighbouring countries for OBR with probability post switch > 0.9 and no mOPV2 campaign in country of emergence. 
p <- which(VDPV_events$probability_after_switch_group>0.9 & VDPV_events$mOPV_correspond2 == 0)
VDPV_events[p,]
#identified Angola, CAR and Pakistan
#No mOPV2 use in countries bordering Pakistan

#1 ANGOLA
p <- which(VDPV_events$probability_after_switch_group>0.9 & 
             VDPV_events$mOPV_correspond2 == 0 &
             VDPV_events$Admin0OfficialName=="ANGOLA")
  for(l in 1:length(p)) {
  i <- p[l]
  admin0 <-VDPV_events$Admin0OfficialName[i]
  maxseed_to_detect <- difftime(ymd(VDPV_events$date[i]), ymd(VDPV_events$emergence_date_max_group[i])) #time dif between date of detection and maximum seeding date
  z <- maxseed_to_detect > 0
  if (TRUE %in% z) { 
    #detection date after seeding interval
    int <- interval(ymd(VDPV_events$emergence_date_min_group[i]), ymd(VDPV_events$emergence_date_max_group[i])) 
  } else {
    int <- interval(ymd(VDPV_events$emergence_date_min_group[i]), ymd(VDPV_events$date[i])) 
  }
  #Q1 : were there campaigns in that admin1 in interval?
  question1 <- ymd(mOPV_by_admin1$ActivityDateFrom) %within% int
  selected <- which(question1==TRUE)
  VDPV_events$mOPV[i] <- "In another country"
  VDPV_events$number_mOPV2_correspond[i] <- length(selected)
  
  #how many campaigns were in interval in neighbouring?
  question2 <- ymd(mOPV_by_admin1$ActivityDateFrom) %within% int & mOPV_by_admin1$Admin0=="DEMOCRATIC REPUBLIC OF THE CONGO"
  selected <- which(question2==TRUE)
  VDPV_events$mOPV[i] <- "In neighbouring country"
  VDPV_events$number_mOPV2_correspond[i] <- length(selected)
  
  #find closest one (to mean seeding date)
  time_diff <- abs(difftime(mOPV_by_admin1$ActivityDateFrom[selected], VDPV_events$emergence_date_group[i]))
  camp <- which(time_diff == min(time_diff)) #closest to mean seeding date but before detection date
  select <- selected[camp[1]]
  #assign this campaign to vaccine virus  
  VDPV_events$mOPV_date[i] <- as.Date(mOPV_by_admin1$ActivityDateFrom[select])
  VDPV_events$mOPV_correspond[i] <- as.character(mOPV_by_admin1$SIA.Sub.Activity.Code[select])
  VDPV_events$mOPV_correspond2[i] <- as.character(mOPV_by_admin1$SIA.Sub.Activity.Code[select])
  VDPV_events$diff_time[i] <- difftime(VDPV_events$mOPV_date[i], VDPV_events$emergence_date_group[i]) 
  }
    

#2 CENTRAL AFRICAN REPUBLIC
p <- which(VDPV_events$probability_after_switch_group>0.9 & 
             VDPV_events$mOPV_correspond2 == 0 &
             VDPV_events$Admin0OfficialName=="CENTRAL AFRICAN REPUBLIC")
for(l in 1:length(p)) {
  i <- p[l]
  admin0 <-VDPV_events$Admin0OfficialName[i]
  maxseed_to_detect <- difftime(ymd(VDPV_events$date[i]), ymd(VDPV_events$emergence_date_max_group[i])) #time dif between date of detection and maximum seeding date
  z <- maxseed_to_detect > 0
  if (TRUE %in% z) { 
    #detection date after seeding interval
    int <- interval(ymd(VDPV_events$emergence_date_min_group[i]), ymd(VDPV_events$emergence_date_max_group[i])) 
  } else {
    int <- interval(ymd(VDPV_events$emergence_date_min_group[i]), ymd(VDPV_events$date[i])) 
  }
  #Q1 : were there campaigns in that admin1 in interval?
  question1 <- ymd(mOPV_by_admin1$ActivityDateFrom) %within% int
  selected <- which(question1==TRUE)
  VDPV_events$mOPV[i] <- "In another country"
  VDPV_events$number_mOPV2_correspond[i] <- length(selected)
  
  #how many campaigns were in interval in neighbouring?
  question2 <- ymd(mOPV_by_admin1$ActivityDateFrom) %within% int & 
    (mOPV_by_admin1$Admin0=="DEMOCRATIC REPUBLIC OF THE CONGO"|
       mOPV_by_admin1$Admin0=="CAMEROON")
  selected <- which(question2==TRUE)
  VDPV_events$mOPV[i] <- "In neighbouring country"
  VDPV_events$number_mOPV2_correspond[i] <- length(selected)
  
  #find closest one (to mean seeding date)
  time_diff <- abs(difftime(mOPV_by_admin1$ActivityDateFrom[selected], VDPV_events$emergence_date_group[i]))
  camp <- which(time_diff == min(time_diff)) #closest to mean seeding date but before detection date
  select <- selected[camp[1]]
  #assign this campaign to vaccine virus  
  VDPV_events$mOPV_date[i] <- as.Date(mOPV_by_admin1$ActivityDateFrom[select])
  VDPV_events$mOPV_correspond[i] <- as.character(mOPV_by_admin1$SIA.Sub.Activity.Code[select])
  VDPV_events$mOPV_correspond2[i] <- as.character(mOPV_by_admin1$SIA.Sub.Activity.Code[select])
  VDPV_events$diff_time[i] <- difftime(VDPV_events$mOPV_date[i], VDPV_events$emergence_date_group[i]) 
}

#Put back all likely seeded VDPV2s into mOPV2 database
mOPV_detection <- mOPV_by_admin1
mOPV_detection$number_VDPV <- 0
mOPV_detection$first_VDPV <- as.Date("01-01-2000")
mOPV_detection$code_VDPV <- "a"
mOPV_detection$number_cVDPV <- 0
mOPV_detection$number_aVDPV <- 0
mOPV_detection$number_pVDPV <- 0

mOPV_detection$number_aVDPV_adm0 <- 0
mOPV_detection$number_aVDPV_adm1 <- 0
mOPV_detection$number_cVDPV_adm0 <- 0
mOPV_detection$number_cVDPV_adm1 <- 0

mOPV_detection$number_pVDPV_adm0 <- 0
mOPV_detection$number_pVDPV_adm1 <- 0
mOPV_detection$number_aVDPV_outside_country <- 0

mOPV_detection$number_pVDPV_outside_country <- 0
mOPV_detection$number_cVDPV_outside_country <- 0

for (i in 1:nrow(mOPV_detection)) {
  if (mOPV_by_admin1$SIA.Sub.Activity.Code[i] %in% unique(as.factor(VDPV_events$mOPV_correspond2))) {
    j <- which(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i])
    a <- sum(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] & 
               VDPV_events$VDPVclassification == "Ambiguous")
    cnum <- sum(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] &
                  VDPV_events$VDPVclassification == "Circulating")
    ccode <- which(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] &
                   VDPV_events$VDPVclassification == "Circulating")
    c <- length(unique(VDPV_events$EmergenceGroup[ccode]))
    mOPV_detection$number_aVDPV_adm1[i] <- sum(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] & 
                                                 VDPV_events$VDPVclassification == "Ambiguous" &
                                             VDPV_events$mOPV=="In same admin 1")
    mOPV_detection$number_aVDPV_adm0[i] <- sum(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] & 
                                                 VDPV_events$VDPVclassification == "Ambiguous"&
                                              VDPV_events$mOPV=="In same admin 0")
    mOPV_detection$number_aVDPV[i] <- a
    mOPV_detection$number_pVDPV[i] <- sum(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] & 
                                                 VDPV_events$VDPVclassification == "Pending"&
                                                 is.na(VDPV_events$EmergenceGroup))
    mOPV_detection$number_aVDPV_outside_country[i] <- sum(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] & 
                                                            VDPV_events$VDPVclassification == "Ambiguous"&
                                                            VDPV_events$mOPV=="In neighbouring country")
    mOPV_detection$number_pVDPV_adm1[i] <- sum(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] & 
                                                 VDPV_events$VDPVclassification == "Pending"&
                                                 is.na(VDPV_events$EmergenceGroup) &
                                                 VDPV_events$mOPV=="In same admin 1")
    mOPV_detection$number_pVDPV_adm0[i] <- sum(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] & 
                                                 VDPV_events$VDPVclassification == "Pending"&
                                                 is.na(VDPV_events$EmergenceGroup) &
                                                 VDPV_events$mOPV=="In same admin 0")
    mOPV_detection$number_pVDPV_outside_country[i] <- sum(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] & 
                                                            VDPV_events$VDPVclassification == "Pending"&
                                                            is.na(VDPV_events$EmergenceGroup) &
                                                            VDPV_events$mOPV=="In neighbouring country")
    
    
    if (cnum > 0) {
      mOPV_detection$number_cVDPV[i] <- c
      mOPV_detection$number_cVDPV_outside_country[i] <- length(unique(VDPV_events$EmergenceGroup[which(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] & 
                                                              VDPV_events$VDPVclassification == "Circulating" &
                                                              VDPV_events$mOPV=="In neighbouring country")]))
      
      mOPV_detection$number_cVDPV_adm1[i] <- length(unique(VDPV_events$EmergenceGroup[which(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] & 
                                                   VDPV_events$VDPVclassification == "Circulating" &
                                                   VDPV_events$mOPV=="In same admin 1")]))
      mOPV_detection$number_cVDPV_adm0[i] <- length(unique(VDPV_events$EmergenceGroup[which(VDPV_events$mOPV_correspond2 == mOPV_by_admin1$SIA.Sub.Activity.Code[i] & 
                                                   VDPV_events$VDPVclassification == "Circulating" &
                                                   VDPV_events$mOPV=="In same admin 0")]))
    }
    mOPV_detection$number_VDPV[i] <- length(j)
    mOPV_detection$first_VDPV[i] <- min(as.Date(VDPV_events$date[j], format = "%d/%m/%Y"))
    if (FALSE %in% (is.na(VDPV_events$EmergenceGroup[j]))) {
    p <- as.character(VDPV_events$EmergenceGroup[j][(which(!is.na(VDPV_events$EmergenceGroup[j])))])
    mOPV_detection$code_VDPV[i] <- p[1]
    } else {
      mOPV_detection$code_VDPV[i] <- NA
  }
    } else {
    mOPV_detection$number_VDPV[i] <- 0
    mOPV_detection$first_VDPV[i] <- NA
    mOPV_detection$code_VDPV[i] <- NA
    }
}
mOPV_detection$time_first_VDPV <- as.numeric(difftime(mOPV_detection$first_VDPV, 
                                                     mOPV_detection$ActivityDateFrom, units = "days"))
mOPV_detection$VDPV_binary[mOPV_detection$number_VDPV>0] <- 1
mOPV_detection$VDPV_binary[mOPV_detection$number_VDPV==0] <- 0

#put  into admin 0
mOPV_byadmin0 <- unique(subset(mOPV_detection, select = c(names(mOPV_detection))[c(1, 4:ncol(mOPV_detection))]))


#save files