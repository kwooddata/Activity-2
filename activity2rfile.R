##beginning of classwork activity 2

install.packages(c("dplyr", "lubridate"))

library(dplyr)
library(lubridate)

streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
#####warn people if yards are going to flood
##see how hurricane irma drove flooding, and how it differs in different watersheds

siteInfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

##parse our date
streamH$dateF <- ymd_hm(streamH$datetime, tz="America/New_York")

year(streamH$dateF)
###stream functions dplyr

###floods
###join site info to stream gauge height
floods <- full_join(streamH, siteInfo, by="siteID")

#dplyr chain functions together to do multiple functions/operations at once
###better formatting
peace <- floods %>%
  filter(siteID == 2295637)
example <- floods %>%
  filter (gheight.ft >= 10)

plot(peace$dateF, peace$gheight.ft, type="l")

max_ht <- floods %>%
  group_by(names) %>%
  summarize(max_ht_ft = max(gheight.ft, na.rm=TRUE), 
            mean_height = mean(gheight.ft, na.rm=TRUE))
#####
#plot(peace$dateF,peace)
#plot(floods)

###3 in activity

flood_date <-floods %>% 
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

####end of classwork

###start of group homework 2

####Group 3

peace <- floods %>% filter(siteID == 2295637)

hist(peace$gheight.ft)

hist(peace$gheight.ft, 
     main = "Peace River Stream Stage Frequency From September 8-30, 2017",
     xlab = "Stream Stage (ft)",
     col = 'blue')

FishEating_creek <- floods %>% filter(siteID == 2256500)

hist(FishEating_creek$gheight.ft, 
     main = "FishEating Creek Stream Stage Frequency From September 8-30, 2017",
     xlab = "Stream Stage (ft)",
     col = 'yellow')

Santa_Fe <- floods %>% filter(siteID == 2322500)

hist(Santa_Fe$gheight.ft, 
     main = "Santa Fe Creek Stream Stage (ft)",
     xlab = "Stream Stage (ft)",
     col = 'blue',
     breaks = 5)

Santa_Fe <- floods %>% filter(siteID == 2322500)

hist(Santa_Fe$gheight.ft, 
     main = "Santa Fe Creek Stream Stage Frequency",
     xlab = "Stream Stage (ft)",
     ylim = c(0, 700),
     xlim = c(0,16),
     col = 'purple',
     border = "pink")
     
##demo using floods df

##simple_floods <- floods %>%
 ## select(-c('agency', 'siteID', 'datetime', 'moderate.ft'', 'action.ft','major.ft''))

##mutate_floods <- simple_floods

##nrow(floods)

#which(floods$hgheight.ft>20)

##ncol(floods)
##nrow(floods)

##example <- maxht$max.ht_ft[nrow(max.ht)])

##which(max.ht >20)

##which(floods$siteID ==2312000)

##floods$region <- lfelse(floods$siteID=2)

##Sante_Fe <- floods %>% filter(siteID == 2322500)

##hist(Sante_Fe$gheight.ft,
     ##main = "xyz",
    ## xlim = c(0,16))

###START OF IN CLASS PROMPTS
#Prompt 1
#Follow the steps in the tutorial to join streamH and siteInfo into a data frame called Floods. Check if the type of join makes a difference in the outcome.
floods_fulljoin <- full_join(streamH, siteInfo, by="siteID")
floods_rightjoin <- right_join(streamH, siteInfo, by="siteID")
floods_innerjoin <- inner_join(streamH, siteInfo, by="siteID")
floods_leftjoin <- left_join(streamH, siteInfo, by="siteID")
###no difference because there are no missing values 
##all joins have 8681 observations and 10 variables

#Prompt 2
#Parse the date for the Floods data frame.
floods$dateF <- ymd_hm(streamH$datetime, tz="America/New_York")

#Prompt 3
#What was the earliest date that each river reached the flood stage?
flood_date <-floods %>% 
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

###install.packages(c("dplyr", "lubridate"))
library(dplyr)
library(lubridate)
##rerun to make sure they're ready in R

####Homework Part 2
##Question 1
##Make a separate plot of the stream stage data for each river. In 3-4 sentences compare general patterns in the stream stage between sites around Hurricane Irma.

###4 streams:

##FISHEATINGCREEK site id:2256500

##PEACERIVER site id:2295637

##SANTAFERIVER site id:2322500

##WITHLACOOCHEERIVER site id:2312000

###Hurricane Irma made landfall over Florida on September 10 in 2017, 8-30th is around Irma

#stages plots
fisheatingcreekstages <- streamH[streamH$siteID == 2256500, ]
plot(fisheatingcreekstages$dateF, fisheatingcreekstages$gheight.ft, 
     type = "b", pch = 15, 
     xlab = "Date (Sept 2017)", ylab = "Stage height (ft)", 
     main = "Fish Eating Creek Stream Stage Heights (Septe 8-30, 2017)")
##################################################################################
peaceriverstages <- streamH[streamH$siteID == 2295637, ]
plot(peaceriverstages$dateF, peaceriverstages$gheight.ft, 
     type = "b", pch = 15, 
     xlab = "Date (Sept 2017)", ylab = "Stage height (ft)", 
     main = "Peace River Stream Stage Heights (Sept 8-30, 2017)")
##################################################################################
santafestages <- streamH[streamH$siteID == 2322500, ]
plot(santafestages$dateF, santafestages$gheight.ft, 
     type = "b", pch = 15, 
     xlab = "Date (Sept 2017)", ylab = "Stage height (ft)", 
     main = "Santa Fe River Stream Stage Heights (Sept 8-30, 2017)")
##################################################################################
withlacoocheeriverstages <- streamH[streamH$siteID == 2312000, ]
plot(withlacoocheeriverstages$dateF, withlacoocheeriverstages$gheight.ft, 
     type = "b", pch = 15, 
     xlab = "Date (Sept 2017)", ylab = "Stage height (ft)", 
     main = "Withlacoochee River Stream Stage Heights (Sept 8-30, 2017)")
##################################################################################
##Question 2
##What was the earliest date of occurrence for each flood category in each river? How quickly did changes in flood category occur for each river? Do you think there was enough time for advanced warning before a flood category changed?

##action
actiondate <- floods %>%
  filter(gheight.ft >= action.ft & gheight.ft < flood.ft) %>%
  group_by(names) %>%
  summarise(action_min_date = min(dateF))
##flood
flooddate <- floods %>%
filter(gheight.ft >= flood.ft & gheight.ft <= moderate.ft) %>%
  group_by(names) %>%
  summarise(flood_min_date = min(dateF))
##moderate
moderatedate <- floods %>%
  filter(gheight.ft >= moderate.ft & gheight.ft <= major.ft) %>%
  group_by(names) %>%
  summarise(moderate_min_date = min(dateF))
##major
majordate <- floods %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise(major_min_date = min(dateF))

###Question 3
###Which river had the highest stream stage above its listed height in the major flood category?
floods$diff <- floods$gheight.ft - floods$major.ft
max_diff <- max(floods$diff)
highdiffrows <- which(floods$diff >= 7.85)
highdiffrivers <- floods$names[highdiffrows]
highdiffrivers


###Question 4
###Copy the url for your R script from GitHub and paste it here.
