#import raw data
library(readr)
library(devtools)
d <- read_csv("data-raw/Signals Main Study_ MTurk.csv")

d <- d[!is.na(d$Score),]
d$pass <- d$Score >= 4 #indicates whether or not four or more attention checks were passed
library(stringr)
d$wordcount <- str_count(d$Feedback, "\\w+") #number of words provided in feedback prompt

#adds ddupe to d, which indicates whether or not GeoIP was duplicated in d
#NOTE:if used, change. Does not measure based on duplicated long and lat in combined manner.
repeatedlat1 <- table(d$LocationLatitude)
repeatedlat1 <- repeatedlat1[repeatedlat1 > 1]
repeatedlong1 <- table(d$LocationLongitude)
repeatedlong1 <- repeatedlong1[repeatedlong1 > 1]
d$ddupe <- FALSE
for(i in 1:nrow(d)){
  if(d$LocationLatitude[i] %in% names(repeatedlat1) & d$LocationLongitude[i] %in% names(repeatedlong1)){
    d$ddupe[i] <- TRUE
  }
}

#creates dupecount, which counts number of GeoIP dupes
d$latlong <- paste(d$LocationLatitude, d$LocationLongitude)
x<- table(d$latlong)
d$dupecount <- as.vector(x[d$latlong])

#new dataframe with only completed responses with passed attention checks
d2 <- d[d$Score >= 4,]
d2 <- d2[d2$Progress == 100,] #remove rows of unfinished surveys
d2$short <- d2$`Duration (in seconds)` < 215

#creates dupe variable for multiple instances of identical GeoIP coordinates in d2 only
repeatedlat <- table(d2$LocationLatitude)
repeatedlat <- repeatedlat[repeatedlat > 1]
repeatedlong <- table(d2$LocationLongitude)
repeatedlong <- repeatedlong[repeatedlong > 1]
d2$dupe <- FALSE
for(i in 1:nrow(d2)){
  if(d2$LocationLatitude[i] %in% names(repeatedlat) & d2$LocationLongitude[i] %in% names(repeatedlong)){
    d2$dupe[i] <- TRUE
  }
}

#VPN Test added to d2
library(rIP)
library(tidyverse)
#d3 <- data.frame(IPAddress = na.omit(d2$IPAddress), stringsAsFactors = F)
#d3$id <- 1:nrow(d3)
csv1 <- read_csv("data-raw/ip_vpn.csv") #1-900
csv2 <- read_csv("data-raw/ip_vpn2.csv") #901-1651 (d2 plus incomplete AC passes CHECK)
csvall <- rbind(csv1, csv2)
ds <- duplicated(csvall$ip)
csvall<- csvall[!ds,]
#dip <- getIPinfo(d3[901:1651,], "IPAddress", "MzU3Mzo5eEl3Z0E1cFJHRUVSSHpkTTNYZnFJeFJ0MFN3Yml2dQ==")
#write.csv(x = dip, file ="ip_vpn2.csv" )
d2 <- left_join(d2,csvall, by = c("IPAddress" = "ip"))

#creates dupe2 variable for multiple instances of identical demographic variables (only exists for passes)
d2$dupe2 <- duplicated(d2[c("Sex", "State", "Age", "BOrder","Children", "Siblings", "RelStat", "Ed")])
sum(d2$dupe2)
d2b <- d2[c("IPAddress", "LocationLatitude", "LocationLongitude", "block", "Sex", "State", "Age", "BOrder", "dupe",
            "dupe2", "Children", "Siblings", "RelStat", "Ed")]

#filtering options
#baseline criteria
sum(d2$dupe == 1) #305 (27.7 mean per cell)
sum(d2$block >= 1) #90 (32.3 mean per cell)
sum(d2$`Duration (in seconds)` < 200) #45 (33.1 mean per cell)

#dupe and block
sum(d2$dupe == 1 & d2$block >= 1) #49 (33.1 mean per cell)

#dupe and block or time under 200 seconds
sum(d2$dupe == 1 & d2$block >= 1) +  sum(d2$`Duration (in seconds)` < 200 & d2$dupe == 0 & d2$block == 0)
#78 (32.5 mean per cell)

#block or time under 200 seconds
sum(d2$`Duration (in seconds)` < 200 | d2$block != 0)
#131 (31.4 mean per cell)   I favor this one. Then do no dupes as main robustness check.

#dupe or block or time under 200 seconds
sum(d2$dupe == 1) + sum(d2$block >= 1 & d2$dupe == 0) +  sum(d2$`Duration (in seconds)` < 200 & d2$dupe == 0 & d2$block == 0)
#375 (26.3 mean per cell)

sum(d2$`Duration (in seconds)` < 200 | d2$countryCode != "US")
#136 (31.3 mean per cell)

sum(d2$dupe == 1 & d2$`Duration (in seconds)` < 200)
#13
#View(d2[(d2$dupe == 1 & d2$`Duration (in seconds)` < 200),])

#Duration in seconds decision
#hist(log10(d2$`Duration (in seconds)`))
hist(d2$`Duration (in seconds)`[d2$`Duration (in seconds)` < 1000])
xtabs(~short + block + ddupe, d2)
plot(xtabs(~short + block + ddupe, d2))

hist(d2$`Duration (in seconds)`[d2$dupe])
hist(d2$`Duration (in seconds)`[!d2$dupe & d2$`Duration (in seconds)` < 1000])
hist(d2$`Duration (in seconds)`[d2$block != 1 & d2$`Duration (in seconds)` < 1000])
plot(xtabs(~block + dupe, d2))

library(ggplot2)
d %>%
  filter(`Duration (in seconds)` < 500) %>%
  ggplot(aes(`Duration (in seconds)` , color = pass)) + geom_density() + geom_vline(xintercept = 215)

plot(ecdf(d2$`Duration (in seconds)`[d2$`Duration (in seconds)` < 1000]))
sum(d2$`Duration (in seconds)` <= 200, na.rm = TRUE )
table(d2$`Duration (in seconds)`[d2$`Duration (in seconds)` < 1000])
plot(xtabs(~I(`Duration (in seconds)` < 200) + dupe, d2))
plot(xtabs(~I(`Duration (in seconds)` < 200) + I(block == 1), d2))
#filter out under 200 seconds and get rid of dupe and block == 1

#narrowed down criteria
sum(d2$`Duration (in seconds)` < 215) #68
sum(d2$ddupe == 1 & d2$block >= 1) #54
sum(d2$dupecount >= 6) #69

#option 1
sum(d2$dupecount >= 6 | d2$block >= 1 | d2$`Duration (in seconds)` < 215)

#190
d2$wordcount[is.na(d2$wordcount)] <- 0
sum((d2$dupecount >= 6 | d2$block >= 1 | d2$`Duration (in seconds)` < 215) & d2$wordcount < 10, na.rm = TRUE)

d2$exclusionopt1 <- ((d2$dupecount >= 6 | d2$block >= 1 | d2$`Duration (in seconds)` < 215) & d2$wordcount < 10)
ggplot(d2, aes(wordcount + 1, color = exclusionopt1)) + geom_density() + scale_x_log10() + geom_vline(xintercept = 2)

na.omit(d2$Feedback[d2$exclusionopt1 & d2$wordcount > 10])

#option 2
sum((d2$ddupe & d2$block) | d2$`Duration (in seconds)` < 215)
#93 1,543/48= 32.15 #120
(1636 - 120)/48 # 31.58333

d2$exclusionopt2 <- ((d2$ddupe & d2$block) | d2$`Duration (in seconds)` < 215)
sum(d2$exclusionopt2)

#option 3 ddupe >= 6
sum((d2$dupecount >= 6 | d2$`Duration (in seconds)` < 215)) #135
d2$exclusionopt3 <- (d2$dupecount >= 6 | d2$`Duration (in seconds)` < 215)
sum(d2$exclusionopt3)

#option 4 just low time
sum(d2$`Duration (in seconds)` < 215) #68
d2$exclusionopt4 <- (d2$`Duration (in seconds)` < 215)
sum(d2$exclusionopt4)

d2 %>%
  filter(`Duration (in seconds)` < 1000) %>%
ggplot(aes(`Duration (in seconds)`, color = (ddupe == 1 & block >= 1))) + geom_density() +geom_vline(xintercept = 200)
d2 %>%
  filter(`Duration (in seconds)` < 1000) %>%
ggplot(aes(`Duration (in seconds)`, color = (dupecount >= 6))) + geom_density() +geom_vline(xintercept = 200)

xtabs(~(dupecount >= 6) + block >= 1, d2 )
plot(xtabs(~(dupecount >= 6) + block >= 1, d2 ))

plot(xtabs(~I(dupecount >= 6) + block, d2))
plot(xtabs(~I(`Duration (in seconds)` < 200) + I(block == 1), d2))

library(ggplot2)
ggplot(d2, aes(`Duration (in seconds)`)) + geom_density() + scale_x_log10() +geom_vline(xintercept = 200)
#215 seconds for me without consent form

# We will also flag responses under four and a half minutes as suspicious and examine them for
# evidence of cheating the rules of the study. However, they will only be eliminated if there is
# further evidence of cheating.

#Outcome variable NAs (no movement = NA)
sum(is.na(d2$MO2.12_1)) #10 "After going to your sister's house, what amount of money would you now be
# comfortable with lending to your sister in this scenario in US dollars?"
sum(is.na(d2$MO2.2_1)) #27 "After going to your sister’s house, are you more willing to lend her
# the money than before, less willing than before, or no change?"

#non-US completion examanation
sum(d2$countryCode != "US")
dforeign <- d2[d2$countryCode != "US",] #dataframe for visual analysis of non-US completions
table(dforeign$block)

dforeign2 <- dforeign[c("IPAddress", "LocationLatitude", "LocationLongitude", "block", "Sex", "State", "Age", "BOrder", "dupe",
            "dupe2", "Children", "Siblings", "RelStat", "Ed", "Feedback", "countryName")]

dblock <- d2[d2$block != 0,] #dataframe for visual analysis of blocks
plot(xtabs(~I(countryCode == "US") + dupe, d2))
plot(xtabs(~I(countryCode == "US") + block, d2))
plot(xtabs(~I(countryCode == "US") + I(`Duration (in seconds)` < 200), d2))

#models to predict failed attention checks(proxy for low quality responses)
library(rpart)
library(rpart.plot)

#creates d5 (d with rIP info for all)
#d4<- data.frame(IPAddress = na.omit(d$IPAddress[d$Score < 4]), stringsAsFactors = F)
#dip <- getIPinfo(d4,"IPAddress", "MzU3Mzo5eEl3Z0E1cFJHRUVSSHpkTTNYZnFJeFJ0MFN3Yml2dQ==")
#write.csv(x = dip, file ="ip_vpn3.csv" )
csv3 <- read_csv("data-raw/ip_vpn3.csv")
#dip <- left_join(dip, d[c("IPAddress", "Score", "LocationLongitude", "LocationLatitude")], by = c(ip = "IPAddress"))
#d5 <- left_join(d, dip, by = c("IPAddress" = "ip"))
#csv1 <- read_csv("ip_vpn.csv")
#csv2 <- read_csv("ip_vpn2.csv")
csvall <- rbind(csv1, csv2, csv3)
ds <- duplicated(csvall$ip)
csvall<- csvall[!ds,]
d5 <- left_join(d, csvall, by = c("IPAddress" = "ip"))
#adds US variable
d5$US <- ifelse(d5$countryCode != "US", FALSE, TRUE)
table(d5$US)

ggplot(d2, aes(wordcount, color = (ddupe == 1 & block >= 1))) + geom_density() + scale_x_log10(breaks = c(1,2,3,4,5,10,20,50,100))

ggplot(d2, aes(wordcount, color = (ddupe == 1))) + geom_density() + scale_x_log10(breaks = c(1,2,3,4,5,10,20,50,100))
ggplot(d2, aes(wordcount, color = (block >= 1))) + geom_density() + scale_x_log10(breaks = c(1,2,3,4,5,10,20,50,100))

d5%>%
  filter(ddupe == 0 & block ==0) %>%
  View()

m <- rpart(pass~ block + countryName + LocationLongitude + LocationLatitude + ddupe, d5)
rpart.plot(m)

m2 <- rpart(pass~ block + countryCode + ddupe, d5)
rpart.plot(m2)

#seems good
m3 <- rpart(pass~ block + US + ddupe, d5)
rpart.plot(m3)
table(d5$US)

m3b <- rpart(pass~ block + US  + dupecount, d5)
rpart.plot(m3b)

ggplot(d5, aes(LocationLongitude, LocationLatitude, color = factor(block))) + geom_point() + facet_wrap(~I(`Duration (in seconds)` > 200)+pass + ddupe + US)
# high percentage of blocks in the non-US passers with responses > 200 seconds

#without US/non-US distinction
ggplot(d5, aes(LocationLongitude, LocationLatitude, color = factor(block))) + geom_point() + facet_wrap(~I(`Duration (in seconds)` > 200)+pass + ddupe)
#adds successful dupes
d5$dupe <- ifelse(d5$Score <4, TRUE, FALSE)
table(d5$dupe)
m4 <- rpart(pass~ block + US + ddupe + dupe, d5)
rpart.plot(m4)

#not US
library(ggplot2)
ggplot(d5, aes(LocationLongitude, LocationLatitude, color = pass)) + geom_point(alpha = .2) + facet_wrap(~pass)
ggplot(d5, aes(LocationLongitude, LocationLatitude, color = pass)) + geom_point(alpha = .2) + facet_grid(I(`Duration (in seconds)` > 200)~pass)
ggplot(d5, aes(LocationLongitude, LocationLatitude, color = factor(block))) + geom_point() + facet_wrap(~I(`Duration (in seconds)` > 200)+pass + block)

#logistic regression
library(binomTools)
d6 <- na.omit(d5[,c("pass","block","countryCode", "countryName", "LocationLongitude", "LocationLatitude", "ddupe", "US", "dupecount")])
d6$pass<-as.factor(d6$pass)
ggplot(d5, aes(LocationLatitude, pass)) + geom_point(alpha = .25)
mglm <- glm(pass ~ block + US + ddupe, family=binomial, data = d6)
mglm
exp(coef(mglm)) #odds ratio
Rsq(mglm)
plot(Rsq(mglm))

mglm2 <- glm(pass ~ block + countryCode + LocationLongitude + LocationLatitude, family=binomial, data = d6)
exp(coef(mglm2)) #odds ratio
Rsq(mglm2)
plot(Rsq(mglm2))

#random forest (need training and test?)
library(randomForest)

mrf <- randomForest(pass ~ block + LocationLongitude + LocationLatitude + ddupe, na.action = na.omit, data = d6)
mrf

mrf1 <- randomForest(pass ~ block + US + ddupe, na.action = na.omit, data = d6)
mrf1

mrf2 <- randomForest(pass ~ block + US + dupecount, na.action = na.omit, data = d6)
mrf2

#preparation for analysis
library(tidyr)
library(dplyr)
d2$FL_4_DO[d2$FL_4_DO == "Vignette:Sister;$50,000;Conflict,Cheating"] <- "Vignette:Sister,$50,000,Conflict,Cheating"
signalingdata2018 <-
  d2 %>%
  # select(FL_4_DO) %>%
  separate(FL_4_DO, sep =",", into = c("Vignette", "Money1", "Money2", "conflict", "p_info")) %>%
  select(-Vignette, -Money1 , -Money2)

#shows NAs
signalingdata2018$signal <- apply(signalingdata2018[c("FL_277_DO", "FL_288_DO", "FL_298_DO", "FL_308_DO", "FL_318_DO", "FL_207_DO")], MARGIN = 1, na.omit)
#d7 <- d2[c("signal", "FL_277_DO", "FL_288_DO", "FL_298_DO", "FL_308_DO", "FL_318_DO", "FL_207_DO")]
#Outcome variable NAs (no movement = NA)
sum(is.na(d2$MO2.12_1)) #10 "After going to your sister's house, what amount of money would you now be
# comfortable with lending to your sister in this scenario in US dollars?"
sum(is.na(d2$MO2.2_1)) #27 "After going to your sister’s house, are you more willing to lend her
# the money than before, less willing than before, or no change?"
sum(is.na(d2$MO2.2_1) & is.na(d2$MO2.12_1))

#m_main <- lm(formula = helping ~ conflict + p_info + signal, data = d2)
#m_int <- lm(formula = helping ~ conflict + p_info + signal +
              #  + conflict:signal + conflict:p_info + signal:p_info, data = d2)

signalingdata2018 <-
  signalingdata2018 %>%
  rename(
    needsmoneyt1 = MO1.1_1,
    likelylendmoneyt1 = MO1.2_1,
    angryt1 = MO1.3_1,
    satisfactiont1 = MO1.4_1,
    howsadt1 = MO1.5_1,
    howreasonablet1 = MO1.6_1,
    believehealtht1 = MO1.7_1,
    daughterharmt1 = MO1.8_1,
    believeneedt1 = MO1.9_1,
    sisterbenefitt1 = MO1.10_1,
    trustrepayt1 = MO1.11_1,
    comfortablelendingt1 = MO1.12_1,
    needsmoneyt2 = MO2.1_1,
    likelylendmoneyt2 = MO2.2_1,
    angryt2 = MO2.3_1,
    howsadt2 = MO2.4_1,
    satisfactiont2 = MO2.5_1,
    howreasonablet2 = MO2.6_1,
    believehealtht2 = MO2.7_1,
    daughterharmt2 = MO2.8_1,
    believeneedt2 = MO2.9_1,
    sisterbenefitt2 = MO2.10_1, #not identical to first showing
    trustrepayt2 = MO2.11_1,
    comfortablelendingt2 = MO2.12_1
  )


use_data(signalingdata2018, overwrite = TRUE)


