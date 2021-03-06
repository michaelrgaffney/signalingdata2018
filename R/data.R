#' @title Signaling Data 2018
#' @description Depression Signaling Experiment 2018
#' @format A data frame with 1636 rows and 116 variables:
#' \describe{
#'   \item{\code{StartDate}}{start date of the study; measured by qualtrics}
#'   \item{\code{EndDate}}{end date of the study; measured by qualtrics}
#'   \item{\code{Status}}{records IP address if qualtrics deems it to be a "normal" response (i.e. not a preview or spam)}
#'   \item{\code{IPAddress}}{Ip address retrieved by qualtrics}
#'   \item{\code{Progress}}{percentage of the study completed}
#'   \item{\code{Duration (in seconds)}}{amount of time from opening survey until completion}
#'   \item{\code{Finished}}{Whether or not individual completed survey or was excluded due to missed attention checks}
#'   \item{\code{RecordedDate}}{date when the study was finished}
#'   \item{\code{ResponseId}}{unique identifier for responses created by qualtrics}
#'   \item{\code{RecipientLastName}}{not collected}
#'   \item{\code{RecipientFirstName}}{not collected}
#'   \item{\code{RecipientEmail}}{not collected}
#'   \item{\code{ExternalReference}}{not collected}
#'   \item{\code{LocationLatitude}}{latitude estimate based on Qualtric's GeoIP technology}
#'   \item{\code{LocationLongitude}}{longitude estimate based on Qualtric's GeoIP technology}
#'   \item{\code{DistributionChannel}}{the method of survey distribution}
#'   \item{\code{UserLanguage}}{respondent’s language code}
#'   \item{\code{MO1.1_1}}{needsmoneyt1(How much do you think your sister needs this money?(presignal; 0 = not at all; 100 = a great deal))}
#'   \item{\code{MO1.2_1}}{likelylendmoneyt1(How likely are you to lend the money to your sister?(presignal; 0 = very unlikely; 100 = very likely))}
#'   \item{\code{MO1.3_1}}{angryt1(How angry do you feel about this request?(presignal; 0 = extremely angry; 100 = not at all angry))}
#'   \item{\code{MO1.4_1}}{satisfactiont1(How much satisfaction would you get from helping with this request?(presignal; 0 = no satisfaction; 100 = a great deal of satisfaction))}
#'   \item{\code{MO1.5_1}}{howsadt1(How sad were you to learn about your sister’s child’s health problem?(presignal; 0 = devastated; 100 = not sad))}
#'   \item{\code{MO1.6_1}}{howreasonablet1(How reasonable is the amount of money being requested?(presignal; 0 = extremely unreasonable; 100 = extremely reasonable))}
#'   \item{\code{MO1.7_1}}{believehealtht1(How much do you believe your sister’s child has a health problem?(presignal; 0 = extremely unbelieving; 100 = extremely believing))}
#'   \item{\code{MO1.8_1}}{daughterharmt1(How much harm do you think this would cause to your daughter Sophie?(presignal; 0 = a great deal; 100 = none at all))}
#'   \item{\code{MO1.9_1}}{believeneedt1(How much do you suspect that this might be an attempt by your sister to get you to lend her money for another purpose?(presignal; 0 = I'm very suspicious; 100 = I'm completely unsuspicious))}
#'   \item{\code{MO1.10_1}}{sisterbenefitt1(How much help would lending the money to your sister help her and her child?(presignal; 0 = none at all; 100 = a great deal))}
#'   \item{\code{MO1.11_1}}{trustrepayt1(How much do you trust that your sister will pay you back?(presignal; 0 = extremely distrustful; 100 = extremely trustful))}
#'   \item{\code{MO1.12_1}}{comfortablelendingt1(What amount of money would you be comfortable with lending to your sister in this scenario in US dollars?(presignal; $0 - $50,000))}
#'   \item{\code{MC1.1_1}}{presignal manipulation check 1(Having read the story, how close do you feel to your sister?(0 = extremely distant; 100 = extremely close))}
#'   \item{\code{MC1.2_1}}{presignal manipulation check 2(In your opinion, is the amount of money your sister requested:(0 = extremely large; 100 = extremely close))}
#'   \item{\code{AC1:CC}}{attention check 1(In the story, who came to you for help?)}
#'   \item{\code{AC2:CC}}{attention check 2(In the story, how much money was requested of you?)}
#'   \item{\code{AC3:CC}}{attention check 3(In the story, what was your original plan for the money?)}
#'   \item{\code{AC4:CC}}{attention check 4(In the story, which of the following best describes your relationship with the person who requested the loan?)}
#'   \item{\code{AC5:CC}}{attention check 5(In the story, you first meet a person who asks you for some money. You then meet a second person who gives you some information about the first person. What was this information?)}
#'   \item{\code{AC1:CH}}{attention check 1(In the story, who came to you for help?)}
#'   \item{\code{AC2:CH}}{attention check 2(In the story, how much money was requested of you?)}
#'   \item{\code{AC3:CH}}{attention check 3(In the story, what was your original plan for the money?)}
#'   \item{\code{AC4:CH}}{attention check 4(In the story, which of the following best describes your relationship with the person who requested the loan?)}
#'   \item{\code{AC5:CH}}{attention check 5(In the story, you first meet a person who asks you for some money. You then meet a second person who gives you some information about the first person. What was this information?)}
#'   \item{\code{AC1:CP}}{attention check 1(In the story, who came to you for help?)}
#'   \item{\code{AC2:CP}}{attention check 2(In the story, how much money was requested of you?)}
#'   \item{\code{AC2:CP_1}}{attention check 3(In the story, what was your original plan for the money?)}
#'   \item{\code{AC3:CP}}{attention check 4(In the story, which of the following best describes your relationship with the person who requested the loan?)}
#'   \item{\code{AC4:CP}}{attention check 5(In the story, you first meet a person who asks you for some money. You then meet a second person who gives you some information about the first person. What was this information?)}
#'   \item{\code{AC1:SC}}{attention check 1(In the story, who came to you for help?)}
#'   \item{\code{AC2:SC}}{attention check 2(In the story, how much money was requested of you?)}
#'   \item{\code{AC3:SC}}{attention check 3(In the story, what was your original plan for the money?)}
#'   \item{\code{AC4:SC}}{attention check 4(In the story, which of the following best describes your relationship with the person who requested the loan?)}
#'   \item{\code{AC5:SC}}{attention check 5(In the story, you first meet a person who asks you for some money. You then meet a second person who gives you some information about the first person. What was this information?)}
#'   \item{\code{AC1:SH}}{attention check 1(In the story, who came to you for help?)}
#'   \item{\code{AC2:SH}}{attention check 2(In the story, how much money was requested of you?)}
#'   \item{\code{AC3:SH}}{attention check 3(In the story, what was your original plan for the money?)}
#'   \item{\code{AC4:SH}}{attention check 4(In the story, which of the following best describes your relationship with the person who requested the loan?)}
#'   \item{\code{AC5:SH}}{attention check 5(In the story, you first meet a person who asks you for some money. You then meet a second person who gives you some information about the first person. What was this information?)}
#'   \item{\code{AC1:SP}}{attention check 1(In the story, who came to you for help?)}
#'   \item{\code{AC2:SP}}{attention check 2(In the story, how much money was requested of you?)}
#'   \item{\code{AC3:SP}}{attention check 3(In the story, what was your original plan for the money?)}
#'   \item{\code{AC4:SP}}{attention check 4(In the story, which of the following best describes your relationship with the person who requested the loan?)}
#'   \item{\code{AC5:SP}}{attention check 5(In the story, you first meet a person who asks you for some money. You then meet a second person who gives you some information about the first person. What was this information?)}
#'   \item{\code{MO2.1_1}}{needsmoneyt2(After going to your sister’s house, has your belief that your sister needs the money increased, decreased, or not changed?(post-signal; 0 = decreased greatly; center = no change; 100 = increased greatly))}
#'   \item{\code{MO2.2_1}}{likelylendmoneyt2(After going to your sister’s house, are you more willing to lend her the money than before, less willing than before, or no change?(post-signal; 0 = much less willing; center = no change; 100 = much more willing))}
#'   \item{\code{MO2.3_1}}{angryt2(After going to your sister’s house, has your sister’s request for money made you more angry than you were, less angry than you were, or no change?(post-signal; 0 = much less angry; center = no change; 100 = much more angry))}
#'   \item{\code{MO2.4_1}}{howsadt2(After going to your sister’s house, are you more sad than you were about your sister’s child’s health problem, less sad than you were, or no change?(post-signal; 0 = much less sad; center = no change; 100 = much more sad))}
#'   \item{\code{MO2.5_1}}{satisfactiont2(After going to your sister's house, would you feel more satisfaction from helping your sister, less satisfaction, or no change?(post-signal; 0 = much less sad; center = no change; 100 = much more sad))}
#'   \item{\code{MO2.6_1}}{howreasonablet2(After going to your sister’s house, do you think the amount of money she requested is more reasonable than before, less reasonable than before, or no change?(post-signal; 0 = much less reasonable; center = no change; 100 = much more reasonable))}
#'   \item{\code{MO2.7_1}}{believehealtht2(After going to your sister’s house, has your belief that your sister’s child has a health problem increased, decreased, or not changed?(post-signal; 0 = decreased greatly; center = no change; 100 = increased greatly))}
#'   \item{\code{MO2.8_1}}{daughterharmt2(After going to your sister’s house, do you now think lending the money to your sister would harm your daughter Sophie more than you thought, less than you thought, or no change?(post-signal; 0 = cause much less harm; center = no change; 100 = cause much more harm))}
#'   \item{\code{MO2.9_1}}{believeneedt2(After going to your sister’s house, are you more suspicious that this might be an attempt to get you to lend her money for another purpose, less suspicious, or no change?(post-signal; 0 = much less suspicious; center = no change; 100 = much more suspicious))}
#'   \item{\code{MO2.10_1}}{sisterbenefitt2(After going to your sister’s house, do you think lending the money to your sister would help her child more than you did before, less than before did before, or no change?(post-signal; 0 = much less help; center = no change; 100 = much more help))}
#'   \item{\code{MO2.11_1}}{trustrepayt2(After going to your sister’s house, are you more trusting that your sister will pay you back than you were before, less trusting than you were before, or no change?(post-signal; 0 = much less trusting; center = no change; 100 = much more trusting))}
#'   \item{\code{MO2.12_1}}{comfortablelendingt2(After going to your sister's house, what amount of money would you now be comfortable with lending to your sister in this scenario in US dollars?(post-signal; $0 - $50,000))}
#'   \item{\code{MC2.1_1}}{post-signal manipulation check 1(After going to your sister's house, how close do you feel to your sister?(0 = extremely distant; 100 = extremely close))}
#'   \item{\code{MC2.2_1}}{post-signal manipulation check 2(After going to your sister's house, is the amount of money your sister requested:(0 = extremely large; 100 = extremely large))}
#'   \item{\code{MC2.3}}{post-signal manipulation check 3(How do you think your sister feels? Check all that apply.(Options include a list of emotional states and mental disorders))}
#'   \item{\code{MC2.4_1}}{post-signal manipulation check 4(How challenging was it to imagine yourself in this scenario?(0 = extremely difficult; 100 = extremely easy))}
#'   \item{\code{Age}}{self-reported age of the participant}
#'   \item{\code{Sex}}{self-reported sex of the participant}
#'   \item{\code{State}}{self-reported state of residence of the participant}
#'   \item{\code{Children}}{self-reported number of offspring of the participant}
#'   \item{\code{Siblings}}{self-reported number of siblings of the participant}
#'   \item{\code{BOrder}}{self-reported birth order of the participant}
#'   \item{\code{RelStat}}{self-reported current relationship status of the participant}
#'   \item{\code{Ed}}{self-reported highest level of education of the participant}
#'   \item{\code{MTurkID}}{MTurkID(used for payment)}
#'   \item{\code{Feedback}}{open feedback prompt(If you have any feedback about this study or if anything was confusing, please let us know by entering your comments here.)}
#'   \item{\code{SC0}}{number of attention checks correct}
#'   \item{\code{Score}}{number of attention checks correct}
#'   \item{\code{MTurkCode}}{randomly generated code to enter in MTurk for payment}
#'   \item{\code{conflict}}{conflict manipulation seen by participant}
#'   \item{\code{p_info}}{information manipulation seen by participant}
#'   \item{\code{FL_277_DO}}{records the conflict and information conditions shown to participants}
#'   \item{\code{FL_288_DO}}{records the conflict and information conditions shown to participants}
#'   \item{\code{FL_298_DO}}{records the conflict and information conditions shown to participants}
#'   \item{\code{FL_308_DO}}{records the conflict and information conditions shown to participants}
#'   \item{\code{FL_318_DO}}{records the conflict and information conditions shown to participants}
#'   \item{\code{FL_207_DO}}{records the conflict and information conditions shown to participants}
#'   \item{\code{MainOutcome1:Sister,$50,000_DO}}{question order for the first set of manipulation checks 1-11}
#'   \item{\code{MainOutcome2:Sister;50_DO}}{question order for the second set of manipulation checks 1-11}
#'   \item{\code{pass}}{whether or not the participant passed 4 of the 5 attention checks and was able to continue with the study}
#'   \item{\code{wordcount}}{word count for the feedback variable}
#'   \item{\code{ddupe}}{*use dupe instead* indicates whether this response is linked to multiple instances of identical GeoIP latitude and longitude coordinates(does not necessarily mean duplicated GeoIP)}
#'   \item{\code{latlong}}{used for creating dupecount variable}
#'   \item{\code{dupecount}}{count of number of times a particular GeoIP was duplicated}
#'   \item{\code{short}}{indicates responses times of < 215 seconds}
#'   \item{\code{dupe}}{indicates whether this response is linked to multiple instances of identical GeoIP coordinates}
#'   \item{\code{X1}}{row number(added by rIP function)}
#'   \item{\code{countryCode}}{code for the country in which the IP address associated with this attempt was located in as determined by rIP function in conjunction with IP Hub}
#'   \item{\code{countryName}}{country in which the IP address associated with this attempt was located in as determined by rIP function in conjunction with IP Hub}
#'   \item{\code{asn}}{autonomous system number of participant(provided by rIP function in conjunction with IP Hub)}
#'   \item{\code{isp}}{internet service provider of  participant(provided by rIP function in conjunction with IP Hub)}
#'   \item{\code{block}}{rIP's recommendations for blocking (0 = do not block; 1 = block; 2 = potentially fraudulent but stricter criteria)}
#'   \item{\code{hostname}}{hostname of participant(provided by rIP function in conjunction with IP Hub)}
#'   \item{\code{dupe2}}{creates variable indicating multiple instances of identical demographic variables to other responses}
#'   \item{\code{signal}}{signal manipulation seen by participant}
#'}
#' @details DETAILS
"signalingdata2018"
