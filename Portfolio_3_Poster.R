#load the packages
#install.packages("polycor")
#install.packages("nFactors")
library(psych)
library(polycor)
library(nFactors)
library(tidyverse)
library(EGAnet)


##Load EATQ data
EATQ_full_data_frame <- read.table('G:\\My Drive\\ABCD\\abcd_eatqp01.txt', 
                                   header = TRUE, sep = "\t", quote = "", fill = TRUE)


##Load UPPS data
UPPS_full_data_frame <- read.table('G:\\My Drive\\ABCD\\abcd_upps01.txt', 
                                   header = TRUE, sep = "\t", quote = "", fill = TRUE)

##Limit EATQ to data and turn numeric
EATQ_col <- EATQ_full_data_frame[-1,-c(1:10, 73:74)] 
EATQ_data <- as.data.frame(sapply(EATQ_col, function(x) gsub("\"", "", x)))
EATQ_data_num <- lapply(EATQ_data, as.numeric)
EATQ_complete <- as.data.frame(EATQ_data_num)

##Limit UPPS to data and turn numeric
UPPS_quote <- as.data.frame(sapply(UPPS_full_data_frame, function(x) gsub("\"", "", x)))
UPPS_limit <- UPPS_quote[UPPS_quote[,9] == "2_year_follow_up_y_arm_1",] 
UPPS_data <- UPPS_limit[,-c(1:9, 30:31)]
UPPS_data_num <- lapply(UPPS_data, as.numeric)
UPPS_complete <- as.data.frame(UPPS_data_num)

#Rename EATQ
EATQ_complete <- rename(EATQ_complete, c("tbl" = "X.eatq_trouble_p."), c("ins" = "X.eatq_insult_p."),
                               c("fin" = "X.eatq_finish_p."), c("afr" = "X.eatq_africa_p."), c("del" = "X.eatq_deal_p."),
                               c("trn" = "X.eatq_turn_taking_p."), c("enj" = "X.eatq_enjoy_p."), c("opn" = "X.eatq_open_present_p."),
                               c("ski" = "X.eatq_ski_slope_p."), c("cry" = "X.eatq_cry_p."), c("ang" = "X.eatq_angry_hit_p."),
                               c("car" = "X.eatq_care_p."), c("shr" = "X.eatq_share_p."), c("bfr" = "X.eatq_before_hw_p."),
                               c("con" = "X.eatq_concentrate_p."), c("cty" = "X.eatq_city_move_p."), c("rgt" = "X.eatq_right_away_p."),
                               c("tme" = "X.eatq_spend_time_p."), c("rde" = "X.eatq_rude_p."),  c("any" = "X.eatq_annoyed_p."),
                               c("irr" = "X.eatq_irritated_crit_p."), c("dis" = "X.eatq_distracted_p."), c("imp" = "X.eatq_impulse_p."),
                               c("hug" = "X.eatq_hugs_p."), c("blm" = "X.eatq_blame_p."), c("sad" = "X.eatq_sad_p."),
                               c("soc" = "X.eatq_social_p."),  c("sea" = "X.eatq_sea_dive_p."), c("trv" = "X.eatq_travel_p."),
                               c("wor" = "X.eatq_worry_p."), c("plc" = "X.eatq_irritated_place_p."), c("slm" = "X.eatq_doorslam_p."),
                               c("hrd" = "X.eatq_hardly_sad_p."), c("rce" = "X.eatq_race_car_p."), c("foc" = "X.eatq_try_focus_p."),
                               c("hw" = "X.eatq_finish_hw_p."), c("exc" = "X.eatq_school_excite_p."), c("stt" = "X.eatq_early_start_p."),
                               c("per" = "X.eatq_peripheral_p."), c("eng" = "X.eatq_energized_p."), c("fun" = "X.eatq_makes_fun_p."),
                               c("crt" = "X.eatq_no_criticize_p."), c("cls" = "X.eatq_close_rel_p."), c("shy" = "X.eatq_is_shy_p."),
                        c("iej" = "X.eatq_irritated_enjoy_p."), c("sde" = "X.eatq_sidetracked_p."),
                        c("att" = "X.eatq_attachment_p."), c("put" = "X.eatq_puts_off_p."), c("lgh" = "X.eatq_laugh_control_p."),
                        c("nsy" = "X.eatq_not_shy_p."), c("fri" = "X.eatq_friendly_p."),
                        c("sms" = "X.eatq_seems_sad_p."), c("bal" = "X.eatq_ball_scared_p."), c("met" = "X.eatq_meet_p."),
                        c("drk" = "X.eatq_dark_scared_p." ), c("fru" = "X.eatq_frustrated_p."),
                        c("dsa" = "X.eatq_disagree_p."), c("srd" = "X.eatq_rides_scared_p."),
                        c("stk" = "X.eatq_stick_to_plan_p."), c("cla" = "X.eatq_close_attention_p."), c("aln" = "X.eatq_alone_p."),
                        c("smt" = "X.eatq_shy_meet_p."))

###Reverse code variables

EATQ_complete %>%
  mutate(fin = recode(fin, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(trn = recode(trn, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(opn = recode(opn, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(ski = recode(ski, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(bfr = recode(bfr, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(dis = recode(dis, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(imp = recode(imp, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(soc = recode(soc, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(hrd = recode(hrd, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(foc = recode(foc, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(crt = recode(crt, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))


EATQ_complete %>%
  mutate(put = recode(put, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(sde = recode(sde, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(nsy = recode(nsy, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(met = recode(met, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

EATQ_complete %>%
  mutate(srd = recode(srd, '1' = '5',
                      '2' = '4',
                      '3' = '3',
                      '4' = '2',
                      '5' = '1'))

###There are sketchy values in the dataframe. The scale is 1-5, 
###but the data has 6's. We're going to delete the 6's.

EATQ_complete[EATQ_complete == 6]<-NA


###Frequency Table of responses-EATQ
EATQ_freq <- lapply(EATQ_data,table)

###Frequency Table of responses-UPPS
UPPS_freq <- lapply(UPPS_data,table)

#Finding correlations: EATQ
EATQ.corr <- polycor::hetcor(EATQ_complete)

##Finding number of factors for EATQ data
ev <- eigen(EATQ.corr$correlations) # get eigenvalues
ap <- parallel(subject=nrow(EATQ.corr$correlations),var=ncol(EATQ.corr$correlations),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

#Scree plot suggests 2 factors, so try EFA with 2 factors

EATQ_EFA_2 <- fa(r=EATQ.corr$correlations, 
                   n.obs=6572, 
                   nfactor=2, 
                   cor="poly", 
                   fm="mle", 
                   rotate = "oblimin")

#I think we'll need to eliminate some low loading (<.4) items,
#but let's try a different number of factors first: 3.

EATQ_EFA_3 <- fa(r=EATQ.corr$correlations, 
                 n.obs=6572, 
                 nfactor=3, 
                 cor="poly", 
                 fm="mle", 
                 rotate = "oblimin")

###The fit indices aren't there yet. Let's try four factors.

EATQ_EFA_4 <- fa(r=EATQ.corr$correlations, 
                 n.obs=6572, 
                 nfactor=4, 
                 cor="poly", 
                 fm="mle", 
                 rotate = "oblimin")

###Still trying to get adequate fit indices: 5 factors.

EATQ_EFA_5 <- fa(r=EATQ.corr$correlations, 
                 n.obs=6572, 
                 nfactor=5, 
                 cor="poly", 
                 fm="mle", 
                 rotate = "oblimin")

##Still doesn't fit: try 6

EATQ_EFA_6 <- fa(r=EATQ.corr$correlations, 
                 n.obs=6572, 
                 nfactor=6, 
                 cor="poly", 
                 fm="mle", 
                 rotate = "oblimin")

##RMSEA fits but TLI does not: try 7 
EATQ_EFA_7 <- fa(r=EATQ.corr$correlations, 
                 n.obs=6572, 
                 nfactor=7, 
                 cor="poly", 
                 fm="mle", 
                 rotate = "oblimin")

###Try 8

EATQ_EFA_8 <- fa(r=EATQ.corr$correlations, 
                 n.obs=6572, 
                 nfactor=8, 
                 cor="poly", 
                 fm="mle", 
                 rotate = "oblimin")

###Try 9 


EATQ_EFA_9 <- fa(r=EATQ.corr$correlations, 
                 n.obs=6572, 
                 nfactor=9, 
                 cor="poly", 
                 fm="mle", 
                 rotate = "oblimin")

###TLI is still really low, so I'm going back to where the RMSEA (6 factors)
###first fit and removing the low loading items.

EATQ_remove <- subset(EATQ_complete, select = -c(tbl,afr,
                                                 opn,con,
                                                 cty, rde,
                                                 sea, rce,
                                                 lgh, bal,
                                                 drk)) 

##Now to rerun the screeplot

#Finding correlations: EATQ
EATQ.corr.rem <- polycor::hetcor(EATQ_remove)

##Finding number of factors for EATQ data
ev <- eigen(EATQ.corr.rem$correlations) # get eigenvalues
ap <- parallel(subject=nrow(EATQ.corr.rem$correlations),var=ncol(EATQ.corr.rem$correlations),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

###Screeplot suggests 2 factors. 

EATQ_EFA_2_rem <- fa(r=EATQ.corr.rem$correlations, 
                 n.obs=6572, 
                 nfactor=2, 
                 cor="poly", 
                 fm="mle", 
                 rotate = "oblimin")

###Does not fit: 3 factor

EATQ_EFA_3_rem <- fa(r=EATQ.corr.rem$correlations, 
                     n.obs=6572, 
                     nfactor=3, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###Does not fit: 4 factor

EATQ_EFA_4_rem <- fa(r=EATQ.corr.rem$correlations, 
                     n.obs=6572, 
                     nfactor=4, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###RMSEA fit, but TLI does not: 5 factors

EATQ_EFA_5_rem <- fa(r=EATQ.corr.rem$correlations, 
                     n.obs=6572, 
                     nfactor=5, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###RMSEA fit, but TLI does not: 6 factors

EATQ_EFA_6_rem <- fa(r=EATQ.corr.rem$correlations, 
                     n.obs=6572, 
                     nfactor=6, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###RMSEA fit, but TLI does not: 7 factors

EATQ_EFA_7_rem <- fa(r=EATQ.corr.rem$correlations, 
                     n.obs=6572, 
                     nfactor=7, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###RMSEA fit, but TLI does not: 8 factors

EATQ_EFA_8_rem <- fa(r=EATQ.corr.rem$correlations, 
                     n.obs=6572, 
                     nfactor=8, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###RMSEA fit, but TLI does not: 9 factors

EATQ_EFA_9_rem <- fa(r=EATQ.corr.rem$correlations, 
                     n.obs=6572, 
                     nfactor=9, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")