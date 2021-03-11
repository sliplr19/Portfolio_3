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

###RMSEA fit, but TLI does not: 11 factors

EATQ_EFA_11_rem <- fa(r=EATQ.corr.rem$correlations, 
                     n.obs=6572, 
                     nfactor=11, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###RMSEA fit, but TLI does not: 12 factors

EATQ_EFA_12_rem <- fa(r=EATQ.corr.rem$correlations, 
                      n.obs=6572, 
                      nfactor=12, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, but TLI does not: 13 factors

EATQ_EFA_13_rem <- fa(r=EATQ.corr.rem$correlations, 
                      n.obs=6572, 
                      nfactor=13, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, but TLI does not: 14 factors

EATQ_EFA_14_rem <- fa(r=EATQ.corr.rem$correlations, 
                      n.obs=6572, 
                      nfactor=14, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, but TLI does not: 15 factors

EATQ_EFA_15_rem <- fa(r=EATQ.corr.rem$correlations, 
                      n.obs=6572, 
                      nfactor=15, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, but TLI does not: 16 factors

EATQ_EFA_16_rem <- fa(r=EATQ.corr.rem$correlations, 
                      n.obs=6572, 
                      nfactor=16, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, but TLI does not: 17 factors

EATQ_EFA_17_rem <- fa(r=EATQ.corr.rem$correlations, 
                      n.obs=6572, 
                      nfactor=17, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, but TLI does not: 18 factors

EATQ_EFA_18_rem <- fa(r=EATQ.corr.rem$correlations, 
                      n.obs=6572, 
                      nfactor=18, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, but TLI does not: 19 factors

EATQ_EFA_19_rem <- fa(r=EATQ.corr.rem$correlations, 
                      n.obs=6572, 
                      nfactor=19, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, but TLI does not: 20 factors

EATQ_EFA_20_rem <- fa(r=EATQ.corr.rem$correlations, 
                      n.obs=6572, 
                      nfactor=20, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###I couldn't get TLI above .9, so I'm going to go back to the 
###original data and try again

###RMSEA fit, TLI not: try 10 


EATQ_EFA_10 <- fa(r=EATQ.corr$correlations, 
                 n.obs=6572, 
                 nfactor=10, 
                 cor="poly", 
                 fm="mle", 
                 rotate = "oblimin")

###RMSEA fit, TLI not: try 11 


EATQ_EFA_11 <- fa(r=EATQ.corr$correlations, 
                  n.obs=6572, 
                  nfactor=11, 
                  cor="poly", 
                  fm="mle", 
                  rotate = "oblimin")

###RMSEA fit, TLI not: try 12 


EATQ_EFA_12 <- fa(r=EATQ.corr$correlations, 
                  n.obs=6572, 
                  nfactor=12, 
                  cor="poly", 
                  fm="mle", 
                  rotate = "oblimin")

###RMSEA fit, TLI not: try 13 


EATQ_EFA_13 <- fa(r=EATQ.corr$correlations, 
                  n.obs=6572, 
                  nfactor=13, 
                  cor="poly", 
                  fm="mle", 
                  rotate = "oblimin")

###RMSEA fit, TLI not: try 14 


EATQ_EFA_14 <- fa(r=EATQ.corr$correlations, 
                  n.obs=6572, 
                  nfactor=14, 
                  cor="poly", 
                  fm="mle", 
                  rotate = "oblimin")

###RMSEA fit, TLI not: try 15 


EATQ_EFA_15 <- fa(r=EATQ.corr$correlations, 
                  n.obs=6572, 
                  nfactor=15, 
                  cor="poly", 
                  fm="mle", 
                  rotate = "oblimin")

###RMSEA fit, TLI not: try 16 


EATQ_EFA_16 <- fa(r=EATQ.corr$correlations, 
                  n.obs=6572, 
                  nfactor=16, 
                  cor="poly", 
                  fm="mle", 
                  rotate = "oblimin")

###RMSEA fit, TLI not: try 17


EATQ_EFA_17 <- fa(r=EATQ.corr$correlations, 
                  n.obs=6572, 
                  nfactor=17, 
                  cor="poly", 
                  fm="mle", 
                  rotate = "oblimin")

###RMSEA fit, TLI not: try 18


EATQ_EFA_18 <- fa(r=EATQ.corr$correlations, 
                  n.obs=6572, 
                  nfactor=18, 
                  cor="poly", 
                  fm="mle", 
                  rotate = "oblimin")

###RMSEA fit, TLI not: try 19


EATQ_EFA_19 <- fa(r=EATQ.corr$correlations, 
                  n.obs=6572, 
                  nfactor=19, 
                  cor="poly", 
                  fm="mle", 
                  rotate = "oblimin")

###RMSEA fit, TLI not: try 20


EATQ_EFA_20 <- fa(r=EATQ.corr$correlations, 
                  n.obs=6572, 
                  nfactor=20, 
                  cor="poly", 
                  fm="mle", 
                  rotate = "oblimin")

##We're going to try this one last time, this time removing a 
##different set of items.


EATQ_remove_2 <- subset(EATQ_complete, select = -c(tbl,del,
                                                   trn, opn,
                                                   car, shr, 
                                                   bfr, con,
                                                   rgt, tme,
                                                   rde, any,
                                                   dis, hug,
                                                   blm, sad,
                                                   wor, hrd,
                                                   rce, foc,
                                                   eng, crt,
                                                   cls)) 

#Finding correlations: EATQ
EATQ.corr.2 <- polycor::hetcor(EATQ_remove_2)

##Finding number of factors for EATQ data
ev <- eigen(EATQ.corr.2$correlations) # get eigenvalues
ap <- parallel(subject=nrow(EATQ.corr.2$correlations),var=ncol(EATQ.corr.2$correlations),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

###Scree plot suggest 2 factors


EATQ_EFA_two_2 <- fa(r=EATQ.corr$correlations, 
                  n.obs=6572, 
                  nfactor=2, 
                  cor="poly", 
                  fm="mle", 
                  rotate = "oblimin")

###Does not fit: try 3 factors


EATQ_EFA_two_3 <- fa(r=EATQ.corr$correlations, 
                     n.obs=6572, 
                     nfactor=3, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###Does not fit: try 4 factors


EATQ_EFA_two_4 <- fa(r=EATQ.corr$correlations, 
                     n.obs=6572, 
                     nfactor=4, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###Does not fit: try 5 factors


EATQ_EFA_two_5 <- fa(r=EATQ.corr$correlations, 
                     n.obs=6572, 
                     nfactor=5, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###Does not fit: try 6 factors


EATQ_EFA_two_6 <- fa(r=EATQ.corr$correlations, 
                     n.obs=6572, 
                     nfactor=6, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 7 factors


EATQ_EFA_two_7 <- fa(r=EATQ.corr$correlations, 
                     n.obs=6572, 
                     nfactor=7, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 8 factors


EATQ_EFA_two_8 <- fa(r=EATQ.corr$correlations, 
                     n.obs=6572, 
                     nfactor=8, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 9 factors


EATQ_EFA_two_9 <- fa(r=EATQ.corr$correlations, 
                     n.obs=6572, 
                     nfactor=9, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 10 factors


EATQ_EFA_two_10 <- fa(r=EATQ.corr$correlations, 
                     n.obs=6572, 
                     nfactor=10, 
                     cor="poly", 
                     fm="mle", 
                     rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 11 factors


EATQ_EFA_two_11 <- fa(r=EATQ.corr$correlations, 
                      n.obs=6572, 
                      nfactor=11, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 12 factors


EATQ_EFA_two_12 <- fa(r=EATQ.corr$correlations, 
                      n.obs=6572, 
                      nfactor=12, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 13 factors


EATQ_EFA_two_13 <- fa(r=EATQ.corr$correlations, 
                      n.obs=6572, 
                      nfactor=13, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 14 factors


EATQ_EFA_two_14 <- fa(r=EATQ.corr$correlations, 
                      n.obs=6572, 
                      nfactor=14, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 15 factors


EATQ_EFA_two_15 <- fa(r=EATQ.corr$correlations, 
                      n.obs=6572, 
                      nfactor=15, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 16 factors


EATQ_EFA_two_16 <- fa(r=EATQ.corr$correlations, 
                      n.obs=6572, 
                      nfactor=16, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 17 factors


EATQ_EFA_two_17 <- fa(r=EATQ.corr$correlations, 
                      n.obs=6572, 
                      nfactor=17, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 18 factors


EATQ_EFA_two_18 <- fa(r=EATQ.corr$correlations, 
                      n.obs=6572, 
                      nfactor=18, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 19 factors


EATQ_EFA_two_19 <- fa(r=EATQ.corr$correlations, 
                      n.obs=6572, 
                      nfactor=19, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###RMSEA fit, TLI does not fit: try 20 factors


EATQ_EFA_two_20 <- fa(r=EATQ.corr$correlations, 
                      n.obs=6572, 
                      nfactor=20, 
                      cor="poly", 
                      fm="mle", 
                      rotate = "oblimin")

###Okay, that was unsuccessful. We're going to change tactics:
###Maybe these variables are binary. Let's look at the distributions.

EATQ_complete %>% 
  select_if(is.numeric) %>%    
  gather() %>%    
  ggplot(aes(value))+ 
  geom_density()+ 
  facet_wrap(~colnames(EATQ_complete))

##There seems to be a natural split between 3 and 4. Let's recode
##the variables to be binary

EATQ_binary <- EATQ_complete %>%
  mutate_all(~ case_when(
    . == 1 ~ 0, 
    . == 2 ~ 0, 
    . == 3 ~ 0, 
    . == 4 ~ 1, 
    . == 5 ~ 1))

###Now I'm going to go over to MPlus to a WLSMV EFA. 
###First, let me save this new data.

write.csv(EATQ_binary, "G:\\My Drive\\ABCD\\EATQ_binary.csv")

###The MPlus WLSMV EFA for binary data suggests 7 factors.
###Here's the relevant output.

#RMSEA (Root Mean Square Error Of Approximation)

#Estimate                           0.019
#90 Percent C.I.                    0.018  0.019
#Probability RMSEA <= .05           1.000

#CFI/TLI

#CFI                                0.965
#TLI                                0.955

OBLIMIN ROTATED LOADINGS (* significant at 5% level)
                 1             2             3             4             5
              ________      ________      ________      ________      ________
#TBL            0.194*        0.036         0.089*       -0.008         0.055*
 # INS            0.702*        0.044*       -0.075*        0.044*       -0.034*
  #FIN            0.060*        0.667*        0.057        -0.052*        0.007
#AFR           -0.160*       -0.059*        0.357*        0.383*       -0.032
#DEL           -0.032         0.208*        0.354*        0.121*       -0.108*
#  TRN           -0.238*        0.227*        0.063*        0.098*        0.237*
 # ENJ            0.115*        0.004        -0.139*       -0.052*        0.141*
#  OPN           -0.052         0.122*        0.286*        0.158*        0.122*
 # SKI            0.069*        0.064*       -0.153*        0.663*        0.001
#CRY            0.063*       -0.009         0.147*       -0.071*       -0.048*
 # ANG            0.632*       -0.013        -0.070*        0.049*       -0.008
#CAR           -0.214*        0.118*        0.562*        0.082*        0.001
#SHR           -0.083*        0.086*        0.505*        0.100*       -0.158*
 # BFR           -0.188*        0.533*       -0.035         0.087*        0.035
#CON           -0.006         0.468*        0.239*       -0.025        -0.045*
 # CTY           -0.042         0.003         0.163*        0.396*        0.002
#RGT           -0.289*        0.503*        0.170*        0.054*       -0.003
#TME           -0.007         0.008         0.599*        0.187*       -0.016
#RDE            0.636*       -0.003        -0.099*        0.037        -0.023
#ANY            0.415*        0.021         0.132*       -0.045*       -0.022
#IRR            0.506*       -0.051*        0.247*       -0.095*       -0.019
#DIS           -0.016         0.362*        0.098*        0.068*        0.035
#IMP            0.091*        0.131*        0.221*        0.147*        0.123*
 # HUG           -0.031         0.074*        0.473*        0.147*       -0.055
#BLM            0.542*       -0.209*        0.104*        0.017        -0.019
#SAD            0.020        -0.057*        0.016         0.020        -0.020
#SOC            0.045*        0.044*       -0.156*       -0.028         0.652*
 # SEA            0.074*       -0.014         0.057         0.700*       -0.004
#TRV           -0.101*       -0.019         0.350*        0.562*       -0.028
#WOR            0.067*        0.155*        0.228*       -0.057*        0.122*
 # PLC            0.586*       -0.108*        0.278*       -0.010         0.032
#SLM            0.582*       -0.125*        0.149*        0.006         0.046*
 # HRD            0.136*        0.012         0.021         0.017         0.091*
  #RCE            0.145*       -0.079*        0.095*        0.525*        0.014
#FOC           -0.087*        0.598*       -0.006         0.061*       -0.006
#HW             0.043*        0.808*        0.013        -0.006         0.006
#EXC            0.165*        0.137*        0.567*        0.267*        0.025
#STT           -0.014         0.928*       -0.054*        0.032*       -0.025*
 # PER            0.000         0.671*        0.142*        0.023        -0.050*
#  ENG            0.120*        0.090*        0.234*        0.321*       -0.252*
 # FUN            0.720*        0.068*       -0.226*        0.127*        0.020
#CRT            0.612*        0.072*       -0.176*        0.069*        0.026
#CLS            0.035         0.016         0.687*        0.137*       -0.068*
 # SHY           -0.046*       -0.036*        0.097*        0.029*        0.958*
#  IEJ            0.509*       -0.149*        0.477*       -0.080*        0.136*
 # PUT           -0.017         0.862*       -0.108*        0.030        -0.002
#LGH            0.011         0.046         0.366*        0.020         0.095*
 # ATT            0.084*        0.086*        0.252*       -0.004         0.149*
  #SDE            0.019         0.619*        0.070*       -0.060*        0.052*
  #NSY           -0.037*       -0.015        -0.030*        0.041*        0.967*
  #FRI            0.060         0.095*        0.705*        0.050         0.007
#SMS            0.067*        0.006        -0.063*        0.013         0.099*
 # BAL           -0.086*        0.063*        0.101*       -0.209*        0.077*
#  MET            0.097*       -0.004        -0.206*       -0.071*        0.601*
 # DRK            0.057*        0.013         0.219*       -0.147*        0.163*
  #SRD            0.076*        0.080*       -0.084*        0.486*        0.057*
  #DSA            0.535*       -0.068*        0.260*       -0.111*       -0.030
#FRU            0.261*        0.132*        0.296*       -0.118*        0.110*
 # STK            0.053         0.439*        0.448*       -0.081*        0.056*
  #CLA           -0.070*        0.509*        0.386*       -0.037         0.046*
  #ALN            0.085*        0.120*        0.141*       -0.140*        0.184*
  #SMT            0.045*        0.030         0.020        -0.054*        0.797*
  
  
#  OBLIMIN ROTATED LOADINGS (* significant at 5% level)
 #                6             7
  #            ________      ________
#TBL            0.245*       -0.192*
 # INS            0.100*       -0.015
#FIN           -0.130*        0.258*
 # AFR            0.170*        0.053
#DEL           -0.183*       -0.064*
 # TRN           -0.066*        0.256*
#  ENJ            0.567*       -0.029
#OPN            0.022         0.318*
#  SKI           -0.060*        0.111*
 # CRY            0.505*       -0.129*
  #ANG            0.095*       -0.036
#CAR            0.011        -0.132*
 # SHR           -0.023        -0.038
#BFR            0.046         0.054*
 # CON            0.039         0.182*
#  CTY            0.114*       -0.053*
 # RGT            0.014         0.029
#TME            0.105*        0.032
#RDE            0.205*        0.048*
#  ANY            0.348*        0.084*
 # IRR            0.221*        0.095*
  #DIS           -0.156*        0.331*
  #IMP           -0.036         0.370*
  #HUG            0.000        -0.152*
  #BLM            0.120*        0.027
#SAD            0.810*        0.026
#SOC            0.025         0.115*
 # SEA           -0.067*       -0.014
#TRV            0.122*       -0.047
#WOR            0.172*       -0.453*
 # PLC           -0.031        -0.058*
  #SLM            0.005         0.002
#HRD            0.385*        0.082*
 # RCE           -0.030        -0.003
#FOC           -0.032         0.249*
 # HW             0.006        -0.127*
  #EXC           -0.085*       -0.064
#STT            0.031        -0.171*
 # PER           -0.060*        0.073*
  #ENG           -0.181*       -0.182*
  #FUN           -0.014        -0.100*
  #CRT            0.025         0.029
#CLS            0.116*        0.131*
 # SHY           -0.022        -0.082*
  #IEJ           -0.037         0.067
#PUT           -0.025        -0.077*
 # LGH           -0.011         0.101*
#  ATT            0.196*       -0.354*
 # SDE           -0.067*        0.326*
  #NSY           -0.041*       -0.033*
  #FRI           -0.129*       -0.142*
  #SMS            0.726*       -0.046*
  #BAL            0.323*       -0.110*
  #MET            0.202*        0.217*
  #DRK            0.089*       -0.344*
  #SRD            0.023         0.099*
  #DSA            0.113*       -0.014
#FRU            0.144*       -0.123*
 # STK           -0.044         0.091*
  #CLA           -0.064*        0.149*
  #ALN            0.098*       -0.370*
  #SMT            0.081*        0.003

###Notice that some of the items don't load on anything.
###I reran without those items and got 6 factors.                 
                 
#RMSEA (Root Mean Square Error Of Approximation)
                 
               
                 #  Estimate                           0.023
                 #90 Percent C.I.                    0.022  0.023
                 #Probability RMSEA <= .05           1.000
                 
                # CFI/TLI
                 
                # CFI                                0.964
                # TLI                                0.953                 
                 
              #   OBLIMIN ROTATED LOADINGS (* significant at 5% level)
               #                   1             2             3             4             5
                #               ________      ________      ________      ________      ________
                # INS            0.720*        0.034         0.030        -0.102*       -0.052*
                 #  FIN            0.101*        0.728*       -0.008        -0.035         0.048*
                  # ENJ            0.202*       -0.025        -0.051*       -0.152*        0.190*
                   #SKI            0.000         0.049*        0.694*       -0.112*        0.009
                 #CRY            0.158*       -0.050*       -0.083*        0.164*       -0.012
                 #ANG            0.650*       -0.021         0.032        -0.081*       -0.030
                 #CAR           -0.196*        0.120*        0.087*        0.589*       -0.012
                 #SHR           -0.043         0.107*        0.100*        0.494*       -0.160*
                  # CON            0.083*        0.521*        0.004         0.156*        0.006
                 #RGT           -0.276*        0.497*        0.071*        0.172*        0.020
                 #TME            0.086*        0.047         0.187*        0.563*        0.011
                 #RDE            0.687*        0.000         0.036        -0.141*       -0.013
                 #ANY            0.534*        0.040*       -0.016         0.064*        0.023
                 #IRR            0.620*       -0.013        -0.065*        0.160*        0.016
                 #HUG           -0.043         0.061*        0.137*        0.520*       -0.083*
                  # BLM            0.598*       -0.195*        0.023         0.060*       -0.010
                 #SAD            0.197*       -0.086*        0.024        -0.003         0.079*
                  # SOC            0.057*        0.061*       -0.029        -0.202*        0.671*
                   #SEA            0.000        -0.035*        0.748*        0.135*       -0.008
                 #TRV           -0.022         0.016         0.444*        0.371*       -0.036
                 #WOR           -0.008         0.059*       -0.109*        0.375*        0.041*
                  # PLC            0.598*       -0.088*       -0.002         0.261*        0.002
                 #SLM            0.606*       -0.105*        0.008         0.121*        0.024
                 #RCE            0.114*       -0.073*        0.530*        0.126*        0.003
                 #FOC           -0.048*        0.636*        0.073*       -0.075*        0.037*
                  # HW            -0.005         0.803*       -0.030         0.034        -0.031
                # STT           -0.082*        0.901*        0.011        -0.004        -0.058*
                #   PER            0.008         0.702*        0.032         0.105*       -0.033
                # FUN            0.652*        0.042*        0.094*       -0.199*       -0.034
                # CRT            0.599*        0.066*        0.060*       -0.197*        0.009
                # CLS            0.175*        0.090*        0.171*        0.610*       -0.017
                # SHY           -0.080*       -0.035*        0.021         0.128*        0.950*
                 #  IEJ            0.569*       -0.095*       -0.023         0.399*        0.135*
                  # PUT           -0.079*        0.844*        0.017        -0.073*       -0.032*
                  # LGH            0.065*        0.094*        0.053         0.308*        0.110*
                  # ATT            0.030         0.016        -0.036         0.366*        0.088*
                  # SDE            0.083*        0.683*       -0.009        -0.052*        0.107*
                  # NSY           -0.082*       -0.022         0.028*       -0.001         0.958*
                  # FRI            0.060         0.118*        0.065*        0.711*       -0.027
                # SMS            0.180*       -0.035*        0.021        -0.061*        0.161*
                 #  BAL           -0.024         0.041        -0.244*        0.111*        0.085*
                  # MET            0.167*        0.026        -0.032        -0.286*        0.651*
                   #DRK            0.010        -0.047*       -0.209*        0.317*        0.087*
                   #SRD            0.047         0.077*        0.502*       -0.064*        0.067*
                   #DSA            0.602*       -0.052*       -0.107*        0.216*       -0.028
                 #FRU            0.298*        0.120*       -0.109*        0.303*        0.105*
                  # STK            0.118*        0.502*       -0.031         0.366*        0.086*
                  # CLA           -0.009         0.570*        0.016         0.310*        0.085*
                   #ALN            0.020         0.036        -0.191*        0.261*        0.114*
                   #SMT            0.059*        0.043*       -0.055*        0.001         0.806*
                   
                   
                   #OBLIMIN ROTATED LOADINGS (* significant at 5% level)
                                #   6
                              #  ________
            #     INS            0.060*
             #      FIN           -0.245*
              #     ENJ            0.499*
               #    SKI           -0.021
                # CRY            0.442*
                 #  ANG            0.083*
                  # CAR            0.057
            #     SHR           -0.026
             #    CON           -0.098*
              #     RGT            0.004
               #  TME            0.023
                # RDE            0.142*
                 #  ANY            0.211*
                  # IRR            0.082*
                   #HUG            0.083*
    #               BLM            0.040
     #            SAD            0.622*
      #             SOC           -0.033
       #          SEA            0.012
        #         TRV            0.068*
         #          WOR            0.386*
          #         PLC           -0.023
           #      SLM           -0.023
            #     RCE            0.014
             #    FOC           -0.139*
              #     HW             0.116*
               #    STT            0.168*
                #   PER           -0.075*
                 #  FUN            0.079*
                  # CRT            0.013
#                 CLS           -0.033
 #                SHY            0.022
  #               IEJ           -0.101*
   #                PUT            0.077*
    #               LGH           -0.068*
     #              ATT            0.373*
      #             SDE           -0.210*
       #            NSY           -0.005
        #         FRI           -0.062
         #        SMS            0.652*
          #         BAL            0.320*
           #        MET            0.066*
            #       DRK            0.251*
             #      SRD            0.024
              #   DSA            0.047*
               #    FRU            0.137*
                #   STK           -0.116*
                 #  CLA           -0.146*
                  # ALN            0.269*
  #                 SMT            0.062*
                   
###I still had things that didn't load, so here we go again!
###6 factors
                 
                 #RMSEA (Root Mean Square Error Of Approximation)
                 
                # Estimate                           0.023
                # 90 Percent C.I.                    0.022  0.024
                # Probability RMSEA <= .05           1.000
                 
                 #CFI/TLI
                 
                 #CFI                                0.973
                 #TLI                                0.963
                 
                 #OBLIMIN ROTATED LOADINGS (* significant at 5% level)
                #                   1             2             3             4             5
                #                ________      ________      ________      ________      ________
                 #INS            0.708*        0.034         0.023        -0.086*       -0.053*
                  # FIN            0.119*        0.710*       -0.004        -0.024         0.051*
                   #ENJ            0.158*        0.001        -0.060*       -0.142*        0.183*
                   #SKI            0.010         0.038*        0.716*       -0.123*        0.001
                 #CRY            0.102*       -0.027        -0.106*        0.195*       -0.013
                 #ANG            0.637*       -0.022         0.031        -0.070*       -0.029
                 #CAR           -0.184*        0.119*        0.064*        0.588*        0.009
                 #SHR           -0.050*        0.102*        0.078*        0.519*       -0.149*
                  # CON            0.069*        0.514*       -0.012         0.188*        0.004
                 #RGT           -0.274*        0.497*        0.063*        0.177*        0.022
                 #TME            0.057         0.048         0.143*        0.609*        0.019
                 #RDE            0.660*        0.001         0.020        -0.102*       -0.015
                 #ANY            0.481*        0.052*       -0.046*        0.106*        0.015
                 #IRR            0.568*       -0.009        -0.090*        0.202*        0.012
                 #HUG           -0.031         0.061*        0.120*        0.523*       -0.065*
                #   BLM            0.571*       -0.195*        0.015         0.084*       -0.012
                # SAD            0.104*       -0.052*       -0.013         0.062*        0.066*
                #   SOC            0.076*        0.056*       -0.009        -0.223*        0.672*
                #   SEA            0.000        -0.044*        0.743*        0.144*       -0.016
                # TRV           -0.028         0.012         0.436*        0.382*       -0.032
                # PLC            0.611*       -0.098*       -0.001         0.256*        0.020
                # SLM            0.600*       -0.111*        0.006         0.128*        0.030
                # RCE            0.113*       -0.075*        0.522*        0.132*       -0.002
                # FOC           -0.030         0.627*        0.077*       -0.078*        0.033
                # HW            -0.008         0.807*       -0.030         0.037        -0.028
                # STT           -0.093*        0.906*        0.010         0.007        -0.056*
                 #  PER            0.015         0.696*        0.021         0.114*       -0.030
                 #FUN            0.682*        0.037         0.123*       -0.232*       -0.027
                 #CRT            0.610*        0.061*        0.072*       -0.206*        0.011
                 #CLS            0.124*        0.091*        0.111*        0.681*       -0.017
                 #SHY           -0.088*       -0.034*        0.000         0.146*        0.946*
                  # IEJ            0.564*       -0.102*       -0.034         0.403*        0.145*
                  # PUT           -0.076*        0.844*        0.031        -0.079*       -0.030*
                  # SDE            0.093*        0.667*       -0.004        -0.044         0.105*
                  # NSY           -0.077*       -0.023*        0.010         0.014         0.950*
                  # FRI            0.063*        0.115*        0.045         0.705*       -0.008
                 #SMS            0.118*       -0.001         0.000        -0.040*        0.153*
                  # MET            0.158*        0.027        -0.016        -0.290*        0.642*
                   #SRD            0.052*        0.068*        0.507*       -0.055*        0.063*
                   #DSA            0.575*       -0.055*       -0.096*        0.212*       -0.022
                # STK            0.117*        0.493*       -0.043         0.373*        0.094*
                 #  CLA           -0.008         0.563*       -0.004         0.322*        0.089*
                  # SMT            0.070*        0.041*       -0.027        -0.032         0.811*
                   
                   
                   #OBLIMIN ROTATED LOADINGS (* significant at 5% level)
                                 #6
                              #________
                 #INS            0.083*
                  # FIN           -0.258*
                   #ENJ            0.531*
                   #SKI           -0.048*
                   #CRY            0.491*
                   #ANG            0.099*
                   #CAR            0.025
                 #SHR           -0.022
                 #CON           -0.068*
                  # RGT           -0.007
                 #TME            0.066
                 #RDE            0.175*
                  # ANY            0.282*
                   #IRR            0.163*
                   #HUG            0.052
                # BLM            0.089*
                 #  SAD            0.720*
                  # SOC           -0.051*
                   #SEA            0.000
                 #TRV            0.060*
                  # PLC           -0.041
                 #SLM           -0.007
                 #RCE            0.012
                 #FOC           -0.156*
                  # HW             0.107*
                   #STT            0.163*
                   #PER           -0.085*
                   #FUN            0.029
                # CRT            0.000
                # CLS            0.055
                # SHY            0.047*
                 #  IEJ           -0.075*
                  # PUT            0.056*
                   #SDE           -0.211*
                   #NSY            0.005
                 #FRI           -0.067
                 #SMS            0.693*
                  # MET            0.085*
                  # SRD            0.003
                 #DSA            0.084*
                  # STK           -0.109*
                   #CLA           -0.137*
                   #SMT            0.047*
                   
###That's a good solution with simple structure.                   
                 
###I want to make a factor diagram. I made an excel 
###speadsheet of the factor loading matrix. Let's
###load it. 
                 
factor_loading <- read.csv("G:\\My Drive\\ABCD\\Portfolio_3\\final_factor_loading.csv", header = FALSE, stringsAsFactors = FALSE)

###There's a weird symbol in the first box. Let's fix that!

factor_loading[1,1] <- 0.708

###Now let's make it a matrix
factor_loading_mtrx <- data.matrix(factor_loading, rownames.force = NA)
          
                        
EFA_diagram <- fa.diagram(factor_loading_mtrx, simple= TRUE, col = "blue",
                          labels= c("ins", "fin", "enj",
                                    "ski", "cry", "ang", "car", "shr",
                                    "con", "rgt", "tme", "rde",
                                    "any", "irr", "hug", "blm",
                                    "sad", "soc", "sea", "trv", "plc",
                                    "slm", "rce", "foc", "hw", 
                                    "stt", "per", "fun", "crt", "cls",
                                    "shy", "iej", "put", "sde",
                                    "nsy", "fri", "sms", "met", 
                                    "srd", "dsa", "stk", "cla", "smt"),
                          digits = 3, e.size = .05, rsize = .05, cex = .5)

###So, fa.diagram produces an ugly factor diagram. I'm just going to 
###make myself one in another program. I'll put the factors here, though.
###Aggression/Fear/Frustration, Attention and Activation/Inhibitory 
###Control, Surgency, Affiliation, Shyness, and Depressive Mood.


###Let's do the EGA now! First
###we need to eliminate the variables that we eliminated via EFA.


EATQ_binary_rev <- subset(EATQ_binary, select = -c(tbl, afr, del, trn, opn, bfr, cty, dis, 
                                                   imp, hrd, exc, eng,
                                                   wor, lgh, att, bal,
                                                   drk, fru, aln, rce))


EATQ_EGA <- EGA(EATQ_binary_rev, uni = TRUE, model = c("glasso"), algorithm = 
                c("walktrap"), plot.EGA = FALSE, verbose = TRUE)

EATQ_plot <- plot(EATQ_EGA, plot.args = list(vsize = 6, alpha = 0.5, edge.alpha = 0.5,  
                                           color.palette = "rainbow", label.size = 3, legend.position="bottom")) 

###Now let's try to add the factor names to the graph.

EATQ_plot <- plot(EATQ_EGA, plot.args = list(vsize = 11, alpha = 0.5, edge.alpha = 0.7,  
                                             color.palette = "rainbow", label.size = 5, 
                                             legend.names = c("Shyness", "Affiliation", 
                                                              "Attention and Activation/Inhibitory Control",
                                                              "Aggression/Fear/Frustration", 
                                                              "Surgency", "Depressive Mood"),  
                                              legend.position="bottom")) 

###Okay, so we see we have the same 6 factors for EFA and EGA. There are some
###items that differ (e.g., trv is in Surgency in the factor model,
###but in Affiliation in the EGA), but otherwise they're basically the same.
###This lends credence to the fact that the EATQ-R Parent's Questionnaire
###has 6 factors, though there are 10 subscales.

###...and now I have to go turn this into a poster.