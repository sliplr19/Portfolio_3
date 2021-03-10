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
