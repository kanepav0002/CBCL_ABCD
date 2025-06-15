library(MplusAutomation)
library(tidyverse)
library(ggplot2)


setwd('/home/kanep/kg98_scratch/Kane/behaviour_denoise/')

# Load in mental health data
# Child Behaviour Checklist
CBCL <- read.csv('abcd-data-release-5.1/core/mental-health/mh_p_cbcl.csv')

# CBCL raw vs subscale scores at time 1
subscale_columns <- c('cbcl_scr_syn_anxdep_r', 'cbcl_scr_syn_withdep_r', 'cbcl_scr_syn_somatic_r', 'cbcl_scr_syn_social_r', 
                      'cbcl_scr_syn_thought_r', 'cbcl_scr_syn_attention_r', 'cbcl_scr_syn_rulebreak_r', 'cbcl_scr_syn_aggressive_r')
CBCL_time1 <- CBCL[CBCL$eventname == "baseline_year_1_arm_1", ]
CBCL_time2 <- CBCL[CBCL$eventname == "2_year_follow_up_y_arm_1", ]
CBCL_t1_raw <- CBCL_time1[,c(4:122)]
CBCL_t2_raw <- CBCL_time2[,c(4:122)]
CBCL_t1_subc <- CBCL_time1[,subscale_columns]
CBCL_t2_subc <- CBCL_time2[,subscale_columns]

CBCL_t1_subc_log <-CBCL_t1_subc+1
CBCL_t1_subc_log <- log(CBCL_t1_subc_log)
###################################################################################################
# Get data ready for Mplus 
###################################################################################################

CBCL_t1_raw[is.na(CBCL_t1_raw)] <- -999 # Mplus doesn't do NA so need to specify missing as -999
CBCL_t1_subc[is.na(CBCL_t1_subc)] <- -999
CBCL_t1_subc_log[is.na(CBCL_t1_subc_log)] <- -999


prepareMplusData(CBCL_t1_raw, filename="mplus_mixmodels/Data/Anxious_dep.dat",
                 keepCols = c("cbcl_q14_p", "cbcl_q29_p", "cbcl_q30_p", "cbcl_q31_p", "cbcl_q32_p", "cbcl_q33_p", "cbcl_q35_p", "cbcl_q45_p", "cbcl_q50_p", "cbcl_q52_p", "cbcl_q71_p", "cbcl_q91_p", "cbcl_q112_p"))
prepareMplusData(CBCL_t1_raw, filename="mplus_mixmodels/Data/Withdrawn_dep.dat",
                 keepCols = c("cbcl_q05_p", "cbcl_q42_p", "cbcl_q65_p", "cbcl_q69_p", "cbcl_q75_p", "cbcl_q102_p", "cbcl_q103_p", "cbcl_q111_p"))
prepareMplusData(CBCL_t1_raw, filename="mplus_mixmodels/Data/Somatic.dat",
                 keepCols = c("cbcl_q47_p", "cbcl_q49_p", "cbcl_q51_p", "cbcl_q54_p", "cbcl_q56a_p", "cbcl_q56b_p", "cbcl_q56c_p", "cbcl_q56d_p", "cbcl_q56e_p", "cbcl_q56f_p", "cbcl_q56g_p"))
prepareMplusData(CBCL_t1_raw, filename="mplus_mixmodels/Data/Social_Problems.dat",
                 keepCols = c("cbcl_q11_p", "cbcl_q12_p", "cbcl_q25_p", "cbcl_q27_p", "cbcl_q34_p", "cbcl_q36_p", "cbcl_q38_p", "cbcl_q48_p", "cbcl_q62_p", "cbcl_q64_p", "cbcl_q79_p"))
prepareMplusData(CBCL_t1_raw, filename="mplus_mixmodels/Data/Thought_Problems.dat",
                 keepCols = c("cbcl_q09_p", "cbcl_q18_p", "cbcl_q40_p", "cbcl_q46_p", "cbcl_q58_p", "cbcl_q59_p", "cbcl_q60_p", "cbcl_q66_p", "cbcl_q70_p", "cbcl_q76_p", "cbcl_q83_p", "cbcl_q84_p", "cbcl_q85_p", "cbcl_q92_p", "cbcl_q100_p"))
prepareMplusData(CBCL_t1_raw, filename="mplus_mixmodels/Data/Attention_Problems.dat",
                 keepCols = c("cbcl_q01_p", "cbcl_q04_p", "cbcl_q07_p", "cbcl_q08_p", "cbcl_q10_p", "cbcl_q13_p", "cbcl_q17_p", "cbcl_q41_p", "cbcl_q61_p", "cbcl_q78_p", "cbcl_q80_p", "cbcl_q93_p", "cbcl_q109_p"))
prepareMplusData(CBCL_t1_raw, filename="mplus_mixmodels/Data/Rule_Breaking.dat",
                 keepCols = c("cbcl_q02_p", "cbcl_q26_p", "cbcl_q28_p", "cbcl_q39_p", "cbcl_q43_p", "cbcl_q63_p", "cbcl_q67_p", "cbcl_q72_p", "cbcl_q73_p", "cbcl_q81_p", "cbcl_q82_p", "cbcl_q90_p", "cbcl_q96_p", "cbcl_q98_p", "cbcl_q99_p", "cbcl_q101_p", "cbcl_q105_p", "cbcl_q106_p"))
prepareMplusData(CBCL_t1_raw, filename="mplus_mixmodels/Data/Aggression.dat",
                 keepCols = c("cbcl_q03_p", "cbcl_q16_p", "cbcl_q19_p", "cbcl_q20_p", "cbcl_q21_p", "cbcl_q22_p", "cbcl_q23_p", "cbcl_q37_p", "cbcl_q57_p", "cbcl_q68_p", "cbcl_q86_p", "cbcl_q87_p", "cbcl_q88_p", "cbcl_q89_p", "cbcl_q94_p", "cbcl_q95_p", "cbcl_q97_p", "cbcl_q104_p"))
prepareMplusData(CBCL_t1_raw, filename="mplus_mixmodels/Data/All_items.dat",
                 keepCols = c('cbcl_q14_p', 'cbcl_q29_p', 'cbcl_q30_p', 'cbcl_q31_p', 'cbcl_q32_p', 'cbcl_q33_p', 'cbcl_q35_p', 'cbcl_q45_p', 'cbcl_q50_p', 'cbcl_q52_p', 'cbcl_q71_p', 'cbcl_q91_p', 'cbcl_q112_p',
                              'cbcl_q05_p', 'cbcl_q42_p', 'cbcl_q65_p', 'cbcl_q69_p', 'cbcl_q75_p', 'cbcl_q102_p', 'cbcl_q103_p',  'cbcl_q111_p',
                              'cbcl_q11_p', 'cbcl_q12_p', 'cbcl_q25_p', 'cbcl_q27_p', 'cbcl_q34_p', 'cbcl_q36_p', 'cbcl_q38_p', 'cbcl_q48_p', 'cbcl_q62_p', 'cbcl_q64_p', 'cbcl_q79_p',
                              'cbcl_q47_p', 'cbcl_q49_p', 'cbcl_q51_p', 'cbcl_q54_p', 'cbcl_q56a_p', 'cbcl_q56b_p', 'cbcl_q56c_p', 'cbcl_q56d_p', 'cbcl_q56e_p', 'cbcl_q56f_p', 'cbcl_q56g_p',
                              'cbcl_q09_p', 'cbcl_q18_p', 'cbcl_q40_p', 'cbcl_q46_p', 'cbcl_q58_p', 'cbcl_q59_p', 'cbcl_q60_p', 'cbcl_q66_p', 'cbcl_q70_p', 'cbcl_q76_p', 'cbcl_q83_p', 'cbcl_q84_p', 'cbcl_q85_p', 'cbcl_q92_p', 'cbcl_q100_p',
                              'cbcl_q01_p', 'cbcl_q04_p', 'cbcl_q07_p', 'cbcl_q08_p', 'cbcl_q10_p', 'cbcl_q13_p', 'cbcl_q17_p', 'cbcl_q41_p', 'cbcl_q61_p', 'cbcl_q78_p', 'cbcl_q80_p', 'cbcl_q93_p', 'cbcl_q109_p',
                              'cbcl_q02_p', 'cbcl_q26_p', 'cbcl_q28_p', 'cbcl_q39_p', 'cbcl_q43_p', 'cbcl_q63_p', 'cbcl_q67_p', 'cbcl_q72_p', 'cbcl_q73_p', 'cbcl_q81_p', 'cbcl_q82_p', 'cbcl_q90_p', 'cbcl_q96_p', 'cbcl_q98_p', 'cbcl_q99_p', 'cbcl_q101_p', 'cbcl_q105_p', 'cbcl_q106_p',
                              'cbcl_q03_p', 'cbcl_q16_p', 'cbcl_q19_p', 'cbcl_q20_p', 'cbcl_q21_p', 'cbcl_q22_p', 'cbcl_q23_p', 'cbcl_q37_p', 'cbcl_q57_p', 'cbcl_q68_p', 'cbcl_q86_p', 'cbcl_q87_p', 'cbcl_q88_p', 'cbcl_q89_p', 'cbcl_q94_p', 'cbcl_q95_p', 'cbcl_q97_p', 'cbcl_q104_p'))
prepareMplusData(CBCL_t1_subc, filename="mplus_mixmodels/Data/subscale.dat",
                 keepCols = c("cbcl_scr_syn_anxdep_r", "cbcl_scr_syn_withdep_r", "cbcl_scr_syn_somatic_r", "cbcl_scr_syn_social_r", "cbcl_scr_syn_thought_r", "cbcl_scr_syn_attention_r", "cbcl_scr_syn_rulebreak_r", "cbcl_scr_syn_aggressive_r"))
                 
prepareMplusData(CBCL_t1_subc_log, filename="mplus_mixmodels/Data/subscale_log.dat",
                 keepCols = c("cbcl_scr_syn_anxdep_r", "cbcl_scr_syn_withdep_r", "cbcl_scr_syn_somatic_r", "cbcl_scr_syn_social_r", "cbcl_scr_syn_thought_r", "cbcl_scr_syn_attention_r", "cbcl_scr_syn_rulebreak_r", "cbcl_scr_syn_aggressive_r"))


