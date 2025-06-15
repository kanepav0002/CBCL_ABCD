library(lavaan)
library(tidyverse)
library(semTools)
library(dplyr)

CBCL <- read.csv('/home/kanep/kg98_scratch/Kane/behaviour_denoise/abcd-data-release-5.1/core/mental-health/mh_p_cbcl.csv')

# CBCL raw vs subscale scores at time 1
subscale_columns <- c('cbcl_scr_syn_anxdep_r', 'cbcl_scr_syn_withdep_r', 'cbcl_scr_syn_somatic_r', 'cbcl_scr_syn_social_r', 
                      'cbcl_scr_syn_thought_r', 'cbcl_scr_syn_attention_r', 'cbcl_scr_syn_rulebreak_r', 'cbcl_scr_syn_aggressive_r')
CBCL_time1 <- CBCL[CBCL$eventname == "baseline_year_1_arm_1", ]
CBCL_t1_raw <- CBCL_time1[,c(4:122)]
CBCL_t1_subc <- CBCL_time1[,subscale_columns]

CBCL_subc_renamed <- CBCL_t1_subc
colnames(CBCL_subc_renamed) <- c("ANXDEP", "WITHDEP", "SOMATIC", "SOCIAL", "THOUGHT", "ATTENT", "RULE_BREAK", "AGGRESS")
CBCL_subc_renamed_log <- CBCL_subc_renamed+1
CBCL_subc_renamed_log <- log(CBCL_subc_renamed_log)

####################################################################################
# Rename item data to subscales
####################################################################################
CBCL_raw_renamed <- CBCL_t1_raw

# ANXIOUS DEPRESSED
new_column_names <- paste0("ANXDEP", 1:13)
old_column_names <- c("cbcl_q14_p", "cbcl_q29_p", "cbcl_q30_p", "cbcl_q31_p", 
                      "cbcl_q32_p", "cbcl_q33_p", "cbcl_q35_p", "cbcl_q45_p", 
                      "cbcl_q50_p", "cbcl_q52_p", "cbcl_q71_p", "cbcl_q91_p", 
                      "cbcl_q112_p")
colnames(CBCL_raw_renamed)[which(colnames(CBCL_raw_renamed) %in% old_column_names)] <- new_column_names
# WITHDRAWN DEPRESSED
new_withdep_names <- paste0("WITHDEP", 1:8)
old_withdep_names <- c("cbcl_q05_p", "cbcl_q42_p", "cbcl_q65_p", "cbcl_q69_p", 
                       "cbcl_q75_p", "cbcl_q102_p", "cbcl_q103_p", "cbcl_q111_p")
colnames(CBCL_raw_renamed)[which(colnames(CBCL_raw_renamed) %in% old_withdep_names)] <- new_withdep_names
# SOMATIC
new_somatic_names <- paste0("SOMATIC", 1:11)

old_somatic_names <- c("cbcl_q47_p", "cbcl_q49_p", "cbcl_q51_p", "cbcl_q54_p", 
                       "cbcl_q56a_p", "cbcl_q56b_p", "cbcl_q56c_p", "cbcl_q56d_p", 
                       "cbcl_q56e_p", "cbcl_q56f_p", "cbcl_q56g_p")
colnames(CBCL_raw_renamed)[which(colnames(CBCL_raw_renamed) %in% old_somatic_names)] <- new_somatic_names
# SOCIAL PROBLEMS
new_social_names <- paste0("SOCIAL", 1:11)
old_social_names <- c("cbcl_q11_p", "cbcl_q12_p", "cbcl_q25_p", "cbcl_q27_p", 
                      "cbcl_q34_p", "cbcl_q36_p", "cbcl_q38_p", "cbcl_q48_p", 
                      "cbcl_q62_p", "cbcl_q64_p", "cbcl_q79_p")
colnames(CBCL_raw_renamed)[which(colnames(CBCL_raw_renamed) %in% old_social_names)] <- new_social_names
# THOUGHT PROBLEMS 
new_thought_names <- paste0("THOUGHT", 1:15)
old_thought_names <- c("cbcl_q09_p", "cbcl_q18_p", "cbcl_q40_p", "cbcl_q46_p",
                       "cbcl_q58_p", "cbcl_q59_p", "cbcl_q60_p", "cbcl_q66_p",
                       "cbcl_q70_p", "cbcl_q76_p", "cbcl_q83_p", "cbcl_q84_p",
                       "cbcl_q85_p", "cbcl_q92_p", "cbcl_q100_p")
colnames(CBCL_raw_renamed)[which(colnames(CBCL_raw_renamed) %in% old_thought_names)] <- new_thought_names
# ATTENTION PROBLEMS
new_attent_names <- paste0("ATTENT", 1:13)
old_attent_names <- c("cbcl_q01_p", "cbcl_q04_p", "cbcl_q07_p", "cbcl_q08_p", 
                      "cbcl_q10_p", "cbcl_q13_p", "cbcl_q17_p", "cbcl_q41_p", 
                      "cbcl_q61_p", "cbcl_q78_p", "cbcl_q80_p", "cbcl_q93_p", 
                      "cbcl_q109_p")
colnames(CBCL_raw_renamed)[which(colnames(CBCL_raw_renamed) %in% old_attent_names)] <- new_attent_names
# RULE BREAKING
new_rule_br_names <- paste0("RULE_BR", 1:18)
old_rule_br_names <- c("cbcl_q02_p", "cbcl_q26_p", "cbcl_q28_p", "cbcl_q39_p", 
                       "cbcl_q43_p", "cbcl_q63_p", "cbcl_q67_p", "cbcl_q72_p", 
                       "cbcl_q73_p", "cbcl_q81_p", "cbcl_q82_p", "cbcl_q90_p", 
                       "cbcl_q96_p", "cbcl_q98_p", "cbcl_q99_p", "cbcl_q101_p", 
                       "cbcl_q105_p", "cbcl_q106_p")
colnames(CBCL_raw_renamed)[which(colnames(CBCL_raw_renamed) %in% old_rule_br_names)] <- new_rule_br_names
# AGGRESSIVE BEHAVIOUR
new_aggress_names <- paste0("AGGRESS", 1:18)
old_aggress_names <- c("cbcl_q03_p", "cbcl_q16_p", "cbcl_q19_p", "cbcl_q20_p", 
                       "cbcl_q21_p", "cbcl_q22_p", "cbcl_q23_p", "cbcl_q37_p", 
                       "cbcl_q57_p", "cbcl_q68_p", "cbcl_q86_p", "cbcl_q87_p", 
                       "cbcl_q88_p", "cbcl_q89_p", "cbcl_q94_p", "cbcl_q95_p", 
                       "cbcl_q97_p", "cbcl_q104_p")
colnames(CBCL_raw_renamed)[which(colnames(CBCL_raw_renamed) %in% old_aggress_names)] <- new_aggress_names

# Delete columns that don't belong to subscales
CBCL_raw_renamed <- CBCL_raw_renamed[, !grepl("cbcl", colnames(CBCL_raw_renamed))]

########################################################################################################################################################################
                                                              # ITEM LEVEL CFAs.
########################################################################################################################################################################

####################################################################################
# CFA Function.
####################################################################################
run_cfa <- function(model, data, rotation = "oblimin", estimator = "WLSMV", ordered = TRUE) {
  result <- cfa(model = model,
                data = data,
                rotation = rotation,
                estimator = estimator,
                ordered = ordered,
                mimic="Mplus")
  
  return(result)
}
####################################################################################
# do CFA for just internalising.
####################################################################################

internalising_syntax <- 'Intern =~ ANXDEP1 + ANXDEP2 + ANXDEP3 + ANXDEP4 + ANXDEP5 + ANXDEP6 + ANXDEP7 + ANXDEP8 + ANXDEP9 + ANXDEP10 + ANXDEP11 + ANXDEP12 + ANXDEP13
+ WITHDEP1 + WITHDEP2 + WITHDEP3 + WITHDEP4 + WITHDEP5 + WITHDEP6 + WITHDEP7 + WITHDEP8 +
 SOMATIC1 + SOMATIC2 + SOMATIC3 + SOMATIC4 + SOMATIC5 + SOMATIC6 + SOMATIC7 + SOMATIC8 + SOMATIC9 + SOMATIC10 + SOMATIC11'


intern_cfa <- run_cfa(internalising_syntax, CBCL_raw_renamed)

fitmeasures(intern_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))

# parameter estimates
p_est <- parameterEstimates(intern_cfa, standardized = TRUE) # can also put rsquare = TRUE to get r2 here.
standardized_p_est <- standardizedSolution(intern_cfa, type="std.all")
standardized_p_est <- standardized_p_est %>%
  mutate_at(vars(est.std, se, z, pvalue, ci.lower, ci.upper), round, digits = 3)
write.csv(standardized_p_est, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/standardised_est/intern_est.csv')

# Examine Residuals
resids <- lavResiduals(intern_cfa, zstat=TRUE)
resid_cov <- resids$cov
resid_cov <- round(resid_cov,3)
write.csv(resid_cov, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/resid_covs/intern_resid.csv')

# Coefficient of Determination (r-squared)
lavInspect(intern_cfa, "rsquare")
# Average variance extracted
AVE(intern_cfa)

####################################################################################
# do CFA for just externalising.
####################################################################################

externalising_syntax <- 'Extern =~ RULE_BR1 + RULE_BR2 + RULE_BR3  + RULE_BR4 + RULE_BR5 + RULE_BR6 + 
RULE_BR7 + RULE_BR8 + RULE_BR9 + RULE_BR10 + RULE_BR11 + RULE_BR12 + RULE_BR13 + RULE_BR14 + RULE_BR15
+ RULE_BR16 + RULE_BR17 + RULE_BR18 + AGGRESS1 + AGGRESS2 + AGGRESS3 + AGGRESS4 + AGGRESS5 + AGGRESS6 + 
AGGRESS7 + AGGRESS8 + AGGRESS9 + AGGRESS9 + AGGRESS10 + AGGRESS11 + AGGRESS12 + AGGRESS13 + AGGRESS14 + 
AGGRESS15 + AGGRESS16 + AGGRESS17 + AGGRESS18'

extern_cfa <- run_cfa(externalising_syntax, CBCL_raw_renamed)

fitmeasures(extern_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))

# parameter estimates
p_est <- parameterEstimates(extern_cfa, standardized = TRUE) # can also put rsquare = TRUE to get r2 here.
standardized_p_est <- standardizedSolution(extern_cfa, type="std.all")
standardized_p_est <- standardized_p_est %>%
  mutate_at(vars(est.std, se, z, pvalue, ci.lower, ci.upper), round, digits = 3)
write.csv(standardized_p_est, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/standardised_est/extern_est.csv')

# Examine Residuals
resids <- lavResiduals(extern_cfa, zstat=TRUE)
resid_cov <- resids$cov
resid_cov <- round(resid_cov,3)
write.csv(resid_cov, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/resid_covs/extern_resid.csv')

# Coefficient of Determination (r-squared)
lavInspect(extern_cfa, "rsquare")
# Average variance extracted
AVE(extern_cfa)

####################################################################################
# do CFA for just P
####################################################################################

p_syntax <- 'P =~ ANXDEP1 + ANXDEP2 + ANXDEP3 + ANXDEP4 + ANXDEP5 + ANXDEP6
+ ANXDEP7 + ANXDEP7 + ANXDEP8 + ANXDEP9 + ANXDEP10 + ANXDEP11 + ANXDEP12 + ANXDEP13 + 
WITHDEP1 + WITHDEP2 + WITHDEP3 + WITHDEP4 + WITHDEP5 + WITHDEP6 + WITHDEP7 + WITHDEP8 +
SOMATIC1 + SOMATIC2 + SOMATIC3 + SOMATIC4 + SOMATIC5 + SOMATIC6 + SOMATIC7 + SOMATIC7 + 
SOMATIC8 + SOMATIC9 + SOMATIC10 + SOMATIC11 + SOCIAL1  + SOCIAL2 + SOCIAL3 + SOCIAL4 + 
SOCIAL5 + SOCIAL6 + SOCIAL7 + SOCIAL8 + SOCIAL9 + SOCIAL10 + SOCIAL11 + THOUGHT1 + THOUGHT2
 + THOUGHT3 + THOUGHT4 + THOUGHT5 + THOUGHT6 + THOUGHT7 + THOUGHT8 + THOUGHT9 + THOUGHT10 + 
 THOUGHT11 + THOUGHT12 + THOUGHT13 + THOUGHT14 + THOUGHT15 + ATTENT1 + ATTENT2 + ATTENT3 + 
 ATTENT4 + ATTENT5 + ATTENT6 + ATTENT7 + ATTENT8 + ATTENT9 + ATTENT10 + ATTENT11 + RULE_BR1 
 + RULE_BR2 + RULE_BR3  + RULE_BR4 + RULE_BR5 + RULE_BR6 + RULE_BR7 + RULE_BR8 + RULE_BR9 + 
 RULE_BR10 + RULE_BR11 + RULE_BR12 + RULE_BR13 + RULE_BR14 + RULE_BR15 + RULE_BR16 + RULE_BR17 
 + RULE_BR18 + AGGRESS1 + AGGRESS2 + AGGRESS3 + AGGRESS4 + AGGRESS5 + AGGRESS6 + AGGRESS7 + 
 AGGRESS8 + AGGRESS9 + AGGRESS9 + AGGRESS10 + AGGRESS11 + AGGRESS12 + AGGRESS13 + AGGRESS14 + 
AGGRESS15 + AGGRESS16 + AGGRESS17 + AGGRESS18'

p_cfa <- run_cfa(p_syntax, CBCL_raw_renamed)

fitmeasures(p_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))

# parameter estimates
p_est <- parameterEstimates(p_cfa, standardized = TRUE) # can also put rsquare = TRUE to get r2 here.
standardized_p_est <- standardizedSolution(extern_cfa, type="std.all")
standardized_p_est <- standardized_p_est %>%
  mutate_at(vars(est.std, se, z, pvalue, ci.lower, ci.upper), round, digits = 3)
write.csv(standardized_p_est, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/standardised_est/p_est.csv')

# Examine Residuals
resids <- lavResiduals(extern_cfa, zstat=TRUE)
resid_cov <- resids$cov
resid_cov <- round(resid_cov,3)
write.csv(resid_cov, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/resid_covs/p_resid.csv')

# Coefficient of Determination (r-squared)
lavInspect(p_cfa, "rsquare")
# Average variance extracted
AVE(p_cfa)

####################################################################################
# do CFA for Anxious Depressed.
####################################################################################
AnxDep_syntax <- 'ANXDEP =~ ANXDEP1 + ANXDEP2 + ANXDEP3 + ANXDEP4 + ANXDEP5 + ANXDEP6 + ANXDEP7 + ANXDEP8 + ANXDEP9 + ANXDEP10 + ANXDEP11 + ANXDEP12 + ANXDEP13'
AnxDep_cfa <- run_cfa(AnxDep_syntax, CBCL_raw_renamed)
fitmeasures(AnxDep_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))

# parameter estimates
p_est <- parameterEstimates(AnxDep_cfa, standardized = TRUE) # can also put rsquare = TRUE to get r2 here.
standardized_p_est <- standardizedSolution(AnxDep_cfa, type="std.all")
standardized_p_est <- standardized_p_est %>%
  mutate_at(vars(est.std, se, z, pvalue, ci.lower, ci.upper), round, digits = 3)
#write.csv(standardized_p_est, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/standardised_est/intern_est.csv')

# Examine Residuals
#resids <- lavResiduals(AnxDep_cfa, zstat=TRUE)
#resid_cov <- resids$cov.z
#lower_t <- resid_cov[lower.tri(resid_cov)]
#sum(lower_t > 1.96 | lower_t < -1.96)
resid <- lavaan::residuals(AnxDep_cfa, type="cor.bollen")
resid_corr <- resid$cov
lower_t <- resid_corr[lower.tri(resid_corr)]
sum(lower_t > 0.1 | lower_t < -0.1)
AVE(AnxDep_cfa)
# Get fail stage optimisations
AnxDep_cfa@optim$iterations
AnxDep_cfa@optim$converged

####################################################################################
# do CFA for Withdrawn Depressed.
####################################################################################
WithDep_syntax <- 'WITHDEP =~ WITHDEP1 + WITHDEP2 + WITHDEP3 + WITHDEP4 + WITHDEP5 + WITHDEP6 + WITHDEP7 + WITHDEP8'
WithDep_cfa <- run_cfa(WithDep_syntax, CBCL_raw_renamed)
fitmeasures(WithDep_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))

# parameter estimates
p_est <- parameterEstimates(WithDep_cfa, standardized = TRUE) # can also put rsquare = TRUE to get r2 here.
standardized_p_est <- standardizedSolution(WithDep_cfa, type="std.all")
standardized_p_est <- standardized_p_est %>%
  mutate_at(vars(est.std, se, z, pvalue, ci.lower, ci.upper), round, digits = 3)
#write.csv(standardized_p_est, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/standardised_est/intern_est.csv')

# Examine Residuals
#resids <- lavResiduals(WithDep_cfa, zstat=TRUE)
#resid_cov <- resids$cov.z
#lower_t <- resid_cov[lower.tri(resid_cov)]
#sum(lower_t > 1.96 | lower_t < -1.96)
#resid_cov <- round(resid_cov,3)
resid <- lavaan::residuals(WithDep_cfa, type="cor.bollen")
resid_corr <- resid$cov
lower_t <- resid_corr[lower.tri(resid_corr)]
sum(lower_t > 0.1 | lower_t < -0.1)
AVE(WithDep_cfa)
# Get fail stage optimisations
WithDep_cfa@optim$iterations
WithDep_cfa@optim$converged

####################################################################################
# do CFA for Somatic Complaints.
####################################################################################
Somatic_syntax <- 'SOMATIC =~ SOMATIC1 + SOMATIC2 + SOMATIC3 + SOMATIC4 + SOMATIC5 + SOMATIC6 + SOMATIC7 + SOMATIC7 + 
SOMATIC8 + SOMATIC9 + SOMATIC10 + SOMATIC11'
Somatic_cfa <- run_cfa(Somatic_syntax, CBCL_raw_renamed)
fitmeasures(Somatic_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))

# parameter estimates
p_est <- parameterEstimates(Somatic_cfa, standardized = TRUE) # can also put rsquare = TRUE to get r2 here.
standardized_p_est <- standardizedSolution(Somatic_cfa, type="std.all")
standardized_p_est <- standardized_p_est %>%
  mutate_at(vars(est.std, se, z, pvalue, ci.lower, ci.upper), round, digits = 3)
#write.csv(standardized_p_est, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/standardised_est/intern_est.csv')

# Examine Residuals
#resids <- lavResiduals(Somatic_cfa, zstat=TRUE)
#resid_cov <- resids$cov.z
#lower_t <- resid_cov[lower.tri(resid_cov)]
#sum(lower_t > 1.96 | lower_t < -1.96)
#resid_cov <- round(resid_cov,3)
resid <- lavaan::residuals(Somatic_cfa, type="cor.bollen")
resid_corr <- resid$cov
lower_t <- resid_corr[lower.tri(resid_corr)]
sum(lower_t > 0.1 | lower_t < -0.1)
AVE(Somatic_cfa)
# Get fail stage optimisations
Somatic_cfa@optim$iterations
Somatic_cfa@optim$converged
####################################################################################
# do CFA for Social Problems.
####################################################################################
Social_syntax <- 'SOCIAL =~ SOCIAL1  + SOCIAL2 + SOCIAL3 + SOCIAL4 + 
SOCIAL5 + SOCIAL6 + SOCIAL7 + SOCIAL8 + SOCIAL9 + SOCIAL10 + SOCIAL11'
Social_cfa <- run_cfa(Social_syntax, CBCL_raw_renamed)
fitmeasures(Social_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))

# parameter estimates
p_est <- parameterEstimates(Social_cfa, standardized = TRUE) # can also put rsquare = TRUE to get r2 here.
standardized_p_est <- standardizedSolution(Social_cfa, type="std.all")
standardized_p_est <- standardized_p_est %>%
  mutate_at(vars(est.std, se, z, pvalue, ci.lower, ci.upper), round, digits = 3)
#write.csv(standardized_p_est, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/standardised_est/intern_est.csv')

# Examine Residuals
#resids <- lavResiduals(Social_cfa, zstat=TRUE)
#resid_cov <- resids$cov.z
#lower_t <- resid_cov[lower.tri(resid_cov)]
#sum(lower_t > 1.96 | lower_t < -1.96)
#resid_cov <- round(resid_cov,3)
resid <- lavaan::residuals(Social_cfa, type="cor.bollen")
resid_corr <- resid$cov
lower_t <- resid_corr[lower.tri(resid_corr)]
sum(lower_t > 0.1 | lower_t < -0.1)
AVE(Social_cfa)
# Get fail stage optimisations
Social_cfa@optim$iterations
Social_cfa@optim$converged
####################################################################################
# do CFA for Thought Problems.
####################################################################################
Thought_syntax <- 'Thought =~ THOUGHT1 + THOUGHT2
 + THOUGHT3 + THOUGHT4 + THOUGHT5 + THOUGHT6 + THOUGHT7 + THOUGHT8 + THOUGHT9 + THOUGHT10 + 
 THOUGHT11 + THOUGHT12 + THOUGHT13 + THOUGHT14 + THOUGHT15'
Thought_cfa <- run_cfa(Thought_syntax, CBCL_raw_renamed)
fitmeasures(Thought_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))

# parameter estimates
p_est <- parameterEstimates(Thought_cfa, standardized = TRUE) # can also put rsquare = TRUE to get r2 here.
standardized_p_est <- standardizedSolution(Thought_cfa, type="std.all")
standardized_p_est <- standardized_p_est %>%
  mutate_at(vars(est.std, se, z, pvalue, ci.lower, ci.upper), round, digits = 3)
#write.csv(standardized_p_est, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/standardised_est/intern_est.csv')

# Examine Residuals
#resids <- lavResiduals(Thought_cfa, zstat=TRUE)
#resid_cov <- resids$cov.z
#lower_t <- resid_cov[lower.tri(resid_cov)]
#sum(lower_t > 1.96 | lower_t < -1.96)
#resid_cov <- round(resid_cov,3)
resid <- lavaan::residuals(Thought_cfa, type="cor.bollen")
resid_corr <- resid$cov
lower_t <- resid_corr[lower.tri(resid_corr)]
sum(lower_t > 0.1 | lower_t < -0.1)
AVE(Thought_cfa)
# Get fail stage optimisations
Thought_cfa@optim$iterations
Thought_cfa@optim$converged
####################################################################################
# do CFA for Attention Problems.
####################################################################################
Attent_syntax <- 'Attention =~ ATTENT1 + ATTENT2 + ATTENT3 + 
 ATTENT4 + ATTENT5 + ATTENT6 + ATTENT7 + ATTENT8 + ATTENT9 + ATTENT10 + ATTENT11'
Attent_cfa <- run_cfa(Attent_syntax, CBCL_raw_renamed)
fitmeasures(Attent_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))

# parameter estimates
p_est <- parameterEstimates(Attent_cfa, standardized = TRUE) # can also put rsquare = TRUE to get r2 here.
standardized_p_est <- standardizedSolution(Thought_cfa, type="std.all")
standardized_p_est <- standardized_p_est %>%
  mutate_at(vars(est.std, se, z, pvalue, ci.lower, ci.upper), round, digits = 3)
#write.csv(standardized_p_est, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/standardised_est/intern_est.csv')

# Examine Residuals
#resids <- lavResiduals(Attent_cfa, zstat=TRUE)
#resid_cov <- resids$cov.z
#lower_t <- resid_cov[lower.tri(resid_cov)]
#sum(lower_t > 1.96 | lower_t < -1.96)
#resid_cov <- round(resid_cov,3)
resid <- lavaan::residuals(Attent_cfa, type="cor.bollen")
resid_corr <- resid$cov
lower_t <- resid_corr[lower.tri(resid_corr)]
sum(lower_t > 0.1 | lower_t < -0.1)
AVE(Attent_cfa)
# Get fail stage optimisations
Attent_cfa@optim$iterations
Attent_cfa@optim$converged
####################################################################################
# do CFA for Rule Breaking Problems.
####################################################################################
Rulebr_syntax <- 'Rule_BR =~ RULE_BR1 
 + RULE_BR2 + RULE_BR3  + RULE_BR4 + RULE_BR5 + RULE_BR6 + RULE_BR7 + RULE_BR8 + RULE_BR9 + 
 RULE_BR10 + RULE_BR11 + RULE_BR12 + RULE_BR13 + RULE_BR14 + RULE_BR15 + RULE_BR16 + RULE_BR17 
 + RULE_BR18'
Rulebr_cfa <- run_cfa(Rulebr_syntax, CBCL_raw_renamed)
fitmeasures(Rulebr_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))

# parameter estimates
p_est <- parameterEstimates(Rulebr_cfa, standardized = TRUE) # can also put rsquare = TRUE to get r2 here.
standardized_p_est <- standardizedSolution(Rulebr_cfa, type="std.all")
standardized_p_est <- standardized_p_est %>%
  mutate_at(vars(est.std, se, z, pvalue, ci.lower, ci.upper), round, digits = 3)
#write.csv(standardized_p_est, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/standardised_est/intern_est.csv')

# Examine Residuals
#resids <- lavResiduals(Rulebr_cfa, zstat=TRUE)
#resid_cov <- resids$cov.z
#lower_t <- resid_cov[lower.tri(resid_cov)]
#sum(lower_t > 1.96 | lower_t < -1.96)
#resid_cov <- round(resid_cov,3)
resid <- lavaan::residuals(Rulebr_cfa, type="cor.bollen")
resid_corr <- resid$cov
lower_t <- resid_corr[lower.tri(resid_corr)]
sum(lower_t > 0.1 | lower_t < -0.1)
AVE(Rulebr_cfa)
# Get fail stage optimisations
Rulebr_cfa@optim$iterations
Rulebr_cfa@optim$converged
####################################################################################
# do CFA for Aggression.
####################################################################################
Aggress_syntax <- 'Aggress =~ AGGRESS1 + AGGRESS2 + AGGRESS3 + AGGRESS4 + AGGRESS5 + AGGRESS6 + AGGRESS7 + 
 AGGRESS8 + AGGRESS9 + AGGRESS9 + AGGRESS10 + AGGRESS11 + AGGRESS12 + AGGRESS13 + AGGRESS14 + 
AGGRESS15 + AGGRESS16 + AGGRESS17 + AGGRESS18'
Aggress_cfa <- run_cfa(Aggress_syntax, CBCL_raw_renamed)
fitmeasures(Aggress_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))

# parameter estimates
p_est <- parameterEstimates(Aggress_cfa, standardized = TRUE) # can also put rsquare = TRUE to get r2 here.
standardized_p_est <- standardizedSolution(Aggress_cfa, type="std.all")
standardized_p_est <- standardized_p_est %>%
  mutate_at(vars(est.std, se, z, pvalue, ci.lower, ci.upper), round, digits = 3)
#write.csv(standardized_p_est, '/home/kanep/kg98_scratch/Kane/behaviour_denoise/CFAs/standardised_est/intern_est.csv')

# Examine Residuals
#resids <- lavResiduals(Aggress_cfa, zstat=TRUE)
#resid_cov <- resids$cov.z
#lower_t <- resid_cov[lower.tri(resid_cov)]
#sum(lower_t > 1.96 | lower_t < -1.96)
#resid_cov <- round(resid_cov,3)
resid <- lavaan::residuals(Aggress_cfa, type="cor.bollen")
resid_corr <- resid$cov
lower_t <- resid_corr[lower.tri(resid_corr)]
sum(lower_t > 0.1 | lower_t < -0.1)
AVE(Aggress_cfa)
# Get fail stage optimisations
Aggress_cfa@optim$iterations
Aggress_cfa@optim$converged
############################################################################################################################################
#                                                   Subscale Models.
############################################################################################################################################

####################################################################################
# CFA Function.
####################################################################################
run_cfa <- function(model, data, rotation = "oblimin", estimator = "MLR") {
  result <- cfa(model = model,
                data = data,
                rotation = rotation,
                estimator = estimator,
                mimic="Mplus")
  
  return(result)
}
####################################################################################
# Log transform the CBCL sum scale scores.
####################################################################################
CBCL_t1_subc_log <-CBCL_t1_subc+1
CBCL_t1_subc_log <- log(CBCL_t1_subc_log)

# Plot original and log
df_combined <- data.frame(
  CBCL_raw = unlist(CBCL_t1_subc),
  CBCL_log = unlist(CBCL_t1_subc_log)
)
df_long <- pivot_longer(df_combined, cols = everything())
# Plot both histograms side by side
ggplot(df_long, aes(x = value, fill = name)) +
  geom_histogram(color = "black", alpha = 0.7, bins = 20) +
  facet_wrap(~name, scales = "free") +
  labs(x = "Subscale Score") +
  scale_fill_manual(values = c("skyblue", "lightgreen")) +
  theme_minimal() +
  theme(legend.position = "none") 

####################################################################################
# Bi-Factor Model.
####################################################################################
bifac_subc_model <- 'P =~ cbcl_scr_syn_anxdep_r + cbcl_scr_syn_withdep_r + cbcl_scr_syn_social_r + cbcl_scr_syn_somatic_r + cbcl_scr_syn_thought_r + cbcl_scr_syn_attention_r + cbcl_scr_syn_rulebreak_r + cbcl_scr_syn_aggressive_r
                    Externalising =~ NA*cbcl_scr_syn_rulebreak_r + v2*cbcl_scr_syn_rulebreak_r + v2*cbcl_scr_syn_aggressive_r
                    Internalising =~ cbcl_scr_syn_anxdep_r + cbcl_scr_syn_withdep_r + cbcl_scr_syn_somatic_r

                    # Covariances
                    P ~~ 0*Internalising
                    P ~~ 0*Externalising
                    
                    Internalising ~~ 0*Externalising
                    Externalising ~~ 1*Externalising'

bifac_subc_cfa <- run_cfa(bifac_subc_model, data=CBCL_t1_subc_log)
fitmeasures(bifac_subc_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
# Examine Residuals
resids <- lavResiduals(bifac_subc_cfa, zstat=T)
resid_cov <- resids$cov.z
resid_cor <- lavaan::residuals(bifac_subc_cfa, type="cor.bollen")
# parameter estimates
p_est <- parameterEstimates(bifac_subc_cfa, standardized = TRUE) 
standardized_p_est <- standardizedSolution(bifac_subc_cfa, type="std.all")
# Starting values
start_vals <- lavInspect(bifac_subc_cfa, what='start')
# Get fail stage optimisations
bifac_subc_cfa@optim$iterations
bifac_subc_cfa@optim$converged

####################################################################################
# Uni-Dimensional Model.
####################################################################################
unidim_subc_model <- 'P =~ cbcl_scr_syn_anxdep_r + cbcl_scr_syn_withdep_r + cbcl_scr_syn_social_r + cbcl_scr_syn_somatic_r + cbcl_scr_syn_thought_r + cbcl_scr_syn_attention_r + cbcl_scr_syn_rulebreak_r + cbcl_scr_syn_aggressive_r
                    '

unidim_subc_cfa <- run_cfa(unidim_subc_model, data=CBCL_t1_subc_log)
fitmeasures(unidim_subc_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
# Examine Residuals
resids <- lavResiduals(unidim_subc_cfa, zstat=T)
resid_cov <- resids$cov.z
resid_cor <- lavaan::residuals(unidim_subc_cfa, type="cor.bollen")
# parameter estimates
p_est <- parameterEstimates(unidim_subc_cfa, standardized = TRUE) 
standardized_p_est <- standardizedSolution(unidim_subc_cfa, type="std.all")

# Get fail stage optimisations
unidim_subc_cfa@optim$iterations
unidim_subc_cfa@optim$converged

####################################################################################
# Internalising / Externalising Model
####################################################################################
IE_subc_model <- '
                    Externalising =~ NA*cbcl_scr_syn_rulebreak_r + v2*cbcl_scr_syn_rulebreak_r + v2*cbcl_scr_syn_aggressive_r
                    Internalising =~ cbcl_scr_syn_anxdep_r + cbcl_scr_syn_withdep_r + cbcl_scr_syn_somatic_r

                    # Covariances
                    Internalising ~~ Externalising
                    Externalising ~~ 1*Externalising'

IE_subc_cfa <- run_cfa(IE_subc_model, data=CBCL_t1_subc_log)
fitmeasures(IE_subc_cfa, c('chisq','df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
# Examine Residuals
resids <- lavResiduals(IE_subc_cfa, zstat=T)
resid_cov <- resids$cov.z
resid_cor <- lavaan::residuals(IE_subc_cfa, type="cor.bollen")
# parameter estimates
p_est <- parameterEstimates(IE_subc_cfa, standardized = TRUE) 
standardized_p_est <- standardizedSolution(IE_subc_cfa, type="std.all")
# Get fail stage optimisations
bifac_subc_cfa@optim$iterations
bifac_subc_cfa@optim$converged
