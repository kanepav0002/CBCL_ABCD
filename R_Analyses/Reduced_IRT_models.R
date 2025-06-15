library(lavaan)
library(tidyverse)
library(semTools)
library(psych)
library(mirt)
library(poLCA)
library(tidyLPA)
library(pheatmap)
library(ggplot2)
library(dplyr)
library(MplusAutomation)

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

anx_dep <- CBCL_t1_raw[,c('cbcl_q14_p', 'cbcl_q29_p', 'cbcl_q30_p', 'cbcl_q31_p', 'cbcl_q32_p', 'cbcl_q33_p', 'cbcl_q35_p', 'cbcl_q45_p', 'cbcl_q50_p', 'cbcl_q52_p', 'cbcl_q71_p', 'cbcl_q91_p', 'cbcl_q112_p')]
colnames(anx_dep) <- paste0("anx_dep", 1:ncol(anx_dep))
withdrawn_dep <- CBCL_t1_raw[, c('cbcl_q05_p', 'cbcl_q42_p', 'cbcl_q65_p', 'cbcl_q69_p', 'cbcl_q75_p', 'cbcl_q102_p', 'cbcl_q103_p',  'cbcl_q111_p')]
colnames(withdrawn_dep) <- paste0("with_dep", 1:ncol(withdrawn_dep))
social <- CBCL_t1_raw[,c('cbcl_q11_p', 'cbcl_q12_p', 'cbcl_q25_p', 'cbcl_q27_p', 'cbcl_q34_p', 'cbcl_q36_p', 'cbcl_q38_p', 'cbcl_q48_p', 'cbcl_q62_p', 'cbcl_q64_p', 'cbcl_q79_p')]
colnames(social) <- paste0("social", 1:ncol(social))
somatic <- CBCL_t1_raw[,c('cbcl_q47_p', 'cbcl_q49_p', 'cbcl_q51_p', 'cbcl_q54_p', 'cbcl_q56a_p', 'cbcl_q56b_p', 'cbcl_q56c_p', 'cbcl_q56d_p', 'cbcl_q56e_p', 'cbcl_q56f_p', 'cbcl_q56g_p')]
colnames(somatic) <- paste0("somatic", 1:ncol(somatic))
thought <- CBCL_t1_raw[,c('cbcl_q09_p', 'cbcl_q18_p', 'cbcl_q40_p', 'cbcl_q46_p', 'cbcl_q58_p', 'cbcl_q59_p', 'cbcl_q60_p', 'cbcl_q66_p', 'cbcl_q70_p', 'cbcl_q76_p', 'cbcl_q83_p', 'cbcl_q84_p', 'cbcl_q85_p', 'cbcl_q92_p', 'cbcl_q100_p')]
colnames(thought) <- paste0("thought", 1:ncol(thought))
attention <- CBCL_t1_raw[,c('cbcl_q01_p', 'cbcl_q04_p', 'cbcl_q07_p', 'cbcl_q08_p', 'cbcl_q10_p', 'cbcl_q13_p', 'cbcl_q17_p', 'cbcl_q41_p', 'cbcl_q61_p', 'cbcl_q78_p', 'cbcl_q80_p', 'cbcl_q93_p', 'cbcl_q109_p')]
colnames(attention) <- paste0("attention", 1:ncol(attention))
rule_break <- CBCL_t1_raw[,c('cbcl_q02_p', 'cbcl_q26_p', 'cbcl_q28_p', 'cbcl_q39_p', 'cbcl_q43_p', 'cbcl_q63_p', 'cbcl_q67_p', 'cbcl_q72_p', 'cbcl_q73_p', 'cbcl_q81_p', 'cbcl_q82_p', 'cbcl_q90_p', 'cbcl_q96_p', 'cbcl_q98_p', 'cbcl_q99_p', 'cbcl_q101_p', 'cbcl_q105_p', 'cbcl_q106_p')]
colnames(rule_break) <- paste0("rule_break", 1:ncol(rule_break))
aggressive <- CBCL_t1_raw[,c('cbcl_q03_p', 'cbcl_q16_p', 'cbcl_q19_p', 'cbcl_q20_p', 'cbcl_q21_p', 'cbcl_q22_p', 'cbcl_q23_p', 'cbcl_q37_p', 'cbcl_q57_p', 'cbcl_q68_p', 'cbcl_q86_p', 'cbcl_q87_p', 'cbcl_q88_p', 'cbcl_q89_p', 'cbcl_q94_p', 'cbcl_q95_p', 'cbcl_q97_p', 'cbcl_q104_p')]
colnames(aggressive) <- paste0("aggresive", 1:ncol(aggressive))

renamed_CBCL_t1 <- cbind(anx_dep, withdrawn_dep, social, somatic, thought, attention, rule_break, aggressive)
#####################################################################################################################################################
# IRT plotting function
#####################################################################################################################################################

plot_panel_data <- function(panel_args, title, x_label = "θ", y_label = "P(θ)") {
  # Prepare the data for plotting
  plot_data <- do.call(rbind, lapply(seq_along(panel_args), function(i) {
    x <- panel_args[[i]][['x']]
    y <- panel_args[[i]][['y']]
    
    # Create a segment column to group every 200 points
    segment <- rep(1:3, each = 200, length.out = length(x))
    
    data.frame(x = x, y = y, segment = factor(segment), trace = paste0("Item ", i))
  }))
  
  # Create the ggplot
  ggplot(plot_data, aes(x = x, y = y, color = segment)) +
    geom_line(size = 1) + # Line plot for each trace and segment
    facet_wrap(~trace, scales = "free_y") + # Create a subplot for each trace
    scale_color_manual(values = c("aquamarine", "darkgoldenrod1", "blueviolet")) + # Custom colors
    labs(title = title, x = x_label, y = y_label, color = "Category") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      legend.position = "right"
    )
}

#####################################################################################################################################################
# TIF plotting function
#####################################################################################################################################################

TIF_plot <- function(grm_model, scale = "Total Information Function (TIF) and Standard Error") {
  library(ggplot2)
  
  # Calculate TIF and SE
  Theta <- seq(-3, 3, by = 0.1)
  tif <- testinfo(grm_model, Theta = Theta)
  tif <- tif+1
  se <- 1 / sqrt(tif)
  se <- round(se,3)
  reliability <- 1 - (1/tif)
  reliability <- round(reliability,3)
  
  # Create data frame
  plot_data <- data.frame(Theta, Information = tif, SE = se, reliability = reliability)
  
  #  scaling for secondary axis
  max_info <- min(max(tif))  
  se_scale <- max_info / max(se)
  
  # Plot with dashed SE line
  p<- ggplot(plot_data, aes(x = Theta)) +
    geom_line(aes(y = Information), color = "blue", linewidth = 1) +
    geom_line(aes(y = SE * se_scale), color = "red", linewidth = 1, linetype = "dashed") +  # <-- Added linetype here
    scale_y_continuous(
      name = "Information",
      limits = c(0, max_info),
      sec.axis = sec_axis(
        ~ . / se_scale,
        name = "Standard Error",
        breaks = scales::pretty_breaks(n = 5)
      )
    ) +
    labs(
      title = "",
      x = scale
    ) +
    theme_minimal() +
    theme(
      axis.title.y.left = element_text(color = "blue"),
      axis.title.y.right = element_text(color = "red"),
      plot.title = element_text(hjust = 0.5)
    )
  return(list(plot=p, params=plot_data))

}

#####################################################################################################################################################
# Reduced Models - items removed
#####################################################################################################################################################

############ Anx dep
class_probs <- read.table("LCA_IRT/anx_dep/6c_cprobs.txt")
class_probs <- class_probs[ ,length(class_probs)]
class_to_use <- anx_dep[!apply(is.na(anx_dep), 1, all), ] # Remove the sub with all NA
hist(rowSums(class_to_use[class_probs==6, ])) # use hist to figure out which is ZI class
class_to_use <- class_to_use[class_probs %in% c(1,2,3,5,6), ]

# Remove items
class_to_use <- class_to_use[, !colnames(class_to_use) %in% c("anx_dep2", "anx_dep6", "anx_dep7", "anx_dep9")]
# Run model
grm_mod <- mirt(class_to_use, 1, itemtype = "graded")
# TIF
TIF_params <- TIF_plot(grm_mod, "Anxious Depressed")
TIF_params$plot
TIF_values <- TIF_params$params
# Run CFA on reduced model
reduced_model <- paste("F1 =~", paste(colnames(class_to_use), collapse = " + "))
reduced <- cfa(reduced_model, data = class_to_use, estimator="WLSMV", ordered=T, mimic="Mplus")
fitmeasures(reduced, c('chisq', 'df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
resids <- lavResiduals(reduced, zstat=TRUE)
resid_cov <- resids$cov
resid_cov <- round(resid_cov,3)
lower_t <- resid_cov[lower.tri(resid_cov)]
sum(lower_t > 0.1| lower_t < -0.1)
AVE(reduced)

############ With dep
class_probs <- read.table("LCA_IRT/with_dep/5c_cprobs.txt")
class_probs <- class_probs[ ,length(class_probs)]
class_to_use <- withdrawn_dep[!apply(is.na(withdrawn_dep), 1, all), ] # Remove the sub with all NA
hist(rowSums(class_to_use[class_probs==5, ])) # use hist to figure out which is ZI class
class_to_use <- class_to_use[class_probs %in% c(1,2,4,5), ]

# Remove items
class_to_use <- class_to_use[, !colnames(class_to_use) %in% c("with_dep3", "with_dep4", "with_dep5")]
# Run model
grm_mod <- mirt(class_to_use, 1, itemtype = "graded")
# TIF
TIF_params <- TIF_plot(grm_mod, "Withdrawn Depressed")
TIF_params$plot
TIF_values <- TIF_params$params
# Run CFA on reduced model
reduced_model <- paste("F1 =~", paste(colnames(class_to_use), collapse = " + "))
reduced <- cfa(reduced_model, data = class_to_use, estimator="WLSMV", ordered=T, mimic="Mplus")
fitmeasures(reduced, c('chisq', 'df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
resids <- lavResiduals(reduced, zstat=TRUE)
resid_cov <- resids$cov
resid_cov <- round(resid_cov,3)
lower_t <- resid_cov[lower.tri(resid_cov)]
sum(lower_t > 0.1| lower_t < -0.1)
AVE(reduced)

############ Social
class_probs <- read.table("LCA_IRT/social/5c_cprobs.txt")
class_probs <- class_probs[ ,length(class_probs)]
class_to_use <- social[!apply(is.na(social), 1, all), ] # Remove the sub with all NA
hist(rowSums(class_to_use[class_probs==5, ])) # use hist to figure out which is ZI class
class_to_use <- class_to_use[class_probs %in% c(1,2,3,4), ]

# Remove items
class_to_use <- class_to_use[, !colnames(class_to_use) %in% c("social9", "social4", "social1", "social6", "social10", "social11")]
# Run model
grm_mod <- mirt(class_to_use, 1, itemtype = "graded")
# TIF
TIF_params <- TIF_plot(grm_mod, "Social Problems")
TIF_params$plot
TIF_values <- TIF_params$params
# Run CFA on reduced model
reduced_model <- paste("F1 =~", paste(colnames(class_to_use), collapse = " + "))
reduced <- cfa(reduced_model, data = class_to_use, estimator="WLSMV", ordered=T, mimic="Mplus")
fitmeasures(reduced, c('chisq', 'df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
resids <- lavResiduals(reduced, zstat=TRUE)
resid_cov <- resids$cov
resid_cov <- round(resid_cov,3)
lower_t <- resid_cov[lower.tri(resid_cov)]
sum(lower_t > 0.1| lower_t < -0.1)
AVE(reduced)

############ Somatic
class_probs <- read.table("LCA_IRT/somatic/5c_cprobs.txt")
class_probs <- class_probs[ ,length(class_probs)]
class_to_use <- somatic[!apply(is.na(somatic), 1, all), ] # Remove the sub with all NA
hist(rowSums(class_to_use[class_probs==5, ])) # use hist to figure out which is ZI class
class_to_use <- class_to_use[class_probs %in% c(2,3,4,5), ]

# Remove items
class_to_use <- class_to_use[, !colnames(class_to_use) %in% c("somatic5", "somatic6", "somatic7", "somatic9", "somatic10", "somatic11")]
# Run model
grm_mod <- mirt(class_to_use, 1, itemtype = "graded")
# TIF
TIF_params <- TIF_plot(grm_mod, "Somatic Problems")
TIF_params$plot
TIF_values <- TIF_params$params
# Run CFA on reduced model
reduced_model <- paste("F1 =~", paste(colnames(class_to_use), collapse = " + "))
reduced <- cfa(reduced_model, data = class_to_use, estimator="WLSMV", ordered=T, mimic="Mplus")
fitmeasures(reduced, c('chisq', 'df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
resids <- lavResiduals(reduced, zstat=TRUE)
resid_cov <- resids$cov
resid_cov <- round(resid_cov,3)
lower_t <- resid_cov[lower.tri(resid_cov)]
sum(lower_t > 0.1| lower_t < -0.1)
AVE(reduced)

############ Thought
class_probs <- read.table("LCA_IRT/thought/3c_cprobs.txt")
class_probs <- class_probs[ ,length(class_probs)]
class_to_use <- thought[!apply(is.na(thought), 1, all), ] # Remove the sub with all NA
hist(rowSums(class_to_use[class_probs==3, ])) # use hist to figure out which is ZI class
class_to_use <- class_to_use[class_probs %in% c(2,3), ]

# Remove items
class_to_use <- class_to_use[, !colnames(class_to_use) %in% c("thought5", "thought6", "thought12", "thought10", 
                                                              "thought11", "thought8", "thought15", "thought7", 
                                                              "thought9")]
# Run model
grm_mod <- mirt(class_to_use, 1, itemtype = "graded")
# TIF
TIF_params <- TIF_plot(grm_mod, "Thought Problems")
TIF_params$plot
TIF_values <- TIF_params$params
# Run CFA on reduced model
reduced_model <- paste("F1 =~", paste(colnames(class_to_use), collapse = " + "))
reduced <- cfa(reduced_model, data = class_to_use, estimator="WLSMV", ordered=T, mimic="Mplus")
fitmeasures(reduced, c('chisq', 'df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
resids <- lavResiduals(reduced, zstat=TRUE)
resid_cov <- resids$cov
resid_cov <- round(resid_cov,3)
lower_t <- resid_cov[lower.tri(resid_cov)]
sum(lower_t > 0.1| lower_t < -0.1)
AVE(reduced)



############ Attention
class_probs <- read.table("LCA_IRT/attention/6c_cprobs.txt")
class_probs <- class_probs[ ,length(class_probs)]
class_to_use <- attention[!apply(is.na(attention), 1, all), ] # Remove the sub with all NA
hist(rowSums(class_to_use[class_probs==6, ])) # use hist to figure out which is ZI class
class_to_use <- class_to_use[class_probs %in% c(1,2,3,4,5), ]

# Remove items
class_to_use <- class_to_use[, !colnames(class_to_use) %in% c("attention3", "attention4", "attention5", "attention7", 
                                                              "attention7", "attention8", "attention11")]
# Run model
grm_mod <- mirt(class_to_use, 1, itemtype = "graded")
# TIF
TIF_params <- TIF_plot(grm_mod, "Attention Problems")
TIF_params$plot
TIF_values <- TIF_params$params
# Run CFA on reduced model
reduced_model <- paste("F1 =~", paste(colnames(class_to_use), collapse = " + "))
reduced <- cfa(reduced_model, data = class_to_use, estimator="WLSMV", ordered=T, mimic="Mplus")
fitmeasures(reduced, c('chisq', 'df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
resids <- lavResiduals(reduced, zstat=TRUE)
resid_cov <- resids$cov
resid_cov <- round(resid_cov,3)
lower_t <- resid_cov[lower.tri(resid_cov)]
sum(lower_t > 0.1| lower_t < -0.1)
AVE(reduced)

############ Rule Breaking
class_probs <- read.table("LCA_IRT/rule_break/3c_cprobs.txt")
class_probs <- class_probs[ ,length(class_probs)]
class_to_use <- rule_break[!apply(is.na(rule_break), 1, all), ] # Remove the sub with all NA
hist(rowSums(class_to_use[class_probs==3, ])) # use hist to figure out which is ZI class
class_to_use <- class_to_use[class_probs %in% c(1,3), ]

# Remove items
class_to_use <- class_to_use[, !colnames(class_to_use) %in% c("rule_break15", "rule_break17", "rule_break6", "rule_break13", 
                                                              "rule_break10", "rule_break14", "rule_break1", "rule_break3")]
# Run model
grm_mod <- mirt(class_to_use, 1, itemtype = "graded")
# TIF
TIF_params <- TIF_plot(grm_mod, "Rule Breaking")
TIF_params$plot
TIF_values <- TIF_params$params
# Run CFA on reduced model
reduced_model <- paste("F1 =~", paste(colnames(class_to_use), collapse = " + "))
reduced <- cfa(reduced_model, data = class_to_use, estimator="WLSMV", ordered=T, mimic="Mplus")
fitmeasures(reduced, c('chisq', 'df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
resids <- lavResiduals(reduced, zstat=TRUE)
resid_cov <- resids$cov
resid_cov <- round(resid_cov,3)
lower_t <- resid_cov[lower.tri(resid_cov)]
sum(lower_t > 0.1| lower_t < -0.1)
AVE(reduced)

############ Aggression
class_probs <- read.table("LCA_IRT/aggress/5c_cprobs.txt")
class_probs <- class_probs[ ,length(class_probs)]
class_to_use <- aggressive[!apply(is.na(aggressive), 1, all), ] # Remove the sub with all NA
hist(rowSums(class_to_use[class_probs==5, ])) # use hist to figure out which is ZI class
class_to_use <- class_to_use[class_probs %in% c(1,2,3,4), ]

# Remove items
class_to_use <- class_to_use[, !colnames(class_to_use) %in% c("aggresive6", "aggresive1", "aggresive13", "aggresive11", 
                                                              "aggresive3", "aggresive8", "aggresive16", "aggresive4",
                                                              "aggresive2")]
# Run model
grm_mod <- mirt(class_to_use, 1, itemtype = "graded")
# TIF
TIF_params <- TIF_plot(grm_mod, "Aggression")
TIF_params$plot
TIF_values <- TIF_params$params
# Run CFA on reduced model
reduced_model <- paste("F1 =~", paste(colnames(class_to_use), collapse = " + "))
reduced <- cfa(reduced_model, data = class_to_use, estimator="WLSMV", ordered=T, mimic="Mplus")
fitmeasures(reduced, c('chisq', 'df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
resids <- lavResiduals(reduced, zstat=TRUE)
resid_cov <- resids$cov
resid_cov <- round(resid_cov,3)
lower_t <- resid_cov[lower.tri(resid_cov)]
sum(lower_t > 0.1| lower_t < -0.1)
AVE(reduced)
