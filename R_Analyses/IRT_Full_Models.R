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
# Do scree test to test for unidimensionality
#####################################################################################################################################################
subscales <- list(
  Anxious_Depressed = anx_dep,
  Withdrawn_Depressed = withdrawn_dep,
  Social_Problems = social,
  Somatic_Problems = somatic,
  Thought_Problems = thought,
  Attention_Problems = attention,
  Rule_Breaking = rule_break,
  Aggression = aggressive
)

# Function to create a plot for each subscale
create_eigenvalue_plot <- function(data, name) {
  rows_with_nan <- apply(data, 1, function(row) any(is.na(row)))
  clean_data <- data[!rows_with_nan, ]
  
  if (nrow(clean_data) > 0) {  # Avoid empty datasets
    comps <- prcomp(clean_data, scale = TRUE)
    eigenvalues <- comps$sdev^2
    eigenvalue_df <- data.frame(
      Component = 1:length(eigenvalues),
      Eigenvalue = eigenvalues
    )
    
    ggplot(eigenvalue_df, aes(x = Component, y = Eigenvalue)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "blue", size = 2) +
      scale_x_continuous(breaks = 1:length(eigenvalues)) +
      labs(
        title = name,
        x = "Principal Component / Factor",
        y = "Eigenvalue"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black")
      )
  } else {
    NULL
  }
}
plots <- lapply(names(subscales), function(name) {
  create_eigenvalue_plot(subscales[[name]], name)
})
# Display the plots (e.g., print the first plot)
plots[[8]]  # Replace 1 with the index of the plot you want to display

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
# Regular IRT (no accounting for ZI class)
#####################################################################################################################################################
# Anxious Depressed
#unimodel <- 'WD = 1-8' # Whatever the length of the subscale is
grm_mod <- mirt(anx_dep, 1, itemtype = "graded")
TIF_params <- TIF_plot(grm_mod, "Anxious Depressed")
TIF_params$plot
TIF_values <- TIF_params$params
trace_data <- plot(grm_mod, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Anxious Depressed")
#itemplot(grm_mod, 2, type="info")

ad_loc_discrim <- coef(grm_mod, IRT=T, simplify=T)$items %>% round(3) %>% 
  as.data.frame() 
ad_chisq_item_fit <- itemfit(grm_mod, na.rm=T)
ad_ld <- mirt::residuals(grm_mod, type="LD", approx.z=T)

# Withdrawn Depressed
grm_mod <- mirt(withdrawn_dep, 1, itemtype = "graded")
TIF_params <- TIF_plot(grm_mod, "Withdrawn Depressed")
TIF_params$plot
TIF_values <- TIF_params$params
trace_data <- plot(grm_mod, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Withdrawn Depressed")
wd_loc_discrim <- coef(grm_mod, IRT=T, simplify=T)$items %>% round(3) %>% 
  as.data.frame() 
wd_chisq_item_fit <- itemfit(grm_mod, na.rm=T)
wd_ld <- mirt::residuals(grm_mod, type="LD", approx.z=T)

# Social
grm_mod <- mirt(social, 1, itemtype = "graded")
TIF_params <- TIF_plot(grm_mod, "Social Problems")
TIF_params$plot
TIF_values <- TIF_params$params
trace_data <- plot(grm_mod, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Social Problems")
soc_loc_discrim <- coef(grm_mod, IRT=T, simplify=T)$items %>% round(3) %>% 
  as.data.frame() 
soc_chisq_item_fit <- itemfit(grm_mod, na.rm=T)
soc_ld <- mirt::residuals(grm_mod, type="LD", approx.z=T)

# Somatic
grm_mod <- mirt(somatic, 1, itemtype = "graded")
TIF_params <- TIF_plot(grm_mod, "Somatic Problems")
TIF_params$plot
TIF_values <- TIF_params$params
trace_data <- plot(grm_mod, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Somatic Problems")
somcom_loc_discrim <- coef(grm_mod, IRT=T, simplify=T)$items %>% round(3) %>% 
  as.data.frame() 
somcom_chisq_item_fit <- itemfit(grm_mod, na.rm=T)
somcom_ld <- mirt::residuals(grm_mod, type="LD", approx.z=T)

# Thought
grm_mod <- mirt(thought, 1, itemtype = "graded")
TIF_params <- TIF_plot(grm_mod, "Thought Problems")
TIF_params$plot
TIF_values <- TIF_params$params
trace_data <- plot(grm_mod, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Thought Problems")
thought_loc_discrim <- coef(grm_mod, IRT=T, simplify=T)$items %>% round(3) %>% 
  as.data.frame() 
thought_chisq_item_fit <- itemfit(grm_mod, na.rm=T)
thought_ld <- mirt::residuals(grm_mod, type="LD", approx.z=T)

# Attention
grm_mod <- mirt(attention, 1, itemtype = "graded")
TIF_params <- TIF_plot(grm_mod, "Attention Problems")
TIF_params$plot
TIF_values <- TIF_params$params
trace_data <- plot(grm_mod, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Attention Problems")
attent_loc_discrim <- coef(grm_mod, IRT=T, simplify=T)$items %>% round(3) %>% 
  as.data.frame() 
attent_chisq_item_fit <- itemfit(grm_mod, na.rm=T)
attent_ld <- mirt::residuals(grm_mod, type="LD", approx.z=T)

# Rule Breaking
grm_mod <- mirt(rule_break, 1, itemtype = "graded")
TIF_params <- TIF_plot(grm_mod, "Rule Breaking")
TIF_params$plot
TIF_values <- TIF_params$params
trace_data <- plot(grm_mod, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Rule Breaking")
rb_loc_discrim <- coef(grm_mod, IRT=T, simplify=T)$items %>% round(3) %>% 
  as.data.frame() 
rb_chisq_item_fit <- itemfit(grm_mod, na.rm=T)
rb_ld <- mirt::residuals(grm_mod, type="LD", approx.z=T)

# Aggression
grm_mod <- mirt(aggressive, 1, itemtype = "graded")
TIF_params <- TIF_plot(grm_mod, "Aggressive Problems")
TIF_params$plot
TIF_values <- TIF_params$params
trace_data <- plot(grm_mod, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Aggression")
agg_loc_discrim <- coef(grm_mod, IRT=T, simplify=T)$items %>% round(3) %>% 
  as.data.frame() 
agg_chisq_item_fit <- itemfit(grm_mod, na.rm=T)
agg_ld <- mirt::residuals(grm_mod, type="LD", approx.z=T)

#####################################################################################################################################################
# IRT plotting function for zero inflation
#####################################################################################################################################################

plot_panel_data <- function(panel_args, title, x_label = "θ", y_label = "P(θ)") {
  # Prepare the data for plotting
  plot_data <- do.call(rbind, lapply(seq_along(panel_args), function(i) {
    x <- panel_args[[i]][['x']]
    y <- panel_args[[i]][['y']]
    
    # Create a segment column to group every 200 points
    segment <- rep(1:6, each = 200, length.out = length(x))
    
    data.frame(x = x, y = y, segment = factor(segment), trace = paste0("Item ", i))
  }))
  
  # Create the ggplot
  ggplot(plot_data, aes(x = x, y = y, color = segment)) +
    geom_line(size = 1) + # Line plot for each trace and segment
    facet_wrap(~trace, scales = "free_y") + # Create a subplot for each trace
    scale_color_manual(values = c("cadetblue4", "darkgoldenrod4", "darkorchid4","aquamarine", "darkgoldenrod1", "darkorchid1"),
                       labels = c("ZI-1", "ZI-2", "ZI-3", "NZ-1", "NZ-2", "NZ-3")) + # Custom colors
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
# Zero Inflated IRT
#####################################################################################################################################################
# Anxious Depressed GRM
ziGRM <- " F = 1-13
START [MIXTURE_1] = (GROUP, MEAN_1, -100), (GROUP, COV_11, .00001),
(1-13, a1, 1.0),
(1-13, d1, 2), (1-13, d2, 0)
FIXED [MIXTURE_1] = (GROUP, MEAN_1), (GROUP, COV_11),
(1-13, a1),
(1-13, d1), (1-13, d2)
"
technical <- list(customTheta = matrix(c(-100, seq(-6, 6, length.out = 61))))
ziGRM_fit <- multipleGroup(anx_dep, ziGRM, dentype = 'mixture-2', technical=technical)
coef(ziGRM_fit, simplify = TRUE)
pi_hat <- fscores(ziGRM_fit, method = 'classify')

trace_data <- plot(ziGRM_fit, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Anxious Depressed")

# Withdrawn Depressed GRM
ziGRM <- " F = 1-8
START [MIXTURE_1] = (GROUP, MEAN_1, -100), (GROUP, COV_11, .00001),
(1-8, a1, 1.0),
(1-8, d1, 2), (1-8, d2, 0)
FIXED [MIXTURE_1] = (GROUP, MEAN_1), (GROUP, COV_11),
(1-8, a1),
(1-8, d1), (1-8, d2)
"
technical <- list(customTheta = matrix(c(-100, seq(-6, 6, length.out = 61))))
ziGRM_fit <- multipleGroup(withdrawn_dep, ziGRM, dentype = 'mixture-2', technical=technical)
coef(ziGRM_fit, simplify = TRUE)

trace_data <- plot(ziGRM_fit, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Withdrawn Depressed")

# Withdrawn Depressed GRM
ziGRM <- " F = 1-8
START [MIXTURE_1] = (GROUP, MEAN_1, -100), (GROUP, COV_11, .00001),
(1-8, a1, 1.0),
(1-8, d1, 2), (1-8, d2, 0)
FIXED [MIXTURE_1] = (GROUP, MEAN_1), (GROUP, COV_11),
(1-8, a1),
(1-8, d1), (1-8, d2)
"
technical <- list(customTheta = matrix(c(-100, seq(-6, 6, length.out = 61))))
ziGRM_fit <- multipleGroup(withdrawn_dep, ziGRM, dentype = 'mixture-2', technical=technical)
coef(ziGRM_fit, simplify = TRUE)

trace_data <- plot(ziGRM_fit, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Withdrawn Depressed")

# Social Problems GRM
ziGRM <- " F = 1-11
START [MIXTURE_1] = (GROUP, MEAN_1, -100), (GROUP, COV_11, .00001),
(1-11, a1, 1.0),
(1-11, d1, 2), (1-11, d2, 0)
FIXED [MIXTURE_1] = (GROUP, MEAN_1), (GROUP, COV_11),
(1-11, a1),
(1-11, d1), (1-11, d2)
"
technical <- list(customTheta = matrix(c(-100, seq(-6, 6, length.out = 61))))
ziGRM_fit <- multipleGroup(social, ziGRM, dentype = 'mixture-2', technical=technical)
coef(ziGRM_fit, simplify = TRUE)

trace_data <- plot(ziGRM_fit, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Social Problems")

# Somatic Problems GRM
ziGRM <- " F = 1-11
START [MIXTURE_1] = (GROUP, MEAN_1, -100), (GROUP, COV_11, .00001),
(1-11, a1, 1.0),
(1-11, d1, 2), (1-11, d2, 0)
FIXED [MIXTURE_1] = (GROUP, MEAN_1), (GROUP, COV_11),
(1-11, a1),
(1-11, d1), (1-11, d2)
"
technical <- list(customTheta = matrix(c(-100, seq(-6, 6, length.out = 61))))
ziGRM_fit <- multipleGroup(somatic, ziGRM, dentype = 'mixture-2', technical=technical)
coef(ziGRM_fit, simplify = TRUE)

trace_data <- plot(ziGRM_fit, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Somatic Problems")

# Attention GRM
ziGRM <- " F = 1-13
START [MIXTURE_1] = (GROUP, MEAN_1, -100), (GROUP, COV_11, .00001),
(1-13, a1, 1.0),
(1-13, d1, 2), (1-13, d2, 0)
FIXED [MIXTURE_1] = (GROUP, MEAN_1), (GROUP, COV_11),
(1-13, a1),
(1-13, d1), (1-13, d2)
"
technical <- list(customTheta = matrix(c(-100, seq(-6, 6, length.out = 61))))
ziGRM_fit <- multipleGroup(attention, ziGRM, dentype = 'mixture-2', technical=technical)
coef(ziGRM_fit, simplify = TRUE)

trace_data <- plot(ziGRM_fit, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Attention Problems")

# Thought Problems GRM
ziGRM <- " F = 1-15
START [MIXTURE_1] = (GROUP, MEAN_1, -100), (GROUP, COV_11, .00001),
(1-15, a1, 1.0),
(1-15, d1, 2), (1-15, d2, 0)
FIXED [MIXTURE_1] = (GROUP, MEAN_1), (GROUP, COV_11),
(1-15, a1),
(1-15, d1), (1-15, d2)
"
technical <- list(customTheta = matrix(c(-100, seq(-6, 6, length.out = 61))))
ziGRM_fit <- multipleGroup(thought, ziGRM, dentype = 'mixture-2', technical=technical)
coef(ziGRM_fit, simplify = TRUE)

trace_data <- plot(ziGRM_fit, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Thought Problems")

# Rule Breaking GRM
ziGRM <- " F = 1-18
START [MIXTURE_1] = (GROUP, MEAN_1, -100), (GROUP, COV_11, .00001),
(1-18, a1, 1.0),
(1-18, d1, 2), (1-18, d2, 0)
FIXED [MIXTURE_1] = (GROUP, MEAN_1), (GROUP, COV_11),
(1-18, a1),
(1-18, d1), (1-18, d2)
"
technical <- list(customTheta = matrix(c(-100, seq(-6, 6, length.out = 61))))
ziGRM_fit <- multipleGroup(rule_break, ziGRM, dentype = 'mixture-2', technical=technical)
coef(ziGRM_fit, simplify = TRUE)

trace_data <- plot(ziGRM_fit, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Rule Breaking")

# Aggression GRM
ziGRM <- " F = 1-18
START [MIXTURE_1] = (GROUP, MEAN_1, -100), (GROUP, COV_11, .00001),
(1-18, a1, 1.0),
(1-18, d1, 2), (1-18, d2, 0)
FIXED [MIXTURE_1] = (GROUP, MEAN_1), (GROUP, COV_11),
(1-18, a1),
(1-18, d1), (1-18, d2)
"
technical <- list(customTheta = matrix(c(-100, seq(-6, 6, length.out = 61))))
ziGRM_fit <- multipleGroup(aggressive, ziGRM, dentype = 'mixture-2', technical=technical)
coef(ziGRM_fit, simplify = TRUE)

trace_data <- plot(ziGRM_fit, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title = "Aggression")

#####################################################################################################################################################
# Dichotomized GRM
#####################################################################################################################################################
# Anxious Depressed GRM
# Dichotomize answers and do a ZI-2PL to see if it pulls out a more sensible 0 class
#anx_dep_dicot <- anx_dep
#anx_dep_dicot[anx_dep_dicot==2] <- 1

#zi2PL <- "F = 1-13
#START [MIXTURE_1] = (GROUP, MEAN_1, -100), (GROUP, COV_11, .00001),
#(1-13, a1, 1.0), (1-13, d, 0)
#FIXED [MIXTURE_1] = (GROUP, MEAN_1), (GROUP, COV_11),
#(1-13, a1), (1-13, d)"
# define custom Theta integration grid that contains extreme theta + normal grid
#technical <- list(customTheta = matrix(c(-100, seq(-6,6,length.out=61))))
# fit ZIM-IRT
#zi2PL.fit <- multipleGroup(anx_dep_dicot, zi2PL, dentype = 'mixture-2', technical=technical)
#coef(zi2PL.fit, simplify=TRUE)
# classification estimates
#pi_hat <- fscores(zi2PL.fit, method = 'classify')
#head(pi_hat)
#tail(pi_hat)
#plot(zi2PL.fit, type="trace")


#####################################################################################################################################################
# Latent Class Analysis on individual subscales - old, now done in mplus
#####################################################################################################################################################

#################################################################################
# output data to Mplus for latent class estimation
#################################################################################
anx_dep <- anx_dep[!apply(is.na(anx_dep), 1, all), ] # Remove the sub with all NA
prepareMplusData(anx_dep, "/home/kanep/kg98_scratch/Kane/behaviour_denoise/LCA_IRT/anx_dep/anx_dep.dat",
                 keepCols = c("anx_dep1", "anx_dep2", "anx_dep3", "anx_dep4", "anx_dep5", "anx_dep6",
                              "anx_dep7", "anx_dep8", "anx_dep9", "anx_dep10", "anx_dep11"))

withdrawn_dep <- withdrawn_dep[!apply(is.na(withdrawn_dep), 1, all), ] # Remove the sub with all NA
withdrawn_dep[is.na(withdrawn_dep)] <- -999
prepareMplusData(withdrawn_dep, "/home/kanep/kg98_scratch/Kane/behaviour_denoise/LCA_IRT/with_dep/with_dep.dat",
                 keepCols = c("with_dep1", "with_dep2", "with_dep3", "with_dep4", "with_dep5", "with_dep6",
                              "with_dep7", "with_dep8"))

social <- social[!apply(is.na(social), 1, all), ] # Remove the sub with all NA
social[is.na(social)] <- -999
prepareMplusData(social, "/home/kanep/kg98_scratch/Kane/behaviour_denoise/LCA_IRT/social/social.dat",
                 keepCols = c("social1", "social2", "social3", "social4", "social5", "social6",
                              "social7", "social8", "social9", "social10", "social11"))

somatic <- somatic[!apply(is.na(somatic), 1, all), ] # Remove the sub with all NA
somatic[is.na(somatic)] <- -999
prepareMplusData(somatic, "/home/kanep/kg98_scratch/Kane/behaviour_denoise/LCA_IRT/somatic/somatic.dat",
                 keepCols = c("somatic1", "somatic2", "somatic3", "somatic4", "somatic5", "somatic6",
                              "somatic7", "somatic8", "somatic9", "somatic10", "somatic11"))

thought <- thought[!apply(is.na(thought), 1, all), ] # Remove the sub with all NA
thought[is.na(thought)] <- -999
prepareMplusData(thought, "/home/kanep/kg98_scratch/Kane/behaviour_denoise/LCA_IRT/thought/thought.dat",
                 keepCols = c("thought1", "thought2", "thought3", "thought4", "thought5", "thought6",
                              "thought7", "thought8", "thought9", "thought10", "thought11", "thought12",
                              "thought13", "thought14", "thought15"))

attention <- attention[!apply(is.na(attention), 1, all), ] # Remove the sub with all NA
attention[is.na(attention)] <- -999
prepareMplusData(attention, "/home/kanep/kg98_scratch/Kane/behaviour_denoise/LCA_IRT/attention/attention.dat",
                 keepCols = c("attention1", "attention2", "attention3", "attention4", "attention5", "attention6",
                              "attention7", "attention8", "attention9", "attention10", "attention11", "attention12",
                              "attention13"))

rule_break <- rule_break[!apply(is.na(rule_break), 1, all), ] # Remove the sub with all NA
rule_break[is.na(rule_break)] <- -999
prepareMplusData(rule_break, "/home/kanep/kg98_scratch/Kane/behaviour_denoise/LCA_IRT/rule_break/rule_break.dat",
                 keepCols = c("rule_break1", "rule_break2", "rule_break3", "rule_break4", "rule_break5", 
                              "rule_break6", "rule_break7", "rule_break8", "rule_break9", "rule_break10", 
                              "rule_break11", "rule_break12", "rule_break13", "rule_break14", "rule_break15",
                              "rule_break16", "rule_break17", "rule_break18"))

aggressive <- aggressive [!apply(is.na(aggressive ), 1, all), ] # Remove the sub with all NA
aggressive[is.na(aggressive)] <- -999
prepareMplusData(aggressive , "/home/kanep/kg98_scratch/Kane/behaviour_denoise/LCA_IRT/aggress/aggress.dat",
                 keepCols = c("aggresive1", "aggresive2", "aggresive3", "aggresive4", "aggresive5", 
                              "aggresive6", "aggresive7", "aggresive8", "aggresive9", "aggresive10", 
                              "aggresive11", "aggresive12", "aggresive13", "aggresive14", "aggresive15",
                              "aggresive16", "aggresive17", "aggresive18"))

#################################################################################
# collate model results
#################################################################################
all_results <- readModels("LCA_IRT/aggress/", recursive=T)

model_res <- all_results[["LCA_IRT.aggress..classes10.out"]]
cat("Parameters:", model_res[['summaries']][['Parameters']], "\n",
    "LL:", model_res[['summaries']][['LL']], "\n",
    "BIC:", model_res[['summaries']][['BIC']], "\n",
    "AIC:", model_res[['summaries']][['AIC']], "\n",
    "LMR:", model_res[['summaries']][['T11_LMR_Value']], "\n",
    "LMR-P:", model_res[['summaries']][['T11_LMR_PValue']], "\n",
    "Entropy:", model_res[['summaries']][['Entropy']], "\n",
    "BLRT LL:", model_res[["summaries"]][["BLRT_2xLLDiff"]], "\n",
    "BLRT p:", model_res[["summaries"]][["BLRT_PValue"]], "\n",
    "Class sizes:", model_res[["class_counts"]][["modelEstimated"]][["count"]])

model_res[['warnings']][[1]]
model_res[['warnings']][[2]]
model_res[['errors']][[1]]
model_res[['errors']][[2]]

# Bayes factor - doesn't work as the BIC values are too high
#exp(-.5*126636) / exp(-.5*112783.8) 
#exp(126636/2 - 112783.8/2)

##########################################################################################################
# IRT on most reasonable latent class - PAPER VERSION.
##########################################################################################################
class_probs <- read.table("LCA_IRT/aggress/5c_cprobs.txt")
class_probs <- class_probs[ ,length(class_probs)]
class_to_use <- aggressive[!apply(is.na(aggressive), 1, all), ] # Remove the sub with all NA

hist(rowSums(class_to_use[class_probs==5, ])) # use hist to figure out which is ZI class
class_to_use <- class_to_use[class_probs %in% c(1,2,3,4), ]

# Run model
grm_mod <- mirt(class_to_use, 1, itemtype = "graded")
# plot model
trace_data <- plot(grm_mod, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title="Thought Problems")

# Check coefs and fit
loc_discrim <- coef(grm_mod, IRT=T, simplify=T)$items %>% round(3) %>% 
  as.data.frame() 
chisq_item_fit <- itemfit(grm_mod, na.rm=T)
ld <- mirt::residuals(grm_mod, type="LD", approx.z=T)
#pheatmap(ld, display_numbers = T)
loc_discrim$X2 <- chisq_item_fit[['S_X2']]
loc_discrim$X2_p <- chisq_item_fit[['p.S_X2']]
loc_discrim <- round(loc_discrim,3)

##########################################
# Remove items iteratively here
class_to_use <- class_to_use[, !colnames(class_to_use) %in% "aggresive2"]
###########################################

#write.csv(loc_discrim, file = "/home/kanep/kg98_scratch/Kane/behaviour_denoise/LCA_IRT/Anxious_Depressed_item_thresholds.csv", 
#          row.names = T)

# Run CFA on reduced model
reduced_model <- paste("F1 =~", paste(colnames(class_to_use), collapse = " + "))
reduced <- cfa(reduced_model, data = class_to_use, estimator="WLSMV", ordered=T, mimic="Mplus")
fitmeasures(reduced, c('chisq', 'df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
resid <- lavaan::residuals(reduced, type="cor.bollen")
resid_corr <- resid$cov
lower_t <- resid_corr[lower.tri(resid_corr)]
sum(lower_t > 0.1 | lower_t < -0.1)
#pheatmap(resid_cov, display_numbers = T)


#################################################################################
# Function to calculate Entropy as in Mplus (Relative Entropy)
#################################################################################
entropy.R2 <- function(fit, i) {
  nume.E<- -sum(fit$posterior * log(fit$posterior), na.rm=T)
  deno.E<-fit$N*log(i)
  ent.ex<-1-(nume.E/deno.E)
  ent.ex
}

#################################################################################
# Function to Plot LCA Classes
#################################################################################
plot_lca_classes <- function(curr_lca_dat, lca_func) {
  # Initialize the plot grid
  par(mfrow = c(1, 5)) 
  
  # Subtract 1 from the data to start
  curr_lca_dat <- curr_lca_dat - 1
  
  for (n_class in c(2, 3, 4, 5)) {
    curr_lca_dat <- curr_lca_dat + 1 # Add 1 to enable LCA estimation
    lca_model <- poLCA(lca_func, curr_lca_dat, nclass = n_class, maxiter=10000)
    pred_class <- lca_model[['predclass']]
    curr_lca_dat <- curr_lca_dat - 1 # Reset data after estimation
    
    # Manually create row sums for each class
    class_1 <- curr_lca_dat[pred_class == 1, ]
    row_sums_class_1 <- rowSums(class_1, na.rm = TRUE)
    class_2 <- curr_lca_dat[pred_class == 2, ]
    row_sums_class_2 <- rowSums(class_2, na.rm = TRUE)
    
    if (n_class > 2) {
      class_3 <- curr_lca_dat[pred_class == 3, ]
      row_sums_class_3 <- rowSums(class_3, na.rm = TRUE)
    }
    if (n_class > 3) {
      class_4 <- curr_lca_dat[pred_class == 4, ]
      row_sums_class_4 <- rowSums(class_4, na.rm = TRUE)
    }
    if (n_class > 4) {
      class_5 <- curr_lca_dat[pred_class == 5, ]
      row_sums_class_5 <- rowSums(class_5, na.rm = TRUE)
    }
    
    # Create an initial histogram for class 1
    hist(row_sums_class_1, main = NULL,
         xlab = "Row Sum", ylab = "Frequency", col = rgb(135, 206, 235, max = 255, alpha = 150),
         border = NA, freq = TRUE, xlim = range(c(row_sums_class_1, row_sums_class_2, 
                                                  if (n_class > 2) row_sums_class_3, 
                                                  if (n_class > 3) row_sums_class_4,
                                                  if (n_class > 4) row_sums_class_5)),
         ylim = c(0, 4000))
    
    # Overlay histograms for additional classes
    hist(row_sums_class_2, col = rgb(250, 128, 114, max = 255, alpha = 150), 
         border = NA, add = TRUE)
    if (n_class > 2) {
      hist(row_sums_class_3, col = rgb(127, 255, 212, max = 255, alpha = 150), 
           border = NA, add = TRUE)
    }
    if (n_class > 3) {
      hist(row_sums_class_4, col = rgb(255, 223, 0, max = 255, alpha = 150), 
           border = NA, add = TRUE)
    }
    if (n_class > 4) {
      hist(row_sums_class_5, col = rgb(138, 43, 226, max = 255, alpha = 150), 
           border = NA, add = TRUE)
    }
    
    # Add a legend for the plot
    legend("topright", legend = paste("Class", 1:n_class), 
           fill = c("skyblue", "salmon", "aquamarine", "darkgoldenrod1", "blueviolet")[1:n_class],
           title = "Classes", cex = 0.8)
  }
}

#################################################################################
# Analysis
#################################################################################

# Choose Which subscale to run on.
curr_lca_dat <- thought[!apply(is.na(thought), 1, all), ] # Remove the sub with all NA
curr_lca_dat <- curr_lca_dat+1 # add 1 to all vals otherwise LCA won't estimate
lca_func <- cbind(thought1, thought2, thought3, thought4, thought5, thought6, thought7,
                  thought8, thought9, thought10, thought11)~1

# get Fit statistics for each class
class_results <- data.frame(classes = 2:10, ll = NA,bic = NA,  
                            lmr = NA, lmr_df = NA, lmr_p = NA, entropy= NA)
for (i in 2:10) {
  model <- poLCA(lca_func, curr_lca_dat, nclass = i, maxiter = 10000)
  lrt <- NA 
  kminus1_model <- poLCA(lca_func, curr_lca_dat, nclass = i - 1)
  lrt <- calc_lrt(kminus1_model$Nobs, kminus1_model$llik, kminus1_model$npar, 
                  length(kminus1_model$P), model$llik, model$npar, length(model$P))
  # Store results
  class_results$ll[i - 1] <- model$llik
  class_results$bic[i - 1] <- model$bic
  class_results$lmr[i - 1] <- lrt[2]
  class_results$lmr_df[i - 1] <- lrt[3]
  class_results$lmr_p[i - 1] <- round(lrt[4],3)
  class_results$entropy[i-1] <- entropy.R2(model, i)
}

# Run LCA Plots
plot_lca_classes(curr_lca_dat, lca_func)
write.csv(class_results, file = "/home/kanep/kg98_scratch/Kane/behaviour_denoise/LCA_IRT/Social_class_results.csv", 
          row.names = FALSE)

#####################################################
# IRT on most reasonable latent class
#####################################################
class_to_use <- social[!apply(is.na(social), 1, all), ] # Remove sub with all NA
curr_lca_dat <- curr_lca_dat+1
lca_model <- poLCA(lca_func, curr_lca_dat, nclass=5)
pred_class <- lca_model[['predclass']]

hist(rowSums(class_to_use[pred_class==5, ])) # use hist to figure out which is ZI class
class_to_use <- class_to_use[pred_class %in% c(1,2,4,5), ]

# Run model
grm_mod <- mirt(class_to_use, 1, itemtype = "graded")
# plot model
trace_data <- plot(grm_mod, type = "trace", plot = FALSE)
panel_args <- trace_data[['panel.args']]
plot_panel_data(panel_args, title="Social Problems")

# Check coefs and fit
loc_discrim <- coef(grm_mod, IRT=T, simplify=T)$items %>% round(3) %>% 
  as.data.frame() 
chisq_item_fit <- itemfit(grm_mod, na.rm=T)
ld <- mirt::residuals(grm_mod, type="LD", approx.z=T)
#pheatmap(ld, display_numbers = T)
loc_discrim$X2 <- chisq_item_fit[['S_X2']]
loc_discrim$X2_p <- chisq_item_fit[['p.S_X2']]
loc_discrim <- round(loc_discrim,3)
##########################################
# Remove items iteratively here
class_to_use <- class_to_use[, !colnames(class_to_use) %in% "social6"]
###########################################
#write.csv(loc_discrim, file = "/home/kanep/kg98_scratch/Kane/behaviour_denoise/LCA_IRT/Anxious_Depressed_item_thresholds.csv", 
#          row.names = T)

# Run CFA on reduced model
reduced_model <- paste("F1 =~", paste(colnames(class_to_use), collapse = " + "))
reduced <- cfa(reduced_model, data = class_to_use, estimator="WLSMV", ordered=T, mimic="Mplus")
fitmeasures(reduced, c('chisq', 'df', 'pvalue', 'rmsea', 'cfi', 'srmr'))
resids <- lavResiduals(reduced, zstat=TRUE)
resid_cov <- resids$cov.z
resid_cov <- round(resid_cov,3)
lower_t <- resid_cov[lower.tri(resid_cov)]
sum(lower_t > 1.96 | lower_t < -1.96)
pheatmap(resid_cov, display_numbers = T)
