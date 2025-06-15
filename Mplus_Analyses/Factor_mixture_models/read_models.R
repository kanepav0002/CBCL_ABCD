library(MplusAutomation)
library(tidyverse)
library(ggplot2)


setwd('/home/kanep/kg98_scratch/Kane/behaviour_denoise/')

#### Read Models
all_results <- readModels("/home/kanep/kg98_scratch/Kane/CBCL_ABCD/Mplus_Analyses/Factor_mixture_models/", recursive=T)

# Get specific res
model_res <- all_results[[
"mplus_mixmodels.Mixture_models_subscales_tau_equivalence..FMM4.5_Classes.unidimensional_subscales_fmm_5c.out"]]

cat("Parameters:", model_res[['summaries']][['Parameters']], "\n",
    "LL:", model_res[['summaries']][['LL']], "\n",
    "BIC:", model_res[['summaries']][['BIC']], "\n",
    "AIC:", model_res[['summaries']][['AIC']], "\n",
    "LMR:", model_res[['summaries']][['T11_LMR_Value']], "\n",
    "LMR-P:", model_res[['summaries']][['T11_LMR_PValue']], "\n",
    "Entropy:", model_res[['summaries']][['Entropy']], "\n",
    "Class sizes:", model_res[["class_counts"]][["modelEstimated"]][["count"]])

model_res[['warnings']][[1]]
model_res[['warnings']][[2]]
model_res[['errors']][[1]]
model_res[['errors']][[2]]
