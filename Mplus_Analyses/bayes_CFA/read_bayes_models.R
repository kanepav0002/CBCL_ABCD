library(MplusAutomation)

setwd('/home/kanep/kg98_scratch/Kane/behaviour_denoise/bayes_sem_proper/')

runModels('bifactor/', recursive = T, showOutput = T)

models <- readModels('bifactor/', recursive=T)


model_res <- models[["bifactor..small_mean_prior.unidim_low_var_small_ecov.out"]]

cat("Parameters:", model_res[['summaries']][['Parameters']], "\n",
    "DIC:", model_res[['summaries']][['DIC']], "\n",
    "BIC:", model_res[['summaries']][['BIC']], "\n",
    "PPP:", model_res[['summaries']][['PostPred_PValue']], "\n",
    "PPPP:", model_res[['summaries']][['PriorPostPred_PValue']], "\n",
    "CFI:", model_res[['summaries']][['CFI']], "\n",
    "pD:", model_res[['summaries']][['pD']], "\n",
    "ChiSqDiff_95CI_LB:", model_res[['summaries']][['ObsRepChiSqDiff_95CI_LB']], "\n",
    "ChiSqDiff_95CI_UB:", model_res[['summaries']][['ObsRepChiSqDiff_95CI_UB']], "\n")

model_res[['warnings']][[1]]
model_res[['errors']][[1]]
model_res[['errors']][[2]]
