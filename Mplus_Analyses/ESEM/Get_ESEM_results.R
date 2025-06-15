library(MplusAutomation)
library(tidyr)

setwd('/home/kanep/kg98_scratch/Kane/behaviour_denoise/')

models <- readModels('ESEM/', recursive=T)

# Bifactor only extracted up to the 6 factor solutions
# ESEM only up to the 8

model_res <- models[["ESEM..bifactor_ESEM_6fac.out"]]

cat("Parameters:", model_res[['summaries']][['Parameters']], "\n",
    "ChiSq:", model_res[['summaries']][['ChiSqM_Value']], "\n",
    "ChiSq_P:", model_res[['summaries']][['ChiSqM_PValue']], "\n",
    "ChiSq_DF:", model_res[['summaries']][['ChiSqM_DF']], "\n",   
    "CFI:", model_res[['summaries']][['CFI']], "\n",
    "RMSEA:", model_res[['summaries']][['RMSEA_Estimate']], "\n",
    "RMSEA_UB:", model_res[['summaries']][['RMSEA_90CI_UB']], "\n",
    "RMSEA_LB:", model_res[['summaries']][['RMSEA_90CI_LB']], "\n",
    "SRMR:", model_res[['summaries']][['SRMR']], "\n")

all_loadings <- data.frame(
  factor = model_res[["parameters"]][["stdyx.standardized"]][["paramHeader"]],
  item_labels = model_res[["parameters"]][["stdyx.standardized"]][["param"]],
  loadings = model_res[["parameters"]][["stdyx.standardized"]][["est"]]
)
  

wide_loadings <- pivot_wider(
  data = all_loadings,
  names_from = "factor",    # Column to get new column names from
  values_from = "loadings", # Column to get values from
  id_cols = "item_labels"  # Column to keep as identifier
)

wide_loadings <- wide_loadings[, grep("item|\\.BY", names(wide_loadings))]
wide_loadings <- wide_loadings[complete.cases(wide_loadings), ]

# Calculate ECV
#loading_columns <- grep("\\.BY$", names(wide_loadings), value = TRUE)
#loadings_matrix <- as.matrix(wide_loadings[, loading_columns])
#squared_loadings <- loadings_matrix^2
#general_ECV <- sum(squared_loadings[, "FG.BY"]) / sum(squared_loadings)

  
#model_res[['warnings']][[1]]
#model_res[['errors']][[1]]
#model_res[['errors']][[2]]


# Convert to long format
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)

# 1. First extract the prefix from each item label
long_loadings <- wide_loadings %>%
  pivot_longer(
    cols = -item_labels,
    names_to = "Factor",
    values_to = "Loading"
  ) %>%
  mutate(
    Item_category = case_when(
      grepl("^AD", item_labels) ~ "AD",
      grepl("^WD", item_labels) ~ "WD",
      grepl("^SOC", item_labels) ~ "SOC",
      grepl("^SOM", item_labels) ~ "SOM",
      grepl("^TH", item_labels) ~ "TH",
      grepl("^ATT", item_labels) ~ "ATT",
      grepl("^RB", item_labels) ~ "RB",
      grepl("^AGG", item_labels) ~ "AGG",
      TRUE ~ "Other"
    ),
    Item_category = factor(Item_category, 
                           levels = c("AD", "WD", "SOC", "SOM", "TH", "ATT", "RB", "AGG"))
  )
long_loadings$Factor <- gsub("\\.BY$", "", long_loadings$Factor)
# 2. Preserve original item order
original_order <- wide_loadings$item_labels
long_loadings$item_labels <- factor(long_loadings$item_labels, levels = original_order)

# 3. Create color palette for categories
category_colors <- c(
  AD = "#1f77b4",   # Blue
  WD = "#ff7f0e",   # Orange
  SOC = "#2ca02c",  # Green
  SOM = "#d62728",  # Red
  TH = "#9467bd",   # Purple
  ATT = "#8c564b",  # Brown
  RB = "#e377c2",   # Pink
  AGG = "#7f7f7f"   # Gray
)
long_loadings <- long_loadings %>%
  group_by(Item_category) %>%
  mutate(category_space = cur_group_id() * 1000) %>%
  ungroup() %>%
  mutate(Item_order = as.numeric(item_labels) + category_space)

# Create heatmap with facets
ggplot(long_loadings, aes(x = Factor, y = reorder(item_labels, Item_order), fill = Loading)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits=c(-1,1)) +
  labs(x=NULL, y=NULL) +
  facet_grid(Item_category ~ ., scales = "free_y", space = "free_y") +
  theme(
    strip.text.y = element_text(angle = 0),
    axis.text.y = element_text(size = 6),
    panel.spacing = unit(0.1, "lines")
  )