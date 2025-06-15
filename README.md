# Assessing the Psychometric Properties of the Child Behavior Checklist in the ABCD Study

This repository contains all code to reproduce analyses for the paper:  
**"Assessing the Psychometric Properties of the Child Behavior Checklist in the ABCD Study"**  
(Pavlovich, K., Tiego, J., Constable, T., Fornito, A.)

---

## Code Organization

### HIERARCHICAL STRUCTURE ANALYSES

| Analysis | Location | Description |
|----------|----------|-------------|
| Confirmatory Factor Analysis (CFA) | `R_Analyses/cfas.R` | Runs all CFAs for Section 1.1 of results |
| Bayesian CFA | `Mplus_Analyses/bayes_CFA/` | Contains Mplus `.inp` files for Section 1.2 models + R script to extract fit indices |
| Factor Mixture Models | `Mplus_Analyses/Factor_Mixture_Models/` | Mplus files for Section 1.3 models + R script to extract fit indices |
| Latent Class Analysis (LCA) | `Mplus_Analyses/LCA_subscale_level/` | Mplus files for prerequisite LCA for factor mixture modeling |

### SUBSCALE STRUCTURE ANALYSES

| Analysis | Location | Description |
|----------|----------|-------------|
| CFA | `R_Analyses/cfas.R` | Runs CFAs for Section 2.1 |
| Full IRT Models | `R_Analyses/IRT_full_models.R` | Runs IRT analyses for Section 2.2 |
| Reduced IRT Models | `R_Analyses/Reduced_IRT_models.R` | Runs reduced subscale IRT for Section 2.2 |
| Item-Level LCA | `Mplus_Analyses/LCA_Item_Level/` | Mplus files to identify zero-inflated classes per subscale |
| ESEM | `Mplus_Analyses/ESEM/` | Mplus files for Section 2.3 models + R script to extract fit indices |

---

## Dependencies

All R code requires:
- [`lavaan`](https://lavaan.ugent.be/) (CFA/SEM package)
- [`MplusAutomation`](https://github.com/michaelhallquist/MplusAutomation) (Mplus interface)

Install from CRAN:
```r
install.packages(c("lavaan", "MplusAutomation"))
```
---

 ## Notes
All code will need to have local paths updated to the file organisation on your local machine.

