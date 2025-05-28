In this repository one can find replication materials for the Bachelor Thesis "From Dictatorship to Democracy? The Influence of Revolutionary Mass Uprisings on Regime Change: a Cross–Country Perspective".

The analysis is performed using R version 4.5.0 on MacOS

To perform the analysis, you need to proceed with two steps:
1. Create datasets by opening "datasets.R" and running the code. To run the code, you will need NAVCO2.1_default.csv, beis_regr.csv and toronto_clean.csv.
2. Perform PanelMatch and SCM analysis by opening "script.R" (or "script_final" if you need up-to-date code) and running the code. Mind the name of the created datasets from step 1.
3. To perform PanelMatch on Revolutionary Episodes Dataset data, open "beis.R" and run the code. Mind the name of the created datasets from step 1.
4. Perform fixed-effects counterfactual estimators analysis by opening "fect.R" and running the code. Mind the name of the created datasets from step 1.

If you need the code for the visualizations from the sample description part, open "visualisations.R" and run the code.

If you need the datasets create by the multiple imputation, open RData files.
