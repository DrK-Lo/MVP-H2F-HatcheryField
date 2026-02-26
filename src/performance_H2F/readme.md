# Performance

Folder for code related to calculating and modeling survival, shell length, and condition index across hatchery, nursery, and field stages of the MVP field experiment.

## Data preparation scripts

| File | Function | Datasets used | Datasets produced |
|------|----------|---------------|-------------------|
| length_calc_field.Rmd | Reads raw tag-level field measurement data, merges with bag metadata, assigns monitoring events by date, and performs all cleaning (population renaming, dead/missing removal, outlier correction). Computes bag-level means and exploratory population-level summaries with line plots. | `MVP23-FieldBags-bags.csv`, `MVP23-FieldBags-tags.csv`, `MVP23-FieldBags-tagsLength.csv` | **Data (CSV):** `field_length_all_times.csv`, `lengths_bags.csv` |
| survival_calc_field.Rmd | Calculates cumulative bag-level survival from raw mortality count data across three monitoring events. Performs all cleaning (population renaming, bag_site attachment) and computes population- and site-level summary statistics with exploratory line plots. | `MVP23-FieldBags-bags.csv`, `MVP23-FieldBags-mortality.csv`, `MVP23-FieldBags-spawn_trt2.csv` | **Data (CSV):** `field_surv_all_times.csv` |
| Q1_nursery_performance.Rmd | Processes larval/nursery length data from hatchery imaging records. Merges group and image data, filters to eyed-stage oysters (days 15–21 and day 77), removes "small" non-eyed individuals, renames populations. Runs ANOVA and Tukey post-hoc tests on pre-deployment (day 77) shell lengths. | `CviMVP_Larvae_Growth-spawn_trt.csv`, `CviMVP_Larvae_Growth-group.csv`, `CviMVP_Larvae_Growth-larvae_img.csv` | **Data (CSV):** `nursery_length.csv` · **Figures (PDF):** `FigS3_nursery_length.pdf` |

## Analysis and modeling scripts

| File | Function | Datasets used | Datasets produced |
|------|----------|---------------|-------------------|
| condition_index_analysis.Rmd | Analyzes condition index across sites and populations for field experiment juveniles. Fits LMMs (site × population) at May 2024 and November 2024 time points, runs Tukey post-hoc comparisons via emmeans, and produces exploratory boxplots of CI, shell weight, and tissue weight by population, site, and grouping (wild, selection, mixtures). | `MVP_phenotyping_final.csv` | None saved (exploratory) |
| Q1_field_performance_models.Rmd | Models differences in field survival and length by population, site, and time point using linear (mixed) models. Runs Type III ANOVAs, backward model selection, and Tukey post-hoc tests with compact letter displays for the final monitoring event. Produces final-time-point data files with significance letters attached. | `field_surv_all_times.csv` (from survival_calc_field.Rmd), `field_length_all_times.csv` (from length_calc_field.Rmd) | **Data (CSV):** `field_surv_final_time.csv`, `field_length_final_time.csv` · **Results (TXT):** `Q1_survival_model_results.txt`, `Q1_length_model_results.txt` · **Results (CSV):** `Q1_field_length_letters.csv` |

## Figure scripts

| File | Function | Datasets used | Datasets produced |
|------|----------|---------------|-------------------|
| Q1_condition_survival_length_figures.Rmd | Creates publication-ready boxplots of condition index, survival, and shell length at the final field monitoring event (November 2024), with significance letter segments overlaid. CI modeling (LMM + emmeans) is performed within the script; survival and length read pre-computed results with letters. | `MVP_phenotyping_final.csv`, `field_surv_final_time.csv` (from Q1_field_performance_models.Rmd), `field_length_final_time.csv` (from Q1_field_performance_models.Rmd) | **Figures (PDF):** `FigS4_field_ci.pdf`, `Fig2B_field_surv.pdf`, `Fig2C_field_len.pdf` |
