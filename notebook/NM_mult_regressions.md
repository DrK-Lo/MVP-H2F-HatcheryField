# Multiple regressions for Q2 (effect of environment of origin and parent genetics on offspring performance)
- The explanatory variables for this question are:
  - mean minimum temp
  - mean maximum temp
  - mean minimum salinity
  - mean maximum salinity
  - average observed heterozygosity
  - average allelic richness
- **Note**: I wanted to include site_name in these analyses and run the models as ANCOVAs, but since I expanded my data set out, with the same environmental/genetic values being applied many times to each individual from the same site, the ANCOVA thinks that site_name is perfectly collinear with all the rest of the variables. Not sure if/how to fix this.
-  I calculated correlation values and VIFs for the above variables. No variables had a correlation value higher than r = 0.7, but three VIFs were higher than 10
  - Mean min temp: VIF = 10.76
  - Mean max salinity: VIF = 13.54
  - Avg Ho: VIF = 15.04
- I ran test three linear models with length at day 21 as the response variable and excluded one of the above high-VIF variables in each lm
  - No real difference in homoscedasticity or outliers. Residuals were not normal for any models, BUT the model excluding mean minimum temperature had the highest p-value (Shapiro-Wilk test)
- There was one super low length entry from TX that was largely responsible for the normality issues, so I removed it and reran the above three models
  - Residuals still weren't normal, but normality was very much improved with the exclusion (p values from Shapiro Wilk tests were orders of magnitude higher than with the full data set)
- Model excluding mean min temp still had best normality, so I recalculated collinearity and VIF for the variables excluding mean min temp
  - Without mean min temp, no variables had r > 0.7, and no variable had a VIF higher than 10. Will use this model with a transformation for normality.
- I ran the model with survival as well, but I don't think it's worth including, too few data points (n=8), many outliers, not homoscedastic.
  
## Results
**Day 21 - End of Hatchery Stage**
- I squared length at day 21 (excluding the one low length point) to attain normality
- All predictors were significant
- Model stats: Adj. R squared = 0.1075, F = 15.87, p = 1.035e-14
**Day 78: End of Nursery Stage**
- No transformations of length would make model residuals normal at this time point
  - I tried log10, square root, cube root, squaring, cubing
  - Shapiro Wilk p-value was 0.025 without the transformations and orders of magnitude lower with them
  - So normality isn't super far off in the raw data, but it is off - non-parametric test?
- Results from the non-transformed length model (the model that violates normality assumption) showed that all predictors were significant except mean max salinity, so I excluded it and reran the model.
  - Model stats **with** mean max salinity: Adj. R squared = 0.280, F = 76.46, p < 2.2e-16
  - Model stats **without** mean max salinity: adj. R squared = 0.279, F = 94.82, p < 2.2e-16
    - Biggest change was in F-stat: Fstat increased in model without mean max salinity, adjusted R squared didn't really change, confirming that mean max salinity was not an important predictor of length at day 78
    - 
