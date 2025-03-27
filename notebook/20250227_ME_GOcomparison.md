# Comparing ShinyGO analyses for genes detected as outliers in lfmm2 and outflank

## Selecting genes from lfmm2: bonferroni or q-values?
#### Bonferroni line is more conservative, q-values fdr threshold can be set at 0.01 (middle ground) or 0.05 (most relaxed)
tldr: I think that the q-value with fdr = 0.01 is probably the best "middle-ground" option. The bonferroni line is too conservative and only returns 117 SNPs. However, it's worth noting that the q-value with fdr = 0.05 returns SNPs in quantities most similar to OutFlank (each ~9k SNPs detected as outliers, with ~800 mapping in ShinyGO). So I think it's justifiable to use q-values with fdr = 0.05 too. 

## Bonferroni results
#### 1570 SNPs detected as significant outliers, 117 map in ShinyGO

<img width="780" alt="bonferroni_chart" src="https://github.com/user-attachments/assets/e420febc-bf46-45d0-af4b-e6a32c30ae8f" />
<img width="1070" alt="bonferroni_genome" src="https://github.com/user-attachments/assets/b9b786cf-9218-4de6-8d94-225b2ec4fbd4" />

## q-value with fdr = 0.01 results
#### 5397 SNPs detected as significant outliers, 453 map in ShinyGO

<img width="744" alt="q_value_01_chart" src="https://github.com/user-attachments/assets/6cc76f08-9e63-4bcd-94dc-6e3f827ce2a8" />
<img width="1056" alt="q_value_01_genome" src="https://github.com/user-attachments/assets/8d1c4820-3224-4679-9f5b-15b63f43f037" />

## q-value with fdr = 0.05 results 
#### 9192 SNPs detected as significant outliers, 893 map in ShinyGO

<img width="746" alt="q_value_05_chart" src="https://github.com/user-attachments/assets/b25d4f31-6451-444e-ac94-d2a0b4d9e443" />
<img width="1071" alt="q_value_05_genome" src="https://github.com/user-attachments/assets/35d255c4-2861-41af-90d1-d4f05176455e" />


