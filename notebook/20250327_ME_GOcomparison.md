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

## Comparing q-value and bonferroni results

### q-value fdr = 0.05 vs q-value fdr = 0.01
![compare_q_value05_q_value01](https://github.com/user-attachments/assets/c0f8926c-9887-4d80-8c53-2ca8b73da5f3)

### q-value fdr = 0.01 vs bonferroni
![compare_bonferroni_qvalue-1](https://github.com/user-attachments/assets/16babcaa-65c9-4eba-8f27-d295f03bdb87)

### q-value fdr = 0.05 vs bonferroni
![compare_bonferroni_q_value05](https://github.com/user-attachments/assets/e1127502-af0f-4ede-bb76-89350443197e)


# Comparing Fst with lfmm2 results

## Fst vs Bonferroni SNPs
![compare_bonferroni_fst](https://github.com/user-attachments/assets/7c643d3d-b571-417a-86e9-56e5a714b1d0)

## Fst vs q-value fdr = 0.05
![compare_fst_q_value05](https://github.com/user-attachments/assets/9b350f20-45a5-4c94-b054-dd4da7635b1c)

## Fst vs q-value fdr = 0.01 
![compare_fst_q_value01](https://github.com/user-attachments/assets/c7bdae85-2f2f-4c99-8201-a623022e8680)

# Comparing overlapping results for q-values fdr = 0.01 or fdr = 0.05 and Fst

## overlapping results between q-value fdr = 0.01 and Fst
<img width="1070" alt="overlapping_genome_qvalue01_fst" src="https://github.com/user-attachments/assets/fc2f94d4-715e-40dd-a885-9bbe69ff8d89" />
<img width="753" alt="overlapping_qvalue01_fst_snps" src="https://github.com/user-attachments/assets/4f859a33-3891-48f7-9a31-4e6b09ee1d65" />

## overlapping results between q-value fdr = 0.05 and Fst
<img width="1064" alt="overlapping_genome_qvalue05_fst" src="https://github.com/user-attachments/assets/45809ebf-3900-43ea-8850-4c7e89211ea5" />
<img width="751" alt="overlapping_quavlue05_fst_snps" src="https://github.com/user-attachments/assets/434923b4-8cc9-40df-ba33-c94bbf31e0df" />





