# all data files related to parental genetic structure 

## FULL SNP set (n = 154,609)
Contains a data set filtered for missingness (n = 2976 SNPs removed) that did not meet a 10% threshold
Thinned for MAF

## THINNED SNP set, putatively neutral (n = 104,958)
Thinned for linkage disequilibrium 

# Files

| Dataset    | Description | Created in |
| -------- | ------- | ------- |
| 20250203_FULLSNPs_exp.rds  | Filtered for missingness and thinned for MAF | src/parental_genetics/genotypes_experimental.R  |
| 20250203_FULLmuts_exp.rds  | |  src/parental_genetics/genotypes_experimental.R  |
| 20250203_THINNEDSNPs_exp.rds  |  Thinned for LD | src/parental_genetics/genotypes_experimental.R  |
| 20250203_pca_data_exp.csv  | | src/parental_genetics/genotypes_experimental.R  |
| 20250203_samp_full_subset_exp.rds  | |  src/parental_genetics/genotypes_experimental.R |
| 20250204geno_imputedfile_exp.geno  | | src/parental_genetics/exp_structure_analysis.R  |
| 20250204geno_imputedfile_exp.snmfProject  | |  src/parental_genetics/exp_structure_analysis.R  |

