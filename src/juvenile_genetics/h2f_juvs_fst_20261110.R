#install.packages("hierfstat")
library(hierfstat)

setwd("/projects/gatins/2025_Mobulid/hetfst")

dir.create("experiment_juvs", showWarnings = FALSE)

# experimental dataset
exp_full <- readRDS("/projects/gatins/2025_Mobulid/hetfst/20250616_FULLSNPs_exp_juvs_labeled.rds")
exp_meta <- read.csv("/projects/gatins/2025_Mobulid/hetfst/20250604_FULLmuts_exp_juvs.rds")
exp_ind <- read.csv("/projects/gatins/2025_Mobulid/hetfst/20251119_juvs_pca_inds_labels.csv")
thinned_dataset <- readRDS("/projects/gatins/2025_Mobulid/hetfst/20250616_THINNEDSNPs_exp_juvs_labeled.rds")

## 11/10/2026 
# explorer has a new pain in the ass kind of way to log in so here is how i scped the files in for future reference
#scp -i ~/.ssh/eppley.m-20261027T134537Z \
#~/Desktop/20250616_FULLSNPs_exp_juvs_labeled.rds \
#~/Desktop/20250616_THINNEDSNPs_exp_juvs_labeled.rds \
#~/Desktop/20251119_juvs_pca_inds_labels.csv \
#~/Desktop/20250604_FULLmuts_exp_juvs.rds \ 
#eppley.m@xfer.discovery.neu.edu:/projects/gatins/2025_Mobulid/hetfst/

#exp_full <- readRDS("/Users/madelineeppley/Desktop/20250616_FULLSNPs_exp_juvs_labeled.rds")
#exp_meta <- readRDS("/Users/madelineeppley/Desktop/20250604_FULLmuts_exp_juvs.rds")
#exp_ind <- read.csv("/Users/madelineeppley/Desktop/20251119_juvs_pca_inds_labels.csv")
#thinned_dataset <- readRDS("/Users/madelineeppley/Desktop/20250616_THINNEDSNPs_exp_juvs_labeled.rds")
all(rownames(exp_full) %in% exp_ind$clean_ID) # TRUE  
thinned_snps <- colnames(thinned_dataset)

# subset the full matrix to the thinned matrix based on the column status in the metadata
thinned_affx <- colnames(thinned_dataset)
exp_thinned <- exp_full[, colnames(exp_full) %in% thinned_affx]
dim(exp_thinned) # 267x93792 - good same as H2F preprint

# hierfstat requires population as the first col
exp_ind_ordered <- exp_ind[match(rownames(exp_thinned), exp_ind$clean_ID), ] # 267 x 16
colnames(exp_ind_ordered)
exp_ind_ordered$SpawnTrt_Label_check
exp_pop_data <- data.frame(
  ID = rownames(exp_thinned),
  pop = exp_ind_ordered$SpawnTrt_Label_check)
recode_pop <- c(
  "TX-CapBay"         = "W1-TX",
  "LA-SisLake"        = "W2-LA",
  "FL-KingPlan"       = "W3-FL",
  "VA-JR-DeepWatSh"   = "W4-VA",
  "LOLA"              = "S1-LOLA",
  "DEBY"              = "S2-DEBY",
  "NH-GrtBay"         = "W5-NH",
  "ME-HogIs"          = "W6-ME",
  "Poly-WithOutcross" = "H1-HYBRIDMIX",
  "Poly-NoOutcross"   = "H2-SEEDMIX")
table(exp_pop_data$pop, useNA = "ifany") # looks good
exp_pop_data$pop <- recode_pop[exp_pop_data$pop]

# hierfstat also requires genotype matrix to data frame
exp_gen_df <- as.data.frame(exp_thinned)

## for hierfstat, 0/1/2 is the incorrect coding format. For diploid organisms, each locus must be coded as a 2-digit number, where each allele is 1 digit.
## recode as 11/12/22 format
exp_gen_recoded <- as.data.frame(lapply(exp_gen_df, function(x) c(`0`=11, `1`=12, `2`=22)[as.character(x)]), row.names = rownames(exp_gen_df))
str(exp_gen_recoded) # looks correct

exp_bs2 <- basic.stats(cbind(pop = exp_pop_data$pop, exp_gen_recoded), diploid = TRUE)

# extract He and Ho from exp_bs2
exp_observed_het <- colMeans(exp_bs2$Ho, na.rm = TRUE)
exp_expected_het <- colMeans(exp_bs2$Hs, na.rm = TRUE)
range(exp_observed_het) # 0.2255629 0.2406028
range(exp_expected_het) # 0.2425698 0.2813096

# sanity check a few pops to make sure everything looks good
print(exp_observed_het) # these values are in the right range but they're close together
print(exp_expected_het) # good
table(exp_pop_data$pop) # good lots of lola

# save heterozygosity results
#saveRDS(exp_bs2, "/Users/madelineeppley/Desktop/experiment_juvs_basic_stats.rds")
#write.csv(data.frame(locus = names(exp_observed_het), Ho = exp_observed_het, He = exp_expected_het),
          #"/Users/madelineeppley/Desktop/experiment_juvs_heterozygosity_20261110.csv", row.names = FALSE)
saveRDS(exp_bs2, "experiment_juvs/experiment_juvs_basic_stats.rds")
write.csv(data.frame(locus = names(exp_observed_het), Ho = exp_observed_het, He = exp_expected_het),
          "experiment/experiment_juvs_heterozygosity_20261110.csv", row.names = FALSE)

# pairwise Fst
# downsample to test locally quick
#set.seed(1)
#exp_gen_sub <- exp_gen_recoded[, sample(ncol(exp_gen_recoded), 2000)]
#exp_fst <- pairwise.WCfst(data.frame(pop = factor(exp_pop_data$pop), exp_gen_sub), diploid = TRUE)
# this worked great :D

# run full fst
exp_fst <- pairwise.WCfst(cbind(pop = exp_pop_data$pop, exp_gen_recoded), diploid = TRUE)
write.csv(exp_fst, "experiment_juvs/experiment_juvs_pairwise_fst_20261110.csv")
