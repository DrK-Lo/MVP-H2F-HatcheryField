# MGE run revised heterozygosity/fst experimental parents for H2F (6/6/26)
library(hierfstat)

# experimental dataset
# test locally first
#exp_full <- readRDS("/Users/madelineeppley/Desktop/Exp_parents_full_SNP_matrix.rds")
#exp_meta <- read.csv("/Users/madelineeppley/Desktop/Exp_parents_SNP_metadata.csv")
#exp_ind <- readRDS("/Users/madelineeppley/Desktop/20250717_samp_full_subset_exp.rds")
#all(rownames(exp_full) %in% exp_ind$clean_ID) # TRUE  

# now move to explorer and run everything below or comment out explorer setup and use local setup above
setwd("/projects/gatins/2025_Mobulid/hetfst")

#dir.create("experiment", showWarnings = FALSE)

# experimental dataset
exp_full <- readRDS("/projects/gatins/2025_Mobulid/hetfst/Exp_parents_full_SNP_matrix.rds")
exp_meta <- read.csv("/projects/gatins/2025_Mobulid/hetfst/Exp_parents_SNP_metadata.csv")
exp_ind <- readRDS("/projects/gatins/2025_Mobulid/hetfst/20250717_samp_full_subset_exp.rds")
all(rownames(exp_full) %in% exp_ind$clean_ID) # TRUE  

# subset the full matrix to the thinned matrix based on the column status in the metadata
thinned_affx <- exp_meta$Affx_ID[exp_meta$thinned_dataset == TRUE]
exp_thinned <- exp_full[, colnames(exp_full) %in% thinned_affx]
dim(exp_thinned) # 160x105356 good same as H2F preprint

# hierfstat requires population as the first col
exp_ind_ordered <- exp_ind[match(rownames(exp_thinned), exp_ind$clean_ID), ]
exp_pop_data <- data.frame(
  ID = rownames(exp_thinned),
  pop = exp_ind_ordered$ID_SiteDate)
recode_pop <- c(
  "TX-CapBay"    = "W1-TX",
  "LA-SisLake"   = "W2-LA",
  "FL-KingPlan"  = "W3-FL",
  "VA-DeepWatSh" = "W4-VA",
  "LOLA"         = "S1-LOLA",
  "DEBY"         = "S2-DEBY",
  "NH-GrtBay"    = "W5-NH",
  "ME-HogIs"     = "W6-ME")
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
exp_fis <- colMeans(exp_bs2$Fis, na.rm = TRUE)
range(exp_observed_het) # 0.2189690 0.2277474
range(exp_expected_het) # 0.2532307 0.2690511
range(exp_fis) # 0.1168022 0.1520994

# sanity check a few pops to make sure everything looks good
print(exp_observed_het) # looks ok
print(exp_expected_het)
print(exp_fis) 
table(exp_pop_data$pop) # checks out 20 inds per pop
#saveRDS(exp_bs2, "/Users/madelineeppley/Desktop/experiment_basic_stats.rds") # save locally on test

# heterozygosity results
saveRDS(exp_bs2, "experiment/experiment_basic_stats.rds")
write.csv(data.frame(locus = names(exp_observed_het), Ho = exp_observed_het, He = exp_expected_het),
          "experiment/experiment_heterozygosity.csv", row.names = FALSE)

# pairwise Fst
exp_fst <- pairwise.WCfst(cbind(pop = exp_pop_data$pop, exp_gen_recoded), diploid = TRUE)
write.csv(exp_fst, "experiment/experiment_pairwise_fst.csv")

# plot
library(ggplot2)
fst_mat <- as.matrix(read.csv("/Users/madelineeppley/Desktop/experiment/experiment_pairwise_fst.csv", row.names = 1, check.names = FALSE))

# pop display order
POP_ORDER <- c("W1-TX", "W2-LA", "W3-FL", "W4-VA", "S1-LOLA", "S2-DEBY", "W5-NH", "W6-ME")
# reorder fst matrix to match pop
fst_mat <- fst_mat[POP_ORDER, POP_ORDER]

# heatmap from a distance matrix
plot_heatmap <- function(mat, fill_label, title = "") {
  as.data.frame(as.table(mat)) %>%
    ggplot(aes(Var1, Var2, fill = Freq)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "D", na.value = "grey50",
                         guide = guide_colorbar(label.theme = element_text(angle = 45, size = 12))) +
    labs(title = title, x = "Treatment group", y = "Treatment group", fill = fill_label) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.position = "bottom")}

heatmap_fst # :)

# predictor distance heatmap
heatmap_fst <- plot_heatmap(fst_mat, expression("Pairwise " * F[ST]))
ggsave("/Users/madelineeppley/Desktop/experiment/experiment_fst_heatmap.pdf", heatmap_fst, width = 12, height = 10)


