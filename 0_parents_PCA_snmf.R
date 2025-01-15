---
title: "expPCA + snmf"
author: "Madeline Eppley"
date: "2025-01-14"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache = FALSE)
```

```{r}
setwd("~/Desktop/cvi-diploid-files")
```

```{r}
library(dplyr)
library(bigsnpr)
library(robustbase)
library(ggplot2)
library(ggrepel)
library(here)
library(LEA)
library(scatterpie)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)
```

```{r}
inds <- readRDS(here("20240922_indsmatrix.rds")) # inds data frame with full metadata
muts <- readRDS(here("20240911_mutmatrix_PASS.rds")) # mutation data frame with SNP info
experimental <- readRDS(here("20240911_experimentalmatrix.rds")) # genotype matrix with exp samples only
```

```{r}
exp <- experimental
samp_full <- inds

# step 1: have to subset just the seascape samples, otherwise dimensions don't line up ####
dim(exp) # 154 samples
dim(samp_full) # all passed samples (902), includes seascape + exp

exp_merge <- exp
exp_merge$merge_ID <- rownames(exp)

exp_subset <- exp_merge %>% 
  semi_join(samp_full, by = c("merge_ID" = "merge_ID_inds"))

exp_row_names <- rownames(exp_merge)

# Find the samples in samp_full that are not in the row names of exp_merge
non_matching_samples <- samp_full$merge_ID_inds[!samp_full$merge_ID_inds %in% exp_row_names]

# Display the non-matching samples
# non_matching_samples # all seascape samples here - don't print for space but can view to double check

dim(exp_subset) # all seascape samples

samp_full_subset <- samp_full %>% 
  semi_join(exp_merge, by = c("merge_ID_inds" = "merge_ID"))

dim(samp_full_subset) # all 901 exp + seascape here

# now make sure that our rows line up 
order_vector <- match(samp_full_subset$merge_ID_inds, exp_subset$merge_ID)
print(order_vector)

if (any(is.na(order_vector))) {
  stop("Some IDs in samp_full_subset are not found in exp_subset.")
} # passses

# Reorder exp based on this order
exp_ordered <- exp_subset[order_vector, , drop = FALSE] 

# now check 
geno_names <- (exp_ordered$merge_ID)
samp_names <- (samp_full_subset$merge_ID_inds)

identical(geno_names, samp_names) # TRUE

compare <- geno_names == samp_names
compare # all TRUE


# make sure dimensions line up
if(!(dim(exp_subset)[1]==dim(samp_full_subset)[1])){
  print("Error dim don't line up")
  break
  } # make sure labels line up, passes 7/17/24

# make sure labels line up
if(!identical((exp_ordered$merge_ID),samp_full_subset$merge_ID_inds)){
   print("Error sample names don't line up")
    break
   } # passes
```

```{r}
# look at where our data is missing in rows
hist_rows <- rowSums(is.na(exp_ordered))
hist(hist_rows)
```

```{r}
# remove the merge_ID column from exp_ordered
exp_ordered <- exp_ordered %>% select(-merge_ID)
dim(exp_ordered)
dim(muts) # they have the same SNPs - can use to check imputation

# look at where our data is missing in columns
cols_hist <- colSums(is.na(exp_ordered))
hist(cols_hist)

# Filter out cols with missing data
cols_with_na <- colSums(is.na(exp_ordered)) > 0
exp_cleaned_columns <- exp_ordered[, !cols_with_na]
dim(exp_cleaned_columns) # reduced to 57622 SNPS with no missing data

```

```{r}
# filter the muts database to just keep our SNPs with full data
muts_filtered <- muts[!cols_with_na ,]
dim(muts_filtered) #57622 SNPs
```

```{r}
# checks to make sure that filtered SNP set still aligns 

# store info
geno_names3 <- (colnames(exp_cleaned_columns))
head(geno_names3)
mut_names3 <- (muts_filtered$Affx.ID)
head(mut_names3)

# now check the filtering again
identical(geno_names3, mut_names3) # TRUE

compare <- geno_names3 == mut_names3
# compare # all TRUE when viewing

# make sure labels line up
if(!identical((geno_names3), mut_names3)){
  print("Error sample names don't line up")
  break
} # passes
```

```{r}
# make sure our muts_filtered has type integer
muts_filtered$Chromosome <- as.integer(muts_filtered$Chromosome)
muts_filtered$Position <- as.integer(muts_filtered$Position)
str(muts_filtered$Chromosome)
str(muts_filtered$Position)

muts$Chromosome <- as.integer(muts$Chromosome)
muts$Position <- as.integer(muts$Position)
str(muts_filtered$Chromosome)
str(muts_filtered$Position)

```

```{r}
# make new data frames that are ordered by chromosome position
muts_filt_ord <- muts_filtered[order(muts_filtered$Chromosome, muts_filtered$Position), ]
sorted_order <- order(muts_filtered$Chromosome, muts_filtered$Position)
exp_cleaned_columns_sort<- exp_cleaned_columns[, sorted_order]

muts_imput_check <- muts[order(muts$Chromosome, muts$Position), ]
sorted_order2 <- order(muts$Chromosome, muts$Position)
exp_imput_check<- exp_ordered[, sorted_order2]
```

```{r}
# checks to make sure that filtered SNP set still aligns 

# store info
geno_names4 <- (colnames(exp_cleaned_columns_sort))
head(geno_names4)
mut_names4 <- (muts_filt_ord$Affx.ID)
head(mut_names4)

# now check the filtering again
identical(geno_names4, mut_names4) # TRUE

compare <- geno_names4 == mut_names4
# compare # all TRUE when viewing

# make sure labels line up
if(!identical((geno_names4), mut_names4)){
  print("Error sample names don't line up")
  break
} # passes
```

# now filter for MAF
```{r}
# The geno matrix has individuals in rows and mutations in columns, with a 0, 1, or 2 entered for the number of alternate alleles in the diploid

GEN <- exp_cleaned_columns_sort
GEN_imput_check <- exp_imput_check

# function to calculate the allele frequency of a column
af <- function(i, GEN){
	sum(GEN[,i], na.rm=TRUE)/(2*sum(!is.na(GEN[,i])))
}
af(1, GEN)

# calculate the allele freq of SNPs in the geno mat
snp_afs <- apply(GEN, 2, function(i){
    sum(i, na.rm=TRUE)/(2*sum(!is.na(i)))})

# calculate MAF 
maf_threshold <- 0.05
keep_snps <- names(snp_afs[snp_afs >= maf_threshold & snp_afs <= (1 - maf_threshold)]) # vector with SNPs above 0.05

# now filter the GEN matrix for MAF
GEN_filtered <- GEN[, keep_snps]
dim(GEN_filtered) #43667 SNPs
```

# now filter the muts again to just retain our SNPs with passing MAF
```{r}
# filter the muts data frame to just keep our SNPs with full data

muts_maf_filtered <- muts_filt_ord[muts_filt_ord$Affx.ID %in% keep_snps, ]
dim(muts_maf_filtered)
```


```{r}
gen2 <- add_code256(big_copy(GEN_filtered,type="raw"),code=bigsnpr:::CODE_012)
gen2_imput_check <- add_code256(big_copy(GEN_imput_check,type="raw"),code=bigsnpr:::CODE_012)

# double check our dimensions
dim(GEN_filtered)
dim(muts_maf_filtered)

# check imputation 
gen2_imput_che<- snp_i

# Check the imputed genotypes
head(gen2_imputed)


pca_inv <- snp_autoSVD(gen2,
                       infos.chr= muts_maf_filtered$Chromosome,
                       infos.pos = muts_maf_filtered$Position,
                       thr.r2=0.2,  # correlation LD
                       size= 100, # explore
                   )

thinned_snps <- attr(pca_inv, which="subset")

# pca scores stored in "u"
# pca_inv$d is the eigenvectors
# pca_inv$u is the scores in PCA space
# pca_inv$v is the scores for the loci (not used)

# plot the eigenvectors 
plot(pca_inv$d)

```


```{r}
# extract just the first 6 axes
pca_data <- as.data.frame(pca_inv$u[, 1:6]) 


colnames(pca_data) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")
pca_data$Plate <- samp_full_subset$Plate
pca_data$ID_SiteDate <- samp_full_subset$ID_SiteDate
```

```{r}
# export CSV data
write.csv(pca_data, "/Users/madelineeppley/Desktop/pca_data_exp.csv")
pca_data <- read.csv("/Users/madelineeppley/Desktop/cvi-diploid-files/20240820_filesbackup/pca_data_exp.csv")
```

```{r}
# can update region colors to whatever we want to see here
region_colors <- c(
  "LOLA" = "#369ACC", 
  "ME-HogIs" = "#CBABD1", 
  "NH-GrtBay" = "#9656A2", 
  "FL-KingPlan" = "#F4895F", 
  "LA-SisLake" = "#DE324C", 
  "TX-CapBay" = "#6F1926",
  "VA-DeepWatSh" = "#F8E16F",
  "DEBY" = "#95CF92")

```

# plot PCA by plate to check for batch effects - just see pl6 and pl7 here
```{r}
pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Plate)) +
  geom_point(size = 1) + 
  labs(title = "PCA - Plate Extractions, exp Dataset", x = "PC1", y = "PC2", color = "Plate") +
  theme_minimal()

print(pca_plot)
```

# PCs 3 and 4 checking for batch effects
```{r}
pca_plot2 <- ggplot(pca_data, aes(x = PC3, y = PC4, color = Plate)) +
  geom_point(size = 1) + 
  labs(title = "PCA - Plate Extractions, Whole Dataset", x = "PC3", y = "PC4", color = "Plate") +
  theme_minimal()

print(pca_plot2)
```

# PCA plot PCs 1 and 2
```{r}
centroids <- pca_data %>%
  group_by(ID_SiteDate) %>%
  summarize(PC1 = mean(PC1), PC2 = mean(PC2))

pca_plot3 <- ggplot(pca_data, aes(x = PC1, y = PC2, color = ID_SiteDate)) +
  geom_point(size = 1) +
  stat_ellipse(aes(group = ID_SiteDate), level = 0.95) +
  #geom_text_repel(data = centroids, aes(x = PC1, y = PC2, label = ID_SiteDate), 
                  # size = 3, fontface = "bold", box.padding = 0.5, point.padding = 0.5) +
  #labs(title = "PCA Plot exp Samples", x = "PC1", y = "PC2", color = "Region") +
  scale_color_manual(values = region_colors) +
  theme_minimal()

print(pca_plot3)


```
# now PCs 3 and 4
```{r}
centroids34 <- pca_data %>%
  group_by(ID_SiteDate) %>%
  summarize(PC3 = mean(PC3), PC4 = mean(PC4))

pca_plot4 <- ggplot(pca_data, aes(x = PC3, y = PC4, color = ID_SiteDate)) +
  geom_point(size = 1) +
  stat_ellipse(aes(group = ID_SiteDate), level = 0.95) +
  geom_text_repel(data = centroids34, aes(x = PC3, y = PC4, label = ID_SiteDate), 
                  size = 3, fontface = "bold", box.padding = 0.5, point.padding = 0.5) +
  labs(title = "PCA Plot exp Samples", x = "PC3", y = "PC4", color = "Region") +
  theme_minimal()

print(pca_plot4)
```

# Run the SNMF structure analysis for the experimental samples
```{r}
# subset the last geno matrix that we made (GEN_filtered) with our thinned SNPs list from the auto_SVD function
gen_filtered_subset <- GEN_filtered[, thinned_snps]
region_labels <- samp_full_subset$ID_SiteDate

# now save our matrix as a .geno format to run the snmf analysis
write.geno(gen_filtered_subset, "/Users/madelineeppley/Desktop/cvi-diploid-files/geno_file_exp_all.geno")

# check that our matrix dimensions look ok after save
# geno_check <- read.geno("/Users/madelineeppley/Desktop/cvi-diploid-files/geno_file_exp_all.geno")
# dim(geno_check)
```

```{r}
# now generate the snmf project for our plate
snmf_6 <- snmf("/Users/madelineeppley/Desktop/cvi-diploid-files/geno_file_exp_all.geno", K = 2, repetitions = 10, ploidy = 2, entropy = TRUE, project = "new")
```

```{r}
# plot cross-entropy criterion of all runs of the project
plot(snmf_6, cex = 1.2, col = "lightblue", pch = 19)

# get the cross-entropy of the 10 runs for k=2
ce = cross.entropy(snmf_6, K = 2)

# select the run with the lowest cross-entropy for k=2
best = which.min(ce)

# display the q-matrix

my.colors <- c("#FF9B71", "#E84855")
```

Plot the ancestry proportions
```{r ancestry-plot}
barchart(snmf_6, K = 2, run = best, 
        border = NA, space = 0, col = my.colors, 
        xlab = "Individuals", ylab = "Ancestry proportions", 
        main = "Ancestry matrix for Experimental Samples") -> bp

reordered_labels <- region_labels[bp$order]
        
axis(1, at = 1:length(bp$order), 
    labels = reordered_labels, las = 3, cex.axis = .4)

```

# create snmf plot results on map 
```{r}
# load US coastline 
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

# read site data
site_data <- read.csv("/Users/madelineeppley/Desktop/MVP23-FieldBags\ -\ spawn_trt2.csv")

# get the Q-matrix from snmf results
geno <- read.geno("/Users/madelineeppley/Desktop/cvi-diploid-files/geno_file_exp_all.geno")
snmf_project <- load.snmfProject("/Users/madelineeppley/Desktop/cvi-diploid-files/geno_file_exp_all.snmfProject")
best_run <- which.min(cross.entropy(snmf_project, K = 2))  # choose the best run
Q_matrix <- Q(snmf_project, K = 2, run = best_run)  # extract best run into q-matrix


# pull the site and region labels and merge them with the ancestry proportions
region_labels <- samp_full_subset$ID_SiteDate
region_labels <- as.data.frame(region_labels)
colnames(region_labels) <- "ID_SiteDate"
region_labels <- samp_full_subset[, c("ID_SiteDate", "merge_ID_inds")]
region_labels$Q1 <- Q_matrix[, 1]  # q matrix ancestry group 1
region_labels$Q2 <- Q_matrix[, 2]  # q matrix ancestry group 2

colnames(site_data)[2] = "ID_SiteDate"

# now we have an issue where VA-JR-DeepWatSh and VA-DeepWatSh don't match ... let's rename
site_data$ID_SiteDate <- gsub('VA-JR-DeepWatSh','VA-DeepWatSh', site_data$ID_SiteDate)


# Merge ancestry proportions into the full data
merged_data <- merge(site_data, samp_full_subset, by = "ID_SiteDate")
merged_data <- merge(region_labels, merged_data, by = "merge_ID_inds", all.x = TRUE)
```

# now plot on the map
```{r}
# create a base map with our coastline as the outline of the plot
base_map <- ggplot(data = coastline) +
  geom_sf() +
  theme_minimal() +
  labs(
    title = "Ancestry Proportions by Sampling Site",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(xlim = c(-100, -60), ylim = c(24, 50), expand = FALSE)

base_map


# now make site averages of our ancestry proportions 

site_data_averaged <- merged_data %>%
  group_by(ID_SiteDate.x) %>%
  summarise(Q1_avg = mean(Q1), Q2_avg = mean(Q2),
            Longitude = first(longitudeDecimal),
            Latitude = first(latitudeDecimal))   

map_with_sites <- base_map + 
  geom_scatterpie(
    data = site_data_averaged,
    aes(x = Longitude, y = Latitude, r = 0.9),  
    cols = c("Q1_avg", "Q2_avg"), 
    color = NA,
    pie_scale = 2.0 
  ) +  
  scale_fill_manual(values = c(
    "Q1_avg" = "#FF9B71", 
    "Q2_avg" = "#E84855"  
  )) +  
  theme(
    legend.position = "right", 
    panel.grid = element_blank(), 
    panel.background = element_blank(), 
    axis.line = element_line(color = "black")
  )

print(map_with_sites)



# we need to add some arbitrary coordinates for the VA populations or they will all plot on top of each other. let's put them in order from south to north, Deep Water Shoal, DEBY (York Riv), LOLA (Lew Riv)
site_data_averaged[site_data_averaged$ID_SiteDate.x == "VA-DeepWatSh", "Latitude"] <- 35.61813
site_data_averaged[site_data_averaged$ID_SiteDate.x == "VA-DeepWatSh", "Longitude"] <- -74.29945
site_data_averaged[site_data_averaged$ID_SiteDate.x == "DEBY", "Latitude"] <- 36.97853
site_data_averaged[site_data_averaged$ID_SiteDate.x == "DEBY", "Longitude"] <- -72.95174
site_data_averaged[site_data_averaged$ID_SiteDate.x == "LOLA", "Latitude"] <- 38.8223
site_data_averaged[site_data_averaged$ID_SiteDate.x == "LOLA", "Longitude"] <- -73.19229

map_with_sites <- base_map + 
  geom_scatterpie(
    data = site_data_averaged,
    aes(x = Longitude, y = Latitude, r = 0.9),  
    cols = c("Q1_avg", "Q2_avg"), 
    color = NA,
    pie_scale = 1.5 
  ) +  
  scale_fill_manual(values = c(
    "Q1_avg" = "#FF9B71", 
    "Q2_avg" = "#E84855"  
  )) +  
  theme(
    legend.position = "right", 
    panel.grid = element_blank(), 
    panel.background = element_blank(), 
    axis.line = element_line(color = "black")
  )

print(map_with_sites)


```


