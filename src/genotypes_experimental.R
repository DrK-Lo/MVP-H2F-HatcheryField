###################
## Genotype Data ## ####
###################

# setup
#######
# set wd
setwd("/work/lotterhos/MVP_Genotypes_DataMerge")

# packages
library(tidyverse) # for data wrangling
library(dplyr) # for data wrangling
library(pedtools) # for working with ped files
library(vcfR) # for working with genotype data
library(openxlsx) # for excel files
library(purrr) # for reduce function
library(LEA) 
library(parallel)
library(tidyr)
library(bigsnpr)
library(stringr)

# read in performance data
pl6_performance <- read.table("raw_data/genotypes/SNP_QC/Ps.performance_PlDNA_6.txt", header = T)
pl7_performance <- read.table("raw_data/genotypes/SNP_QC/Ps.performance_PlDNA_7.txt", header = T)


# read in vcf files - come back to this once vcf files are generated for all plates
pl6_vcf <- read.vcfR("raw_data/large_data/PlDNA_6_genotypes/NE_Marine_Science_OysterCVI200K_PlDNA_6_VCF.vcf")
pl7_vcf <- read.vcfR("raw_data/large_data/PlDNA_7_genotypes/NE_Marine_Science_OysterCVI200K_PlDNA_7_VCF.vcf")
#######

# Mutation Markers - DO NOT RERUN unless adding new plates - MGE 8/19/2024 ####

# read in snp chip info
snp_chip_raw <- read.xlsx("raw_data/genotypes/SNP_chip/selected_markers277K_20220322.xlsx")

# check data
head(snp_chip_raw)
dim(snp_chip_raw) # start with 277138 SNPs

# filter the SNP chip mutations for duplicates
dup_snps <- which(duplicated(snp_chip_raw$Affx.ID))
length(dup_snps) # we have 679 duplicated SNPs

snp_chip_no_dups <- snp_chip_raw[- dup_snps ,]
dim(snp_chip_no_dups) # looks good, now we have 276459 SNPs

snp_chip_no_disease <- snp_chip_no_dups[snp_chip_no_dups$organism == "Crassostrea virginica", ] # keep just our oyster SNPs, remove disease SNPs
dim(snp_chip_no_disease) # now we have 275703 SNPs

snp_chip_filtered <- snp_chip_no_disease[snp_chip_no_disease$Chromosome != "MT", ] # remove chromosomes with MT
dim(snp_chip_filtered) # 275671 SNPs
range(snp_chip_filtered$Chromosome) # data checks out, we only have chromosomes 1-10

# order the filtered data frame
snp_chip <- snp_chip_filtered[order(snp_chip_filtered$Affx.ID), ] # now order by the Affx.ID

# checks
snp_chip$mutID = paste(snp_chip$Chromosome, snp_chip$Position, sep = "_")
check <- which(duplicated(snp_chip$mutID)) #empty

# save as an .RDS object
saveRDS(snp_chip, "merged_data/genotypes/20240719_mutmatrix.rds")


# Selected Markers ####

# check data
snp_chip <- readRDS("merged_data/genotypes/20240719_mutmatrix.rds")
head(snp_chip)
tail(snp_chip)
dim(snp_chip)

snp_chip$mutID = paste(snp_chip$Chromosome, snp_chip$Position, sep = "_")

# prepare the for loop

vcf_obj <- c(paste0("pl", c(6:7), "_vcf"))
print(vcf_obj)
length(vcf_obj)

snp_chip$order = 1:nrow(snp_chip)
snp_chip_full = snp_chip

# Combine VCFs ####

for (i in (1:length(vcf_obj))){
  print(c(i, vcf_obj[i]))
  pl_vcf = get(vcf_obj[i]) # get the object that is stored in the character vector
  str(pl_vcf)
  ext_vcfgt_num2 <- extract.gt(pl_vcf, as.numeric = FALSE) # extract the genotype matrix from the vcf fil
  str(ext_vcfgt_num2)
  # "0/0" --> 0
  # "0/1" or "1/0" --> 1
  # "1/1" is --> 2
  FirstAllele<- matrix(as.numeric(substring(ext_vcfgt_num2,1,1)), ncol=ncol(ext_vcfgt_num2))
  SecondAllele<- matrix(as.numeric(substring(ext_vcfgt_num2,3,3)), ncol=ncol(ext_vcfgt_num2))
  Genotype <- FirstAllele + SecondAllele
  str(Genotype)
  print(dim(Genotype))
  print(table(Genotype))
  sum(is.na(Genotype))
  hist(colSums(is.na(Genotype)), breaks=seq(0,10000,10), main=paste(vcf_obj[i],"Missing loci per individual"))
  hist(rowSums(is.na(Genotype)), breaks=seq(0, max(rowSums(is.na(Genotype))), 1), main=paste(vcf_obj[i],"Missing individuals per locus"))
  colnames(Genotype) <- colnames(ext_vcfgt_num2)
  rownames(Genotype) <- rownames(ext_vcfgt_num2)
  ext_vcfgt_num3 = as.data.frame(Genotype) # create the data frame
  ext_vcfgt_num3$Name = row.names(ext_vcfgt_num2) # add the affymetrix name as the key for merger
  snp_chip_full2 <- merge(snp_chip_full, ext_vcfgt_num3, by.x = "Name", by.y = "Name", all.x = TRUE) # create snp_chip_full2 from _full
  if(! (ncol(snp_chip_full2) ==  ncol(snp_chip_full) + ncol(ext_vcfgt_num2))) {
    print("error columns don't match")
    break()
  }
  snp_chip_full = snp_chip_full2 # reset snp_chip_full2 to just snp_chip_full
}


##########################################################################################

##################################### now check column names ##################################

plate6_columns <- colnames(snp_chip_full)[grepl("PlDNA_6", colnames(snp_chip_full))]
print(plate6_columns)

plate7_columns <- colnames(snp_chip_full)[grepl("PlDNA_7", colnames(snp_chip_full))]
print(plate7_columns)

###############################################################################################


# for loop ran! - the merged file should be stored as snp_chip_full
dim(snp_chip_full) # 205 inds in the data set
class(snp_chip_full)

# save this output file
snp_chip_full3 <- write.csv(snp_chip_full, "merged_data/large_data/20250203_allexp_vcf.csv")



# Data formatting - read in full CSV here - MGE #### 
geno_matrix <- read.csv("/work/lotterhos/MVP_Genotypes_DataMerge/merged_data/large_data/20250203_allexp_vcf.csv") # read in geno matrix to work with here
head(geno_matrix, n = 3) # check head
dim(geno_matrix) # check dimensions - rows contain SNP info
head(colnames(geno_matrix), n = 30) # check column names - contains individuals


# now back to the geno_matrix, keep just the first part of the column name before '.CEL', this gets rid of the double name
extract_first_part <- function(name) {
  return(sub("(.CEL.*$)", "", name))
}

# Apply the function to columns 21 to 206 (this is where our individual columns are) in our geno_matrix and wells_df
colnames(geno_matrix)[21:206] <- sapply(colnames(geno_matrix)[21:206], extract_first_part)

# We have the well location at the beginning and end. Remove the one at the end
colnames(geno_matrix) <- gsub("_[A-Z0-9]+$", "", colnames(geno_matrix))

# Check the updated column names
head(colnames(geno_matrix), n = 30)
tail(colnames(geno_matrix), n = 30)
head(geno_matrix[21:26])

# set our row names as the Affx.ID
rownames(geno_matrix) <- geno_matrix$Affx.ID

# Make a filtered matrix that just contains our SNP info - remove mut info and just keep the SNP data
filtered_matrix <- geno_matrix[21:206]
dim(filtered_matrix) # removed first 20 cols with the mut info

# transpose the data frame columns to rows
geno_flip <- as.data.frame(t(filtered_matrix))

# check the shape of the data
head(rownames(geno_flip), n = 30)
head(colnames(geno_flip), n = 30)
geno_flip[1:5, 1:5] # looks good here


# making a merge key ####

# Add row names as a new column "merge_key"
row_names <- rownames(geno_flip)
geno_flip$merge_key <- row_names
dim(geno_flip)

geno_flip <- geno_flip %>%
  select(merge_key, everything()) # put the merge key at the front

# Check the new column order
head(geno_flip[1:5, 1:5]) # check
head(geno_flip[1]) # moved ok

# split the names - but KEEP sharedLotterhosPuritz
geno_flip_split1 <- str_split_fixed(geno_flip$merge_key, "__", 3) # break into 3 pieces
geno_flip_split5 <- str_split_fixed(geno_flip_split1[,1], "_Replace", 2)
geno_flip_split2 <- str_split_fixed(geno_flip_split1[,3], "_", 2) # code split
geno_flip_split3 <- str_split_fixed(geno_flip_split1[,2], "_", 2)
geno_flip_split4 <- str_split_fixed(geno_flip_split3[,1], ".sharedLotterhosPuritz", 2)
geno_flip_split6 <- str_split_fixed(geno_flip_split5[,1], "_", 2)
geno_flip_split7 <- str_split_fixed(geno_flip_split6[,2], "PlDNA_", 2)
geno_flip_split7[, 2] <- paste0("CviMVP_PlDNA_", geno_flip_split7[, 2])
merge_ID <- paste(geno_flip_split6[, 1], geno_flip_split7[, 2], geno_flip_split3[, 1], geno_flip_split3[, 2], geno_flip_split2[, 1], sep = "_")
head(merge_ID)

# the merge_ID list for the double underscore "__" and period between LotterhosPuritz "."
merge_ID <- gsub("__", "_", merge_ID)
merge_ID <- gsub("\\.", "-", merge_ID)
merge_ID <- gsub("__$", "", merge_ID)
head(merge_ID)
print(merge_ID) # this prints all merge_IDs in case you need to check through all

# add it back into geno_flip
geno_flip_full <- cbind(merge_ID, geno_flip)
print(geno_flip_full[1:5, 1:5])

# remove old merge_key and the seq_well cols 
geno_flip_full <- geno_flip_full[, -which(colnames(geno_flip_full) == "merge_key")]

print(geno_flip_full[1:5, 1:5])
rownames(geno_flip_full) <- NULL # set the rownames back to null
rownames(geno_flip_full) <- geno_flip_full$merge_ID
print(geno_flip_full[1:5, 1:5])

# remove the merge_ID column
geno_flip_full <- geno_flip_full[, -which(colnames(geno_flip_full) == "merge_ID")]
print(geno_flip_full[1:5, 1:5]) # check, looks good

# order the genotype matrix by the Affx-ID
ordered_columns <- order(colnames(geno_flip_full))
geno_flip_full_ordered <- geno_flip_full[, ordered_columns]
print(geno_flip_full_ordered[1:5, 1:5])

# Ordering - check if the order of the column names matches the order of the mutation matrix ####
snp_chip <- readRDS("merged_data/genotypes/20240719_mutmatrix.rds")
order_vector <- match(colnames(geno_flip_full_ordered), snp_chip$Affx.ID)
print(order_vector)

if (any(is.na(order_vector))) {
  stop("Some IDs in geno mat are not found in mut mat.")
} # passes

# now check 
geno_names <- (colnames(geno_flip_full_ordered))
head(geno_names)
mut_names <- (snp_chip$Affx.ID)
head(mut_names)

identical(geno_names, mut_names) # TRUE

compare <- geno_names == mut_names
compare # all TRUE


# make sure labels line up
if(!identical((geno_names), mut_names)){
  print("Error sample names don't line up")
  break
} # passes


# save unfiltered file ####
saveRDS(geno_flip_full_ordered, "merged_data/genotypes/20250203_filtered_ordered_exp.rds")


# Filter NA data ####
# now remove columns that have 100% NA data
# read in geno_matrix or save as new var
# matrix_to_filter <- geno_flip_full_ordered
matrix_to_filter <- readRDS("/work/lotterhos/MVP_Genotypes_DataMerge/merged_data/genotypes/20250203_filtered_ordered_exp.rds")
ncol(matrix_to_filter) # count the number of columns - starting with 275671

# find columns with all NAs by counting # of NAs, compare to total # of rows
na_columns <- colSums(is.na(matrix_to_filter)) == nrow(matrix_to_filter)

# Remove columns with all NA values from the genotype matrix
fil_mat <- matrix_to_filter[, !na_columns]
dim(fil_mat) # down to 194970 columns (SNPs)
fil_mat[1:5, 1:5]

# Remove those same SNPs from the mutation matrix
fil_mat_mut <- snp_chip[!na_columns ,]
dim(fil_mat_mut) # 194970 rows (SNPs)
fil_mat_mut[1:5, 1:5]

# now check the filtering again
geno_names2 <- (colnames(fil_mat))
head(geno_names2)
mut_names2 <- (fil_mat_mut$Affx.ID)
head(mut_names2)

identical(geno_names2, mut_names2) # TRUE

compare <- geno_names2 == mut_names2
compare # all TRUE

# make sure labels line up
if(!identical((geno_names2), mut_names2)){
  print("Error sample names don't line up")
  break
} # passes


# save BOTH as an RDS file ####
saveRDS(fil_mat, "merged_data/genotypes/20250203_genomatrix_PASS_exp.rds")
saveRDS(fil_mat_mut, "merged_data/genotypes/20250203_mutmatrix_PASS_exp.rds")

# subset to just experimental inds and remove seascape inds ####
full_rds <- readRDS("/work/lotterhos/MVP_Genotypes_DataMerge/merged_data/genotypes/20250203_genomatrix_PASS_exp.rds")
full_rds[1:5, 1:5]

# experimental samples vector
experimental_sample <- grepl("_D", rownames(full_rds))

# Create new experimental matrix
experimental_matrix <- full_rds[experimental_sample, ]

# save as .RDS datasets
saveRDS(experimental_matrix, "merged_data/genotypes/20250203_experimentalmatrix.rds")


# Individual data format ####
inds_matrix <- read.csv("merged_data/individual/20241125_merged_inds_all.csv") # read in our csv from the indiv_merge.R file
inds_matrix <- inds_matrix[, -1]
dim(inds_matrix)

# remove "replace" from the plate col - this was causing issues in geno matrix merge - MGE 9/23/2024
inds_matrix_rep <- str_split_fixed(inds_matrix$Plate, "_Replace", 2)

# now we need to make our merge ID using the seq_well (column 9) - CHANGED THIS 11/4/24 since PlDNA 5 was fixed, now use seq_well MGE
merge_ID_inds <- paste(inds_matrix[, 4], "CviMVP", inds_matrix_rep[, 1], inds_matrix[, 1], inds_matrix[, 2], sep = "_")
head(merge_ID_inds)

# add into inds matrix
inds_matrix <- cbind(merge_ID_inds, inds_matrix)

# save as an RDS file ####
saveRDS(inds_matrix, "merged_data/individual/20250203_indsmatrix.rds")

# now subset the RDS object into just experimental
exp_rds_mat <- inds_matrix[grepl("CviMVPe", inds_matrix$Vial_Label), ]

saveRDS(exp_rds_mat, "merged_data/individual/20250203_experimental_indsmatrix.rds")

###############################################################################################################
inds <- readRDS("merged_data/individual/20250203_experimental_indsmatrix.rds") # inds data frame with full metadata
muts <- readRDS("merged_data/genotypes/20250203_mutmatrix_PASS_exp.rds") # mutation data frame with SNP info
experimental <- readRDS("merged_data/genotypes/20250203_experimentalmatrix.rds") # genotype matrix with exp samples only

exp <- experimental
samp_full <- inds

# step 1: have to subset just the seascape samples, otherwise dimensions don't line up ####
dim(exp) # 154 samples
dim(samp_full) # all exp samples totals 248 (incl. not sequenced individuals)

exp_merge <- exp
exp_merge$merge_ID <- rownames(exp)

exp_subset <- exp_merge %>% 
  semi_join(samp_full, by = c("merge_ID" = "merge_ID_inds"))

exp_row_names <- rownames(exp_merge)

# Find the samples in samp_full that are not in the row names of exp_merge
non_matching_samples <- samp_full$merge_ID_inds[!samp_full$merge_ID_inds %in% exp_row_names]

# Display the non-matching samples
non_matching_samples # all plate 11 juvenile samples - this is ok

dim(exp_subset) # all 154 experimental samples 

samp_full_subset <- samp_full %>% 
  semi_join(exp_merge, by = c("merge_ID_inds" = "merge_ID"))

dim(samp_full_subset) # all 154 experimental samples here

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


# look at where our data is missing in rows
hist_rows <- rowSums(is.na(exp_ordered))
hist(hist_rows)


# remove the merge_ID column from exp_ordered
exp_ordered <- exp_ordered %>% select(-merge_ID)
dim(exp_ordered) #194970
dim(muts) #194970, matches

# look at where our data is missing in columns
cols_hist <- colSums(is.na(exp_ordered))
hist(cols_hist)

# Filter out cols with missing data
missing_threshold <- 0.10  #10%
snp_missing_prop <- colMeans(is.na(exp_ordered))
exp_cleaned_columns <- exp_ordered[, snp_missing_prop <= missing_threshold]
dim(exp_cleaned_columns) # removed 2976 SNPs

# Filter out the rows with missing data
ind_missing_prop <- rowMeans(is.na(exp_ordered))
inds_missing <- which(ind_missing_prop > missing_threshold) # length 0, all inds pass

# filter the muts database to just keep our SNPs that meet the missingness threshold
muts_filtered <- muts[snp_missing_prop <= missing_threshold, ]
dim(muts_filtered) #191994 SNPs

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

# make sure our muts_filtered has type integer
muts_filtered$Chromosome <- as.integer(muts_filtered$Chromosome)
muts_filtered$Position <- as.integer(muts_filtered$Position)
str(muts_filtered$Chromosome)
str(muts_filtered$Position)

muts$Chromosome <- as.integer(muts$Chromosome)
muts$Position <- as.integer(muts$Position)
str(muts_filtered$Chromosome)
str(muts_filtered$Position)

# make new data frames that are ordered by chromosome position
muts_filt_ord <- muts_filtered[order(muts_filtered$Chromosome, muts_filtered$Position), ]
sorted_order <- order(muts_filtered$Chromosome, muts_filtered$Position)
exp_cleaned_columns_sort<- exp_cleaned_columns[, sorted_order]

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


# The geno matrix has individuals in rows and mutations in columns, with a 0, 1, or 2 entered for the number of alternate alleles in the diploid

GEN <- exp_cleaned_columns_sort

# function to calculate the allele frequency of a column
af <- function(i, GEN){
  sum(GEN[,i], na.rm=TRUE)/(2*sum(!is.na(GEN[,i])))
}
# af(1, GEN)

# calculate the allele freq of SNPs in the geno mat
snp_afs <- apply(GEN, 2, function(i){
  sum(i, na.rm=TRUE)/(2*sum(!is.na(i)))})

# calculate MAF 
maf_threshold <- 0.05
keep_snps <- names(snp_afs[snp_afs >= maf_threshold & snp_afs <= (1 - maf_threshold)]) # vector with SNPs above 0.05

# now filter the GEN matrix for MAF
GEN_filtered <- GEN[, keep_snps]
dim(GEN_filtered) #154609 SNPs

# filter the muts data frame to just keep our SNPs with full data
muts_maf_filtered <- muts_filt_ord[muts_filt_ord$Affx.ID %in% keep_snps, ]
dim(muts_maf_filtered) #154609 SNPs
saveRDS(muts_maf_filtered, file = "merged_data/genotypes/20250203_FULLmuts_exp.rds")

# Impute #####################################################################
# use snmf on our dataset of 154k SNPs
write.lfmm(GEN_filtered, "merged_data/genotypes/20250203_genotypes_exp.lfmm")

exp.project.snmf <- snmf("merged_data/genotypes/20250203_genotypes_exp.lfmm", K = 2, repetitions = 10, ploidy = 2, entropy = TRUE, project = "new")

# run with the lowest cross-entropy value and impute
best = which.min(cross.entropy(exp.project.snmf, K = 2))
impute(exp.project.snmf, "merged_data/genotypes/20250203_genotypes_exp.lfmm", method = 'mode', K = 2, run = best)

# store the imputed SNP set as a matrix 
GEN_imputed <- as.data.frame(read.table("merged_data/genotypes/20250203_genotypes_exp.lfmm_imputed.lfmm", header = FALSE))
saveRDS(GEN_imputed, file = "merged_data/genotypes/20250203_FULLSNPs_exp.rds") # this is the FULL SNP set

GEN_imputed <- readRDS("/Users/madelineeppley/Desktop/20250203_FULLSNPs_exp.rds")
gen_impute <- add_code256(big_copy(GEN_imputed,type="raw"),code=bigsnpr:::CODE_012)

# double check our dimensions
muts_maf_filtered <- readRDS("/Users/madelineeppley/Desktop/20250203_FULLmuts_exp.rds")
dim(GEN_imputed)
dim(muts_maf_filtered) # matches

thr.r2 <- 0.2
# create the thinned SNP set 
pca_inv <- snp_autoSVD(gen_impute,
                       infos.chr= muts_maf_filtered$Chromosome,
                       infos.pos = muts_maf_filtered$Position,
                       thr.r2=thr.r2,  # correlation LD
                       size= 100/thr.r2, # explore
)

thinned_snps <- attr(pca_inv, which="subset") # SNPs n=104958
saveRDS(thinned_snps, file = "/Users/madelineeppley/Desktop/20250203_THINNEDSNPs_exp.rds")

# plot the eigenvectors 
plot(pca_inv$d)

# extract just the first 6 axes
pca_data <- as.data.frame(pca_inv$u[, 1:6]) 

colnames(pca_data) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")

samp_full_subset <- readRDS("/Users/madelineeppley/Desktop/20250203_samp_full_subset_exp.rds")
pca_data$Plate <- samp_full_subset$Plate
pca_data$ID_SiteDate <- samp_full_subset$ID_SiteDate

# export CSV data
write.csv(pca_data, "/Users/madelineeppley/Desktop/20250203_pca_data_exp.csv")
