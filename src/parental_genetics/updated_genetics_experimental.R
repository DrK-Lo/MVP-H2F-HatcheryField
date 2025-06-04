###################
## Genotype Data ## ####
###################

# setup
#######
# set wd
# setwd("/projects/lotterhos/MVP_Genotypes_DataMerge")
setwd("/Users/madelineeppley/Desktop/cvi25files")

## MGE note on use in local environment vs population genomics environment on explorer cluster
# After switching from discovery to explorer, I needed to install the following dependencies to make these packages work 
# tzdb, readr, and rlang

# packages
library(tidyverse) # for data wrangling
library(dplyr) # for data wrangling
library(pedtools) # for working with ped files
library(vcfR) # for working with genotype data
library(openxlsx) # for excel files
library(readxl) # testing this to replace openxlsx 5/13/15 MGE
library(purrr) # for reduce function
library(LEA) # for snmf and imputation
library(tidyr) # for data wrangling
library(bigsnpr) # for PCA and LD filtering
library(WGCNA) # use transpose function for big matrices

# read in performance data
#pl_all_performance <- read.table("") we don't have a performance file, waiting on response from Jeremy in April 2025
#pl1_performance <- read.table("raw_data/genotypes/SNP_QC/Ps.performance_PlDNA_1.txt", header = T)
#pl2_performance <- read.table("raw_data/genotypes/SNP_QC/Ps.performance_PlDNA_2.txt", header = T)
#pl3_performance <- read.table("raw_data/genotypes/SNP_QC/Ps.performance_PlDNA_3.txt", header = T)
#pl4_performance <- read.table("raw_data/genotypes/SNP_QC/Ps.performance_PlDNA_4.txt", header = T)
#pl6_performance <- read.table("raw_data/genotypes/SNP_QC/Ps.performance_PlDNA_6.txt", header = T)
#pl7_performance <- read.table("raw_data/genotypes/SNP_QC/Ps.performance_PlDNA_7.txt", header = T)
#pl8_performance <- read.table("raw_data/genotypes/SNP_QC/Ps.performance_PlDNA_8.txt", header = T)
#pl9_performance <- read.table("raw_data/genotypes/SNP_QC/Ps.performance_PlDNA_9.txt", header = T)
#pl10_performance <- read.table("raw_data/genotypes/SNP_QC/Ps.performance_PlDNA_10.txt", header= T)


# read in vcf files - come back to this once vcf files are generated for all plates
#pl1_vcf <- read.vcfR("raw_data/large_data/PlDNA_1_genotypes/NE_Marine_Science_OysterCVI200K_PlDNA_1_PLINK.vcf")
#pl2_5_vcf <- read.vcfR("raw_data/large_data/PlDNA_5_genotypes/NE_Marine_Science_OysterCVI200K_PlDNA_2_5_PLINK.vcf")
#pl2_5_vcf <- read.vcfR("raw_data/large_data/PlDNA_5_genotypes/NE_Marine_Science_OysterCVI200K_REUPLOAD_PlDNA_5_VCF.vcf")
#pl3_vcf <- read.vcfR("raw_data/large_data/PlDNA_3_genotypes/NE_Marine_Science_OysterCVI200K_PlDNA_3_VCF.vcf")
#pl4_vcf <- read.vcfR("raw_data/large_data/PlDNA_4_genotypes/NE_Marine_Science_OysterCVI200K_PlDNA_4_VCF.vcf")
#pl6_vcf <- read.vcfR("raw_data/large_data/PlDNA_6_genotypes/NE_Marine_Science_OysterCVI200K_PlDNA_6_VCF.vcf")
#pl7_vcf <- read.vcfR("raw_data/large_data/PlDNA_7_genotypes/NE_Marine_Science_OysterCVI200K_PlDNA_7_VCF.vcf")
#pl8_vcf <- read.vcfR("raw_data/large_data/PlDNA_8_genotypes/NE_Marine_Science_OysterCVI200K_PlDNA_8_VCF.vcf")
pl9_vcf <- read.vcfR("/Users/madelineeppley/Desktop/cvi25files/NE_Marine_Science_OysterCVI200K_20240301_VCF.vcf")
#pl10_vcf <- read.vcfR("raw_data/large_data/PlDNA_10_genotypes/NE_Marine_Science_OysterCVI200K_PlDNA_10_VCF.vcf")
#pl11_vcf <- read.vcfR("raw_data/large_data/PlDNA_11_genotypes/NE_Marine_Science_OysterCVI200K_PlDNA_11_VCF.vcf")
#######

# read VCF
plall_vcf <- read.vcfR("/Users/madelineeppley/Desktop/cvi25files/NE_Marine_Science_OysterCVI200K_All_Plates_VCF.vcf")
# we have 336 columns (inds) and 195220K SNPs in this file

str(plall_vcf)
head(plall_vcf@gt) # get the genotype information - there are some NAs in here 

###### 
# Mutation Markers - DO NOT RERUN unless adding new plates - MGE 8/19/2024 ####

# read in snp chip info
#snp_chip_raw <- read_excel("/Users/madelineeppley/Desktop/selected_markers277K_20220322.xlsx")

# check data
#head(snp_chip_raw)
#dim(snp_chip_raw) # start with 277138 SNPs

# filter the SNP chip mutations for duplicates
#dup_snps <- which(duplicated(snp_chip_raw$Affx.ID))
#length(dup_snps) # we have 679 duplicated SNPs

#snp_chip_no_dups <- snp_chip_raw[- dup_snps ,]
#dim(snp_chip_no_dups) # looks good, now we have 276459 SNPs

#snp_chip_no_disease <- snp_chip_no_dups[snp_chip_no_dups$organism == "Crassostrea virginica", ] # keep just our oyster SNPs, remove disease SNPs
#dim(snp_chip_no_disease) # now we have 275703 SNPs

#snp_chip_filtered <- snp_chip_no_disease[snp_chip_no_disease$Chromosome != "MT", ] # remove chromosomes with MT
#dim(snp_chip_filtered) # 275671 SNPs
#range(snp_chip_filtered$Chromosome) # data checks out, we only have chromosomes 1-10

# order the filtered data frame
#snp_chip <- snp_chip_filtered[order(snp_chip_filtered$Affx.ID), ] # now order by the Affx.ID

# checks
#snp_chip$mutID = paste(snp_chip$Chromosome, snp_chip$Position, sep = "_")
#check <- which(duplicated(snp_chip$mutID)) #empty

# save as an .RDS object
#saveRDS(snp_chip, "/Users/madelineeppley/Desktop/20240719_mutmatrix.rds") # use this same mutmatrix


# Selected Markers ####

snp_chip <- readRDS("/Users/madelineeppley/Desktop/cvi25files/20240719_mutmatrix.rds")
head(snp_chip)
tail(snp_chip)
dim(snp_chip)

snp_chip$mutID = paste(snp_chip$Chromosome, snp_chip$Position, sep = "_")

vcf_obj <- c("pl9_vcf", "plall_vcf")
print(vcf_obj)
length(vcf_obj)

snp_chip$order = 1:nrow(snp_chip)
snp_chip_full = snp_chip


################################### trial new loop ################################

for (i in 1:length(vcf_obj)) {
  print(c(i, vcf_obj[i]))
  
  # Get the VCF object
  pl_vcf <- get(vcf_obj[i])
  ext_vcfgt_num2 <- extract.gt(pl_vcf, as.numeric = FALSE)  # Extract genotype matrix from VCF
  
  # Convert genotype calls (0/0 -> 0, 0/1 or 1/0 -> 1, 1/1 -> 2)
  FirstAllele <- matrix(as.numeric(substring(ext_vcfgt_num2, 1, 1)), ncol = ncol(ext_vcfgt_num2))
  SecondAllele <- matrix(as.numeric(substring(ext_vcfgt_num2, 3, 3)), ncol = ncol(ext_vcfgt_num2))
  Genotype <- FirstAllele + SecondAllele
  
  # Assign VCF sample names to Genotype
  colnames(Genotype) <- colnames(ext_vcfgt_num2)
  rownames(Genotype) <- rownames(ext_vcfgt_num2)
  
  # Sort the wells in the desired order (A10, A11, A12, A1, A2, A3, etc.)
  sequenced_inds_split <- str_split_fixed(colnames(Genotype), ".CEL", 2)
  sequenced_inds_split2 <- str_split_fixed(sequenced_inds_split[,1], "_", 12)
  
  seq_wells <- sequenced_inds_split2[,11]
  seq_wells[which(!sequenced_inds_split2[,12] == "")] <- sequenced_inds_split2[which(!sequenced_inds_split2[,12] == ""),12]
  
  # Generate new names with sequencing well info
  new_names <- paste(seq_wells, colnames(Genotype), sep = "_")
  
  # Custom sorting logic to ensure A10, A11, A12 come first, followed by A1, A2, etc.
  well_order <- substr(seq_wells, 2, 4)  # Extract numerical part of the well
  row_order <- substr(seq_wells, 1, 1)  # Extract the row letter (A, B, etc.)
  
  # Sort the column names based on the custom well sorting
  sorted_indices <- order(row_order, as.numeric(well_order))
  Genotype <- Genotype[, sorted_indices]
  
  # Update column names to reflect the sorted order
  colnames(Genotype) <- new_names[sorted_indices]
  
  # Merge with snp_chip_full and check column counts
  ext_vcfgt_num3 <- as.data.frame(Genotype)
  ext_vcfgt_num3$Name <- row.names(ext_vcfgt_num2)
  snp_chip_full2 <- merge(snp_chip_full, ext_vcfgt_num3, by.x = "Name", by.y = "Name", all.x = TRUE)
  
  if (!(ncol(snp_chip_full2) == ncol(snp_chip_full) + ncol(ext_vcfgt_num2))) {
    print("Error: columns don't match")
    break
  }
  
  snp_chip_full <- snp_chip_full2  # Update snp_chip_full for the next iteration
}



##################################### now check column names ##################################
plate2_columns <- colnames(snp_chip_full)[grepl("PlDNA_2", colnames(snp_chip_full))] 
print(plate2_columns) #87 samples

plate3_columns <- colnames(snp_chip_full)[grepl("PlDNA_3", colnames(snp_chip_full))] 
print(plate3_columns) #94 samples

plate4_columns <- colnames(snp_chip_full)[grepl("PlDNA_4", colnames(snp_chip_full))] 
print(plate4_columns) #84 samples

# Check for Plate 5
plate5_columns <- colnames(snp_chip_full)[grepl("PlDNA_5", colnames(snp_chip_full))]
print(plate5_columns) #88 samples here now 6/3/25

plate6_columns <- colnames(snp_chip_full)[grepl("PlDNA_6", colnames(snp_chip_full))]
print(plate6_columns) #95 samples

plate7_columns <- colnames(snp_chip_full)[grepl("PlDNA_7", colnames(snp_chip_full))] 
print(plate7_columns) #90 samples

plate8_columns <- colnames(snp_chip_full)[grepl("PlDNA_8", colnames(snp_chip_full))] 
print(plate8_columns) #93 samples

plate10_columns <- colnames(snp_chip_full)[grepl("PlDNA_10", colnames(snp_chip_full))] 
print(plate10_columns) #94 samples

plate11_columns <- colnames(snp_chip_full)[grepl("PlDNA_11", colnames(snp_chip_full))] 
print(plate11_columns) #91 samples

plate12_columns <- colnames(snp_chip_full)[grepl("PlDNA_12", colnames(snp_chip_full))] 
print(plate12_columns) #96 samples

plate13_columns <- colnames(snp_chip_full)[grepl("PlDNA_13", colnames(snp_chip_full))] 
print(plate13_columns) #93 samples

plate1_columns <- colnames(snp_chip_full)[grepl("PlDNA_1", colnames(snp_chip_full))] 
print(plate1_columns) #468 samples - but we know this is also returning PlDNA10, PlDNA11, PlDNA12, and PlDNA 13
468 - 94 - 91 - 96 - 93 # = 94 samples that are on Plate 1


# Check for Plate 9
plate9_columns <- colnames(snp_chip_full)[grepl("PlDNA_9", colnames(snp_chip_full))]
print(plate9_columns) #108 samples, but we know that we have 25 duplicates that passed both the recall and the original plate, so we will deal with those. 108-25 = 83

# 6/3/2025 investigating the start of the .1 at the ends of the column names
all_rep_columns <- grep("\\.1$", colnames(snp_chip_full), value = TRUE)
length(all_rep_columns) # 0 repeated columns - nice, all sample columns are unique

sequenced_pl9 <- read.csv("/Users/madelineeppley/Desktop/cvi25files/NE_Marine_Science_OysterCVI200K_20240301_Sample_Table.csv")
sequenced_all <- read.csv("/Users/madelineeppley/Desktop/cvi25files/NE_Marine_Science_OysterCVI200K_All_Plates_SampleTable.csv")

# check dimensions of each table
dim(sequenced_pl9) # 96 samples - full # on plate 9
dim(sequenced_all) # 1248 samples - full # in the global recall, which includes recalled plate 9


# plate 1, plate 9, plate 10, and plate 11 are missing last two columns - create these and fill with NAs
sequenced_all$Organization <- NA
sequenced_all$Super.ID <- NA
sequenced_pl9$Organization <- NA
sequenced_pl9$Super.ID <- NA

# check again
dim(sequenced_pl9) # 14 cols match
head(sequenced_pl9)

# plate 10 and plate 11 have a column naming issue - used to be gender, is now sex
names(sequenced_pl9)[names(sequenced_pl9) == "QC.computed_gender"] <- "QC.computed_sex"

combined_metadata <- rbind(sequenced_all, sequenced_pl9) # 1248 + 96 = 1344 samples, but we know we have some plate 9 duplicates

# we also know we have both passed and failed samples in these files, so let's first remove the failed sequences

pass_df <- combined_metadata[combined_metadata$Pass.Fail == "Pass", ]
dim(pass_df) # 1207 samples passed sequencing, and we know we have 1102 in our genetic matrix. However, we still have those plate 9 duplicates

fail_df <- combined_metadata[combined_metadata$Pass.Fail == "Fail", ]
dim(fail_df) # 137 samples failed sequencing in the global recall or on plate 9


snp_chip_samples <- colnames(snp_chip_full[20:ncol(snp_chip_full)])

standardize_name <- function(names) {
  # Remove leading underscore
  names <- gsub("^_", "", names)
  
  # Remove duplicate well positions at the beginning (e.g., A1_A1_)
  names <- gsub("^([A-H]\\d+)_\\1_", "\\1_", names)
  
  return(names)
}

standardized_snp_chip <- standardize_name(snp_chip_samples)

# Get the passing sample filenames
passing_samples <- pass_df$Sample.Filename

# Find passing samples that aren't in snp_chip_full
missing_samples <- passing_samples[!passing_samples %in% standardized_snp_chip] # here we have 7 samples
missing_samples
# however, I know none of those are actually true missing samples, because the other 6 are a naming exception that wasn't caught with the standardize_name function
# looking at the other sample, there's an extra "71" shoved in that generated label. we will take care of removing that later and everything should align


#6/3/25 generally all plate wells are matching up here, things look good between sequenced and shipped wells
# now we need to address the plate 9 issue. We have the global recall plate and the original plate. Plate 9 had a very high rate of failure in the recall plate.
# to fix this we need to keep just the data from the old plate 9. 

# check to see how many plate 9 samples we have 
sample_cols <- colnames(snp_chip_full)[20:ncol(snp_chip_full)] #1207 matches

# function that extracts the core sample IDs (just well, plate, ind key)
extract_sample_id <- function(col_name) {
  # Remove the leading underscore and trailing .CEL part
  cleaned <- gsub("^_|_[A-H][0-9]+\\.CEL$", "", col_name)
  # Remove the plate number suffix (e.g., _284527)
  cleaned <- gsub("_[0-9]{6}$", "", cleaned)
  return(cleaned)
}

# extract the core ids
core_ids <- sapply(sample_cols, extract_sample_id)

# check for duplicates across all of the plates
duplicate_ids <- core_ids[duplicated(core_ids)]
unique_duplicates <- unique(duplicate_ids) # ok great we have just 25 plate 9 samples here.
# this makes sense, these are the 25 samples that passed both the global recall and the original plate 9
# we know that these samples are between 95-99% the same genetics, so they are decent tech reps, however, we will just keep the original plate 9 data


original_cols <- grep("_284527_", colnames(snp_chip_full), value = TRUE)
recall_cols <- grep("_287421_", colnames(snp_chip_full), value = TRUE) 
length(original_cols) # we originally had 79 samples pass plate 9
length(recall_cols) # we had 29 recalled samples pass on new plate 9
# out of those 29, only 25 passed on the original plate. so, can we find a way to keep those 4?

# get core sample IDs from both batches
original_ids <- sapply(original_cols, extract_sample_id)
recall_ids <- sapply(recall_cols, extract_sample_id)

# now find our 4 recall samples that aren't in the original batch
unique_recall_ids <- setdiff(recall_ids, original_ids)
length(unique_recall_ids) # ok nice we have the 4 samples here

# get the unique recall sample IDs
unique_recall_cols <- recall_cols[recall_ids %in% unique_recall_ids]
paste(unique_recall_cols, collapse=", ")

plate9_cols <- c(original_cols, recall_cols)
non_plate9_cols <- setdiff(colnames(snp_chip_full), plate9_cols)
cols_to_keep <- c(non_plate9_cols, original_cols, unique_recall_cols) # keeping 79 + 4 samples here from Plate 9 plus all other non plate 9 samples

# now filter out the cols we don't want - originally 1226 cols
snp_chip_full <- snp_chip_full[, cols_to_keep] # great, now 1201 cols - we removed those 25 duplicated samples!

plate9_columns <- colnames(snp_chip_full)[grepl("PlDNA_9", colnames(snp_chip_full))] 
print(plate9_columns) # now 83 samples

###############################################################################################

# for loop ran! - the merged file should be stored as snp_chip_full
dim(snp_chip_full) # 1201 columns - 19 columns for metadata = 1182 samples
class(snp_chip_full) # stored as a data frame

# save this output file
snp_chip_full3 <- write.csv(snp_chip_full, "/Users/madelineeppley/Desktop/cvi25files/20250603_all_vcf.csv")

# Data formatting - read in full CSV here if not running for loop above - MGE #### 
#geno_matrix <- snp_chip_full OR
geno_matrix <- read.csv("/Users/madelineeppley/Desktop/cvi25files/20250603_all_vcf.csv") # read in geno matrix to work with here

head(geno_matrix, n = 3) # check head
dim(geno_matrix) # check dimensions - rows contain SNP info
head(colnames(geno_matrix), n = 100) # check column names - contains individuals

# first step get rid of some of the messy individual IDs and replace with clearer label
colnames(geno_matrix)[1:20] # these are all of the genotype info stuff
colnames(geno_matrix)[20:25] # start with 20

# we want to split the geno_matrix into 3 projects, format the IDs, then transpose and merge back together
sample_cols <- colnames(geno_matrix)[20:ncol(geno_matrix)] #1182 samples

# identify seascape samples with SG
seascape_sample_cols <- grepl("SG", sample_cols)

# get the column indices for seascape samples after the first 19 cols
seascape_col_indices <- which(seascape_sample_cols) + 19
seascape_geno_matrix <- geno_matrix[, seascape_col_indices] #748 samples here

# identify experimental samples with CviMVPe
exp_sample_cols <- grepl("CviMVPe", sample_cols)
exp_col_indices <- which(exp_sample_cols) + 19 # 465 samples here
exp_geno_matrix <- geno_matrix[, exp_col_indices]
# we know that some samples made it into both matrices, and some samples might not have at all. let's check those cases
# some seascape samples were SG on plates 6 and 7 that got assigned the CviMVPe code
# 748 + 465 = 1213 > 1182 that we were expecting 

overlap_samples <- seascape_sample_cols & exp_sample_cols
overlap_count <- sum(overlap_samples) # we have 31 overlapping samples 
1182+31 # 1213, which is the number we found. great, so all samples got allocated to one of the two subsets

overlap_sample_names <- sample_cols[overlap_samples]
print(overlap_sample_names)

# next we need to replace CviMVPe with CviMVP for those 33, add them to seascape, and remove them from exp
sample_cols_fixed <- sample_cols # get the original sample columns again
sample_cols_fixed[overlap_samples] <- gsub("CviMVPe", "CviMVP", sample_cols_fixed[overlap_samples])

# put the fixed column names back into our geno_matrix
colnames(geno_matrix)[20:ncol(geno_matrix)] <- sample_cols_fixed

# now do our subsets again
# identify seascape samples with SG
seascape_sample_cols <- grepl("SG", sample_cols_fixed)

# get the column indices for seascape samples after the first 20 cols
seascape_col_indices <- which(seascape_sample_cols) + 19
seascape_geno_matrix <- geno_matrix[, seascape_col_indices] #748 samples here

# identify experimental samples with CviMVPe
exp_sample_cols <- grepl("CviMVPe", sample_cols_fixed)
exp_col_indices <- which(exp_sample_cols) + 19
exp_geno_matrix <- geno_matrix[, exp_col_indices] # now 434 samples here - we removed the 31 that were actually seascape!

# ok now our goal is sample retention, we want to get nicely formatted IDs and keep all 1182 samples retained
# start with the seascape samples 
seascape_col_names <- colnames(seascape_geno_matrix)
head(seascape_col_names, 10)

# make a new dataframe to store the information that we need to make the IDs 
seascape_parsed <- data.frame(
  original_name = seascape_col_names,
  stringsAsFactors = FALSE
)

seascape_parsed$project_type <- "CviMVP"

# Extract PlDNA plate number
seascape_parsed$plate_number <- str_extract(seascape_parsed$original_name, "PlDNA_([0-9]+)") %>%
  str_replace("PlDNA_", "")

# Extract well position (letter followed by 1-2 digits)
seascape_parsed$well_position <- str_extract(seascape_parsed$original_name, "[A-H][0-9]{1,2}")

# Extract SG type (SG or SG.sharedLotterhosPuritz)
seascape_parsed$SG_type <- ifelse(
  grepl("SG[-.]sharedLotterhosPuritz", seascape_parsed$original_name),
  "SG.sharedLotterhosPuritz",
  "SG"
)

seascape_parsed$individual_key <- str_extract(seascape_parsed$original_name, "__([a-f0-9]{8})_") %>%
  str_replace_all("__|_", "")

seascape_parsed$individual_ID <- str_extract(seascape_parsed$original_name, "SG(?:[-.]sharedLotterhosPuritz)?_([0-9]{4})__") %>%
  str_extract("[0-9]{4}")

# now make the clean ID
seascape_parsed$clean_ID <- paste(
  seascape_parsed$well_position,
  seascape_parsed$project_type, 
  "PlDNA",
  seascape_parsed$plate_number,
  seascape_parsed$SG_type,
  seascape_parsed$individual_ID,
  seascape_parsed$individual_key,
  sep = "_"
)

# check order before re-merge
identical(seascape_parsed$original_name, colnames(seascape_geno_matrix))
colnames(seascape_geno_matrix) <- seascape_parsed$clean_ID #ok great, now we have those clean IDs in our seascape matrix
dim(seascape_geno_matrix) # still all 748 inds here


# now we do this same thing with the experimental matrix
exp_col_names <- colnames(exp_geno_matrix) # goal is to retain all 434 inds here - left off here 6/3/25
head(exp_col_names, 10)

exp_parsed <- data.frame(
  original_name = exp_col_names,
  stringsAsFactors = FALSE
)

# add CviMVPe
exp_parsed$project_type <- "CviMVPe"

# extract plate number
exp_parsed$plate_number <- str_extract(exp_parsed$original_name, "PlDNA_([0-9]+)")  %>%
  str_replace("PlDNA_", "")

# extract well position 
exp_parsed$well_position <- str_extract(exp_parsed$original_name, "[A-H][0-9]{1,2}")
head(exp_parsed, 10) # data check looks ok 

# Extract individual ID based on different patterns
exp_parsed$individual_ID <- ifelse(
  exp_parsed$plate_number %in% c("11", "12"), 
  str_extract(exp_parsed$original_name, "70[0-9]{3}"),
  ifelse(
    exp_parsed$plate_number %in% c("6", "7"),
    str_extract(exp_parsed$original_name, "D[0-9]{3}"),
    ifelse(
      exp_parsed$plate_number == "13",
      str_extract(exp_parsed$original_name, "(?:50|70)[0-9]{3}"),
      NA_character_
    )
  )
)

# Extract individual key (8-character hex code)
exp_parsed$individual_key <- str_extract(exp_parsed$original_name, "[a-fA-F0-9]{8}")

head(exp_parsed, 10)
# we have a weird case that I didn't catch before, which is the 7 seascape samples that were re-sequenced on plate 13 in the event of data loss. 
# these samples can be kept for now, they are now essentially tech reps with plate 1 samples that did pass

# add SG col to exp df
exp_parsed$SG_type <- NA_character_

# find seascape occurrences
seascape_rows <- grepl("seascape", exp_parsed$original_name)

# Fix all seascape samples at once
exp_parsed$project_type[seascape_rows] <- "CviMVP"
exp_parsed$SG_type[seascape_rows] <- "SG"

# Extract the individual_key (8-char hex after "seascape_")
exp_parsed$individual_key[seascape_rows] <- str_extract(exp_parsed$original_name[seascape_rows], "seascape_([a-f0-9]{8})") %>% 
  str_replace("seascape_", "")

# Extract the individual_ID (4-digit number after "CviMVP_")  
exp_parsed$individual_ID[seascape_rows] <- str_extract(exp_parsed$original_name[seascape_rows], "CviMVP_([0-9]{4})") %>%
  str_replace("CviMVP_", "")

exp_parsed$clean_ID <- ifelse(
  !is.na(exp_parsed$SG_type),  # If it's a seascape sample
  paste(exp_parsed$well_position, exp_parsed$project_type, "PlDNA", exp_parsed$plate_number, 
        exp_parsed$SG_type, exp_parsed$individual_ID, exp_parsed$individual_key, sep = "_"),
  # If it's a regular experimental sample  
  paste(exp_parsed$well_position, exp_parsed$project_type, "PlDNA", exp_parsed$plate_number,
        exp_parsed$individual_ID, exp_parsed$individual_key, sep = "_")
)

# ok everything is looking good, but we need to replace those 5000 vial labels with the D ind labels for the 6 that got resequenced on plate 13
exp_parsed$clean_ID[exp_parsed$individual_ID == "50111"] <- "A12_CviMVPe_PlDNA_13_D850_E95F93C1"
exp_parsed$clean_ID[exp_parsed$individual_ID == "50064"] <- "A5_CviMVPe_PlDNA_13_D314_B19CE842"
exp_parsed$clean_ID[exp_parsed$individual_ID == "50179"] <- "B6_CviMVPe_PlDNA_13_D505_DA998D5E"
exp_parsed$clean_ID[exp_parsed$individual_ID == "50180"] <- "C8_CviMVPe_PlDNA_13_D503_8BAE41AF"
exp_parsed$clean_ID[exp_parsed$individual_ID == "50016"] <- "D4_CviMVPe_PlDNA_13_D494_C3847A2F"
exp_parsed$clean_ID[exp_parsed$individual_ID == "50113"] <- "E10_CviMVPe_PlDNA_13_D840_9DE5BDBE"

# merge back in
identical(exp_parsed$original_name, colnames(exp_geno_matrix))
colnames(exp_geno_matrix) <- exp_parsed$clean_ID
dim(exp_geno_matrix) # nice, still have 434 samples 


# bind everything back together 
combined_geno_matrix <- cbind(seascape_geno_matrix, exp_geno_matrix) #1182 samples still here
any(duplicated(colnames(combined_geno_matrix))) # FALSE
dup_values <- colnames(combined_geno_matrix)[duplicated(colnames(combined_geno_matrix))]
seascape_internal_dups <- any(duplicated(colnames(seascape_geno_matrix)))
seascape_dup_names <- colnames(seascape_geno_matrix)[duplicated(colnames(seascape_geno_matrix))] # empty
exp_internal_dups <- any(duplicated(colnames(exp_geno_matrix)))
exp_dup_names <- colnames(exp_geno_matrix)[duplicated(colnames(exp_geno_matrix))] # empty

# everything here looks good to proceed with transposing the matrix

###################################################################################
# Make a filtered matrix that just contains our SNP info - remove mut info and just keep the SNP data
dim(combined_geno_matrix) # 275671 by 1182
dim(geno_matrix) #275671 by 1201 
geno_matrix <- combined_geno_matrix # ok typically I don't overwrite but I double-checked that this worked
dim(geno_matrix) # indeed we have 1182 individuals here

# Check the updated column names
head(colnames(geno_matrix), n = 30)
tail(colnames(geno_matrix), n = 30)
head(geno_matrix[1:5, 1:5]) # already removed the mut info when we subset the matrices, so we're good to go

# set our row names as the Affx.ID
rownames(geno_matrix) <- snp_chip_full$Affx.ID # pull the muts info from snp_chip_full, which made geno_matrix
head(geno_matrix[1:5, 1:5]) # nice, now we have our SNP identifiers as rownames
geno_to_transpose <- write.csv(geno_matrix, "/Users/madelineeppley/Desktop/cvi25files/20250604_geno_to_transpose.csv") # write file in case job fails and R crashes lol
geno_to_transpose <- read.csv("/Users/madelineeppley/Desktop/cvi25files/20250604_geno_to_transpose.csv")

# TRANSPOSING MATRIX IS FASTER IN DISCOVERY CLUSTER (HIGH-PERFORMANCE COMPUTING ENVIRONMENT)
# HOWEVER, we are going to transpose the data frame columns to rows locally here with the transposeBigData function
dim(geno_to_transpose)
head(geno_to_transpose[1:5, 1:5])
rownames(geno_to_transpose) <- geno_to_transpose$X # we lost the rownames and they're now a column called X, rename and remove col X
geno_to_transpose$X <- NULL
head(geno_to_transpose[1:5, 1:5])
geno_flip <- transposeBigData(geno_to_transpose, blocksize = 10) # yay, this worked! it only took about 5 minutes to run locally

#geno_flip <- as.data.frame(t(geno_matrix)) # this line is failing with my local memory 5/13/25
#geno_flip <- t(geno_matrix) # also failed to do just this 5/13/25
# let's export this file, use Discovery to transpose, and then read the file back in

saveRDS(geno_flip, "/Users/madelineeppley/Desktop/cvi25files/20250604_geno_flip_prefilter.rds")

# examine the new matrix
head(rownames(geno_flip), n = 30)
head(colnames(geno_flip), n = 30)
geno_flip[1:5, 1:5] # looks good here

# order the genotype matrix by the Affx-ID
ordered_columns <- order(colnames(geno_flip))
geno_flip_ordered <- geno_flip[, ordered_columns]
print(geno_flip_ordered[1:5, 1:5])

# re-read in our snp_chip mut info
snp_chip <- readRDS("/Users/madelineeppley/Desktop/cvi25files/20240719_mutmatrix.rds")
order_vector <- match(colnames(geno_flip_ordered), snp_chip$Affx.ID)
print(order_vector)

if (any(is.na(order_vector))) {
  stop("Some IDs in geno mat are not found in mut mat.")
} # passes

# now check 
geno_names <- (colnames(geno_flip_ordered))
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
saveRDS(geno_flip_ordered, "/Users/madelineeppley/Desktop/cvi25files/20250604_ordered.rds")


# Filter NA data ####
# now remove columns that have 100% NA data
# read in geno_matrix or save as new var

matrix_to_filter <- geno_flip_ordered 
# matrix_to_filter <- readRDS("/Users/madelineeppley/Desktop/cvi25files/20250513_ordered.rds") # or read in ordered file that we will filter
ncol(matrix_to_filter) # count the number of columns - starting with 275671

# find columns with all NAs by counting # of NAs, compare to total # of rows
na_columns <- colSums(is.na(matrix_to_filter)) == nrow(matrix_to_filter)

# Remove columns with all NA values from the genotype matrix
fil_mat <- matrix_to_filter[, !na_columns]
dim(fil_mat) # down to 195025 columns (SNPs)
fil_mat[1:5, 1:5]

# Remove those same SNPs from the mutation matrix
fil_mat_mut <- snp_chip[!na_columns ,]
dim(fil_mat_mut) # 195025 rows (SNPs)
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
saveRDS(fil_mat, "/Users/madelineeppley/Desktop/cvi25files/20250604_genomatrix_PASS.rds")
saveRDS(fil_mat_mut, "/Users/madelineeppley/Desktop/cvi25files/0250604_mutmatrix_PASS.rds")


#############################
## Merging Individual Data ##
#############################
# project notes
# MGE read in individuals sequenced data - add plate 9 8/19/2024
# MGE add environmental data and site colors 9/22/2024
# MGE fix plate 5 inds by adding shipped_well and seq_well 9/30/2024 
# MGE removed the shipped_well and seq_well because plate 5 labels were fixed 11/04/2024
# MGE add juveniles plate 11 on 11/24/2024
# MGE add all plates with global recall (incl. 12 and 13) on 5/13/2025
# MGE run script with new for loop (removing plate 5 labeling issue) on 6/4/2025

# setup
#######

# install packages
# install.packages("stringr")

# packages
library(tidyr) # for data wrangling
library(stringr) # for parsing single columns into multiple cols

# data 
########

# read in the genotype matrix, check format, and print all of our row names to check the individual IDs
genomatrixfull <- readRDS("/Users/madelineeppley/Desktop/cvi25files/20250604_genomatrix_PASS.rds")
genomatrixfull[1:5, 1:5] 
dim(genomatrixfull) # 1182 and 195025

# we have 1182 individuals in our dataset across all 3 projects
# our SG.sharedLotterhosPuritz samples have a "." between them
# the juvenile samples have vial number 70000's
# the experimental parents have individual labels that have "DXXX" with 3 numbers following

# print the first 1000 row names to take a look
print(rownames(genomatrixfull))
current_rownames <- rownames(genomatrixfull) # store the original rownames
print(current_rownames[1000:1102]) # check the last 102 that didn't print above 
# everything is looking good here

# let's split into 3 projects 

# experimental samples vector
experimental_sample <- grepl("_D", rownames(genomatrixfull))

# Create new experimental matrix
experimental_matrix <- genomatrixfull[experimental_sample, ] #160 samples in our experimental adults matrix

# seascape samples vector
seascape_sample <- grepl("_SG", rownames(genomatrixfull))

# Create new experimental matrix
seascape_matrix <- genomatrixfull[seascape_sample, ] #675 samples in our seascape project matrix

# juvenile samples vector - this one is more difficult because we only have vial numbers. grep for "_70" will also find seascape labels like SG-1700 and experimental samples
# let's filter by NOT in the experimental or seascape
juvs_sample <- grepl("_70", rownames(genomatrixfull)) & !experimental_sample & !seascape_sample

juvs_matrix <- genomatrixfull[juvs_sample, ] #267 samples

# sanity check
160 + 755 + 267 # = 1182, great, that was what we were expecting 

# let's print row names to do a check and make sure that all samples were correctly subset and look good
print(rownames(experimental_matrix)) # slay everything there is looking good 

# move on to the seascape matrix
print(rownames(seascape_matrix)) # great

# move on to the juvs matrix
print(rownames(juvs_matrix)) # perf

# let's double check our math 
all_rownames <- c(rownames(juvs_matrix), rownames(seascape_matrix), rownames(experimental_matrix)) #1182 nice
combined_matrix <- rbind(experimental_matrix, seascape_matrix, juvs_matrix) #1182 inds 

# find rownames in genomatrixfull that are NOT in any of the three matrices to double-check
all_found <- experimental_sample | seascape_sample | juvs_sample

# Find the samples that don't match any of your three criteria
missing <- rownames(genomatrixfull)[!all_found]
print(missing) # YAY none are missing, all samples have been allocated to a project 

# output the separate genotype matrices
########################################
# make sure the SNP order is preserved with the mut matrix for seascape matrix
geno_names3 <- (colnames(seascape_matrix))
head(geno_names3)

identical(geno_names3, mut_names2) # TRUE

compare <- geno_names3 == mut_names2
compare # all TRUE

# make sure labels line up
if(!identical((geno_names3), mut_names2)){
  print("Error sample names don't line up")
  break
} # passes

# make sure the SNP order is preserved with the mut matrix for experimental matrix
geno_names4 <- (colnames(experimental_matrix))
head(geno_names3)

identical(geno_names4, mut_names2) # TRUE

compare <- geno_names4 == mut_names2
compare # all TRUE

# make sure labels line up
if(!identical((geno_names4), mut_names2)){
  print("Error sample names don't line up")
  break
} # passes


# make sure the SNP order is preserved with the mut matrix for juvs matrix
geno_names5 <- (colnames(juvs_matrix))
head(geno_names5)

identical(geno_names5, mut_names2) # TRUE

compare <- geno_names5 == mut_names2
compare # all TRUE

# make sure labels line up
if(!identical((geno_names5), mut_names2)){
  print("Error sample names don't line up")
  break
} # passes

# save all as .RDS datasets
saveRDS(seascape_matrix, "/Users/madelineeppley/Desktop/cvi25files/20250604_seascapematrix.rds")
saveRDS(experimental_matrix, "/Users/madelineeppley/Desktop/cvi25files/20250604_experimentalmatrix.rds")
saveRDS(juvs_matrix, "/Users/madelineeppley/Desktop/cvi25files/20250604_experimental_juvs_matrix.rds")



# sequencing facility data
###########################
# read in individuals sequenced data
# MGE note here, we replace all but sequenced pl9 with the global recall file
sequenced_pl9 <- read.csv("/Users/madelineeppley/Desktop/cvi25files/NE_Marine_Science_OysterCVI200K_20240301_Sample_Table.csv")
sequenced_all <- read.csv("/Users/madelineeppley/Desktop/cvi25files/NE_Marine_Science_OysterCVI200K_All_Plates_SampleTable.csv")

# check dimensions of each table
dim(sequenced_pl9) # 96 samples - full # on plate 9
dim(sequenced_all) # 1248 samples - full # in the global recall, which includes recalled plate 9


# plate 1, plate 9, plate 10, and plate 11 are missing last two columns - create these and fill with NAs
sequenced_all$Organization <- NA
sequenced_all$Super.ID <- NA
sequenced_pl9$Organization <- NA
sequenced_pl9$Super.ID <- NA

# check again
dim(sequenced_pl9) # 14 cols match
head(sequenced_pl9)

# plate 10 and plate 11 have a column naming issue - used to be gender, is now sex
names(sequenced_pl9)[names(sequenced_pl9) == "QC.computed_gender"] <- "QC.computed_sex"

combined_metadata <- rbind(sequenced_all, sequenced_pl9) # 1248 + 96 = 1344 samples, but we know we have some plate 9 duplicates

# we also know we have both passed and failed samples in these files, so let's first remove the failed sequences

pass_df <- combined_metadata[combined_metadata$Pass.Fail == "Pass", ]
dim(pass_df) # 1207 samples passed sequencing, and we know we have 1102 in our genetic matrix. However, we still have those plate 9 duplicates

fail_df <- combined_metadata[combined_metadata$Pass.Fail == "Fail", ]
dim(fail_df) # 137 samples failed sequencing in the global recall or on plate 9

# check for any duplicate errors in the metadata 
has_duplicates <- any(duplicated(pass_df$Sample.Filename))
has_duplicates # nope, we are good to move on


# now we need to assign the same identifer IDs to each row of the seq facility data as what we have in our geno matrix
# use the same approach that we did for the genotype matrix, split the Sample.Filename into columns and then bind the relevant info together

# we want to split the geno_matrix into 3 projects, format the IDs, then transpose and merge back together
dim(pass_df) # our individuals are now in rows, the Sample.Filename is a column
meta_rows <- pass_df$Sample.Filename #1207 samples

# identify seascape samples with SG
seascape_meta_rows <- grepl("SG", meta_rows)

# get the column indices for seascape samples after the first 19 cols
seascape_row_indices <- which(seascape_meta_rows)
seascape_meta_matrix <- pass_df[seascape_row_indices ,] #773 samples here

# identify experimental samples with CviMVPe
exp_meta_rows <- grepl("CviMVPe", meta_rows)
exp_row_indices <- which(exp_meta_rows) # 465 samples here
exp_meta_matrix <- pass_df[exp_row_indices, ]
# we know that some samples made it into both matrices, and some samples might not have at all. let's check those cases
# some seascape samples were SG on plates 6 and 7 that got assigned the CviMVPe code
# 773 + 465 = 1238 > 1182 that we were expecting 
# but we know that we removed those 25 extra samples that passed both plate 9's from the genotype matrix, so that actually leaves us with 773 - 25 = 748
# 748 + 465 = 1213

overlap_samples2 <- seascape_meta_rows & exp_meta_rows
overlap_count2 <- sum(overlap_samples2) # we have 31 overlapping samples 
1182+31 # 1213, which is the number we found. great, so all samples got allocated to one of the two subsets

overlap_meta_names <- meta_rows[overlap_samples2]
print(overlap_meta_names)

# next we need to replace CviMVPe with CviMVP for those 31, add them to seascape, and remove them from exp
meta_rows_fixed <- meta_rows # get the original sample columns again
meta_rows_fixed[overlap_samples2] <- gsub("CviMVPe", "CviMVP", meta_rows_fixed[overlap_samples2]) # ran this line last 6/4/25

# put the fixed column names back into our pass_df
pass_df$Sample.Filename <- meta_rows_fixed

# now do our subsets again
# identify seascape samples with SG
seascape_meta_rows <- grepl("SG", meta_rows_fixed)

# get the column indices for seascape samples after the first 20 cols
seascape_row_indices <- which(seascape_meta_rows)
seascape_meta_matrix <- pass_df[seascape_row_indices ,] #773 samples still here - this is correct because we are not removing any from seascape, only exp

# identify experimental samples with CviMVPe
exp_meta_rows <- grepl("CviMVPe", meta_rows_fixed)
exp_row_indices <- which(exp_meta_rows) # 465 samples here
exp_meta_matrix <- pass_df[exp_row_indices, ] # now 434 samples here - we removed the 31 that were actually seascape!

# ok now our goal is sample retention, we want to get nicely formatted IDs and keep all samples retained
# start with the seascape samples 
seascape_row_names <- seascape_meta_matrix$Sample.Filename
head(seascape_row_names, 10)

# make a new dataframe to store the information that we need to make the IDs 
seascape_parsed2 <- data.frame(
  original_name = seascape_row_names,
  stringsAsFactors = FALSE
)

seascape_parsed2$project_type <- "CviMVP"

# Extract PlDNA plate number
seascape_parsed2$plate_number <- str_extract(seascape_parsed2$original_name, "PlDNA_([0-9]+)") %>%
  str_replace("PlDNA_", "")

# Extract well position (letter followed by 1-2 digits)
seascape_parsed2$well_position <- str_extract(seascape_parsed2$original_name, "[A-H][0-9]{1,2}")

# Extract SG type (SG or SG.sharedLotterhosPuritz)
seascape_parsed2$SG_type <- ifelse(
  grepl("SG[-.]sharedLotterhosPuritz", seascape_parsed2$original_name),
  "SG.sharedLotterhosPuritz",
  "SG"
)

seascape_parsed2$individual_key <- str_extract(seascape_parsed2$original_name, "__([a-f0-9]{8})_") %>%
  str_replace_all("__|_", "")

seascape_parsed2$individual_ID <- str_extract(seascape_parsed2$original_name, "SG(?:[-.]sharedLotterhosPuritz)?_([0-9]{4})__") %>%
  str_extract("[0-9]{4}")

# now make the clean ID
seascape_parsed2$clean_ID <- paste(
  seascape_parsed2$well_position,
  seascape_parsed2$project_type, 
  "PlDNA",
  seascape_parsed2$plate_number,
  seascape_parsed2$SG_type,
  seascape_parsed2$individual_ID,
  seascape_parsed2$individual_key,
  sep = "_"
)

# check order before re-merge
identical(seascape_parsed2$original_name, seascape_meta_matrix$Sample.Filename)
seascape_meta_matrix$clean_ID <- seascape_parsed2$clean_ID #ok great, now we have those clean IDs in our seascape meta matrix
dim(seascape_meta_matrix) # still all 773 inds here


# now we do this same thing with the experimental matrix
exp_row_names <- exp_meta_matrix$Sample.Filename # goal is to retain all 434 inds
head(exp_row_names, 10)

exp_parsed2 <- data.frame(
  original_name = exp_row_names,
  stringsAsFactors = FALSE
)

# add CviMVPe
exp_parsed2$project_type <- "CviMVPe"

# extract plate number
exp_parsed2$plate_number <- str_extract(exp_parsed2$original_name, "PlDNA_([0-9]+)")  %>%
  str_replace("PlDNA_", "")

# extract well position 
exp_parsed2$well_position <- str_extract(exp_parsed2$original_name, "[A-H][0-9]{1,2}")
head(exp_parsed2, 10) # data check looks ok 

# since we have the D pattern and the 70000 vial pattern, we can use a for loop to deal with both at the same time
exp_parsed2$individual_ID <- ifelse(
  exp_parsed2$plate_number %in% c("11", "12"), 
  str_extract(exp_parsed2$original_name, "70[0-9]{3}"),
  ifelse(
    exp_parsed2$plate_number %in% c("6", "7"),
    str_extract(exp_parsed2$original_name, "D[0-9]{3}"),
    ifelse(
      exp_parsed2$plate_number == "13",
      str_extract(exp_parsed2$original_name, "(?:50|70)[0-9]{3}"),
      NA_character_
    )
  )
)

# Extract individual key (8-character hex code)
exp_parsed2$individual_key <- str_extract(exp_parsed2$original_name, "[a-fA-F0-9]{8}")

head(exp_parsed2, 10)
# we have a weird case that I didn't catch before, which is the 7 seascape samples that were re-sequenced on plate 13 in the event of data loss. 
# these samples can be kept for now, they are now essentially tech reps with plate 1 samples that did pass

# add SG col to exp df
exp_parsed2$SG_type <- NA_character_

# find seascape occurrences
seascape_rows <- grepl("seascape", exp_parsed2$original_name)

# Fix all seascape samples at once
exp_parsed2$project_type[seascape_rows] <- "CviMVP"
exp_parsed2$SG_type[seascape_rows] <- "SG"

# Extract the individual_key (8-char hex after "seascape_")
exp_parsed2$individual_key[seascape_rows] <- str_extract(exp_parsed2$original_name[seascape_rows], "seascape_([a-f0-9]{8})") %>% 
  str_replace("seascape_", "")

# Extract the individual_ID (4-digit number after "CviMVP_")  
exp_parsed2$individual_ID[seascape_rows] <- str_extract(exp_parsed2$original_name[seascape_rows], "CviMVP_([0-9]{4})") %>%
  str_replace("CviMVP_", "")

exp_parsed2$clean_ID <- ifelse(
  !is.na(exp_parsed2$SG_type),  # If it's a seascape sample
  paste(exp_parsed2$well_position, exp_parsed2$project_type, "PlDNA", exp_parsed2$plate_number, 
        exp_parsed2$SG_type, exp_parsed2$individual_ID, exp_parsed2$individual_key, sep = "_"),
  # If it's a regular experimental sample  
  paste(exp_parsed2$well_position, exp_parsed2$project_type, "PlDNA", exp_parsed2$plate_number,
        exp_parsed2$individual_ID, exp_parsed2$individual_key, sep = "_")
)

# ok everything is looking good, but we need to replace those 5000 vial labels with the D ind labels for the 6 that got resequenced on plate 13
exp_parsed2$clean_ID[exp_parsed2$individual_ID == "50111"] <- "A12_CviMVPe_PlDNA_13_D850_E95F93C1"
exp_parsed2$clean_ID[exp_parsed2$individual_ID == "50064"] <- "A5_CviMVPe_PlDNA_13_D314_B19CE842"
exp_parsed2$clean_ID[exp_parsed2$individual_ID == "50179"] <- "B6_CviMVPe_PlDNA_13_D505_DA998D5E"
exp_parsed2$clean_ID[exp_parsed2$individual_ID == "50180"] <- "C8_CviMVPe_PlDNA_13_D503_8BAE41AF"
exp_parsed2$clean_ID[exp_parsed2$individual_ID == "50016"] <- "D4_CviMVPe_PlDNA_13_D494_C3847A2F"
exp_parsed2$clean_ID[exp_parsed2$individual_ID == "50113"] <- "E10_CviMVPe_PlDNA_13_D840_9DE5BDBE"


# merge back in
identical(exp_parsed2$original_name, exp_meta_matrix$Sample.Filename)
exp_meta_matrix$clean_ID <- exp_parsed2$clean_ID 
dim(exp_meta_matrix) # nice, still have 434 samples 


# bind everything back together and check for dups
combined_meta_matrix <- rbind(seascape_meta_matrix, exp_meta_matrix) #1207 samples still here (minus those 25 plate 9 repeats = 1182)
any(duplicated(combined_meta_matrix$clean_ID)) # FALSE 
dup_values <- combined_meta_matrix$clean_ID[duplicated(combined_meta_matrix$clean_ID)] # the 25 plate 9s
print(dup_values)

# repeat of original function for core IDs
extract_sample_id2 <- function(sample_name) {
  # First standardize the format
  cleaned <- gsub("__", "_", sample_name)
  
  if (grepl("PlDNA_9.*SG", cleaned)) {
    match <- regexpr("SG_[0-9]+_[a-z0-9]+", cleaned)
    if (match > 0) {
      return(substr(cleaned, match, match + attr(match, "match.length") - 1))
    }
  }
  
  return(cleaned)
}

# apply to combined_meta_matrix to get core IDs
combined_meta_matrix$core_id <- sapply(combined_meta_matrix$Sample.Filename, extract_sample_id2)

# original batch vs. recall batch
original_batch_rows <- grepl("_284527_", combined_meta_matrix$Sample.Filename)
recall_batch_rows <- grepl("_287421_", combined_meta_matrix$Sample.Filename)

rows_to_keep <- rep(TRUE, nrow(combined_meta_matrix))

# find dups
for (i in 1:nrow(combined_meta_matrix)) {
  if (original_batch_rows[i]) {
    # This is an original batch row, check if it has a duplicate in recall batch
    current_id <- combined_meta_matrix$core_id[i]
    has_recall_duplicate <- any(recall_batch_rows & combined_meta_matrix$core_id == current_id)
    
    if (has_recall_duplicate) {
      # If it has a duplicate in recall batch, remove this original version
      rows_to_keep[i] <- FALSE
    }
  }
}

# filter
filtered_meta_matrix <- combined_meta_matrix[rows_to_keep, ]

# check
nrow(combined_meta_matrix) # original num of rows
nrow(filtered_meta_matrix) # new num of rows
nrow(combined_meta_matrix) - nrow(filtered_meta_matrix) # num of removed rows = 24 but we were anticipating 25, I think the shared label is causing issues

# Check how many plate 9 samples we have
plate9_rows <- grepl("PlDNA_9", filtered_meta_matrix$Sample.Filename)
sum(plate9_rows) # total num of plate 9 rows = 84 but we were anticipating 83

# just deal with this one sample manually 
b4_original <- grep("B4.*sharedLotterhosPuritz.*1770.*284527", filtered_meta_matrix$Sample.Filename)

# Print the row to confirm it's the correct one
print(filtered_meta_matrix$Sample.Filename[b4_original])

# Filter it out manually
filtered_meta_matrix <- filtered_meta_matrix[-b4_original, ]


# remove the core_ids since we don't need those anymore
filtered_meta_matrix$core_id <- NULL


# now check to make sure the filtering worked
dim(filtered_meta_matrix) #1182 samples, perf
any(duplicated(filtered_meta_matrix$clean_ID)) # FALSE 
dup_values <- filtered_meta_matrix$clean_ID[duplicated(filtered_meta_matrix$clean_ID)] # none
print(dup_values)

# Check if all genotype samples now have metadata
matching_ids <- filtered_meta_matrix$clean_ID %in% rownames(genomatrixfull)
cat("\nAfter renaming - Number of matching IDs:", sum(matching_ids), "out of", nrow(genomatrixfull), "\n") # nope, we have one that doesn't match

missing_sample <- rownames(genomatrixfull)[!rownames(genomatrixfull) %in% filtered_meta_matrix$clean_ID]
missing_sample # it is F11_CviMVP_PlDNA_5_SG_1266_4e08d2b2
# looking at the data, the sequencing company put the wrong well number at the front of the metadata and called it H2, let's manually fix

h2_sample <- grep("H2_CviMVP_PlDNA_5_SG_1266_4e08d2b2", filtered_meta_matrix$clean_ID)

# Print to verify it's the right sample
print(filtered_meta_matrix$Sample.Filename[h2_sample])
print(filtered_meta_matrix$clean_ID[h2_sample])

# Fix the clean_ID to match the genotype matrix rowname
filtered_meta_matrix$clean_ID[h2_sample] <- "F11_CviMVP_PlDNA_5_SG_1266_4e08d2b2"


# Check if all genotype samples now have metadata
matching_ids2 <- filtered_meta_matrix$clean_ID %in% rownames(genomatrixfull)
cat("\nAfter renaming - Number of matching IDs:", sum(matching_ids2), "out of", nrow(genomatrixfull), "\n") # let's goooo

# fix the Label_inds in the filtered_meta_matrix to help us bind with the project data below
# the SG samples are good, just need to fix the experimental ones
exp_samples <- grepl("CviMVPe", filtered_meta_matrix$clean_ID)

# Create a mapping between clean_ID and individual_ID from exp_parsed2
id_mapping <- setNames(exp_parsed2$individual_ID, exp_parsed2$clean_ID)

# Update Label_ind column for experimental samples
filtered_meta_matrix$Label_ind[exp_samples] <- id_mapping[filtered_meta_matrix$clean_ID[exp_samples]]

# Also update the exp_meta_matrix if needed
exp_meta_matrix$Label_ind <- exp_parsed2$individual_ID

# ok great, so now we have filtered metadata and our genotype matrix that all have the same clean_IDs and the numbers match (1182)
# identify SG's for seascape
seascape_meta_filt_rows <- grepl("SG", filtered_meta_matrix$clean_ID)

# get the column indices for seascape samples after the first 20 cols
seascape_row_filt_indices <- which(seascape_meta_filt_rows)
seascape_meta_filt_matrix <- filtered_meta_matrix[seascape_row_filt_indices ,] #755 samples here, which exactly matches our genotype matrix, yay!


# let's export metadata
saveRDS(filtered_meta_matrix, "/Users/madelineeppley/Desktop/cvi25files/20250604_filtered_seq_data_matrix.rds")
saveRDS(exp_meta_matrix, "/Users/madelineeppley/Desktop/cvi25files/20250604_exp_filtered_seq_data_matrix.rds")
saveRDS(seascape_meta_filt_matrix, "/Users/madelineeppley/Desktop/cvi25files/20250604_seascape_filtered_seq_data_matrix.rds")


# project data
################
# MGE note - these files contain seascape data, experimental data, and broodstock data for all Lotterhos CviMVP projects
# we will be making a massive joined database for all projects, and then subsetting into the 3 separate projects.

# read in seascape data
tissue <- read.csv("/Users/madelineeppley/Desktop/cvi25files/SeascapeSamples - tissue.csv")
plan <- read.csv("/Users/madelineeppley/Desktop/cvi25files/SeascapeSamples Extractions - plan.csv") # figure out a way to separate out those experimentals
ind <- read.csv("/Users/madelineeppley/Desktop/cvi25files/SeascapeSamples - individual.csv")
site <- read.csv("/Users/madelineeppley/Desktop/cvi25files/SeascapeSamples - site.csv")
date <- read.csv("/Users/madelineeppley/Desktop/cvi25files/SeascapeSamples - date.csv")

# read in experimental adults data
exptissue <- read.csv("/Users/madelineeppley/Desktop/cvi25files/MVP Experimental Spawning Populations - tissue.csv")
broodstock <- read.csv("/Users/madelineeppley/Desktop/cvi25files/MVP Experimental Spawning Populations - broodstock.csv")

# read in experimental juvs data
expbiopsy <- read.csv("/Users/madelineeppley/Desktop/cvi25files/MVP23-FieldBags - tagsBiopsy.csv")
expspawntrt <- read.csv("/Users/madelineeppley/Desktop/cvi25files/MVP23-FieldBags - spawn_trt2.csv")
expjuvdna <- read.csv("/Users/madelineeppley/Desktop/cvi25files/MVP23-FieldBags - extraction.csv")
expbags <- read.csv("/Users/madelineeppley/Desktop/cvi25files/MVP23-FieldBags - bags.csv")
exptags <- read.csv("/Users/madelineeppley/Desktop/cvi25files/MVP23-FieldBags - tags.csv")


# fix seascape data issues - MGE made these edits to consolidate GalBay sites 8/19/2024

# we merged the Gal Bay samples - this fixes a dropped sample issue
ind$ID_SiteDate <- gsub("_Grid312", "", ind$ID_SiteDate) 
ind$ID_SiteDate <- gsub("_Grid169", "", ind$ID_SiteDate)
ind$ID_SiteDate <- gsub("_308&435", "", ind$ID_SiteDate)
ind$ID_SiteDate <- gsub("Galbay", "GalBay", ind$ID_SiteDate) # fix cap issue
#######

# so now we have our full data set in the filtered_meta_matrix, and we want to bind this together with our project-level data
# we're going to do this by assigning the same clean ID to all of the project metadata columns first, and then semi-joining everything together

# step 1 is to subset the plan into the seascape and the exp adult individuals (we have plates 6 and 7 in the plan)
exp_plan <- plan[grepl("D", plan$Label_ind), ]
sea_plan <- plan[grepl("SG", plan$Label_ind), ]

# now add a matching clean_ID to the seascape plan datasheet
sea_plan$project_type <- "CviMVP"

# Extract plate number from Extraction_plan_Label
sea_plan$plate_number <- gsub(".*PlDNA_([0-9]+).*", "\\1", sea_plan$Extraction_plan_Label)

# Get well position from Well column (already exists)
sea_plan$well_position <- sea_plan$Well

# Determine SG type 
sea_plan$SG_type <- ifelse(
  grepl("sharedLotterhosPuritz", sea_plan$Label_ind),
  "SG.sharedLotterhosPuritz",
  "SG"
)

sea_plan$individual_ID <- gsub("SG[._-](?:sharedLotterhosPuritz_)?([0-9]+).*", "\\1", sea_plan$Label_ind, perl=TRUE)

# Individual key is already in Key_ind
sea_plan$individual_key <- sea_plan$Key_ind

sea_plan$clean_ID <- paste(
  sea_plan$well_position,
  sea_plan$project_type,
  "PlDNA",
  sea_plan$plate_number,
  sea_plan$SG_type,
  sea_plan$individual_ID,
  sea_plan$individual_key,
  sep = "_"
)

# Check a few examples
head(sea_plan$clean_ID)

# start by making one seascape merged data table
seascape_data <- filtered_meta_matrix[grepl("SG", filtered_meta_matrix$clean_ID), ]
seascape_inds <- merge(seascape_data, sea_plan, by = "clean_ID") # showing 747 samples here 

seascape_inds$Label_ind <- seascape_inds$Label_ind.x

# Remove the .x and .y versions
seascape_inds <- seascape_inds[, !names(seascape_inds) %in% c("Label_ind.x", "Label_ind.y")]
seascape_inds$Label_ind <- gsub("SG\\.sharedLotterhosPuritz", "SG-sharedLotterhosPuritz", seascape_inds$Label_ind)

complete_inds1 <- merge(tissue, seascape_inds, by = c("Label_ind","Key_ind","Box_ID",
                                                      "Key_box","Vial_Label","Key_Vial"))

complete_inds2 <- merge(ind, complete_inds1, by = c("Label_ind","Key_ind","SiteDate_Key","ID_SiteDate"))

date <- date[, !colnames(date) %in% "Notes"]
complete_inds3 <- merge(complete_inds2, date, by = c("ID_SiteDate", "SiteDate_Key"), all.x = TRUE) # still have 746 through here

# keep just the columns we want
inds <- complete_inds3[,c("Label_ind","Key_ind", "clean_ID", "Box_ID","Key_box","Vial_Label","Key_Vial",
                          "Tissue","Buffer","Well","Extraction_plan_Key","PlateID_Key","ID_SiteDate",
                          "SiteDate_Key","Extraction_plan_Label","project_type","plate_number","individual_ID", "Sample.Filename",
                          "Pass.Fail","DQC","QC.call_rate","call_rate","QC.het_rate","het_rate",
                          "Average.call.rate.for.passing.samples","Sex","Shell_length_cm","Shell_width_cm",
                          "Boring_sponge","Pea_crab", "Polydora.active", "Polydora.repaired", "ID_Site", "Site_Key")]

complete_inds4 <- merge(inds, site, by = c("ID_Site", "Site_Key"), all.x = TRUE)

inds <- complete_inds4[,c("Label_ind","Key_ind", "clean_ID", "Box_ID","Key_box","Vial_Label","Key_Vial",
                          "Tissue","Buffer","Well","Extraction_plan_Key","PlateID_Key","ID_SiteDate",
                          "SiteDate_Key","Extraction_plan_Label","project_type","plate_number","individual_ID", "Sample.Filename",
                          "Pass.Fail","DQC","QC.call_rate","call_rate","QC.het_rate","het_rate",
                          "Average.call.rate.for.passing.samples","Sex","Shell_length_cm","Shell_width_cm",
                          "Boring_sponge","Pea_crab", "Polydora.active", "Polydora.repaired", "ID_Site", "Site_Key","EstuaryRegion_name", "Site_Order", "Site_Color",
                          "Region", "Region_color", "sal_firstyear", "sal_finalyear", "Mean_Annual_Salinity_ppt", "Mean_max_Salinity_ppt", 
                          "Mean_min_Salinity_ppt", "Salinity_st_dev", "temp_firstyear", "temp_finalyear", "Mean_Annual_Temperature_C", 
                          "Mean_max_temperature_C", "Mean_min_temperature_C", "Temperature_st_dev")]


# now make the exp parents Label_id

# now add a matching clean_ID to the seascape plan datasheet
exp_plan$project_type <- "CviMVPe"

# Extract plate number from Extraction_plan_Label
exp_plan$plate_number <- gsub(".*PlDNA_([0-9]+).*", "\\1", exp_plan$Extraction_plan_Label)

# Get well position from Well column (already exists)
exp_plan$well_position <- exp_plan$Well

exp_plan$individual_ID <- exp_plan$Label_ind

# Individual key is already in Key_ind
exp_plan$individual_key <- exp_plan$Key_ind

exp_plan$clean_ID <- paste(
  exp_plan$well_position,
  exp_plan$project_type,
  "PlDNA",
  exp_plan$plate_number,
  exp_plan$individual_ID,
  exp_plan$individual_key,
  sep = "_"
)

# Check a few examples
head(exp_plan$clean_ID)


# now make an experimental merged data table
# rename the Broodstock_label to be Label_ind and broodstock_Key to be Key_ind
colnames(exptissue)[colnames(exptissue) == "broodstock_Key"] <- "Key_ind"
colnames(exptissue)[colnames(exptissue) == "tissue_Label"] <- "Vial_Label"
colnames(exptissue)[colnames(exptissue) == "tissue_vial"] <- "Key_Vial"
colnames(exptissue)[colnames(exptissue) == "Key_Box"] <- "Key_box"

colnames(broodstock)[colnames(broodstock) == "broodstock_Label"] <- "Label_ind"
colnames(broodstock)[colnames(broodstock) == "broodstock_Key"] <- "Key_ind"
colnames(broodstock)[colnames(broodstock) == "Length_cm"] <- "Shell_length_cm"
colnames(broodstock)[colnames(broodstock) == "Width_cm"] <- "Shell_width_cm"

# experimental juveniles merged data table
colnames(expjuvdna)[colnames(expjuvdna) == "tagsBiopsy_key"] <- "Key_ind"
colnames(expbiopsy)[colnames(expbiopsy) == "tagsBiopsy_key"] <- "Key_ind"
colnames(expbiopsy)[colnames(expbiopsy) == "tagsBiopsy_VialLabel"] <- "Vial_Label"
colnames(expbags)[colnames(expbags) == "X689525dd"] <- "bags_key"

# create a Label_ind using the vial label # - this was used in sequencing
expbiopsy_split1 <- str_split_fixed(expbiopsy$Vial_Label, "_", 2)
expbiopsy_split <- cbind(expbiopsy_split1[,2])
colnames(expbiopsy_split) <- c("Label_ind")
expbiopsy <- cbind(expbiopsy, expbiopsy_split)

complete_juv_inds <- merge(expjuvdna, expbiopsy, by = c("Key_ind"))
complete_juv_inds2 <- merge(exptags, complete_juv_inds, by = c("tags_key"))
complete_juv_inds3 <- merge(expbags, complete_juv_inds2, by = c("bags_key"))
complete_juv_inds4 <- merge(expspawntrt, complete_juv_inds3, by = c("SpawnTrt_Key"))

complete_juv_inds5 <- merge(filtered_meta_matrix, complete_juv_inds4, by = "Label_ind", suffixes = c("_meta", "")) # get the Key_ind label here to stay after the merge
colnames(complete_juv_inds5)
#complete_juv_inds5$Key_ind <- complete_juv_inds5$Key_ind.x

complete_exp_inds <- merge(exptissue, exp_plan, by = c("Key_ind", "Key_box", "Vial_Label"))
complete_exp_inds2 <- merge(broodstock, complete_exp_inds, by = c("Label_ind", "Key_ind"))
colnames(complete_exp_inds2)

juvexp_inds <- complete_juv_inds5[,c("Label_ind", "clean_ID", "Key_ind", "Vial_Label", "extractPlateWell", "tags_key","tags_label", "bags_key", 
                                     "SpawnTrt_Key", "SpawnTrt_Label", "Date_deploy_Lewisetta", "Date_deploy_York", "predeployment_age_days",
                                     "predeployment_falcon_tube_numbers","predeployment_falcon_tube_label", "bag_site", "bags_label", "tags_timestamp", "tag_notes", 
                                     "extraction_key", "extractionID", "Final_plate_Label", "seqPlateLabel", "tagsBiopsy_timestamp_anesthetize", 
                                     "tagsBiopsy_timestamp_biopsy", "tagsBiopsy_Box_key","Sample.Filename",
                                     "Pass.Fail","DQC","QC.call_rate","call_rate","QC.het_rate","het_rate",
                                     "Average.call.rate.for.passing.samples")]

# fix some labels for the 6 experimental samples that were on plate 13
old_ids <- c("50111", "50064", "50179", "50180", "50016", "50113")
new_ids <- c("D850", "D314", "D505", "D503", "D494", "D840")
rows_to_update <- filtered_meta_matrix$Label_ind %in% old_ids
match_positions <- match(filtered_meta_matrix$Label_ind[rows_to_update], old_ids) # find positions
filtered_meta_matrix$Label_ind[rows_to_update] <- new_ids[match_positions]

# now the exp matrix
complete_exp_inds6 <- merge(filtered_meta_matrix, complete_exp_inds2, by = "Label_ind", suffixes = c("_meta", ""))


exp_inds <- complete_exp_inds6[,c("Label_ind","Key_ind", "clean_ID", "pops_Key", "Key_box","Vial_Label","Key_Vial",
                                  "Tissue","Buffer", "Extraction_plan_Key","PlateID_Key","ID_SiteDate","SiteDate_Key",
                                  "Extraction_plan_Label","Sample.Filename",
                                  "Pass.Fail","DQC","QC.call_rate","call_rate","QC.het_rate","het_rate",
                                  "Average.call.rate.for.passing.samples", "Alive", "Mortality_Date", "New_Growth", "Sex", "Sperm_Motility", "Shell_length_cm","Shell_width_cm")]


# now make one large data frame by combining the seascape and experimental inds all together

common_col_names_1 <- intersect(names(exp_inds), names(inds))

merged_inds_1 <- merge(exp_inds, inds, by = common_col_names_1, all.x=TRUE, all.y= TRUE)

common_col_names_2 <- intersect(names(merged_inds_1), names(juvexp_inds))

merged_inds_all <- merge(juvexp_inds, merged_inds_1, by = common_col_names_2, all.x=TRUE, all.y= TRUE)

# fix those plate 13's AGAIN 
plate13_labs <- c(
  "D850" = "A12_CviMVPe_PlDNA_13_D850_E95F93C1",
  "D314" = "A5_CviMVPe_PlDNA_13_D314_B19CE842",
  "D505" = "B6_CviMVPe_PlDNA_13_D505_DA998D5E",
  "D503" = "C8_CviMVPe_PlDNA_13_D503_8BAE41AF",
  "D494" = "D4_CviMVPe_PlDNA_13_D494_C3847A2F",
  "D840" = "E10_CviMVPe_PlDNA_13_D840_9DE5BDBE"
)

# Update clean_ID for these specific D-numbers
for (d_num in names(plate13_labs)) {
  # Find rows with this D-number
  matching_rows <- which(merged_inds_all$Label_ind == d_num)
  
  # Update the clean_ID
  if (length(matching_rows) > 0) {
    merged_inds_all$clean_ID[matching_rows] <- plate13_labs[d_num]
  }
}



####################
# now subset merged_inds_all into each project
seascape_samples <- merged_inds_all[grepl("^SG", merged_inds_all$Label_ind), ]
experimental_samples <- merged_inds_all[grepl("^D[0-9]", merged_inds_all$Label_ind), ]
experimental_juv_samples <- merged_inds_all[grepl("^70[0-9]", merged_inds_all$Label_ind), ]

# save all as a .csv for future processing

inds_csv <- write.csv(seascape_samples, "/Users/madelineeppley/Desktop/cvi25files/20250604_seascape_inds.csv")
exp_csv <- write.csv(experimental_samples, "/Users/madelineeppley/Desktop/cvi25files/20250604_exp_inds.csv")
exp_juv_csv <- write.csv(experimental_juv_samples, "/Users/madelineeppley/Desktop/cvi25files/20250604_juvexp_inds.csv")
all_merged_csv <- write.csv(merged_inds_all, "/Users/madelineeppley/Desktop/cvi25files/20250604_merged_inds_all.csv")

saveRDS(seascape_samples, "/Users/madelineeppley/Desktop/cvi25files/20250604_seascape_indsmatrix.rds")
saveRDS(experimental_samples, "/Users/madelineeppley/Desktop/cvi25files/20250604_exp_indsmatrix.rds")
saveRDS(experimental_juv_samples, "/Users/madelineeppley/Desktop/cvi25files/20250604_expjuv_indsmatrix.rds")
saveRDS(merged_inds_all, "/Users/madelineeppley/Desktop/cvi25files/20250604_indsmatrix.rds")

##################################
##################################
#### EXPERIMENTAL DATA ###########
##################################
##################################

# this will process JUST the experimental data all the way to a PCA
# steps in this part of the script include SNP filtering, allele freq calculations, and LD-thinning
# if looking for the seascape dataset, jump to the next part of the script

###############################################################################################################
###############################################################################################################
inds2 <- readRDS("/Users/madelineeppley/Desktop/cvi25files/20250604_exp_indsmatrix.rds") # inds data frame with full metadata
muts <- readRDS("/Users/madelineeppley/Desktop/cvi25files/20250604_mutmatrix_PASS.rds") # mutation data frame with SNP info
experimental <- readRDS("/Users/madelineeppley/Desktop/cvi25files/20250604_experimentalmatrix.rds") # genotype matrix with exp samples only

exp <- experimental
samp_full <- inds2

# step 1: have to subset just the seascape samples, otherwise dimensions don't line up ####
dim(exp) # 160 samples
dim(samp_full) # 160 samples

exp_merge <- exp
exp_merge$clean_ID <- rownames(exp)

exp_subset <- exp_merge %>% 
  semi_join(samp_full, by = "clean_ID")

exp_row_names <- rownames(exp_merge)

# Find the samples in samp_full that are not in the row names of exp_merge
non_matching_samples <- samp_full$merge_ID_inds[!samp_full$merge_ID_inds %in% exp_row_names] # none

# Display the non-matching samples
non_matching_samples # none

dim(exp_subset) # all 160 experimental samples 

samp_full_subset <- samp_full %>% 
  semi_join(exp_merge, by = "clean_ID")

dim(samp_full_subset) # all 160 experimental samples here

# now make sure that our rows line up 
order_vector <- match(samp_full_subset$clean_ID, exp_subset$clean_ID)
print(order_vector)

if (any(is.na(order_vector))) {
  stop("Some IDs in samp_full_subset are not found in exp_subset.")
} # passses

# Reorder exp based on this order
exp_ordered <- exp_subset[order_vector, , drop = FALSE] 

# now check 
geno_names <- (exp_ordered$clean_ID)
samp_names <- (samp_full_subset$clean_ID)

identical(geno_names, samp_names) # TRUE

compare <- geno_names == samp_names
compare # all TRUE


# make sure dimensions line up
if(!(dim(exp_subset)[1]==dim(samp_full_subset)[1])){
  print("Error dim don't line up")
  break
} # make sure labels line up, passes 7/17/24

# make sure labels line up
if(!identical((exp_ordered$clean_ID),samp_full_subset$clean_ID)){
  print("Error sample names don't line up")
  break
} # passes

saveRDS(samp_full_subset, file = "/Users/madelineeppley/Desktop/cvi25files/20250604_samp_full_subset_exp.rds")

# look at where our data is missing in rows
hist_rows <- rowSums(is.na(exp_ordered))
hist(hist_rows)


dim(exp_ordered) #195026
dim(muts) #195025
exp_ordered <- exp_ordered %>% select(-clean_ID)
dim(exp_ordered) #195025, matches

# look at where our data is missing in columns
cols_hist <- colSums(is.na(exp_ordered))
hist(cols_hist)

# Filter out cols with missing data
missing_threshold <- 0.10  #10%
snp_missing_prop <- colMeans(is.na(exp_ordered))
exp_cleaned_columns <- exp_ordered[, snp_missing_prop <= missing_threshold]
dim(exp_cleaned_columns) # down to 188096 SNPs, removed 6,929 SNPs

# Filter out the rows with missing data
ind_missing_prop <- rowMeans(is.na(exp_ordered))
inds_missing <- which(ind_missing_prop > missing_threshold) # length 0, all inds pass

# filter the muts database to just keep our SNPs that meet the missingness threshold
muts_filtered <- muts[snp_missing_prop <= missing_threshold, ]
dim(muts_filtered) #188096 SNPs, matches

# checks to make sure that filtered SNP set still aligns 

# store info
geno_names6 <- (colnames(exp_cleaned_columns))
head(geno_names6)
mut_names4 <- (muts_filtered$Affx.ID)
head(mut_names4)

# now check the filtering again
identical(geno_names6, mut_names4) # TRUE

compare <- geno_names6 == mut_names4
# compare # all TRUE when viewing

# make sure labels line up
if(!identical((geno_names6), mut_names4)){
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
geno_names7 <- (colnames(exp_cleaned_columns_sort))
head(geno_names7)
mut_names7 <- (muts_filt_ord$Affx.ID)
head(mut_names7)

# now check the filtering again
identical(geno_names7, mut_names7) # TRUE

compare <- geno_names7 == mut_names7
# compare # all TRUE when viewing

# make sure labels line up
if(!identical((geno_names7), mut_names7)){
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
dim(GEN_filtered) #154102 SNPs

# filter the muts data frame to just keep our SNPs with full data
muts_maf_filtered <- muts_filt_ord[muts_filt_ord$Affx.ID %in% keep_snps, ]
dim(muts_maf_filtered) #154102 SNPs
saveRDS(muts_maf_filtered, file = "/Users/madelineeppley/Desktop/cvi25files/20250604_FULLmuts_exp.rds")

# Impute #####################################################################
# use snmf on our dataset of 154k SNPs
write.lfmm(GEN_filtered, "/Users/madelineeppley/Desktop/cvi25files/20250604_genotypes_exp.lfmm")

exp.project.snmf <- snmf("/Users/madelineeppley/Desktop/cvi25files/20250604_genotypes_exp.lfmm", K = 2, repetitions = 10, ploidy = 2, entropy = TRUE, project = "new")

# run with the lowest cross-entropy value and impute
best = which.min(cross.entropy(exp.project.snmf, K = 2))
impute(exp.project.snmf, "/Users/madelineeppley/Desktop/cvi25files/20250604_genotypes_exp.lfmm", method = 'mode', K = 2, run = best)

# store the imputed SNP set as a matrix 
GEN_imputed <- as.data.frame(read.table("/Users/madelineeppley/Desktop/cvi25files/20250604_genotypes_exp.lfmm_imputed.lfmm", header = FALSE))
saveRDS(GEN_imputed, file = "/Users/madelineeppley/Desktop/cvi25files/20250604_FULLSNPs_exp.rds") # this is the FULL SNP set

GEN_imputed <- readRDS("/Users/madelineeppley/Desktop/cvi25files/20250604_FULLSNPs_exp.rds")
gen_impute <- add_code256(big_copy(GEN_imputed,type="raw"),code=bigsnpr:::CODE_012)

# double check our dimensions
muts_maf_filtered <- readRDS("/Users/madelineeppley/Desktop/cvi25files/20250604_FULLmuts_exp.rds")
dim(GEN_imputed)
dim(muts_maf_filtered) # matches

thr.r2 <- 0.2 #default
# create the thinned SNP set 
pca_inv <- snp_autoSVD(gen_impute,
                       infos.chr= muts_maf_filtered$Chromosome,
                       infos.pos = muts_maf_filtered$Position,
                       thr.r2=thr.r2,  # correlation LD
                       size= 100/thr.r2, # explore
)

thinned_snps <- attr(pca_inv, which="subset") # SNPs n=105356
saveRDS(thinned_snps, file = "/Users/madelineeppley/Desktop/cvi25files/20250604_THINNEDSNPs_exp.rds")

# plot the eigenvectors 
plot(pca_inv$d)

# extract just the first 6 axes
pca_data <- as.data.frame(pca_inv$u[, 1:6]) 

colnames(pca_data) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")

samp_full_subset <- readRDS("/Users/madelineeppley/Desktop/cvi25files/20250604_samp_full_subset_exp.rds")
samp_full_subset$Plate <- gsub(".*_(PlDNA_[0-9]+)_.*", "\\1", samp_full_subset$clean_ID)
pca_data$Plate <- samp_full_subset$Plate
pca_data$ID_SiteDate <- samp_full_subset$ID_SiteDate

# export CSV data
write.csv(pca_data, "/Users/madelineeppley/Desktop/cvi25files/20250604_pca_data_exp.csv")



library(ggplot2)
library(LEA)
library(scatterpie)
library(sf)
library(rnaturalearth)
library(dplyr)


# can update region colors to whatever we want to see here
region_colors <- c(
  "LOLA" = "#440154", 
  "ME-HogIs" = "#6ece58", 
  "NH-GrtBay" = "#35b779", 
  "FL-KingPlan" = "#26828e", 
  "LA-SisLake" = "#31688e", 
  "TX-CapBay" = "#3e4989",
  "VA-DeepWatSh" = "#1f9e89",
  "DEBY" = "#482878")

pca_data <- read.csv("/Users/madelineeppley/Desktop/cvi25files/20250604_pca_data_exp.csv")

# plot PCA by plate to check for batch effects - just see pl6 and pl7 here
pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Plate)) +
  geom_point(size = 1) + 
  labs(title = "PCA - Plate Extractions, exp Dataset", x = "PC1", y = "PC2", color = "Plate") +
  theme_minimal()

pca_plot

# PCs 3 and 4 checking for batch effects
pca_plot2 <- ggplot(pca_data, aes(x = PC3, y = PC4, color = Plate)) +
  geom_point(size = 1) + 
  labs(title = "PCA - Plate Extractions, Whole Dataset", x = "PC3", y = "PC4", color = "Plate") +
  theme_minimal()

pca_plot2


# PCA plot PCs 1 and 2

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

pca_plot3



# now PCs 3 and 4

centroids34 <- pca_data %>%
  group_by(ID_SiteDate) %>%
  summarize(PC3 = mean(PC3), PC4 = mean(PC4))

pca_plot4 <- ggplot(pca_data, aes(x = PC3, y = PC4, color = ID_SiteDate)) +
  geom_point(size = 1) +
  stat_ellipse(aes(group = ID_SiteDate), level = 0.95) +
  #geom_text_repel(data = centroids34, aes(x = PC3, y = PC4, label = ID_SiteDate), 
  #size = 3, fontface = "bold", box.padding = 0.5, point.padding = 0.5) +
  labs(title = "PCA Plot exp Samples", x = "PC3", y = "PC4", color = "Region") +
  theme_minimal()

pca_plot4


# snmf ancestry analysis

# subset the last geno matrix that we made (GEN_filtered) with our thinned SNPs list from the auto_SVD function
gen_filtered_subset <- GEN_imputed[, thinned_snps]

# or read in if not in env
samp_full_subset <- readRDS("/Users/madelineeppley/Desktop/cvi25files/20250604_samp_full_subset_exp.rds")
region_labels <- samp_full_subset$ID_SiteDate

# now save our matrix as a .geno format to run the snmf analysis
write.geno(gen_filtered_subset, "/Users/madelineeppley/Desktop/cvi25files/20250604geno_imputedfile_exp.geno")
gen_filtered_subset <- read.geno("/Users/madelineeppley/Desktop/cvi25files/20250604geno_imputedfile_exp.geno")

# now generate the snmf project for our plate
snmf_exp <- snmf("/Users/madelineeppley/Desktop/cvi25files/20250604geno_imputedfile_exp.geno", K = 2, repetitions = 10, ploidy = 2, entropy = TRUE, project = "continue")

# plot cross-entropy criterion of all runs of the project
plot(snmf_exp, cex = 1.2, col = "lightblue", pch = 19)

# get the cross-entropy of the 10 runs for k=2
ce = cross.entropy(snmf_exp, K = 2)

# select the run with the lowest cross-entropy for k=2
best = which.min(ce)

# display the q-matrix
qmatrix <- as.matrix(Q(snmf_exp, K = 2, run = best))
str(qmatrix)

my.colors <- c("#FF9B71", "#E84855")

# plot the ancestry proportions
barchart(snmf_exp, K = 2, run = best, 
         border = NA, space = 0, col = my.colors, 
         xlab = "Individuals", ylab = "Ancestry proportions", 
         main = "Ancestry matrix for Experimental Samples") -> bp

reordered_labels <- region_labels[bp$order]

axis(1, at = 1:length(bp$order), 
     labels = reordered_labels, las = 3, cex.axis = .4)

# create snmf plot results on map 

# load US coastline 
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

# read site data
site_data <- read.csv("/Users/madelineeppley/Desktop/cvi25files/MVP23-FieldBags\ -\ spawn_trt2.csv")

# get the Q-matrix from snmf results
geno <- read.geno("/Users/madelineeppley/Desktop/cvi25files/20250604geno_imputedfile_exp.geno")
snmf_project <- load.snmfProject("/Users/madelineeppley/Desktop/cvi25files/20250604geno_imputedfile_exp.snmfProject")
best_run <- which.min(cross.entropy(snmf_project, K = 2))  # choose the best run
Q_matrix <- Q(snmf_project, K = 2, run = best_run)  # extract best run into q-matrix


# pull the site and region labels and merge them with the ancestry proportions
region_labels <- samp_full_subset$ID_SiteDate
region_labels <- as.data.frame(region_labels)
colnames(region_labels) <- "ID_SiteDate"
region_labels <- samp_full_subset[, c("ID_SiteDate", "clean_ID")]
region_labels$Q1 <- Q_matrix[, 1]  # q matrix ancestry group 1
region_labels$Q2 <- Q_matrix[, 2]  # q matrix ancestry group 2

colnames(site_data)[2] = "ID_SiteDate"

# now we have an issue where VA-JR-DeepWatSh and VA-DeepWatSh don't match ... let's rename
site_data$ID_SiteDate <- gsub('VA-JR-DeepWatSh','VA-DeepWatSh', site_data$ID_SiteDate)


# Merge ancestry proportions into the full data
merged_data <- merge(site_data, samp_full_subset, by = "ID_SiteDate")
merged_data <- merge(region_labels, merged_data, by = "clean_ID", all.x = TRUE)


# now plot on the map - create a base map with our coastline as the outline of the plot
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

#### re-try the plotting with some better visual arrangements
# rename our sides AGAIN
site_to_code <- c(
  "ME-HogIs" = "W6_ME",
  "NH-GrtBay" = "W5_NH",
  "FL-KingPlan" = "W3_FL",
  "LA-SisLake" = "W2_LA",
  "TX-CapBay" = "W1_TX",
  "VA-DeepWatSh" = "W4_VA",
  "DEBY" = "S2-DEBY",
  "LOLA" = "S1-LOLA"
)

coded_labels <- site_to_code[region_labels$ID_SiteDate]

# pop order that we actually want
population_order <- c("W1_TX", "W2_LA", "W3_FL", "S1-LOLA", "S2-DEBY", "W4_VA", "W5_NH", "W6_ME")

pop_indices <- list()
for (pop in population_order) {
  pop_indices[[pop]] <- which(coded_labels == pop)
}

# order q matrix
ordered_q <- matrix(0, nrow = nrow(Q_matrix), ncol = ncol(Q_matrix))
current_row <- 1
for (pop in population_order) {
  indices <- pop_indices[[pop]]
  if (length(indices) > 0) {
    ordered_q[current_row:(current_row + length(indices) - 1), ] <- Q_matrix[indices, ]
    current_row <- current_row + length(indices)
  }
}

par(mar = c(8, 4, 2, 1))
bp <- barplot(t(ordered_q), 
              col = my.colors,
              border = NA, 
              space = 0,
              xlab = "", 
              ylab = "Ancestry proportions",
              main = "Ancestry matrix",
              axes = FALSE)

axis(2)

# pop edges
pop_sizes <- sapply(pop_indices, length)
pop_boundaries <- cumsum(pop_sizes)
pop_boundaries <- pop_boundaries[-length(pop_boundaries)]

for (i in 1:length(pop_boundaries)) {
  if (pop_boundaries[i] < length(bp)) {
    line_pos <- (bp[pop_boundaries[i]] + bp[pop_boundaries[i] + 1]) / 2
    abline(v = line_pos, lwd = 1.5)
  }
}

pop_centers <- numeric(length(population_order))
current_pos <- 0
for (i in 1:length(population_order)) {
  pop <- population_order[i]
  count <- pop_sizes[pop]
  if (count > 0) {
    start_idx <- current_pos + 1
    end_idx <- current_pos + count
    pop_centers[i] <- mean(bp[start_idx:end_idx])
  }
  current_pos <- current_pos + count
}


axis(1, at = pop_centers, labels = population_order, las = 2, cex.axis = 0.8)

###############################################################################
######################### END EXPERIMENTAL ####################################
###############################################################################
