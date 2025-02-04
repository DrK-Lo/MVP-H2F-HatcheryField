library(ggplot2)
library(LEA)
library(scatterpie)
library(sf)
library(rnaturalearth)
library(dplyr)

setwd("~/Desktop")

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

pca_data <- read.csv("/Users/madelineeppley/Desktop/20250203_pca_data_exp.csv")

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
region_labels <- samp_full_subset$ID_SiteDate

# now save our matrix as a .geno format to run the snmf analysis
write.geno(gen_filtered_subset, "/Users/madelineeppley/Desktop/cvi-diploid-files/20250204geno_imputedfile_exp.geno")


# now generate the snmf project for our plate
snmf_exp <- snmf("/Users/madelineeppley/Desktop/cvi-diploid-files/20250204geno_imputedfile_exp.geno", K = 2, repetitions = 10, ploidy = 2, entropy = TRUE, project = "new")

# plot cross-entropy criterion of all runs of the project
plot(snmf_exp, cex = 1.2, col = "lightblue", pch = 19)

# get the cross-entropy of the 10 runs for k=2
ce = cross.entropy(snmf_exp, K = 2)

# select the run with the lowest cross-entropy for k=2
best = which.min(ce)

# display the q-matrix

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
site_data <- read.csv("/Users/madelineeppley/Desktop/MVP23-FieldBags\ -\ spawn_trt2.csv")

# get the Q-matrix from snmf results
geno <- read.geno("/Users/madelineeppley/Desktop/cvi-diploid-files/20250204geno_imputedfile_exp.geno")
snmf_project <- load.snmfProject("/Users/madelineeppley/Desktop/cvi-diploid-files/20250204geno_imputedfile_exp.snmfProject")
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



