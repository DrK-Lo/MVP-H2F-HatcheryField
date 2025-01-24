###################################
### OYSTER PRELIMINARY ANALYSIS ###
###################################

## setup
########
# set wd
setwd("~/MVP_Oyster_Analysis_Camille/MVP_Oysters_Analysis_Camille")

# load libraries
library(tidyverse) # for data wrangling
library(lubridate) # for dates
library(ggplot2) # for plotting
library(plotrix) # for std error
########


## load data
############
bags <- read.csv("data/ooters_prelim/MVP23-FieldBags - bags.csv")
mort <- read.csv("data/ooters_prelim/MVP23-FieldBags - mortality.csv")
pheno_bags <- read.csv("data/ooters_prelim/MVP23-FieldBags - phenotyping_bag.csv")
pheno <- read.csv("data/ooters_prelim/MVP23-FieldBags - phenotyping.csv")
expsites <- read.csv("data/ooters_prelim/MVP23-FieldBags - sites.csv")
spawntrt <- read.csv("data/ooters_prelim/MVP23-FieldBags - spawn_trt2.csv")
tags <- read.csv("data/ooters_prelim/MVP23-FieldBags - tags.csv")
tagleng <- read.csv("data/ooters_prelim/MVP23-FieldBags - tagsLength.csv")
############


## merging data
###############
# merging bags and mortality
head(bags)
colnames(bags) <- c("bags_key", "bag_site", "bags_label", "SpawnTrt_Key")
head(mort)
merge1 <- merge(bags, mort, by = c("bags_key"), all = T)
head(merge1)

# add spawntrt data
head(merge1)
head(spawntrt)
merge2 <- merge(merge1, spawntrt, by = c("SpawnTrt_Key"), all = T)
head(merge2)

# add phenotyping data
#colnames(merge2)
#colnames(pheno_bags)
#merge3 <- merge(merge2, pheno_bags, by = c("bags_key"))
#colnames(pheno)

#merge4 <- merge(merge3, pheno, by = c("phenotyping_bag_key"))
#head(merge4)

# leave pheno data for later
# leave tags data for later
tagsbags <- merge(tags, bags, by = c("bags_key"), all = T)
tagsbags1 <- merge(tagsbags, tagleng, by = c("tags_key"), all = T)
###############

# clean data
############
# for mort data...
# trim datasets
colnames(merge2)
merge4_trim <- merge2[,c("bags_key","bag_site","bags_label","SpawnTrt_Key","SpawnTrt_Label",
                         "latitudeDecimal","longitudeDecimal","mortality_key","mortality_label",
                         "mortality_timestamp","alive_count","dead_count","alive_returned")]

# split timestamps to get dates - I'll use mortality_timestamp for this
# dates are 4/29/24 - 5/7/24 for most recent monitoring event
# quick checks - looks good!
head(merge4_trim[,c("mortality_timestamp")])
length(merge4_trim[,c("mortality_timestamp")])

# split timestamp column into two
merge4_trim[,c("mortality_timestamp")]
datetime_split <- str_split_fixed(merge4_trim[,c("mortality_timestamp")], " ", 2) # split timestamp
colnames(datetime_split) <- c("mortality_date", "mortality_time") # name columns
merge5_trim <- cbind(merge4_trim, datetime_split) # add to full dataset

# use dates to select monitoring event before thinning
# dates are 4/29/24 - 5/7/24
merge5_trim$mortality_date <- mdy(merge5_trim$mortality_date)
merge6_trim <-merge5_trim %>%
  filter(as_date(mortality_date) >= as_date("2024-04-29") & as_date(mortality_date) < as_date("2024-11-06"))

# take out practice rows
merge5_rm <- merge5_trim[!merge5_trim$SpawnTrt_Label == "Practice",]

# also put a monitoring event column in here
merge5_rm$monitoring_event <- ifelse(as_date(merge5_rm$mortality_date) > as_date("2023-11-16"),
                                     ifelse(as_date(merge5_rm$mortality_date) > as_date("2024-05-09"), 3, 2),
                                     1)

# for tags data...
# trim dataset
tagsbags2 <- tagsbags1[,c("tags_label","tags_timestamp","bag_site","bags_label",
                          "length","width","tagsLength_timestamp")]

# split timestamps to get dates, dates 4/29-5/7 for most recent monitoring
# split timestamp column into two
tagsbags2[,c("tagsLength_timestamp")]
datetime_split_tag <- str_split_fixed(tagsbags2[,c("tagsLength_timestamp")], " ", 2) # split timestamp
colnames(datetime_split_tag) <- c("tagsLength_date", "tagsLength_time") # name columns
tagsbags3 <- cbind(tagsbags2, datetime_split_tag) # add to full dataset

# use dates to split by monitoring event
# dates are 11/7/23 - 11/15/23, 4/29/24 - 5/7/24
tagsbags3$tagsLength_date <- mdy(tagsbags3$tagsLength_date)

# remove practice
tagsbags4 <- tagsbags3[!(tagsbags3$tagsLength_date == "2024-03-13"),]
tagsbags5 <- tagsbags4[!(is.na(tagsbags4$tagsLength_date)),]

tagsbags5$monitoring_event <- ifelse(as_date(tagsbags5$tagsLength_date) > as_date("2023-11-16"),
                                     ifelse(as_date(tagsbags5$tagsLength_date) > as_date("2024-05-09"), 3, 2),
                                     1)
############

# analyze bag level mort trends
###############################
# check bags
levels(as.factor(merge6_trim$bags_label))

# define number of bags and create matrix to store data
bag <- levels(as.factor(merge6_trim$bags_label))
mortality_bag <- matrix(data = NA, nrow = length(bag), ncol = 2)

# function that calculates mortality for each bag at a given timepoint
# note that this doesn't calc mortality for the whole experiment, just from one
# monitoring event to the next
for (i in 1:length(bag)) {
  mortality_bag[i,1] = bag[i]
  alive = merge6_trim[merge6_trim$bags_label == bag[i],]$alive_count
  dead = merge6_trim[merge6_trim$bags_label == bag[i],]$dead_count
  mortality_bag[i,2] = alive / (alive + dead)
}

mortality_bag <- as.data.frame(mortality_bag)
colnames(mortality_bag) <- c("bags_label","bag_mortality")
mortality_bag

# if I want survival from the beginning of the experiment...
merge6.5_trim <-merge5_trim %>%
  filter(as_date(mortality_date) < as_date("2024-4-29"))

mortality_bag_overall <- matrix(data = NA, nrow = length(bag), ncol = 2)
for (i in 1:length(bag)) {
  mortality_bag_overall[i,1] = bag[i]
  alive_orig = merge6.5_trim[merge6.5_trim$bags_label == bag[i],]$alive_count + merge6.5_trim[merge6.5_trim$bags_label == bag[i],]$dead_count
  alive_now = merge6_trim[merge6_trim$bags_label == bag[i],]$alive_count
  mortality_bag_overall[i,2] = alive_now / (alive_orig)
}

mortality_bag_overall <- as.data.frame(mortality_bag_overall)
colnames(mortality_bag_overall) <- c("bags_label","overall_bag_mortality")
mortality_bag_overall

barplot(as.numeric(mortality_bag_overall$overall_bag_mortality), las = 2, 
        names.arg = mortality_bag_overall$bags_label)

# try calculating survival at each time point
event1 <- merge5_rm[merge5_rm$monitoring_event == 1,]
event2 <- merge5_rm[merge5_rm$monitoring_event == 2,]
event3 <- merge5_rm[merge5_rm$monitoring_event == 3,]

mortality_bag_overall <- matrix(data = NA, nrow = length(bag), ncol = 4)
for (i in 1:length(bag)) {
  mortality_bag_overall[i,1] = bag[i]
  alive_orig = event1[event1$bags_label == bag[i],]$alive_count + event1[event1$bags_label == bag[i],]$dead_count
  alive_one = event1[event1$bags_label == bag[i],]$alive_count
  alive_two = event2[event2$bags_label == bag[i],]$alive_count
  alive_three = event3[event3$bags_label == bag[i],]$alive_count
  mortality_bag_overall[i,2] = alive_one / alive_orig
  mortality_bag_overall[i,3] = alive_two / alive_orig
  mortality_bag_overall[i,4] = alive_three / event2$alive_returned[i]
}
mortality_bag_overall_df <- as.data.frame(mortality_bag_overall)
colnames(mortality_bag_overall_df) <- c("bags_label","survival_1","survival_2","survival_3")
###############################


# analyze pop level mort trends
###############################
# determine bag population
mortality_bag$pop <- str_split_fixed(mortality_bag$bags_label, "-", 3)[,2]
mortality_bag$bag_num <- str_split_fixed(mortality_bag$bags_label, "-", 3)[,3]

mortality_bag_overall_df$pop <- str_split_fixed(mortality_bag_overall_df$bags_label, "-", 3)[,2]
mortality_bag_overall_df$bag_num <- str_split_fixed(mortality_bag_overall_df$bags_label, "-", 3)[,3]

# separate by site
mortality_bag$site <- ifelse(mortality_bag$bag_num > 3, "Lewisetta", "York River")
mortality_bag_overall_df$site <- ifelse(mortality_bag_overall_df$bag_num > 3, "Lewisetta", "York River")
write.csv(mortality_bag_overall_df, "data/ooters_prelim/mortality_bag_overall_12092024.csv")

# overall population survival
mortality_bag_pop_surv <- mortality_bag_overall_df %>% group_by(pop) %>% 
  summarise(mean_surv_1 = mean(as.numeric(survival_1)), 
            sd_surv_1 = sd(as.numeric(survival_1)),
            se_surv_1 = std.error(as.numeric(survival_1)),
            mean_surv_2 = mean(as.numeric(survival_2)), 
            sd_surv_2 = sd(as.numeric(survival_2)),
            se_surv_2 = std.error(as.numeric(survival_2)),
            mean_surv_3 = mean(as.numeric(survival_3)), 
            sd_surv_3 = sd(as.numeric(survival_3)),
            se_surv_3 = std.error(as.numeric(survival_3)),
            .groups = 'drop')
mortality_bag_pop_surv

# plot it
barplot(as.numeric(mortality_bag_pop_surv$mean_surv), las = 2, 
        names.arg = mortality_bag_pop_surv$pop, ylim = c(0,1))

# per site survival
mortality_bag_site_surv <- mortality_bag_overall %>% group_by(site) %>%
  summarise(mean_surv = mean(as.numeric(overall_bag_mortality)),
            .groups = 'drop')
mortality_bag_site_surv

# plot it
barplot(as.numeric(mortality_bag_site_surv$mean_surv), las = 2, 
        names.arg = mortality_bag_site_surv$site, ylim = c(0,1))

# per site population survival
mortality_bag_popsite_surv <- mortality_bag_overall %>% group_by(site,pop) %>%
  summarise(mean_surv = mean(as.numeric(overall_bag_mortality)), 
            sd_surv = sd(as.numeric(overall_bag_mortality)),
            se_surv = std.error(as.numeric(overall_bag_mortality)),
            .groups = 'drop')
mortality_bag_popsite_surv
mortality_bag_popsite_surv$col <- ifelse(mortality_bag_popsite_surv$site == "Lewisetta", "red", "blue")
write.csv(mortality_bag_popsite_surv, "data/ooters_prelim/mortality_sitepop_oyster_10312024.csv")

# plot it
barplot(as.numeric(mortality_bag_popsite_surv$mean_surv), las = 2, 
        names.arg = mortality_bag_popsite_surv$pop, ylim = c(0,1), 
        col = mortality_bag_popsite_surv$col, beside = T)
legend("topleft",  fill = c("red", "blue"), c("Lewisetta", "York River"), horiz = T)

# better plotting - for some reason legend isn't working
ggplot(mortality_bag_popsite_surv[!mortality_bag_popsite_surv$pop == "LARMIX" & !mortality_bag_popsite_surv$pop == "SEEDMIX",], 
       aes(x = pop, y = mean_surv, fill = site)) +
  theme_classic() +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(name = "Site", values = c("cadetblue2","cyan4"),
                    labels = c("Lewisetta","York River")) +
  theme(plot.title = element_text(size = 24), 
        legend.title = element_text(size = 15),
        legend.position = "right") +
  labs(fill = "Site") +
  xlab("Population") + 
  ylab("Mean Survival") +
  ggtitle("Survival by Population & Site")

ggplot(mortality_bag_pop_surv, aes(x = pop, y = mean_surv, fill = pop)) +
  theme_classic() +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  theme(plot.title = element_text(size = 24), 
        legend.title = element_text(size = 15),
        legend.position = "right") +
  xlab("Population") + 
  ylab("Mean Survival") +
  ggtitle("Survival by Population")

# need to now have all of the means in one column, all the sd, the se - I've really messed up something with the other graphs
mortality_bag_pop_surv_df <- data.frame(mean = c(mortality_bag_pop_surv$mean_surv_1,
                                                 mortality_bag_pop_surv$mean_surv_2,
                                                 mortality_bag_pop_surv$mean_surv_3),
                                        sd = c(mortality_bag_pop_surv$sd_surv_1,
                                               mortality_bag_pop_surv$sd_surv_2,
                                               mortality_bag_pop_surv$sd_surv_3),
                                        se = c(mortality_bag_pop_surv$se_surv_1,
                                               mortality_bag_pop_surv$se_surv_2,
                                               mortality_bag_pop_surv$se_surv_2),
                                        monitoring_event = c(rep(1,10),rep(2,10),rep(3,10)),
                                        pop = rep(mortality_bag_pop_surv$pop,3))
mortality_bag_pop_surv_df

# plot mortality at all time points
ggplot(mortality_bag_pop_surv_df) +
  theme_classic() +
  geom_point(aes(x = monitoring_event, y = mean, color = pop)) +
  geom_line(aes(x = monitoring_event, y = mean, color = pop))
###############################

# survival as a function of environmental distance
##################################################
# first download environmental data for experimental sites
library(sdmpredictors)
library(raster)

# pull env layers
layers <- load_layers(layercodes = c("MS_biogeo13_sst_mean_5m",
                                     "MS_biogeo15_sst_max_5m",
                                     "MS_biogeo14_sst_min_5m",
                                     "MS_biogeo08_sss_mean_5m",
                                     "MS_biogeo10_sss_max_5m",
                                     "MS_biogeo09_sss_min_5m"))

# extract environmental values from layers - this isn't working, might be no data at these points
sites_environ <- data.frame(Name=expsites$X, depth=raster::extract(layers,expsites[2:3]), raster::extract(layers,expsites[2:3]))
head(sites_environ)

# I have York River monitoring data
# for period from May 2023-May 2024...
york_env <- data.frame(site_name = "York", lat = 37.247284,
                       Mean_min_temperature_C = 3.95, 
                       Mean_max_temperature_C = 30.61, 
                       Mean_Annual_Temperature_C = 17.44,
                       Mean_min_Salinity_ppt = 12.10, 
                       Mean_max_Salinity_ppt = 25.06, 
                       Mean_Annual_Salinity_ppt = 20.06)

# and env data for other sites
envleng <- read.csv("data/ooters_prelim/Survival_Length_Envr_Data.csv")
env_sites <- envleng[,1:8]
env_sites <- env_sites[colnames(york_env)]

# put it together
env_york_sites <- rbind(york_env, env_sites)

# calculate environmental distance
library(vegan)
env_dist <- round(vegdist(env_york_sites[,3:8], method = "euclidian",
                         upper = FALSE, diag = TRUE), 4)
env_dist_df <- as.data.frame(as.matrix(env_dist))
colnames(env_dist_df) <- rownames(env_dist_df) <- env_york_sites$site_name
env_dist_df

# put together environmental distance with survival in York river
# york river survival
york_surv <- mortality_bag_popsite_surv[mortality_bag_popsite_surv$site == "York River",c("pop","mean_surv")]

env_dists_to_york <- data.frame(pop = colnames(env_dist_df[1,2:9]), dist = as.numeric(env_dist_df[1,2:9]))
dist_surv <- merge(york_surv, env_dists_to_york, by = c("pop"))
cor(dist_surv$mean_surv, dist_surv$dist) # 0.1221391

ggplot(data = dist_surv,aes(x = dist, y = mean_surv)) + 
  theme_classic() +
  geom_point(aes(x = dist, y = mean_surv, col = pop), size = 4) + 
  geom_smooth(method = "lm", color = "black") +
  ggtitle("Survival as a Function of Environmental Distance") + 
  xlab("Euclidean Environmental Distance") + 
  ylab("Mean Survival")

summary(lm(dist_surv$mean_surv ~ dist_surv$dist))
##################################################

# tag lengths at each monitoring event
######################################
# start with pop level averages at each monitoring event
levels(as.factor(tagsbags5$bags_label))
tagsbags5$pop <- str_split_fixed(tagsbags5$bags_label, "-", 3)[,2]
levels(as.factor(tagsbags5$pop))
tagsbags6 <- tagsbags5[!(tagsbags5$pop == ""),]
tagsbags6 <- tagsbags6[!(tagsbags6$pop == "LEW"),]
tagsbags6 <- tagsbags6[!(tagsbags6$pop == "YORK"),]
levels(as.factor(tagsbags6$pop))

# calc pop level averages for each event
event1_york <- tapply(tagsbags6[tagsbags6$monitoring_event == 1 & tagsbags6$bag_site == "YorkRiver",]$length, 
                      tagsbags6[tagsbags6$monitoring_event == 1 & tagsbags6$bag_site == "YorkRiver",]$pop, 
                 mean)
event1_lew <- tapply(tagsbags6[tagsbags6$monitoring_event == 1 & tagsbags6$bag_site == "Lewisetta",]$length, 
                     tagsbags6[tagsbags6$monitoring_event == 1 & tagsbags6$bag_site == "Lewisetta",]$pop, 
                     mean)
event1_tot <- tapply(tagsbags6[tagsbags6$monitoring_event == 1,]$length, 
                     tagsbags6[tagsbags6$monitoring_event == 1,]$pop, 
                     mean)
event2_york <- tapply(tagsbags6[tagsbags6$monitoring_event == 2 & tagsbags6$bag_site == "YorkRiver",]$length, 
                      tagsbags6[tagsbags6$monitoring_event == 2 & tagsbags6$bag_site == "YorkRiver",]$pop, 
                      mean)
event2_lew <- tapply(tagsbags6[tagsbags6$monitoring_event == 2 & tagsbags6$bag_site == "Lewisetta",]$length, 
                     tagsbags6[tagsbags6$monitoring_event == 2 & tagsbags6$bag_site == "Lewisetta",]$pop, 
                     mean)
event2_tot <- tapply(tagsbags6[tagsbags6$monitoring_event == 2,]$length, 
                     tagsbags6[tagsbags6$monitoring_event == 2,]$pop, 
                     mean)
event3_york <- tapply(tagsbags6[tagsbags6$monitoring_event == 3 & tagsbags6$bag_site == "YorkRiver",]$length, 
                      tagsbags6[tagsbags6$monitoring_event == 3 & tagsbags6$bag_site == "YorkRiver",]$pop, 
                      mean)
event3_lew <- tapply(tagsbags6[tagsbags6$monitoring_event == 3 & tagsbags6$bag_site == "Lewisetta",]$length, 
                     tagsbags6[tagsbags6$monitoring_event == 3 & tagsbags6$bag_site == "Lewisetta",]$pop, 
                     mean)
event3_tot <- tapply(tagsbags6[tagsbags6$monitoring_event == 3,]$length, 
                     tagsbags6[tagsbags6$monitoring_event == 3,]$pop, 
                     mean)

# put it all into a df
df <- data.frame(site = "Lewisetta", pop = rownames(event1_lew), 
                 monitoring_event = 1, length = event1_lew)
df1 <- data.frame(site = "Lewisetta", pop = rownames(event1_lew),
                  monitoring_event = 2, length = event2_lew)
df3 <- data.frame(site = "Lewisetta", pop = rownames(event1_lew),
                  monitoring_event = 3, length = event3_lew)
df4 <- data.frame(site = "York", pop = rownames(event1_york), 
                 monitoring_event = 1, length = event1_york)
df5 <- data.frame(site = "York", pop = rownames(event1_york),
                  monitoring_event = 2, length = event2_york)
df6 <- data.frame(site = "York", pop = rownames(event1_york),
                  monitoring_event = 3, length = event3_york)
df7 <- data.frame(site = "Overall", pop = rownames(event1_tot), 
                 monitoring_event = 1, length = event1_tot)
df8 <- data.frame(site = "Overall", pop = rownames(event1_lew),
                  monitoring_event = 2, length = event2_tot)
df9 <- data.frame(site = "Overall", pop = rownames(event1_lew),
                  monitoring_event = 3, length = event3_tot)

lengths <- rbind(df, df1, df3, df4, df5, df6, df7, df8, df9)

# plot
lew_len <- ggplot(data = lengths[lengths$site == "Lewisetta" & !lengths$pop == "LARMIX" & !lengths$pop == "SEEDMIX",]) + 
  geom_point(aes(x = monitoring_event, y = length, col = pop)) + 
  geom_line(aes(x = monitoring_event, y = length, col = pop)) + 
  ylim(20,80) + 
  labs(color = "Population") +
  ylab("Shell Length (mm)") +
  scale_x_continuous("Monitoring Event", breaks = c(1,2,3),
                     labels = c("Fall '23", "Spring '24", "Fall '24")) +
  theme_classic() + 
  ggtitle("Lewisetta: Oyster Shell Length thru Time")
york_len <- ggplot(data = lengths[lengths$site == "York" & !lengths$pop == "LARMIX" & !lengths$pop == "SEEDMIX",,]) + 
  geom_point(aes(x = monitoring_event, y = length, col = pop)) + 
  geom_line(aes(x = monitoring_event, y = length, col = pop)) + 
  ylim(20,80) + 
  labs(color = "Population") +
  ylab("Shell Length (mm)") +
  scale_x_continuous("Monitoring Event", breaks = c(1,2,3),
                     labels = c("Fall '23", "Spring '24", "Fall '24")) +
  theme_classic() +
  ggtitle("York River: Oyster Shell Length thru Time")
all_len <- ggplot(data = lengths[lengths$site == "Overall",]) + 
  geom_point(aes(x = monitoring_event, y = length, col = pop)) + 
  geom_line(aes(x = monitoring_event, y = length, col = pop)) + 
  ylim(20,80) + 
  theme_classic()

lew_len
york_len
######################################
