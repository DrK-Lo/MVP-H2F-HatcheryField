### H2F barcharts for dermo & MSX 
### LEW and YORK rivers
#### Madeline Eppley 8/15/25
# edited 9/11/2025 for time series - now we want to include nov 2023 and may 2024, although my original notes do not have these dates, NM suggested to add on 9/11/25


# we have 2 rivers and 2 diseases and 2 methods of detection
# rftm ONLY calculates DERMO
# histo can be used for both DERMO and MSX
both_histo_york <- read.csv("/Users/madelineeppley/Desktop/H2F_seascape_disease/H2F_prev_york_bothdiseases.csv")
rftm_york <- read.csv("/Users/madelineeppley/Desktop/H2F_seascape_disease/H2F_dermo_york_RFTM.csv")
lewisetta_dermo <- read.csv("/Users/madelineeppley/Desktop/H2F_seascape_disease/H2F_lewisetta_dermo.csv")

# now we just need to subset to our desired dates
# july 2023, june 2024, nov 2024
# UPDATE now we want to include nov 2023 and may 2024, although my original notes do not have these dates, NM suggested to add on 9/11/25
print(both_histo_york)

# so our total dates now are: july 2023, nov 2023, may 2024, june 2024, nov 2024
# there is no june 2024 data available here
both_histo_york_subset <- both_histo_york[c(17, 20, 21, 26), ]

# matrix for bar chart and keep both columns to plot together
both_york_histo_mat <- as.matrix(both_histo_york_subset[, c("Histo_Dermo_York", "Histo_MSX_York")])
rownames(both_york_histo_mat) <- paste(both_histo_york_subset$Month, both_histo_york_subset$Year, sep="-")

bp <- barplot(t(both_york_histo_mat), 
        beside = TRUE,
        legend.text = c("Dermo", "MSX"),
        main = "Histology York River Disease: MSX and Dermo",
        xlab = "Sampling Date", 
        ylab = "Histology Prevalence (%)", 
        ylim = c(0, 70))
text(bp, t(both_york_histo_mat) + 1, labels = t(both_york_histo_mat), 
     cex = 0.8, pos = 3)

# now repeat with rftm york
print(rftm_york)

# so our total dates now are: july 2023, nov 2023, may 2024, june 2024, nov 2024
# there is no june 2024 data available here
dermo_rftm_york_subset <- rftm_york[c(17, 20, 21, 26), ]

# matrix for bar chart and keep both columns to plot together
dermo_rftm_york_mat <- as.matrix(dermo_rftm_york_subset[, ("RFTM_Dermo_York")])
rownames(dermo_rftm_york_mat) <- paste(dermo_rftm_york_subset$Month, dermo_rftm_york_subset$Year, sep="-")

bp2 <- barplot(t(dermo_rftm_york_mat),
              main = "RFTM York River Disease: ONLY Dermo",
              xlab = "Sampling Date", 
              ylab = "RFTM Prevalence (%)", 
              ylim = c(0, 120))
text(bp2, t(dermo_rftm_york_mat) + 1, labels = t(dermo_rftm_york_mat), 
     cex = 0.8, pos = 3)

### now we can see the issue - the RFTM and Histo Dermo data show really different results
dermo_combined <- data.frame(
  Month = dermo_rftm_york_subset$Month,
  Year = dermo_rftm_york_subset$Year,
  RFTM_Dermo_York = dermo_rftm_york_subset$RFTM_Dermo_York,
  Histo_Dermo_York = both_histo_york_subset$Histo_Dermo_York)

dermo_mat <- as.matrix(dermo_combined[, c("RFTM_Dermo_York", "Histo_Dermo_York")])
rownames(dermo_mat) <- paste(dermo_combined$Month, dermo_combined$Year, sep="-")

bp3 <- barplot(t(dermo_matrix),
               beside = TRUE,
               main = "RFTM vs Histo York River Disease: ONLY Dermo",
               xlab = "Sampling Date", 
               ylab = "Parasite prevalence (%)", 
               ylim = c(0, 120))
text(bp3, t(dermo_matrix) + 1, labels = t(dermo_matrix), 
     cex = 0.8, pos = 3)
legend("topleft",
       fill = c("grey40", "lightgrey"), 
       legend = c("RFTM", "Histo"))

# ok last plot is the lewisetta dermo data, we do NOT have MSX here
print(lewisetta_dermo)

# so our total dates now are: july 2023, nov 2023, may 2024, june 2024, nov 2024
# there is no may 2024 data available here, we will. use june instead
lewisetta_dermo_subset <- lewisetta_dermo[c(1, 5, 6, 11), ]

# matrix for bar chart and keep both columns to plot together
lewisetta_dermo_mat <- as.matrix(lewisetta_dermo_subset[, ("RFTM_Dermo_Lewisetta")])
rownames(lewisetta_dermo_mat) <- paste(lewisetta_dermo_subset$Month, lewisetta_dermo_subset$Year, sep="-")

bp4 <- barplot(t(lewisetta_dermo_mat),
               main = "RFTM Lewisetta River Disease: ONLY Dermo",
               xlab = "Sampling Date", 
               ylab = "RFTM Prevalence (%)", 
               ylim = c(0, 120))
text(bp4, t(lewisetta_dermo_mat) + 1, labels = t(lewisetta_dermo_mat), 
     cex = 0.8, pos = 3)

  
  
  