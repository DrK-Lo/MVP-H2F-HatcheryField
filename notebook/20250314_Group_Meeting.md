# 14 March Group Meeting Notes
- Attendees: NM, KB, ME, CR, KEL

### Disease analysis
- Disease analysis again - what info do we need?
    - Doing a 1/0 for disease presence and absence is not very relevant since all sites have Dermo and almost all have MSX
      - Show instead as on a scale from 0-5 - 0 is absent, 1 is rarely reported, 5 is occurs every year in high prevalence
      -  Could also use qPCR results to gauge disease data

### Environment of origin figure
- Put 3 PCAs under the violin/introgression plots to make a five panel plot
  - PCAs are: parent genetics, all environmental data (field and envr of origin), and disease
  - ME advocates for including time series environmental plots in supplement
 
### Research question analysis/figures
- Treat hatchery as one analysis and field data as another - cannot run full interaction model looking at length ~ time and group for both hatchery and field sites together  
- **For Q1:**
  - Boxplot for end of nursery length, boxplot for end of hatchery length, time series of field survival in Lew, time series of field survival in York, two way boxplot of survival at final field time points for both field sites, time series of field length in Lewisetta, time series of field length in York, two way box plot of length at final field time point for both field sites. 
    - We may exclude the field length time series plots if things are looking too busy
    - We also want to include Zea's condition results in here somehow
  - Interaction plot for nursery/hatchery lengths in supplement
  - See image of drawn out plots for Q1 figures
  - KEL recommends making each plot individually in R, exporting as PDFs, then arranging them in a PPT and exporting as PDF
 - **For Q2:**
   - Camille has a LMM for each field time point length
   - NM - touch base with Camille to figure this one out
   - Ho and temperature are collinear with high VIFs (VIF > 5)
     - may just have to say that we cannot tease apart Ho from temperature
   - For salinity- is mean, min, or max salinity the biggest selective force?
- **For Q3:**
  - Camille will average length/survival across the bags for each group at each field site.
    - I.e. for DEBY bags 1-3 in the York, will average length and survival across all three bags to come up with data to use in partial Mantel tests 


## By next meeting
   - Madeline making new PCAs for environment of origin figure, including high/low salinity/temp stress day (also RDA if there is time, but not a priority)
     - Madeline will make correlation heat map looking at the environmental parameters (min/max/mean temp and salinity) in relation to all other environmental parameters
   - Camille and Nicole (and Zea?) will talk about Q2 models, and Q1 figures (also consider incorporating Condition data from Zea)
   - Kiran - update the Q4 figures
   - Camille - add Tukey significance letters to final field boxplots
   - Kiran can update partial Mantels with new environmental variables
