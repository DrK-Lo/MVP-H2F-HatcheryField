W5_NH - Processed Environmental Data
================
Madeline Eppley
7/11/2023

``` r
setwd("/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment")
```

### Load required packages.

``` r
library("dplyr") #Used for working with data frames
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("lubridate") #Used for time-date conversions
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library("readr") #Used to read the CSV file
library("ggplot2") 
```

### Note the date of data download and source. All available data should be used for each site regardless of year. Note from the CSV file how often the site was sampled, and if there are replicates in the data. Also describe if the sampling occurred at only low tide, only high tide, or continuously.

``` r
#Data was downloaded on 7/11/2023
#Source - https://cdmo.baruch.sc.edu//dges/- Selected Great Bay, Squamscott River. The station code is GRBSQWQ.

#Create text strings with metadata information that we want to include in the final data frame. 
download_date <- ("07-11-2023")
source_description <- ("NERR Centralized Data. Great Bay - Squamscott River GRBSQWQ")
site_name <- ("W5_NH") #Use site code with site number based on lat position and state
collection_type <- ("continuous")
```

### Use the file path name in your working directory or desktop, see example below. Or, import data set through the “Files” window in R studio. Store the file in a variable with the “raw_ID_Site” format. If salinity and temperature data are in separate files, read in both and store them with “\_sal” or “\_temp” in the variable names.

``` r
#The file we will be working with is from Great Bay, Mouth of the Squamscott River in New Hampshire. The ID_Site for this site is W5_NH. 
raw_W5_NH <- read_csv("/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W5-NH-raw.csv")
```

    ## Rows: 591234 Columns: 26
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (14): Station_Code, isSWMP, DateTimeStamp, F_Record, F_Temp, F_SpCond, F...
    ## dbl (12): Historical, ProvisionalPlus, Temp, SpCond, Sal, DO_pct, DO_mgl, De...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# View how the data is stored. Note the variable names and the format and units that the data are stored in.  
#spec(raw_W5_NH)
#View(raw_W5_NH)
```

### Start with the date and time of collection. We will use the lubridate package to standardize all values into the date-time format called POSIXct. This format stores the date and time in number of seconds since a past point (1/1/1970). This makes comparisons easy and helps to standardizes values.

``` r
# SKIP combining, date and time of collection is already in a column together 

# Use unclass to view the way that the time and date are stored 
# unclass(raw_W5_NH$DateTimeStamp)
# The data is stored in month-day-yearXX hours(12):minutes format

#Convert to POSIXct format. Store it into a column named datetime in the data frame.
raw_W5_NH$datetime <- as.POSIXct(raw_W5_NH$DateTimeStamp, "%m/%d/%y %H:%M", tz = "")

#Print the new data frame and examine to make sure the new datetime column is in the correct format. 
summary(raw_W5_NH)
```

    ##  Station_Code          isSWMP          DateTimeStamp        Historical    
    ##  Length:591234      Length:591234      Length:591234      Min.   :0.0000  
    ##  Class :character   Class :character   Class :character   1st Qu.:1.0000  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :1.0000  
    ##                                                           Mean   :0.8252  
    ##                                                           3rd Qu.:1.0000  
    ##                                                           Max.   :1.0000  
    ##                                                                           
    ##  ProvisionalPlus    F_Record              Temp           F_Temp         
    ##  Min.   :0.0000   Length:591234      Min.   :-0.7     Length:591234     
    ##  1st Qu.:1.0000   Class :character   1st Qu.:11.9     Class :character  
    ##  Median :1.0000   Mode  :character   Median :18.0     Mode  :character  
    ##  Mean   :0.9412                      Mean   :16.8                       
    ##  3rd Qu.:1.0000                      3rd Qu.:22.3                       
    ##  Max.   :1.0000                      Max.   :29.4                       
    ##                                      NA's   :153499                     
    ##      SpCond         F_SpCond              Sal            F_Sal          
    ##  Min.   : 0.01    Length:591234      Min.   : 0.00    Length:591234     
    ##  1st Qu.:21.57    Class :character   1st Qu.:12.90    Class :character  
    ##  Median :32.13    Mode  :character   Median :20.00    Mode  :character  
    ##  Mean   :29.73                       Mean   :18.64                      
    ##  3rd Qu.:39.91                       3rd Qu.:25.40                      
    ##  Max.   :52.33                       Max.   :34.50                      
    ##  NA's   :158356                      NA's   :158863                     
    ##      DO_pct         F_DO_pct             DO_mgl         F_DO_mgl        
    ##  Min.   : 24.50   Length:591234      Min.   : 1.90    Length:591234     
    ##  1st Qu.: 85.70   Class :character   1st Qu.: 7.10    Class :character  
    ##  Median : 93.20   Mode  :character   Median : 8.10    Mode  :character  
    ##  Mean   : 93.43                      Mean   : 8.28                      
    ##  3rd Qu.:101.10                      3rd Qu.: 9.40                      
    ##  Max.   :500.00                      Max.   :39.00                      
    ##  NA's   :163864                      NA's   :168659                     
    ##      Depth          F_Depth              cDepth         F_cDepth        
    ##  Min.   : 0.03    Length:591234      Min.   : 0.0     Length:591234     
    ##  1st Qu.: 1.35    Class :character   1st Qu.: 1.4     Class :character  
    ##  Median : 1.98    Mode  :character   Median : 2.0     Mode  :character  
    ##  Mean   : 2.05                       Mean   : 2.0                       
    ##  3rd Qu.: 2.61                       3rd Qu.: 2.6                       
    ##  Max.   :26.14                       Max.   :26.3                       
    ##  NA's   :158167                      NA's   :335102                     
    ##        pH             F_pH                Turb            F_Turb         
    ##  Min.   : 5.20    Length:591234      Min.   :  -4.00   Length:591234     
    ##  1st Qu.: 7.30    Class :character   1st Qu.:   7.00   Class :character  
    ##  Median : 7.60    Mode  :character   Median :  14.00   Mode  :character  
    ##  Mean   : 7.58                       Mean   :  28.51                     
    ##  3rd Qu.: 7.80                       3rd Qu.:  27.00                     
    ##  Max.   :11.10                       Max.   :2502.00                     
    ##  NA's   :163399                      NA's   :179436                      
    ##     ChlFluor       F_ChlFluor           datetime                     
    ##  Min.   :  0.4    Length:591234      Min.   :1997-07-21 14:30:00.00  
    ##  1st Qu.:  4.0    Class :character   1st Qu.:2008-04-19 12:41:15.00  
    ##  Median :  5.4    Mode  :character   Median :2012-09-24 17:22:30.00  
    ##  Mean   :  6.4                       Mean   :2012-05-13 11:25:56.24  
    ##  3rd Qu.:  7.3                       3rd Qu.:2017-05-16 00:18:45.00  
    ##  Max.   :398.3                       Max.   :2022-08-15 23:45:00.00  
    ##  NA's   :479896                      NA's   :46

``` r
# we have NAs, so remove those
raw_W5_NH <- raw_W5_NH %>%
    filter(!is.na(datetime))
summary(raw_W5_NH$datetime)
```

    ##                       Min.                    1st Qu. 
    ## "1997-07-21 14:30:00.0000" "2008-04-19 12:41:15.0000" 
    ##                     Median                       Mean 
    ## "2012-09-24 17:22:30.0000" "2012-05-13 11:25:56.2369" 
    ##                    3rd Qu.                       Max. 
    ## "2017-05-16 00:18:45.0000" "2022-08-15 23:45:00.0000"

### Analyze the ranges of all of our variables of interest - time, salinity, and temperature. Make sure that the latitude and longitude values are consistent for a static collection site. This is a quick check so we can determine how to conduct the next filtering step.

``` r
#Standardize column and variable names. We will use "temp" for temperature in degrees C, "salinity" for salinity in parts per thousand (ppt), "lat" for latitude in degrees, and "lon" for longitude in degrees. 
#Use the dyplr format to rename multiple columns in the format "dataframe %>% rename("new name 1" = "old name 1", "new name 2", "old name 2")
raw_W5_NH <- raw_W5_NH %>% rename("temp" = "Temp", "salinity" = "Sal") #No lat and long data in this file - check metadata files

#Print the range (minimum and maximum) of dates of data collection. 
print(summary(raw_W5_NH$datetime))
```

    ##                       Min.                    1st Qu. 
    ## "1997-07-21 14:30:00.0000" "2008-04-19 12:41:15.0000" 
    ##                     Median                       Mean 
    ## "2012-09-24 17:22:30.0000" "2012-05-13 11:25:56.2369" 
    ##                    3rd Qu.                       Max. 
    ## "2017-05-16 00:18:45.0000" "2022-08-15 23:45:00.0000"

``` r
#Print the range (minimum and maximum) of the salinity values. 
print(summary(raw_W5_NH$salinity))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00   12.90   20.00   18.64   25.40   34.50  158819

``` r
#Print the range (minimum and maximum) of the temperature values.
print(summary(raw_W5_NH$temp))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    -0.7    11.9    18.0    16.8    22.3    29.4  153455

``` r
#Store variables that we will include in the final data frame. Pull metadata from metadata file in download .zip file. 
lat <- 43.05240
lon <- -70.91181
firstyear <- 1997
finalyear <- 2022
```

### We can see that some of the values make sense - the minimum and maximum latitude and longitude values are the same.

Filter any of the variables that have data points outside of normal
range. We will use 0-40 as the accepted range for salinity (ppt) and
temperature (C) values. Note, in the summer, salinity values can
sometimes exceed 40. Check to see if there are values above 40. In this
case, adjust the range or notify someone that the site has particularly
high salinity values.

``` r
#Filter the data between the values of 0 and 40 for both salinity and temperature. 
filtered_W5_NH<- raw_W5_NH %>%
    filter(between(salinity, 0, 40) & between(temp, -1, 40))

# Sanity check - print the ranges to ensure values are filtered properly. We can see that the ranges for both are now in the appropriate range.  
print(summary(filtered_W5_NH$salinity))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00   12.90   20.00   18.64   25.40   34.50

``` r
print(summary(filtered_W5_NH$temp))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   -0.70   11.80   17.90   16.72   22.20   29.40

``` r
#Store our data into a variable name with just the site name. 
W5_NH <- filtered_W5_NH
```

### Visualize the salinity, temperature, and date ranges over time. This can help us see if there are any anomalies or gaps in the data and make sure the filtering was done correctly. Sanity check - do the temperature and salinity ranges look appropriate for the geography of the site (ex. near full ocean salinity for coastal sites, lower salinity for estuaries or near rivers)?

``` r
salplot <- ggplot(W5_NH, aes(x = datetime)) +
    geom_line(aes(y = salinity, color = "Salinity (ppt)")) +
    ylim(0,45) +
    labs(x = "Time", y = "Salinity ppt", title = "Salinity Plot for W5_NH - Great Bay - Squamscott River") +
    scale_color_manual(values = c("Salinity (ppt)" = "blue")) +
    theme_minimal()


salplot
```

![](NH1-EnvrData_files/figure-gfm/salinity-plot-1.png)<!-- -->

``` r
tempplot <- ggplot(W5_NH, aes(x = datetime)) +
    geom_line(aes(y = temp, color = "Temperature (C)")) +
    ylim(-10, 45) +
    labs(x = "Time", y = "Temperature C", title = "Temperature Plot for W5_NH - Great Bay - Squamscott River") +
    scale_color_manual(values = c( "Temperature (C)" = "red")) +
    theme_minimal()


tempplot
```

![](NH1-EnvrData_files/figure-gfm/temperature-plot-1.png)<!-- -->

### We need to calculate the mean, maximum, and minimum values for salinity and temperature per month and year. First make two data frames to contain each of the annual and monthly averages.

``` r
#Calculate the mean, maximum, and minimum values for salinity and temperature for each month. 
W5_NH_envrmonth <- W5_NH %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
    group_by(year, month) %>%
    summarise(
      min_salinity = min(salinity),
      max_salinity = max(salinity),
      mean_salinity = mean(salinity),
      length_salinity = length(salinity),
      min_temp = min(temp),
      max_temp = max(temp),
      mean_temp = mean(temp),
      length_temp = length(temp))
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
print(W5_NH_envrmonth)
```

    ## # A tibble: 221 × 10
    ## # Groups:   year [26]
    ##     year month min_salinity max_salinity mean_salinity length_salinity min_temp
    ##    <dbl> <dbl>        <dbl>        <dbl>         <dbl>           <int>    <dbl>
    ##  1  1997     7         12           24.5         19.7              499     19.1
    ##  2  1997     8         11.8         27.9         22.5             1430     19.2
    ##  3  1997     9         13.1         26.5         21.6             1312     14.3
    ##  4  1997    10         14.2         27.1         22.7             1327      6.9
    ##  5  1997    11          1.2         23.7         13.2             1307      0.8
    ##  6  1998     4          0.1         14.9          5.16             358      9.3
    ##  7  1998     5          0           21.8          6.00            1429     11.1
    ##  8  1998     6          0           22.1          4.45            1115     15.7
    ##  9  1998     7          0.1         24.4         11.7             1044     19.6
    ## 10  1998     8         13.4         29.9         25.9              873     20.2
    ## # ℹ 211 more rows
    ## # ℹ 3 more variables: max_temp <dbl>, mean_temp <dbl>, length_temp <int>

``` r
#Calculate the mean, maximum, and minimum values for salinity and temperature for each year. 
W5_NH_envryear <- W5_NH %>%
    mutate(year = year(datetime)) %>%
    group_by(year) %>%
    summarise(
      min_salinity = min(salinity),
      max_salinity = max(salinity),
      mean_salinity = mean(salinity),
      min_temp = min(temp),
      max_temp = max(temp),
      mean_temp = mean(temp))

print(W5_NH_envryear)
```

    ## # A tibble: 26 × 7
    ##     year min_salinity max_salinity mean_salinity min_temp max_temp mean_temp
    ##    <dbl>        <dbl>        <dbl>         <dbl>    <dbl>    <dbl>     <dbl>
    ##  1  1997          1.2         27.9          20.0      0.8     26.6      15.6
    ##  2  1998          0           29.9          16.0      4.8     27        17.1
    ##  3  1999          1.7         30.8          20.8      2.7     28.4      16.5
    ##  4  2000          0           28.5          19.3     10.5     25.6      20.2
    ##  5  2001          1.1         33.9          24.2      4.1     28.3      17.7
    ##  6  2002          0.2         33.8          22.7     -0.2     29        17.0
    ##  7  2003          0.2         29.8          18.2      2.6     27.3      16.8
    ##  8  2004          0.1         29            16.8     -0.7     26.5      15.8
    ##  9  2005          0           30.5          14.3     -0.2     27.3      15.8
    ## 10  2006          0           28.9          13.1      4.1     28.1      16.1
    ## # ℹ 16 more rows

``` r
#Calculate the mean, maximum, and minimum values for salinity and temperature for each day. 
W5_NH_envrday <- W5_NH %>%
    mutate(year = year(datetime), month = month(datetime), day = day(datetime)) %>%
    group_by(year, month, day) %>%
    summarise(
      min_salinity = min(salinity),
      max_salinity = max(salinity),
      mean_salinity = mean(salinity),
      length_salinity = length(salinity),
      min_temp = min(temp),
      max_temp = max(temp),
      mean_temp = mean(temp),
      length_temp = length(temp))
```

    ## `summarise()` has grouped output by 'year', 'month'. You can override using the
    ## `.groups` argument.

``` r
print(W5_NH_envrday)
```

    ## # A tibble: 5,610 × 11
    ## # Groups:   year, month [221]
    ##     year month   day min_salinity max_salinity mean_salinity length_salinity
    ##    <dbl> <dbl> <int>        <dbl>        <dbl>         <dbl>           <int>
    ##  1  1997     7    21         12.2         23            18.0              19
    ##  2  1997     7    22         12           23.2          18.9              48
    ##  3  1997     7    23         12           23.3          19.1              48
    ##  4  1997     7    24         12.2         23.5          19.3              48
    ##  5  1997     7    25         13           23.7          19.7              48
    ##  6  1997     7    26         13.4         24            19.8              48
    ##  7  1997     7    27         13.9         24.2          20.0              48
    ##  8  1997     7    28         14.4         24.3          20.1              48
    ##  9  1997     7    29         14.5         24.4          20.1              48
    ## 10  1997     7    30         14.5         24.5          20.2              48
    ## # ℹ 5,600 more rows
    ## # ℹ 4 more variables: min_temp <dbl>, max_temp <dbl>, mean_temp <dbl>,
    ## #   length_temp <int>

### Plot the months and years of data collection to check if there are any collection gaps in the data.

``` r
timeplot <- ggplot(W5_NH_envrmonth, aes(x = year)) +
    geom_point(aes(y = month, color = length_salinity), size = 4) +
    labs(x = "Time", y = "Month", title = "Timeplot for W5_NH - Great Bay - Squamscott River") +
    ylim(1,12) +
    theme_minimal()

timeplot
```

![](NH1-EnvrData_files/figure-gfm/timeplot-1.png)<!-- -->

# Calculate days above and below thresholds and plot

``` r
# open up a blank data frame that spans all 12 months for all years of data collection for this site
# we will merge this with the observations so that we can plot over time 

complete_year_month <- expand.grid(
  year = unique(firstyear:finalyear),
  month = 1:12
)
```

# start with low salinity stress

``` r
# first calculate for low salinity
W5_NH$low_sal_stress <- W5_NH$salinity < 12

low_sal_stress_count <- W5_NH %>%
  mutate(year = year(datetime), 
         month = month(datetime), 
         day = day(datetime)) %>% 
  filter(low_sal_stress == 1) %>%  
  distinct(year, month, day) %>%  # remove dups
  group_by(year, month) %>%
  summarise(low_sal_stress = n(), .groups = "drop") # group all occurrences together by month rather than by numeric day

print(low_sal_stress_count)
```

    ## # A tibble: 184 × 3
    ##     year month low_sal_stress
    ##    <dbl> <dbl>          <int>
    ##  1  1997     8              2
    ##  2  1997    11             27
    ##  3  1998     4              8
    ##  4  1998     5             31
    ##  5  1998     6             22
    ##  6  1998     7             24
    ##  7  1998    10             17
    ##  8  1998    11              5
    ##  9  1999     4             12
    ## 10  1999     5             25
    ## # ℹ 174 more rows

``` r
# this plots low sal stress days only, years with no observations of low salinity stress will not appear in the graph 
ggplot(low_sal_stress_count, aes(x = factor(month), y = low_sal_stress, fill = factor(month))) +
  geom_bar(stat = "identity", fill = "#1f77b4") +
  facet_wrap(~ year) +
  labs(title = "W5_NH: count of low salinity (< 12 ppt) days per month",
       x = "month",
       y = "count of days") +
  theme_minimal()
```

![](NH1-EnvrData_files/figure-gfm/low-sal-stress-1.png)<!-- -->

``` r
# Merge with the original dataset, filling missing values with 0 for low_sal_stress
low_sal_complete_data <- complete_year_month %>%
  left_join(low_sal_stress_count, by = c("year", "month")) %>%
  mutate(low_sal_stress = ifelse(is.na(low_sal_stress), 0, low_sal_stress))

low_sal_complete_data$month <- as.numeric(low_sal_complete_data$month)

# bars
ggplot(low_sal_complete_data, aes(x = factor(month), y = low_sal_stress)) +
  geom_bar(stat = "identity", fill = "#1f77b4") + 
  facet_wrap(~ year) +
  labs(title = "W5_NH: count of low salinity (< 12 ppt) days per month",
       x = "month",
       y = "count of low salinity stress days") +
  theme_minimal()
```

![](NH1-EnvrData_files/figure-gfm/low-sal-stress-2.png)<!-- -->

``` r
# points
ggplot(low_sal_complete_data, aes(x = factor(month), y = low_sal_stress)) +
  geom_point(color = "#1f77b4", size = 2) + 
  facet_wrap(~ year) +  
  labs(title = "W5_NH: count of low salinity (< 12 ppt) days per month",
       x = "month",
       y = "count of low salinity stress days") +
  theme_minimal()
```

![](NH1-EnvrData_files/figure-gfm/low-sal-stress-3.png)<!-- -->

# now calculate for high salinity stress

``` r
W5_NH$high_sal_stress <- W5_NH$salinity > 35

high_sal_stress_count <- W5_NH %>%
  mutate(year = year(datetime), 
         month = month(datetime), 
         day = day(datetime)) %>% 
  filter(high_sal_stress == 1) %>%  
  distinct(year, month, day) %>%  # remove dups
  group_by(year, month) %>%
  summarise(high_sal_stress = n(), .groups = "drop") # group all occurrences together by month rather than by numeric day

print(high_sal_stress_count) # no high salinity stress 
```

    ## # A tibble: 0 × 3
    ## # ℹ 3 variables: year <dbl>, month <dbl>, high_sal_stress <int>

# high temp stress calculations

``` r
W5_NH$high_temp_stress <- W5_NH$temp > 28

high_temp_stress_count <- W5_NH %>%
  mutate(year = year(datetime), 
         month = month(datetime), 
         day = day(datetime)) %>% 
  filter(high_temp_stress == 1) %>%  
  distinct(year, month, day) %>%  # remove dups
  group_by(year, month) %>%
  summarise(high_temp_stress = n(), .groups = "drop") # group all occurrences together by month rather than by numeric day

print(high_temp_stress_count)
```

    ## # A tibble: 18 × 3
    ##     year month high_temp_stress
    ##    <dbl> <dbl>            <int>
    ##  1  1999     7                2
    ##  2  2001     8                2
    ##  3  2002     7                2
    ##  4  2002     8                4
    ##  5  2006     7                1
    ##  6  2007     8                3
    ##  7  2010     7                3
    ##  8  2013     7                4
    ##  9  2016     7                2
    ## 10  2016     8                1
    ## 11  2018     7                3
    ## 12  2019     7                2
    ## 13  2020     7                6
    ## 14  2020     8                2
    ## 15  2021     6                2
    ## 16  2021     7                1
    ## 17  2022     7                4
    ## 18  2022     8                6

``` r
ggplot(high_temp_stress_count, aes(x = factor(month), y = high_temp_stress, fill = factor(month))) +
  geom_bar(stat = "identity", fill = "#DD4124FF") +
  facet_wrap(~ year) +
  labs(title = "W5_NH: count of high temperature (> 28 C) days per month",
       x = "month",
       y = "count of days") +
  theme_minimal()
```

![](NH1-EnvrData_files/figure-gfm/high-temp-stress-1.png)<!-- -->

``` r
high_temp_complete_data <- complete_year_month %>%
  left_join(high_temp_stress_count, by = c("year", "month")) %>%
  mutate(high_temp_stress = ifelse(is.na(high_temp_stress), 0, high_temp_stress))

high_temp_complete_data$month <- as.numeric(high_temp_complete_data$month)

# bars
ggplot(high_temp_complete_data, aes(x = factor(month), y = high_temp_stress)) +
  geom_bar(stat = "identity", fill = "#DD4124FF") + 
  facet_wrap(~ year) +
  labs(title = "W5_NH: count of high temperature (> 28 C) days per month",
       x = "month",
       y = "count of days") +
  theme_minimal()
```

![](NH1-EnvrData_files/figure-gfm/high-temp-stress-2.png)<!-- -->

``` r
# points
ggplot(high_temp_complete_data, aes(x = factor(month), y = high_temp_stress)) +
  geom_point(color = "#DD4124FF", size = 2) + 
  facet_wrap(~ year) +  
  labs(title = "W5_NH: count of high temperature (> 28 C) days per month",
       x = "month",
       y = "count of days") +
  theme_minimal()
```

![](NH1-EnvrData_files/figure-gfm/high-temp-stress-3.png)<!-- -->

### We can now calculate a list of variables that we will have collected for all sites. This will allow us to compare sites easily. We will calculate the number of observations from each site, the mean annual, maximum annual, and minimum annual value for all variables.

Our list of variables includes:

- Mean_Annual_Temperature_C: average of all available data

- Mean_max_temperature_C: average of maximums for each year

- Mean_min_temperature_C: average of minimums for each year

- Temperature_st_dev: standard deviation of all available data

- Temperature_n: total number of data points

- Temperature_years: number of years in data set

- Mean_Annual_Salinity_ppt: average of all available data

- Mean_min_Salinity_ppt: average of minimums for each year

- Mean_max_Salinity_ppt: average of maximums for each year

- Salinity_st_dev: standard deviation of all available data

- Salinity_n: total number of data points

- Salinity_years: number of years in data set

``` r
#Calculate temperature variables. 
Mean_Annual_Temperature_C <- mean(W5_NH$temp)
Mean_max_temperature_C <- mean(W5_NH_envryear$max_temp)
Mean_min_temperature_C <- mean(W5_NH_envryear$min_temp)
Temperature_st_dev <- sd(W5_NH$temp)
Temperature_n <- nrow(W5_NH)
Temperature_years <- nrow(W5_NH_envryear)
high_temp_stress_days <- sum(high_temp_stress_count$high_temp_stress)
frac_high_temp_stress_days <- high_temp_stress_days/nrow(W5_NH_envrday)

Mean_Monthly_Temperature_C <- W5_NH_envrmonth %>%
  filter(!is.na(month)) %>% 
  group_by(month) %>%
  summarise(Mean_Temperature = mean(mean_temp))

Mean_min_Monthly_Temperature_C <- W5_NH_envrmonth %>%
  filter(!is.na(month)) %>% 
  group_by(month) %>%
  summarise(Mean_min_Temperature = mean(min_temp))

Mean_max_Monthly_Temperature_C <- W5_NH_envrmonth %>%
  filter(!is.na(month)) %>% 
  group_by(month) %>%
  summarise(Mean_max_Temperature = mean(max_temp))

#Create a data frame to store the temperature results
W5_NH_temp <- cbind(site_name, download_date, source_description, lat, lon, firstyear, finalyear, Mean_Annual_Temperature_C, Mean_max_temperature_C, Mean_min_temperature_C, Temperature_st_dev, high_temp_stress_days, frac_high_temp_stress_days, Temperature_n, Temperature_years, collection_type)
print(W5_NH_temp)
```

    ##      site_name download_date
    ## [1,] "W5_NH"   "07-11-2023" 
    ##      source_description                                            lat      
    ## [1,] "NERR Centralized Data. Great Bay - Squamscott River GRBSQWQ" "43.0524"
    ##      lon         firstyear finalyear Mean_Annual_Temperature_C
    ## [1,] "-70.91181" "1997"    "2022"    "16.7171312400548"       
    ##      Mean_max_temperature_C Mean_min_temperature_C Temperature_st_dev
    ## [1,] "27.8807692307692"     "2.31923076923077"     "6.44961550527541"
    ##      high_temp_stress_days frac_high_temp_stress_days Temperature_n
    ## [1,] "50"                  "0.0089126559714795"       "432368"     
    ##      Temperature_years collection_type
    ## [1,] "26"              "continuous"

``` r
W5_NH_monthly_temp <- cbind(Mean_Monthly_Temperature_C, Mean_min_Monthly_Temperature_C, Mean_max_Monthly_Temperature_C)
W5_NH_monthly_temp <- W5_NH_monthly_temp[, !duplicated(names(W5_NH_monthly_temp))]
print(W5_NH_monthly_temp)
```

    ##    month Mean_Temperature Mean_min_Temperature Mean_max_Temperature
    ## 1      3         8.914942             7.066667             11.26667
    ## 2      4        10.715700             7.568182             14.94091
    ## 3      5        15.221635            10.532000             21.32800
    ## 4      6        20.218822            15.228000             25.39200
    ## 5      7        23.507120            20.084615             27.55000
    ## 6      8        23.361366            20.257692             26.95769
    ## 7      9        19.777527            15.816000             24.18800
    ## 8     10        13.358193             8.379167             18.34167
    ## 9     11         6.668474             2.375000             11.47500
    ## 10    12         3.955681             1.866667              5.77619

``` r
# Write to the combined file with all sites 
write.table(W5_NH_temp, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/all_temperature.csv", sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE) # The column names should be changed to FALSE after 1st row is added to the data frame

# Write to a unique new CSV file
write.csv(W5_NH_temp, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W5_NH_temperature.csv")

# Write all montly data to a unique new CSV file
write.csv(W5_NH_monthly_temp, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W5_NH_monthly_temp.csv")
```

``` r
#Calculate the salinity variables
Mean_Annual_Salinity_ppt <- mean(W5_NH$salinity)
Mean_max_Salinity_ppt <- mean(W5_NH_envryear$max_salinity)
Mean_min_Salinity_ppt <- mean(W5_NH_envryear$min_salinity)
Salinity_st_dev <- sd(W5_NH$salinity)
Salinity_n <- nrow(W5_NH)
Salinity_years <- nrow(W5_NH_envryear)
high_sal_stress_days <- sum(high_sal_stress_count$high_sal_stress)
low_sal_stress_days <- sum(low_sal_stress_count$low_sal_stress)
frac_high_sal_stress_days <- high_sal_stress_days/nrow(W5_NH_envrday)
frac_low_sal_stress_days <- low_sal_stress_days/nrow(W5_NH_envrday)

Mean_Monthly_Salinity <- W5_NH_envrmonth %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(Mean_Salinity = mean(mean_salinity))

Min_Monthly_Salinity <- W5_NH_envrmonth %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(Min_Salinity = mean(min_salinity))

Max_Monthly_Salinity <- W5_NH_envrmonth %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(Max_Salinity = mean(max_salinity))

#Create a data frame to store the temperature results
W5_NH_salinity <- cbind(site_name, download_date, source_description, lat, lon, firstyear, finalyear, Mean_Annual_Salinity_ppt, Mean_max_Salinity_ppt, Mean_min_Salinity_ppt, high_sal_stress_days,low_sal_stress_days, frac_high_sal_stress_days, frac_low_sal_stress_days, Salinity_st_dev, Salinity_n, Salinity_years, collection_type)
print(W5_NH_salinity)
```

    ##      site_name download_date
    ## [1,] "W5_NH"   "07-11-2023" 
    ##      source_description                                            lat      
    ## [1,] "NERR Centralized Data. Great Bay - Squamscott River GRBSQWQ" "43.0524"
    ##      lon         firstyear finalyear Mean_Annual_Salinity_ppt
    ## [1,] "-70.91181" "1997"    "2022"    "18.6413680013322"      
    ##      Mean_max_Salinity_ppt Mean_min_Salinity_ppt high_sal_stress_days
    ## [1,] "30.2923076923077"    "0.284615384615385"   "0"                 
    ##      low_sal_stress_days frac_high_sal_stress_days frac_low_sal_stress_days
    ## [1,] "3383"              "0"                       "0.603030303030303"     
    ##      Salinity_st_dev    Salinity_n Salinity_years collection_type
    ## [1,] "8.32093836689361" "432368"   "26"           "continuous"

``` r
W5_NH_monthly_sal <- cbind(Mean_Monthly_Salinity, Min_Monthly_Salinity, Max_Monthly_Salinity)
W5_NH_monthly_sal <- W5_NH_monthly_sal[, !duplicated(names(W5_NH_monthly_sal))]
print(W5_NH_monthly_sal)
```

    ##    month Mean_Salinity Min_Salinity Max_Salinity
    ## 1      3     14.520492    2.6666667     21.60000
    ## 2      4      9.965738    0.4727273     18.67727
    ## 3      5     12.634558    0.6160000     23.14400
    ## 4      6     16.496917    2.9600000     25.28800
    ## 5      7     20.853885    7.6576923     27.81538
    ## 6      8     23.293945   10.8269231     28.96538
    ## 7      9     23.754638   11.9040000     29.15600
    ## 8     10     21.593318    5.8958333     28.73750
    ## 9     11     16.043350    3.3708333     24.57500
    ## 10    12     14.411785    3.7476190     22.04286

``` r
# Write to the combined file with all sites 
write.table(W5_NH_salinity, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/all_salinity.csv", sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE) # The column names should be changed to FALSE after 1st row is added to the data frame

# Write all year data to a unique new CSV file
write.csv(W5_NH_salinity, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W5_NH_salinity.csv")
# Write all montly data to a unique new CSV file
write.csv(W5_NH_monthly_sal, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W5_NH_monthly_sal.csv")
```
