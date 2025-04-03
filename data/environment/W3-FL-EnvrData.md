W3_FL - Processed Environmental Data
================
Madeline Eppley
9/13/2023

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
#Data was downloaded on 9/13/2023
#Source - https://irma.nps.gov/AQWebPortal/Data/Location/Summary/Location/TIMUking01/Interval/Latest
#The site was sampled every 30 minutes continuously.

#Create text strings with metadata information that we want to include in the final data frame. 
download_date <- ("09-13-2023")
source_description <- ("National Parks Service Continuous Water Data - Timucuan Preserve")
site_name <- ("W3_FL") #Use site code with site number based on lat position and state
collection_type <- ("continuous")
```

### Use the file path name in your working directory or desktop, see example below. Or, import data set through the “Files” window in R studio. Store the file in a variable with the “raw_ID_Site” format. If salinity and temperature data are in separate files, read in both and store them with “\_sal” or “\_temp” in the variable names.

``` r
#The file we will be working with is from Lower Cedar Point, Maryland. The ID_Site for this site is W3_FL. 
raw_W3_FL_sal <- read_csv("/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W3-FL-raw_sal.csv")
```

    ## Rows: 249752 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Timestamp
    ## dbl (1): PSU
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
raw_W3_FL_temp <- read_csv("/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W3-FL-raw_temp.csv")
```

    ## Rows: 258086 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Timestamp
    ## dbl (1): Temp
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# View how the data is stored. Note the variable names and the format and units that the data are stored in.  
#spec(raw_W3_FL_sal)
#View(raw_W3_FL_sal)

#spec(raw_W3_FL_temp)
#View(raw_W3_FL_temp)
```

### Start with the date and time of collection. We will use the lubridate package to standardize all values into the date-time format called POSIXct. This format stores the date and time in number of seconds since a past point (1/1/1970). This makes comparisons easy and helps to standardizes values.

``` r
#Convert to POSIXct format. Store it into a column named datetime in the data frame.
raw_W3_FL_sal$datetime <- as.POSIXct(raw_W3_FL_sal$Timestamp, "%m/%d/%y %H:%M", tz = "")
raw_W3_FL_temp$datetime <- as.POSIXct(raw_W3_FL_temp$Timestamp, "%m/%d/%y %H:%M", tz = "")

#Print the new data frame and examine to make sure the new datetime column is in the correct format. 
head(raw_W3_FL_sal)
```

    ## # A tibble: 6 × 3
    ##   Timestamp       PSU datetime           
    ##   <chr>         <dbl> <dttm>             
    ## 1 6/22/05 12:30  32.1 2005-06-22 12:30:00
    ## 2 6/22/05 13:00  31.1 2005-06-22 13:00:00
    ## 3 6/22/05 13:30  30.2 2005-06-22 13:30:00
    ## 4 6/22/05 14:00  29.4 2005-06-22 14:00:00
    ## 5 6/22/05 14:30  29.2 2005-06-22 14:30:00
    ## 6 6/22/05 15:00  29.4 2005-06-22 15:00:00

``` r
head(raw_W3_FL_temp)
```

    ## # A tibble: 6 × 3
    ##   Timestamp      Temp datetime           
    ##   <chr>         <dbl> <dttm>             
    ## 1 6/22/05 12:30  28.2 2005-06-22 12:30:00
    ## 2 6/22/05 13:00  28.3 2005-06-22 13:00:00
    ## 3 6/22/05 13:30  28.5 2005-06-22 13:30:00
    ## 4 6/22/05 14:00  28.5 2005-06-22 14:00:00
    ## 5 6/22/05 14:30  28.7 2005-06-22 14:30:00
    ## 6 6/22/05 15:00  29.0 2005-06-22 15:00:00

### Analyze the ranges of all of our variables of interest - time, salinity, and temperature. Make sure that the latitude and longitude values are consistent for a static collection site. This is a quick check so we can determine how to conduct the next filtering step.

``` r
#Standardize column and variable names. We will use "temp" for temperature in degrees C, "salinity" for salinity in parts per thousand (ppt), "lat" for latitude in degrees, and "lon" for longitude in degrees. 
#Use the dyplr format to rename multiple columns in the format "dataframe %>% rename("new name 1" = "old name 1", "new name 2", "old name 2")
raw_W3_FL_sal <- raw_W3_FL_sal %>% rename("salinity" = "PSU")
raw_W3_FL_temp <- raw_W3_FL_temp  %>% rename("temp" = "Temp")

#Print the range (minimum and maximum) of dates of data collection. 
#print(range(raw_W3_FL_sal$datetime))
#print(range(raw_W3_FL_temp$datetime))

#Print the range (minimum and maximum) of the salinity values. 
print(range(raw_W3_FL_sal$salinity))
```

    ## [1]  6.37187 41.22546

``` r
#Print the range (minimum and maximum) of the temperature values.
print(range(raw_W3_FL_temp$temp))
```

    ## [1]  5.830 32.754

``` r
#Store variables that we will include in the final data frame
lat <- 30.44116
lon <- -81.43908
firstyear <- 2005
finalyear <- 2022
```

### Filter any of the variables that have data points outside of normal range. We will use 0-40 as the accepted range for salinity (ppt) and temperature (C) values. Note, in the summer, salinity values can sometimes exceed 40. Check to see if there are values above 40. In this case, adjust the range or notify someone that the site has particularly high salinity values.

``` r
#Filter the data between the values of 0 and 40 for both salinity and temperature. 
filtered_W3_FL_sal <- raw_W3_FL_sal %>%
    filter(between(salinity, 0, 42)) 
           
filtered_W3_FL_temp <- raw_W3_FL_temp %>%
    filter(between(temp, 0, 40))

# Sanity check - print the ranges to ensure values are filtered properly. We can see that the ranges for both are now in the appropriate range.  
print(summary(filtered_W3_FL_sal$salinity))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   6.372  30.879  33.197  32.352  34.901  41.225

``` r
print(summary(filtered_W3_FL_temp$temp))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    5.83   17.25   22.44   22.16   27.52   32.75

``` r
#Store our data into a variable name with just the site name. 
W3_FL_temp <- filtered_W3_FL_temp
W3_FL_sal <- filtered_W3_FL_sal
```

### Visualize the salinity, temperature, and date ranges over time. This can help us see if there are any anomalies or gaps in the data and make sure the filtering was done correctly. Sanity check - do the temperature and salinity ranges look appropriate for the geography of the site (ex. near full ocean salinity for coastal sites, lower salinity for estuaries or near rivers)?

``` r
salplot <- ggplot(W3_FL_sal, aes(x = datetime)) +
    geom_line(aes(y = salinity, color = "Salinity (ppt)")) +
    ylim(0,40) +
    labs(x = "Time", y = "Salinity ppt", title = "Salinity Plot for W3_FL") +
    scale_color_manual(values = c("Salinity (ppt)" = "blue")) +
    theme_minimal()

salplot
```

    ## Warning: Removed 34 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](W3-FL-EnvrData_files/figure-gfm/salinity-plot-1.png)<!-- -->

``` r
tempplot <- ggplot(W3_FL_temp, aes(x = datetime)) +
    geom_line(aes(y = temp, color = "Temperature (C)")) +
    ylim(0, 45) +
    labs(x = "Time", y = "Temperature C", title = "Temperature Plot for W3_FL") +
    scale_color_manual(values = c( "Temperature (C)" = "red")) +
    theme_minimal()


tempplot
```

    ## Warning: Removed 36 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](W3-FL-EnvrData_files/figure-gfm/temperature-plot-1.png)<!-- -->

### We need to calculate the mean, maximum, and minimum values for salinity and temperature per month and year. First make two data frames to contain each of the annual and monthly averages.

``` r
#Calculate the mean, maximum, and minimum values for salinity and temperature for each month. 
W3_FL_envrmonth_sal <- W3_FL_sal %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
    group_by(year, month) %>%
    summarise(
      min_salinity = min(salinity),
      max_salinity = max(salinity),
      mean_salinity = mean(salinity),
      length_salinity = length(salinity))
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
W3_FL_envrmonth_temp <- W3_FL_temp %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
    group_by(year, month) %>%
    summarise(      
      min_temp = min(temp),
      max_temp = max(temp),
      mean_temp = mean(temp),
      length_temp = length(temp))
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
print(W3_FL_envrmonth_sal)
```

    ## # A tibble: 187 × 6
    ## # Groups:   year [20]
    ##     year month min_salinity max_salinity mean_salinity length_salinity
    ##    <dbl> <dbl>        <dbl>        <dbl>         <dbl>           <int>
    ##  1  2005     6        19.3          34.7          31.4             407
    ##  2  2005     7        12.0          36.3          25.0            1482
    ##  3  2005     8        16.8          36.7          29.6            1487
    ##  4  2005     9        18.6          35.5          29.9            1440
    ##  5  2005    10        12.0          33.9          27.0            1488
    ##  6  2005    11         9.77         35.2          26.2             815
    ##  7  2005    12         6.37         35.7          26.5            1488
    ##  8  2006     1        12.5          33.9          28.8            1488
    ##  9  2006     2        15.7          33.4          27.7            1344
    ## 10  2006     3        20.5          34.3          31.2            1488
    ## # ℹ 177 more rows

``` r
print(W3_FL_envrmonth_temp)
```

    ## # A tibble: 191 × 6
    ## # Groups:   year [20]
    ##     year month min_temp max_temp mean_temp length_temp
    ##    <dbl> <dbl>    <dbl>    <dbl>     <dbl>       <int>
    ##  1  2005     6     27.2     29.8      28.3         407
    ##  2  2005     7     27.2     32.1      29.5        1482
    ##  3  2005     8     28.6     32.4      30.2        1487
    ##  4  2005     9     25.8     31.3      28.1        1440
    ##  5  2005    10     18.1     29.5      25.1        1488
    ##  6  2005    11     15.2     23.4      19.0         815
    ##  7  2005    12     11.4     18.7      15.0        1488
    ##  8  2006     1     11.8     18.2      15.3        1488
    ##  9  2006     2     10.9     17.9      14.8        1344
    ## 10  2006     3     14.2     21.2      17.5        1488
    ## # ℹ 181 more rows

``` r
#Calculate the mean, maximum, and minimum values for salinity and temperature for each year. 
W3_FL_envryear_sal <- W3_FL_sal %>%
    mutate(year = year(datetime)) %>%
    group_by(year) %>%
    summarise(
      min_salinity = min(salinity),
      max_salinity = max(salinity),
      mean_salinity = mean(salinity))

W3_FL_envryear_temp <- W3_FL_temp %>%
    mutate(year = year(datetime)) %>%
    group_by(year) %>%
    summarise(
      min_temp = min(temp),
      max_temp = max(temp),
      mean_temp = mean(temp))

print(W3_FL_envryear_sal)
```

    ## # A tibble: 20 × 4
    ##     year min_salinity max_salinity mean_salinity
    ##    <dbl>        <dbl>        <dbl>         <dbl>
    ##  1  2005         6.37         36.7          27.6
    ##  2  2006        12.5          37.3          33.4
    ##  3  2007        17.5          37.2          33.3
    ##  4  2008        12.4          38.1          31.8
    ##  5  2009        14.0          36.9          31.8
    ##  6  2010        16.4          37.0          32.9
    ##  7  2011        25.6          38.2          34.6
    ##  8  2012        12.1          37.9          34.0
    ##  9  2013        17.9          36.8          32.5
    ## 10  2014        15.5          41.2          33.1
    ## 11  2015        20.6          38.1          32.5
    ## 12  2016        18.7          37.6          32.9
    ## 13  2017         7.66         37.9          32.3
    ## 14  2018        18.8          35.3          30.7
    ## 15  2019        18.7          37.0          33.0
    ## 16  2020        13.5          35.2          29.9
    ## 17  2021        11.8          38.3          30.6
    ## 18  2022         9.07         36.8          31.6
    ## 19  2023        21.3          35.2          31.6
    ## 20    NA        18.0          35.5          30.6

``` r
print(W3_FL_envryear_temp)
```

    ## # A tibble: 20 × 4
    ##     year min_temp max_temp mean_temp
    ##    <dbl>    <dbl>    <dbl>     <dbl>
    ##  1  2005    11.4      32.4      25.1
    ##  2  2006    10.9      31.9      21.9
    ##  3  2007    11.1      32.3      22.6
    ##  4  2008    10.5      30.6      21.7
    ##  5  2009     9.56     31.8      22.2
    ##  6  2010     5.83     32.1      20.7
    ##  7  2011     7.6      29.9      19.3
    ##  8  2012    12.5      31.4      23.0
    ##  9  2013    11.8      31.2      22.5
    ## 10  2014     9.07     32.0      21.6
    ## 11  2015     9.35     31.6      21.8
    ## 12  2016    10.1      32.3      22.9
    ## 13  2017    12.1      31.6      22.7
    ## 14  2018     6.53     32.0      19.3
    ## 15  2019    13.7      32.8      23.9
    ## 16  2020    11.1      27.9      19.5
    ## 17  2021    11.0      32.1      22.5
    ## 18  2022     9.02     32.3      22.9
    ## 19  2023    11.5      24.2      17.8
    ## 20    NA    13.1      20.5      17.0

``` r
#Calculate the mean, maximum, and minimum values for salinity and temperature for each day. 
W3_FL_envrday_sal <- W3_FL_sal %>%
    mutate(year = year(datetime), month = month(datetime), day = day(datetime)) %>%
    group_by(year, month, day) %>%
    summarise(
      min_salinity = min(salinity),
      max_salinity = max(salinity),
      mean_salinity = mean(salinity),
      length_salinity = length(salinity))
```

    ## `summarise()` has grouped output by 'year', 'month'. You can override using the
    ## `.groups` argument.

``` r
print(W3_FL_envrday_sal)
```

    ## # A tibble: 5,096 × 7
    ## # Groups:   year, month [187]
    ##     year month   day min_salinity max_salinity mean_salinity length_salinity
    ##    <dbl> <dbl> <int>        <dbl>        <dbl>         <dbl>           <int>
    ##  1  2005     6    22         29.2         34.6          33.0              23
    ##  2  2005     6    23         30.4         34.7          33.4              48
    ##  3  2005     6    24         32.1         34.4          33.4              48
    ##  4  2005     6    25         32.6         34.6          33.6              48
    ##  5  2005     6    26         32.9         34.6          33.6              48
    ##  6  2005     6    27         30.9         34.2          32.7              48
    ##  7  2005     6    28         27.2         34.1          30.6              48
    ##  8  2005     6    29         23.3         33.1          28.6              48
    ##  9  2005     6    30         19.3         34.2          24.7              48
    ## 10  2005     7     1         16.1         34.9          22.8              48
    ## # ℹ 5,086 more rows

``` r
W3_FL_envrday_temp <- W3_FL_temp %>%
    mutate(year = year(datetime), month = month(datetime), day = day(datetime)) %>%
    group_by(year, month, day) %>%
    summarise(
      min_temp = min(temp),
      max_temp = max(temp),
      mean_temp = mean(temp),
      length_temp = length(temp))
```

    ## `summarise()` has grouped output by 'year', 'month'. You can override using the
    ## `.groups` argument.

``` r
print(W3_FL_envrday_temp)
```

    ## # A tibble: 5,253 × 7
    ## # Groups:   year, month [191]
    ##     year month   day min_temp max_temp mean_temp length_temp
    ##    <dbl> <dbl> <int>    <dbl>    <dbl>     <dbl>       <int>
    ##  1  2005     6    22     28.2     29.3      28.7          23
    ##  2  2005     6    23     27.6     29.8      28.6          48
    ##  3  2005     6    24     27.6     29.3      28.5          48
    ##  4  2005     6    25     27.2     28.3      27.8          48
    ##  5  2005     6    26     27.5     29.8      28.4          48
    ##  6  2005     6    27     28.8     29.4      29.1          48
    ##  7  2005     6    28     27.8     28.8      28.4          48
    ##  8  2005     6    29     27.5     28.2      27.9          48
    ##  9  2005     6    30     27.3     28.5      27.8          48
    ## 10  2005     7     1     27.6     29.2      28.3          48
    ## # ℹ 5,243 more rows

### Plot the months and years of data collection to check if there are any collection gaps in the data.

``` r
timeplot <- ggplot(W3_FL_envrmonth_sal, aes(x = year)) +
    geom_point(aes(y = month, color = length_salinity), size = 4) +
    labs(x = "Time", y = "Month", title = "Salinity Timeplot for W3_FL") +
    ylim(1,12) +
    theme_minimal()

timeplot
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](W3-FL-EnvrData_files/figure-gfm/timeplot%20-%20salinity-1.png)<!-- -->

### Plot the months and years of data collection to check if there are any collection gaps in the data.

``` r
timeplot <- ggplot(W3_FL_envrmonth_temp, aes(x = year)) +
    geom_point(aes(y = month, color = length_temp), size = 4) +
    labs(x = "Time", y = "Month", title = "Temperature Timeplot for W3_FL") +
    ylim(1,12) +
    theme_minimal()

timeplot
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](W3-FL-EnvrData_files/figure-gfm/timeplot%20-%20temperature-1.png)<!-- -->

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
W3_FL_sal$low_sal_stress <- W3_FL_sal$salinity < 12

low_sal_stress_count <- W3_FL_sal %>%
  mutate(year = year(datetime), 
         month = month(datetime), 
         day = day(datetime)) %>% 
  filter(low_sal_stress == 1) %>%  
  distinct(year, month, day) %>%  # remove dups
  group_by(year, month) %>%
  summarise(low_sal_stress = n(), .groups = "drop") # group all occurrences together by month rather than by numeric day

print(low_sal_stress_count)
```

    ## # A tibble: 7 × 3
    ##    year month low_sal_stress
    ##   <dbl> <dbl>          <int>
    ## 1  2005    10              1
    ## 2  2005    11              3
    ## 3  2005    12              6
    ## 4  2017     9              3
    ## 5  2021     8              1
    ## 6  2022     4              1
    ## 7  2022    10              3

``` r
# this plots low sal stress days only, years with no observations of low salinity stress will not appear in the graph 
ggplot(low_sal_stress_count, aes(x = factor(month), y = low_sal_stress, fill = factor(month))) +
  geom_bar(stat = "identity", fill = "#1f77b4") +
  facet_wrap(~ year) +
  labs(title = "W3_FL: count of low salinity (< 12 ppt) days per month",
       x = "month",
       y = "count of days") +
  theme_minimal()
```

![](W3-FL-EnvrData_files/figure-gfm/low-sal-stress-1.png)<!-- -->

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
  labs(title = "W3_FL: count of low salinity (< 12 ppt) days per month",
       x = "month",
       y = "count of low salinity stress days") +
  theme_minimal()
```

![](W3-FL-EnvrData_files/figure-gfm/low-sal-stress-2.png)<!-- -->

``` r
# points
ggplot(low_sal_complete_data, aes(x = factor(month), y = low_sal_stress)) +
  geom_point(color = "#1f77b4", size = 2) + 
  facet_wrap(~ year) +  
  labs(title = "W3_FL: count of low salinity (< 12 ppt) days per month",
       x = "month",
       y = "count of low salinity stress days") +
  theme_minimal()
```

![](W3-FL-EnvrData_files/figure-gfm/low-sal-stress-3.png)<!-- -->

# now calculate for high salinity stress

``` r
W3_FL_sal$high_sal_stress <- W3_FL_sal$salinity > 35

high_sal_stress_count <- W3_FL_sal %>%
  mutate(year = year(datetime), 
         month = month(datetime), 
         day = day(datetime)) %>% 
  filter(high_sal_stress == 1) %>%  
  distinct(year, month, day) %>%  # remove dups
  group_by(year, month) %>%
  summarise(high_sal_stress = n(), .groups = "drop") # group all occurrences together by month rather than by numeric day

print(high_sal_stress_count)
```

    ## # A tibble: 138 × 3
    ##     year month high_sal_stress
    ##    <dbl> <dbl>           <int>
    ##  1  2005     7              11
    ##  2  2005     8              20
    ##  3  2005     9               4
    ##  4  2005    11               1
    ##  5  2005    12              10
    ##  6  2006     5              20
    ##  7  2006     6              17
    ##  8  2006     7              31
    ##  9  2006     8              31
    ## 10  2006     9              18
    ## # ℹ 128 more rows

``` r
ggplot(high_sal_stress_count, aes(x = factor(month), y = high_sal_stress, fill = factor(month))) +
  geom_bar(stat = "identity", fill = "#1B9AAA") +
  facet_wrap(~ year) +
  labs(title = "W3_FL: count of high salinity (> 35 ppt) days per month",
       x = "month",
       y = "count of days") +
  theme_minimal()
```

![](W3-FL-EnvrData_files/figure-gfm/high-sal-stress-1.png)<!-- -->

``` r
high_sal_complete_data <- complete_year_month %>%
  left_join(high_sal_stress_count, by = c("year", "month")) %>%
  mutate(high_sal_stress = ifelse(is.na(high_sal_stress), 0, high_sal_stress))

high_sal_complete_data$month <- as.numeric(high_sal_complete_data$month)

# bars
ggplot(high_sal_complete_data, aes(x = factor(month), y = high_sal_stress)) +
  geom_bar(stat = "identity", fill = "#1B9AAA") + 
  facet_wrap(~ year) +
  labs(title = "W3_FL: count of high salinity (> 35 ppt) days per month",
       x = "month",
       y = "count of days") +
  theme_minimal()
```

![](W3-FL-EnvrData_files/figure-gfm/high-sal-stress-2.png)<!-- -->

``` r
# points
ggplot(high_sal_complete_data, aes(x = factor(month), y = high_sal_stress)) +
  geom_point(color = "#1B9AAA", size = 2) + 
  facet_wrap(~ year) +  
  labs(title = "W3_FL: count of high salinity (> 35 ppt) days per month",
       x = "month",
       y = "count of days") +
  theme_minimal()
```

![](W3-FL-EnvrData_files/figure-gfm/high-sal-stress-3.png)<!-- -->

``` r
ggplot() +
  geom_bar(data = low_sal_complete_data, aes(x = factor(month), y = low_sal_stress), 
           stat = "identity", fill = "#1f77b4") +
  geom_bar(data = high_sal_complete_data, aes(x = factor(month), y = high_sal_stress), 
           stat = "identity", fill = "#1B9AAA") + 
  facet_wrap(~ year) +
  labs(title = "W3_FL: count of stressful salinity days per month", 
       x = "month", 
       y = "count of days") + 
  theme_minimal()
```

![](W3-FL-EnvrData_files/figure-gfm/combined-salinity-stress-1.png)<!-- -->

``` r
ggplot() +
  geom_point(data = low_sal_complete_data, aes(x = factor(month), y = low_sal_stress), 
           color = "#1f77b4", size = 2) +
  geom_point(data = high_sal_complete_data, aes(x = factor(month), y = high_sal_stress), 
           color = "#1B9AAA", size = 2) + 
  facet_wrap(~ year) +
  labs(title = "W3_FL: count of stressful salinity days per month", 
       x = "month", 
       y = "count of days") + 
  theme_minimal()
```

![](W3-FL-EnvrData_files/figure-gfm/combined-salinity-stress-2.png)<!-- -->

# high temp stress calculations

``` r
W3_FL_temp$high_temp_stress <- W3_FL_temp$temp > 28

high_temp_stress_count <- W3_FL_temp %>%
  mutate(year = year(datetime), 
         month = month(datetime), 
         day = day(datetime)) %>% 
  filter(high_temp_stress == 1) %>%  
  distinct(year, month, day) %>%  # remove dups
  group_by(year, month) %>%
  summarise(high_temp_stress = n(), .groups = "drop") # group all occurrences together by month rather than by numeric day

print(high_temp_stress_count)
```

    ## # A tibble: 83 × 3
    ##     year month high_temp_stress
    ##    <dbl> <dbl>            <int>
    ##  1  2005     6                9
    ##  2  2005     7               31
    ##  3  2005     8               31
    ##  4  2005     9               21
    ##  5  2005    10                6
    ##  6  2006     5                7
    ##  7  2006     6               15
    ##  8  2006     7               31
    ##  9  2006     8               31
    ## 10  2006     9               14
    ## # ℹ 73 more rows

``` r
ggplot(high_temp_stress_count, aes(x = factor(month), y = high_temp_stress, fill = factor(month))) +
  geom_bar(stat = "identity", fill = "#DD4124FF") +
  facet_wrap(~ year) +
  labs(title = "W3_FL: count of high temperature (> 28 C) days per month",
       x = "month",
       y = "count of days") +
  theme_minimal()
```

![](W3-FL-EnvrData_files/figure-gfm/high-temp-stress-1.png)<!-- -->

``` r
high_temp_complete_data <- complete_year_month %>%
  left_join(high_temp_stress_count, by = c("year", "month")) %>%
  mutate(high_temp_stress = ifelse(is.na(high_temp_stress), 0, high_temp_stress))

high_temp_complete_data$month <- as.numeric(high_temp_complete_data$month)

# bars
ggplot(high_temp_complete_data, aes(x = factor(month), y = high_temp_stress)) +
  geom_bar(stat = "identity", fill = "#DD4124FF") + 
  facet_wrap(~ year) +
  labs(title = "W3_FL: count of high temperature (> 28 C) days per month",
       x = "month",
       y = "count of days") +
  theme_minimal()
```

![](W3-FL-EnvrData_files/figure-gfm/high-temp-stress-2.png)<!-- -->

``` r
# points
ggplot(high_temp_complete_data, aes(x = factor(month), y = high_temp_stress)) +
  geom_point(color = "#DD4124FF", size = 2) + 
  facet_wrap(~ year) +  
  labs(title = "W3_FL: count of high temperature (> 28 C) days per month",
       x = "month",
       y = "count of days") +
  theme_minimal()
```

![](W3-FL-EnvrData_files/figure-gfm/high-temp-stress-3.png)<!-- -->

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
Mean_Annual_Temperature_C <- mean(W3_FL_temp$temp)
Mean_max_temperature_C <- mean(W3_FL_envryear_temp$max_temp)
Mean_min_temperature_C <- mean(W3_FL_envryear_temp$min_temp)
Temperature_st_dev <- sd(W3_FL_temp$temp)
Temperature_n <- nrow(W3_FL_temp)
Temperature_years <- nrow(W3_FL_envryear_temp)
high_temp_stress_days <- sum(high_temp_stress_count$high_temp_stress)
frac_high_temp_stress_days <- high_temp_stress_days/nrow(W3_FL_envrday_temp)
temp_quantile_10 <- quantile(W3_FL_temp$temp, 0.1)
temp_quantile_90 <- quantile(W3_FL_temp$temp, 0.9)

Mean_Monthly_Temperature_C <- W3_FL_envrmonth_temp %>%
  filter(!is.na(month)) %>% 
  group_by(month) %>%
  summarise(Mean_Temperature = mean(mean_temp))

Mean_min_Monthly_Temperature_C <- W3_FL_envrmonth_temp %>%
  filter(!is.na(month)) %>% 
  group_by(month) %>%
  summarise(Mean_min_Temperature = mean(min_temp))

Mean_max_Monthly_Temperature_C <- W3_FL_envrmonth_temp %>%
  filter(!is.na(month)) %>% 
  group_by(month) %>%
  summarise(Mean_max_Temperature = mean(max_temp))

#Create a data frame to store the temperature results
W3_FL_temp <- cbind(site_name, download_date, source_description, lat, lon, firstyear, finalyear, Mean_Annual_Temperature_C, Mean_max_temperature_C, Mean_min_temperature_C, temp_quantile_10, temp_quantile_90, Temperature_st_dev, high_temp_stress_days, frac_high_temp_stress_days, Temperature_n, Temperature_years, collection_type)
print(W3_FL_temp)
```

    ##     site_name download_date
    ## 10% "W3_FL"   "09-13-2023" 
    ##     source_description                                                
    ## 10% "National Parks Service Continuous Water Data - Timucuan Preserve"
    ##     lat        lon         firstyear finalyear Mean_Annual_Temperature_C
    ## 10% "30.44116" "-81.43908" "2005"    "2022"    "22.1556673008222"       
    ##     Mean_max_temperature_C Mean_min_temperature_C temp_quantile_10
    ## 10% "30.6387"              "10.38665"             "14.559"        
    ##     temp_quantile_90 Temperature_st_dev high_temp_stress_days
    ## 10% "29.279"         "5.70766431643303" "1610"               
    ##     frac_high_temp_stress_days Temperature_n Temperature_years collection_type
    ## 10% "0.306491528650295"        "258086"      "20"              "continuous"

``` r
W3_FL_monthly_temp <- cbind(Mean_Monthly_Temperature_C, Mean_min_Monthly_Temperature_C, Mean_max_Monthly_Temperature_C)
W3_FL_monthly_temp <- W3_FL_monthly_temp[, !duplicated(names(W3_FL_monthly_temp))]
print(W3_FL_monthly_temp)
```

    ##    month Mean_Temperature Mean_min_Temperature Mean_max_Temperature
    ## 1      1         14.42469             10.68465             18.18247
    ## 2      2         15.08157             11.41169             19.24219
    ## 3      3         17.63505             13.48647             21.72212
    ## 4      4         21.24945             17.00067             25.49140
    ## 5      5         24.84163             20.94372             28.61353
    ## 6      6         28.03144             25.05371             30.78153
    ## 7      7         28.92036             25.88794             31.47087
    ## 8      8         28.81000             25.81860             31.43927
    ## 9      9         28.14280             25.00193             30.87313
    ## 10    10         24.65823             20.23663             27.99587
    ## 11    11         19.35738             15.11940             23.99073
    ## 12    12         16.43117             12.84400             20.04950

``` r
# Write to the combined file with all sites 
write.table(W3_FL_temp, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/all_temperature.csv", sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE) # The column names should be changed to FALSE after 1st row is added to the data frame

# Write to a unique new CSV file
write.csv(W3_FL_temp, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W3_FL_temperature.csv")

# Write all montly data to a unique new CSV file
write.csv(W3_FL_monthly_temp, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W3_FL_monthly_temp.csv")
```

``` r
#Calculate the salinity variables
Mean_Annual_Salinity_ppt <- mean(W3_FL_sal$salinity)
Mean_max_Salinity_ppt <- mean(W3_FL_envryear_sal$max_salinity)
Mean_min_Salinity_ppt <- mean(W3_FL_envryear_sal$min_salinity)
Salinity_st_dev <- sd(W3_FL_sal$salinity)
Salinity_n <- nrow(W3_FL_sal)
Salinity_years <- nrow(W3_FL_envryear_sal)
high_sal_stress_days <- sum(high_sal_stress_count$high_sal_stress)
low_sal_stress_days <- sum(low_sal_stress_count$low_sal_stress)
frac_high_sal_stress_days <- high_sal_stress_days/nrow(W3_FL_envrday_sal)
frac_low_sal_stress_days <- low_sal_stress_days/nrow(W3_FL_envrday_sal)
salinity_quantile_10 <- quantile(W3_FL_sal$salinity, 0.1)
salinity_quantile_90 <- quantile(W3_FL_sal$salinity, 0.9)

Mean_Monthly_Salinity <- W3_FL_envrmonth_sal %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(Mean_Salinity = mean(mean_salinity))

Min_Monthly_Salinity <- W3_FL_envrmonth_sal %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(Min_Salinity = mean(min_salinity))

Max_Monthly_Salinity <- W3_FL_envrmonth_sal %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(Max_Salinity = mean(max_salinity))

#Create a data frame to store the temperature results
W3_FL_salinity <- cbind(site_name, download_date, source_description, lat, lon, firstyear, finalyear, Mean_Annual_Salinity_ppt, Mean_max_Salinity_ppt, Mean_min_Salinity_ppt, salinity_quantile_10, salinity_quantile_90, high_sal_stress_days,low_sal_stress_days, frac_high_sal_stress_days, frac_low_sal_stress_days, Salinity_st_dev, Salinity_n, Salinity_years, collection_type)
print(W3_FL_salinity)
```

    ##     site_name download_date
    ## 10% "W3_FL"   "09-13-2023" 
    ##     source_description                                                
    ## 10% "National Parks Service Continuous Water Data - Timucuan Preserve"
    ##     lat        lon         firstyear finalyear Mean_Annual_Salinity_ppt
    ## 10% "30.44116" "-81.43908" "2005"    "2022"    "32.3522536547562"      
    ##     Mean_max_Salinity_ppt Mean_min_Salinity_ppt salinity_quantile_10
    ## 10% "37.2042051805"       "15.4224441766"       "27.850664281"      
    ##     salinity_quantile_90 high_sal_stress_days low_sal_stress_days
    ## 10% "35.97585067"        "2472"               "18"               
    ##     frac_high_sal_stress_days frac_low_sal_stress_days Salinity_st_dev   
    ## 10% "0.485086342229199"       "0.00353218210361068"    "3.71539685491799"
    ##     Salinity_n Salinity_years collection_type
    ## 10% "249752"   "20"           "continuous"

``` r
W3_FL_monthly_sal <- cbind(Mean_Monthly_Salinity, Min_Monthly_Salinity, Max_Monthly_Salinity)
W3_FL_monthly_sal <- W3_FL_monthly_sal[, !duplicated(names(W3_FL_monthly_sal))]
print(W3_FL_monthly_sal)
```

    ##    month Mean_Salinity Min_Salinity Max_Salinity
    ## 1      1      32.06987     23.21464     35.10137
    ## 2      2      31.54371     24.04226     35.17848
    ## 3      3      31.74700     24.57532     34.88501
    ## 4      4      32.41533     26.11596     35.36003
    ## 5      5      34.12190     28.19313     36.76262
    ## 6      6      33.66464     26.04255     36.61912
    ## 7      7      32.38558     24.72742     36.56183
    ## 8      8      32.94747     23.84207     36.99356
    ## 9      9      32.30926     23.05972     36.35169
    ## 10    10      31.15622     20.89796     35.17352
    ## 11    11      31.80450     22.51476     35.44977
    ## 12    12      31.47130     21.31737     35.10996

``` r
# Write to the combined file with all sites 
write.table(W3_FL_salinity, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/all_salinity.csv", sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE) # The column names should be changed to FALSE after 1st row is added to the data frame

# Write all year data to a unique new CSV file
write.csv(W3_FL_salinity, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W3_FL_salinity.csv")
# Write all montly data to a unique new CSV file
write.csv(W3_FL_monthly_sal, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W3_FL_monthly_sal.csv")
```
