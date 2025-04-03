W6_ME - Processed Environmental Data
================
Madeline Eppley
3/21/2024

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
#Data was downloaded on 3/21/2024 from UMaine Loboviz 
#Source - University of Maine http://maine.loboviz.com/ and http://maine.loboviz.com/cgi-lobo/lobo and http://gyre.umeoce.maine.edu/data/gomoos/buoy/html/E05.html. 
#The site was sampled intermittently

#Create text strings with metadata information that we want to include in the final data frame. 
download_date <- ("03-21-2024")
source_description <- ("University of Maine")
site_name <- ("W6_ME") #Use site code with site number based on lat position and state
collection_type <- ("intermittent")
```

### Use the file path name in your working directory or desktop, see example below. Or, import data set through the “Files” window in R studio. Store the file in a variable with the “raw_ID_Site” format. If salinity and temperature data are in separate files, read in both and store them with “\_sal” or “\_temp” in the variable names.

``` r
#The file we will be working with is from the Upper Damariscotta Estuary. The ID_Site for this site is W6_ME. 
raw_W6_ME <- read_csv("/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W6-ME-raw.csv")
```

    ## Rows: 26314 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Timestamp
    ## dbl (2): salinity, temperature
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# View how the data is stored. Note the variable names and the format and units that the data are stored in.  
spec(raw_W6_ME)
```

    ## cols(
    ##   Timestamp = col_character(),
    ##   salinity = col_double(),
    ##   temperature = col_double()
    ## )

``` r
#View(raw_W6_ME)
```

### Start with the date and time of collection. We will use the lubridate package to standardize all values into the date-time format called POSIXct. This format stores the date and time in number of seconds since a past point (1/1/1970). This makes comparisons easy and helps to standardizes values.

``` r
#Convert to POSIXct format. Store it into a column named datetime in the data frame.
raw_W6_ME$datetime <- as.POSIXct(raw_W6_ME$Timestamp, "%m/%d/%y %H:%M", tz = "")

#Print the new data frame and examine to make sure the new datetime column is in the correct format. 
head(raw_W6_ME)
```

    ## # A tibble: 6 × 4
    ##   Timestamp     salinity temperature datetime           
    ##   <chr>            <dbl>       <dbl> <dttm>             
    ## 1 9/25/15 16:00     31.4        19.1 2015-09-25 16:00:00
    ## 2 9/25/15 17:00     31.4        19.0 2015-09-25 17:00:00
    ## 3 9/25/15 18:00     31.5        18.7 2015-09-25 18:00:00
    ## 4 9/25/15 19:00     31.6        18.2 2015-09-25 19:00:00
    ## 5 9/25/15 20:00     31.6        18.0 2015-09-25 20:00:00
    ## 6 9/25/15 21:00     31.6        17.7 2015-09-25 21:00:00

``` r
# we have NAs, so remove those
raw_W6_ME <- raw_W6_ME %>%
    filter(!is.na(datetime))
summary(raw_W6_ME$datetime)
```

    ##                       Min.                    1st Qu. 
    ## "2015-09-25 16:00:00.0000" "2017-06-24 11:15:00.0000" 
    ##                     Median                       Mean 
    ## "2018-08-02 23:30:00.0000" "2018-12-16 04:21:01.1423" 
    ##                    3rd Qu.                       Max. 
    ## "2021-04-30 04:45:00.0000" "2022-10-12 14:00:00.0000"

### Analyze the ranges of all of our variables of interest - time, salinity, and temperature. Make sure that the latitude and longitude values are consistent for a static collection site. This is a quick check so we can determine how to conduct the next filtering step.

``` r
#Standardize column and variable names. We will use "temp" for temperature in degrees C, "salinity" for salinity in parts per thousand (ppt), "lat" for latitude in degrees, and "lon" for longitude in degrees. 
#Use the dyplr format to rename multiple columns in the format "dataframe %>% rename("new name 1" = "old name 1", "new name 2", "old name 2")
raw_W6_ME <- raw_W6_ME %>% rename("salinity" = "salinity")
raw_W6_ME <- raw_W6_ME  %>% rename("temp" = "temperature")

#Store variables that we will include in the final data frame
lat <- 43.986
lon <- -69.55
firstyear <- 2015
finalyear <- 2023
```

### Filter any of the variables that have data points outside of normal range. We will use 0-40 as the accepted range for salinity (ppt) and temperature (C) values. Note, in the summer, salinity values can sometimes exceed 40. Check to see if there are values above 40. In this case, adjust the range or notify someone that the site has particularly high salinity values.

``` r
#Filter the data between the values of 0 and 40 for both salinity and temperature. 
filtered_W6_ME <- raw_W6_ME %>%
    filter(between(salinity, 0, 42)) 
           
filtered_W6_ME <- raw_W6_ME %>%
    filter(between(temp, 0, 40))

# Sanity check - print the ranges to ensure values are filtered properly. We can see that the ranges for both are now in the appropriate range.  
print(summary(filtered_W6_ME$salinity))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00   28.61   30.19   29.48   30.92   32.39

``` r
print(summary(filtered_W6_ME$temp))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1.68   10.85   16.27   15.33   19.83   26.15

``` r
#Store our data into a variable name with just the site name. 
W6_ME <- filtered_W6_ME

# check if we we have NAs in the our salinity data frame in the datetime column
count.nas_datetime <- is.na(W6_ME$datetime) # store our NAs in a variable
summary(count.nas_datetime) # no, we don't have any NAs, so we are good to go
```

    ##    Mode   FALSE 
    ## logical   26314

``` r
count.nas_temp <- is.na(W6_ME$temp)
summary(count.nas_temp) # no, we don't have any NAs, so we are good to go
```

    ##    Mode   FALSE 
    ## logical   26314

``` r
count.nas_sal <- is.na(W6_ME$salinity)
summary(count.nas_sal) # no, we don't have any NAs, so we are good to go
```

    ##    Mode   FALSE 
    ## logical   26314

### Visualize the salinity, temperature, and date ranges over time. This can help us see if there are any anomalies or gaps in the data and make sure the filtering was done correctly. Sanity check - do the temperature and salinity ranges look appropriate for the geography of the site (ex. near full ocean salinity for coastal sites, lower salinity for estuaries or near rivers)?

``` r
salplot <- ggplot(W6_ME, aes(x = datetime)) +
    geom_line(aes(y = salinity, color = "Salinity (ppt)")) +
    ylim(0,40) +
    labs(x = "Time", y = "Salinity ppt", title = "Salinity Plot for W6_ME - Damariscotta Estuary") +
    scale_color_manual(values = c("Salinity (ppt)" = "blue")) +
    theme_minimal()

salplot
```

![](W6-ME-EnvrData_files/figure-gfm/salinity-plot-1.png)<!-- -->

``` r
tempplot <- ggplot(W6_ME, aes(x = datetime)) +
    geom_line(aes(y = temp, color = "Temperature (C)")) +
    ylim(0, 45) +
    labs(x = "Time", y = "Temperature C", title = "Temperature Plot for W6_ME - Damariscotta Estuary") +
    scale_color_manual(values = c( "Temperature (C)" = "red")) +
    theme_minimal()


tempplot
```

![](W6-ME-EnvrData_files/figure-gfm/temperature-plot-1.png)<!-- -->

### We need to calculate the mean, maximum, and minimum values for salinity and temperature per month and year. First make two data frames to contain each of the annual and monthly averages.

``` r
#Calculate the mean, maximum, and minimum values for salinity and temperature for each month. 
W6_ME_envrmonth_sal <- W6_ME %>%
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
W6_ME_envrmonth_temp <- W6_ME %>%
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
print(W6_ME_envrmonth_sal)
```

    ## # A tibble: 46 × 6
    ## # Groups:   year [7]
    ##     year month min_salinity max_salinity mean_salinity length_salinity
    ##    <dbl> <dbl>        <dbl>        <dbl>         <dbl>           <int>
    ##  1  2015     9        28.1          31.7          31.4             128
    ##  2  2015    10        25.5          31.4          29.5             724
    ##  3  2015    11        24.5          30.7          28.9             543
    ##  4  2016     4         0.02         29.0          28.0             275
    ##  5  2016     5        27.6          30.3          29.1             744
    ##  6  2016     6         0.15         30.8          29.9             720
    ##  7  2016     7         0.4          31.4          30.8             744
    ##  8  2016     8         0.35         31.7          31.4             744
    ##  9  2016     9         0.3          32.2          31.8             720
    ## 10  2016    10        31.5          32.3          32.1             744
    ## # ℹ 36 more rows

``` r
print(W6_ME_envrmonth_temp)
```

    ## # A tibble: 46 × 6
    ## # Groups:   year [7]
    ##     year month min_temp max_temp mean_temp length_temp
    ##    <dbl> <dbl>    <dbl>    <dbl>     <dbl>       <int>
    ##  1  2015     9    16.9      19.1     17.8          128
    ##  2  2015    10     9.13     17.3     13.0          724
    ##  3  2015    11     6.5      11.6      9.46         543
    ##  4  2016     4     7.55     17.0      9.51         275
    ##  5  2016     5     8.93     18.4     12.5          744
    ##  6  2016     6    13.2      22.0     17.6          720
    ##  7  2016     7    17.6      24.3     21.2          744
    ##  8  2016     8    20.3      24.3     22.0          744
    ##  9  2016     9    16.0      22.9     19.3          720
    ## 10  2016    10     9.8      16.7     14.1          744
    ## # ℹ 36 more rows

``` r
#Calculate the mean, maximum, and minimum values for salinity and temperature for each year. 
W6_ME_envryear <- W6_ME %>%
    mutate(year = year(datetime)) %>%
    group_by(year) %>%
    summarise(
      min_salinity = min(salinity),
      max_salinity = max(salinity),
      mean_salinity = mean(salinity),
      min_temp = min(temp),
      max_temp = max(temp),
      mean_temp = mean(temp))

print(W6_ME_envryear)
```

    ## # A tibble: 7 × 7
    ##    year min_salinity max_salinity mean_salinity min_temp max_temp mean_temp
    ##   <dbl>        <dbl>        <dbl>         <dbl>    <dbl>    <dbl>     <dbl>
    ## 1  2015        24.5          31.7          29.4     6.5      19.1      12.1
    ## 2  2016         0.02         32.4          30.8     7.55     24.3      16.6
    ## 3  2017         0.01         31.7          30.6     4.47     23.7      16.9
    ## 4  2018         0            31.4          29.0     2.08     26.2      15.6
    ## 5  2019        22.0          31.4          29.5     4        24.2      16.6
    ## 6  2021        14.6          31.1          28.9     1.68     23.8      14.1
    ## 7  2022         0.01         30.8          27.2     1.87     22.9      12.0

``` r
#Calculate the mean, maximum, and minimum values for salinity and temperature for each day. 
W6_ME_envrday <- W6_ME %>%
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
print(W6_ME_envrday)
```

    ## # A tibble: 1,128 × 11
    ## # Groups:   year, month [46]
    ##     year month   day min_salinity max_salinity mean_salinity length_salinity
    ##    <dbl> <dbl> <int>        <dbl>        <dbl>         <dbl>           <int>
    ##  1  2015     9    25         31.4         31.6          31.5               8
    ##  2  2015     9    26         31.4         31.7          31.5              24
    ##  3  2015     9    27         31.4         31.7          31.6              24
    ##  4  2015     9    28         31.4         31.7          31.6              24
    ##  5  2015     9    29         31.5         31.7          31.6              24
    ##  6  2015     9    30         28.1         31.5          30.4              24
    ##  7  2015    10     1         28.1         30.5          29.6              24
    ##  8  2015    10     2         27.4         30.2          29.2              24
    ##  9  2015    10     3         26.8         29.9          28.8              24
    ## 10  2015    10     4         26.4         29.4          28.3              24
    ## # ℹ 1,118 more rows
    ## # ℹ 4 more variables: min_temp <dbl>, max_temp <dbl>, mean_temp <dbl>,
    ## #   length_temp <int>

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
W6_ME$low_sal_stress <- W6_ME$salinity < 12

low_sal_stress_count <- W6_ME %>%
  mutate(year = year(datetime), 
         month = month(datetime), 
         day = day(datetime)) %>% 
  filter(low_sal_stress == 1) %>%  
  distinct(year, month, day) %>%  # remove dups
  group_by(year, month) %>%
  summarise(low_sal_stress = n(), .groups = "drop") # group all occurrences together by month rather than by numeric day

print(low_sal_stress_count)
```

    ## # A tibble: 12 × 3
    ##     year month low_sal_stress
    ##    <dbl> <dbl>          <int>
    ##  1  2016     4              1
    ##  2  2016     6              1
    ##  3  2016     7              1
    ##  4  2016     8              1
    ##  5  2016     9              1
    ##  6  2016    11              1
    ##  7  2017     6              1
    ##  8  2018     4              1
    ##  9  2018     5              1
    ## 10  2022     3              2
    ## 11  2022     4              1
    ## 12  2022    10              1

``` r
# this plots low sal stress days only, years with no observations of low salinity stress will not appear in the graph 
ggplot(low_sal_stress_count, aes(x = factor(month), y = low_sal_stress, fill = factor(month))) +
  geom_bar(stat = "identity", fill = "#1f77b4") +
  facet_wrap(~ year) +
  labs(title = "W6_ME: count of low salinity (< 12 ppt) days per month",
       x = "month",
       y = "count of days") +
  theme_minimal()
```

![](W6-ME-EnvrData_files/figure-gfm/low-sal-stress-1.png)<!-- -->

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
  labs(title = "W6_ME: count of low salinity (< 12 ppt) days per month",
       x = "month",
       y = "count of low salinity stress days") +
  theme_minimal()
```

![](W6-ME-EnvrData_files/figure-gfm/low-sal-stress-2.png)<!-- -->

``` r
# points
ggplot(low_sal_complete_data, aes(x = factor(month), y = low_sal_stress)) +
  geom_point(color = "#1f77b4", size = 2) + 
  facet_wrap(~ year) +  
  labs(title = "W6_ME: count of low salinity (< 12 ppt) days per month",
       x = "month",
       y = "count of low salinity stress days") +
  theme_minimal()
```

![](W6-ME-EnvrData_files/figure-gfm/low-sal-stress-3.png)<!-- -->

# now calculate for high salinity stress

``` r
W6_ME$high_sal_stress <- W6_ME$salinity > 35

high_sal_stress_count <- W6_ME %>%
  mutate(year = year(datetime), 
         month = month(datetime), 
         day = day(datetime)) %>% 
  filter(high_sal_stress == 1) %>%  
  distinct(year, month, day) %>%  # remove dups
  group_by(year, month) %>%
  summarise(high_sal_stress = n(), .groups = "drop") # group all occurrences together by month rather than by numeric day

print(high_sal_stress_count) # no high sal stress
```

    ## # A tibble: 0 × 3
    ## # ℹ 3 variables: year <dbl>, month <dbl>, high_sal_stress <int>

# high temp stress calculations

``` r
W6_ME$high_temp_stress <- W6_ME$temp > 28

high_temp_stress_count <- W6_ME %>%
  mutate(year = year(datetime), 
         month = month(datetime), 
         day = day(datetime)) %>% 
  filter(high_temp_stress == 1) %>%  
  distinct(year, month, day) %>%  # remove dups
  group_by(year, month) %>%
  summarise(high_temp_stress = n(), .groups = "drop") # group all occurrences together by month rather than by numeric day

print(high_temp_stress_count) # no high temp stress
```

    ## # A tibble: 0 × 3
    ## # ℹ 3 variables: year <dbl>, month <dbl>, high_temp_stress <int>

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
Mean_Annual_Temperature_C <- mean(W6_ME$temp)
Mean_max_temperature_C <- mean(W6_ME_envryear$max_temp)
Mean_min_temperature_C <- mean(W6_ME_envryear$min_temp)
Temperature_st_dev <- sd(W6_ME$temp)
Temperature_n <- nrow(W6_ME)
Temperature_years <- nrow(W6_ME_envryear)
high_temp_stress_days <- sum(high_temp_stress_count$high_temp_stress)
frac_high_temp_stress_days <- high_temp_stress_days/nrow(W6_ME_envrday)
temp_quantile_10 <- quantile(W6_ME$temp, 0.1)
temp_quantile_90 <- quantile(W6_ME$temp, 0.9)

Mean_Monthly_Temperature_C <- W6_ME_envrmonth_temp %>%
  filter(!is.na(month)) %>% 
  group_by(month) %>%
  summarise(Mean_Temperature = mean(mean_temp))

Mean_min_Monthly_Temperature_C <- W6_ME_envrmonth_temp %>%
  filter(!is.na(month)) %>% 
  group_by(month) %>%
  summarise(Mean_min_Temperature = mean(min_temp))

Mean_max_Monthly_Temperature_C <- W6_ME_envrmonth_temp %>%
  filter(!is.na(month)) %>% 
  group_by(month) %>%
  summarise(Mean_max_Temperature = mean(max_temp))

#Create a data frame to store the temperature results
W6_ME_temp <- cbind(site_name, download_date, source_description, lat, lon, firstyear, finalyear, Mean_Annual_Temperature_C, Mean_max_temperature_C, Mean_min_temperature_C, temp_quantile_10, temp_quantile_90, Temperature_st_dev, high_temp_stress_days, frac_high_temp_stress_days, Temperature_n, Temperature_years, collection_type)
print(W6_ME_temp)
```

    ##     site_name download_date source_description    lat      lon      firstyear
    ## 10% "W6_ME"   "03-21-2024"  "University of Maine" "43.986" "-69.55" "2015"   
    ##     finalyear Mean_Annual_Temperature_C Mean_max_temperature_C
    ## 10% "2023"    "15.3288717032758"        "23.46"               
    ##     Mean_min_temperature_C temp_quantile_10 temp_quantile_90 Temperature_st_dev
    ## 10% "4.02142857142857"     "7.563"          "21.5"           "5.22149721972098"
    ##     high_temp_stress_days frac_high_temp_stress_days Temperature_n
    ## 10% "0"                   "0"                        "26314"      
    ##     Temperature_years collection_type
    ## 10% "7"               "intermittent"

``` r
W6_ME_monthly_temp <- cbind(Mean_Monthly_Temperature_C, Mean_min_Monthly_Temperature_C, Mean_max_Monthly_Temperature_C)
W6_ME_monthly_temp <- W6_ME_monthly_temp[, !duplicated(names(W6_ME_monthly_temp))]
print(W6_ME_monthly_temp)
```

    ##    month Mean_Temperature Mean_min_Temperature Mean_max_Temperature
    ## 1      3         5.412228             1.775000             14.73000
    ## 2      4         8.524736             5.737500             14.52750
    ## 3      5        13.333948             8.902500             18.42500
    ## 4      6        17.942470            14.618000             22.29400
    ## 5      7        20.582932            18.460000             22.93333
    ## 6      8        21.390830            19.308000             24.28800
    ## 7      9        18.827409            16.685000             21.66500
    ## 8     10        15.211887            11.984286             18.16286
    ## 9     11         8.784046             4.781667             12.02500
    ## 10    12         6.290101             4.070000              7.76000

``` r
# Write to the combined file with all sites 
write.table(W6_ME_temp, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/all_temperature.csv", sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE) # The column names should be changed to FALSE after 1st row is added to the data frame

# Write to a unique new CSV file
write.csv(W6_ME_temp, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W6_ME_temperature.csv")

# Write all montly data to a unique new CSV file
write.csv(W6_ME_monthly_temp, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W6_ME_monthly_temp.csv")
```

``` r
#Calculate the salinity variables
Mean_Annual_Salinity_ppt <- mean(W6_ME$salinity)
Mean_max_Salinity_ppt <- mean(W6_ME_envryear$max_salinity)
Mean_min_Salinity_ppt <- mean(W6_ME_envryear$min_salinity)
Salinity_st_dev <- sd(W6_ME$salinity)
Salinity_n <- nrow(W6_ME)
Salinity_years <- nrow(W6_ME_envryear)
high_sal_stress_days <- sum(high_sal_stress_count$high_sal_stress)
low_sal_stress_days <- sum(low_sal_stress_count$low_sal_stress)
frac_high_sal_stress_days <- high_sal_stress_days/nrow(W6_ME_envrday)
frac_low_sal_stress_days <- low_sal_stress_days/nrow(W6_ME_envrday)
salinity_quantile_10 <- quantile(W6_ME$salinity, 0.1)
salinity_quantile_90 <- quantile(W6_ME$salinity, 0.9)

Mean_Monthly_Salinity <- W6_ME_envrmonth_sal %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(Mean_Salinity = mean(mean_salinity))

Min_Monthly_Salinity <- W6_ME_envrmonth_sal %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(Min_Salinity = mean(min_salinity))

Max_Monthly_Salinity <- W6_ME_envrmonth_sal %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(Max_Salinity = mean(max_salinity))

#Create a data frame to store the temperature results
W6_ME_salinity <- cbind(site_name, download_date, source_description, lat, lon, firstyear, finalyear, Mean_Annual_Salinity_ppt, Mean_max_Salinity_ppt, Mean_min_Salinity_ppt, salinity_quantile_10, salinity_quantile_90, high_sal_stress_days,low_sal_stress_days, frac_high_sal_stress_days, frac_low_sal_stress_days, Salinity_st_dev, Salinity_n, Salinity_years, collection_type)
print(W6_ME_salinity)
```

    ##     site_name download_date source_description    lat      lon      firstyear
    ## 10% "W6_ME"   "03-21-2024"  "University of Maine" "43.986" "-69.55" "2015"   
    ##     finalyear Mean_Annual_Salinity_ppt Mean_max_Salinity_ppt
    ## 10% "2023"    "29.4782830432469"       "31.5042857142857"   
    ##     Mean_min_Salinity_ppt salinity_quantile_10 salinity_quantile_90
    ## 10% "8.74142857142857"    "26.29"              "31.48"             
    ##     high_sal_stress_days low_sal_stress_days frac_high_sal_stress_days
    ## 10% "0"                  "13"                "0"                      
    ##     frac_low_sal_stress_days Salinity_st_dev   Salinity_n Salinity_years
    ## 10% "0.0115248226950355"     "2.2778779612237" "26314"    "7"           
    ##     collection_type
    ## 10% "intermittent"

``` r
W6_ME_monthly_sal <- cbind(Mean_Monthly_Salinity, Min_Monthly_Salinity, Max_Monthly_Salinity)
W6_ME_monthly_sal <- W6_ME_monthly_sal[, !duplicated(names(W6_ME_monthly_sal))]
print(W6_ME_monthly_sal)
```

    ##    month Mean_Salinity Min_Salinity Max_Salinity
    ## 1      3      26.85994     12.97000     29.38500
    ## 2      4      26.98511      6.79500     29.10750
    ## 3      5      28.23448     20.21500     29.87750
    ## 4      6      29.62502     17.12600     30.48800
    ## 5      7      29.80361     23.42333     30.57500
    ## 6      8      30.58019     20.92400     31.15200
    ## 7      9      31.04901     24.27167     31.53000
    ## 8     10      26.25573     23.97857     27.05429
    ## 9     11      28.21759     19.16000     30.48833
    ## 10    12      27.13188     24.32000     29.05000

``` r
# Write to the combined file with all sites 
write.table(W6_ME_salinity, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/all_salinity.csv", sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE) # The column names should be changed to FALSE after 1st row is added to the data frame

# Write all year data to a unique new CSV file
write.csv(W6_ME_salinity, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W6_ME_salinity.csv")
# Write all montly data to a unique new CSV file
write.csv(W6_ME_monthly_sal, "/Users/madelineeppley/GitHub/MVP-H2F-HatcheryField/data/environment/W6_ME_monthly_sal.csv")
```
