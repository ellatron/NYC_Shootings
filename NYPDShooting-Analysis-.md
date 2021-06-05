NYPDShooting\_peer
================
Ellen Coy
5/17/2021

# Statement of Interest

This analysis is to look at shootings in the NYC five borough in 2020.
The focus will be on time of year by comparing totals for each month,
days by looking at totals for each weekday and time of day by looking at
total shootings in each hour of the day.

# Data Source

The data for this analysis is from Data.gov \| [NYPD Shooting Incident
Data](https://catalog.data.gov/dataset/nypd-shooting-incident-data-historic)

This data is provided and maintained by the City of New York and is a
report of shooting incidents. \#\# Import Data and set up environment

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.2     ✓ dplyr   1.0.6
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
NYPDShooting <- read.csv("https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD")
```

# Cleaning and Transforming Data

## Selecting columns for analysis

``` r
NYPDShootingv2 <- NYPDShooting %>%
  select(OCCUR_DATE, OCCUR_TIME, BORO, PERP_AGE_GROUP, PERP_SEX, PERP_RACE, VIC_AGE_GROUP, VIC_SEX, VIC_RACE) 

NYPDShootingv2$OCCUR_DATE = as.Date(NYPDShootingv2$OCCUR_DATE, "%m/%d/%y")
NYPDShootingv2$OCCUR_TIME = hms(NYPDShootingv2$OCCUR_TIME)

NYPDShootingv2$HOUR <- hour(NYPDShootingv2$OCCUR_TIME)
NYPDShootingv2$MONTH <- month(NYPDShootingv2$OCCUR_DATE, label = TRUE)
NYPDShootingv2$WEEK_DAY <- wday(NYPDShootingv2$OCCUR_DATE, label = TRUE)
```

## Summary of new Data set

``` r
summary(NYPDShootingv2)
```

    ##    OCCUR_DATE           OCCUR_TIME                            BORO          
    ##  Min.   :2020-01-01   Min.   :0S                          Length:23568      
    ##  1st Qu.:2020-05-04   1st Qu.:3H 20M 0S                   Class :character  
    ##  Median :2020-07-16   Median :15H 0M 0S                   Mode  :character  
    ##  Mean   :2020-07-12   Mean   :12H 32M 59.1318737270849S                     
    ##  3rd Qu.:2020-09-24   3rd Qu.:20H 44M 15S                                   
    ##  Max.   :2020-12-31   Max.   :23H 59M 0S                                    
    ##                                                                             
    ##  PERP_AGE_GROUP       PERP_SEX          PERP_RACE         VIC_AGE_GROUP     
    ##  Length:23568       Length:23568       Length:23568       Length:23568      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##    VIC_SEX            VIC_RACE              HOUR           MONTH     
    ##  Length:23568       Length:23568       Min.   : 0.00   Jul    :2798  
    ##  Class :character   Class :character   1st Qu.: 3.00   Aug    :2774  
    ##  Mode  :character   Mode  :character   Median :15.00   Jun    :2458  
    ##                                        Mean   :12.08   Sep    :2219  
    ##                                        3rd Qu.:20.00   May    :2175  
    ##                                        Max.   :23.00   Oct    :2019  
    ##                                                        (Other):9125  
    ##  WEEK_DAY  
    ##  Sun:3527  
    ##  Mon:3309  
    ##  Tue:3353  
    ##  Wed:3414  
    ##  Thu:3249  
    ##  Fri:3316  
    ##  Sat:3400

# Analysis

## Data Table for Shootings per Month

This data table includes the total number of shootings each month for
each of the five NYC boroughs.

``` r
borough <- NYPDShootingv2 %>%
  group_by(BORO, MONTH) %>%
  summarise(number_of_shootings = n(), .groups = "drop")
View(borough)
```

### Chart to represent Shootings per Month

Using the borough data table, plot a line for each borough to represent
total number of shootings per month.

#### Findings

-   Brooklyn saw the most number of shootings in 2020, the Bronx was the
    next highest.
-   The highest total shootings was July in Brooklyn with just over 1250
    total
-   Both Brooklyn and the Bronx saw a large increase of shootings in the
    summer months June - August
-   Staten Island had the lowest number of shootings, consistently below
    125 monthly.

``` r
ggplot(data = borough) +
    geom_line(mapping = aes(x = MONTH, y = number_of_shootings, group = BORO, color = BORO)) +
  labs(title = "Number of Shootings by Month", subtitle = "Categorized by NYC Borrough", caption = "Data from Jan 2020 - Dec 2020", y = "Total Number of Shootings")
```

![](NYPDShooting-Analysis-_files/figure-gfm/Borrough%20-%20Shootings%20per%20month%20chart-1.png)<!-- -->
\#\# Data Table for Shootings per Day of the Week This data table
includes the total number of shootings each Day of the week for each of
the five NYC boroughs.

``` r
DayOfWeek <- NYPDShootingv2 %>%
  group_by(BORO, WEEK_DAY) %>%
  summarise(number_of_shootings = n(), .groups = "drop")
View(DayOfWeek)
```

### Chart to represent Shootings per Day of the week

Using the borough data table, plot a line for each borough to represent
total number of shootings per weekday.

#### Findings

-   Total number of shootings per Day of the Week is consistent
-   Each borough sees a consistent number of shootings per Day of the
    Week

``` r
ggplot(data = DayOfWeek) +
    geom_col(mapping = aes(x = WEEK_DAY, y = number_of_shootings, group = BORO, color = BORO, fill = BORO)) +
  labs(title = "Number of Shootings by Day of Week", subtitle = "Categorized by NYC Borrough", caption = "Data from Jan 2020 - Dec 2020", y = "Total Number of Shootings")
```

![](NYPDShooting-Analysis-_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Data Table for Shootings per Time of Day

This data table includes the total number of shootings each hour for
each of the five NYC boroughs. 0 = Midnight - 23 = 11pm

``` r
TimeDay <- NYPDShootingv2 %>%
  group_by(BORO, HOUR) %>%
  summarise(number_of_shootings = n(), .groups = "drop")
View(TimeDay)
```

### Chart to represent Shootings per Time of Day

Using the borough data table, plot a line for each borough to represent
total number of shootings per month.

#### Findings

-   Brooklyn saw the most number of shootings in 2020, the Bronx was the
    next highest.
-   The highest total shootings was between 11pm and 1am in Brooklyn
-   Both Brooklyn and the Bronx saw a large increase of shootings in the
    evening and night from 7pm - 4am.
-   Lowest number of shootings across all boroughs was between 5am and
    1pm
-   Staten Island had the lowest number of shootings, consistently below
    100 at any time of day.

``` r
ggplot(data = TimeDay) +
  geom_line(mapping = aes(x = HOUR, y = number_of_shootings, color = BORO)) +
  labs(title = "Number of Shootings by Time of Day", subtitle = "Categorized by NYC Borrough", caption = "Data from Jan 2020 - Dec 2020", y = "Total Number of Shootings")
```

![](NYPDShooting-Analysis-_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
\# Conclusion

## What was not included in this analysis - Bias

The data available from NYPD on shootings includes information on
gender, race and age of victims and perpetrator in addition to locations
and timing. With many incidents where the perpetrator is unknown it
would be bias and unfair to use this information in the analysis without
further data collection. For an unbias analysis involving race, gender
and age of perpetrators information on pending investigations and
policing practices for the area would be needed. This analysis does not
include total population numbers for each borough so while Brooklyn had
the highest total of murders it could potentially have the highest
population. Because of this this analysis should only be used to look at
total shootings but not to conclude if any borough is more dangerous
than another.

## Focus of this analysis

The focus of this analysis was on time and places of shootings. This was
a quantitative analysis of number of shootings in each borough by Month,
day of the week, and time of day.

## Key findings

-   Brooklyn and the Bronx saw the most shootings in 2020 particularly
    between the months June and August in the late evenings and
    overnight.
-   Staten Island saw the fewest number of shooting in 2020 with
    consistency in numbers across months and time of day.
-   Day of the week does not impact the number of shootings across all
    boroughs.
-   The fewest shootings happen between 5am and 1pm across all boroughs.

``` r
sessionInfo(
  
)
```

    ## R version 4.0.5 (2021-03-31)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Big Sur 10.16
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] lubridate_1.7.10 forcats_0.5.1    stringr_1.4.0    dplyr_1.0.6     
    ##  [5] purrr_0.3.4      readr_1.4.0      tidyr_1.1.3      tibble_3.1.2    
    ##  [9] ggplot2_3.3.3    tidyverse_1.3.1 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.1  xfun_0.23         haven_2.4.1       colorspace_2.0-1 
    ##  [5] vctrs_0.3.8       generics_0.1.0    htmltools_0.5.1.1 yaml_2.2.1       
    ##  [9] utf8_1.2.1        rlang_0.4.11      pillar_1.6.1      glue_1.4.2       
    ## [13] withr_2.4.2       DBI_1.1.1         dbplyr_2.1.1      modelr_0.1.8     
    ## [17] readxl_1.3.1      lifecycle_1.0.0   munsell_0.5.0     gtable_0.3.0     
    ## [21] cellranger_1.1.0  rvest_1.0.0       evaluate_0.14     labeling_0.4.2   
    ## [25] knitr_1.33        fansi_0.4.2       highr_0.9         broom_0.7.6      
    ## [29] Rcpp_1.0.6        scales_1.1.1      backports_1.2.1   jsonlite_1.7.2   
    ## [33] farver_2.1.0      fs_1.5.0          hms_1.1.0         digest_0.6.27    
    ## [37] stringi_1.6.2     grid_4.0.5        cli_2.5.0         tools_4.0.5      
    ## [41] magrittr_2.0.1    crayon_1.4.1      pkgconfig_2.0.3   ellipsis_0.3.2   
    ## [45] xml2_1.3.2        reprex_2.0.0      assertthat_0.2.1  rmarkdown_2.8    
    ## [49] httr_1.4.2        rstudioapi_0.13   R6_2.5.0          compiler_4.0.5
