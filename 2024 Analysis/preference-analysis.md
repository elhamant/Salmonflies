Preference Analysis
================
Emily
2024-11-08

# This is where I am analysing the preference data

# Prep

## load packages

``` r
library(tidyverse)
library(dplyr)
library(janitor)
library(knitr)
```

## read data

``` r
raw_preference = read_csv("C:\\github\\Salmonflies\\Data\\Used_in_2024_Analysis\\Rawdata_allpreference_prepped.csv")
```

# 7 Days of Conditioning

## Cleaning

``` r
seven_day_preference = raw_preference |>
  clean_names() |>
  separate_wider_delim(cols = "cup", delim = " ", names = c("treatment", "cup"))|>
  filter(conditioning_time == 7)
```

<br>

## Calculate Chi Square statistics

``` r
contingency_table_7 = seven_day_preference |>
  filter(terminated == FALSE) |>
  group_by(treatment)|>
  summarize(count_cottonwood = sum(cottonwood), count_other = sum(other))

contingency_table_7 |>
  kable()
```

| treatment | count_cottonwood | count_other |
|:----------|-----------------:|------------:|
| C-Alder   |               36 |          80 |
| C-Choke   |               30 |           1 |
| C-Dog     |               28 |          14 |
| C-Willow  |               56 |           7 |

``` r
for (treat in unique(contingency_table_7$treatment)) {
  treatment_table <- contingency_table_7 |>
    filter(treatment == treat) |>
    select(-treatment)
  
  assign(paste0("treatment_", gsub(" ", "_", treat)), treatment_table)
  
  chi2_result <- chisq.test(treatment_table)
  
  assign(paste0("chi2_result_", gsub(" ", "_", treat)), chi2_result)
  
  print(paste("Chi-square test result for treatment:", treat))
  print(chi2_result)
}
```

    ## [1] "Chi-square test result for treatment: C-Alder"
    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  treatment_table
    ## X-squared = 16.69, df = 1, p-value = 4.402e-05
    ## 
    ## [1] "Chi-square test result for treatment: C-Choke"
    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  treatment_table
    ## X-squared = 27.129, df = 1, p-value = 1.903e-07
    ## 
    ## [1] "Chi-square test result for treatment: C-Dog"
    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  treatment_table
    ## X-squared = 4.6667, df = 1, p-value = 0.03075
    ## 
    ## [1] "Chi-square test result for treatment: C-Willow"
    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  treatment_table
    ## X-squared = 38.111, df = 1, p-value = 6.683e-10

We measured the leaf preference of salmonflies as the daily location of
individuals on each leaf type. Under seven days of conditioning,
salmonflies preferred to occupy alder leaves over cottonwood leaves
(Chi-square test, P\<0.0001 under 1 df), but preferred cottonwood leaves
over chokecherry, dogwood, and willow leaves Chi-square test, P\<0.0001,
P=0.03075, and P\<0.00001, respectively, under 1 df).

<br>

# 25 Days of Conditioning

## Cleaning

``` r
twentyfive_day_preference = raw_preference |>
  clean_names() |>
  separate_wider_delim(cols = "cup", delim = " ", names = c("treatment", "cup"))|>
  filter(conditioning_time == 25)
```

<br>

## Calculate Chi Square statistics

``` r
contingency_table_25 = twentyfive_day_preference |>
  filter(terminated == FALSE) |>
  group_by(treatment)|>
  summarize(count_cottonwood = sum(cottonwood), count_other = sum(other))

contingency_table_25 |>
  kable()
```

| treatment | count_cottonwood | count_other |
|:----------|-----------------:|------------:|
| C-Alder   |               57 |          63 |
| C-Choke   |              102 |          18 |
| C-Dog     |               70 |          50 |
| C-Willow  |              101 |           6 |

``` r
for (treat in unique(contingency_table_25$treatment)) {
  treatment_table <- contingency_table_25 |>
    filter(treatment == treat) |>
    select(-treatment)
  
  assign(paste0("treatment_", gsub(" ", "_", treat)), treatment_table)
  
  chi2_result <- chisq.test(treatment_table)
  
  assign(paste0("chi2_result_", gsub(" ", "_", treat)), chi2_result)
  
  print(paste("Chi-square test result for treatment:", treat))
  print(chi2_result)
}
```

    ## [1] "Chi-square test result for treatment: C-Alder"
    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  treatment_table
    ## X-squared = 0.3, df = 1, p-value = 0.5839
    ## 
    ## [1] "Chi-square test result for treatment: C-Choke"
    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  treatment_table
    ## X-squared = 58.8, df = 1, p-value = 1.745e-14
    ## 
    ## [1] "Chi-square test result for treatment: C-Dog"
    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  treatment_table
    ## X-squared = 3.3333, df = 1, p-value = 0.06789
    ## 
    ## [1] "Chi-square test result for treatment: C-Willow"
    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  treatment_table
    ## X-squared = 84.346, df = 1, p-value < 2.2e-16

Under twenty-five days of conditioning, there was no difference in the
leaf preference of salmonflies between alder leaves and cottonwood
leaves (Chi-square test, P = .5839 under 1 df), or dogwood and
cottonwood (Chi-square test, p = .06789 under 1 df). Salmonflies
preferred cottonwood leaves over chokecherry and willow leaves
(Chi-square test, P\<0.00001 for both treatments under 1 df). Under both
conditioning periods, there is evidence of a difference in preferred
location of salmonflies, where individuals prefer to occupy cottonwood
leaves over other species, excluding alder.
