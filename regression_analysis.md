Regression Analysis
================
Lin Yang
11/20/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.3     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

Load and clean air quality datasets for 100 cities.

``` r
df = tibble(
  file = list.files("100_cities_data")[-22]) %>% 
  mutate(
    city = str_remove(file, "-air-quality.csv"),
    path = str_c("100_cities_data/", file),
    data = map(path, read_csv)
  ) %>% 
  unnest(data) %>% 
  select(-file, -path) %>% 
  mutate(
    city = str_to_title(city),
    date = as.Date(date, format = "%Y/%m/%d"))
```

Select pm2.5 data during the lockdown period (Feb-Apr) for both 2019 and
2020.

``` r
pm25_2020_mean = 
  df %>% 
  filter(date > "2020-01-31" & date < "2020-05-01") %>% 
  group_by(city) %>% 
  summarize(mean_pm25_2020 = mean(pm25, na.rm = T))
 
pm25_2019_mean = 
  df %>% 
  filter(date > "2019-01-31" & date < "2019-05-01") %>% 
  group_by(city) %>% 
  summarize(mean_pm25_2019 = mean(pm25, na.rm = T))

pm25_diff = 
  left_join(pm25_2020_mean, pm25_2019_mean) %>% 
  mutate(diff = mean_pm25_2019 - mean_pm25_2020)
```

    ## Joining, by = "city"

``` r
pm25_diff
```

    ## # A tibble: 98 × 4
    ##    city      mean_pm25_2020 mean_pm25_2019   diff
    ##    <chr>              <dbl>          <dbl>  <dbl>
    ##  1 Anyang             135.            179. 44.0  
    ##  2 Baoding            125.            151. 26.1  
    ##  3 Baotou             123.            128.  4.78 
    ##  4 Beijing            101.            117. 15.6  
    ##  5 Cangzhou           113.            134. 21.6  
    ##  6 Changchun          130.            131.  0.970
    ##  7 Changde            114.            116.  2.06 
    ##  8 Changsha           120.            125.  4.98 
    ##  9 Changzhou           97.0           124. 26.6  
    ## 10 Chengdu            120.            130. 10.4  
    ## # … with 88 more rows
