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
library(ggridges)

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
city_100_df = tibble(
  file = list.files("100_cities_data")) %>% 
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

Select pm2.5 AQI during the lockdown period (Feb-Apr) for both 2019 and
2020.

``` r
pm25_2020 = 
  city_100_df %>% 
  filter(date > "2020-01-31" & date < "2020-05-01") %>% 
  rename(pm25_2020 = pm25) %>% 
  mutate(date = format(date, format = "%m-%d")) %>% 
  select(city, date, pm25_2020)
  
pm25_2019 = 
  city_100_df %>% 
  filter(date > "2019-01-31" & date < "2019-05-01") %>% 
  rename(pm25_2019 = pm25) %>% 
  mutate(date = format(date, format = "%m-%d")) %>% 
  select(city, date, pm25_2019)
```

Calculate daily pm2.5 AQI differences between 2019 and 2020 for each
city.

``` r
pm25_diff = 
  left_join(pm25_2020, pm25_2019, by = c("city", "date")) %>% 
  drop_na() %>% 
  mutate(pm25_diff = pm25_2019 - pm25_2020) %>% 
  group_by(city) %>% 
  summarize(mean_diff = mean(pm25_diff, na.rm = T))

pm25_diff
```

    ## # A tibble: 100 × 2
    ##    city      mean_diff
    ##    <chr>         <dbl>
    ##  1 Anyang        45.5 
    ##  2 Baoding       28.4 
    ##  3 Baotou         6.53
    ##  4 Beijing       16.0 
    ##  5 Cangzhou      23.3 
    ##  6 Changchun      1.18
    ##  7 Changde        1.61
    ##  8 Changsha       4.03
    ##  9 Changzhou     27.1 
    ## 10 Chengdu       10.5 
    ## # … with 90 more rows

Load the gdp and population dataset and join it to `pm25_diff`.

``` r
gdp_pop_df = 
  read_csv("data/gpd_and_popluation.csv") %>% 
  janitor::clean_names() %>% 
  mutate(pop_million = population_thousand / 1000,
         gdp_ln = log(gdp_billion, base = exp(1))) %>% 
  select(city, gdp_ln, pop_million)
```

    ## Rows: 100 Columns: 4

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): city
    ## dbl (3): rank, GDP_billion, population_thousand

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df = left_join(pm25_diff, gdp_pop_df) 
```

    ## Joining, by = "city"

Fit linear models

``` r
fit = lm(mean_diff ~gdp_ln + pop_million, data = df)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = mean_diff ~ gdp_ln + pop_million, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -21.0218  -9.5258  -0.4169   8.8149  30.9338 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)   6.7908    12.5002   0.543    0.588
    ## gdp_ln        1.4845     2.2686   0.654    0.514
    ## pop_million  -0.1217     0.3490  -0.349    0.728
    ## 
    ## Residual standard error: 11.16 on 95 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.004806,   Adjusted R-squared:  -0.01615 
    ## F-statistic: 0.2294 on 2 and 95 DF,  p-value: 0.7955

Load weather data for 10 representative cities.

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("CHM00054511", "CHM00058362", "CHM00050953", "CHM00054342", "CHM00055591", "CHM00056294", "CHM00056778", "CHM00059287", "CHM00057036", "CHM00057494"),
    var = c("PRCP", "TAVG"), 
    date_min = "2020-02-01",
    date_max = "2020-04-30") %>%
  mutate(
    name = recode(
      id, 
      CHM00054511 = "Beijing", 
      CHM00058362 = "Shanghai",
      CHM00050953 = "Harbin",
      CHM00054342 = "Shenyang",
      CHM00055591 = "Lasa",
      CHM00056294 = "Chengdu",
      CHM00056778 = "Kunming",
      CHM00059287 = "Guangzhou",
      CHM00057036 = "Xian",
      CHM00057494 = "Wuhan"),
    tavg = tavg / 10,
    prcp = prcp / 10) %>%
  select(-id) %>% 
  rename(city = name) %>% 
  relocate(city)
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/CHM00054511.dly

    ## date created (size, mb): 2021-11-25 17:49:10 (1.703)

    ## file min/max dates: 1945-10-01 / 2021-11-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/CHM00058362.dly

    ## date created (size, mb): 2021-11-25 17:46:32 (0.736)

    ## file min/max dates: 1991-01-01 / 2021-11-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/CHM00050953.dly

    ## date created (size, mb): 2021-11-25 18:39:26 (1.73)

    ## file min/max dates: 1951-01-01 / 2021-11-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/CHM00054342.dly

    ## date created (size, mb): 2021-11-25 18:39:30 (1.716)

    ## file min/max dates: 1951-01-01 / 2021-11-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/CHM00055591.dly

    ## date created (size, mb): 2021-11-25 18:39:34 (1.593)

    ## file min/max dates: 1955-01-01 / 2021-11-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/CHM00056294.dly

    ## date created (size, mb): 2021-11-25 18:39:37 (1.398)

    ## file min/max dates: 1951-01-01 / 2003-12-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/CHM00056778.dly

    ## date created (size, mb): 2021-11-25 18:39:41 (1.739)

    ## file min/max dates: 1942-07-01 / 2021-11-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/CHM00059287.dly

    ## date created (size, mb): 2021-11-25 18:39:45 (1.718)

    ## file min/max dates: 1945-11-01 / 2021-11-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/CHM00057036.dly

    ## date created (size, mb): 2021-11-25 18:39:49 (1.482)

    ## file min/max dates: 1951-01-01 / 2008-12-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/CHM00057494.dly

    ## date created (size, mb): 2021-11-25 18:39:53 (1.686)

    ## file min/max dates: 1951-01-01 / 2021-11-30

``` r
weather_df
```

    ## # A tibble: 900 × 4
    ##    city    date        prcp  tavg
    ##    <chr>   <date>     <dbl> <dbl>
    ##  1 Beijing 2020-02-01   1     1.6
    ##  2 Beijing 2020-02-02   0.3  -2.8
    ##  3 Beijing 2020-02-03   0    -2.6
    ##  4 Beijing 2020-02-04   0    -4.1
    ##  5 Beijing 2020-02-05   2.8  -6  
    ##  6 Beijing 2020-02-06   1.3  -6.1
    ##  7 Beijing 2020-02-07   0    -4.4
    ##  8 Beijing 2020-02-08   0    -0.6
    ##  9 Beijing 2020-02-09   0    -0.5
    ## 10 Beijing 2020-02-10   0     1.8
    ## # … with 890 more rows
