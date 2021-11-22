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
