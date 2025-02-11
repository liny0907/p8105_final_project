barGraph + boxPlot + line Charts
================
Weiheng Zhang
2021/11/19

``` r
library(tidyverse)
library(lubridate)
library(dplyr)
library(p8105.datasets)
library(leaflet)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

``` r
city_files = list.files("30_cities_data")
onehundrd_city_files = list.files("100_cities_data")
```

Time period we are interested in

``` r
period_18 = interval(ymd("2018-02-01"), ymd("2018-04-30"))
period_19 = interval(ymd("2019-02-01"), ymd("2019-04-30"))
period_20 = interval(ymd("2020-02-01"), ymd("2020-04-30"))
period_21 = interval(ymd("2021-02-01"), ymd("2021-04-30"))
```

The daily mean PM2.5 AQI from Feb to Aprl of year 2019 and year 2020 in
each city.

``` r
city_period_meanPM25 = 
  tibble(city = character(),
         mean_19 = numeric(),
         mean_20 = numeric(),
         mean_diff = numeric())
```

``` r
for (city_file in city_files) {
  #print(city_file)
  
  path = str_c("30_cities_data/", city_file)
  city = strsplit(city_file, split = '-')[[1]][1]
  
  cityAir = read_csv(path) %>% 
    mutate(date = as.Date(date, "%Y/%m/%d")) %>%
    arrange(date)
  
  cityAir_19 = cityAir %>% 
    filter(date %within% period_19) 
  
  cityAir_20 = cityAir %>% 
    filter(date %within% period_20)
  
  mean_19 = mean(cityAir_19$pm25, na.rm = T)
  mean_20 = mean(cityAir_20$pm25, na.rm = T)
  mean_diff = mean_20 - mean_19
  
  city_period_meanPM25 = 
    city_period_meanPM25 %>% 
    add_row(city = city, 
            mean_19 = mean_19, 
            mean_20 = mean_20, 
            mean_diff = mean_diff)
}
```

``` r
city_period_meanPM25 = 
  city_period_meanPM25 %>% 
  mutate(
    city = paste(
      toupper(substring(city, 1, 1)), 
      substring(city, 2), 
      sep = ""),
    city = fct_reorder(city, mean_diff, .desc = T))
```

``` r
city_period_meanPM25 %>% 
  arrange(mean_diff) %>% 
  ggplot() +
  geom_bar(
    aes(x = mean_diff, y = city, fill = city), 
  stat = "identity") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme(legend.position = "none") +
  labs(
    title = "Feb-Aprl Daily mean PM2.5 AQI Difference, 2020 minus 2019",
    x = "PM25 AQI Difference",
    y = "City")
```

![](initial_analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#write.csv(x = city_period_meanPM25,file = "data.csv")
```

Now we will see how the distribution of daily PM25 AQI differ between
time period 2019 Feb-Aprl and 2020 Feb-Aprl.

``` r
city_PM25_Distribution = tibble()

for (city_file in city_files) {

  path = str_c("30_cities_data/", city_file)
  city = strsplit(city_file, split = '-')[[1]][1]
  
  cityAir = read_csv(path) %>% 
    mutate(date = as.Date(date, "%Y/%m/%d")) %>%
    arrange(date)
  
  city_19 = cityAir %>% 
    filter(date %within% period_19) %>% 
    mutate(period = "2019Feb-Aprl",
           day = format(date,"%m-%d"),
           city = city) %>% 
    relocate(city, period, day)
  
  #add a fake date "2019-02-29" with all AQI values as NA
  city_19 = 
    city_19 %>% 
    add_row(city = city, 
            period = "2019Feb-Aprl", 
            day = "02-29") %>% 
    mutate(day = as.factor(day))
  
  
  city_20 = cityAir %>% 
    filter(date %within% period_20) %>% 
    mutate(period = "2020Feb-Aprl",
           day = format(date,"%m-%d"),
           day = as.factor(day),
           city = city) %>% 
    relocate(city, period, day)
  
  city_PM25_Distribution = rbind(city_PM25_Distribution, city_19)
  city_PM25_Distribution = rbind(city_PM25_Distribution, city_20)
}
```

``` r
city_PM25_Distribution = 
  city_PM25_Distribution %>% 
  mutate(period = factor(period, levels = c("2020Feb-Aprl", "2019Feb-Aprl")),
         city = paste(
           toupper(substring(city, 1, 1)), 
           substring(city, 2), 
           sep = ""))

city_PM25_Distribution %>% 
  group_by(city, period) %>% 
  ggplot(aes(y = city, x = pm25, fill = period)) + 
  geom_boxplot() + 
  scale_fill_hue(direction = -1) +
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 15,
    position = position_dodge(width = 0.75)) +
  labs(
    title = "Daily PM25 AQI Distribution, 2019 and 2020 Feb-Aprl",
    xlab = "Daily PM25 AQI")
```

    ## Warning: Removed 60 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 60 rows containing non-finite values (stat_summary).

![](initial_analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Line chart of daily pm25 AQI changes for all 30 cities. Note that year
2019 does not have the date “Feb 29”, but year 2020 does.

``` r
city_PM25_Distribution %>% 
  ggplot(aes(x = day, y = pm25, color = period)) + 
  geom_line(aes(group = period), size = 0.8) + 
  #geom_point() +
  scale_color_hue(direction = -1) +
  ylim(0, 400) +
  labs(
    title = "Daily PM25 AQI Starting From Feb 1 to Aprl 30",
    x = "Day",
    y = "Daily PM25 AQI",
    color = "year period") +
  facet_wrap(~city, nrow = 10) +
  scale_x_discrete(breaks = c("02-01", "02-11", "02-21", 
                              "03-01", "03-11", "03-21", 
                              "04-01", "04-11", "04-21")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
```

![](initial_analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
city_4year_meanPM25 = 
  tibble(city = character(),
         mean_18 = numeric(),
         mean_19 = numeric(),
         mean_20 = numeric(),
         mean_21 = numeric())
```

A bar graph of mean PM2.5 AQI from Feb to Apr in the past four years in
30 representative cities.

``` r
for (city_file in city_files) {
  #print(city_file)
  
  path = str_c("30_cities_data/", city_file)
  city = strsplit(city_file, split = '-')[[1]][1]
  
  cityAir = read_csv(path) %>% 
    mutate(date = as.Date(date, "%Y/%m/%d")) %>%
    arrange(date)
  
  cityAir_18 = cityAir %>% 
    filter(date %within% period_18)
  
  cityAir_19 = cityAir %>% 
    filter(date %within% period_19) 
  
  cityAir_20 = cityAir %>% 
    filter(date %within% period_20)
  
  cityAir_21 = cityAir %>% 
    filter(date %within% period_21)
  
  mean_18 = mean(cityAir_18$pm25, na.rm = T)
  mean_19 = mean(cityAir_19$pm25, na.rm = T)
  mean_20 = mean(cityAir_20$pm25, na.rm = T)
  mean_21 = mean(cityAir_21$pm25, na.rm = T)
  
  city_4year_meanPM25 = 
    city_4year_meanPM25 %>%
    add_row(city = city,
            mean_18 = mean_18,
            mean_19 = mean_19, 
            mean_20 = mean_20, 
            mean_21 = mean_21)
}
```

``` r
city_4year_meanPM25 = 
  city_4year_meanPM25 %>% 
  mutate(city = factor(city)) %>% 
  pivot_longer(
    mean_18:mean_21,
    values_to = "mean",
    names_to = "years"
  )
```

``` r
city_4year_meanPM25 %>% 
  ggplot() +
  geom_bar(
    aes(y = years, x = mean, fill = years), 
  stat = "identity") +
  facet_wrap(~city, nrow = 5) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme(legend.position = "none") +
  labs(
    title = "A bar graph of mean PM2.5 AQI from Feb to Apr in the past four years",
    x = "PM25 AQI Mean",
    y = "City")
```

    ## Warning: Removed 1 rows containing missing values (position_stack).

![](initial_analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
