test
================
FEI SUN
2021/11/19

\#clean data

``` r
#这个是合并三十个城市pm25数据的文件
city_30_df_pm25 = tibble(
  file = list.files("30_cities_data")) %>% 
  mutate(
    city = str_remove(file, "-air-quality.csv"),
    path = str_c("30_cities_data/", file),
    data = map(path, read_csv)
  ) %>% 
  unnest(data) %>% 
  select(-file, -path) %>% 
  mutate(
    city = str_to_title(city),
    date = as.Date(date, format = "%Y/%m/%d"))%>% 
  select(city,date,pm25)

#这个是合并2020年三十个城市pm25数据的文件
pm25_2020 = 
  city_30_df_pm25 %>% 
  filter(date > "2020-01-31" & date < "2020-05-01") %>% 
  mutate(date = format(date, format = "%y-%m-%d")) %>% 
  select(city, date, pm25)

#这个是合并2019年三十个城市pm25数据的文件
pm25_2019 = 
  city_30_df_pm25 %>% 
  filter(date > "2019-01-31" & date < "2019-05-01") %>% 
  mutate(date = format(date, format = "%y-%m-%d")) %>% 
  select(city, date, pm25)

#这个是合并2018年三十个城市pm25数据的文件
pm25_2018 = 
  city_30_df_pm25 %>% 
  filter(date > "2018-01-31" & date < "2018-05-01") %>% 
  mutate(date = format(date, format = "%y-%m-%d")) %>% 
  select(city, date, pm25)

#这个是合并2017年三十个城市pm25数据的文件
pm25_2017 = 
  city_30_df_pm25 %>% 
  filter(date > "2017-01-31" & date < "2017-05-01") %>% 
  mutate(date = format(date, format = "%y-%m-%d")) %>% 
  select(city, date, pm25)

#这个是合并2017 2018 2019 三年三十个城市pm25数据的文件
pm25_171819= rbind(pm25_2017,pm25_2018,pm25_2019)

#这个是合并2019 2020 两年三十个城市pm25数据的文件
pm25_1920= rbind(pm25_2020,pm25_2019)
```

# chi-squared test: figure out if there is an association between air quality level and geographical locations.

Are cities and air quality level dependent at 5% level of significance?
In other words, given the data collected above, is there a relationship
between the cities and the level of air quality that they have obtained?

Null hypothesis (H0): the air quality level and the different cities
variables of the contingency table are independent. Alternative
hypothesis (H1): the air quality level and the different cities
variables of the contingency table are dependent

``` r
#按等级划分 图片在Google doc里面
city_PM25= pm25_1920 %>%
  drop_na() %>%
  select(city,pm25)%>%
  mutate(level=as.character(pm25))%>%
  mutate(
    level = case_when(
      pm25 <= 50 ~ 'Good',
      pm25 <= 100 ~ 'Moderate',
      pm25 <= 150 ~ 'Unhealthy for Sensitive People',
      pm25 <= 200 ~ ' Unhealthy',
      pm25 <= 300 ~ 'Very Unhealthy',
      pm25 <= 500 ~ 'Hazardous'))%>%
  arrange(city,level,pm25)

city_level=
  city_PM25%>%
  group_by(city,level)%>%
  summarise(n=n())%>%
  pivot_wider(names_from = "level", values_from = "n")
```

    ## `summarise()` has grouped output by 'city'. You can override using the `.groups` argument.

``` r
#把na 缺失的数据换乘0
city_level[is.na(city_level)]= 0
knitr::kable(city_level)
```

| city         | Unhealthy | Good | Moderate | Unhealthy for Sensitive People | Very Unhealthy | Hazardous |  NA |
|:-------------|----------:|-----:|---------:|-------------------------------:|---------------:|----------:|----:|
| Beijing      |        38 |   35 |       50 |                             50 |              6 |         0 |   0 |
| Changchun    |        29 |    2 |       54 |                             74 |             13 |         5 |   0 |
| Changsha     |        33 |    3 |       37 |                            103 |              1 |         0 |   0 |
| Chengdu      |        43 |    1 |       39 |                             94 |              0 |         0 |   0 |
| Chongqing    |        33 |    0 |       46 |                             98 |              0 |         0 |   0 |
| Fuzhou       |         2 |   18 |      101 |                             56 |              0 |         0 |   0 |
| Guangzhou    |         6 |   24 |      110 |                             37 |              0 |         0 |   0 |
| Guiyang      |         6 |    0 |       77 |                             94 |              0 |         0 |   0 |
| Harbin       |        25 |    7 |       63 |                             66 |             13 |         2 |   1 |
| Hefei        |        26 |    3 |       61 |                             86 |              1 |         0 |   0 |
| Jinan        |        53 |    1 |       35 |                             80 |              8 |         0 |   0 |
| Kunming      |         4 |    1 |      101 |                             71 |              0 |         0 |   0 |
| Lanzhou      |         3 |    0 |       50 |                            124 |              0 |         0 |   0 |
| Lhasa        |         0 |   91 |       86 |                              0 |              0 |         0 |   0 |
| Nanchang     |        21 |    1 |       58 |                             97 |              0 |         0 |   0 |
| Nanjing      |        31 |    0 |       56 |                             87 |              0 |         0 |   0 |
| Nanning      |         7 |   13 |      112 |                             45 |              0 |         0 |   0 |
| Shanghai     |        19 |    5 |       82 |                             67 |              3 |         1 |   0 |
| Shenyang     |        47 |    1 |       56 |                             65 |              8 |         0 |   0 |
| Shenzhen     |         3 |    2 |      129 |                             43 |              0 |         0 |   0 |
| Shijiazhuang |        50 |    0 |       34 |                             70 |             19 |         4 |   0 |
| Suzhou       |        44 |    0 |       51 |                             80 |              2 |         0 |   0 |
| Taiyuan      |        45 |    3 |       54 |                             62 |             13 |         0 |   0 |
| Tianjin      |        45 |   19 |       56 |                             49 |              7 |         1 |   0 |
| Wuhan        |        47 |    1 |       26 |                            102 |              0 |         0 |   0 |
| Wulumuqi     |        41 |    6 |       79 |                             34 |             13 |         4 |   0 |
| Xian         |        44 |    0 |       27 |                             92 |             14 |         0 |   0 |
| Xining       |         8 |    0 |       44 |                            125 |              0 |         0 |   0 |
| Yinchuan     |        20 |    2 |      107 |                             48 |              0 |         0 |   0 |
| Zhengzhou    |        51 |    2 |       29 |                             83 |             10 |         2 |   0 |

``` r
#这个我在想想因为直接用table换matrix 前面index number去除不了 就做不了test
airquality_level=read.csv("test (Fei)/city_air_quality_level.csv", row.names = 1 )
#chi test
chisq.test(airquality_level, simulate.p.value = TRUE)
```

    ## 
    ##  Pearson's Chi-squared test with simulated p-value (based on 2000
    ##  replicates)
    ## 
    ## data:  airquality_level
    ## X-squared = 2359.7, df = NA, p-value = 0.0004998

``` r
qchisq(0.05, 174, lower.tail=TRUE) 
```

    ## [1] 144.494

For a Chi-square test, the p-value(0.0004998) that is less than 0.05
significance level. We can reject the null hypothesis (H0) and indicates
there is no evidence to conclude that the air quality level and the
different cities variables of the contingency table are independent and
there is a relationship between them.

# Two-Sample Paired T-test:figure out whether the true mean difference between the average number of AQI (pm25) for each 30 cities from February to April 2020 and the average number between 2017-2019 is equal to zero

Null hypothesis (H0): The true mean difference between the average
number of AQI (pm25) for each 30 cities from February to April 2020 and
the average number between 2017-2019 is equal to zero. Alternative
hypothesis (H1): The true mean difference between the average number of
AQI (pm25) for each 30 cities from February to April 2020 and the
average number between 2017-2019 is not equal to zero.

``` r
#算2020年的mean
mean_2020=pm25_2020%>%
  drop_na() %>% 
  select(-date)%>%
  group_by(city)%>%
  summarise(mean=mean(pm25))%>%
  select(mean)%>%
  pull()

#算17 18 19年的总mean
mean_171819=pm25_171819%>%
  drop_na() %>% 
  select(-date)%>%
  group_by(city)%>%
  summarise(mean=mean(pm25))%>%
  select(mean) %>%
  pull()

t.test(mean_2020, mean_171819, paired = T)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  mean_2020 and mean_171819
    ## t = -11.116, df = 29, p-value = 5.679e-12
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -23.87243 -16.45294
    ## sample estimates:
    ## mean of the differences 
    ##               -20.16268

``` r
qt(0.05, 29)
```

    ## [1] -1.699127

For a Two-Sample Paired T-test, the p-value(5.679e-12 that is less than
0.05 significance level. We can reject the null hypothesis (H0) and
indicates there is no evidence to conclude that the true mean difference
between the average number of AQI (pm25) for each 30 cities from
February to April 2020 and the average number between 2017-2019 is equal
to zero and there are different mean for each 30 cities between 2020 and
2017-2019.
