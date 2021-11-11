P8105 Final Project
================
11/11/2021

### The group members (names and UNIs)

Zhuolun Huang, zh2494  
Fei Sun, fs2757  
Lin Yang, ly2565  
Yihan Qiu, yq2321  
Weiheng Zhang, wz2590

### The tentative project title

The Impact of COVID-19 Lockdowns on Air Quality in Chinese Cities

### The motivation for this project

In order to contain the COVID-19 outbreak, China entered lockdown in
early February 2020, minimizing all industrial, transportation, and
commercial activities. Although the lockdown caused tremendous loss to
the economy, previous research has shown the air condition in the top
four megacities of China improved significantly during the lockdown
period, due to a dramatic decrease in automobile and industrial
emissions. Our motivation is to find out whether “lockdown improves air
quality” is a common phenomenon in Chinese cities including but not only
limited to megacities. We are also interested in how the geographical
location, population, and GDP of each city affect the degree of air
quality improvement, and which pollutant’s daily air quality index(AQI)
was affected the most. AQI is a variable describing the daily level of
air pollution from pollutants such as PM2.5, PM10, NO2. Each pollutant
has its own AQI value for each day.

### The intended final products

We will present interactive graphs and thematic maps showing the degree
of improvement of daily average AQI for multiple pollutants, among
thirty representative Chinese cities during lockdown compared to air
quality data in the same months of the year 2019.  
We will also build interactive graphs showing how the population and GDP
of cities in the same geographical district correlate to the degree of
improvement of daily average AQI.

### The anticipated data sources

[Air Quality Historical Data
Platform](https://aqicn.org/data-platform/register/)  
[List of Chinese prefecture-level cities by
GDP](https://en.wikipedia.org/wiki/List_of_Chinese_prefecture-level_cities_by_GDP)  
[China Population](https://populationstat.com/china/)

### The planned analyses / visualizations

We will make various plots to evaluate how lockdown policies affected
air quality in most Chinese cities, and how population and GDP affected
air quality improvements in these cities. We will also analyze which
cities had the most and the least air quality improvements and figure
out if there is an association between air quality improvements and
geographical locations. Here’re our expected results:

-   A bar graph of mean AQI difference for each air pollutant(PM2.5,
    PM10, SO2, O3, NO2, CO) between 2019 and 2020.  
-   A boxplot of daily PM2.5 AQI from Feb. to Apr. (lockdown period) in
    the past year (2019, 2020) in 10 representative cities.  
-   A bar graph of mean PM2.5 AQI from Feb to Apr in the past three
    years in 10 representative cities.  
-   A line plot of trends in monthly average PM2.5 AQIs from 2019/02 -
    2020/06 in 10 representative cities.  
-   Scatterplots of PM2.5 AQI differences vs. GDP and PM2.5 AQI
    differences vs. population in all cities.  
-   A map of China showing each city’s AQI difference between early 2020
    and 2019.

### Coding challenges

1.  Although we already have accurate data on air quality in each city,
    the data of every 30 cities are independent files, so it is tedious
    to combine the air pollution data of each city into one file.
    Comparing data from different cities could be complicated.  
2.  To evaluate the relationship between population/GDP and air quality
    improvements, we have to manually gather data from the web.  
3.  There are some errors and obscure information in the air quality
    data, so we need to review and investigate the data of each city
    carefully before we proceed.  
4.  It can be time-consuming to generate reader-friendly plots,
    especially plots containing multiple cities.
