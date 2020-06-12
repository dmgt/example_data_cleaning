Example of standardizing, summarizing, and plotting NOAA LCD weather
data
================
Dana Miller

The examples below are exerpts from a scientific manuscript
demonstrating common data manipulation and visualization taaks using the
popular `tidyverse` and `ggplot2` packages in R. My goal is to provide
an accesible introduction to this workflow for users new to RMarkdown
documents, using publically avalible data. This is a simplified version
of relevant steps when processing, summarizing, and presenting data in a
research report or manuscript, demonstrated on one relatively small
dataset that is already in a ‘tidy’ format.

``` r
# Load required libraries
library(lubridate)  # note lubridate::here conflicts with here::here, specify namespace explicitly when used below
library(tidyverse)
library(here)       # package to manage relative file paths
library(skimr)
library(cowplot)    # package for additional plotting options
library(gt)
library(knitr)      # package for creating formatted output
```

### About the data

  - I downloaded outdoor weather data from a nearby active weather
    station for each field site via the National Oceanic and Atmospheric
    Administration’s [Local Climatological Data
    tool](https://www.ncdc.noaa.gov/cdo-web/datatools/lcd) for April
    2017 through October 2019 from three weather stations: Stockton
    Metropolitan Airport (WBAN:23237, for Site 1), Merced Municipal
    Airport (WBAN:23257, for Site 2), and Merced 23 WSW (WBAN:93243 ,
    for Site 3 and 4).
  - This data included hourly measurements for many variables, including
    dry bulb temperature, wet bulb temperature, air speed, and sky
    condition measurements. All weather variable names and decriptions
    from NOAA Local Climatological Data are documented
    [here](https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/LCD_documentation.pdf)
  - The weather files have already been cleaned by converting the
    timezone to UTC, and saving only the variables used for this example
    (temperature, humdidity, and wind
speed)

### Load data

``` r
# Will use convention of `_cleaned` to designate data read in from `/cleaned_data`, but note that each dataset has 
# additional processing below, eg taking hourly averages, merging with metadata, aligning naming 
# conventions between datasets etc
weather_cleaned <- read_rds(here::here("cleaned_data/allWeather.rds"))
```

### Load helper functions

  - One helper function is included below. If there were much longer
    helper functions, they could be stored in a .R script in a dedicated
    subfolder (often named `/src` by convention) and called here.

<!-- end list -->

``` r
filter_hvac_failure <- function(input_df) {
  # Function to remove data from dates during mechanical system failure at one site
  # since this period is not representitive to compare to other data
  
  # Function input is a dataframe with columns 'datetime_utc_hourly' and 'site'
  
      site1_filtered <-  input_df %>%
                        filter(site == "Site1") %>% 
                        filter(datetime_utc_hourly < "2018-08-10" | datetime_utc_hourly > "2019-06-24")
      
      other_sites_filtered <-  input_df %>%
                              filter(site != "Site1") 
      
      compressors_field_study_filtered <- bind_rows(site1_filtered, other_sites_filtered) %>% 
                              arrange(datetime_utc_hourly)
      return(compressors_field_study_filtered)
    }
```

### Data standarization

#### Notes for conventions for all datasets (including weather data)

  - `_hourly` : version of dataset with hourly averages
  - `_info`: version of dataset with the following additional variables
    created:
      - intervention: ‘Baseline’ or ‘Intervention’ (based on dates of
        week fans started operating, in code below)
      - From zone\_metadata file
          - hvac\_zone\_display: eg C1, C2…R1-R6 (C = Commercial, R =
            Residential)
          - space\_occupancy\_type: Residential, Commercial - irregular
            occupancy, Commercial - regular occupancy
      - From transformations of existing data
          - month (numeric) - for functions that filter by month
          - month\_factor (factor) - for boxplots that need factor input
          - year (numeric) - for functions that filter by calender year
          - hour (numeric) - for functions that filter by hour of day (
            0 - 23)
          - hour\_factor(factor) - for boxplots that need factor input
      - Only for weather file
          - site\_display - To combine Site 3 + 4 as one factor for
            graphing since weather files are same (co-located sites)
  - The cleaned file, with the info above appended, will be called
    `*_info`
  - The file with info appended, and additionally summarized to hourly
    averages, will be named `*_hourly` (not `*_hourly_info`) for
    brevity, if it’s hourly the ‘info’ has alreadly been appended :)
  - Dataframes that have additionally been filtered to remove the HVAC
    failure period for data from Site 1 (ie dataframe has full
    timeperiod for site 2-4 data, and Site 1 data only putside of HVAC
    failure period) are labeled as ‘filtered’

#### Standardize weather data

##### Add metadata to cleaned data

``` r
# Append additional metadata info to cleaned weather data 
weather_info  <- weather_cleaned %>%
   filter(datetime_utc<"2019-10-31 00:00:00")%>%  # Filter for period during field study ie July 2017 - Oct 2020
   filter(datetime_utc>"2017-07-18 00:00:00")%>%  
    # select relevant columns
     select(datetime_utc, temperature_c, humidity_rh, wind_speed_mps, unique_datastream_id, site) %>% 
  mutate(intervention = case_when(   
    # Create new column reflecting intervention status at each field site at each timestamp              
                site == "Site1" & datetime_utc <= as_datetime("2018-07-06 00:00:00") ~ "Baseline",
                site == "Site2" & datetime_utc <= as_datetime("2018-07-13 00:00:00") ~ "Baseline",
                site == "Site3" & datetime_utc  <= as_datetime("2018-07-20 00:00:00") ~ "Baseline",
                site == "Site4" & datetime_utc <= as_datetime("2018-07-20 00:00:00") ~ "Baseline",   
                TRUE ~ "Intervention")) %>%  # Logic assumes that any periods that are not baseline are intervention  
   mutate(site_display = case_when(  
     # Create new column called 'site_display', since Sites 3 and 4 share the same weather file           
                site == "Site1" ~ "Site 1",
                site == "Site2" ~ "Site 2",           
                site == "Site3" ~ "Site 3 and 4",
                site == "Site4" ~ "Site 3 and 4")) %>%
  mutate(month_factor = as.factor(lubridate::month(datetime_utc)),  # Must be formatted as factor for plot formatting
         year_factor = as.factor(lubridate::year(datetime_utc)),
         hour_factor = as.factor(lubridate::year(datetime_utc))) %>%
  # Additional columsn below formatted as numeric for subsequent numeric-based filtering that can't take factors
  mutate(month = lubridate::month(datetime_utc),
         year = lubridate::year(datetime_utc),
         yday = lubridate::yday(datetime_utc),
         wday = lubridate::wday(datetime_utc),
         hour = lubridate::hour(datetime_utc)) 
```

##### Create hourly summary and add dew point, vapor pressure, and absolute humidity conversion

``` r
weather_hourly <- weather_info %>% 
     group_by(datetime_utc_hourly = floor_date(datetime_utc, "hour"), unique_datastream_id,  # Setting time interval 
              site, intervention, site_display, month_factor, year_factor, hour_factor,   
              month, year, hour, yday, wday) %>%                     # Selecting columns to track in grouping
    # summarize hourly temperature, humidity, and wind speed values
     mutate(temperature_c_hourly = mean(temperature_c, na.rm=TRUE),  
            humidity_rh_hourly = mean(humidity_rh, na.rm=TRUE),
            wind_speed_mps_hourly = mean(wind_speed_mps, na.rm=TRUE)) %>%
     ungroup() %>% 
  # select columns to keep 
     select(datetime_utc_hourly, unique_datastream_id, site, temperature_c_hourly, humidity_rh_hourly,   
         wind_speed_mps_hourly, intervention, site_display, month_factor, year_factor, hour_factor, 
         month, year, hour, yday, wday) %>% 
  distinct() %>%  # Only retain distinct columns 
                 # (ie drop any duplicate identical rows with same timestamp and same values)
   mutate(  # see notes below on data conversion
     dew_point_temperature_c_hourly = (243.12*(log(humidity_rh_hourly/100) +17.62*temperature_c_hourly/(243.12+temperature_c_hourly)))/(17.62-(log(humidity_rh_hourly/100) +17.62*temperature_c_hourly/(243.12+temperature_c_hourly))),
         vapour_p_hourly = 6.116441*10^(7.59138*temperature_c_hourly/(temperature_c_hourly+240.726)) * humidity_rh_hourly/100, # in hPa units
         humidity_ah_hourly = 2.16679 * (vapour_p_hourly*100)/(273.15+temperature_c_hourly)) # in g/m3 units
  
# Notes on estimating absolute humidity
# Based on reference from Sonja here: https://github.com/CenterForTheBuiltEnvironment/fans-field-tstats/blob/master/code/hamilton_rh_analysis.Rmd 
#approximation of dew point temperature if you know the observed temperature and relative humidity, The dew point is calculated according to the following formula:
#Ts = (b * α(T,RH)) / (a - α(T,RH))
#where:
#    Ts is the dew point;
#    T is the temperature;
#    RH is the relative humidity of the air;
#    a and b are coefficients. For Sonntag90 constant set, a=17.62 and b=243.12°C;
#    α(T,RH) = ln(RH/100) +a*T/(b+T).
  
# Subset weather data with equipment failure period at Site 1 removed  
weather_filtered <- filter_hvac_failure(weather_hourly)
```

##### Save standarized data frames

  - In a lengthy document, the output of the chunks above could be
    saved, and subsequent steps could start with re-loading the saved
    output files if the data standardization does not need to be re-run.

### Summarize data

  - In this example, we are interested in calculating summary variables
    for the warmest months of the year, namely April - October.
  - It is helpful to assign key results to descriptively-named variables
    (eg `baseline_cooling_period_mean_hourly_oat_c`) to directly these
    values in calculations or other outputs
later

#### Across all sites

``` r
# Calculate mean temperature and humdity during April - October, averaged across all sites, per period

##Temperature - overall averages across all sites
baseline_cooling_period_mean_hourly_oat_c <- weather_filtered %>%
                                     filter(month >= 4 & month<= 10) %>%
                                     filter(site != "Site4") %>%   # Avoid double counting values from Site 3 and 4 w/ same weather
                                     filter(intervention == "Baseline") %>%
                                     summarise(mean_hourly_temp_c = mean(temperature_c_hourly, na.rm = T))

intervention_cooling_period_mean_hourly_oat_c <- weather_filtered %>%
                                     filter(month >= 4 & month <= 10) %>%
                                      filter(site != "Site4") %>% 
                                     filter(intervention == "Intervention") %>%
                                     summarise(mean_hourly_temp_c = mean(temperature_c_hourly, na.rm = T))

mean_difference_hourly_oat_c <- round(intervention_cooling_period_mean_hourly_oat_c - baseline_cooling_period_mean_hourly_oat_c, 1)


## Relative humidity -  overall averages across all sites
baseline_cooling_period_mean_hourly_rh  <- weather_filtered %>%
                                     filter(month >= 4 & month<= 10) %>%
                                     filter(site != "Site4") %>%  
                                     filter(intervention == "Baseline") %>%
                                     summarise(mean_hourly_rh = mean(humidity_rh_hourly, na.rm = T))

intervention_cooling_period_mean_hourly_rh <- weather_filtered %>%
                                     filter(month >= 4 & month <= 10) %>%
                                      filter(site != "Site4") %>% 
                                     filter(intervention == "Intervention") %>%
                                      summarise(mean_hourly_rh = mean(humidity_rh_hourly, na.rm = T))

mean_difference_hourly_rh <- round(intervention_cooling_period_mean_hourly_rh  - baseline_cooling_period_mean_hourly_rh,2)
```

``` r
# Calculate range of mean daily high temperatures throughout entire field study duration
 mean_daily_high_temps <- weather_filtered %>%
  filter(month >= 4 & month<= 10) %>%
  filter(site != "Site4") %>% # avoid double counting
  group_by(yday, site) %>%
  summarise(max_daily_temp = max(temperature_c_hourly, na.rm = T),
          mean_daily_temp =mean(temperature_c_hourly, na.rm = T)) %>% 
  ungroup()

# Assign to variables for later use 
mean_daily_high_temps_c_min <-  min(mean_daily_high_temps$max_daily_temp)
mean_daily_high_temps_c_max <- max(mean_daily_high_temps$max_daily_temp)
```

#### Calculate mean values per site, and make a summary table

``` r
# Mean temperatures - each site separately
outdoor_temp_per_site_baseline <- weather_filtered %>%
                                  filter(month >= 4 & month<= 10) %>%
                                  filter(intervention == "Baseline") %>%
                                  group_by(site_display)%>%
                                  summarise(mean_hourly_temp_c_baseline = mean(temperature_c_hourly, na.rm = T)) 

outdoor_temp_per_site_intervention <- weather_filtered %>%
                                     filter(month >= 4 & month<= 10) %>%
                                     filter(intervention == "Intervention") %>%
                                     group_by(site_display)%>%
                                     summarise(mean_hourly_temp_c_intervention = mean(temperature_c_hourly, na.rm = T)) 

### Mean relative humidities - each site separately
outdoor_rh_per_site_baseline <- weather_filtered %>%
                                filter(month >= 4 & month <= 10) %>%
                                filter(intervention == "Baseline") %>%
                                group_by(site_display)%>%
                                summarise(mean_hourly_rh_baseline = mean(humidity_rh_hourly, na.rm = T)) 

outdoor_rh_per_site_intervention  <- weather_filtered %>%
                                     filter(month >= 4 & month <= 10) %>%
                                     filter(intervention == "Intervention") %>%
                                     group_by(site_display)%>%
                                     summarise(mean_hourly_rh_intervention = mean(humidity_rh_hourly, na.rm = T))

weather_df_for_table <- outdoor_temp_per_site_baseline %>% 
  left_join(outdoor_temp_per_site_intervention, by = "site_display") %>%
  mutate(delta_temp_c = mean_hourly_temp_c_intervention - mean_hourly_temp_c_baseline) %>%
  left_join(outdoor_rh_per_site_baseline, by = "site_display") %>%
  left_join(outdoor_rh_per_site_intervention, by = "site_display") %>%
  mutate(delta_rh_pct = mean_hourly_rh_intervention - mean_hourly_rh_baseline)
```

  - The code below creates a summary table, which is currently saved to
    the `/results` folder as an image file, and embedded below.

<!-- end list -->

``` r
weather_summary_table <- weather_df_for_table %>%
  gt() %>%
  tab_header(
    title = "Outdoor environmental conditions per site",
    subtitle = "Hourly means during April - October cooling season"
    ) %>%
  tab_spanner(
    label = html("Air temperature [&deg;C]"),
    columns = vars(mean_hourly_temp_c_baseline, mean_hourly_temp_c_intervention, delta_temp_c)
    ) %>% 
   fmt_number(
    columns = vars(mean_hourly_temp_c_baseline, mean_hourly_temp_c_intervention, delta_temp_c),
    decimals = 1
  ) %>%
  tab_spanner(
    label = "Relative humidity [%]",
    columns = vars(mean_hourly_rh_baseline, mean_hourly_rh_intervention, delta_rh_pct)
    ) %>%  
   fmt_number(
    columns = vars(mean_hourly_rh_baseline, mean_hourly_rh_intervention, delta_rh_pct),
    decimals = 0
  ) %>%
  cols_label(
    site_display = html("Site"),
    mean_hourly_temp_c_baseline =  html("Baseline"),
    mean_hourly_temp_c_intervention = html("Intervention"),
    delta_temp_c = html("&Delta; T"),
    mean_hourly_rh_baseline = html("Baseline"),
    mean_hourly_rh_intervention = html("Intervention"),
    delta_rh_pct = html("&Delta; RH")
  ) %>%
  cols_align( align = "center")  %>%
  tab_source_note(
    source_note = html("Data from NOAA Local Climactic Data monitoring stations WBAN:23237 (Site 1), WBAN:23257 (Site 2), and WBAN:93242 (Sites 3 and 4)"))
```

<img src="/home/trc/example_data_cleaning/results/delta_t_rh_example_table.PNG" width="836" />

### Plot data

``` r
# Draw plots - first temperature, then humdity, and combine both panels with cowplot
temp_plot <- weather_filtered %>%
              ggplot(aes(month_factor, temperature_c_hourly,
                         fill = intervention,
                         group(interaction(intervention, month)))) + 
              geom_boxplot(show.legend = F, outlier.shape = NA) +
              scale_y_continuous(sec.axis = sec_axis(~ . * 1.8 + 32, name = "°F"))+ #turning to Fahrenheit
              #facet_wrap(.~site_display, ncol = 1, nrow = 3) +
  labs(title = "",
       subtitle = "",
         x = "",
         y = "Drybulb temperature °C") +
          theme_bw(base_size = 12) +
  scale_fill_manual(values=c("Baseline"="#F18E3D","Intervention"="#BA5C0E"), labels= c("Baseline", "Intervention"))

rh_humidity_plot <- weather_filtered %>%
              ggplot(aes(month_factor, humidity_rh_hourly,
                         fill = intervention,
                         group(interaction(intervention, month)))) + 
              geom_boxplot(outlier.shape = NA) +
              #facet_wrap(.~site_display, ncol = 1, nrow = 3) +
  labs(title = "",
       subtitle = "",
         x = "Month") +
         ylab("Relative humidity %") +
         theme_bw(base_size = 12) +
  scale_fill_manual(values=c("Baseline"="#F18E3D","Intervention"="#BA5C0E"), 
                    labels= c("Baseline (July 2017 - July 2018)", "Intervention (July 2018 - October 2019)")) +
  theme(legend.position="bottom", legend.title = element_blank()) + guides(color=guide_legend(""))
  
weather_plot <- cowplot::plot_grid(temp_plot, rh_humidity_plot, nrow=2, ncol=1, 
                                   #labels = c('a', 'b'),
                                   align = "v", 
                                   rel_widths = c(1, 1),
                                   rel_heights = c(1, 1.25))

# save results
ggsave("weather_plot.png", plot = weather_plot, 
       path= here("results"),
       dpi = 300, 
       width = 5.5,
       height = 4.25,
       units = "in"
)
```

![](example_data_processing_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Write about the data with variables calculated inline

**Here is some summary text:** During the cooling season months of April
– October, mean daily high temperatures ranged from 13.3 °C to 43.9 °C.
Cooling season mean hourly outdoor temperatures at all three locations
were higher during the intervention period than the baseline period,
with a mean increase of 0.8 °C. Mean hourly relative humidity and
absolute humidity decreased slightly at Sites 1, 3, and 4 during the
intervention compared to the baseline period , but increased at Site 2,
as shown in the table above. Overall, mean measured outdoor temperature
and relative humidity levels at each location during the intervention
period were within 6% of values measured during the baseline period.

Here is what the code to produce this text looks like with values
encoded as
variables:

``` r
During the cooling season months of April – October, mean daily high temperatures ranged from `r  round(mean_daily_high_temps_c_min,1)` °C to `r round(mean_daily_high_temps_c_max,1)` °C. Cooling season mean hourly outdoor temperatures at all three locations were higher during the intervention period than the baseline period, with a mean increase of `r mean_difference_hourly_oat_c` °C. Mean hourly relative humidity and absolute humidity decreased slightly at Sites 1, 3, and 4 during the intervention compared to the baseline period , but increased at Site 2, as shown in the table above. Overall, mean measured outdoor temperature and relative humidity levels at each location during the intervention period were within 6% of values measured during the baseline period.
```
