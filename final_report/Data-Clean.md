Final Project Data Clean
================
Ziqi Zhou
12/02/2019

### Clean Data

``` r
fire = read_csv("./data/fire_0515.csv")
```

    ## Warning: 660575 parsing failures.
    ##   row    col expected actual                   file
    ## 72832 county a double Mohave './data/fire_0515.csv'
    ## 72833 county a double Mohave './data/fire_0515.csv'
    ## 72834 county a double Mohave './data/fire_0515.csv'
    ## 72835 county a double Mohave './data/fire_0515.csv'
    ## 72836 county a double Mohave './data/fire_0515.csv'
    ## ..... ...... ........ ...... ......................
    ## See problems(...) for more details.

select useful columns

``` r
tidy_fire = 
  fire %>% 
  separate(cont_time, into = c("cont_hour","cont_min") ,sep = 2) %>% 
  separate(discovery_time, into = c("disc_hour","disc_min") ,sep = 2) %>% 
  mutate(cont_hour = as.numeric(cont_hour),
         cont_min = as.numeric(cont_min),
         disc_hour = as.numeric(disc_hour),
         disc_min = as.numeric(disc_min))
```

calculate duration

``` r
state.abb = append(state.abb, c("DC", "PR"))
state.name = append(state.name, c("District of Columbia", "Puerto Rico"))

tidy_fire = 
  tidy_fire %>% 
  # change julian days
  mutate(discovery_date = as.Date(discovery_date - 2458014.5, origin = '2017-09-18'),
         cont_date = as.Date(cont_date - 2458014.5, origin = '2017-09-18'),
         duration_day = as.numeric(difftime(cont_date, discovery_date))
          ) %>% 
  mutate(
    duration_hour = cont_hour - disc_hour,
    duration_min = cont_min - disc_min,
    duration = duration_day / 60 + duration_hour * 60 + duration_min
  ) %>% 
  select(-duration_day, -duration_hour,-duration_min) %>% 
  mutate(fips_name = tolower(fips_name),
         state = fct_inorder(state),
         fire_size_class = fct_inorder(fire_size_class),
         region = state.name[match(state, state.abb)],
         stat_cause_descr = as.factor(stat_cause_descr),
         srat_cause_descr = relevel(stat_cause_descr,ref = "Missing/Undefined"))
```
