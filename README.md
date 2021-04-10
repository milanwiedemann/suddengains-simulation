
<!-- README.md is generated from README.Rmd. Please edit that file -->

# suddengains-simulation

## Overview

The function `sim_tx()` simulates longitudinal data.

The following parameters are available to define the trajectories:

-   `n`: Numeric, specifying the sample size
-   `tp`: Numeric, specifying the number of repeated measurements
-   `mean_b0`: Numeric, specifying the mean at the first time point
-   `sd_b0`: Numeric, specifying the standard deviation at the first
    time point
-   `low_b0`: Numeric, specifying the lower limit at the first time
    point
-   `up_b0`: Numeric, specifying the upper limit at the first time point
-   `mean_yf`: Numeric, specifying the mean at the last time point
-   `sd_yf`: Numeric, specifying the standard deviation at the last time
    point
-   `mu`: Numeric, specifying the mean autocorrelation
-   `s`: Numeric, specifying the standard deviation of autocorrelations
-   `mu_j`: Numeric, specifying the mean of the jiggle
-   `s_j`: Numeric, specifying the standard deviation of the jiggle
-   `tm`: Numeric, specifying the time point separating the first and
    second piece of the change
-   `mean_ym`: Numeric, specifying the mean at time point “tm”
-   `sd_ym`: Numeric, specifying the standard deviation at time point
    “tm”
-   `jiggle`: Logical, specifying whether to add jiggle (TRUE) or not
    (FALSE)

Additional arguments: - `na_pct`: Numeric, specifying percentage of
missing data from 0 (no missing data) to 1 (all values are missing). -
`sim_method`: String, specifying the underlying trajectories for
simulating data - `return`: String, specifying whether to return a
“long” (one row per time point per ID) or “wide” data set

``` r
# Load packages ----
library(tidyverse)
library(patchwork)
library(here)

# Load simulation function ----
source(here("scripts/tx_sim_fun.R"))
```

# Simulate data

## No change

``` r
# No change
# No jiggle
# Simulate data for n = 100 individuals with 20 repeated measures
df_no_01_long_jf <- sim_tx(n = 100, tp = 20, 
                           mean_b0 = 29.33, sd_b0 = 8.04, 
                           low_b0 = 15, up_b0 = 63, 
                           mean_yf = 15.9, sd_yf = 11.9, 
                           mu = 0, s = 0.2, 
                           mu_j = 0, s_j = 1, 
                           tm = 8, mean_ym = 20, sd_ym = 7, 
                           sim_method = "nochange",
                           jiggle = FALSE, 
                           seed = 1213,  
                           na_pct = .1,
                           return = "long")

# Create plot
plot_no_01_long_jf <- df_no_01_long_jf %>%
  ggplot(aes(x = factor(time), y = value, group = id)) +
  geom_line(alpha = .3, colour = "blue") +
  geom_point(alpha = .3, size = .5) +
  labs(x = "Session", y = "BDI-II")

# No change
# With jiggle
# Simulate data for n = 100 individuals with 20 repeated measures
df_no_01_long_jt <- sim_tx(n = 100, tp = 20, 
                           mean_b0 = 29.33, sd_b0 = 8.04, 
                           low_b0 = 15, up_b0 = 63, 
                           mean_yf = 15.9, sd_yf = 11.9, 
                           mu = 0, s = 0.2, 
                           mu_j = 0, s_j = 1, 
                           tm = 8, mean_ym = 20, sd_ym = 7, 
                           sim_method = "nochange",
                           jiggle = TRUE, 
                           seed = 1213,  
                           na_pct = .1,
                           return = "long")

# Create plot
plot_no_01_long_jt <- df_no_01_long_jt %>%
  ggplot(aes(x = factor(time), y = value, group = id)) +
  geom_line(alpha = .3, colour = "blue") +
  geom_point(alpha = .3, size = .5) +
  labs(x = "Session", y = "BDI-II")

# Plot data
plot_no_01_long_jf + plot_no_01_long_jt
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Linear change

``` r
# Linear change
# No jiggle
# Simulate data for n = 100 individuals with 20 repeated measures
df_lin_01_long_jf <- sim_tx(n = 100, tp = 20, 
                            mean_b0 = 29.33, sd_b0 = 8.04, 
                            low_b0 = 15, up_b0 = 63, 
                            mean_yf = 15.9, sd_yf = 11.9, 
                            mu = 0, s = 0.2, 
                            mu_j = 0, s_j = 5, 
                            tm = 8, mean_ym = 20, sd_ym = 7, 
                            sim_method = "linear",
                            jiggle = FALSE, 
                            seed = 123,  
                            na_pct = .1,
                            return = "long")

# Create plot
plot_lin_01_long_jf <- df_lin_01_long_jf %>%
  ggplot(aes(x = factor(time), y = value, group = id)) +
  geom_line(alpha = .3, colour = "blue") +
  geom_point(alpha = .3, size = .5) +
  labs(x = "Session", y = "BDI-II")


# Linear change
# With jiggle
# Simulate data for n = 100 individuals with 20 repeated measures
df_lin_01_long_jt <- sim_tx(n = 100, tp = 20, 
                        mean_b0 = 29.33, sd_b0 = 8.04, 
                        low_b0 = 15, up_b0 = 63, 
                        mean_yf = 15.9, sd_yf = 11.9, 
                        mu = 0, s = 0.2, 
                        mu_j = 0, s_j = 5, 
                        tm = 8, mean_ym = 20, sd_ym = 7, 
                        sim_method = "linear",
                        jiggle = TRUE, 
                        seed = 123,  
                        na_pct = .1,
                        return = "long")

# Create plot
plot_lin_01_long_jt <- df_lin_01_long_jt %>%
  ggplot(aes(x = factor(time), y = value, group = id)) +
  geom_line(alpha = .3, colour = "blue") +
  geom_point(alpha = .3, size = .5) +
  labs(x = "Session", y = "BDI-II")

# Plot data
plot_lin_01_long_jf + plot_lin_01_long_jt
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Loglinear change

``` r
df_log_01_long_jf <- sim_tx(n = 100, tp = 20, 
                        mean_b0 = 29.33, sd_b0 = 8.04, 
                        low_b0 = 15, up_b0 = 63, 
                        mean_yf = 15.9, sd_yf = 11.9, 
                        mu = 0, s = 0.2, 
                        mu_j = 0, s_j = 1, 
                        tm = 8, mean_ym = 20, sd_ym = 7, 
                        sim_method = "pseudolog",
                        jiggle = FALSE, 
                        seed = 1213,  
                        na_pct = .1,
                        return = "long")

plot_log_01_long_jf <- df_log_01_long_jf %>%
  ggplot(aes(x = factor(time), y = value, group = id)) +
  geom_line(alpha = .3, colour = "blue") +
  geom_point(alpha = .3, size = .5) +
  labs(x = "Session", y = "BDI-II")



df_log_01_long_jt <- sim_tx(n = 100, tp = 20, 
                        mean_b0 = 29.33, sd_b0 = 8.04, 
                        low_b0 = 15, up_b0 = 63, 
                        mean_yf = 15.9, sd_yf = 11.9, 
                        mu = 0, s = 0.2, 
                        mu_j = 0, s_j = 1, 
                        tm = 8, mean_ym = 20, sd_ym = 7, 
                        sim_method = "pseudolog",
                        jiggle = TRUE, 
                        seed = 1213,  
                        na_pct = .1,
                        return = "long")

plot_log_01_long_jt <- df_log_01_long_jt %>%
  ggplot(aes(x = factor(time), y = value, group = id)) +
  geom_line(alpha = .3, colour = "blue") +
  geom_point(alpha = .3, size = .5) +
  labs(x = "Session", y = "BDI-II")


plot_log_01_long_jf + plot_log_01_long_jt
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Identify sudden gains

``` r
df_no_01_long_jf <- df_no_01_long_jf %>% 
  mutate(source = "df_no_01_long_jf")

df_no_01_long_jt <- df_no_01_long_jt %>% 
  mutate(source = "df_no_01_long_jt")


df_lin_01_long_jf <- df_lin_01_long_jf %>% 
  mutate(source = "df_lin_01_long_jf")

df_lin_01_long_jt <- df_lin_01_long_jt %>% 
  mutate(source = "df_lin_01_long_jt")


df_log_01_long_jf <- df_log_01_long_jf %>% 
  mutate(source = "df_log_01_long_jf")

df_log_01_long_jt <- df_log_01_long_jt %>% 
  mutate(source = "df_log_01_long_jt")
```
