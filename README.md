
<!-- README.md is generated from README.Rmd. Please edit that file -->

# suddengains-simulation

<!-- badges: start -->
<!-- badges: end -->

The goal of suddengains-simulation is to …

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
#> ✓ tibble  3.1.0     ✓ dplyr   1.0.5
#> ✓ tidyr   1.1.3     ✓ stringr 1.4.0
#> ✓ readr   1.4.0     ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(patchwork)
library(here)
#> here() starts at /Users/milanwiedemann/Projects/suddengains-simulation
source(here("scripts/tx_sim_fun.R"))
```

``` r
df_lin_01_long_jf <- sim_tx(n = 100, tp = 12, 
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

plot_lin_01_long_jf <- df_lin_01_long_jf %>%
  ggplot(aes(x = factor(time), y = value, group = id)) +
  geom_line(alpha = .3, colour = "blue") +
  geom_point(alpha = .3, size = .5) +
  labs(x = "Session", y = "BDI-II")

df_lin_01_long_jt <- sim_tx(n = 100, tp = 12, 
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

plot_lin_01_long_jt <- df_lin_01_long_jt %>%
  ggplot(aes(x = factor(time), y = value, group = id)) +
  geom_line(alpha = .3, colour = "blue") +
  geom_point(alpha = .3, size = .5) +
  labs(x = "Session", y = "BDI-II")


plot_lin_01_long_jf + plot_lin_01_long_jt
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

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
#> Warning: Removed 22 row(s) containing missing values (geom_path).
#> Warning: Removed 193 rows containing missing values (geom_point).
#> Warning: Removed 22 row(s) containing missing values (geom_path).
#> Warning: Removed 193 rows containing missing values (geom_point).
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
