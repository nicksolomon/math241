`ggplot2` and `dplyr` review
================
Nick Solomon
February 24, 2017

``` r
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

``` r
library(tidyverse)
library(mosaic)
library(Lahman)
```

Exercise 3.1
------------

### 1

``` r
height_v_father <- ggplot(Galton, aes(height, father)) +
  geom_point() +
  labs(title = "Height vs. father's height", 
       x = "Height (in)", 
       y = "Father's height (in)") +
  theme_bw()
height_v_father
```

![](hw_2_review_files/figure-markdown_github/unnamed-chunk-3-1.png)

### 2

``` r
height_v_father <- height_v_father + facet_wrap(~sex)
height_v_father
```

![](hw_2_review_files/figure-markdown_github/unnamed-chunk-4-1.png)

### 3

``` r
height_v_father <- height_v_father + geom_smooth(method = "lm")
height_v_father
```

![](hw_2_review_files/figure-markdown_github/unnamed-chunk-5-1.png)

Exercise 3.2
------------

### 1

``` r
vol_vs_htemp <- ggplot(RailTrail, aes(volume, hightemp)) +
  geom_point() +
  labs(title = "Volume vs. high temperature",
       x = "Volume (# of crossings)",
       y = "High temperature (°F)") +
  theme_bw()
vol_vs_htemp
```

![](hw_2_review_files/figure-markdown_github/unnamed-chunk-6-1.png)

### 2

``` r
labels <- c("1" = "Weekday", "0" = "Weekend")
vol_vs_htemp <- vol_vs_htemp + 
  facet_wrap(~weekday, labeller = labeller(weekday = labels))
vol_vs_htemp
```

![](hw_2_review_files/figure-markdown_github/unnamed-chunk-7-1.png)

### 3

``` r
vol_vs_htemp <- vol_vs_htemp + 
  geom_smooth(method = "lm")
vol_vs_htemp
```

![](hw_2_review_files/figure-markdown_github/unnamed-chunk-8-1.png)

Exercise 4.8
------------

``` r
my_teams <- Teams %>% 
  mutate(BA = H/AB,
         SLG = (H + X2B + 2*X3B + 3*HR)/AB)
```

Exercise 4.9
------------

``` r
labels <- c("AL" = "American League",
            "NL" = "National League")
my_teams %>% 
  filter(yearID >= 1954) %>% 
  ggplot(aes(yearID, SLG)) +
    geom_line() +
    facet_wrap(~lgID, labeller = labeller(lgID = labels)) +
    labs(title = "Year vs Slugging percentage",
         x = "Year",
         y = "Slugging percentage")+
    theme_bw()
```

![](hw_2_review_files/figure-markdown_github/unnamed-chunk-10-1.png)

The slugging percentage seems to be higher in the National League. This could be because National League teams tend to play on smaller fields, and so they score more home runs.

Exercise 4.10
-------------

### All time

``` r
Teams %>% 
  group_by(name) %>%
  summarize(SLG = (sum(H) + sum(X2B) + 2*sum(X3B) + 3*sum(HR))/sum(AB)) %>%
  arrange(desc(SLG)) %>% 
  head(15) %>% 
  knitr::kable(row.names = TRUE, col.name = c("Team", "Slugging percentage"))
```

|     | Team                          |  Slugging percentage|
|-----|:------------------------------|--------------------:|
| 1   | Colorado Rockies              |            0.4425709|
| 2   | Anaheim Angels                |            0.4224833|
| 3   | Cincinnati Redlegs            |            0.4200032|
| 4   | Toronto Blue Jays             |            0.4174859|
| 5   | Boston Red Stockings          |            0.4155170|
| 6   | Arizona Diamondbacks          |            0.4152749|
| 7   | Texas Rangers                 |            0.4142062|
| 8   | Milwaukee Braves              |            0.4134909|
| 9   | Los Angeles Angels of Anaheim |            0.4134079|
| 10  | New York Yankees              |            0.4112961|
| 11  | Tampa Bay Devil Rays          |            0.4059607|
| 12  | Florida Marlins               |            0.4053438|
| 13  | Tampa Bay Rays                |            0.4052265|
| 14  | Seattle Mariners              |            0.4038961|
| 15  | Milwaukee Brewers             |            0.4004238|

### Since 1969

``` r
Teams %>% 
  filter(yearID >= 1969) %>% 
  group_by(name) %>%
  summarize(SLG = (sum(H) + sum(X2B) + 2*sum(X3B) + 3*sum(HR))/sum(AB)) %>%
  arrange(desc(SLG)) %>% 
  head(15) %>% 
  knitr::kable(row.names = TRUE, col.name = c("Team", "Slugging percentage"))
```

|     | Team                          |  Slugging percentage|
|-----|:------------------------------|--------------------:|
| 1   | Colorado Rockies              |            0.4425709|
| 2   | Boston Red Sox                |            0.4271114|
| 3   | Anaheim Angels                |            0.4224833|
| 4   | Toronto Blue Jays             |            0.4174859|
| 5   | New York Yankees              |            0.4168594|
| 6   | Arizona Diamondbacks          |            0.4152749|
| 7   | Texas Rangers                 |            0.4142062|
| 8   | Los Angeles Angels of Anaheim |            0.4134079|
| 9   | Detroit Tigers                |            0.4115627|
| 10  | Baltimore Orioles             |            0.4078467|
| 11  | Tampa Bay Devil Rays          |            0.4059607|
| 12  | Florida Marlins               |            0.4053438|
| 13  | Tampa Bay Rays                |            0.4052265|
| 14  | Chicago White Sox             |            0.4049664|
| 15  | Cleveland Indians             |            0.4040719|

Exercise 4.11
-------------
