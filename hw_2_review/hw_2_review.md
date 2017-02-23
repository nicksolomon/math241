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

Exercise 4.8
------------

Exercise 4.9
------------

Exercise 4.10
-------------

Exercise 4.11
-------------
