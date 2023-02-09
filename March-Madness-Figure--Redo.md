March Madness Figure
================
Nick Zirkle

``` r
library(dplyr)
library(ggplot2)
```

``` r
conferences <- read.csv("MTeamConferences.csv")
conferences_2022 <- conferences %>% subset(Season == "2022")
counter <- conferences_2022 %>% count(ConfAbbrev)
```

``` r
ggplot(counter, aes(x = ConfAbbrev, y = n)) +
  geom_bar(stat = "identity") +
  xlab("Conference Abbreviation") +
  ylab("Number of Teams") +
  theme_get() +
  ggtitle("Number of Basketball Teams per NCAA Conference in 2022") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

![](March-Madness-Figure--Redo_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
