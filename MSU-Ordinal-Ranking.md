MSU Ordinal Ranking
================
Nick Zirkle

``` r
library(dplyr)
library(ggplot2)
```

``` r
ordinals <- read.csv("MMasseyOrdinals.csv")
msu_ordinals <- ordinals %>% subset(TeamID == 1286)
plot <- msu_ordinals %>% group_by(Season) %>% summarize(mean = mean(OrdinalRank))
```

``` r
ggplot(data = plot, aes(x = factor(Season), y = mean)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Season") +
  ylab("Average Ordinal Rank") +
  ggtitle("MSU Men's Basketball Ordinal Rank")
```

![](MSU-Ordinal-Ranking_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
