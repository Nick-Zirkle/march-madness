Historical Dataset
================

``` r
library(dplyr)
```

``` r
results <- read.csv("MNCAATourneyCompactResults.csv")
seeds <- read.csv("MNCAATourneySeeds.csv")
ordinal <- read.csv("MMasseyOrdinals.csv")
```

``` r
results <- results %>% filter(Season >= 2003)
seeds <- seeds %>% filter(Season >= 2003)
```

Since ordinal rankings started in 2003, but my NCAA game results started
in 1985, my historical dataset won’t have any games before 2003, so I
can incorporate ordinal ranking differences into a linear regression
model.

``` r
decision1 <- ordinal %>% count(SystemName)
decision1 <- decision1[order(-decision1$n),]
decision1[1,]
```

    ##     SystemName      n
    ## 145        SAG 122047

Since the `SAG` ordinal rankings had the most observations, I decided to
use them for the ordinal ranking difference for my historical dataset.

``` r
ordinal <- ordinal %>% filter(SystemName == "SAG") %>% filter(RankingDayNum == 133)
```

Additionally, I decided to only use ordinal rankings from the final
133rd ranking day, since this was the day closest to when the NCAA
tournament games were actually played.

``` r
seeds$Seed <- gsub('[abWXYZ]', '', seeds$Seed)
seeds$Seed <- as.numeric(seeds$Seed)
```

I also decided to edit the team seed from a region and seed to just a
number for a seed, since we aren’t interested in which regions perform
better in the tournament.

``` r
num_games <- nrow(results)
HistoricalDataset <- data.frame(Result = rep("W", num_games),
                 Point.Differential = rep(0, num_games),
                 Seed.Differential = rep(0, num_games),
                 Ordinal.Ranking.Differential = rep(0, num_games))
```

This is the code used to start the historical dataset. I created a data
frame with the same amount of rows as games available to us as data, and
then I created 4 columns: Result, Point.Differential, Seed.Differential,
and Ordinal.Ranking.Differential. These columns contained the result of
the game, the point differential, the difference in NCAA seeds, and the
difference in SAG ordinal rankings on day 133. The result of the game
was always a win, along with the point differential always being
positive, since the point differential was always set up as
`Winning Score - Losing Score`, and there were no ties allowed in the
tournament. I also defaulted all of the other values to `0`.

``` r
for (i in 1:num_games) {
  Season <- results[i, 1]
  WTeamID <- results[i, 3]
  WScore <- results[i, 4]
  LTeamID <- results[i, 5]
  LScore <- results[i, 6]
  
  score_diff <- WScore -LScore
  HistoricalDataset[i, 2] <- score_diff
  
  season_opt <- which(seeds$Season == Season)
  w_id_opt <- which(seeds$TeamID == WTeamID)
  l_id_opt <- which(seeds$TeamID == LTeamID)
  w_row_index <- intersect(season_opt, w_id_opt)
  l_row_index <- intersect(season_opt, l_id_opt)
  w_seed <- seeds[w_row_index, 2]
  l_seed <- seeds[l_row_index, 2]
  seed_diff <- w_seed - l_seed
  HistoricalDataset[i, 3] <- seed_diff
  
  season_opt <- which(ordinal$Season == Season)
  w_id_opt <- which(ordinal$TeamID == WTeamID)
  l_id_opt <- which(ordinal$TeamID == LTeamID)
  w_row_index <- intersect(season_opt, w_id_opt)
  l_row_index <- intersect(season_opt, l_id_opt)
  w_ordinal <- ordinal[w_row_index, 5]
  l_ordinal <- ordinal[l_row_index, 5]
  ordinal_diff <- w_ordinal - l_ordinal
  HistoricalDataset[i, 4] <- ordinal_diff
}
```

This for loop is pretty complicated, but I’ll attempt to explain how I
got all of the data from the three original datasets into the historical
dataset. This loop is going through every game in the
MNCAATourneyCompactResults dataset.

The first thing I did was write down all of the relevant information
from the current game, including the season, winning team ID and score,
and losing team ID and score. Since all of the games were sorted by
winning team vs losing team, I didn’t have to edit the result column of
my historical dataset. I then got the point differential by subtracting
the losing score from the winning score. In order to find the seed and
ordinal ranking differentials, I had to reference the MNCAATourneySeeds
and MMasseyOrdinals datasets, which required much more work than the
previous two columns in the historical dataset.

I got these differentials by using a combination of `which()` and
`intersect()`. I used `which()` to return every row index of the given
criteria, that being the seeds/rankings from the current season, from
the winning team, and from the losing team. I used `intersect()` to find
the combinations of row indices that matched from the current season and
from the winning/losing team, essentially finding each team’s row index
in the seed and ordinal ranking datasets. Then, I used these indices to
extract each team’s seed and ordinal ranking, and then got the
differentials by subtracting the losing team’s seed/ordinal ranking from
the winning team’s seed/ordinal ranking, which I then input into the
historical dataset.
