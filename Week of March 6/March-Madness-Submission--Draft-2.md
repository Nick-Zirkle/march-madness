March Madness Submission- Draft 2
================
Nick Zirkle

As opposed to the approach in my first draft, I intended on using a
logistic regression model this time around. I decided to attempt to use
game data from the historic March Madness tournaments available to me.

``` r
men_tourney_results <- read.csv("MNCAATourneyDetailedResults.csv")
men_ordinals <- read.csv("MMasseyOrdinals_thru_Season2023_Day128.csv")
men_seeds <- read.csv("MNCAATourneySeeds.csv")
```

I first loaded up all the datasets I needed: `men_tourney_results`,
which was data from March Madness tournaments dating back to 2003;
`men_ordinals`, which was ordinal ranking data dating back to 2003; and
`men_seeds`, which was seed ranking data dating back to 1985 (eventually
shortened to 2003).

``` r
men_ordinals <- men_ordinals %>% filter(SystemName == "SAG")
men_ordinals <- men_ordinals[, -c(3)]

men_seeds <- men_seeds %>% filter(Season > 2002)
men_seeds$Seed <- gsub('[abWXYZ]', '', men_seeds$Seed)
men_seeds$Seed <- as.numeric(men_seeds$Seed)
```

I continued to use the SAG ordinal ranking system, to stay consistent
with my previous datasets and simulations. I then filtered the
`men_seeds` dataset to only include seed information dating back to
2003, since I didn’t have any game or ordinal ranking information from
before then. I also removed the region information from the seeds,
making them strictly numeric values.

``` r
men_tourney_results$WOrdinal <- 0
men_tourney_results$WSeed <- 0
men_tourney_results$LOrdinal <- 0
men_tourney_results$LSeed <- 0

men_tourney_results <- men_tourney_results[, -c(8)]
men_tourney_results <- men_tourney_results[, c(1:3, 34:35, 4:5, 36:37, 6:33)]

men_num_tourney_games <- nrow(men_tourney_results)
```

In the tournament game dataset, I created empty columns for the winning
team’s ordinal ranking and seed, and the losing team’s ordinal ranking
and seed. I then removed the column for the number of overtimes, since I
wasn’t interested in this information, and re-ordered the columns to my
liking. Finally, I recorded the number of games in the tournament
dataset (1,248 games).

``` r
men_fill_tourney <- function(men_tourney_results) {
  for (i in 1:men_num_tourney_games) {
    season <- men_tourney_results[i, 1]
    win_id <- men_tourney_results[i, 3]
    loss_id <- men_tourney_results[i, 7]
    
    season_opt <- which(men_ordinals$Season == season)
    day_opt <- which(men_ordinals$RankingDayNum == 133)
    win_id_opt <- which(men_ordinals$TeamID == win_id)
    w_row_index_pre <- intersect(season_opt, day_opt)
    w_row_index <- intersect(w_row_index_pre, win_id_opt)
    w_ordinal <- men_ordinals[w_row_index, 4]
    men_tourney_results[i, 4] <- w_ordinal
    
    season_opt <- which(men_seeds$Season == season)
    win_id_opt <- which(men_seeds$TeamID == win_id)
    w_row_index <- intersect(season_opt, win_id_opt)
    w_seed <- men_seeds[w_row_index, 2]
    men_tourney_results[i, 5] <- w_seed
    
    season_opt <- which(men_ordinals$Season == season)
    day_opt <- which(men_ordinals$RankingDayNum == 133)
    loss_id_opt <- which(men_ordinals$TeamID == loss_id)
    l_row_index_pre <- intersect(season_opt, day_opt)
    l_row_index <- intersect(l_row_index_pre, loss_id_opt)
    l_ordinal <- men_ordinals[l_row_index, 4]
    men_tourney_results[i, 8] <- l_ordinal
    
    season_opt <- which(men_seeds$Season == season)
    loss_id_opt <- which(men_seeds$TeamID == loss_id)
    l_row_index <- intersect(season_opt, loss_id_opt)
    l_seed <- men_seeds[l_row_index, 2]
    men_tourney_results[i, 9] <- l_seed
  }
  return(men_tourney_results)
}
```

The job of the `men_fill_tourney()` function was to fill the ordinal and
seed rankings of each team in all recorded tournament games. For each
game, the function recorded the season it occurred, the winning team’s
ID, and the losing team’s ID. Then, it found the row for each
team/season combination in both the `men_ordinals` and `men_seeds`
datasets, and copied each team’s ordinal ranking and seed to the
`men_tourney_results` database. It then returned the the
`men_tourney_results` dataset, “updating” its values for its empty
columns.

``` r
men_tourney_results <- men_fill_tourney(men_tourney_results)
```

This function call runs the `men_fill_tourney()` function, which fills
the ordinal and seed rankings for each team in every recorded tournament
game, updating the dataset.

``` r
men_tourney_results <- men_tourney_results[, c(3:10)]
names(men_tourney_results) <- c("Team1ID", "Team1Ordinal", "Team1Seed", "Team1Score",
                                "Team2ID", "Team2Ordinal", "Team2Seed", "Team2Score")

for (i in 1:men_num_tourney_games) {
  if (i %% 2 == 0) {
    temp_id <- men_tourney_results[i, 1]
    temp_ordinal <- men_tourney_results[i, 2]
    temp_seed <- men_tourney_results[i, 3]
    temp_score <- men_tourney_results[i, 4]
    men_tourney_results[i, 1] <- men_tourney_results[i, 5]
    men_tourney_results[i, 2] <- men_tourney_results[i, 6]
    men_tourney_results[i, 3] <- men_tourney_results[i, 7]
    men_tourney_results[i, 4] <- men_tourney_results[i, 8]
    men_tourney_results[i, 5] <- temp_id
    men_tourney_results[i, 6] <- temp_ordinal
    men_tourney_results[i, 7] <- temp_seed
    men_tourney_results[i, 8] <- temp_score
  }
}
```

After all this work, I realized that a lot of the box score data (like
WFGM, WFGA, WFGM3, etc.) wasn’t useful in predicting team success, since
we don’t get this data until after games are played, making the box
score data useless for our method of prediction. So, I removed most of
the columns from the tournament games dataset, only keeping the columns
for ID (winning and losing teams), score (winning and losing teams),
ordinal rankings (winning and losing teams), and seeds (winning and
losing teams). Later on when fitting the logistic regression model, I
had an error where the model didn’t converge, and the only way of fixing
this that I could find was to randomize which team was `WTeam` and which
team was `LTeam`. So, I renamed all the columns from `W<statistic>` and
`L<statistic>` to `Team1<statistic` and `Team2<statistic>`, and then
alternated which team was in which spot using the for-loop above. This
ended up fixing the convergence issue.

``` r
men_tourney_results$Result <- rep(c(1, 0), men_num_tourney_games/2)
men_tourney_results$Ordinal.Diff <- men_tourney_results$Team1Ordinal - men_tourney_results$Team2Ordinal
men_tourney_results$Ordinal.Sum <- men_tourney_results$Team1Ordinal + men_tourney_results$Team2Ordinal
men_tourney_results$Seed.Diff <- men_tourney_results$Team1Seed - men_tourney_results$Team2Seed
men_tourney_results$Seed.Sum <- men_tourney_results$Team1Seed + men_tourney_results$Team2Seed

men_tourney_results <- men_tourney_results[, c(9:13)]
```

I then added a column for the result, which alternated between 1 for a
win and 0 for a loss (because we alternated which team was in each spot
above). We additionally then added columns for the difference in ordinal
rankings, sum of ordinal rankings, difference in seeds, and sum of
seeds, since these were the only variables I would be interested in for
predictions. I then got rid of all of the other columns to just leave
the last 5 columns mentioned.

``` r
log_fit_1 <- glm(Result ~ Ordinal.Diff + Ordinal.Sum + Seed.Diff + Seed.Sum, data = men_tourney_results, family = "binomial")
summary(log_fit_1)
```

    ## 
    ## Call:
    ## glm(formula = Result ~ Ordinal.Diff + Ordinal.Sum + Seed.Diff + 
    ##     Seed.Sum, family = "binomial", data = men_tourney_results)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.47571  -0.98743  -0.02019   0.97382   2.60998  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   0.120700   0.174890   0.690 0.490102    
    ## Ordinal.Diff -0.013840   0.002828  -4.894 9.89e-07 ***
    ## Ordinal.Sum   0.002395   0.001494   1.603 0.108903    
    ## Seed.Diff    -0.066814   0.020279  -3.295 0.000985 ***
    ## Seed.Sum     -0.021180   0.017189  -1.232 0.217873    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1730.1  on 1247  degrees of freedom
    ## Residual deviance: 1382.2  on 1243  degrees of freedom
    ## AIC: 1392.2
    ## 
    ## Number of Fisher Scoring iterations: 5

The first logistic regression model I fit included an intercept and all
4 predictors (ordinal ranking difference/sum and seed difference/sum)
predicting the result (1 for a Team 1 win, 0 for a Team 1 loss). It
appeared as though the ordinal ranking difference and seed difference
variables were our only “statistically significant” variables in this
model, so I first attempted to fit other models before proceeding with
this one.

``` r
log_fit_2 <- glm(Result ~ Ordinal.Diff + Seed.Diff, data = men_tourney_results, family = "binomial")
summary(log_fit_2)
```

    ## 
    ## Call:
    ## glm(formula = Result ~ Ordinal.Diff + Seed.Diff, family = "binomial", 
    ##     data = men_tourney_results)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.41658  -0.97905   0.00696   0.97853   2.68244  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   0.002258   0.065017   0.035 0.972291    
    ## Ordinal.Diff -0.013694   0.002806  -4.881 1.05e-06 ***
    ## Seed.Diff    -0.067426   0.020176  -3.342 0.000832 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1730.1  on 1247  degrees of freedom
    ## Residual deviance: 1384.8  on 1245  degrees of freedom
    ## AIC: 1390.8
    ## 
    ## Number of Fisher Scoring iterations: 5

The next model I attempted to fit included an intercept and the 2
significant variables from the previous model (ordinal ranking
difference and seed difference). Both variables were still significant
in this new model, so I decided to eventually compare this model to the
original model.

``` r
log_fit_3 <- glm(Result ~ Ordinal.Diff + Ordinal.Sum + Seed.Diff + Seed.Sum - 1, data = men_tourney_results, family = "binomial")
summary(log_fit_3)
```

    ## 
    ## Call:
    ## glm(formula = Result ~ Ordinal.Diff + Ordinal.Sum + Seed.Diff + 
    ##     Seed.Sum - 1, family = "binomial", data = men_tourney_results)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.46660  -0.97163  -0.01533   0.98447   2.62178  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## Ordinal.Diff -0.013823   0.002826  -4.892 9.97e-07 ***
    ## Ordinal.Sum   0.001939   0.001337   1.450 0.147070    
    ## Seed.Diff    -0.066787   0.020266  -3.296 0.000982 ***
    ## Seed.Sum     -0.011048   0.008922  -1.238 0.215605    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1730.1  on 1248  degrees of freedom
    ## Residual deviance: 1382.7  on 1244  degrees of freedom
    ## AIC: 1390.7
    ## 
    ## Number of Fisher Scoring iterations: 5

The next model I attempted to fit included the 4 significant variables
from the previous model (ordinal ranking difference/sum and seed
difference/sum), but without an intercept term. Both variables that were
significant in the original model were still significant in this new
model, so I decided to eventually compare this model to the original
model.

``` r
log_fit_4 <- glm(Result ~ Ordinal.Diff + Seed.Diff - 1, data = men_tourney_results, family = "binomial")
summary(log_fit_4)
```

    ## 
    ## Call:
    ## glm(formula = Result ~ Ordinal.Diff + Seed.Diff - 1, family = "binomial", 
    ##     data = men_tourney_results)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.41570  -0.97817   0.00709   0.97940   2.68326  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## Ordinal.Diff -0.013694   0.002806  -4.881 1.06e-06 ***
    ## Seed.Diff    -0.067427   0.020176  -3.342 0.000832 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1730.1  on 1248  degrees of freedom
    ## Residual deviance: 1384.8  on 1246  degrees of freedom
    ## AIC: 1388.8
    ## 
    ## Number of Fisher Scoring iterations: 5

The final model I attempted to fit included the 2 significant variables
from the previous model (ordinal ranking difference and seed
difference), but without an intercept term. Both variables were still
significant in this new model, so I decided to eventually compare this
model to the original model.

``` r
anova(log_fit_1, log_fit_2, log_fit_3, log_fit_4)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Result ~ Ordinal.Diff + Ordinal.Sum + Seed.Diff + Seed.Sum
    ## Model 2: Result ~ Ordinal.Diff + Seed.Diff
    ## Model 3: Result ~ Ordinal.Diff + Ordinal.Sum + Seed.Diff + Seed.Sum - 
    ##     1
    ## Model 4: Result ~ Ordinal.Diff + Seed.Diff - 1
    ##   Resid. Df Resid. Dev Df Deviance
    ## 1      1243     1382.2            
    ## 2      1245     1384.8 -2  -2.6345
    ## 3      1244     1382.7  1   2.1580
    ## 4      1246     1384.8 -2  -2.1592

After comparing the 4 fit models using `anova()`, it appears as though
all 4 models explained the data relatively similarly, since all of the
residual deviations were within 2.6 units of each other. Based on this,
I chose to use the last fit model, which didn’t include an intercept
term, but included the ordinal ranking difference and seed difference
variables, since this was the only model that contained entirely
“statistically significant” variables, and the model with the smallest
degrees of freedom.

``` r
women_tourney_results <- read.csv("WNCAATourneyCompactResults.csv")
women_seeds <- read.csv("WNCAATourneySeeds.csv")
```

I then started working on predicting the women’s win probabilities by
loading up both datasets I needed: `women_tourney_results`, which was
data from March Madness tournaments dating back to 1998; and
`women_seeds`, which was seed ranking data dating back to 1998.

``` r
women_seeds$Seed <- gsub('[abWXYZ]', '', women_seeds$Seed)
women_seeds$Seed <- as.numeric(women_seeds$Seed)

women_tourney_results$WSeed <- 0
women_tourney_results$LSeed <- 0

women_tourney_results <- women_tourney_results[, -c(7:8)]
women_tourney_results <- women_tourney_results[, c(1:3, 7, 4:5, 8, 6)]

women_num_tourney_games <- nrow(women_tourney_results)
```

I then removed the region information from the seeds, making them
strictly numeric values and created empty columns in the tournament game
dataset for the winning team’s seed and the losing team’s seed. I then
removed the columns for the location and number of overtimes, since I
wasn’t interested in this information, and re-ordered the columns to my
liking. Finally, I recorded the number of games in the tournament
dataset (1,516 games).

``` r
women_fill_tourney <- function(women_tourney_results) {
  for (i in 1:women_num_tourney_games) {
    season <- women_tourney_results[i, 1]
    win_id <- women_tourney_results[i, 3]
    loss_id <- women_tourney_results[i, 6]
    
    season_opt <- which(women_seeds$Season == season)
    win_id_opt <- which(women_seeds$TeamID == win_id)
    w_row_index <- intersect(season_opt, win_id_opt)
    w_seed <- women_seeds[w_row_index, 2]
    women_tourney_results[i, 4] <- w_seed
    
    season_opt <- which(women_seeds$Season == season)
    loss_id_opt <- which(women_seeds$TeamID == loss_id)
    l_row_index <- intersect(season_opt, loss_id_opt)
    l_seed <- women_seeds[l_row_index, 2]
    women_tourney_results[i, 7] <- l_seed
  }
  return(women_tourney_results)
}
```

The job of the `women_fill_tourney()` function was to fill the seed
rankings of each team in all recorded tournament games. For each game,
the function recorded the season it occurred, the winning team’s ID, and
the losing team’s ID. Then, it found the row for each team/season
combination in the `women_seeds` datasets, and copied each team’s seed
to the `women_tourney_results` database. It then returned the the
`women_tourney_results` dataset, “updating” its values for its empty
columns.

``` r
women_tourney_results <- women_fill_tourney(women_tourney_results)
```

This function call runs the `women_fill_tourney()` function, which fills
the seed rankings for each team in every recorded tournament game,
updating the dataset.

``` r
women_tourney_results <- women_tourney_results[, -c(1:2)]
names(women_tourney_results) <- c("Team1ID", "Team1Seed", "Team1Score",
                                  "Team2ID", "Team2Seed", "Team2Score")

for (i in 1:women_num_tourney_games) {
  if (i %% 2 == 0) {
    temp_id <- women_tourney_results[i, 1]
    temp_seed <- women_tourney_results[i, 2]
    temp_score <- women_tourney_results[i, 3]
    women_tourney_results[i, 1] <- women_tourney_results[i, 4]
    women_tourney_results[i, 2] <- women_tourney_results[i, 5]
    women_tourney_results[i, 3] <- women_tourney_results[i, 6]
    women_tourney_results[i, 4] <- temp_id
    women_tourney_results[i, 5] <- temp_seed
    women_tourney_results[i, 6] <- temp_score
  }
}
```

I then removed the season and day columns from the dataset, only keeping
the columns for ID (winning and losing teams), score (winning and losing
teams), and seeds (winning and losing teams). Later on when fitting the
logistic regression model, I had an error where the model didn’t
converge, and the only way of fixing this that I could find was to
randomize which team was `WTeam` and which team was `LTeam`. So, I
renamed all the columns from `W<statistic>` and `L<statistic>` to
`Team1<statistic` and `Team2<statistic>`, and then alternated which team
was in which spot using the for-loop above. This ended up fixing the
convergence issue.

``` r
women_tourney_results$Result <- rep(c(1, 0), women_num_tourney_games/2)
women_tourney_results$Seed.Diff <- women_tourney_results$Team1Seed - women_tourney_results$Team2Seed
women_tourney_results$Seed.Sum <- women_tourney_results$Team1Seed + women_tourney_results$Team2Seed

women_tourney_results <- women_tourney_results[, c(7:9)]
```

I then added a column for the result, which alternated between 1 for a
win and 0 for a loss (because we alternated which team was in each spot
above). We additionally then added columns for the difference in seeds
and sum of seeds, since these were the only variables I would be
interested in for predictions. I then got rid of all of the other
columns to just leave the last 3 columns mentioned.

``` r
log_fit_5 <- glm(Result ~ Seed.Diff + Seed.Sum, data = women_tourney_results, family = "binomial")
summary(log_fit_5)
```

    ## 
    ## Call:
    ## glm(formula = Result ~ Seed.Diff + Seed.Sum, family = "binomial", 
    ##     data = women_tourney_results)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8526  -0.7174  -0.0090   0.7633   2.1811  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.029039   0.149851  -0.194    0.846    
    ## Seed.Diff   -0.263864   0.013627 -19.364   <2e-16 ***
    ## Seed.Sum     0.007199   0.012070   0.596    0.551    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2101.6  on 1515  degrees of freedom
    ## Residual deviance: 1387.4  on 1513  degrees of freedom
    ## AIC: 1393.4
    ## 
    ## Number of Fisher Scoring iterations: 5

The first logistic regression model I fit included an intercept and all
2 predictors (seed difference/sum) predicting the result (1 for a Team 1
win, 0 for a Team 1 loss). It appeared as though the seed difference
variable was our only “statistically significant” variable in this
model, so I first attempted to fit other models before proceeding with
this one.

``` r
log_fit_6 <- glm(Result ~ Seed.Diff, data = women_tourney_results, family = "binomial")
summary(log_fit_6)
```

    ## 
    ## Call:
    ## glm(formula = Result ~ Seed.Diff, family = "binomial", data = women_tourney_results)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.83674  -0.70450  -0.00493   0.75623   2.19780  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  0.05109    0.06646   0.769    0.442    
    ## Seed.Diff   -0.26363    0.01361 -19.368   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2101.6  on 1515  degrees of freedom
    ## Residual deviance: 1387.7  on 1514  degrees of freedom
    ## AIC: 1391.7
    ## 
    ## Number of Fisher Scoring iterations: 5

The next model I attempted to fit included an intercept and just the 1
significant variable from the previous model (seed difference). This
variable was still significant in this new model, so I decided to
eventually compare this model to the original model.

``` r
log_fit_7 <- glm(Result ~ Seed.Diff + Seed.Sum - 1, data = women_tourney_results, family = "binomial")
summary(log_fit_7)
```

    ## 
    ## Call:
    ## glm(formula = Result ~ Seed.Diff + Seed.Sum - 1, family = "binomial", 
    ##     data = women_tourney_results)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.85018  -0.71535  -0.00836   0.75932   2.18380  
    ## 
    ## Coefficients:
    ##            Estimate Std. Error z value Pr(>|z|)    
    ## Seed.Diff -0.263843   0.013625 -19.364   <2e-16 ***
    ## Seed.Sum   0.005104   0.005356   0.953    0.341    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2101.6  on 1516  degrees of freedom
    ## Residual deviance: 1387.4  on 1514  degrees of freedom
    ## AIC: 1391.4
    ## 
    ## Number of Fisher Scoring iterations: 5

The next model I attempted to fit included the 2 original predictor
variables (seed difference/sum), but without an intercept term. Only the
seed difference variable was significant in this new model, so I decided
to eventually compare this model to the original model.

``` r
log_fit_8 <- glm(Result ~ Seed.Diff - 1, data = women_tourney_results, family = "binomial")
summary(log_fit_8)
```

    ## 
    ## Call:
    ## glm(formula = Result ~ Seed.Diff - 1, family = "binomial", data = women_tourney_results)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8173  -0.6892   0.0000   0.7736   2.2177  
    ## 
    ## Coefficients:
    ##           Estimate Std. Error z value Pr(>|z|)    
    ## Seed.Diff -0.26330    0.01359  -19.37   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2101.6  on 1516  degrees of freedom
    ## Residual deviance: 1388.3  on 1515  degrees of freedom
    ## AIC: 1390.3
    ## 
    ## Number of Fisher Scoring iterations: 5

The final model I attempted to fit included the 1 significant variable
from the previous model (seed difference), but without an intercept
term. This variable was still significant in this new model, so I
decided to eventually compare this model to the original model.

``` r
anova(log_fit_5, log_fit_6, log_fit_7, log_fit_8)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Result ~ Seed.Diff + Seed.Sum
    ## Model 2: Result ~ Seed.Diff
    ## Model 3: Result ~ Seed.Diff + Seed.Sum - 1
    ## Model 4: Result ~ Seed.Diff - 1
    ##   Resid. Df Resid. Dev Df Deviance
    ## 1      1513     1387.4            
    ## 2      1514     1387.7 -1 -0.35599
    ## 3      1514     1387.4  0  0.31843
    ## 4      1515     1388.3 -1 -0.90975

After comparing the 4 fit models using `anova()`, it appears as though
all 4 models explained the data relatively similarly, since all of the
residual deviations were within 0.9 units of each other. Based on this,
I chose to use the last fit model, which didn’t include an intercept
term, but included the seed difference variable, since this was the only
model that contained entirely “statistically significant” variables, and
the model with the smallest degrees of freedom.

``` r
submission <- read.csv("SampleSubmission2023.csv")
```

I loaded the sample submission from the Kaggle competition webpage to
get the format for the submission onto the document.

``` r
men_submission_a <- data.frame(ID = submission[c(1:65703), 1],
                             Team1Ordinal = rep(0, 65703),
                             Team1Seed = rep(0, 65703),
                             Team2Ordinal = rep(0, 65703),
                             Team2Seed = rep(0, 65703))
men_sub_games <- nrow(men_submission_a)

women_submission_a <- data.frame(ID = submission[c(65704:130683), 1],
                               Team1Seed = rep(0, 130683-65703),
                               Team2Seed = rep(0, 130683-65703))
women_sub_games <- nrow(women_submission_a)
```

I created two sub-datasets of the submission file: one with only men’s
games, and one with only women’s games. I created empty columns for each
team’s ordinal ranking and seed (only seed for the women’s dataset), and
noted the number of games in each dataset.

``` r
men_ordinals <- men_ordinals %>% filter(Season == 2023)
men_ordinals <- men_ordinals %>% filter(RankingDayNum == 128)
men_ordinals <- men_ordinals[, c("TeamID", "OrdinalRank")]

men_seeds <- men_seeds %>% filter(Season == 2023)
men_seeds <- men_seeds[, c("TeamID", "Seed")]
men_seeds$Seed <- gsub('[abWXYZ]', '', men_seeds$Seed)
men_seeds$Seed <- as.numeric(men_seeds$Seed)
```

For the ordinal rankings, I was only interested in a team’s
end-of-season ordinal ranking, so I filtered the dataset to only include
rankings from day 128 (last day available) of the 2023 season.
Additionally, I decided to only use the SAG ordinal rankings, since this
is the ordinal ranking system I used previously during the creation of
my historical dataset. For the seeds, I was only interested in a team’s
seed ranking for this tournament, so I filtered the dataset to only
include rankings from the 2023 season. Additionally, I didn’t care which
region the seed was from, so I made the seed just a number (1-16)
instead of the region also being attached. For both datasets, I also
filtered them to just include the team’s ID and the statistic of
interest (ordinal ranking or seed).

``` r
men_joint <- full_join(men_seeds, men_ordinals, by = "TeamID")
men_joint[is.na(men_joint)] <- 0
```

I then combined both datasets into one, which included the team’s ID,
ordinal ranking, and seed. Since not every D1 team made the March
Madness tournament, a lot of the values for seed were NA, which messed
up some of my code later down the line. To combat this, I changed all
NAs in the dataset to 0, so my code could run. While this does affect
the win probabilities of teams in the tournament vs teams out of the
tournament, we aren’t interested in this comparison for the March
Madness competition, so I kept the dataset edited like previously
mentioned.

``` r
men_fill_sub <- function(men_submission_a) {
  for (i in 1:men_sub_games) {
    teams <- str_split(submission[i, 1], "_", simplify = T)
    team1 <- as.numeric(teams[2])
    team2 <- as.numeric(teams[3])
    
    team1_index <- which(men_joint$TeamID == team1)
    men_submission_a[i, 2] <- men_joint[team1_index, 3]
    men_submission_a[i, 3] <- men_joint[team1_index, 2]
    
    team2_index <- which(men_joint$TeamID == team2)
    men_submission_a[i, 4] <- men_joint[team2_index, 3]
    men_submission_a[i, 5] <- men_joint[team2_index, 2]
  }
  return(men_submission_a)
}
```

I decided to create a function that would go through every row in the
submission for men’s teams, and get both teams’ ordinal rankings and
seeds. First, I grabbed the team IDs from the `ID` column, by splitting
the string at each “\_” to get the season, first team ID, and second
team ID. Then, using the team IDs and our joint men’s dataset, we
grabbed each team’s seed and ordinal ranking. Finally, we returned the
submission file to “update” the win probabilities.

``` r
women_seeds <- women_seeds %>% filter(Season == 2023)
women_seeds <- women_seeds[, c("TeamID", "Seed")]
```

Then, I needed a dataset that contained the updated seeds for the 2023
women’s March Madness tournament, so I filtered the original
`women_seeds` dataset to only include seeds from the 2023 season.

``` r
women_teams <- read.csv("WTeams.csv")
```

To make sure all D1 women’s teams were included, I found a dataset on
Kaggle listing every current D1 women’s team, and uploaded it (purpose
will be explained later).

``` r
women_joint <- full_join(women_teams, women_seeds, by = "TeamID")
women_joint <- women_joint[, c("TeamID", "Seed")]
women_joint[is.na(women_joint)] <- 0
```

Using the `women_teams` dataset and the updated `women_seeds` dataset, I
created a joint dataset, which included each team’s ID and their seed.
However, since all teams needed a seed for my function (even if they
didn’t make the tournament), I replaced all NAs in the dataset with
zeros, which only affects win probability calculations between a team in
the tournament and a team out of the tournament (which we aren’t
interested in).

``` r
women_fill_sub <- function(women_submission_a) {
  for (j in 1:women_sub_games) {
    teams <- str_split(submission[65703+j, 1], "_", simplify = T)
    team1 <- as.numeric(teams[2])
    team2 <- as.numeric(teams[3])
    
    team1_index <- which(women_joint$TeamID == team1)
    women_submission_a[j, 2] <- women_joint[team1_index, 2]
    
    team2_index <- which(women_joint$TeamID == team2)
    women_submission_a[j, 3] <- women_joint[team2_index, 2]
  }
  return(women_submission_a)
}
```

Similar to the men’s tournament above, I decided to create a function
that would go through every row in the submission for women’s teams, and
get both teams’ seeds. First, I grabbed the team IDs from the `ID`
column, by splitting the string at each “\_” to get the season, first
team ID, and second team ID. Then, using the team IDs and our joint
women’s dataset, we grabbed each team’s seed. Finally, we returned the
submission file to “update” the win probabilities.

``` r
men_submission_a <- men_fill_sub(men_submission_a)
women_submission_a <- women_fill_sub(women_submission_a)
```

These lines run the submission-filling functions for both the men’s and
women’s tournaments.

``` r
men_submission_b <- men_submission_a
men_submission_b$Seed.Diff <- men_submission_b$Team1Seed - men_submission_b$Team2Seed
men_submission_b$Ordinal.Diff <- men_submission_b$Team1Ordinal - men_submission_b$Team2Ordinal
men_submission_b <- men_submission_b[, c(6:7)]
```

I created a copy of the men’s submission subset, and created seed
difference and ordinal ranking difference columns based on the given
information. Then, I removed all of the other columns, so only these 2
columns were in the subset.

``` r
men_submission_b$Pred <- predict(log_fit_4, newdata = men_submission_b, type = "response")

men_submission <- data.frame(ID = men_submission_a$ID, Pred = men_submission_b$Pred)
```

Using the men’s logistic regression model we fit previously, I predicted
the win probabilities for each matchup based on the seed and ordinal
ranking differences. I then merged these probabilities, along with the
original game IDs, into a final men’s submission dataset.

``` r
women_submission_b <- women_submission_a
women_submission_b$Seed.Diff <- women_submission_b$Team1Seed - women_submission_b$Team2Seed
women_submission_b <- data.frame(Seed.Diff = women_submission_b[, 4])
```

I created a copy of the women’s submission subset, and created a seed
difference column based on the given information. Then, I removed all of
the other columns, so only this column was in the subset.

``` r
women_submission_b$Pred <- predict(log_fit_8, newdata = women_submission_b, type = "response")

women_submission <- data.frame(ID = women_submission_a$ID, Pred = women_submission_b$Pred)
```

Using the women’s logistic regression model we fit previously, I
predicted the win probabilities for each matchup based on the seed
difference. I then merged these probabilities, along with the original
game IDs, into a final women’s submission dataset.

``` r
submission <- bind_rows(men_submission, women_submission)
```

I merged both the men’s and women’s datasets into one final submission
dataset, containing all 130,683 game IDs and win probabilities.

``` r
write.csv(submission, "C:\\Users\\nijaz\\Documents\\MSU\\Junior\\Second Semester\\Sports Analytics\\Weekly Modules\\Week of March 6\\Draft 2\\submission_draft_2.csv", row.names=FALSE)
```

This line then writes my updated submission file to a `.csv` file on my
computer, which I then can turn directly in to the Kaggle March Madness
competition.

After using two different approaches, I have some comments about which
methods I prefer. I personally preferred the first method (linear
regression and random simulation), since it was easier to set up than
the second method, and kept the win probabilities closer to 0.5 than the
second method. This is important because submissions with more
variability between win probabilities get punished harder than models
with less variability between win probabilities, meaning that an
upset/wrong prediction doesn’t affect my final Brier score as much if I
use the first method. However, I can see the appeal of using the second
method, especially if we had more predictor variables available to
us/ready at the time of the competition (i.e: conference strength, form
heading into tournament, team health, etc.). If more variables were
available/ready, I would definitely prefer the second method, since it
spits out a single win probability based on a variety of factors,
instead of a random win probability based on random noise and only a
couple of variables. Additionally, I believe that the variability of win
probabilities using the second method would decrease as the number of
predictor variables increased, which would help our model even further.
