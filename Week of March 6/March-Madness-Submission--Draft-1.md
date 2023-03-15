March Madness Submission- Draft 1
================
Nick Zirkle

``` r
men_historical <- read.csv("..//Week of February 20//HistoricalDataset.csv")
men_historical <- men_historical[, c("Point.Differential", "Seed.Differential", "Ordinal.Ranking.Differential")]
```

I first decided to load the historical dataset created a couple weeks
ago, and use that dataset in a fitted linear model. Since the result
category was arbitrary (all games were wins because of the order of
teams), I decided to remove that column and just use the other 3
columns.

``` r
men_lm.fit <- lm(Point.Differential ~ ., data = men_historical)
summary(men_lm.fit)
```

    ## 
    ## Call:
    ## lm(formula = Point.Differential ~ ., data = men_historical)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -18.452  -5.814  -1.353   4.591  34.534 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   9.500128   0.253930  37.412   <2e-16 ***
    ## Seed.Differential             0.093216   0.064969   1.435    0.152    
    ## Ordinal.Ranking.Differential -0.072726   0.006822 -10.661   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.605 on 1178 degrees of freedom
    ## Multiple R-squared:  0.2193, Adjusted R-squared:  0.2179 
    ## F-statistic: 165.4 on 2 and 1178 DF,  p-value: < 2.2e-16

After fitting this model, we see that our seed differential variable
isn’t “statistically significant”, but the intercept and ordinal ranking
differential variable are. As explored previously, however, this is our
best linear model, even though our seed differential variable isn’t as
impactful as we hoped it’d be.

``` r
men_get_win_prob <- function(seed1, ordinal1, seed2, ordinal2) {
  point.diff <- rep(0, 1000)
  beta0 <- rnorm(1000, 9.500128, 0.25393^2)
  beta1 <- rnorm(1000, 0.093216, 0.064969^2)
  beta2 <- rnorm(1000, -0.072726, 0.006822^2)
  for (i in 1:1000) {
    point.diff[i] <- rnorm(1, beta0[i] + beta1[i] * (seed1 - seed2) + beta2[i] * (ordinal1 - ordinal2), 7.605^2)
  }
  return(sum(point.diff > 0) / 1000)
}
```

For my first attempt at figuring out win probabilities, I decided to use
a simulation-based approach, using the linear model fit above. I decided
to use the coefficient estimates in a unique way, such that each
$\beta_i \sim N(\mu_i, \sigma_i^2).$ I decided to simulate 1000 random
values from each normal distribution for each beta, simulating 1000
“games” between the two teams involved. Then, using the two teams’ seeds
and ordinal rankings, I’d plug them into our model for each of the 1000
games, and see which team had the higher point differential. Since our
model assumes normality, I simulated 1 random value per game from
$Y \sim N(\beta_0 + \beta_1 seed + \beta_2 ordinal, \sigma^2)$ to find
the point differential for that game. Then, using all 1000 games, I
calculated the winning probability by dividing the number of positive
point differentials (wins for the first seed) by 1000, to get a
probability between 0 and 1.

``` r
men_ordinals <- read.csv("MMasseyOrdinals_thru_Season2023_Day128.csv")
men_seeds <- read.csv("MNCAATourneySeeds.csv")
```

Now that my method of calculating win probabilities was completed, I
needed to access the seed and ordinal data for the current men’s D1
basketball teams.

``` r
men_ordinals <- men_ordinals %>% filter(Season == 2023)
men_ordinals <- men_ordinals %>% filter(SystemName == "SAG")
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
submission <- read.csv("SampleSubmission2023.csv")
```

I found a naive sample submission (all win probabilities = 0.5) on the
Kaggle competition webpage, and decided to use this to format my
submission, since it included all of the correct team ID comparisons for
both the men’s and women’s competition.

``` r
men_fill_sub <- function(submission) {
  for (i in 1:65703) {
    teams <- str_split(submission[i, 1], "_", simplify = T)
    team1 <- as.numeric(teams[2])
    team2 <- as.numeric(teams[3])
    
    team1_index <- which(men_joint$TeamID == team1)
    team1_seed <- men_joint[team1_index, 2]
    team1_ordinal <- men_joint[team1_index, 3]
    
    team2_index <- which(men_joint$TeamID == team2)
    team2_seed <- men_joint[team2_index, 2]
    team2_ordinal <- men_joint[team2_index, 3]
    
    submission[i, 2] <- men_get_win_prob(team1_seed, team1_ordinal, team2_seed, team2_ordinal)
  }
  return(submission)
}
```

I decided to create a function that would update the win probabilities
on the submission file from naive (0.5) to the win probabilities found
in `men_get_win_prob()`. This function goes through every row in the
submission for men’s teams, and first gets the team IDs from the `ID`
column, by splitting the string at each “\_” to get the season, first
team ID, and second team ID. Then, using the team IDs and our joint
men’s dataset, we grabbed each team’s seed and ordinal ranking. Then, we
updated each submission using `men_get_win_prob()` and the proper inputs
for the function. After all rows were edited, we returned the submission
file to “update” the win probabilities.

``` r
women_results <- read.csv("WNCAATourneyCompactResults.csv")
women_seeds <- read.csv("WNCAATourneySeeds.csv")
```

I then realized that the submission needed to include the results for
the women’s tournament as well, so I decided to create a historical
women’s dataset and fit a model for it using linear regression. Since
there were no ordinal rankings available for the women’s teams, I
decided to fit a model using seed differential as our only explanatory
variable.

``` r
women_seeds$Seed <- gsub('[abWXYZ]', '', women_seeds$Seed)
women_seeds$Seed <- as.numeric(women_seeds$Seed)
```

Like above, I wanted the seeds to just be numeric, instead of related to
region and number, so I made them numeric using the same process as the
men’s seeds.

``` r
num_games <- nrow(women_results)
women_dataset <- data.frame(Point.Differential = rep(0, num_games),
                 Seed.Differential = rep(0, num_games))
```

I created an empty dataset for the women’s historical games, and noted
the number of games that I’d have to include in this dataset.

``` r
for (i in 1:num_games) {
  Season <- women_results[i, 1]
  WTeamID <- women_results[i, 3]
  WScore <- women_results[i, 4]
  LTeamID <- women_results[i, 5]
  LScore <- women_results[i, 6]
  
  score_diff <- WScore - LScore
  women_dataset[i, 1] <- score_diff
  
  season_opt <- which(women_seeds$Season == Season)
  w_id_opt <- which(women_seeds$TeamID == WTeamID)
  l_id_opt <- which(women_seeds$TeamID == LTeamID)
  w_row_index <- intersect(season_opt, w_id_opt)
  l_row_index <- intersect(season_opt, l_id_opt)
  w_seed <- women_seeds[w_row_index, 2]
  l_seed <- women_seeds[l_row_index, 2]
  seed_diff <- w_seed - l_seed
  women_dataset[i, 2] <- seed_diff
}
```

This for-loop is used to fill in the empty women’s historical dataset.
The for loop goes through each game in the `women_results` dataset, and
records the season, winning team ID, winning team score, winning team
seed, losing team ID, losing team score, and losing team seed. Then, it
calculates the point differential and seed differential, and stores them
to the empty women’s dataset.

``` r
women_lm.fit <- lm(Point.Differential ~ ., data = women_dataset)
summary(women_lm.fit)
```

    ## 
    ## Call:
    ## lm(formula = Point.Differential ~ ., data = women_dataset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -25.436  -7.184  -0.827   5.627  60.242 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       11.34474    0.34479   32.90   <2e-16 ***
    ## Seed.Differential -1.16090    0.04652  -24.96   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.54 on 1514 degrees of freedom
    ## Multiple R-squared:  0.2915, Adjusted R-squared:  0.291 
    ## F-statistic: 622.8 on 1 and 1514 DF,  p-value: < 2.2e-16

Using this newly completed dataset, we fit a linear model with seed
differential attempting to predict point differential. Both our
intercept and seed differential variable were “statistically
significant”, so I decided to keep this model.

``` r
women_get_win_prob <- function(seed1, seed2) {
  point.diff <- rep(0, 1000)
  beta0 <- rnorm(1000, 11.34474, 0.34479^2)
  beta1 <- rnorm(1000, -1.16090, 0.04652^2)
  for (i in 1:1000) {
    point.diff[i] <- rnorm(1, beta0[i] + beta1[i] * (seed1 - seed2), 10.54^2)
  }
  return(sum(point.diff > 0) / 1000)
}
```

I then created a function used to predict the win probability of one
team vs another, similar to the function created for the men’s games. I
simulated 1000 values from $\beta_i \sim N(\mu_i, \sigma_i^2)$ for each
“game” between the two teams, and then used coefficient estimates from
our linear model to calculate the point differential of each simulated
game, where each game was from
$Y \sim N(\beta_0 + \beta_1 seed, \sigma^2).$ I then calculated the win
probability by averaging the number of wins (positive point
differentials) over the number of simulated games (1000).

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
women_fill_sub <- function(submission) {
  for (j in 65704:130683) {
    teams <- str_split(submission[j, 1], "_", simplify = T)
    team1 <- as.numeric(teams[2])
    team2 <- as.numeric(teams[3])
    
    team1_index <- which(women_joint$TeamID == team1)
    team1_seed <- women_joint[team1_index, 2]
    
    team2_index <- which(women_joint$TeamID == team2)
    team2_seed <- women_joint[team2_index, 2]
    
    submission[j, 2] <- women_get_win_prob(team1_seed, team2_seed)
  }
  return(submission)
}
```

Similar to the men’s tournament above, I decided to create a function
that would update the win probabilities on the submission file from
naive (0.5) to the win probabilities found in `women_get_win_prob()`.
This function goes through every row in the submission for women’s
teams, and first gets the team IDs from the `ID` column, by splitting
the string at each “\_” to get the season, first team ID, and second
team ID. Then, using the team IDs and our joint women’s dataset, we
grabbed each team’s seed. Then, we updated each submission using
`women_get_win_prob()` and the proper inputs for the function. After all
rows were edited, we returned the submission file to “update” the win
probabilities.

``` r
submission <- men_fill_sub(submission)
submission <- women_fill_sub(submission)
```

These lines run the submission-filling functions for both the men’s and
women’s tournaments. While this process is lengthy (usually takes around
5-10 minutes to run both lines), it updates all 130,000+ win
probabilities in the submission file, which is well worth the wait.

``` r
write.csv(submission, "C:\\Users\\nijaz\\Documents\\MSU\\Junior\\Second Semester\\Sports Analytics\\Weekly Modules\\Week of March 6\\submission_draft_1.csv", row.names=FALSE)
```

This line then writes my updated submission file to a `.csv` file on my
computer, which I then can turn directly in to the Kaggle March Madness
competition.