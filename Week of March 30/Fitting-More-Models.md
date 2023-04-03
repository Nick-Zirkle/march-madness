Fitting More Models
================
Nick Zirkle

``` r
data <- read.csv("Andy's Dataset.csv")
```

#### Fitting a linear regression model based on Score Differential

``` r
linear_fit_full <- lm(Score.Diff ~ Seed.Diff + Higher.Seed + COL.Diff + Higher.COL + DOL.Diff + Higher.DOL +
                   MOR.Diff + Higher.MOR + POM.Diff + Higher.POM +
                   SAG.Diff + Higher.SAG + WLK.Diff + Higher.WLK, data = data)
summary(linear_fit_full)
```

    ## 
    ## Call:
    ## lm(formula = Score.Diff ~ Seed.Diff + Higher.Seed + COL.Diff + 
    ##     Higher.COL + DOL.Diff + Higher.DOL + MOR.Diff + Higher.MOR + 
    ##     POM.Diff + Higher.POM + SAG.Diff + Higher.SAG + WLK.Diff + 
    ##     Higher.WLK, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -40.813  -7.378   0.388   7.612  41.553 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  3.274193   1.007060   3.251  0.00118 **
    ## Seed.Diff   -0.008735   0.154529  -0.057  0.95493   
    ## Higher.Seed -0.541327   0.190295  -2.845  0.00452 **
    ## COL.Diff     0.034766   0.033234   1.046  0.29572   
    ## Higher.COL   0.059496   0.070601   0.843  0.39955   
    ## DOL.Diff    -0.014084   0.034734  -0.405  0.68519   
    ## Higher.DOL   0.076202   0.079062   0.964  0.33532   
    ## MOR.Diff    -0.040222   0.019072  -2.109  0.03515 * 
    ## Higher.MOR  -0.060782   0.048979  -1.241  0.21485   
    ## POM.Diff    -0.031846   0.030943  -1.029  0.30358   
    ## Higher.POM   0.111434   0.076335   1.460  0.14460   
    ## SAG.Diff    -0.016624   0.041301  -0.403  0.68738   
    ## Higher.SAG   0.062904   0.093631   0.672  0.50182   
    ## WLK.Diff    -0.036204   0.038728  -0.935  0.35006   
    ## Higher.WLK  -0.194610   0.105662  -1.842  0.06574 . 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.16 on 1233 degrees of freedom
    ## Multiple R-squared:  0.2348, Adjusted R-squared:  0.2261 
    ## F-statistic: 27.03 on 14 and 1233 DF,  p-value: < 2.2e-16

*As we can see from the summary output from above, only 3 of our
predictor variables are “statistically significant” at p = 0.05: our
intercept term, the higher seed variable, and the MOR ordinal ranking
differential variable.*

``` r
pred <- data.frame(Season = data$Season, Score.Diff = predict(object = linear_fit_full))

years <- seq(2003, 2022, by = 1)
years <- years[-c(18)]
averages <- rep(0, 19)
for (i in years) {
  temp_data <- data %>% filter(Season == i)
  temp_pred <- pred %>% filter(Season == i)
  num_games <- nrow(temp_data)
  if (i < 2020) {
    index <- i - 2002
  } else {
    index <- i - 2003
  }
  for (j in 1:num_games) {
    averages[index] <- averages[index] + (temp_data$Score.Diff[j] - temp_pred$Score.Diff[j])
  }
  averages[index] <- averages[index] / num_games
}

df <- data.frame(years = years, averages = averages)
ggplot(data = df, aes(x = years, y = averages)) + geom_bar(stat = "identity") +
  theme_bw() +
  xlab("Season") +
  ylab("Average Difference in Score") +
  ggtitle("Actual vs Predicted Score Differentials by Season")
```

![](Fitting-More-Models_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

*When we compare our fitted linear regression model to our observed
score differentials, we notice that our model is “relatively” good at
predicting score differentials, with 16 of the 19 observed seasons
having an average difference between actual and predicted score
differential within 2 points in either direction. One thing I noticed
when running the predict() function was that our model was a lot better
at predicting higher seed wins than lower seed wins, which makes sense
considering the model is based on seeds and ordinal rankings.*

#### Fitting a logistic regression model based on Result

``` r
for (i in 1:nrow(data)) {
  if (data$Result[i] == "Win") {
    data$Result[i] <- 1
  } else {
    data$Result[i] <- 0
  }
}
```

``` r
data$Result <- as.numeric(data$Result)
log_fit_full <- glm(as.numeric(Result) ~ Seed.Diff + Higher.Seed + COL.Diff + Higher.COL + DOL.Diff + Higher.DOL +
                   MOR.Diff + Higher.MOR + POM.Diff + Higher.POM +
                   SAG.Diff + Higher.SAG + WLK.Diff + Higher.WLK, data = data, family = "binomial")
summary(log_fit_full)
```

    ## 
    ## Call:
    ## glm(formula = as.numeric(Result) ~ Seed.Diff + Higher.Seed + 
    ##     COL.Diff + Higher.COL + DOL.Diff + Higher.DOL + MOR.Diff + 
    ##     Higher.MOR + POM.Diff + Higher.POM + SAG.Diff + Higher.SAG + 
    ##     WLK.Diff + Higher.WLK, family = "binomial", data = data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9572  -1.2066   0.6348   0.8820   1.4804  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)  0.525175   0.194972   2.694  0.00707 **
    ## Seed.Diff    0.008399   0.034867   0.241  0.80963   
    ## Higher.Seed -0.119581   0.043849  -2.727  0.00639 **
    ## COL.Diff     0.007618   0.007975   0.955  0.33940   
    ## Higher.COL   0.031770   0.016198   1.961  0.04983 * 
    ## DOL.Diff    -0.003990   0.008585  -0.465  0.64210   
    ## Higher.DOL  -0.011446   0.017839  -0.642  0.52111   
    ## MOR.Diff    -0.007188   0.004752  -1.513  0.13037   
    ## Higher.MOR  -0.012955   0.010910  -1.187  0.23508   
    ## POM.Diff    -0.003258   0.007337  -0.444  0.65702   
    ## Higher.POM   0.026239   0.016543   1.586  0.11273   
    ## SAG.Diff     0.005187   0.009750   0.532  0.59468   
    ## Higher.SAG   0.015245   0.021348   0.714  0.47516   
    ## WLK.Diff    -0.016161   0.009671  -1.671  0.09470 . 
    ## Higher.WLK  -0.025565   0.022634  -1.129  0.25870   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1467.7  on 1247  degrees of freedom
    ## Residual deviance: 1336.1  on 1233  degrees of freedom
    ## AIC: 1366.1
    ## 
    ## Number of Fisher Scoring iterations: 5

*As we can see from the summary output from above, only 3 of our
predictor variables are “statistically significant” at p = 0.05: our
intercept term, the higher seed variable, and the higher COL ordinal
ranking variable.*

``` r
pred <- data.frame(Season = data$Season, Result = predict(object = log_fit_full, type = "response"))

years <- seq(2003, 2022, by = 1)
years <- years[-c(18)]
averages <- rep(0, 19)
for (i in years) {
  temp_data <- data %>% filter(Season == i)
  temp_pred <- pred %>% filter(Season == i)
  num_games <- nrow(temp_data)
  if (i < 2020) {
    index <- i - 2002
  } else {
    index <- i - 2003
  }
  for (j in 1:num_games) {
    averages[index] <- averages[index] + (temp_data$Result[j] - temp_pred$Result[j])
  }
  averages[index] <- averages[index] / num_games
}

df <- data.frame(years = years, averages = averages)
ggplot(data = df, aes(x = years, y = averages)) + geom_bar(stat = "identity") +
  theme_bw() +
  xlab("Season") +
  ylab("Average Difference in Result") +
  ggtitle("Actual vs Predicted Results by Season")
```

![](Fitting-More-Models_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

*When we compare our fitted logistic regression model to our observed
results, we notice that our model is “relatively” good at predicting
score differentials, with 18 of the 19 observed seasons having an
average difference between actual and predicted result within 0.1
“prediction points” in either direction (prediction points can be seen
as average percentage of error in this case). One thing I noticed when
running the predict() function was that our model was a lot better at
predicting higher seed wins than lower seed wins, which makes sense
considering the model is based on seeds and ordinal rankings.*
