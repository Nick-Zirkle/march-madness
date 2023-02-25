Linear Regression
================

``` r
data <- read.csv("HistoricalDataset.csv")
```

Before proceeding with linear regression, I first wanted to think about
what I expected the estimated models to look like. Since
`Seed.Differential` and `Ordinal.Ranking.Differential` are calculated as
`winning team seed/ordinal ranking - losing team seed/ordinal ranking`,
and the lower seeds/ordinal rankings signify better teams, we would
expect the point estimates for these variables to be negative. This is
because extremely negative values for those variables mean that the
winning team had a much better seed/ordinal ranking than the losing
team, meaning we would want negative point estimates if we assume that a
higher point differential is more likely to occur between less
evenly-matched teams.

``` r
no_intercept_model <- lm(Point.Differential ~ Seed.Differential + Ordinal.Ranking.Differential - 1, data = data)
summary(no_intercept_model)
```

    ## 
    ## Call:
    ## lm(formula = Point.Differential ~ Seed.Differential + Ordinal.Ranking.Differential - 
    ##     1, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -18.667   1.529   6.814  12.603  43.814 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## Seed.Differential            -0.35274    0.09443  -3.735 0.000196 ***
    ## Ordinal.Ranking.Differential -0.09315    0.01005  -9.265  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.24 on 1179 degrees of freedom
    ## Multiple R-squared:  0.3895, Adjusted R-squared:  0.3885 
    ## F-statistic: 376.1 on 2 and 1179 DF,  p-value: < 2.2e-16

I decided to first fit a linear model without an intercept, since I was
interested in the difference between this model and the model with an
intercept. As we can see from our output, both of our point estimates
are negative, which supports my previous hypothesis above that point
differential is usually higher with less evenly-matched teams.
Additionally, both of our estimates are “statistically significant” at p
= 0.05, which is interesting to see. However, this model can predict
negative point differentials based on the seed/ordinal ranking
differentials, which is realistic for our eventual end goal of
predicting game results, but isn’t ideal when compared to our data,
since all of our point differentials in our data were positive.

``` r
intercept_model <- lm(Point.Differential ~ Seed.Differential + Ordinal.Ranking.Differential, data = data)
summary(intercept_model)
```

    ## 
    ## Call:
    ## lm(formula = Point.Differential ~ Seed.Differential + Ordinal.Ranking.Differential, 
    ##     data = data)
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

I then decided to fit a linear model with an intercept term, and our
results changed pretty drastically. Although it’s not practical to
analyze the intercept estimate in this case (since ordinal ranking
differential can never be equal to zero), having the average point
differential be about 9.5 for two “evenly-matched” teams (teams with no
difference in seed and ordinal ranking) doesn’t make sense in the
context of our situation, since I’d assume that the games would be
closer (on average) than 9-10 points. Additionally, while our ordinal
ranking differential point estimate is negative like the previous model,
our seed differential point estimate is positive. This estimate being
positive means that according to our model, the winning team having a
worse seed than the losing team adds to the average point differential,
which goes against my previous hypothesis. Furthermore, while the
intercept and ordinal ranking differential point estimates are both
“statistically significant” at p = 0.05, the seed differential estimate
is not “statistically significant” at this level, in addition to this
model also still being able to predict negative point differentials
based on the seed/ordinal ranking differentials. Based on the analysis
given of this model, we should potentially rethink the form of our model
or use the model with no intercept.

``` r
seed_intercept_model <- lm(Point.Differential ~ Seed.Differential, data = data)
summary(seed_intercept_model)
```

    ## 
    ## Call:
    ## lm(formula = Point.Differential ~ Seed.Differential, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.194  -6.190  -1.215   4.771  38.810 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        9.71682    0.26493   36.68   <2e-16 ***
    ## Seed.Differential -0.49824    0.03539  -14.08   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.96 on 1179 degrees of freedom
    ## Multiple R-squared:  0.1439, Adjusted R-squared:  0.1432 
    ## F-statistic: 198.2 on 1 and 1179 DF,  p-value: < 2.2e-16

Because the seed differential point estimate wasn’t “statistically
significant” at p = 0.05 in the previous model, I decided to explore
this variable more, and fit another model with an intercept term, but
without the ordinal ranking differential variable. Based on the summary
output, we once again find a similar intercept term point estimate to
the previous model (about 9.7), but this time we find that our point
estimate for the seed differential variable is negative, which agrees
with my previous hypothesis from above. Additionally, both point
estimates are “statistically significant” at p = 0.05, and this model
can’t predict negative point differentials, which matches our data
pretty well, meaning that this model is pretty good at estimating point
differentials for our data.

``` r
seed_no_intercept_model <- lm(Point.Differential ~ Seed.Differential - 1, data = data)
summary(seed_no_intercept_model)
```

    ## 
    ## Call:
    ## lm(formula = Point.Differential ~ Seed.Differential - 1, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.668   0.872   6.642  12.973  44.000 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## Seed.Differential -1.12833    0.04525  -24.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.64 on 1180 degrees of freedom
    ## Multiple R-squared:  0.3451, Adjusted R-squared:  0.3445 
    ## F-statistic: 621.7 on 1 and 1180 DF,  p-value: < 2.2e-16

I then fit a model without an intercept term and the ordinal ranking
differential variable. We can see from the summary output that our point
estimate for our seed differential variable is negative, which agrees
with our previous hypothesis, and that this point estimate is
“statistically significant” at p = 0.05. However, like our previous no
intercept model, this model can predict negative point differentials,
which doesn’t match with our data super well.

``` r
ordinal_intercept_model <- lm(Point.Differential ~ Ordinal.Ranking.Differential, data = data)
summary(ordinal_intercept_model)
```

    ## 
    ## Call:
    ## lm(formula = Point.Differential ~ Ordinal.Ranking.Differential, 
    ##     data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -18.084  -5.776  -1.307   4.507  34.438 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   9.433283   0.249732   37.77   <2e-16 ***
    ## Ordinal.Ranking.Differential -0.064368   0.003552  -18.12   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.608 on 1179 degrees of freedom
    ## Multiple R-squared:  0.2179, Adjusted R-squared:  0.2172 
    ## F-statistic: 328.5 on 1 and 1179 DF,  p-value: < 2.2e-16

I then decided to do the same thing as the previous two models, but for
the ordinal ranking differential variable instead of the seed
differential variable. We get a very similar point estimate for the
intercept term as the previous two intercept models (about 9.4), and we
also see that the point estimate for the ordinal ranking differential
variable is negative, which agrees with our previous hypothesis. Both
point estimates are also “statistically significant” at p = 0.05. But
even with the intercept term, this model can still predict negative
point differentials, which isn’t great for comparing to our data.

``` r
ordinal_no_intercept_model <- lm(Point.Differential ~ Ordinal.Ranking.Differential - 1, data = data)
summary(ordinal_no_intercept_model)
```

    ## 
    ## Call:
    ## lm(formula = Point.Differential ~ Ordinal.Ranking.Differential - 
    ##     1, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -20.301   1.907   6.632  12.655  43.747 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## Ordinal.Ranking.Differential -0.126446   0.004679  -27.02   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.31 on 1180 degrees of freedom
    ## Multiple R-squared:  0.3823, Adjusted R-squared:  0.3818 
    ## F-statistic: 730.3 on 1 and 1180 DF,  p-value: < 2.2e-16

I then continued the previous trend by fitting a model with no intercept
and the ordinal ranking differential variable. Our point estimate for
this variable is negative and “statistically significant” at p = 0.05,
but our model can once again predict negative point differentials,
making it likely a bad candidate for predicting our data.

``` r
anova(intercept_model, no_intercept_model, ordinal_intercept_model,
           ordinal_no_intercept_model, seed_intercept_model, seed_no_intercept_model)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: Point.Differential ~ Seed.Differential + Ordinal.Ranking.Differential
    ## Model 2: Point.Differential ~ Seed.Differential + Ordinal.Ranking.Differential - 
    ##     1
    ## Model 3: Point.Differential ~ Ordinal.Ranking.Differential
    ## Model 4: Point.Differential ~ Ordinal.Ranking.Differential - 1
    ## Model 5: Point.Differential ~ Seed.Differential
    ## Model 6: Point.Differential ~ Seed.Differential - 1
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1   1178  68127                                  
    ## 2   1179 149074 -1    -80947 1399.7 < 2.2e-16 ***
    ## 3   1179  68246  0     80828                     
    ## 4   1180 150838 -1    -82593 1428.1 < 2.2e-16 ***
    ## 5   1179  74700  1     76138 1316.5 < 2.2e-16 ***
    ## 6   1180 159928 -1    -85228 1473.7 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Because I had trouble deciding which linear model to use to estimate the
data, I decided to run `anova()` to compare the validity of each of the
prospective models. Based on the residual sum of squares output for each
model, it appears as though the two best models for estimating our data
are the intercept model with both seed and ordinal ranking differential
variables, and the intercept model with only the ordinal ranking
differential variable. The intercept model with only the seed
differential variable was also relatively good at predicting the data,
but not as good as the other two previously mentioned models.

``` r
anova(intercept_model, ordinal_intercept_model)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: Point.Differential ~ Seed.Differential + Ordinal.Ranking.Differential
    ## Model 2: Point.Differential ~ Ordinal.Ranking.Differential
    ##   Res.Df   RSS Df Sum of Sq      F Pr(>F)
    ## 1   1178 68127                           
    ## 2   1179 68246 -1   -119.05 2.0586 0.1516

I decided to then use `anova()` again to compare just the two previously
mentioned models. According to the output, it appears as though the
intercept model with only the ordinal ranking differential variable is
not “statistically significantly” better than the intercept model with
both variables at p = 0.05, meaning we should go proceed with our
intercept model with both variables, seen below:

Point.Differential = 9.5001 + 0.0932 \* Seed.Differential - 0.0727 \*
Ordinal.Ranking.Differential
