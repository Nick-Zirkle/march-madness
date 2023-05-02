NCAA Predictions Using Decision Tree
================
Nick Zirkle

#### 8. (1 EC point)

Rather than using DIC as a model selection criteria, compare your
model’s ability to predict outcomes during the 2021 and 2022 tournaments
(`ncca_test`).

Consider using classification error (win/loss) and Brier score
(`brier.scoer()` in the `iterativeBMA` package.)

``` r
ncaa <- read_csv('https://raw.githubusercontent.com/stat456/labs/main/Lab12_data.csv') %>%
  filter(Seed.Diff != 0) %>%
  mutate(Seed.Diff = -1 * Seed.Diff,
         SAG.Diff = -1 * SAG.Diff,
         upset = as.numeric(Result == 'Loss'))
ncaa_train <- ncaa %>% filter(Season < 2020)
ncaa_test <- ncaa %>% filter(Season >= 2020)
ncaa_train <- ncaa_train[, c(1, 4:7)]
ncaa_test <- ncaa_test[, c(1, 4:7)]
```

``` r
tree <- rpart(Result ~ ., data = ncaa_train)
tree.survived.predicted <- predict(tree, ncaa_test, type = "class")
comparison <- data.frame(ncaa_test$Result, tree.survived.predicted)
head(comparison)
```

    ##   ncaa_test.Result tree.survived.predicted
    ## 1              Win                     Win
    ## 2              Win                     Win
    ## 3              Win                     Win
    ## 4              Win                     Win
    ## 5              Win                     Win
    ## 6              Win                     Win

``` r
confusionMatrix(tree.survived.predicted, as.factor(ncaa_test$Result))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction Loss Win
    ##       Loss    2   1
    ##       Win    38  83
    ##                                          
    ##                Accuracy : 0.6855         
    ##                  95% CI : (0.596, 0.7659)
    ##     No Information Rate : 0.6774         
    ##     P-Value [Acc > NIR] : 0.4663         
    ##                                          
    ##                   Kappa : 0.0503         
    ##                                          
    ##  Mcnemar's Test P-Value : 8.185e-09      
    ##                                          
    ##             Sensitivity : 0.05000        
    ##             Specificity : 0.98810        
    ##          Pos Pred Value : 0.66667        
    ##          Neg Pred Value : 0.68595        
    ##              Prevalence : 0.32258        
    ##          Detection Rate : 0.01613        
    ##    Detection Prevalence : 0.02419        
    ##       Balanced Accuracy : 0.51905        
    ##                                          
    ##        'Positive' Class : Loss           
    ## 

*As we can see from the above output, our prediction algorithm using a
decision tree is 68.55% accurate in predicting the result of games from
the 2021 and 2022 NCAA tournaments. It appears as though our algorithm
underestimates the amount of upsets, with only 2 of the 40 “upsets”
being predicted by the algorithm, as opposed to 83/84 non-upsets being
predicted by the algorithm.*

``` r
prp(tree)
```

![](NCAA-Predictions-Using-Decision-Tree_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

*As we can see from the decision tree above, it appears as though the
only statistic that the decision tree used was* `SAG.Diff` *where a win
was predicted for any value over -11. It’s important to note that all
values for* `SAG.Diff` *were calculated based on the higher seed, so the
only time that negative values were recorded were if the higher seed had
a worse Sagarin rating than the lower seed (only happened 9/124 times in
2021 and 2022), which is why our model tended to underestimate the
number of upsets that occurred.*
