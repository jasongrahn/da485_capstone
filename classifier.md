classifier
================
jason grahn
5/25/2019

``` r
#load training
training <- read_csv(here::here("data/train_data.csv")) %>% 
  dplyr::select(-contains("X"), -id)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   institution_name = col_character(),
    ##   basic = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
#load testing
testing <- read_csv(here::here("data/test_data.csv")) %>% 
  dplyr::select(-contains("X"), -id) 
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   institution_name = col_character(),
    ##   basic = col_character()
    ## )
    ## See spec(...) for full column specifications.

# Rebuilding models

## model 1 to find outliers

(this repeats Di’s
analysis)

``` r
model.1 <- lm(grad_rate ~ student_count +spending_per_award +full_time_pct
             +full_time_count +med_sat_value +aid_value
             +endow_value +grad_on_time_pct +pell_value +fresh_retain_value
             + full_time_fac_pct,
             data = training)

#remove outliers using model 1
training_without_outliers <- add_residuals(data = training,
                                           model = model.1,
                                           var = "resid") %>% 
  mutate(sdev = sd(resid),
                std.norm = resid / sdev,
                abs.std = abs(std.norm)) %>% #note: 407 rows at this point
   #dplyr::arrange(desc(abs.std)) %>% 
  filter(abs.std < 2)
```

## model 10 to build predictions

``` r
model.10 <- lm(grad_rate ~ spending_per_award 
               + full_time_count
               + med_sat_value
               + endow_value
               + grad_on_time_pct
               + pell_value
               + fresh_retain_value
             + endow_value*spending_per_award     #EndowXSpend
             + pell_value*med_sat_value           #PellXSat
             + fresh_retain_value*med_sat_value   #RetainXSat
             ,data = training_without_outliers)
summary(model.10)
```

    ## 
    ## Call:
    ## lm(formula = grad_rate ~ spending_per_award + full_time_count + 
    ##     med_sat_value + endow_value + grad_on_time_pct + pell_value + 
    ##     fresh_retain_value + endow_value * spending_per_award + pell_value * 
    ##     med_sat_value + fresh_retain_value * med_sat_value, data = training_without_outliers)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.090583 -0.019322 -0.002423  0.018602  0.099142 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                       8.082e-01  1.967e-01   4.109 4.89e-05
    ## spending_per_award               -7.724e-07  6.446e-08 -11.982  < 2e-16
    ## full_time_count                   9.008e-07  2.184e-07   4.125 4.57e-05
    ## med_sat_value                    -6.143e-04  1.958e-04  -3.138 0.001834
    ## endow_value                      -2.216e-06  5.588e-07  -3.965 8.77e-05
    ## grad_on_time_pct                  1.030e-01  1.257e-02   8.189 4.15e-15
    ## pell_value                       -5.199e-01  1.372e-01  -3.790 0.000175
    ## fresh_retain_value               -4.762e-01  2.017e-01  -2.361 0.018733
    ## spending_per_award:endow_value    2.709e-11  6.687e-12   4.051 6.21e-05
    ## med_sat_value:pell_value          5.257e-04  1.381e-04   3.807 0.000164
    ## med_sat_value:fresh_retain_value  5.096e-04  2.008e-04   2.538 0.011566
    ##                                     
    ## (Intercept)                      ***
    ## spending_per_award               ***
    ## full_time_count                  ***
    ## med_sat_value                    ** 
    ## endow_value                      ***
    ## grad_on_time_pct                 ***
    ## pell_value                       ***
    ## fresh_retain_value               *  
    ## spending_per_award:endow_value   ***
    ## med_sat_value:pell_value         ***
    ## med_sat_value:fresh_retain_value *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02798 on 376 degrees of freedom
    ## Multiple R-squared:  0.5046, Adjusted R-squared:  0.4915 
    ## F-statistic:  38.3 on 10 and 376 DF,  p-value: < 2.2e-16

## putting predictions into training

``` r
training_with_predictions <- add_predictions(training_without_outliers, model.10, var = "pred_grad_rate") %>% 
  add_residuals(model = model.10, var = "resid") %>% 
  #and we dont need these other variables either
  dplyr::select(-sdev, -std.norm, -abs.std) %>% 
  mutate(scaled = scale(pred_grad_rate))
```

## and some plotting with the training data

This builds out reqs for plotting the testing data.

``` r
#and a quick visual of known graduation to predicuted graduation
#training_predictions_scatter <- training_with_predictions %>% 
#  ggplot(aes(x = grad_rate, y = pred_grad_rate)) +
#  geom_point() + 
#  geom_smooth(method = "lm") + 
#  geom_rug() +
#  theme_light()
##################################################
##                                              ##
## ^^^^ TAKE THIS OUT AND PUT IN A QQ PLOT ^^^^ ##
##                                              ##
##################################################

training_QQ <- training_with_predictions %>% 
  ggplot(aes(sample = pred_grad_rate)) +
  stat_qq() +
  theme_light() +
  labs(title = "Training plots...")


training_predictions_boxplot <- training_with_predictions %>% 
  arrange(basic) %>% 
  ggplot(aes(y = pred_grad_rate, x = "x")) +
  geom_boxplot() + 
  geom_jitter(alpha = 0.2, width = .09) +
  theme_light() +
  labs(title = "...do not use for presi")

#histogram of scaled predictions
training_predictions_histogram <- training_with_predictions %>% 
  ggplot() +
  geom_histogram(aes(x = pred_grad_rate), bins = 25) +
  theme_light() 

training_pred_vs_resids <- training_with_predictions %>% 
  ggplot() +
  geom_point(aes(x = pred_grad_rate, y = resid)) +
  theme_light() 

cowplot::plot_grid(training_QQ, 
                   training_predictions_boxplot, 
                   training_predictions_histogram,
                   training_pred_vs_resids,
                   labels = c("1", "2", "3", "4"))
```

![](classifier_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

1.  QQ plot shows generally normal behavior with a few
outliers.

2.  
3.  
4.  
# Testing

## Application of the model

``` r
testing_with_predictions <- add_predictions(testing, model = model.10, var = "pred_grad_rate") %>% 
  add_residuals(model = model.10, var = "resid") %>% 
  arrange(pred_grad_rate)
```

``` r
#and a quick visual of known graduation to predicuted graduation
#testing_predictions_scatter <- testing_with_predictions %>% 
#  ggplot(aes(x = grad_rate, y = pred_grad_rate)) +
#  geom_point() + 
#  geom_smooth(method = "lm") + 
#  geom_rug() +
#  theme_light()
##################################################
##                                              ##
## ^^^^ TAKE THIS OUT AND PUT IN A QQ PLOT ^^^^ ##
##                                              ##
##################################################

testing_QQ <- testing_with_predictions %>% 
  ggplot(aes(sample = pred_grad_rate)) +
  stat_qq() +
  theme_light() +
  labs(title = "QQ Plot")

#histogram of scaled predictions
testing_predictions_histogram <- testing_with_predictions %>% 
  ggplot() +
  geom_histogram(aes(x = pred_grad_rate), bins = 25) +
  theme_light() +
  labs(title = "Histogram of predictions")

#boxplot the predictions
testing_predictions_boxplot <- testing_with_predictions %>% 
  arrange(basic) %>% 
  ggplot(aes(y = pred_grad_rate, x = "x")) +
  geom_boxplot() + 
  geom_jitter(alpha = 0.2, width = .09, color = "darkblue") +
  theme_light() +
  labs(title = "Boxplot of predictions",
       x = "")

#scatterplot predictions vs residuals
testing_pred_vs_resids <- testing_with_predictions %>% 
  ggplot(aes(x = pred_grad_rate, y = resid, 
             label = round(pred_grad_rate,2))) +
  geom_point() +
 # geom_label() +
  theme_light() +
  labs(title = "Scatterplot of residuals to predictions")

testing_pred_vs_resids
```

![](classifier_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
cowplot_results <- cowplot::plot_grid(testing_QQ, 
                   testing_predictions_boxplot, 
                   testing_predictions_histogram, 
                   testing_pred_vs_resids,
                   labels = c("1", "2", "3", "4"))
cowplot_results
```

![](classifier_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
ggsave("results_cowplot.jpg")
```

    ## Saving 7 x 5 in image

1.  QQ plot shows generally normal behavior with a few outliers on the
    top end; much like the training data.

2.  The boxplot provides an additional view of distribution with a tight
    distribution round the median.

3.  Histogram reinforces normal distribution.

4.  Scatterplot of residuals to predictions is generally equal around
    zero, with a few outliers.

# Classification

A classifier should be as simple as possible for the given application.
We investigated a variety of methods to automatically find and classify
five performance buckets. In the end, the simplest model is also the
most easily understood.

After predicting and scaling graduations rates for each of the colleges
in the testing data, we identified that they sit in a generally normal
distribution. Using the scaled values of the prediction, we plot a
standard normal distribution. \[fill this out better\]

``` r
# add classification buckets
testing_with_predictions <- testing_with_predictions %>% 
  mutate(scaled_act = scale(grad_rate), 
         scaled_pred = scale(pred_grad_rate),
         Classifications = factor(case_when(scaled_pred <= (-3) ~ "Poor",
                                           scaled_pred <= (-2) ~ "Below Average",
                                           scaled_pred <= (1) ~ "Average",
                                           scaled_pred <= (2) ~ "Above Average",
                                           scaled_pred > (2) ~ "Excellent"),
                                 levels = c("Excellent", "Above Average", "Average", "Below Average", "Poor"))) 

# build some lines
vlines <- as.matrix(testing_with_predictions %>% 
                      filter(institution_name == "Bellevue College") %>% 
                      select(scaled_pred, scaled_act))

# force coloration
colorlist <- c("chartreuse4", "green3", "ivory4", "orangered2", "darkred")

# visualize the classification
classified_histogram <- testing_with_predictions %>% 
  ggplot() +
  geom_histogram(aes(x = scaled_pred, fill = Classifications), binwidth = .15) +
  geom_vline(aes(xintercept = vlines[1]), color = "blue", size = 1.5) +
  geom_vline(aes(xintercept = vlines[2]), color = "red", size = 1.5) +
  theme_light() +
  theme(legend.position="right") +
  labs(title = "Histogram of Graduation Predictions",
       subtitle = "BC shows Average rates for both actual and prediction",
       x = "Predicted Graduation Rate (scaled)",
       caption = "Red line is BC actual graduation rate,
  Blue line is BC predicted grad rate") +
  scale_fill_manual(values = colorlist)
classified_histogram
```

![](classifier_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
classified_histogram <- testing_with_predictions %>% 
  ggplot() +
  geom_histogram(aes(x = pred_grad_rate, fill = Classifications)) +
  theme_light() +
  theme(legend.position="right") +
  labs(title = "Histogram of Graduation Predictions",
       subtitle = "BC shows Average rates for both actual and prediction",
       x = "Predicted Graduation Rate (scaled)",
       caption = "Red line is BC actual graduation rate,
  Blue line is BC predicted grad rate") +
  scale_fill_manual(values = colorlist)
classified_histogram
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](classifier_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
ggsave("classified_histogram.jpg")
```

    ## Saving 7 x 5 in image
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

``` r
#show the worst outliers
head(testing_with_predictions %>% 
  mutate(delta = round(pred_grad_rate - grad_rate,3),
         delta = abs(delta)) %>% 
  arrange(desc(delta)) %>% 
  select(institution_name, pred_grad_rate, grad_rate, delta)
  , 10)
```

    ## # A tibble: 10 x 4
    ##    institution_name                          pred_grad_rate grad_rate delta
    ##    <chr>                                              <dbl>     <dbl> <dbl>
    ##  1 Virginia Military Institute                        0.424     0.179 0.245
    ##  2 Pennsylvania State University-Lehigh Val…          0.242     0.084 0.158
    ##  3 Texas A & M University-Texarkana                   0.174     0.329 0.155
    ##  4 Bismarck State College                             0.201     0.355 0.154
    ##  5 Southern Illinois University at Carbonda…          0.171     0.322 0.151
    ##  6 Pennsylvania State University-Altoona              0.257     0.106 0.151
    ##  7 University of South Florida-Sarasota-Man…          0.207     0.345 0.138
    ##  8 Metropolitan State University                      0.206     0.338 0.132
    ##  9 Fort Hays State University                         0.217     0.347 0.13 
    ## 10 Pennsylvania State University-Berks                0.242     0.114 0.128

``` r
# 
testing_with_predictions %>% 
  select(pred_grad_rate) %>% 
  summarize(mean_cl = mean(pred_grad_rate),
            stddev = sd(pred_grad_rate),
            stddev_3 = stddev * 3,
            stddev_2 = stddev * 2,
            stddev_1 = stddev * 1,
            low = mean_cl - stddev_3,
            medlow = mean_cl - stddev_2,
            medhigh = mean_cl + stddev_2,
            high = mean_cl + stddev)
```

    ## # A tibble: 1 x 9
    ##   mean_cl stddev stddev_3 stddev_2 stddev_1    low medlow medhigh  high
    ##     <dbl>  <dbl>    <dbl>    <dbl>    <dbl>  <dbl>  <dbl>   <dbl> <dbl>
    ## 1   0.200 0.0337    0.101   0.0674   0.0337 0.0984  0.132   0.267 0.233
