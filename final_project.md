Final Project
================

``` r
library(gtsummary)
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.3

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.2 --
    ## v ggplot2 3.3.6     v purrr   1.0.1
    ## v tibble  3.2.1     v dplyr   1.1.2
    ## v tidyr   1.2.1     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 1.0.0

    ## Warning: package 'ggplot2' was built under R version 4.1.3

    ## Warning: package 'tibble' was built under R version 4.1.3

    ## Warning: package 'tidyr' was built under R version 4.1.3

    ## Warning: package 'readr' was built under R version 4.1.3

    ## Warning: package 'purrr' was built under R version 4.1.3

    ## Warning: package 'dplyr' was built under R version 4.1.3

    ## Warning: package 'forcats' was built under R version 4.1.3

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(car)
```

    ## Warning: package 'car' was built under R version 4.1.2

    ## Loading required package: carData
    ## 
    ## Attaching package: 'car'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.1.3

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(corrplot)
```

    ## Warning: package 'corrplot' was built under R version 4.1.3

    ## corrplot 0.92 loaded

``` r
library(glmnet)
```

    ## Warning: package 'glmnet' was built under R version 4.1.3

    ## Loading required package: Matrix

    ## Warning: package 'Matrix' was built under R version 4.1.3

    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack
    ## 
    ## Loaded glmnet 4.1-7

``` r
library(leaps)
```

    ## Warning: package 'leaps' was built under R version 4.1.3

``` r
library(pROC)
```

    ## Warning: package 'pROC' was built under R version 4.1.2

    ## Type 'citation("pROC")' for a citation.
    ## 
    ## Attaching package: 'pROC'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
library(broom)

set.seed(123)

knitr::opts_chunk$set(
    echo = TRUE,
    warning = FALSE,
    fig.width = 8, 
  fig.height = 6,
  out.width = "90%"
)

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d

theme_set(theme_minimal() + theme(legend.position = "bottom"))
```

``` r
data = read_csv("./data/data.csv") |>
  janitor::clean_names()
```

    ## Rows: 4024 Columns: 16
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (11): Race, Marital Status, T Stage, N Stage, 6th Stage, differentiate, ...
    ## dbl  (5): Age, Tumor Size, Regional Node Examined, Reginol Node Positive, Su...
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data = data |>
  select(-survival_months)
```

``` r
# Convert character to factor for regression analysis
clean_data = data |>
  mutate(
    race = as.numeric(factor(race, levels = c("White", "Black", "Other"))) - 1,
    marital_status = as.numeric(factor(marital_status, levels = c("Married", "Single", "Divorced", "Widowed", "Separated"))) - 1,
    t_stage = as.numeric(factor(t_stage, levels = c("T1", "T2", "T3", "T4"))) - 1,
    n_stage = as.numeric(factor(n_stage, levels = c("N1", "N2", "N3"))) - 1,
    x6th_stage = as.numeric(factor(x6th_stage, levels = c("IIA", "IIB", "IIIA", "IIIB", "IIIC"))) - 1,
    differentiate = as.numeric(factor(differentiate, levels = c("Undifferentiated", "Poorly differentiated", "Moderately differentiated", "Well differentiated"))) - 1,
    grade = as.numeric(factor(grade, levels = c("1", "2", "3", "anaplastic; Grade IV"))),
    a_stage = as.numeric(factor(a_stage, levels = c("Regional", "Distant"))) - 1,
    estrogen_status = as.numeric(factor(estrogen_status, levels = c("Negative", "Positive"))) - 1,
    progesterone_status = as.numeric(factor(progesterone_status, levels = c("Negative", "Positive"))) - 1,
    status = as.numeric(factor(status, levels = c("Dead", "Alive"))) - 1)|>
  rename(regional_node_positive = reginol_node_positive)
```

\#Model fitting \#Based on boxplots, transformaiton is necesessary to
reduce outliers \#cube root of tumor size \#log of regional_node_examied
\#log of regional_node_positive \#Figure 1

``` r
proj2 = data |>
tbl_summary(by="status",
  missing_text = "(Missing)", # counts missing values
  statistic = list(all_continuous() ~ "mean={mean} (min={min}, max={max}, sd={sd})",
                   all_categorical() ~ "n={n} (p={p}%)") # stats for categorical
  ) |>
bold_labels()  |>
italicize_levels()


clean_data2=clean_data
clean_data2$tumor_size= (clean_data$tumor_size)^(1/3)
clean_data2$regional_node_examined = log(clean_data$regional_node_examined)
clean_data2$regional_node_positive = log(clean_data$regional_node_positive)
```

\#Find correlation

``` r
corplot=cor(clean_data2)
corrplot(corplot)
```

<img src="final_project_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

``` r
#tumor_size vs t_stage = 0.801
#grade=differentiate =>1
#n_stage = x6th_stage => 0.881
#n_stage = regional positive status =>0.838073333
selected_data = clean_data2 |>
  select(-tumor_size, -grade,-n_stage,-regional_node_positive,-x6th_stage)

corplot=cor(selected_data)
corrplot(corplot)
```

<img src="final_project_files/figure-gfm/unnamed-chunk-5-2.png" width="90%" />

\#Separate training and testing set (80% training 20% testing )

``` r
# Calculate the size of each of the data sets
data_size = nrow(clean_data2)
train_size = floor(0.8 * data_size)

# Create a random sample of row indices for the training set
train_indices = sample(sample(seq_len(data_size), size = train_size))

# Subset the data into training and testing sets
train_set = clean_data2[train_indices, ]
selectedData_train_set = selected_data[train_indices,]
test_set = clean_data2[-train_indices, ]
selectedData_test_set = selected_data[-train_indices, ]
```

# Fit a full model

``` r
selected_train = train_set |>
  select(-tumor_size, -grade,-n_stage,-regional_node_positive,-x6th_stage)

null_model = glm(status ~ 1, family = binomial(link = "logit"), data = selected_train)

full_model=glm(status ~ . , family = binomial(link = "logit"), data = selected_train)

interaction_race_age = glm(status ~ . + race:age, family = binomial(link = "logit"), data = selected_train)

interaction_race_marital_status = glm(status ~ . + race:marital_status,  family = binomial(link = "logit"), data = selected_train)
```

# Check logistic regression assumptions

Binary logistic regression relies on underlying assumptions to be true:

1.The outcome is a binary or dichotomous variable like yes vs no,
positive vs negative, 1 vs 0. 2.There is a linear relationship between
the logit of the outcome(status) and each predictor variables. Recall
that the logit function is logit(p) = log(p/(1-p)), where p is the
probabilities of the outcome. 3.There is no influential values in the
continuous predictors. 4.There is no multicollinearity among the
predictors.

## Checking Linearity of continuous variables to the response

\##HENRY - MAY BE NOT NECESSARY

# Using Forward, BackWard

    ## Start:  AIC=2782.19
    ## status ~ 1
    ## 
    ##                          Df Deviance    AIC
    ## + progesterone_status     1   2684.9 2688.9
    ## + estrogen_status         1   2689.6 2693.6
    ## + differentiate           1   2698.9 2702.9
    ## + t_stage                 1   2705.8 2709.8
    ## + marital_status          1   2758.5 2762.5
    ## + a_stage                 1   2762.0 2766.0
    ## + age                     1   2768.2 2772.2
    ## + regional_node_examined  1   2777.7 2781.7
    ## <none>                        2780.2 2782.2
    ## + race                    1   2780.2 2784.2
    ## 
    ## Step:  AIC=2688.88
    ## status ~ progesterone_status
    ## 
    ##                          Df Deviance    AIC
    ## + t_stage                 1   2620.4 2626.4
    ## + differentiate           1   2632.0 2638.0
    ## + estrogen_status         1   2659.5 2665.5
    ## + marital_status          1   2666.5 2672.5
    ## + a_stage                 1   2668.0 2674.0
    ## + age                     1   2674.2 2680.2
    ## <none>                        2684.9 2688.9
    ## + regional_node_examined  1   2683.0 2689.0
    ## + race                    1   2684.9 2690.9
    ## 
    ## Step:  AIC=2626.4
    ## status ~ progesterone_status + t_stage
    ## 
    ##                          Df Deviance    AIC
    ## + differentiate           1   2578.9 2586.9
    ## + estrogen_status         1   2597.8 2605.8
    ## + marital_status          1   2602.2 2610.2
    ## + age                     1   2605.3 2613.3
    ## + a_stage                 1   2616.4 2624.4
    ## <none>                        2620.4 2626.4
    ## + regional_node_examined  1   2620.2 2628.2
    ## + race                    1   2620.4 2628.4
    ## 
    ## Step:  AIC=2586.92
    ## status ~ progesterone_status + t_stage + differentiate
    ## 
    ##                          Df Deviance    AIC
    ## + age                     1   2557.0 2567.0
    ## + marital_status          1   2560.7 2570.7
    ## + estrogen_status         1   2565.2 2575.2
    ## + a_stage                 1   2575.2 2585.2
    ## <none>                        2578.9 2586.9
    ## + regional_node_examined  1   2578.9 2588.9
    ## + race                    1   2578.9 2588.9
    ## 
    ## Step:  AIC=2567
    ## status ~ progesterone_status + t_stage + differentiate + age
    ## 
    ##                          Df Deviance    AIC
    ## + estrogen_status         1   2539.7 2551.7
    ## + marital_status          1   2544.1 2556.1
    ## + a_stage                 1   2552.8 2564.8
    ## <none>                        2557.0 2567.0
    ## + race                    1   2556.9 2568.9
    ## + regional_node_examined  1   2557.0 2569.0
    ## 
    ## Step:  AIC=2551.74
    ## status ~ progesterone_status + t_stage + differentiate + age + 
    ##     estrogen_status
    ## 
    ##                          Df Deviance    AIC
    ## + marital_status          1   2526.1 2540.1
    ## + a_stage                 1   2536.5 2550.5
    ## <none>                        2539.7 2551.7
    ## + race                    1   2539.7 2553.7
    ## + regional_node_examined  1   2539.7 2553.7
    ## 
    ## Step:  AIC=2540.14
    ## status ~ progesterone_status + t_stage + differentiate + age + 
    ##     estrogen_status + marital_status
    ## 
    ##                          Df Deviance    AIC
    ## + a_stage                 1   2522.9 2538.9
    ## <none>                        2526.1 2540.1
    ## + regional_node_examined  1   2526.1 2542.1
    ## + race                    1   2526.1 2542.1
    ## 
    ## Step:  AIC=2538.89
    ## status ~ progesterone_status + t_stage + differentiate + age + 
    ##     estrogen_status + marital_status + a_stage
    ## 
    ##                          Df Deviance    AIC
    ## <none>                        2522.9 2538.9
    ## + race                    1   2522.9 2540.9
    ## + regional_node_examined  1   2522.9 2540.9

    ## Start:  AIC=2542.89
    ## status ~ age + race + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status + regional_node_examined
    ## 
    ##                          Df Deviance    AIC
    ## - regional_node_examined  1   2522.9 2540.9
    ## - race                    1   2522.9 2540.9
    ## <none>                        2522.9 2542.9
    ## - a_stage                 1   2526.1 2544.1
    ## - marital_status          1   2536.5 2554.5
    ## - estrogen_status         1   2539.8 2557.8
    ## - progesterone_status     1   2541.6 2559.6
    ## - age                     1   2542.5 2560.5
    ## - differentiate           1   2559.6 2577.6
    ## - t_stage                 1   2567.4 2585.4
    ## 
    ## Step:  AIC=2540.89
    ## status ~ age + race + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status
    ## 
    ##                       Df Deviance    AIC
    ## - race                 1   2522.9 2538.9
    ## <none>                     2522.9 2540.9
    ## - a_stage              1   2526.1 2542.1
    ## - marital_status       1   2536.5 2552.5
    ## - estrogen_status      1   2539.8 2555.8
    ## - progesterone_status  1   2541.6 2557.6
    ## - age                  1   2542.5 2558.5
    ## - differentiate        1   2559.8 2575.8
    ## - t_stage              1   2567.8 2583.8
    ## 
    ## Step:  AIC=2538.89
    ## status ~ age + marital_status + t_stage + differentiate + a_stage + 
    ##     estrogen_status + progesterone_status
    ## 
    ##                       Df Deviance    AIC
    ## <none>                     2522.9 2538.9
    ## - a_stage              1   2526.1 2540.1
    ## - marital_status       1   2536.5 2550.5
    ## - estrogen_status      1   2539.9 2553.9
    ## - progesterone_status  1   2541.6 2555.6
    ## - age                  1   2542.7 2556.7
    ## - differentiate        1   2559.8 2573.8
    ## - t_stage              1   2567.8 2581.8

    ## 
    ## Call:
    ## glm(formula = status ~ age + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status, family = binomial(link = "logit"), 
    ##     data = selected_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5313   0.3530   0.4539   0.5824   1.7480  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          1.570637   0.359045   4.374 1.22e-05 ***
    ## age                 -0.026116   0.005936  -4.400 1.08e-05 ***
    ## marital_status      -0.180202   0.048001  -3.754 0.000174 ***
    ## t_stage             -0.444968   0.066122  -6.729 1.70e-11 ***
    ## differentiate        0.510408   0.084940   6.009 1.87e-09 ***
    ## a_stage             -0.506600   0.275308  -1.840 0.065750 .  
    ## estrogen_status      0.789337   0.190743   4.138 3.50e-05 ***
    ## progesterone_status  0.617193   0.138443   4.458 8.27e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2780.2  on 3218  degrees of freedom
    ## Residual deviance: 2522.9  on 3211  degrees of freedom
    ## AIC: 2538.9
    ## 
    ## Number of Fisher Scoring iterations: 5

    ## 
    ## Call:
    ## glm(formula = status ~ progesterone_status + t_stage + differentiate + 
    ##     age + estrogen_status + marital_status + a_stage, family = binomial(link = "logit"), 
    ##     data = selected_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5313   0.3530   0.4539   0.5824   1.7480  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          1.570637   0.359045   4.374 1.22e-05 ***
    ## progesterone_status  0.617193   0.138443   4.458 8.27e-06 ***
    ## t_stage             -0.444968   0.066122  -6.729 1.70e-11 ***
    ## differentiate        0.510408   0.084940   6.009 1.87e-09 ***
    ## age                 -0.026116   0.005936  -4.400 1.08e-05 ***
    ## estrogen_status      0.789337   0.190743   4.138 3.50e-05 ***
    ## marital_status      -0.180202   0.048001  -3.754 0.000174 ***
    ## a_stage             -0.506600   0.275308  -1.840 0.065750 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2780.2  on 3218  degrees of freedom
    ## Residual deviance: 2522.9  on 3211  degrees of freedom
    ## AIC: 2538.9
    ## 
    ## Number of Fisher Scoring iterations: 5

    ## Analysis of Deviance Table
    ## 
    ## Model 1: status ~ age + marital_status + t_stage + differentiate + a_stage + 
    ##     estrogen_status + progesterone_status
    ## Model 2: status ~ progesterone_status + t_stage + differentiate + age + 
    ##     estrogen_status + marital_status + a_stage
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      3211     2522.9                     
    ## 2      3211     2522.9  0        0

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Area under the curve: 0.7

# Interaction: race + age

``` r
step_modelF_1 = step(null_model, scope = list(lower = null_model, upper = interaction_race_age), 
                   direction = "forward")
```

    ## Start:  AIC=2782.19
    ## status ~ 1
    ## 
    ##                          Df Deviance    AIC
    ## + progesterone_status     1   2684.9 2688.9
    ## + estrogen_status         1   2689.6 2693.6
    ## + differentiate           1   2698.9 2702.9
    ## + t_stage                 1   2705.8 2709.8
    ## + marital_status          1   2758.5 2762.5
    ## + a_stage                 1   2762.0 2766.0
    ## + age                     1   2768.2 2772.2
    ## + regional_node_examined  1   2777.7 2781.7
    ## <none>                        2780.2 2782.2
    ## + race                    1   2780.2 2784.2
    ## 
    ## Step:  AIC=2688.88
    ## status ~ progesterone_status
    ## 
    ##                          Df Deviance    AIC
    ## + t_stage                 1   2620.4 2626.4
    ## + differentiate           1   2632.0 2638.0
    ## + estrogen_status         1   2659.5 2665.5
    ## + marital_status          1   2666.5 2672.5
    ## + a_stage                 1   2668.0 2674.0
    ## + age                     1   2674.2 2680.2
    ## <none>                        2684.9 2688.9
    ## + regional_node_examined  1   2683.0 2689.0
    ## + race                    1   2684.9 2690.9
    ## 
    ## Step:  AIC=2626.4
    ## status ~ progesterone_status + t_stage
    ## 
    ##                          Df Deviance    AIC
    ## + differentiate           1   2578.9 2586.9
    ## + estrogen_status         1   2597.8 2605.8
    ## + marital_status          1   2602.2 2610.2
    ## + age                     1   2605.3 2613.3
    ## + a_stage                 1   2616.4 2624.4
    ## <none>                        2620.4 2626.4
    ## + regional_node_examined  1   2620.2 2628.2
    ## + race                    1   2620.4 2628.4
    ## 
    ## Step:  AIC=2586.92
    ## status ~ progesterone_status + t_stage + differentiate
    ## 
    ##                          Df Deviance    AIC
    ## + age                     1   2557.0 2567.0
    ## + marital_status          1   2560.7 2570.7
    ## + estrogen_status         1   2565.2 2575.2
    ## + a_stage                 1   2575.2 2585.2
    ## <none>                        2578.9 2586.9
    ## + regional_node_examined  1   2578.9 2588.9
    ## + race                    1   2578.9 2588.9
    ## 
    ## Step:  AIC=2567
    ## status ~ progesterone_status + t_stage + differentiate + age
    ## 
    ##                          Df Deviance    AIC
    ## + estrogen_status         1   2539.7 2551.7
    ## + marital_status          1   2544.1 2556.1
    ## + a_stage                 1   2552.8 2564.8
    ## <none>                        2557.0 2567.0
    ## + race                    1   2556.9 2568.9
    ## + regional_node_examined  1   2557.0 2569.0
    ## 
    ## Step:  AIC=2551.74
    ## status ~ progesterone_status + t_stage + differentiate + age + 
    ##     estrogen_status
    ## 
    ##                          Df Deviance    AIC
    ## + marital_status          1   2526.1 2540.1
    ## + a_stage                 1   2536.5 2550.5
    ## <none>                        2539.7 2551.7
    ## + race                    1   2539.7 2553.7
    ## + regional_node_examined  1   2539.7 2553.7
    ## 
    ## Step:  AIC=2540.14
    ## status ~ progesterone_status + t_stage + differentiate + age + 
    ##     estrogen_status + marital_status
    ## 
    ##                          Df Deviance    AIC
    ## + a_stage                 1   2522.9 2538.9
    ## <none>                        2526.1 2540.1
    ## + regional_node_examined  1   2526.1 2542.1
    ## + race                    1   2526.1 2542.1
    ## 
    ## Step:  AIC=2538.89
    ## status ~ progesterone_status + t_stage + differentiate + age + 
    ##     estrogen_status + marital_status + a_stage
    ## 
    ##                          Df Deviance    AIC
    ## <none>                        2522.9 2538.9
    ## + race                    1   2522.9 2540.9
    ## + regional_node_examined  1   2522.9 2540.9

``` r
step_model_1 = step(interaction_race_age, direction = "backward")
```

    ## Start:  AIC=2544.32
    ## status ~ age + race + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status + regional_node_examined + 
    ##     race:age
    ## 
    ##                          Df Deviance    AIC
    ## - regional_node_examined  1   2522.3 2542.3
    ## - age:race                1   2522.9 2542.9
    ## <none>                        2522.3 2544.3
    ## - a_stage                 1   2525.4 2545.4
    ## - marital_status          1   2536.1 2556.1
    ## - estrogen_status         1   2539.4 2559.4
    ## - progesterone_status     1   2541.1 2561.1
    ## - differentiate           1   2559.2 2579.2
    ## - t_stage                 1   2566.9 2586.9
    ## 
    ## Step:  AIC=2542.32
    ## status ~ age + race + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status + age:race
    ## 
    ##                       Df Deviance    AIC
    ## - age:race             1   2522.9 2540.9
    ## <none>                     2522.3 2542.3
    ## - a_stage              1   2525.4 2543.4
    ## - marital_status       1   2536.1 2554.1
    ## - estrogen_status      1   2539.4 2557.4
    ## - progesterone_status  1   2541.1 2559.1
    ## - differentiate        1   2559.5 2577.5
    ## - t_stage              1   2567.3 2585.3
    ## 
    ## Step:  AIC=2540.89
    ## status ~ age + race + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status
    ## 
    ##                       Df Deviance    AIC
    ## - race                 1   2522.9 2538.9
    ## <none>                     2522.9 2540.9
    ## - a_stage              1   2526.1 2542.1
    ## - marital_status       1   2536.5 2552.5
    ## - estrogen_status      1   2539.8 2555.8
    ## - progesterone_status  1   2541.6 2557.6
    ## - age                  1   2542.5 2558.5
    ## - differentiate        1   2559.8 2575.8
    ## - t_stage              1   2567.8 2583.8
    ## 
    ## Step:  AIC=2538.89
    ## status ~ age + marital_status + t_stage + differentiate + a_stage + 
    ##     estrogen_status + progesterone_status
    ## 
    ##                       Df Deviance    AIC
    ## <none>                     2522.9 2538.9
    ## - a_stage              1   2526.1 2540.1
    ## - marital_status       1   2536.5 2550.5
    ## - estrogen_status      1   2539.9 2553.9
    ## - progesterone_status  1   2541.6 2555.6
    ## - age                  1   2542.7 2556.7
    ## - differentiate        1   2559.8 2573.8
    ## - t_stage              1   2567.8 2581.8

``` r
summary(step_model_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ age + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status, family = binomial(link = "logit"), 
    ##     data = selected_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5313   0.3530   0.4539   0.5824   1.7480  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          1.570637   0.359045   4.374 1.22e-05 ***
    ## age                 -0.026116   0.005936  -4.400 1.08e-05 ***
    ## marital_status      -0.180202   0.048001  -3.754 0.000174 ***
    ## t_stage             -0.444968   0.066122  -6.729 1.70e-11 ***
    ## differentiate        0.510408   0.084940   6.009 1.87e-09 ***
    ## a_stage             -0.506600   0.275308  -1.840 0.065750 .  
    ## estrogen_status      0.789337   0.190743   4.138 3.50e-05 ***
    ## progesterone_status  0.617193   0.138443   4.458 8.27e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2780.2  on 3218  degrees of freedom
    ## Residual deviance: 2522.9  on 3211  degrees of freedom
    ## AIC: 2538.9
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
summary(step_modelF_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ progesterone_status + t_stage + differentiate + 
    ##     age + estrogen_status + marital_status + a_stage, family = binomial(link = "logit"), 
    ##     data = selected_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5313   0.3530   0.4539   0.5824   1.7480  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          1.570637   0.359045   4.374 1.22e-05 ***
    ## progesterone_status  0.617193   0.138443   4.458 8.27e-06 ***
    ## t_stage             -0.444968   0.066122  -6.729 1.70e-11 ***
    ## differentiate        0.510408   0.084940   6.009 1.87e-09 ***
    ## age                 -0.026116   0.005936  -4.400 1.08e-05 ***
    ## estrogen_status      0.789337   0.190743   4.138 3.50e-05 ***
    ## marital_status      -0.180202   0.048001  -3.754 0.000174 ***
    ## a_stage             -0.506600   0.275308  -1.840 0.065750 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2780.2  on 3218  degrees of freedom
    ## Residual deviance: 2522.9  on 3211  degrees of freedom
    ## AIC: 2538.9
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
anova(step_model_1,step_modelF_1,test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: status ~ age + marital_status + t_stage + differentiate + a_stage + 
    ##     estrogen_status + progesterone_status
    ## Model 2: status ~ progesterone_status + t_stage + differentiate + age + 
    ##     estrogen_status + marital_status + a_stage
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      3211     2522.9                     
    ## 2      3211     2522.9  0        0

``` r
test_predictions_log_oddsStep_1 = predict(step_model_1, newdata  = (test_set),type='response')

test_predictions_probStep_1 = plogis(test_predictions_log_oddsStep_1)

roc_curveStep_1 = roc(response = (test_set$status), predictor = as.numeric(test_predictions_probStep_1))
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
auc(roc_curveStep_1)
```

    ## Area under the curve: 0.7

# Interaction: race + marital_status

``` r
step_modelF_2 = step(null_model, scope = list(lower = null_model, upper = interaction_race_marital_status), 
                   direction = "forward")
```

    ## Start:  AIC=2782.19
    ## status ~ 1
    ## 
    ##                          Df Deviance    AIC
    ## + progesterone_status     1   2684.9 2688.9
    ## + estrogen_status         1   2689.6 2693.6
    ## + differentiate           1   2698.9 2702.9
    ## + t_stage                 1   2705.8 2709.8
    ## + marital_status          1   2758.5 2762.5
    ## + a_stage                 1   2762.0 2766.0
    ## + age                     1   2768.2 2772.2
    ## + regional_node_examined  1   2777.7 2781.7
    ## <none>                        2780.2 2782.2
    ## + race                    1   2780.2 2784.2
    ## 
    ## Step:  AIC=2688.88
    ## status ~ progesterone_status
    ## 
    ##                          Df Deviance    AIC
    ## + t_stage                 1   2620.4 2626.4
    ## + differentiate           1   2632.0 2638.0
    ## + estrogen_status         1   2659.5 2665.5
    ## + marital_status          1   2666.5 2672.5
    ## + a_stage                 1   2668.0 2674.0
    ## + age                     1   2674.2 2680.2
    ## <none>                        2684.9 2688.9
    ## + regional_node_examined  1   2683.0 2689.0
    ## + race                    1   2684.9 2690.9
    ## 
    ## Step:  AIC=2626.4
    ## status ~ progesterone_status + t_stage
    ## 
    ##                          Df Deviance    AIC
    ## + differentiate           1   2578.9 2586.9
    ## + estrogen_status         1   2597.8 2605.8
    ## + marital_status          1   2602.2 2610.2
    ## + age                     1   2605.3 2613.3
    ## + a_stage                 1   2616.4 2624.4
    ## <none>                        2620.4 2626.4
    ## + regional_node_examined  1   2620.2 2628.2
    ## + race                    1   2620.4 2628.4
    ## 
    ## Step:  AIC=2586.92
    ## status ~ progesterone_status + t_stage + differentiate
    ## 
    ##                          Df Deviance    AIC
    ## + age                     1   2557.0 2567.0
    ## + marital_status          1   2560.7 2570.7
    ## + estrogen_status         1   2565.2 2575.2
    ## + a_stage                 1   2575.2 2585.2
    ## <none>                        2578.9 2586.9
    ## + regional_node_examined  1   2578.9 2588.9
    ## + race                    1   2578.9 2588.9
    ## 
    ## Step:  AIC=2567
    ## status ~ progesterone_status + t_stage + differentiate + age
    ## 
    ##                          Df Deviance    AIC
    ## + estrogen_status         1   2539.7 2551.7
    ## + marital_status          1   2544.1 2556.1
    ## + a_stage                 1   2552.8 2564.8
    ## <none>                        2557.0 2567.0
    ## + race                    1   2556.9 2568.9
    ## + regional_node_examined  1   2557.0 2569.0
    ## 
    ## Step:  AIC=2551.74
    ## status ~ progesterone_status + t_stage + differentiate + age + 
    ##     estrogen_status
    ## 
    ##                          Df Deviance    AIC
    ## + marital_status          1   2526.1 2540.1
    ## + a_stage                 1   2536.5 2550.5
    ## <none>                        2539.7 2551.7
    ## + race                    1   2539.7 2553.7
    ## + regional_node_examined  1   2539.7 2553.7
    ## 
    ## Step:  AIC=2540.14
    ## status ~ progesterone_status + t_stage + differentiate + age + 
    ##     estrogen_status + marital_status
    ## 
    ##                          Df Deviance    AIC
    ## + a_stage                 1   2522.9 2538.9
    ## <none>                        2526.1 2540.1
    ## + regional_node_examined  1   2526.1 2542.1
    ## + race                    1   2526.1 2542.1
    ## 
    ## Step:  AIC=2538.89
    ## status ~ progesterone_status + t_stage + differentiate + age + 
    ##     estrogen_status + marital_status + a_stage
    ## 
    ##                          Df Deviance    AIC
    ## <none>                        2522.9 2538.9
    ## + race                    1   2522.9 2540.9
    ## + regional_node_examined  1   2522.9 2540.9

``` r
step_model_2 = step(interaction_race_marital_status, direction = "backward")
```

    ## Start:  AIC=2539.46
    ## status ~ age + race + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status + regional_node_examined + 
    ##     race:marital_status
    ## 
    ##                          Df Deviance    AIC
    ## - regional_node_examined  1   2517.5 2537.5
    ## <none>                        2517.5 2539.5
    ## - a_stage                 1   2520.9 2540.9
    ## - race:marital_status     1   2522.9 2542.9
    ## - estrogen_status         1   2534.5 2554.5
    ## - progesterone_status     1   2536.1 2556.1
    ## - age                     1   2536.5 2556.5
    ## - differentiate           1   2554.3 2574.3
    ## - t_stage                 1   2560.6 2580.6
    ## 
    ## Step:  AIC=2537.46
    ## status ~ age + race + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status + race:marital_status
    ## 
    ##                       Df Deviance    AIC
    ## <none>                     2517.5 2537.5
    ## - a_stage              1   2520.9 2538.9
    ## - race:marital_status  1   2522.9 2540.9
    ## - estrogen_status      1   2534.5 2552.5
    ## - progesterone_status  1   2536.1 2554.1
    ## - age                  1   2536.5 2554.5
    ## - differentiate        1   2554.6 2572.6
    ## - t_stage              1   2561.0 2579.0

``` r
summary(step_model_2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ age + race + marital_status + t_stage + 
    ##     differentiate + a_stage + estrogen_status + progesterone_status + 
    ##     race:marital_status, family = binomial(link = "logit"), data = selected_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6231   0.3528   0.4552   0.5791   1.9452  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          1.504256   0.364651   4.125 3.70e-05 ***
    ## age                 -0.025747   0.005962  -4.319 1.57e-05 ***
    ## race                 0.147211   0.113443   1.298   0.1944    
    ## marital_status      -0.130895   0.053038  -2.468   0.0136 *  
    ## t_stage             -0.438824   0.066252  -6.624 3.51e-11 ***
    ## differentiate        0.512156   0.085013   6.024 1.70e-09 ***
    ## a_stage             -0.525599   0.275654  -1.907   0.0566 .  
    ## estrogen_status      0.794240   0.191501   4.147 3.36e-05 ***
    ## progesterone_status  0.617158   0.138682   4.450 8.58e-06 ***
    ## race:marital_status -0.184933   0.079098  -2.338   0.0194 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2780.2  on 3218  degrees of freedom
    ## Residual deviance: 2517.5  on 3209  degrees of freedom
    ## AIC: 2537.5
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
summary(step_modelF_2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ progesterone_status + t_stage + differentiate + 
    ##     age + estrogen_status + marital_status + a_stage, family = binomial(link = "logit"), 
    ##     data = selected_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5313   0.3530   0.4539   0.5824   1.7480  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          1.570637   0.359045   4.374 1.22e-05 ***
    ## progesterone_status  0.617193   0.138443   4.458 8.27e-06 ***
    ## t_stage             -0.444968   0.066122  -6.729 1.70e-11 ***
    ## differentiate        0.510408   0.084940   6.009 1.87e-09 ***
    ## age                 -0.026116   0.005936  -4.400 1.08e-05 ***
    ## estrogen_status      0.789337   0.190743   4.138 3.50e-05 ***
    ## marital_status      -0.180202   0.048001  -3.754 0.000174 ***
    ## a_stage             -0.506600   0.275308  -1.840 0.065750 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2780.2  on 3218  degrees of freedom
    ## Residual deviance: 2522.9  on 3211  degrees of freedom
    ## AIC: 2538.9
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
anova(step_model_2,step_modelF_2,test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: status ~ age + race + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status + race:marital_status
    ## Model 2: status ~ progesterone_status + t_stage + differentiate + age + 
    ##     estrogen_status + marital_status + a_stage
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
    ## 1      3209     2517.5                       
    ## 2      3211     2522.9 -2  -5.4288  0.06624 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
test_predictions_log_oddsStep_2 = predict(step_model_2, newdata  = (test_set),type='response')

test_predictions_probStep_2 = plogis(test_predictions_log_oddsStep_2)

roc_curveStep_2 = roc(response = (test_set$status), predictor = as.numeric(test_predictions_probStep_2))
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
auc(roc_curveStep_2)
```

    ## Area under the curve: 0.6976

\#Elastic Net

``` r
# Prepare your data
X <- as.matrix(train_set[, setdiff(names(train_set), "status")])  # Predictor variables
y <- train_set$status  # Response variable

lambda_seq <- 10^seq(-2, 0, by = .001)

# Use cross-validation to find the optimal lambda
cv_object <- cv.glmnet(X, y, family = "binomial", alpha = 0.5, type.measure = "class",nfolds=5, lambda = lambda_seq)

tibble(lambda = cv_object$lambda,
mean_cv_error = cv_object$cvm) %>%
ggplot(aes(x = lambda, y = mean_cv_error)) +
geom_point()
```

<img src="final_project_files/figure-gfm/unnamed-chunk-11-1.png" width="90%" />

``` r
# Best lambda value
best_lambda <- cv_object$lambda.min
# Refit the model using the best lambda
final_model <- glmnet(X, y, family = "binomial", alpha = 0.5, lambda = best_lambda)

test_set2 <- test_set|> select(-status)
test_predictions_log_odds <- predict(final_model, newx = as.matrix(test_set2))

# Convert log-odds to probabilities
test_predictions_probElastic <- plogis(test_predictions_log_odds)
# Create the ROC curve
roc_curve <- roc(response = as.matrix(test_set$status), predictor = as.numeric(test_predictions_probElastic) )
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
auc(roc_curve)
```

    ## Area under the curve: 0.7734

\#Elastic net 2 \##Training without full variables

``` r
X2 <- as.matrix(selectedData_train_set[, setdiff(names(selectedData_train_set), "status")])  # Predictor variables
# Use cross-validation to find the optimal lambda
cv_object <- cv.glmnet(X2, y, family = "binomial", alpha = 0.5, type.measure = "class",nfolds=5, lambda = lambda_seq)

tibble(lambda = cv_object$lambda,
mean_cv_error = cv_object$cvm) %>%
ggplot(aes(x = lambda, y = mean_cv_error)) +
geom_point()
```

<img src="final_project_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" />

``` r
# Best lambda value
best_lambda <- cv_object$lambda.min
# Refit the model using the best lambda
final_model2 <- glmnet(X2, y, family = "binomial", alpha = 0.5, lambda = best_lambda)

selectedData_test_set <- selectedData_test_set|> select(-status)
test_predictions_log_odds2 <- predict(final_model, newx = as.matrix(test_set2))

# Convert log-odds to probabilities
test_predictions_probElastic2 <- plogis(test_predictions_log_odds2)
# Create the ROC curve
roc_curvenet2 <- roc(response = (test_set$status), predictor = as.numeric(test_predictions_probElastic2) )
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
auc(roc_curvenet2)
```

    ## Area under the curve: 0.7734

``` r
plot(roc_curve, main = "ROC Curve", col = "#1c61b6", lwd = 2)
lines(roc_curveStep,col='yellow')
lines(roc_curvenet2,col='green')
lines(roc_curveStep_2,col='purple')
```

<img src="final_project_files/figure-gfm/unnamed-chunk-12-2.png" width="90%" />
Based on the ROC and AUC, final_model2 is the best prediction.
finalModel has more variables than final_model2 but perform the same as
finalModel2 Logistics regression without Elastic net has less AUC than
final_Model2

Second model diagonstics \#final Model Diagonstics :

``` r
predicted_classes <- as.numeric(test_predictions_probElastic2 >sum(clean_data$status)/nrow(clean_data))

predicted_classes <- as.numeric(test_predictions_probElastic2 >0.5)
predicted.classes = as.factor(test_set$status)

outcome <- as.factor(test_set$status)
predicted_classes <- factor(predicted_classes, levels = c("0", "1"))
outcome <- factor(outcome, levels = c("0", "1"))



conf_matrix <- confusionMatrix(predicted_classes, outcome)
conf_matrix
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0  11  11
    ##          1 105 678
    ##                                           
    ##                Accuracy : 0.8559          
    ##                  95% CI : (0.8297, 0.8794)
    ##     No Information Rate : 0.8559          
    ##     P-Value [Acc > NIR] : 0.5247          
    ##                                           
    ##                   Kappa : 0.1189          
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.09483         
    ##             Specificity : 0.98403         
    ##          Pos Pred Value : 0.50000         
    ##          Neg Pred Value : 0.86590         
    ##              Prevalence : 0.14410         
    ##          Detection Rate : 0.01366         
    ##    Detection Prevalence : 0.02733         
    ##       Balanced Accuracy : 0.53943         
    ##                                           
    ##        'Positive' Class : 0               
    ## 
