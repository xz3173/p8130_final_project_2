Final Project
================

``` r
library(gtsummary)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(car)
```

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

    ## corrplot 0.92 loaded

``` r
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
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (11): Race, Marital Status, T Stage, N Stage, 6th Stage, differentiate, ...
    ## dbl  (5): Age, Tumor Size, Regional Node Examined, Reginol Node Positive, Su...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
summary(data)
```

    ##       age            race           marital_status       t_stage         
    ##  Min.   :30.00   Length:4024        Length:4024        Length:4024       
    ##  1st Qu.:47.00   Class :character   Class :character   Class :character  
    ##  Median :54.00   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :53.97                                                           
    ##  3rd Qu.:61.00                                                           
    ##  Max.   :69.00                                                           
    ##    n_stage           x6th_stage        differentiate         grade          
    ##  Length:4024        Length:4024        Length:4024        Length:4024       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##    a_stage            tumor_size     estrogen_status    progesterone_status
    ##  Length:4024        Min.   :  1.00   Length:4024        Length:4024        
    ##  Class :character   1st Qu.: 16.00   Class :character   Class :character   
    ##  Mode  :character   Median : 25.00   Mode  :character   Mode  :character   
    ##                     Mean   : 30.47                                         
    ##                     3rd Qu.: 38.00                                         
    ##                     Max.   :140.00                                         
    ##  regional_node_examined reginol_node_positive survival_months
    ##  Min.   : 1.00          Min.   : 1.000        Min.   :  1.0  
    ##  1st Qu.: 9.00          1st Qu.: 1.000        1st Qu.: 56.0  
    ##  Median :14.00          Median : 2.000        Median : 73.0  
    ##  Mean   :14.36          Mean   : 4.158        Mean   : 71.3  
    ##  3rd Qu.:19.00          3rd Qu.: 5.000        3rd Qu.: 90.0  
    ##  Max.   :61.00          Max.   :46.000        Max.   :107.0  
    ##     status         
    ##  Length:4024       
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ## 

``` r
str(data)
```

    ## spc_tbl_ [4,024 × 16] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ age                   : num [1:4024] 68 50 58 58 47 51 51 40 40 69 ...
    ##  $ race                  : chr [1:4024] "White" "White" "White" "White" ...
    ##  $ marital_status        : chr [1:4024] "Married" "Married" "Divorced" "Married" ...
    ##  $ t_stage               : chr [1:4024] "T1" "T2" "T3" "T1" ...
    ##  $ n_stage               : chr [1:4024] "N1" "N2" "N3" "N1" ...
    ##  $ x6th_stage            : chr [1:4024] "IIA" "IIIA" "IIIC" "IIA" ...
    ##  $ differentiate         : chr [1:4024] "Poorly differentiated" "Moderately differentiated" "Moderately differentiated" "Poorly differentiated" ...
    ##  $ grade                 : chr [1:4024] "3" "2" "2" "3" ...
    ##  $ a_stage               : chr [1:4024] "Regional" "Regional" "Regional" "Regional" ...
    ##  $ tumor_size            : num [1:4024] 4 35 63 18 41 20 8 30 103 32 ...
    ##  $ estrogen_status       : chr [1:4024] "Positive" "Positive" "Positive" "Positive" ...
    ##  $ progesterone_status   : chr [1:4024] "Positive" "Positive" "Positive" "Positive" ...
    ##  $ regional_node_examined: num [1:4024] 24 14 14 2 3 18 11 9 20 21 ...
    ##  $ reginol_node_positive : num [1:4024] 1 5 7 1 1 2 1 1 18 12 ...
    ##  $ survival_months       : num [1:4024] 60 62 75 84 50 89 54 14 70 92 ...
    ##  $ status                : chr [1:4024] "Alive" "Alive" "Alive" "Alive" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   Age = col_double(),
    ##   ..   Race = col_character(),
    ##   ..   `Marital Status` = col_character(),
    ##   ..   `T Stage` = col_character(),
    ##   ..   `N Stage` = col_character(),
    ##   ..   `6th Stage` = col_character(),
    ##   ..   differentiate = col_character(),
    ##   ..   Grade = col_character(),
    ##   ..   `A Stage` = col_character(),
    ##   ..   `Tumor Size` = col_double(),
    ##   ..   `Estrogen Status` = col_character(),
    ##   ..   `Progesterone Status` = col_character(),
    ##   ..   `Regional Node Examined` = col_double(),
    ##   ..   `Reginol Node Positive` = col_double(),
    ##   ..   `Survival Months` = col_double(),
    ##   ..   Status = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
# Check for missing data
colSums(is.na(data))
```

    ##                    age                   race         marital_status 
    ##                      0                      0                      0 
    ##                t_stage                n_stage             x6th_stage 
    ##                      0                      0                      0 
    ##          differentiate                  grade                a_stage 
    ##                      0                      0                      0 
    ##             tumor_size        estrogen_status    progesterone_status 
    ##                      0                      0                      0 
    ## regional_node_examined  reginol_node_positive        survival_months 
    ##                      0                      0                      0 
    ##                 status 
    ##                      0

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
    status = as.numeric(factor(status, levels = c("Dead", "Alive"))) - 1) |>
  rename(regional_node_positive = reginol_node_positive)

summary(clean_data)
```

    ##       age             race        marital_status      t_stage      
    ##  Min.   :30.00   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:47.00   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :54.00   Median :0.0000   Median :0.0000   Median :1.0000  
    ##  Mean   :53.97   Mean   :0.2314   Mean   :0.6143   Mean   :0.7848  
    ##  3rd Qu.:61.00   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :69.00   Max.   :2.0000   Max.   :4.0000   Max.   :3.0000  
    ##     n_stage         x6th_stage    differentiate       grade      
    ##  Min.   :0.0000   Min.   :0.000   Min.   :0.000   Min.   :1.000  
    ##  1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:1.000   1st Qu.:2.000  
    ##  Median :0.0000   Median :1.000   Median :2.000   Median :2.000  
    ##  Mean   :0.4384   Mean   :1.322   Mean   :1.849   Mean   :2.151  
    ##  3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:3.000  
    ##  Max.   :2.0000   Max.   :4.000   Max.   :3.000   Max.   :4.000  
    ##     a_stage          tumor_size     estrogen_status  progesterone_status
    ##  Min.   :0.00000   Min.   :  1.00   Min.   :0.0000   Min.   :0.0000     
    ##  1st Qu.:0.00000   1st Qu.: 16.00   1st Qu.:1.0000   1st Qu.:1.0000     
    ##  Median :0.00000   Median : 25.00   Median :1.0000   Median :1.0000     
    ##  Mean   :0.02286   Mean   : 30.47   Mean   :0.9332   Mean   :0.8265     
    ##  3rd Qu.:0.00000   3rd Qu.: 38.00   3rd Qu.:1.0000   3rd Qu.:1.0000     
    ##  Max.   :1.00000   Max.   :140.00   Max.   :1.0000   Max.   :1.0000     
    ##  regional_node_examined regional_node_positive survival_months     status      
    ##  Min.   : 1.00          Min.   : 1.000         Min.   :  1.0   Min.   :0.0000  
    ##  1st Qu.: 9.00          1st Qu.: 1.000         1st Qu.: 56.0   1st Qu.:1.0000  
    ##  Median :14.00          Median : 2.000         Median : 73.0   Median :1.0000  
    ##  Mean   :14.36          Mean   : 4.158         Mean   : 71.3   Mean   :0.8469  
    ##  3rd Qu.:19.00          3rd Qu.: 5.000         3rd Qu.: 90.0   3rd Qu.:1.0000  
    ##  Max.   :61.00          Max.   :46.000         Max.   :107.0   Max.   :1.0000

``` r
str(clean_data)
```

    ## tibble [4,024 × 16] (S3: tbl_df/tbl/data.frame)
    ##  $ age                   : num [1:4024] 68 50 58 58 47 51 51 40 40 69 ...
    ##  $ race                  : num [1:4024] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ marital_status        : num [1:4024] 0 0 2 0 0 1 0 0 2 0 ...
    ##  $ t_stage               : num [1:4024] 0 1 2 0 1 0 0 1 3 3 ...
    ##  $ n_stage               : num [1:4024] 0 1 2 0 0 0 0 0 2 2 ...
    ##  $ x6th_stage            : num [1:4024] 0 2 4 0 1 0 0 1 4 4 ...
    ##  $ differentiate         : num [1:4024] 1 2 2 1 1 2 3 2 1 3 ...
    ##  $ grade                 : num [1:4024] 3 2 2 3 3 2 1 2 3 1 ...
    ##  $ a_stage               : num [1:4024] 0 0 0 0 0 0 0 0 0 1 ...
    ##  $ tumor_size            : num [1:4024] 4 35 63 18 41 20 8 30 103 32 ...
    ##  $ estrogen_status       : num [1:4024] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ progesterone_status   : num [1:4024] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ regional_node_examined: num [1:4024] 24 14 14 2 3 18 11 9 20 21 ...
    ##  $ regional_node_positive: num [1:4024] 1 5 7 1 1 2 1 1 18 12 ...
    ##  $ survival_months       : num [1:4024] 60 62 75 84 50 89 54 14 70 92 ...
    ##  $ status                : num [1:4024] 1 1 1 1 1 1 1 0 1 1 ...

``` r
colSums(is.na(clean_data))
```

    ##                    age                   race         marital_status 
    ##                      0                      0                      0 
    ##                t_stage                n_stage             x6th_stage 
    ##                      0                      0                      0 
    ##          differentiate                  grade                a_stage 
    ##                      0                      0                      0 
    ##             tumor_size        estrogen_status    progesterone_status 
    ##                      0                      0                      0 
    ## regional_node_examined regional_node_positive        survival_months 
    ##                      0                      0                      0 
    ##                 status 
    ##                      0

\#Figure 1

``` r
proj2 = data |>
tbl_summary(by="status",
  missing_text = "(Missing)", # counts missing values
  statistic = list(all_continuous() ~ "mean={mean} (min={min}, max={max}, sd={sd})",
                   all_categorical() ~ "n={n} (p={p}%)") # stats for categorical
 # specify variables to include
  ) |>
bold_labels()  |>
italicize_levels()
```

\#Find correlation

\#Model fitting

``` r
null_model = glm(status ~ 1, family = binomial(link = "logit"), data = clean_data)

model = glm(status ~ (age * race * marital_status + t_stage + n_stage + x6th_stage + differentiate + grade + a_stage + tumor_size + estrogen_status + progesterone_status + regional_node_examined + regional_node_positive),
             family = binomial(link = "logit"), data = clean_data)


model = glm(status ~ (age * race * marital_status + t_stage + n_stage + x6th_stage + grade + a_stage + tumor_size  + regional_node_examined + regional_node_positive),
             family = binomial(link = "logit"), data = clean_data)


step_model = step(null_model, scope = list(lower = null_model, upper = model), 
                   direction = "forward")
```

    ## Start:  AIC=3446.68
    ## status ~ 1
    ## 
    ##                          Df Deviance    AIC
    ## + x6th_stage              1   3198.0 3202.0
    ## + n_stage                 1   3214.2 3218.2
    ## + regional_node_positive  1   3236.8 3240.8
    ## + grade                   1   3338.0 3342.0
    ## + t_stage                 1   3353.3 3357.3
    ## + tumor_size              1   3380.1 3384.1
    ## + a_stage                 1   3415.7 3419.7
    ## + marital_status          1   3421.0 3425.0
    ## + age                     1   3432.0 3436.0
    ## + regional_node_examined  1   3439.9 3443.9
    ## <none>                        3444.7 3446.7
    ## + race                    1   3444.6 3448.6
    ## 
    ## Step:  AIC=3202
    ## status ~ x6th_stage
    ## 
    ##                          Df Deviance    AIC
    ## + grade                   1   3138.7 3144.7
    ## + marital_status          1   3181.7 3187.7
    ## + age                     1   3183.2 3189.2
    ## + regional_node_examined  1   3185.2 3191.2
    ## + regional_node_positive  1   3186.9 3192.9
    ## + n_stage                 1   3193.9 3199.9
    ## + t_stage                 1   3196.0 3202.0
    ## <none>                        3198.0 3202.0
    ## + tumor_size              1   3196.7 3202.7
    ## + a_stage                 1   3197.6 3203.6
    ## + race                    1   3197.7 3203.7
    ## 
    ## Step:  AIC=3144.67
    ## status ~ x6th_stage + grade
    ## 
    ##                          Df Deviance    AIC
    ## + age                     1   3117.5 3125.5
    ## + marital_status          1   3123.2 3131.2
    ## + regional_node_examined  1   3124.8 3132.8
    ## + regional_node_positive  1   3125.9 3133.9
    ## + n_stage                 1   3134.3 3142.3
    ## <none>                        3138.7 3144.7
    ## + t_stage                 1   3137.3 3145.3
    ## + tumor_size              1   3137.8 3145.8
    ## + race                    1   3138.1 3146.1
    ## + a_stage                 1   3138.1 3146.1
    ## 
    ## Step:  AIC=3125.46
    ## status ~ x6th_stage + grade + age
    ## 
    ##                          Df Deviance    AIC
    ## + regional_node_examined  1   3104.2 3114.2
    ## + regional_node_positive  1   3106.2 3116.2
    ## + marital_status          1   3106.4 3116.4
    ## + n_stage                 1   3113.7 3123.7
    ## + t_stage                 1   3115.1 3125.1
    ## <none>                        3117.5 3125.5
    ## + tumor_size              1   3115.7 3125.7
    ## + a_stage                 1   3116.6 3126.6
    ## + race                    1   3117.3 3127.3
    ## 
    ## Step:  AIC=3114.15
    ## status ~ x6th_stage + grade + age + regional_node_examined
    ## 
    ##                          Df Deviance    AIC
    ## + regional_node_positive  1   3078.5 3090.5
    ## + marital_status          1   3093.1 3105.1
    ## + n_stage                 1   3098.2 3110.2
    ## <none>                        3104.2 3114.2
    ## + t_stage                 1   3103.0 3115.0
    ## + tumor_size              1   3103.2 3115.2
    ## + a_stage                 1   3103.6 3115.6
    ## + race                    1   3104.0 3116.0
    ## 
    ## Step:  AIC=3090.49
    ## status ~ x6th_stage + grade + age + regional_node_examined + 
    ##     regional_node_positive
    ## 
    ##                  Df Deviance    AIC
    ## + marital_status  1   3068.1 3082.1
    ## + t_stage         1   3068.9 3082.9
    ## + tumor_size      1   3073.9 3087.9
    ## <none>                3078.5 3090.5
    ## + a_stage         1   3077.8 3091.8
    ## + race            1   3078.4 3092.4
    ## + n_stage         1   3078.4 3092.4
    ## 
    ## Step:  AIC=3082.13
    ## status ~ x6th_stage + grade + age + regional_node_examined + 
    ##     regional_node_positive + marital_status
    ## 
    ##                      Df Deviance    AIC
    ## + t_stage             1   3058.8 3074.8
    ## + tumor_size          1   3063.8 3079.8
    ## <none>                    3068.1 3082.1
    ## + a_stage             1   3067.5 3083.5
    ## + age:marital_status  1   3067.7 3083.7
    ## + race                1   3068.0 3084.0
    ## + n_stage             1   3068.0 3084.0
    ## 
    ## Step:  AIC=3074.84
    ## status ~ x6th_stage + grade + age + regional_node_examined + 
    ##     regional_node_positive + marital_status + t_stage
    ## 
    ##                      Df Deviance    AIC
    ## + n_stage             1   3053.4 3071.4
    ## <none>                    3058.8 3074.8
    ## + age:marital_status  1   3058.4 3076.4
    ## + a_stage             1   3058.6 3076.6
    ## + race                1   3058.8 3076.8
    ## + tumor_size          1   3058.8 3076.8
    ## 
    ## Step:  AIC=3071.37
    ## status ~ x6th_stage + grade + age + regional_node_examined + 
    ##     regional_node_positive + marital_status + t_stage + n_stage
    ## 
    ##                      Df Deviance    AIC
    ## <none>                    3053.4 3071.4
    ## + age:marital_status  1   3052.9 3072.9
    ## + a_stage             1   3053.2 3073.2
    ## + tumor_size          1   3053.2 3073.2
    ## + race                1   3053.3 3073.3

``` r
step_model = step(model, direction = "backward")
```

    ## Start:  AIC=3074.59
    ## status ~ (age * race * marital_status + t_stage + n_stage + x6th_stage + 
    ##     grade + a_stage + tumor_size + regional_node_examined + regional_node_positive)
    ## 
    ##                           Df Deviance    AIC
    ## - x6th_stage               1   3042.7 3072.7
    ## - a_stage                  1   3042.7 3072.7
    ## - tumor_size               1   3042.7 3072.7
    ## - age:race:marital_status  1   3042.8 3072.8
    ## <none>                         3042.6 3074.6
    ## - n_stage                  1   3048.7 3078.7
    ## - t_stage                  1   3051.4 3081.4
    ## - regional_node_positive   1   3065.7 3095.7
    ## - regional_node_examined   1   3068.8 3098.8
    ## - grade                    1   3109.9 3139.9
    ## 
    ## Step:  AIC=3072.66
    ## status ~ age + race + marital_status + t_stage + n_stage + grade + 
    ##     a_stage + tumor_size + regional_node_examined + regional_node_positive + 
    ##     age:race + age:marital_status + race:marital_status + age:race:marital_status
    ## 
    ##                           Df Deviance    AIC
    ## - a_stage                  1   3042.7 3070.7
    ## - tumor_size               1   3042.7 3070.7
    ## - age:race:marital_status  1   3042.8 3070.8
    ## <none>                         3042.7 3072.7
    ## - t_stage                  1   3054.6 3082.6
    ## - n_stage                  1   3057.6 3085.6
    ## - regional_node_positive   1   3066.2 3094.2
    ## - regional_node_examined   1   3068.9 3096.9
    ## - grade                    1   3109.9 3137.9
    ## 
    ## Step:  AIC=3070.74
    ## status ~ age + race + marital_status + t_stage + n_stage + grade + 
    ##     tumor_size + regional_node_examined + regional_node_positive + 
    ##     age:race + age:marital_status + race:marital_status + age:race:marital_status
    ## 
    ##                           Df Deviance    AIC
    ## - tumor_size               1   3042.8 3068.8
    ## - age:race:marital_status  1   3042.9 3068.9
    ## <none>                         3042.7 3070.7
    ## - t_stage                  1   3055.7 3081.7
    ## - n_stage                  1   3058.1 3084.1
    ## - regional_node_positive   1   3066.3 3092.3
    ## - regional_node_examined   1   3069.2 3095.2
    ## - grade                    1   3109.9 3135.9
    ## 
    ## Step:  AIC=3068.85
    ## status ~ age + race + marital_status + t_stage + n_stage + grade + 
    ##     regional_node_examined + regional_node_positive + age:race + 
    ##     age:marital_status + race:marital_status + age:race:marital_status
    ## 
    ##                           Df Deviance    AIC
    ## - age:race:marital_status  1   3043.0 3067.0
    ## <none>                         3042.8 3068.8
    ## - n_stage                  1   3058.1 3082.1
    ## - regional_node_positive   1   3066.4 3090.4
    ## - regional_node_examined   1   3069.2 3093.2
    ## - t_stage                  1   3070.0 3094.0
    ## - grade                    1   3110.0 3134.0
    ## 
    ## Step:  AIC=3067.03
    ## status ~ age + race + marital_status + t_stage + n_stage + grade + 
    ##     regional_node_examined + regional_node_positive + age:race + 
    ##     age:marital_status + race:marital_status
    ## 
    ##                          Df Deviance    AIC
    ## - age:marital_status      1   3043.2 3065.2
    ## <none>                        3043.0 3067.0
    ## - age:race                1   3045.2 3067.2
    ## - race:marital_status     1   3052.0 3074.0
    ## - n_stage                 1   3058.2 3080.2
    ## - regional_node_positive  1   3066.6 3088.6
    ## - regional_node_examined  1   3069.3 3091.3
    ## - t_stage                 1   3070.1 3092.1
    ## - grade                   1   3110.3 3132.3
    ## 
    ## Step:  AIC=3065.15
    ## status ~ age + race + marital_status + t_stage + n_stage + grade + 
    ##     regional_node_examined + regional_node_positive + age:race + 
    ##     race:marital_status
    ## 
    ##                          Df Deviance    AIC
    ## <none>                        3043.2 3065.2
    ## - age:race                1   3045.4 3065.4
    ## - race:marital_status     1   3052.4 3072.4
    ## - n_stage                 1   3058.4 3078.4
    ## - regional_node_positive  1   3066.7 3086.7
    ## - regional_node_examined  1   3069.5 3089.5
    ## - t_stage                 1   3070.2 3090.2
    ## - grade                   1   3110.4 3130.4

``` r
# Use windows or quartz
quartz(width = 12, height = 12)

pairs(clean_data)
```

<img src="final_project_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

``` r
quartz(width = 12, height = 12)

corrplot(cor(clean_data), type = "upper", diag = FALSE)
```

<img src="final_project_files/figure-gfm/unnamed-chunk-9-1.png" width="90%" />

``` r
# Boxplots for each variable
par(mar = c(2, 2, 2, 2))
par(mfrow = c(4, 4))
boxplot(clean_data$age, main = "age")
boxplot(clean_data$race, main = "race")
boxplot(clean_data$marital_status, main = "marital_status")
boxplot(clean_data$t_stage, main = "t_stage")
boxplot(clean_data$n_stage, main = "n_stage")
boxplot(clean_data$x6th_stage, main = "x6th_stage")
boxplot(clean_data$differentiate, main = "differentiate")
boxplot(clean_data$grade, main = "grade")
boxplot(clean_data$a_stage, main = "a_stage")
boxplot(clean_data$tumor_size, main = "tumor_size")
boxplot(clean_data$estrogen_status, main = "estrogen_status")
boxplot(clean_data$progesterone_status, main = "progesterone_status")
boxplot(clean_data$regional_node_examined, main = "regional_node_examined")
boxplot(clean_data$regional_node_positive, main = "regional_node_positive")
boxplot(clean_data$survival_months, main = "survival_months")
boxplot(clean_data$status, main = "status")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />

``` r
# Fit regression using all predictors
mult.fit = lm(survival_months ~., data = clean_data)
summary(mult.fit)
```

    ## 
    ## Call:
    ## lm(formula = survival_months ~ ., data = clean_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -73.583 -15.243   0.223  16.238  56.701 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            43.0401885  2.6101499  16.490  < 2e-16 ***
    ## age                     0.0407115  0.0364164   1.118  0.26366    
    ## race                   -0.0288895  0.5503922  -0.052  0.95814    
    ## marital_status         -0.2944651  0.3277776  -0.898  0.36904    
    ## t_stage                 1.6072840  0.9770341   1.645  0.10003    
    ## n_stage                 0.9163048  1.4331942   0.639  0.52264    
    ## x6th_stage             -1.1297933  0.9114789  -1.240  0.21523    
    ## differentiate          -0.7944617  0.5230411  -1.519  0.12886    
    ## grade                          NA         NA      NA       NA    
    ## a_stage                -3.2118177  2.2496037  -1.428  0.15345    
    ## tumor_size             -0.0463811  0.0261002  -1.777  0.07564 .  
    ## estrogen_status         4.3949022  1.5097150   2.911  0.00362 ** 
    ## progesterone_status    -0.6536390  0.9877921  -0.662  0.50819    
    ## regional_node_examined  0.0006144  0.0432811   0.014  0.98867    
    ## regional_node_positive  0.0578738  0.1210518   0.478  0.63261    
    ## status                 29.7834905  0.9455169  31.500  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.13 on 4009 degrees of freedom
    ## Multiple R-squared:  0.2311, Adjusted R-squared:  0.2284 
    ## F-statistic: 86.05 on 14 and 4009 DF,  p-value: < 2.2e-16

``` r
# Histograms for each variable
par(mar = c(2, 2, 2, 2))
par(mfrow = c(4, 4))
hist(clean_data$age, main = "age")
hist(clean_data$race, main = "race")
hist(clean_data$marital_status, main = "marital_status")
hist(clean_data$t_stage, main = "t_stage")
hist(clean_data$n_stage, main = "n_stage")
hist(clean_data$x6th_stage, main = "x6th_stage")
hist(clean_data$differentiate, main = "differentiate")
hist(clean_data$grade, main = "grade")
hist(clean_data$a_stage, main = "a_stage")
hist(clean_data$tumor_size, main = "tumor_size")
hist(clean_data$estrogen_status, main = "estrogen_status")
hist(clean_data$progesterone_status, main = "progesterone_status")
hist(clean_data$regional_node_examined, main = "regional_node_examined")
hist(clean_data$regional_node_positive, main = "regional_node_positive")
hist(clean_data$survival_months, main = "survival_months")
hist(clean_data$status, main = "status")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" />
