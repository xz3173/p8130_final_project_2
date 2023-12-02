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
    status = as.numeric(factor(status, levels = c("Dead", "Alive"))) - 1)|>
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

``` r
corplot=cor(clean_data)
corrplot(corplot)
```

<img src="final_project_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

``` r
#tumor_size vs t_stage = 0.801
#grade=differentiate =>1
#n_stage = x6th_stage => 0.881
#n_stage = regional positive status =>0.838073333
selected_data = clean_data |>
  select(-tumor_size, -grade,-n_stage,-regional_node_positive,-x6th_stage)

corplot=cor(selected_data)
corrplot(corplot)
```

<img src="final_project_files/figure-gfm/unnamed-chunk-6-2.png" width="90%" />

# Pairs plot

# Use `windows` for windows system / `quartz` for macos system

# Use `quartz(width = 12, height = 12)` to open the window

# Use `dev.off()` to close the window

``` r
if (!dir.exists("plots")) {
    dir.create("plots")
}

png("plots/pairs_plot.png", 
    width = 12 * 600, 
    height = 12 * 600, 
    res = 600)

pairs(clean_data)
```

# Corr plot

``` r
png("plots/corr_plot.png", 
    width = 12 * 600, 
    height = 12 * 600, 
    res = 600)

corrplot(cor(clean_data), type = "upper", diag = FALSE)
```

# Plotting boxplot

``` r
plot_boxplot = function(data_vector, main_title, x_label = "") {
  boxplot(data_vector, 
          main = main_title, 
          xlab = x_label, 
          col = "lightblue")
}

png("plots/box_plot.png", 
    width = 12 * 600, 
    height = 12 * 600, 
    res = 600)

par(mar = c(2, 2, 2, 2))
par(mfrow = c(4, 4))


column_names = names(clean_data)
for (col_name in column_names) {
  plot_boxplot(clean_data[[col_name]], 
               main_title = col_name, 
               x_label = col_name)
}


dev.off()
```

    ## quartz_off_screen 
    ##                 2

# plotting histogram

``` r
plot_histogram = 
  function(data_vector, main_title, x_label = "") {
  hist(data_vector, 
       main = main_title, 
       xlab = x_label, 
       col = "blue")
}


png("plots/histogram_plot.png", 
    width = 12 * 600, 
    height = 12 * 600, res = 600)

par(mar = c(2, 2, 2, 2))
par(mfrow = c(4, 4))


column_names = names(clean_data)
for (col_name in column_names) {
  plot_histogram(clean_data[[col_name]], 
                 main_title = col_name, 
                 x_label = col_name)
}

dev.off()
```

    ## quartz_off_screen 
    ##                 2
