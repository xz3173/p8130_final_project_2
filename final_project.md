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
library(glmnet)
```

    ## Loading required package: Matrix
    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack
    ## 
    ## Loaded glmnet 4.1-8

``` r
library(leaps)
library(pROC)
```

    ## Type 'citation("pROC")' for a citation.
    ## 
    ## Attaching package: 'pROC'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
library(broom)
library(patchwork)

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

# Load dataset

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

# Data cleaning

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

# Generates a summary table

``` r
# Make descriptive statistics for each variable 
proj2 = clean_data |>
tbl_summary(by="status",
  missing_text = "(Missing)", # counts missing values
  statistic = list(all_continuous() ~ "mean={mean} (min={min}, max={max}, sd={sd})",
                   all_categorical() ~ "n={n} (p={p}%)") # stats for categorical
 # specify variables to include
  ) |>
bold_labels()  |>
italicize_levels()
proj2
```

<div id="osncjrvket" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#osncjrvket table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#osncjrvket thead, #osncjrvket tbody, #osncjrvket tfoot, #osncjrvket tr, #osncjrvket td, #osncjrvket th {
  border-style: none;
}
&#10;#osncjrvket p {
  margin: 0;
  padding: 0;
}
&#10;#osncjrvket .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#osncjrvket .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#osncjrvket .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#osncjrvket .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#osncjrvket .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#osncjrvket .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#osncjrvket .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#osncjrvket .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#osncjrvket .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#osncjrvket .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#osncjrvket .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#osncjrvket .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#osncjrvket .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#osncjrvket .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#osncjrvket .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#osncjrvket .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#osncjrvket .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#osncjrvket .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#osncjrvket .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#osncjrvket .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#osncjrvket .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#osncjrvket .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#osncjrvket .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#osncjrvket .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#osncjrvket .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#osncjrvket .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#osncjrvket .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#osncjrvket .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#osncjrvket .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#osncjrvket .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#osncjrvket .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#osncjrvket .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#osncjrvket .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#osncjrvket .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#osncjrvket .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#osncjrvket .gt_left {
  text-align: left;
}
&#10;#osncjrvket .gt_center {
  text-align: center;
}
&#10;#osncjrvket .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#osncjrvket .gt_font_normal {
  font-weight: normal;
}
&#10;#osncjrvket .gt_font_bold {
  font-weight: bold;
}
&#10;#osncjrvket .gt_font_italic {
  font-style: italic;
}
&#10;#osncjrvket .gt_super {
  font-size: 65%;
}
&#10;#osncjrvket .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#osncjrvket .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#osncjrvket .gt_indent_1 {
  text-indent: 5px;
}
&#10;#osncjrvket .gt_indent_2 {
  text-indent: 10px;
}
&#10;#osncjrvket .gt_indent_3 {
  text-indent: 15px;
}
&#10;#osncjrvket .gt_indent_4 {
  text-indent: 20px;
}
&#10;#osncjrvket .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;0&lt;/strong&gt;, N = 616&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>0</strong>, N = 616<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;1&lt;/strong&gt;, N = 3,408&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>1</strong>, N = 3,408<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">age</td>
<td headers="stat_1" class="gt_row gt_center">mean=55 (min=30, max=69, sd=10)</td>
<td headers="stat_2" class="gt_row gt_center">mean=54 (min=30, max=69, sd=9)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">race</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    0</td>
<td headers="stat_1" class="gt_row gt_center">n=510 (p=83%)</td>
<td headers="stat_2" class="gt_row gt_center">n=2,903 (p=85%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    1</td>
<td headers="stat_1" class="gt_row gt_center">n=73 (p=12%)</td>
<td headers="stat_2" class="gt_row gt_center">n=218 (p=6.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    2</td>
<td headers="stat_1" class="gt_row gt_center">n=33 (p=5.4%)</td>
<td headers="stat_2" class="gt_row gt_center">n=287 (p=8.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">marital_status</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    0</td>
<td headers="stat_1" class="gt_row gt_center">n=358 (p=58%)</td>
<td headers="stat_2" class="gt_row gt_center">n=2,285 (p=67%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    1</td>
<td headers="stat_1" class="gt_row gt_center">n=104 (p=17%)</td>
<td headers="stat_2" class="gt_row gt_center">n=511 (p=15%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    2</td>
<td headers="stat_1" class="gt_row gt_center">n=90 (p=15%)</td>
<td headers="stat_2" class="gt_row gt_center">n=396 (p=12%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    3</td>
<td headers="stat_1" class="gt_row gt_center">n=49 (p=8.0%)</td>
<td headers="stat_2" class="gt_row gt_center">n=186 (p=5.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    4</td>
<td headers="stat_1" class="gt_row gt_center">n=15 (p=2.4%)</td>
<td headers="stat_2" class="gt_row gt_center">n=30 (p=0.9%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">t_stage</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    0</td>
<td headers="stat_1" class="gt_row gt_center">n=157 (p=25%)</td>
<td headers="stat_2" class="gt_row gt_center">n=1,446 (p=42%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    1</td>
<td headers="stat_1" class="gt_row gt_center">n=303 (p=49%)</td>
<td headers="stat_2" class="gt_row gt_center">n=1,483 (p=44%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    2</td>
<td headers="stat_1" class="gt_row gt_center">n=116 (p=19%)</td>
<td headers="stat_2" class="gt_row gt_center">n=417 (p=12%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    3</td>
<td headers="stat_1" class="gt_row gt_center">n=40 (p=6.5%)</td>
<td headers="stat_2" class="gt_row gt_center">n=62 (p=1.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">n_stage</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    0</td>
<td headers="stat_1" class="gt_row gt_center">n=270 (p=44%)</td>
<td headers="stat_2" class="gt_row gt_center">n=2,462 (p=72%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    1</td>
<td headers="stat_1" class="gt_row gt_center">n=165 (p=27%)</td>
<td headers="stat_2" class="gt_row gt_center">n=655 (p=19%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    2</td>
<td headers="stat_1" class="gt_row gt_center">n=181 (p=29%)</td>
<td headers="stat_2" class="gt_row gt_center">n=291 (p=8.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">x6th_stage</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    0</td>
<td headers="stat_1" class="gt_row gt_center">n=96 (p=16%)</td>
<td headers="stat_2" class="gt_row gt_center">n=1,209 (p=35%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    1</td>
<td headers="stat_1" class="gt_row gt_center">n=135 (p=22%)</td>
<td headers="stat_2" class="gt_row gt_center">n=995 (p=29%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    2</td>
<td headers="stat_1" class="gt_row gt_center">n=184 (p=30%)</td>
<td headers="stat_2" class="gt_row gt_center">n=866 (p=25%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    3</td>
<td headers="stat_1" class="gt_row gt_center">n=20 (p=3.2%)</td>
<td headers="stat_2" class="gt_row gt_center">n=47 (p=1.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    4</td>
<td headers="stat_1" class="gt_row gt_center">n=181 (p=29%)</td>
<td headers="stat_2" class="gt_row gt_center">n=291 (p=8.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">differentiate</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    0</td>
<td headers="stat_1" class="gt_row gt_center">n=9 (p=1.5%)</td>
<td headers="stat_2" class="gt_row gt_center">n=10 (p=0.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    1</td>
<td headers="stat_1" class="gt_row gt_center">n=263 (p=43%)</td>
<td headers="stat_2" class="gt_row gt_center">n=848 (p=25%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    2</td>
<td headers="stat_1" class="gt_row gt_center">n=305 (p=50%)</td>
<td headers="stat_2" class="gt_row gt_center">n=2,046 (p=60%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    3</td>
<td headers="stat_1" class="gt_row gt_center">n=39 (p=6.3%)</td>
<td headers="stat_2" class="gt_row gt_center">n=504 (p=15%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">grade</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    1</td>
<td headers="stat_1" class="gt_row gt_center">n=39 (p=6.3%)</td>
<td headers="stat_2" class="gt_row gt_center">n=504 (p=15%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    2</td>
<td headers="stat_1" class="gt_row gt_center">n=305 (p=50%)</td>
<td headers="stat_2" class="gt_row gt_center">n=2,046 (p=60%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    3</td>
<td headers="stat_1" class="gt_row gt_center">n=263 (p=43%)</td>
<td headers="stat_2" class="gt_row gt_center">n=848 (p=25%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-style: italic;">    4</td>
<td headers="stat_1" class="gt_row gt_center">n=9 (p=1.5%)</td>
<td headers="stat_2" class="gt_row gt_center">n=10 (p=0.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">a_stage</td>
<td headers="stat_1" class="gt_row gt_center">n=35 (p=5.7%)</td>
<td headers="stat_2" class="gt_row gt_center">n=57 (p=1.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">tumor_size</td>
<td headers="stat_1" class="gt_row gt_center">mean=37 (min=1, max=140, sd=24)</td>
<td headers="stat_2" class="gt_row gt_center">mean=29 (min=1, max=140, sd=20)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">estrogen_status</td>
<td headers="stat_1" class="gt_row gt_center">n=508 (p=82%)</td>
<td headers="stat_2" class="gt_row gt_center">n=3,247 (p=95%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">progesterone_status</td>
<td headers="stat_1" class="gt_row gt_center">n=412 (p=67%)</td>
<td headers="stat_2" class="gt_row gt_center">n=2,914 (p=86%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">regional_node_examined</td>
<td headers="stat_1" class="gt_row gt_center">mean=15 (min=1, max=57, sd=8)</td>
<td headers="stat_2" class="gt_row gt_center">mean=14 (min=1, max=61, sd=8)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">regional_node_positive</td>
<td headers="stat_1" class="gt_row gt_center">mean=7.2 (min=1.0, max=46.0, sd=7.3)</td>
<td headers="stat_2" class="gt_row gt_center">mean=3.6 (min=1.0, max=41.0, sd=4.4)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">survival_months</td>
<td headers="stat_1" class="gt_row gt_center">mean=46 (min=2, max=102, sd=24)</td>
<td headers="stat_2" class="gt_row gt_center">mean=76 (min=1, max=107, sd=19)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> mean=Mean (min=Minimum, max=Maximum, sd=SD); n=n (p=%)</td>
    </tr>
  </tfoot>
</table>
</div>

# Create another data set for making plots

``` r
clean_data2=clean_data
```

## Pairs plot

## Use `windows` for windows system / `quartz` for macos system

## Use `quartz(width = 12, height = 12)` to open the window

## Use `dev.off()` to close the window

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

## Corr plot

``` r
png("plots/corr_plot.png", 
    width = 12 * 600, 
    height = 12 * 600, 
    res = 600)

corrplot(cor(clean_data), type = "upper", diag = FALSE)
```

## Plotting boxplot

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

## plotting histogram

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

``` r
boxplot(clean_data$age, main = "age")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />

1.  Age: The age box plot shows a fairly symmetrical distribution with a
    median around 50 years old, and the interquartile range (IQR)
    appears tight, suggesting that half of the values fall within a
    relatively small range. There are no visible outliers, and the
    distribution does not appear to be skewed, which indicates that a
    transformation may not be necessary for age in this dataset.

``` r
boxplot(clean_data$race, main = "race")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-11-1.png" width="90%" />

2.  Race: The race box plot indicates that the majority of the dataset’s
    observations fall into one category, with a few outliers in the
    higher categories. Given that race is a categorical variable, it
    does not require a numerical transformation.

``` r
boxplot(clean_data$marital_status, main = "marital status")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" />

3.  Marital Status: The box plot for marital status shows a
    concentration of data in the lower categories with some outliers
    present in the higher categories, it does not require a numerical
    transformation.

``` r
boxplot(clean_data$t_stage, main = "T Stage")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" />

4.  T Stage: The boxplot for T Stage suggests that most patients are in
    the early stages (0 and 1), with fewer patients in more advanced
    stages (2 and 3), as indicated by the median and interquartile
    range. There are outliers in stage 3, which might represent more
    severe cases. T Stage is an ordinal variable, representing the size
    and extent of the main tumor, and does not typically require
    numerical transformation.

``` r
boxplot(clean_data$n_stage, main = "N Stage")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-14-1.png" width="90%" />

5.  N Stage: The boxplot for N Stage indicates a distribution that is
    skewed towards the lower stages, with most data points falling in
    stage 0 or 1, and fewer in stage 2. There are no outliers, and the
    spread of the data suggests that most patients have no or minimal
    regional lymph node involvement.

``` r
boxplot(clean_data$x6th_stage, main = "X6th Stage")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-15-1.png" width="90%" />

6.  X6th Stage: The box plot for the x6th stage shows a relatively even
    distribution across the stags with the median at stage 1, indicating
    a moderate level of spread to distant parts of the body. The data
    points are well-contained within the whiskers, suggesting there are
    no outliers and no extreme values that would require transformation.

``` r
boxplot(clean_data$differentiate, main = "differentiate")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-16-1.png" width="90%" />

7.  Differentiate: The box plot for the ‘differentiate’ variable shows
    that the median level of tumor differentiation is around the lower
    middle range, with a fairly symmetrical distribution around this
    median. There are no visible outliers, indicating that there are no
    extreme cases in terms of tumor differentiation. Since
    ‘differentiate’ is likely an ordinal variable representing ordered
    categories of tumor differentiation, it does not require a numerical
    transformation

``` r
boxplot(clean_data$grade, main = "grade")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-17-1.png" width="90%" />

8.  Grade: The box plot for ‘grade’ shows a distribution that is fairly
    centered, with the median around the midpoint of the scale, which
    suggests an even spread of tumor grades in the dataset. The data is
    contained within the whiskers, indicating no outliers, and the
    grades are likely to be an ordinal variable where the numerical
    value indicates a ranking or level of severity. Therefore, no
    transformation is needed.

``` r
boxplot(clean_data$a_stage, main = "A Stage")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-18-1.png" width="90%" />

9.  A Stage: The box plot for ‘A Stage’ suggests that the majority of
    observations are concentrated at the lower end of the scale, with a
    single outlier indicating a case with a higher stage. This variable
    is also likely categorical or ordinal, reflecting stages of cancer,
    and as such, numerical transformation is not appropriate.

``` r
boxplot(clean_data$tumor_size, main = "Tumor Size")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-19-1.png" width="90%" />

10. Tumor Size: The box plot for ‘Tumor Size’ shows a wide range of
    sizes, with a concentration of smaller tumors and several outliers
    indicating much larger tumors. The distribution appears right-skewed
    due to these outliers. Given the skewness and presence of outliers,
    a transformation such as a logarithmic scale could be beneficial to
    normalize the data, which may be especially useful if ‘Tumor Size’
    is used as a predictor in regression analysis.

``` r
boxplot(clean_data$estrogen_status, main = "Estrogen Status")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-20-1.png" width="90%" />

11. Estrogen Status: The box plot for Estrogen Status suggests that it
    is a binary categorical variable, likely indicating the presence (1)
    or absence (0) of estrogen receptors in tumor samples. The plot
    shows that a large majority of the tumors are positive for estrogen
    receptors, with very few negative cases, as indicated by the outlier
    point at zero. As a binary variable, Estrogen Status does not
    require a numerical transformation for analysis.

``` r
boxplot(clean_data$progesterone_status, main = "Progesterone Status")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-21-1.png" width="90%" />

12. Progesterone Status: The box plot for Progesterone Status indicates
    that it is also a binary categorical variable, representing the
    presence (1) or absence (0) of progesterone receptors. Similar to
    the Estrogen Status, the vast majority of the data points indicate a
    positive status for progesterone receptors, with the outlier at zero
    representing the few negative cases. No transformation is needed for
    this type of categorical data.

``` r
boxplot(clean_data$regional_node_examined, main = "Regional Node Examined")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-22-1.png" width="90%" />

13. Regional Node Examined: The box plot for ‘Regional Node Examined’
    displays a right-skewed distribution with a significant number of
    outliers on the higher end, indicating that while most patients had
    a smaller number of nodes examined, there are some patients with a
    much larger number. Given the skewness and presence of outliers, a
    log transformation might be appropriate to normalize the
    distribution, especially if this variable is to be used in
    parametric statistical analyses that assume normality.

``` r
boxplot(clean_data$regional_node_positive, main = "Regional Node Positive")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-23-1.png" width="90%" />

14. Regional Node Positive: The box plot for ‘Regional Node Positive’
    indicates a distribution with a large number of outliers on the
    upper end, reflecting that while most patients have a relatively low
    number of positive regional nodes, there are several patients with a
    significantly higher count. Given the right skewness and the
    presence of many outliers, a log transformation could be appropriate
    to reduce the skewness and diminish the influence of outliers.

``` r
boxplot(clean_data$survival_months, main = "Survival Months")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-24-1.png" width="90%" />

15. Survival Months: The box plot for ‘Survival Months’ shows a
    distribution with a median below the halfway point of the box,
    suggesting a slight skew towards shorter survival times. There are
    outliers on the lower end, representing patients with very short
    survival times. Given the distribution’s skewness and presence of
    outliers, a transformation such as logarithmic or square root might
    help to normalize the data, particularly if ‘Survival Months’ is
    used as a continuous predictor in regression models requiring
    normally distributed residuals.

## Race

``` r
# Bar Plot for Race
p1 = ggplot(clean_data, aes(x = factor(race))) +
  geom_bar(fill = "orange") +
  ggtitle("Race Distribution") +
  xlab("Race") +
  ylab("Count")
```

#### Race Distribution

The bar chart indicates a significant imbalance in the race distribution
of the patient sample, with the majority of patients being of the race
categorized as ‘o’ (White). Races ‘1’ (Black) and ‘2’ (other) are much
less represented, suggesting that the patient data may not be as diverse
in terms of racial demographics.

## Marital Status

``` r
# Bar Plot for Marital Status
p2 = ggplot(clean_data, aes(x = factor(marital_status))) +
  geom_bar(fill = "orange") +
  ggtitle("Marital Status Distribution") +
  xlab("Marital Status") +
  ylab("Count")
```

#### Marital Status

The bar chart shows that the largest group of patients falls under the
marital status category ‘0’, which is represent married, while the other
categories, which represent single, divorced, widowed, separated, are
less frequent. This suggests that the married patients are more
prevalent in this particular dataset.

## t_stage

``` r
# Bar Plot for t_stage
p3 = ggplot(clean_data, aes(x = factor(t_stage))) +
  geom_bar(fill = "orange") +
  ggtitle("T Stage Distribution") +
  xlab("T Stage") +
  ylab("Count")
```

## n_stage

``` r
# Bar Plot for n_stage
p4 = ggplot(clean_data, aes(x = factor(n_stage))) +
  geom_bar(fill = "orange") +
  ggtitle("N Stage Distribution") +
  xlab("N Stage") +
  ylab("Count")
```

## x6th_stage

``` r
# Bar Plot for x6th_stage
p5 = ggplot(clean_data, aes(x = factor(x6th_stage))) +
  geom_bar(fill = "orange") +
  ggtitle("x6th Stage Distribution") +
  xlab("x6th Stage") +
  ylab("Count")
```

## Tumor Size and Stage

``` r
# Scatter Plot for Tumor Size and T stage
ggplot(clean_data, aes(x = t_stage, y = tumor_size)) +
  geom_point(alpha = 0.6) +
  ggtitle("Tumor Size vs T Stage") +
  xlab("T Stage") +
  ylab("Tumor Size")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-30-1.png" width="90%" />

**The scatter plot suggests a possible trend where higher T stage
classifications; however, there is considerable variation within each T
stage category. Notably, T stages 1(2) through 3(4) show a wide range of
tumor sizes, with some larger tumors present in earlier stages and
smaller tumors in later stages, indicating that tumor size alone may not
be a definitive indicator of T stage.**

## differentiate

``` r
# Bar Plot for differentiate
p6 = ggplot(clean_data, aes(x = factor(differentiate))) +
  geom_bar(fill = "orange") +
  ggtitle("Diffferentiate Distribution") +
  xlab("Differentiate") +
  ylab("Count")
```

## grade

``` r
# Bar Plot for grade
p7 = ggplot(clean_data, aes(x = factor(grade))) +
  geom_bar(fill = "orange") +
  ggtitle("Grade Distribution") +
  xlab("Grade") +
  ylab("Count")
```

# a_stage

``` r
# Bar Plot for a_stage
p8 = ggplot(clean_data, aes(x = factor(a_stage))) +
  geom_bar(fill = "orange") +
  ggtitle("A Stage Distribution") +
  xlab("A Stage") +
  ylab("Count")
```

## tumor_size

``` r
# Bar Plot for tumor_size
ggplot(clean_data, aes(x = factor(tumor_size))) +
  geom_bar(fill = "orange") +
  ggtitle("Tumor Size Distribution") +
  xlab("Tumor Size") +
  ylab("Count")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-34-1.png" width="90%" />

## Survival Months and Status

``` r
# Boxplot for Survival Months by Status
ggplot(clean_data, aes(x = factor(status), y = survival_months)) +
  geom_boxplot(fill = "purple") +
  ggtitle("Survival Months by Patient Status") +
  xlab("Status (0: Dead, 1: Alive") +
  ylab("Survival Months")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-35-1.png" width="90%" />

**The boxplot displays that patients who are alive(Status 1) have a
wider range and generally higher survival months compared to those who
are dead (Status 0), where the survival time is more concentrated in a
lower range. This suggests a clear distinction in survival months
between the two groups, with patients who are alive experiencing longer
survival peridos post-diagnosis or treatment.**

## estrogen_status

``` r
# Bar Plot for estrogen_sstatus
p9 = ggplot(clean_data, aes(x = factor(estrogen_status))) +
  geom_bar(fill = "orange") +
  ggtitle("Estrogen Status Distribution") +
  xlab("Estrogen Status Stage") +
  ylab("Count")
```

# Progesterone_status

``` r
# Bar Plot for progesterone_status
p10 = ggplot(clean_data, aes(x = factor(progesterone_status))) +
  geom_bar(fill = "orange") +
  ggtitle("Progesterone Status Distribution") +
  xlab("Progesterone Status") +
  ylab("Count")
```

## Status

``` r
# Bar Plot for status
p11 = ggplot(clean_data, aes(x = factor(status))) +
  geom_bar(fill = "orange") +
  ggtitle("Status Distribution") +
  xlab("Status Stage") +
  ylab("Count")
```

## Hormone Status and Stage

``` r
# Bar Plot for Estrogen Status by T Stage
ggplot(clean_data, aes(x = factor(t_stage), fill = factor(estrogen_status))) +
  geom_bar(position = "dodge") +
  ggtitle("Estrogen Status by T Stage") +
  xlab("T Stage") +
  ylab("Count")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-39-1.png" width="90%" />

**The bar chart illustrates that the majority of patients across all T
stages have an estrogen status of “1” , which indicate a positive
hormone receptor sstatus. The prevalence of estrogen-positive status
decreases slightly in higher T stages, but it remains the dominant
category, suggesting a potential correlation between estrogen receptor
positivity and the presence of cancer across different T stages.**

## Regional Node Analysis

``` r
# Scatter Plot for Regional Nodes Examined vs Positive
ggplot(clean_data, aes(x = regional_node_examined, y = regional_node_positive)) +
  geom_point(color = "red", alpha = 0.5) +
  ggtitle("Regional Nodes Examined vs Positive") +
  xlab("Regional Nodes Examined") +
  ylab("Regional Nodes Positive")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-40-1.png" width="90%" />

**The scatter plot reveals a trend where the number of positive regional
nodes increases with the number off nodes examined, up to a point.
However, there is notable variability, especially when fewer nodes are
examined. Beyond a certain number of examined nodes, the count of
positive nodes tot level off, suggesting that examining more nodes does
not always correlate with finding a higher number of positive nodes.**

``` r
ggplot(clean_data, aes(x = differentiate, fill = factor(status))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "red", "1" = "green"),
                    labels = c("Dead", "Alive")) +
  labs(x = "Differentiation Level",
       y = "Proportion",
       fill = "Status") +
  ggtitle("Patient Status by Tumor Diffferentiation") +
  theme_minimal()
```

<img src="final_project_files/figure-gfm/unnamed-chunk-41-1.png" width="90%" />

**The bar chart indicates that the proportion of deceased patients (red)
is consistent across all levles of tumor differentiation, suggesting
that within this dataset, the differentiation level of the tumor may not
be a strong predictor of patient survival status.**

``` r
ggplot(clean_data, aes(x = factor(differentiate), 
                       y = survival_months, 
                       fill = as.factor(status))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "red", "1" = "green"),
                    labels = c("Dead", "Alive")) +
  labs(x = "Differentiation Level",
       y = "Proportion",
       fill = "Status") +
  ggtitle("Patient Status by Tumor Diffferentiation") +
  theme_minimal()
```

<img src="final_project_files/figure-gfm/unnamed-chunk-42-1.png" width="90%" />

## Combine barplot for charactter variables

``` r
p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11
```

<img src="final_project_files/figure-gfm/unnamed-chunk-43-1.png" width="90%" />

## Histogram for numeric variables

``` r
par(mfrow = c(3,2))
hist(data$age, main = "Age")
hist(data$tumor_size, main = "Tumor Size")
hist(data$regional_node_examined, main = "Regional Node Examined")
hist(data$reginol_node_positive, main = "Regional Node Positive")
hist(data$survival_months, main = "Survival Months")
```

<img src="final_project_files/figure-gfm/unnamed-chunk-44-1.png" width="90%" />

## race : age

``` r
# Convert numerical values to factors with appropriate labels
clean_data$race = factor(clean_data$race,
                         levels = c(0, 1, 2),
                         labels = c("White", "Black", "Other"))

clean_data$age = cut(clean_data$age, 
                        breaks = c(-Inf, 30, 50, Inf),
                        labels = c("Under 30", "30-50", "Over 50"),
                        right = FALSE)

clean_data$status = factor(clean_data$status, 
                            levels = c(0, 1),
                            labels = c("Dead", "Alive"))

# Group and summarize the data
summary_data_a = clean_data |>
  group_by(age, race, status) |>
  summarise(count = n(), .groups = "drop")

# Create a bar plot
ggplot(summary_data_a, aes(x = age, y = count, fill = status)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~race, scales = "free_y") +
  labs(title = "Age and Death Status by Race", 
       x = "Age", 
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        strip.background = element_rect(fill = "lightblue"),
        strip.text.x = element_text(size = 10, face = "bold"))
```

<img src="final_project_files/figure-gfm/unnamed-chunk-45-1.png" width="90%" />

## race: marital status

``` r
# Convert numerical values to factors with appropriate labels


clean_data$marital_status = factor(clean_data$marital_status, 
                                    levels = c(0, 1, 2, 3, 4),
                                    labels = c("Married", "Single", "Divorced", "Widowed", "Seperated"))


# Group and summarize the data
summary_data_b = clean_data |>
  group_by(marital_status, race, status) |>
  summarise(count = n(), .groups = "drop")

# Create a bar plot
ggplot(summary_data_b, aes(x = marital_status, y = count, fill = status)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~race, scales = "free_y") +
  labs(title = "Marital Status vs Death Status by Race", 
       x = "Marital Status", 
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        strip.background = element_rect(fill = "lightblue"),
        strip.text.x = element_text(size = 10, face = "bold"))
```

<img src="final_project_files/figure-gfm/unnamed-chunk-46-1.png" width="90%" />

The bar chart illustrates the count of individuals by marital status and
their survival status, segmented by race. It’s evident that for the
White and Other race categories, the majority of individuals are married
and alive, while the Black race category has a higher count of single
individuals who are alive. Across all race categories, the number of
deceased individuals is significantly lower than those alive, with the
widowed status showing a higher count of deceased individuals
particularly in the White race category.

``` r
data = data |>
  select(-survival_months)
```

# Model fitting

Based on boxplots, transformaiton is necesessary to reduce outliers

``` r
proj2 = data |>
tbl_summary(by="status",
  missing_text = "(Missing)", # counts missing values
  statistic = list(all_continuous() ~ "mean={mean} (min={min}, max={max}, sd={sd})",
                   all_categorical() ~ "n={n} (p={p}%)") # stats for categorical
  ) |>
bold_labels()  |>
italicize_levels()

#cube root of tumor size
clean_data2$tumor_size= (clean_data$tumor_size)^(1/3)
#log of regional_node_examied
clean_data2$regional_node_examined = log(clean_data$regional_node_examined)
#log of regional_node_positive
clean_data2$regional_node_positive = log(clean_data$regional_node_positive)
```

\#Find correlation

``` r
corplot=cor(clean_data2)
corrplot(corplot)
```

<img src="final_project_files/figure-gfm/unnamed-chunk-49-1.png" width="90%" />

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

<img src="final_project_files/figure-gfm/unnamed-chunk-49-2.png" width="90%" />

\#Separate training and testing set (80% training 20% testing )

``` r
# Calculate the size of each of the data sets
data_size = nrow(clean_data2)
train_size = floor(0.8 * data_size)
# Create a random sample of row indices for the training set
train_indices = sample(sample(seq_len(data_size), size = train_size))
# Subset the data into training and testing sets
#FUll variable
train_set = clean_data2[train_indices, ]
selectedData_train_set = selected_data[train_indices,]

test_set = clean_data2[-train_indices, ]
selectedData_test_set = selected_data[-train_indices, ]
```

# Fit a full model

``` r
# Data preparation
selected_train = train_set |>
  select(-tumor_size, -grade,-n_stage,-regional_node_positive,-x6th_stage)
# Fit a null model
null_model = glm(status ~ 1, family = binomial(link = "logit"), data = selected_train)#cleaned data
# Fit a full model
full_model=glm(status ~ . , family = binomial(link = "logit"), data = selected_train)
# Fit the interaction models
interaction_race_age = glm(status ~ . + race:age, family = binomial(link = "logit"), data = selected_train)

interaction_race_marital_status = glm(status ~ . + race:marital_status,  family = binomial(link = "logit"), data = selected_train)
```

# Using Forward, BackWard

    ## Start:  AIC=2778.8
    ## status ~ 1
    ## 
    ##                          Df Deviance    AIC
    ## + survival_months         1   2058.0 2062.0
    ## + progesterone_status     1   2673.9 2677.9
    ## + estrogen_status         1   2674.5 2678.5
    ## + differentiate           1   2684.4 2688.4
    ## + t_stage                 1   2706.5 2710.5
    ## + a_stage                 1   2755.0 2759.0
    ## + marital_status          1   2758.6 2762.6
    ## + age                     1   2767.0 2771.0
    ## + regional_node_examined  1   2772.5 2776.5
    ## <none>                        2776.8 2778.8
    ## + race                    1   2776.4 2780.4
    ## 
    ## Step:  AIC=2061.96
    ## status ~ survival_months
    ## 
    ##                          Df Deviance    AIC
    ## + differentiate           1   1994.3 2000.3
    ## + progesterone_status     1   2013.0 2019.0
    ## + t_stage                 1   2015.8 2021.8
    ## + estrogen_status         1   2020.8 2026.8
    ## + age                     1   2044.8 2050.8
    ## + marital_status          1   2046.2 2052.2
    ## + a_stage                 1   2051.7 2057.7
    ## + regional_node_examined  1   2055.6 2061.6
    ## <none>                        2058.0 2062.0
    ## + race                    1   2057.4 2063.4
    ## 
    ## Step:  AIC=2000.33
    ## status ~ survival_months + differentiate
    ## 
    ##                          Df Deviance    AIC
    ## + t_stage                 1   1964.3 1972.3
    ## + progesterone_status     1   1968.0 1976.0
    ## + age                     1   1974.3 1982.3
    ## + estrogen_status         1   1976.1 1984.1
    ## + marital_status          1   1982.9 1990.9
    ## + a_stage                 1   1989.5 1997.5
    ## <none>                        1994.3 2000.3
    ## + race                    1   1993.3 2001.3
    ## + regional_node_examined  1   1993.7 2001.7
    ## 
    ## Step:  AIC=1972.31
    ## status ~ survival_months + differentiate + t_stage
    ## 
    ##                          Df Deviance    AIC
    ## + progesterone_status     1   1939.3 1949.3
    ## + age                     1   1940.3 1950.3
    ## + estrogen_status         1   1947.8 1957.8
    ## + marital_status          1   1953.8 1963.8
    ## <none>                        1964.3 1972.3
    ## + a_stage                 1   1963.4 1973.4
    ## + race                    1   1963.4 1973.4
    ## + regional_node_examined  1   1964.3 1974.3
    ## 
    ## Step:  AIC=1949.28
    ## status ~ survival_months + differentiate + t_stage + progesterone_status
    ## 
    ##                          Df Deviance    AIC
    ## + age                     1   1918.1 1930.1
    ## + marital_status          1   1931.0 1943.0
    ## + estrogen_status         1   1936.0 1948.0
    ## <none>                        1939.3 1949.3
    ## + race                    1   1938.4 1950.4
    ## + a_stage                 1   1938.4 1950.4
    ## + regional_node_examined  1   1939.2 1951.2
    ## 
    ## Step:  AIC=1930.11
    ## status ~ survival_months + differentiate + t_stage + progesterone_status + 
    ##     age
    ## 
    ##                          Df Deviance    AIC
    ## + estrogen_status         1   1913.0 1927.0
    ## + marital_status          1   1913.3 1927.3
    ## <none>                        1918.1 1930.1
    ## + a_stage                 1   1916.8 1930.8
    ## + race                    1   1917.9 1931.9
    ## + regional_node_examined  1   1917.9 1931.9
    ## 
    ## Step:  AIC=1927.05
    ## status ~ survival_months + differentiate + t_stage + progesterone_status + 
    ##     age + estrogen_status
    ## 
    ##                          Df Deviance    AIC
    ## + marital_status          1   1908.3 1924.3
    ## <none>                        1913.0 1927.0
    ## + a_stage                 1   1911.8 1927.8
    ## + race                    1   1912.8 1928.8
    ## + regional_node_examined  1   1912.9 1928.9
    ## 
    ## Step:  AIC=1924.32
    ## status ~ survival_months + differentiate + t_stage + progesterone_status + 
    ##     age + estrogen_status + marital_status
    ## 
    ##                          Df Deviance    AIC
    ## <none>                        1908.3 1924.3
    ## + a_stage                 1   1907.1 1925.1
    ## + race                    1   1907.9 1925.9
    ## + regional_node_examined  1   1908.2 1926.2

    ## Start:  AIC=1928.6
    ## status ~ age + race + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status + regional_node_examined + 
    ##     survival_months
    ## 
    ##                          Df Deviance    AIC
    ## - regional_node_examined  1   1906.7 1926.7
    ## - race                    1   1907.0 1927.0
    ## - a_stage                 1   1907.8 1927.8
    ## <none>                        1906.6 1928.6
    ## - marital_status          1   1911.4 1931.4
    ## - estrogen_status         1   1911.5 1931.5
    ## - progesterone_status     1   1914.6 1934.6
    ## - age                     1   1925.5 1945.5
    ## - t_stage                 1   1932.4 1952.4
    ## - differentiate           1   1942.7 1962.7
    ## - survival_months         1   2513.4 2533.4
    ## 
    ## Step:  AIC=1926.73
    ## status ~ age + race + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status + survival_months
    ## 
    ##                       Df Deviance    AIC
    ## - race                 1   1907.1 1925.1
    ## - a_stage              1   1907.9 1925.9
    ## <none>                     1906.7 1926.7
    ## - marital_status       1   1911.6 1929.6
    ## - estrogen_status      1   1911.7 1929.7
    ## - progesterone_status  1   1914.8 1932.8
    ## - age                  1   1925.6 1943.6
    ## - t_stage              1   1933.1 1951.1
    ## - differentiate        1   1943.3 1961.3
    ## - survival_months      1   2513.5 2531.5
    ## 
    ## Step:  AIC=1925.11
    ## status ~ age + marital_status + t_stage + differentiate + a_stage + 
    ##     estrogen_status + progesterone_status + survival_months
    ## 
    ##                       Df Deviance    AIC
    ## - a_stage              1   1908.3 1924.3
    ## <none>                     1907.1 1925.1
    ## - marital_status       1   1911.8 1927.8
    ## - estrogen_status      1   1912.0 1928.0
    ## - progesterone_status  1   1915.1 1931.1
    ## - age                  1   1926.8 1942.8
    ## - t_stage              1   1933.7 1949.7
    ## - differentiate        1   1943.5 1959.5
    ## - survival_months      1   2514.3 2530.3
    ## 
    ## Step:  AIC=1924.32
    ## status ~ age + marital_status + t_stage + differentiate + estrogen_status + 
    ##     progesterone_status + survival_months
    ## 
    ##                       Df Deviance    AIC
    ## <none>                     1908.3 1924.3
    ## - marital_status       1   1913.0 1927.0
    ## - estrogen_status      1   1913.3 1927.3
    ## - progesterone_status  1   1916.3 1930.3
    ## - age                  1   1927.7 1941.7
    ## - t_stage              1   1938.9 1952.9
    ## - differentiate        1   1944.7 1958.7
    ## - survival_months      1   2518.3 2532.3

    ## 
    ## Call:
    ## glm(formula = status ~ age + marital_status + t_stage + differentiate + 
    ##     estrogen_status + progesterone_status + survival_months, 
    ##     family = binomial(link = "logit"), data = selected_train)
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.94042    0.44677  -4.343 1.40e-05 ***
    ## age                 -0.02958    0.00681  -4.344 1.40e-05 ***
    ## marital_status      -0.12613    0.05746  -2.195  0.02815 *  
    ## t_stage             -0.41753    0.07532  -5.544 2.96e-08 ***
    ## differentiate        0.58809    0.09908   5.935 2.93e-09 ***
    ## estrogen_status      0.53662    0.24020   2.234  0.02548 *  
    ## progesterone_status  0.47233    0.16416   2.877  0.00401 ** 
    ## survival_months      0.06120    0.00299  20.467  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2776.8  on 3218  degrees of freedom
    ## Residual deviance: 1908.3  on 3211  degrees of freedom
    ## AIC: 1924.3
    ## 
    ## Number of Fisher Scoring iterations: 6

    ## 
    ## Call:
    ## glm(formula = status ~ survival_months + differentiate + t_stage + 
    ##     progesterone_status + age + estrogen_status + marital_status, 
    ##     family = binomial(link = "logit"), data = selected_train)
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.94042    0.44677  -4.343 1.40e-05 ***
    ## survival_months      0.06120    0.00299  20.467  < 2e-16 ***
    ## differentiate        0.58809    0.09908   5.935 2.93e-09 ***
    ## t_stage             -0.41753    0.07532  -5.544 2.96e-08 ***
    ## progesterone_status  0.47233    0.16416   2.877  0.00401 ** 
    ## age                 -0.02958    0.00681  -4.344 1.40e-05 ***
    ## estrogen_status      0.53662    0.24020   2.234  0.02548 *  
    ## marital_status      -0.12613    0.05746  -2.195  0.02815 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2776.8  on 3218  degrees of freedom
    ## Residual deviance: 1908.3  on 3211  degrees of freedom
    ## AIC: 1924.3
    ## 
    ## Number of Fisher Scoring iterations: 6

    ## Analysis of Deviance Table
    ## 
    ## Model 1: status ~ age + marital_status + t_stage + differentiate + estrogen_status + 
    ##     progesterone_status + survival_months
    ## Model 2: status ~ survival_months + differentiate + t_stage + progesterone_status + 
    ##     age + estrogen_status + marital_status
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      3211     1908.3                     
    ## 2      3211     1908.3  0        0

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Area under the curve: 0.863

    ## 
    ## Call:
    ## glm(formula = status ~ survival_months + differentiate + t_stage + 
    ##     progesterone_status + age + estrogen_status + marital_status, 
    ##     family = binomial(link = "logit"), data = selected_train)
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.94042    0.44677  -4.343 1.40e-05 ***
    ## survival_months      0.06120    0.00299  20.467  < 2e-16 ***
    ## differentiate        0.58809    0.09908   5.935 2.93e-09 ***
    ## t_stage             -0.41753    0.07532  -5.544 2.96e-08 ***
    ## progesterone_status  0.47233    0.16416   2.877  0.00401 ** 
    ## age                 -0.02958    0.00681  -4.344 1.40e-05 ***
    ## estrogen_status      0.53662    0.24020   2.234  0.02548 *  
    ## marital_status      -0.12613    0.05746  -2.195  0.02815 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2776.8  on 3218  degrees of freedom
    ## Residual deviance: 1908.3  on 3211  degrees of freedom
    ## AIC: 1924.3
    ## 
    ## Number of Fisher Scoring iterations: 6

# Interaction: race \*marital_status

``` r
# Forward Stepwise Model Selection with Interaction Term
step_modelF_2 = step(null_model, scope = list(lower = null_model, upper = interaction_race_marital_status), 
                   direction = "forward")
```

    ## Start:  AIC=2778.8
    ## status ~ 1
    ## 
    ##                          Df Deviance    AIC
    ## + survival_months         1   2058.0 2062.0
    ## + progesterone_status     1   2673.9 2677.9
    ## + estrogen_status         1   2674.5 2678.5
    ## + differentiate           1   2684.4 2688.4
    ## + t_stage                 1   2706.5 2710.5
    ## + a_stage                 1   2755.0 2759.0
    ## + marital_status          1   2758.6 2762.6
    ## + age                     1   2767.0 2771.0
    ## + regional_node_examined  1   2772.5 2776.5
    ## <none>                        2776.8 2778.8
    ## + race                    1   2776.4 2780.4
    ## 
    ## Step:  AIC=2061.96
    ## status ~ survival_months
    ## 
    ##                          Df Deviance    AIC
    ## + differentiate           1   1994.3 2000.3
    ## + progesterone_status     1   2013.0 2019.0
    ## + t_stage                 1   2015.8 2021.8
    ## + estrogen_status         1   2020.8 2026.8
    ## + age                     1   2044.8 2050.8
    ## + marital_status          1   2046.2 2052.2
    ## + a_stage                 1   2051.7 2057.7
    ## + regional_node_examined  1   2055.6 2061.6
    ## <none>                        2058.0 2062.0
    ## + race                    1   2057.4 2063.4
    ## 
    ## Step:  AIC=2000.33
    ## status ~ survival_months + differentiate
    ## 
    ##                          Df Deviance    AIC
    ## + t_stage                 1   1964.3 1972.3
    ## + progesterone_status     1   1968.0 1976.0
    ## + age                     1   1974.3 1982.3
    ## + estrogen_status         1   1976.1 1984.1
    ## + marital_status          1   1982.9 1990.9
    ## + a_stage                 1   1989.5 1997.5
    ## <none>                        1994.3 2000.3
    ## + race                    1   1993.3 2001.3
    ## + regional_node_examined  1   1993.7 2001.7
    ## 
    ## Step:  AIC=1972.31
    ## status ~ survival_months + differentiate + t_stage
    ## 
    ##                          Df Deviance    AIC
    ## + progesterone_status     1   1939.3 1949.3
    ## + age                     1   1940.3 1950.3
    ## + estrogen_status         1   1947.8 1957.8
    ## + marital_status          1   1953.8 1963.8
    ## <none>                        1964.3 1972.3
    ## + a_stage                 1   1963.4 1973.4
    ## + race                    1   1963.4 1973.4
    ## + regional_node_examined  1   1964.3 1974.3
    ## 
    ## Step:  AIC=1949.28
    ## status ~ survival_months + differentiate + t_stage + progesterone_status
    ## 
    ##                          Df Deviance    AIC
    ## + age                     1   1918.1 1930.1
    ## + marital_status          1   1931.0 1943.0
    ## + estrogen_status         1   1936.0 1948.0
    ## <none>                        1939.3 1949.3
    ## + race                    1   1938.4 1950.4
    ## + a_stage                 1   1938.4 1950.4
    ## + regional_node_examined  1   1939.2 1951.2
    ## 
    ## Step:  AIC=1930.11
    ## status ~ survival_months + differentiate + t_stage + progesterone_status + 
    ##     age
    ## 
    ##                          Df Deviance    AIC
    ## + estrogen_status         1   1913.0 1927.0
    ## + marital_status          1   1913.3 1927.3
    ## <none>                        1918.1 1930.1
    ## + a_stage                 1   1916.8 1930.8
    ## + race                    1   1917.9 1931.9
    ## + regional_node_examined  1   1917.9 1931.9
    ## 
    ## Step:  AIC=1927.05
    ## status ~ survival_months + differentiate + t_stage + progesterone_status + 
    ##     age + estrogen_status
    ## 
    ##                          Df Deviance    AIC
    ## + marital_status          1   1908.3 1924.3
    ## <none>                        1913.0 1927.0
    ## + a_stage                 1   1911.8 1927.8
    ## + race                    1   1912.8 1928.8
    ## + regional_node_examined  1   1912.9 1928.9
    ## 
    ## Step:  AIC=1924.32
    ## status ~ survival_months + differentiate + t_stage + progesterone_status + 
    ##     age + estrogen_status + marital_status
    ## 
    ##                          Df Deviance    AIC
    ## <none>                        1908.3 1924.3
    ## + a_stage                 1   1907.1 1925.1
    ## + race                    1   1907.9 1925.9
    ## + regional_node_examined  1   1908.2 1926.2

``` r
# Backward Stepwise Model Selection with Interaction Term:
step_model_2 = step(interaction_race_marital_status, direction = "backward")
```

    ## Start:  AIC=1926.8
    ## status ~ age + race + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status + regional_node_examined + 
    ##     survival_months + race:marital_status
    ## 
    ##                          Df Deviance    AIC
    ## - regional_node_examined  1   1903.0 1925.0
    ## - a_stage                 1   1904.0 1926.0
    ## <none>                        1902.8 1926.8
    ## - race:marital_status     1   1906.6 1928.6
    ## - estrogen_status         1   1907.5 1929.5
    ## - progesterone_status     1   1910.6 1932.6
    ## - age                     1   1921.8 1943.8
    ## - t_stage                 1   1928.0 1950.0
    ## - differentiate           1   1939.5 1961.5
    ## - survival_months         1   2510.3 2532.3
    ## 
    ## Step:  AIC=1925
    ## status ~ age + race + marital_status + t_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status + survival_months + 
    ##     race:marital_status
    ## 
    ##                       Df Deviance    AIC
    ## - a_stage              1   1904.3 1924.3
    ## <none>                     1903.0 1925.0
    ## - race:marital_status  1   1906.7 1926.7
    ## - estrogen_status      1   1907.8 1927.8
    ## - progesterone_status  1   1910.7 1930.7
    ## - age                  1   1921.8 1941.8
    ## - t_stage              1   1928.9 1948.9
    ## - differentiate        1   1940.2 1960.2
    ## - survival_months      1   2510.4 2530.4
    ## 
    ## Step:  AIC=1924.26
    ## status ~ age + race + marital_status + t_stage + differentiate + 
    ##     estrogen_status + progesterone_status + survival_months + 
    ##     race:marital_status
    ## 
    ##                       Df Deviance    AIC
    ## <none>                     1904.3 1924.3
    ## - race:marital_status  1   1907.9 1925.9
    ## - estrogen_status      1   1909.2 1927.2
    ## - progesterone_status  1   1912.0 1930.0
    ## - age                  1   1922.8 1940.8
    ## - t_stage              1   1934.2 1952.2
    ## - differentiate        1   1941.3 1959.3
    ## - survival_months      1   2514.5 2532.5

``` r
# Model Summaries
summary(step_model_2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ age + race + marital_status + t_stage + 
    ##     differentiate + estrogen_status + progesterone_status + survival_months + 
    ##     race:marital_status, family = binomial(link = "logit"), data = selected_train)
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -2.027489   0.453765  -4.468 7.89e-06 ***
    ## age                 -0.029152   0.006854  -4.253 2.11e-05 ***
    ## race                 0.198053   0.130472   1.518  0.12902    
    ## marital_status      -0.079671   0.063124  -1.262  0.20690    
    ## t_stage             -0.413882   0.075476  -5.484 4.17e-08 ***
    ## differentiate        0.594638   0.099208   5.994 2.05e-09 ***
    ## estrogen_status      0.537206   0.241564   2.224  0.02616 *  
    ## progesterone_status  0.464737   0.164444   2.826  0.00471 ** 
    ## survival_months      0.061365   0.002999  20.461  < 2e-16 ***
    ## race:marital_status -0.177620   0.091529  -1.941  0.05231 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2776.8  on 3218  degrees of freedom
    ## Residual deviance: 1904.3  on 3209  degrees of freedom
    ## AIC: 1924.3
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
summary(step_modelF_2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ survival_months + differentiate + t_stage + 
    ##     progesterone_status + age + estrogen_status + marital_status, 
    ##     family = binomial(link = "logit"), data = selected_train)
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.94042    0.44677  -4.343 1.40e-05 ***
    ## survival_months      0.06120    0.00299  20.467  < 2e-16 ***
    ## differentiate        0.58809    0.09908   5.935 2.93e-09 ***
    ## t_stage             -0.41753    0.07532  -5.544 2.96e-08 ***
    ## progesterone_status  0.47233    0.16416   2.877  0.00401 ** 
    ## age                 -0.02958    0.00681  -4.344 1.40e-05 ***
    ## estrogen_status      0.53662    0.24020   2.234  0.02548 *  
    ## marital_status      -0.12613    0.05746  -2.195  0.02815 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2776.8  on 3218  degrees of freedom
    ## Residual deviance: 1908.3  on 3211  degrees of freedom
    ## AIC: 1924.3
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# Prediction and ROC Curve with Interaction Term:
test_predictions_log_oddsStep_2 = predict(step_model_2, newdata  = (test_set),type='response')
test_predictions_probStep_2 = plogis(test_predictions_log_oddsStep_2)
roc_curveStep_2 = roc(response = (test_set$status), predictor = as.numeric(test_predictions_probStep_2))
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
auc(roc_curveStep_2)
```

    ## Area under the curve: 0.8637

\#Elastic Net

``` r
# Prepare your data
X <- as.matrix(train_set[, setdiff(names(train_set), "status")])  
# Predictor variables
y <- train_set$status  
# Response variable
lambda_seq <- 10^seq(-2, 0, by = .001)

# Use cross-validation to find the optimal lambda
cv_object <- cv.glmnet(X, y, family = "binomial", alpha = 0.5, type.measure = "class",nfolds=5, lambda = lambda_seq)

tibble(lambda = cv_object$lambda,
mean_cv_error = cv_object$cvm) %>%
ggplot(aes(x = lambda, y = mean_cv_error)) +
geom_point()
```

<img src="final_project_files/figure-gfm/unnamed-chunk-54-1.png" width="90%" />

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

    ## Area under the curve: 0.8721

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

<img src="final_project_files/figure-gfm/unnamed-chunk-55-1.png" width="90%" />

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

    ## Area under the curve: 0.8721

``` r
plot(roc_curve, main = "ROC Curve", col = "#1c61b6", lwd = 2)
lines(roc_curveStep,col='yellow')
lines(roc_curvenet2,col='green')
lines(roc_curveStep_2,col='purple')
M1=paste('M1',round(auc(roc_curveStep),4))
M2=paste('M2',round(auc(roc_curveStep_2),4))
M3=paste('M3',round(auc(roc_curve),4))
M4=paste('M4:',round(auc(roc_curvenet2),4))
legend("bottomright", 
       legend=c(M1,M2,M3,M4),
       col=c("yellow","purple","#1c61b6","green"), 
       lwd=2)
```

<img src="final_project_files/figure-gfm/unnamed-chunk-55-2.png" width="90%" />
Based on the ROC and AUC, final_model2 is the best prediction.
finalModel has more variables than final_model2 but perform the same as
finalModel2 Logistics regression without Elastic net has less AUC than
final_Model2

\#final Model Diagonstics :

``` r
predicted_classes = as.numeric(test_predictions_probElastic2 >sum(clean_data2$status)/nrow(clean_data2))

predicted_classes = as.numeric(test_predictions_probElastic2 >0.5)
predicted.classes = as.factor(test_set$status)

outcome = as.factor(test_set$status)
predicted_classes = factor(predicted_classes, levels = c("0", "1"))
outcome = factor(outcome, levels = c("0", "1"))

conf_matrix = confusionMatrix(predicted_classes, outcome)
conf_matrix
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0  53  15
    ##          1  64 673
    ##                                           
    ##                Accuracy : 0.9019          
    ##                  95% CI : (0.8792, 0.9215)
    ##     No Information Rate : 0.8547          
    ##     P-Value [Acc > NIR] : 4.200e-05       
    ##                                           
    ##                   Kappa : 0.5219          
    ##                                           
    ##  Mcnemar's Test P-Value : 6.648e-08       
    ##                                           
    ##             Sensitivity : 0.45299         
    ##             Specificity : 0.97820         
    ##          Pos Pred Value : 0.77941         
    ##          Neg Pred Value : 0.91316         
    ##              Prevalence : 0.14534         
    ##          Detection Rate : 0.06584         
    ##    Detection Prevalence : 0.08447         
    ##       Balanced Accuracy : 0.71559         
    ##                                           
    ##        'Positive' Class : 0               
    ## 
