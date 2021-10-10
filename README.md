
# tidyreport

## Overview

tidyreport is a pipeline to conduct common statistical analyses,
especially those done in the field of epidemiology and biostatistics and
generate clean/formatted tables from the statistical output. The
analyses include sample descriptive statistics, univariable testing
(Wilcoxon rank sum, KW tests, Chisq/Fisher), regression analysis
(linear, logistic, tobit, ordinal, LME, Normal GEE, Logistic GEE,
Poisson GEE, Ordinal GEE, cox). The generated tables can be readily
copied into Excel or Word for scientific paper writing.

The main functions are:

-   `get_desc_stat()` generates descrptive statistics of the sample.
-   `get_desc_stat_grouping()` generates descrptive statistics of the
    sample and stratified by a grouping variable, along with statistical
    testing of group differences.
-   `get_regression_estimates()` runs different types of regression and
    summarizes its results.
-   `cox_summary()` summarizes the results of cox regression.

## Installation

``` r
install.packages("devtools")
devtools::install_github("JiyueQin/tidyreport")
```

## Usage

``` r
library(tidyreport)
# here is a sample dataset modified from the dataset starwars in tidyverse .
str(sample_dat)
```

    ## tibble[,5] [83 x 5] (S3: tbl_df/tbl/data.frame)
    ##  $ height          : int [1:83] 172 167 96 202 150 178 165 97 183 182 ...
    ##  $ sex             : chr [1:83] "male" "none" "none" "male" ...
    ##  $ gender          : chr [1:83] "masculine" "masculine" "masculine" "masculine" ...
    ##  $ mass            : num [1:83] 77 75 32 136 49 120 75 32 84 77 ...
    ##  $ hair_color_group: chr [1:83] "other" "other" "other" "none" ...

### Get sample descriptive statistics

``` r
get_desc_stat(sample_dat)
```

<table class="table" style="width: auto !important; ">
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:left;">
Full Sample(N=83)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Height, Mean(SD)
</td>
<td style="text-align:left;">
174.1(35.4)
</td>
</tr>
<tr>
<td style="text-align:left;">
Mass, Mean(SD)
</td>
<td style="text-align:left;">
98.2(170.8)
</td>
</tr>
<tr>
<td style="text-align:left;">
Sex, N(%)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Female
</td>
<td style="text-align:left;">
16(19.3)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Hermaphroditic
</td>
<td style="text-align:left;">
1(1.2)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Male
</td>
<td style="text-align:left;">
60(72.3)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
None
</td>
<td style="text-align:left;">
6(7.2)
</td>
</tr>
<tr>
<td style="text-align:left;">
Gender, N(%)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Feminine
</td>
<td style="text-align:left;">
17(20.5)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Masculine
</td>
<td style="text-align:left;">
66(79.5)
</td>
</tr>
<tr>
<td style="text-align:left;">
Hair\_color\_group, N(%)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Black
</td>
<td style="text-align:left;">
12(14.5)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Brown
</td>
<td style="text-align:left;">
17(20.5)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
None
</td>
<td style="text-align:left;">
36(43.4)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Other
</td>
<td style="text-align:left;">
18(21.7)
</td>
</tr>
</tbody>
</table>

### Get descriptive statistics stratified by groups

``` r
# stratified by gender and testing for gender difference 
get_desc_stat_grouping(sample_dat, 'gender')
```

<table class="table" style="width: auto !important; ">
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:left;">
Full Sample(N=83)
</th>
<th style="text-align:left;">
Feminine(N=17)
</th>
<th style="text-align:left;">
Masculine(N=66)
</th>
<th style="text-align:left;">
P Value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Height, Mean(SD)
</td>
<td style="text-align:left;">
174.1(35.4)
</td>
<td style="text-align:left;">
164.7(23.6)
</td>
<td style="text-align:left;">
176.5(37.6)
</td>
<td style="text-align:left;">
0.003
</td>
</tr>
<tr>
<td style="text-align:left;">
Mass, Mean(SD)
</td>
<td style="text-align:left;">
98.2(170.8)
</td>
<td style="text-align:left;">
54.7(8.6)
</td>
<td style="text-align:left;">
106.1(185)
</td>
<td style="text-align:left;">
0.002
</td>
</tr>
<tr>
<td style="text-align:left;">
Sex, N(%)
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Female
</td>
<td style="text-align:left;">
16(19.3)
</td>
<td style="text-align:left;">
16(94.1)
</td>
<td style="text-align:left;">
0(0)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Hermaphroditic
</td>
<td style="text-align:left;">
1(1.2)
</td>
<td style="text-align:left;">
0(0)
</td>
<td style="text-align:left;">
1(1.5)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Male
</td>
<td style="text-align:left;">
60(72.3)
</td>
<td style="text-align:left;">
0(0)
</td>
<td style="text-align:left;">
60(90.9)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
None
</td>
<td style="text-align:left;">
6(7.2)
</td>
<td style="text-align:left;">
1(5.9)
</td>
<td style="text-align:left;">
5(7.6)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Hair\_color\_group, N(%)
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
0.316
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Black
</td>
<td style="text-align:left;">
12(14.5)
</td>
<td style="text-align:left;">
3(17.6)
</td>
<td style="text-align:left;">
9(13.6)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Brown
</td>
<td style="text-align:left;">
17(20.5)
</td>
<td style="text-align:left;">
6(35.3)
</td>
<td style="text-align:left;">
11(16.7)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
None
</td>
<td style="text-align:left;">
36(43.4)
</td>
<td style="text-align:left;">
5(29.4)
</td>
<td style="text-align:left;">
31(47)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Other
</td>
<td style="text-align:left;">
18(21.7)
</td>
<td style="text-align:left;">
3(17.6)
</td>
<td style="text-align:left;">
15(22.7)
</td>
<td style="text-align:left;">
</td>
</tr>
</tbody>
</table>

``` r
# report median(IQR) instead of mean(SD) for height, highlight significant p values(default is 0.05)
get_desc_stat_grouping(sample_dat, 'gender', median_vars = 'height', highlight=T)
```

<table class="table" style="width: auto !important; ">
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:left;">
Full Sample(N=83)
</th>
<th style="text-align:left;">
Feminine(N=17)
</th>
<th style="text-align:left;">
Masculine(N=66)
</th>
<th style="text-align:left;">
P Value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Mass, Mean(SD)
</td>
<td style="text-align:left;">
98.2(170.8)
</td>
<td style="text-align:left;">
54.7(8.6)
</td>
<td style="text-align:left;">
106.1(185)
</td>
<td style="text-align:left;">
<span
style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: yellow !important;">0.002</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
Height, Median(IQR)
</td>
<td style="text-align:left;">
180(166.2, 191)
</td>
<td style="text-align:left;">
165.5(161.5, 172)
</td>
<td style="text-align:left;">
183(171.2, 193)
</td>
<td style="text-align:left;">
<span
style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: yellow !important;">0.003</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
Sex, N(%)
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
<span
style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: yellow !important;">&lt;0.001</span>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Female
</td>
<td style="text-align:left;">
16(19.3)
</td>
<td style="text-align:left;">
16(94.1)
</td>
<td style="text-align:left;">
0(0)
</td>
<td style="text-align:left;">
<span
style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: white !important;"></span>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Hermaphroditic
</td>
<td style="text-align:left;">
1(1.2)
</td>
<td style="text-align:left;">
0(0)
</td>
<td style="text-align:left;">
1(1.5)
</td>
<td style="text-align:left;">
<span
style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: white !important;"></span>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Male
</td>
<td style="text-align:left;">
60(72.3)
</td>
<td style="text-align:left;">
0(0)
</td>
<td style="text-align:left;">
60(90.9)
</td>
<td style="text-align:left;">
<span
style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: white !important;"></span>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
None
</td>
<td style="text-align:left;">
6(7.2)
</td>
<td style="text-align:left;">
1(5.9)
</td>
<td style="text-align:left;">
5(7.6)
</td>
<td style="text-align:left;">
<span
style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: white !important;"></span>
</td>
</tr>
<tr>
<td style="text-align:left;">
Hair\_color\_group, N(%)
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
<span
style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: white !important;">0.316</span>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Black
</td>
<td style="text-align:left;">
12(14.5)
</td>
<td style="text-align:left;">
3(17.6)
</td>
<td style="text-align:left;">
9(13.6)
</td>
<td style="text-align:left;">
<span
style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: white !important;"></span>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Brown
</td>
<td style="text-align:left;">
17(20.5)
</td>
<td style="text-align:left;">
6(35.3)
</td>
<td style="text-align:left;">
11(16.7)
</td>
<td style="text-align:left;">
<span
style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: white !important;"></span>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
None
</td>
<td style="text-align:left;">
36(43.4)
</td>
<td style="text-align:left;">
5(29.4)
</td>
<td style="text-align:left;">
31(47)
</td>
<td style="text-align:left;">
<span
style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: white !important;"></span>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Other
</td>
<td style="text-align:left;">
18(21.7)
</td>
<td style="text-align:left;">
3(17.6)
</td>
<td style="text-align:left;">
15(22.7)
</td>
<td style="text-align:left;">
<span
style="     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: white !important;"></span>
</td>
</tr>
</tbody>
</table>

``` r
# get detailed descriptive statistics stratified by gender, sort the table by the order of variables in the data, no statistical testing
get_desc_stat_grouping(sample_dat, 'gender', detail = T, sort = T, test = F)
```

<table class="table" style="width: auto !important; ">
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:left;">
Full Sample(N=83)
</th>
<th style="text-align:left;">
Feminine(N=17)
</th>
<th style="text-align:left;">
Masculine(N=66)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Height
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Min
</td>
<td style="text-align:left;">
66
</td>
<td style="text-align:left;">
96
</td>
<td style="text-align:left;">
66
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Q1
</td>
<td style="text-align:left;">
166.2
</td>
<td style="text-align:left;">
161.5
</td>
<td style="text-align:left;">
171.2
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Median
</td>
<td style="text-align:left;">
180
</td>
<td style="text-align:left;">
165.5
</td>
<td style="text-align:left;">
183
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Mean
</td>
<td style="text-align:left;">
174.1
</td>
<td style="text-align:left;">
164.7
</td>
<td style="text-align:left;">
176.5
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Q3
</td>
<td style="text-align:left;">
191
</td>
<td style="text-align:left;">
172
</td>
<td style="text-align:left;">
193
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Max
</td>
<td style="text-align:left;">
264
</td>
<td style="text-align:left;">
213
</td>
<td style="text-align:left;">
264
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Sd
</td>
<td style="text-align:left;">
35.4
</td>
<td style="text-align:left;">
23.6
</td>
<td style="text-align:left;">
37.6
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Nmiss
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Sex, N(%)
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Female
</td>
<td style="text-align:left;">
16(19.3)
</td>
<td style="text-align:left;">
16(94.1)
</td>
<td style="text-align:left;">
0(0)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Hermaphroditic
</td>
<td style="text-align:left;">
1(1.2)
</td>
<td style="text-align:left;">
0(0)
</td>
<td style="text-align:left;">
1(1.5)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Male
</td>
<td style="text-align:left;">
60(72.3)
</td>
<td style="text-align:left;">
0(0)
</td>
<td style="text-align:left;">
60(90.9)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
None
</td>
<td style="text-align:left;">
6(7.2)
</td>
<td style="text-align:left;">
1(5.9)
</td>
<td style="text-align:left;">
5(7.6)
</td>
</tr>
<tr>
<td style="text-align:left;">
Mass
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Min
</td>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
45
</td>
<td style="text-align:left;">
15
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Q1
</td>
<td style="text-align:left;">
56.4
</td>
<td style="text-align:left;">
50
</td>
<td style="text-align:left;">
75
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Median
</td>
<td style="text-align:left;">
79
</td>
<td style="text-align:left;">
55
</td>
<td style="text-align:left;">
80
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Mean
</td>
<td style="text-align:left;">
98.2
</td>
<td style="text-align:left;">
54.7
</td>
<td style="text-align:left;">
106.1
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Q3
</td>
<td style="text-align:left;">
84.8
</td>
<td style="text-align:left;">
56.2
</td>
<td style="text-align:left;">
88
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Max
</td>
<td style="text-align:left;">
1358
</td>
<td style="text-align:left;">
75
</td>
<td style="text-align:left;">
1358
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Sd
</td>
<td style="text-align:left;">
170.8
</td>
<td style="text-align:left;">
8.6
</td>
<td style="text-align:left;">
185
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Nmiss
</td>
<td style="text-align:left;">
25
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
Hair\_color\_group, N(%)
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Black
</td>
<td style="text-align:left;">
12(14.5)
</td>
<td style="text-align:left;">
3(17.6)
</td>
<td style="text-align:left;">
9(13.6)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Brown
</td>
<td style="text-align:left;">
17(20.5)
</td>
<td style="text-align:left;">
6(35.3)
</td>
<td style="text-align:left;">
11(16.7)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
None
</td>
<td style="text-align:left;">
36(43.4)
</td>
<td style="text-align:left;">
5(29.4)
</td>
<td style="text-align:left;">
31(47)
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
Other
</td>
<td style="text-align:left;">
18(21.7)
</td>
<td style="text-align:left;">
3(17.6)
</td>
<td style="text-align:left;">
15(22.7)
</td>
</tr>
</tbody>
</table>

### Regression

``` r
# get formatted table for a logistic regression model
get_regression_estimates(dplyr::mutate(sample_dat, gender = as.factor(gender)), outcome = 'gender', predictor_vec = c( 'height', 'hair_color_group'), outcome_type = 'binary', format =T)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
outcome
</th>
<th style="text-align:left;">
term
</th>
<th style="text-align:left;">
OR
</th>
<th style="text-align:left;">
CI
</th>
<th style="text-align:left;">
p
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;vertical-align: top !important;" rowspan="5">
gender
</td>
<td style="text-align:left;">
height
</td>
<td style="text-align:left;">
1.01
</td>
<td style="text-align:left;">
(0.99,1.03)
</td>
<td style="text-align:left;">
0.216
</td>
</tr>
<tr>
<td style="text-align:left;">
hair\_color\_group
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
brown vs black
</td>
<td style="text-align:left;">
0.67
</td>
<td style="text-align:left;">
(0.12,3.8)
</td>
<td style="text-align:left;">
0.654
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
none vs black
</td>
<td style="text-align:left;">
2.19
</td>
<td style="text-align:left;">
(0.43,11.3)
</td>
<td style="text-align:left;">
0.348
</td>
</tr>
<tr>
<td style="text-align:left; padding-left:  2em;" indentlevel="1">
other vs black
</td>
<td style="text-align:left;">
2.21
</td>
<td style="text-align:left;">
(0.35,14.17)
</td>
<td style="text-align:left;">
0.401
</td>
</tr>
</tbody>
</table>

``` r
# perform linear regression for multiple outcomes with purrr and get tables with kableExtra
purrr::map_df(c('height', 'mass'), ~get_regression_estimates(sample_dat, outcome = .x, predictor_vec = c( 'sex', 'hair_color_group'), outcome_type = 'linear')) %>% kableExtra::kable() %>% kableExtra::kable_styling() %>% kableExtra::collapse_rows(1)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
outcome
</th>
<th style="text-align:left;">
term
</th>
<th style="text-align:left;">
estimate
</th>
<th style="text-align:left;">
CI
</th>
<th style="text-align:left;">
p
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;vertical-align: middle !important;" rowspan="7">
height
</td>
<td style="text-align:left;">
(Intercept)
</td>
<td style="text-align:left;">
167.72
</td>
<td style="text-align:left;">
(142.44,192.99)
</td>
<td style="text-align:left;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
sexhermaphroditic
</td>
<td style="text-align:left;">
14.15
</td>
<td style="text-align:left;">
(-57.95,86.26)
</td>
<td style="text-align:left;">
0.697
</td>
</tr>
<tr>
<td style="text-align:left;">
sexmale
</td>
<td style="text-align:left;">
8.02
</td>
<td style="text-align:left;">
(-12.26,28.29)
</td>
<td style="text-align:left;">
0.433
</td>
</tr>
<tr>
<td style="text-align:left;">
sexnone
</td>
<td style="text-align:left;">
-35.86
</td>
<td style="text-align:left;">
(-72.53,0.81)
</td>
<td style="text-align:left;">
0.055
</td>
</tr>
<tr>
<td style="text-align:left;">
hair\_color\_groupbrown
</td>
<td style="text-align:left;">
1.85
</td>
<td style="text-align:left;">
(-25.64,29.33)
</td>
<td style="text-align:left;">
0.894
</td>
</tr>
<tr>
<td style="text-align:left;">
hair\_color\_groupnone
</td>
<td style="text-align:left;">
8.66
</td>
<td style="text-align:left;">
(-15.14,32.46)
</td>
<td style="text-align:left;">
0.47
</td>
</tr>
<tr>
<td style="text-align:left;">
hair\_color\_groupother
</td>
<td style="text-align:left;">
-6.87
</td>
<td style="text-align:left;">
(-33.87,20.13)
</td>
<td style="text-align:left;">
0.614
</td>
</tr>
<tr>
<td style="text-align:left;vertical-align: middle !important;" rowspan="7">
mass
</td>
<td style="text-align:left;">
(Intercept)
</td>
<td style="text-align:left;">
53.19
</td>
<td style="text-align:left;">
(26.83,79.54)
</td>
<td style="text-align:left;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
sexhermaphroditic
</td>
<td style="text-align:left;">
1315.04
</td>
<td style="text-align:left;">
(1252.5,1377.58)
</td>
<td style="text-align:left;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
sexmale
</td>
<td style="text-align:left;">
27.82
</td>
<td style="text-align:left;">
(6.52,49.11)
</td>
<td style="text-align:left;">
0.011
</td>
</tr>
<tr>
<td style="text-align:left;">
sexnone
</td>
<td style="text-align:left;">
23.72
</td>
<td style="text-align:left;">
(-13.09,60.52)
</td>
<td style="text-align:left;">
0.202
</td>
</tr>
<tr>
<td style="text-align:left;">
hair\_color\_groupbrown
</td>
<td style="text-align:left;">
5.85
</td>
<td style="text-align:left;">
(-21.68,33.39)
</td>
<td style="text-align:left;">
0.671
</td>
</tr>
<tr>
<td style="text-align:left;">
hair\_color\_groupnone
</td>
<td style="text-align:left;">
2.05
</td>
<td style="text-align:left;">
(-22.48,26.59)
</td>
<td style="text-align:left;">
0.867
</td>
</tr>
<tr>
<td style="text-align:left;">
hair\_color\_groupother
</td>
<td style="text-align:left;">
-10.23
</td>
<td style="text-align:left;">
(-38.17,17.72)
</td>
<td style="text-align:left;">
0.466
</td>
</tr>
</tbody>
</table>
