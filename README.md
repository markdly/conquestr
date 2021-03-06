
<!-- README.md is generated from README.Rmd. Please edit that file -->
conquestr
---------

ConQuest generalised item response modelling software (Wu, Adams, Wilson, & Haldane, 2007) is used for a wide variety of purposes including the analysis of educational assessment data.

`conquestr` is an R package whose first goal was to make it easier to import ConQuest item analysis files into R. This functionality does not appear to be available in other R packages as far as we are aware.

Other resources
---------------

Several other R packages already integrate with ConQuest to various degrees. For example:

See [TAM](https://CRAN.R-project.org/package=TAM) if you'd like to do analysis similar to ConQuest within R.

The `R2conquest` function in the [sirt](https://CRAN.R-project.org/package=sirt) package provides functionality for using ConQuest from within R.

The [WrightMap](https://CRAN.R-project.org/package=WrightMap) package also contains various features for working with ConQuest and other IRT packages.

Installation
------------

You can install `conquestr` from github with:

``` r
# install.packages("devtools")
devtools::install_github("markdly/conquestr")
```

Alternatively, the latest updates are made and tested using the develop branch which can be installed with

``` r
# install.packages("devtools")
devtools::install_github("markdly/conquestr#develop")
```

Example
-------

First load the relevant packages

``` r
library(conquestr)
```

Take a ConQuest version 2 item analysis file. In ConQuest, item analysis text files are generated by the ConQuest itanal command (e.g. `itanal >> my_item_analysis_file.itn;`). An example item analysis file is included with `conquestr` out of the box:

``` r
cq_example()
#> Example itanal for first n = 10 lines is:
#> 
#>  [1] "\f================================================================================="
#>  [2] "ex1                                                        Tue Nov 21 13:58 2017"   
#>  [3] "GENERALISED ITEM ANALYSIS"                                                          
#>  [4] "================================================================================="  
#>  [5] "Item 1"                                                                             
#>  [6] "------"                                                                             
#>  [7] "item:1 (q1)                                                                     "   
#>  [8] "Cases for this item   1475   Discrimination  0.40"                                  
#>  [9] "Item Threshold(s):    -1.36   Weighted MNSQ   1.25"                                 
#> [10] "Item Delta(s):        -1.36"
```

Use the example file to try out the other functions. The main one is `cq_itanal`:

``` r
# get the path to the example itanal file that comes with conquestr
fname <- cq_example(display = FALSE)

# import the itanal as a tibble
df <- cq_itanal(fname)
```

Once imported, the original text imported for each 'item' can be found in the `data` column.

``` r
df$data[1]
#> [[1]]
#> # A tibble: 17 x 1
#>    value                                                                  
#>    <chr>                                                                  
#>  1 Item 1                                                                 
#>  2 ------                                                                 
#>  3 "item:1 (q1)                                                          ~
#>  4 Cases for this item   1475   Discrimination  0.40                      
#>  5 Item Threshold(s):    -1.36   Weighted MNSQ   1.25                     
#>  6 Item Delta(s):        -1.36                                            
#>  7 ----------------------------------------------------------------------~
#>  8 " Label    Score     Count   % of tot  Pt Bis     t  (p)   PV1Avg:1 PV~
#>  9 ----------------------------------------------------------------------~
#> 10 "   1       0.00       91       6.17   -0.19    -7.32(.000) -1.39     ~
#> 11 "   2       0.00      113       7.66   -0.04    -1.48(.140) -0.23     ~
#> 12 "   3       1.00     1104      74.85    0.40    16.86(.000)  1.20     ~
#> 13 "   4       0.00       41       2.78   -0.04    -1.55(.121) -0.29     ~
#> 14 "   5       0.00       16       1.08   -0.05    -2.10(.036) -0.47     ~
#> 15 "   9       0.00      110       7.46   -0.41   -17.10(.000) -2.74     ~
#> 16 ======================================================================~
#> 17 "                                                                     ~
```

Item level details are provided in separate columns:

``` r
head(dplyr::select(df, -c(data, resp_stat)))
```

| item\_index | id          |  case|  disc|  thrsh|  mnsq|  delta|
|:------------|:------------|-----:|-----:|------:|-----:|------:|
| 1           | item:1 (q1) |  1475|  0.40|  -1.36|  1.25|  -1.36|
| 2           | item:2 (q2) |  1475|  0.54|  -1.95|  0.89|  -1.95|
| 3           | item:3 (q3) |  1475|  0.62|   0.58|  1.01|   0.58|
| 4           | item:4 (q4) |  1475|  0.78|   0.63|  0.67|   0.63|
| 5           | item:5 (q5) |  1475|  0.22|   0.25|  1.84|   0.25|
| 6           | item:6 (q6) |  1475|  0.79|   0.43|  0.67|   0.43|

The details for each response category is contained in the list column `resp_stat`. The function `cq_resp_stats()` makes viewing these easier:

``` r
head(cq_resp_stats(df))
```

| id          | label |  score|  count|  pct\_tot|  pt\_bis|       t|      p|  pv\_avg|  pv\_sd|
|:------------|:------|------:|------:|---------:|--------:|-------:|------:|--------:|-------:|
| item:1 (q1) | 1     |      0|     91|      6.17|    -0.19|   -7.32|  0.000|    -1.39|    1.91|
| item:1 (q1) | 2     |      0|    113|      7.66|    -0.04|   -1.48|  0.140|    -0.23|    1.75|
| item:1 (q1) | 3     |      1|   1104|     74.85|     0.40|   16.86|  0.000|     1.20|    1.91|
| item:1 (q1) | 4     |      0|     41|      2.78|    -0.04|   -1.55|  0.121|    -0.29|    1.35|
| item:1 (q1) | 5     |      0|     16|      1.08|    -0.05|   -2.10|  0.036|    -0.47|    1.95|
| item:1 (q1) | 9     |      0|    110|      7.46|    -0.41|  -17.10|  0.000|    -2.74|    1.26|

Other examples
--------------

### Plausible values files

``` r
fname <- cq_example(display = FALSE, example_name = "ex1_10.pv") 
pv <- cq_pv(fname, np = 10)  
head(pv)
```

| index | val   |  recid| field |
|:------|:------|------:|:------|
| 1     | 10001 |      1| pid   |
| 1     | 3.02  |      1| pv1   |
| 2     | 3.16  |      1| pv2   |
| 3     | 2.43  |      1| pv3   |
| 4     | 5.34  |      1| pv4   |
| 5     | 2.85  |      1| pv5   |

By default the pv files are imported in long format. To convert them to a more typical 'wide' format one way this can be done is:

``` r
library(dplyr)
library(tidyr)
pv_wide <- pv %>% select(recid, field, val) %>% spread(field, val)
head(pv_wide)
```

|  recid| eap      | eap\_se | pid   | pv1   | pv10  | pv2   | pv3   | pv4   | pv5   | pv6   | pv7   | pv8   | pv9   |
|------:|:---------|:--------|:------|:------|:------|:------|:------|:------|:------|:------|:------|:------|:------|
|      1| 3.02232  | 1.16795 | 10001 | 3.02  | 2.23  | 3.16  | 2.43  | 5.34  | 2.85  | 1.62  | 1.66  | 4.22  | 2.70  |
|      2| -2.06897 | 0.92490 | 10002 | -1.39 | -2.77 | -2.38 | -3.05 | -3.56 | -0.87 | -0.41 | -2.11 | -2.67 | -0.51 |
|      3| -3.17088 | 1.15766 | 10003 | -2.34 | -5.86 | -1.35 | -2.47 | -3.97 | -2.39 | -3.11 | -3.54 | -2.64 | -2.40 |
|      4| 1.94699  | 0.91246 | 10004 | 2.11  | 3.32  | 2.55  | 3.90  | 1.84  | 1.12  | 1.18  | 2.26  | 1.43  | 2.69  |
|      5| -0.78847 | 0.72965 | 10005 | -0.86 | -2.02 | 0.20  | -0.03 | -0.78 | -2.58 | -1.46 | -1.47 | -0.86 | -1.82 |
|      6| -1.35811 | 0.78444 | 10006 | -0.87 | -1.46 | -2.49 | -2.42 | -1.37 | -0.20 | -0.71 | -2.15 | -0.58 | -0.84 |

References
----------

Wu, M., Adams, R., Wilson, M., & Haldane, S. (2007). ConQuest Version 2: Generalised Item Response Modelling Software.
