Lab 4
================
Melany 20170474
8/28/2019

``` r
library(readr)
library(tidyr)
library(reshape2)
library(dplyr)
library(stringr)
df <- data.frame(row = LETTERS[1:3], a = 1:3, b = 4:6, c = 7:9)
head(df)
```

    ##   row a b c
    ## 1   A 1 4 7
    ## 2   B 2 5 8
    ## 3   C 3 6 9

``` r
#Melt (id como default es la columna con caracteres)
dfm <- melt(df, id = "row")
head(dfm)
```

    ##   row variable value
    ## 1   A        a     1
    ## 2   B        a     2
    ## 3   C        a     3
    ## 4   A        b     4
    ## 5   B        b     5
    ## 6   C        b     6

``` r
#gather (a:c son las que toma como variable)
dfg <- gather(df, key="variable", value = "value", a:c)
head(dfg)
```

    ##   row variable value
    ## 1   A        a     1
    ## 2   B        a     2
    ## 3   C        a     3
    ## 4   A        b     4
    ## 5   B        b     5
    ## 6   C        b     6

``` r
colnames(dfg) <- c("row", "letras", "valores")
```

``` r
load("/Users/melany/Desktop/Data Wrangling/Github/Lab 4/wide_religion.Rda")
religion <- melt(wide_religion, id = "religion")
colnames(religion) <- c("religion","income","cantidad")
head(religion)
```

    ##             religion income cantidad
    ## 1           Agnostic  <$10k       27
    ## 2            Atheist  <$10k       12
    ## 3           Buddhist  <$10k       27
    ## 4           Catholic  <$10k      418
    ## 5 Don’t know/refused  <$10k       15
    ## 6   Evangelical Prot  <$10k      575

``` r
raw <- read_csv('/Users/melany/Desktop/Data Wrangling/data/Archive/Raw.csv')[,2:18]
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   country = col_character(),
    ##   year = col_double(),
    ##   m014 = col_double(),
    ##   m1524 = col_double(),
    ##   m2534 = col_double(),
    ##   m3544 = col_double(),
    ##   m4554 = col_double(),
    ##   m5564 = col_double(),
    ##   m65 = col_double(),
    ##   mu = col_logical(),
    ##   f014 = col_double(),
    ##   f1524 = col_double(),
    ##   f2534 = col_double(),
    ##   f3544 = col_double(),
    ##   f4554 = col_double(),
    ##   f5564 = col_double(),
    ##   f65 = col_double(),
    ##   fu = col_logical()
    ## )

``` r
raw$mu <- NULL
head(raw)
```

    ## # A tibble: 6 x 16
    ##   country  year  m014 m1524 m2534 m3544 m4554 m5564   m65  f014 f1524 f2534
    ##   <chr>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 AD       2000     0     0     1     0     0     0     0    NA    NA    NA
    ## 2 AE       2000     2     4     4     6     5    12    10     3    16     1
    ## 3 AF       2000    52   228   183   149   129    94    80    93   414   565
    ## 4 AG       2000     0     0     0     0     0     0     1     1     1     1
    ## 5 AL       2000     2    19    21    14    24    19    16     3    11    10
    ## 6 AM       2000     2   152   130   131    63    26    21     1    24    27
    ## # … with 4 more variables: f3544 <dbl>, f4554 <dbl>, f5564 <dbl>,
    ## #   f65 <dbl>

``` r
raw <- melt(raw, id = c("country","year"))
```

``` r
new<- separate(raw, variable, c("Gender", "Age"), sep = 1)
```

``` r
library(plyr)
```

    ## -------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

``` r
new$Age <- revalue(new$Age, c("014" = "0-14", "1524" = "15-24", "2534" = "25-34", "3544" = "35-34", "4554" = "45-54", "5564" = "55-64", "65" = "65+"))
colnames(new) <- c("country","year","gender","age","cantidad")
head(new)
```

    ##   country year gender  age cantidad
    ## 1      AD 2000      m 0-14        0
    ## 2      AE 2000      m 0-14        2
    ## 3      AF 2000      m 0-14       52
    ## 4      AG 2000      m 0-14        0
    ## 5      AL 2000      m 0-14        2
    ## 6      AM 2000      m 0-14        2
