Repaso
================
Melany 20170474
7/24/2019

El cache = TRUE hace que los chuncks esten conectados, para no cargar las librerias cada vez y echo = TRUE para mostrar lo que esta ahi.
========================================================================================================================================

si no tuviera dbplyr con require solo me sale que no lo tengo y sigue cargando el codigo, con library deja de cargar cuando sale error.
=======================================================================================================================================

Cargar librerias
================

``` r
require(dbplyr)
```

    ## Loading required package: dbplyr

``` r
require(RMySQL)
```

    ## Loading required package: RMySQL

    ## Warning: package 'RMySQL' was built under R version 3.5.2

    ## Loading required package: DBI

``` r
require(lubridate)
```

    ## Loading required package: lubridate

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
require(openxlsx)
```

    ## Loading required package: openxlsx

    ## Warning: package 'openxlsx' was built under R version 3.5.2

``` r
require(tidyverse)
```

    ## Loading required package: tidyverse

    ## ── Attaching packages ──────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   0.8.1     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.3.0

    ## Warning: package 'tibble' was built under R version 3.5.2

    ## Warning: package 'dplyr' was built under R version 3.5.2

    ## Warning: package 'stringr' was built under R version 3.5.2

    ## ── Conflicts ─────────── tidyverse_conflicts() ──
    ## ✖ lubridate::as.difftime() masks base::as.difftime()
    ## ✖ lubridate::date()        masks base::date()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ dplyr::ident()           masks dbplyr::ident()
    ## ✖ lubridate::intersect()   masks base::intersect()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ lubridate::setdiff()     masks base::setdiff()
    ## ✖ dplyr::sql()             masks dbplyr::sql()
    ## ✖ lubridate::union()       masks base::union()

``` r
require(stringr)
require(readr)
```

Tipo de datos
=============

``` r
string <- "This is a string"
class(string)
```

    ## [1] "character"

``` r
nchar(string) #Numero de caracteres
```

    ## [1] 16

``` r
length(string) #Longitud del vector
```

    ## [1] 1

``` r
number <-234L #L sirve para definir que es un numero entero
class(number)
```

    ## [1] "integer"

``` r
typeof(number)
```

    ## [1] "integer"

``` r
logical <- FALSE #vectores logicos sirven para escoger filas de tablas
logical
```

    ## [1] FALSE

``` r
logical*1
```

    ## [1] 0

``` r
factor_1 <- c("Mon", "Tue", "Mon", "Wed", "Thu", "Fri", "Sat", "Sun", "Wed", "Thu")
factor_1 <- factor(factor_1)
factor_1
```

    ##  [1] Mon Tue Mon Wed Thu Fri Sat Sun Wed Thu
    ## Levels: Fri Mon Sat Sun Thu Tue Wed

``` r
as.numeric(factor_1) #label enconding
```

    ##  [1] 2 6 2 7 5 1 3 4 7 5

``` r
factor_2 <- c("Mon", "Tue", "Mon", "Wed", "Thu", "Fri", "Sat", "Sun", "Wed", "Thu")
factor_2 <- ordered(factor_2, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
factor_2
```

    ##  [1] Mon Tue Mon Wed Thu Fri Sat Sun Wed Thu
    ## Levels: Mon < Tue < Wed < Thu < Fri < Sat < Sun

Vectors
=======

``` r
sample (x= 1:100, size = 8, replace =FALSE) # replace = false no repite los numeros
```

    ## [1] 41 94 42 81 51 76 10 84

``` r
sample (x= 1:5, size = 8, replace =TRUE)
```

    ## [1] 4 2 2 4 5 3 5 1

``` r
vector_1 <- c(1:5, "A") #Con el A vuelve los numeros a string, pero el A no puede convertirse a int
vector_1
```

    ## [1] "1" "2" "3" "4" "5" "A"

``` r
c(vector_1, "B", "C", "D")
```

    ## [1] "1" "2" "3" "4" "5" "A" "B" "C" "D"

``` r
as.logical(vector_1)
```

    ## [1] NA NA NA NA NA NA

Listas
======

``` r
vector_1 <- c(1,2,3,4,5)
vector_2 <- c(FALSE,TRUE,FALSE)
list_1 <- list(vector_1, vector_2)
names(list_1) <- c("first", "second")
list_1$second[2]
```

    ## [1] TRUE

Matrices
========

``` r
matrix(data = 1, nrow = 4, ncol = 5)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    1    1    1    1    1
    ## [2,]    1    1    1    1    1
    ## [3,]    1    1    1    1    1
    ## [4,]    1    1    1    1    1

``` r
vector_5 <- c(1:5)
vector_6 <- c(6:10)
vector_7 <- c(11:15)
matrix_1 <- cbind(vector_5, vector_6, vector_7)
matrix_1
```

    ##      vector_5 vector_6 vector_7
    ## [1,]        1        6       11
    ## [2,]        2        7       12
    ## [3,]        3        8       13
    ## [4,]        4        9       14
    ## [5,]        5       10       15

Data Frames
===========

``` r
df <- data.frame(
  col1 = c("This", "is", "a", "vector", "of", "strings"),
  col2 = 1:6,
  col3 = letters[1:6],
  stringsAsFactors = FALSE
)
df
```

    ##      col1 col2 col3
    ## 1    This    1    a
    ## 2      is    2    b
    ## 3       a    3    c
    ## 4  vector    4    d
    ## 5      of    5    e
    ## 6 strings    6    f

``` r
nrow(df)
```

    ## [1] 6

``` r
ncol(df)
```

    ## [1] 3

``` r
str(df)
```

    ## 'data.frame':    6 obs. of  3 variables:
    ##  $ col1: chr  "This" "is" "a" "vector" ...
    ##  $ col2: int  1 2 3 4 5 6
    ##  $ col3: chr  "a" "b" "c" "d" ...

``` r
names(df)
```

    ## [1] "col1" "col2" "col3"

``` r
length(df)
```

    ## [1] 3

``` r
dim(df) #Dimensiones del df
```

    ## [1] 6 3

``` r
head(df)
```

    ##      col1 col2 col3
    ## 1    This    1    a
    ## 2      is    2    b
    ## 3       a    3    c
    ## 4  vector    4    d
    ## 5      of    5    e
    ## 6 strings    6    f

``` r
tail(df)
```

    ##      col1 col2 col3
    ## 1    This    1    a
    ## 2      is    2    b
    ## 3       a    3    c
    ## 4  vector    4    d
    ## 5      of    5    e
    ## 6 strings    6    f

Funciones BaseR
===============

``` r
df$Column4 <- 11:16
new_elements <- c("new_string", 19, "z")
df <- rbind(df, new_elements)
df_copy <- rbind(df, new_elements)
df
```

    ##         col1 col2 col3    Column4
    ## 1       This    1    a         11
    ## 2         is    2    b         12
    ## 3          a    3    c         13
    ## 4     vector    4    d         14
    ## 5         of    5    e         15
    ## 6    strings    6    f         16
    ## 7 new_string   19    z new_string

``` r
find_sample <- function(x){
  for_index <- sample(1:nrow(x), size = 10, replace = FALSE)
  new_df <- x[for_index,]
  return(new_df)
}
data <- data.frame(
  a = 1:10,
  b = sample(c("GT","US","CA"), size = 10, replace = TRUE)
)
find_sample(data)
```

    ##     a  b
    ## 10 10 GT
    ## 6   6 CA
    ## 1   1 GT
    ## 9   9 GT
    ## 7   7 US
    ## 2   2 US
    ## 8   8 US
    ## 5   5 US
    ## 4   4 GT
    ## 3   3 GT

``` r
generate_df <- function(x){
  df<- data.frame(
    a = sample(letters, size = 10, replace = TRUE),
    b = sample (1:10, size = 10, replace = TRUE)
  )
  return(df)
}
generate_df(1)
```

    ##    a  b
    ## 1  s  6
    ## 2  t 10
    ## 3  j  6
    ## 4  a  3
    ## 5  x  3
    ## 6  a  1
    ## 7  p  2
    ## 8  z 10
    ## 9  c  2
    ## 10 o  7

``` r
result_list <- list()
#system.time(for(i in 1:10000){
#result_list[[i]] <- generate_df(1)
#})
system.time(lapply(1:100000, generate_df)) #lapply sirve para seguir aplicando un vector a una lista, devuelve una lista.
```

    ##    user  system elapsed 
    ##  27.855   0.185  28.228
