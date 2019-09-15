Laboratorio 6
================
Melany 20170474
9/12/2019

``` r
library(stringr)
```

    ## Warning: package 'stringr' was built under R version 3.5.2

``` r
#1
str_detect(
  string = c("P200BBB", "M140BBB", "1150BBB", "P2345JH", "P123BCDE", "P123ACD", "P123bcd", "P243CNJ", "P214HNS", "P345FVJ", "A344SDF", "P2314ASD", "P245ABC"),
  pattern = "^P([0-9]{3})(([B-D]|[F-H]|[J-N]|[P-T]|[V-Z]){3})$"
)
```

    ##  [1]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE FALSE
    ## [12] FALSE FALSE

``` r
#2
str_detect(
  string = c("Ejemplo1.pdf", "Ejemplo.PDF", "Ejemplo.jpg", "Ejemplo.JPG", "hola.hpg", "respuestas_del_examen.jpg"),
  pattern = "\\.(pdf|PDF|jpg|JPG)$"
)
```

    ## [1]  TRUE  TRUE  TRUE  TRUE FALSE  TRUE

``` r
#3
str_detect(
  string = c("123AbcdE","123A*bcd","12Avbcd","abcabvbcd","12/A*Bcd","Hola123!", "$H123488", "$H123488Nu", "AERFSdnf", "12345678", "asdfghjk", "ASDFGHJK", "adfd!!1L", "Datawrangling2019!"),
  pattern = "^(?=.*(\\!|\\#|\\$|\\%|\\&|\\*|\\+|\\-|\\/|\\:|\\;|\\<|\\>|\\=))(?=.*[A-Z]).{8,20}$"
)
```

    ##  [1] FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
    ## [12] FALSE  TRUE  TRUE

``` r
#4
str_detect(
  string = c("12345678", "19003459", "30001234", "00001234", "31001234", "02008979", "0200979","19002324", "31001564", "14011110", "11008921", "2003421"),
  pattern = "^([0-2][1-9]|10|20|30)(0{2})[1-8][1-9]([1-6][0-9]|70)$"
)
```

    ##  [1] FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE
    ## [12] FALSE

``` r
#5
str_detect(
  string = c("pit", "spot", "spate", "slap two", "respite",  "pt", "Pot", "peat", "part"),
  pattern = "(p.t)"
)
```

    ## [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE

``` r
#6
str_detect(
  string = c("+50254821151", "4210-7640", "52018150", "2434 6854", "11234569", "50211234578", "5021"),
  pattern = "^(\\+)?(502)?(2|4|5|6)(\\d{3})(\\s|-)?(\\d{4})$"
)
```

    ## [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE

``` r
#7
str_detect(
  string = c("melany@ufm.edu", "melany123@ufm.edu", "melany.@ufm.edu", "*melany@ufm.edu"),
  pattern = "^([a-z]|[A-Z]){1,20}@ufm.edu$"
)
```

    ## [1]  TRUE FALSE FALSE FALSE

``` r
#8
str_detect(
  string = c("abc12333ABCDEEEE", "12333ABCDEEEE", "abc12333AB", "abcd12333ABCDEEEE"),
  pattern = "^([a-z]{0,3})([0-9]{2,9})([A-Z]{3,10})$"
)
```

    ## [1]  TRUE  TRUE FALSE FALSE
