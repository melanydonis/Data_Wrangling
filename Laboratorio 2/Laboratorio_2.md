Laboratorio 2
================
Melany 20170474
8/7/2019

    ## Loading required package: RMySQL

    ## Warning: package 'RMySQL' was built under R version 3.5.2

    ## Loading required package: DBI

``` r
dbListFields(mydb, "heroes_information")
```

    ##  [1] "id"         "name"       "Gender"     "Eye color"  "Race"      
    ##  [6] "Hair color" "Height"     "Publisher"  "Skin color" "Alignment" 
    ## [11] "Weight"

``` r
#Ejercicio 1
#Use datawrangling (no necesario porque solo hay una base de datos)
#Select field   from table;
Query<- "Select name, Publisher from heroes_information"
hero <- dbGetQuery(mydb, Query)
head(hero)
```

    ##            name         Publisher
    ## 1        A-Bomb     Marvel Comics
    ## 2    Abe Sapien Dark Horse Comics
    ## 3      Abin Sur         DC Comics
    ## 4   Abomination     Marvel Comics
    ## 5       Abraxas     Marvel Comics
    ## 6 Absorbing Man     Marvel Comics

``` r
#Ejercicio 2
#Select distinct field  from table
Query<- "Select distinct Publisher from heroes_information"
hero <- dbGetQuery(mydb, Query)
head(hero)
```

    ##           Publisher
    ## 1     Marvel Comics
    ## 2 Dark Horse Comics
    ## 3         DC Comics
    ## 4      NBC - Heroes
    ## 5         Wildstorm
    ## 6      Image Comics

``` r
#Ejercicio 3
#Select Count(*)  from table    Select count(field) from table    Select count(1) from table
#Select count(distinct field)
Query<- "Select Count(distinct Publisher) from heroes_information"
hero <- dbGetQuery(mydb, Query)
head(hero)
```

    ##   Count(distinct Publisher)
    ## 1                        24

``` r
#Ejercicio 4 y 5
#Select field from table where field = 2;
# <> not equal    in ('v','a','b')
Query1<- "Select * 
        from heroes_information 
        where Height > 200"
hero1 <- dbGetQuery(mydb, Query1)
head(hero1)
```

    ##   id        name Gender Eye color              Race Hair color Height
    ## 1  0      A-Bomb   Male    yellow             Human    No Hair    203
    ## 2  3 Abomination   Male     green Human / Radiation    No Hair    203
    ## 3 17       Alien   Male      NULL   Xenomorph XX121    No Hair    244
    ## 4 19       Amazo   Male       red           Android       NULL    257
    ## 5 29     Ant-Man   Male      blue             Human      Blond    211
    ## 6 33  Anti-Venom   Male      blue          Symbiote      Blond    229
    ##           Publisher Skin color Alignment Weight
    ## 1     Marvel Comics       NULL      good    441
    ## 2     Marvel Comics       NULL       bad    441
    ## 3 Dark Horse Comics      black       bad    169
    ## 4         DC Comics       NULL       bad    173
    ## 5     Marvel Comics       NULL      good    122
    ## 6     Marvel Comics       NULL      NULL    358

``` r
Query2<- "Select * 
        from heroes_information 
        where race = 'Human'"
hero2 <- dbGetQuery(mydb, Query2)
head(hero2)
```

    ##   id              name Gender Eye color  Race Hair color Height
    ## 1  0            A-Bomb   Male    yellow Human    No Hair    203
    ## 2  5     Absorbing Man   Male      blue Human    No Hair    193
    ## 3  7      Adam Strange   Male      blue Human      Blond    185
    ## 4  9         Agent Bob   Male     brown Human      Brown    178
    ## 5 14       Alex Mercer   Male      NULL Human       NULL    -99
    ## 6 16 Alfred Pennyworth   Male      blue Human      Black    178
    ##       Publisher Skin color Alignment Weight
    ## 1 Marvel Comics       NULL      good    441
    ## 2 Marvel Comics       NULL       bad    122
    ## 3     DC Comics       NULL      good     88
    ## 4 Marvel Comics       NULL      good     81
    ## 5     Wildstorm       NULL       bad    -99
    ## 6     DC Comics       NULL      good     72

``` r
#Ejercicio 6 y 7
#Select field from table where field1 = 2  and field2= 3;
#and es varias condiciones en muchas columnas, in es un or en una columna
Query_1<- "Select * 
        from heroes_information 
        where Race = 'Human'
        And Height > 200"
Query_2<- "Select Count(name)
        from heroes_information 
        where Weight > 100
        And Weight < 200"
hero1 <- dbGetQuery(mydb, Query_1)
head(hero1)
```

    ##    id        name Gender Eye color  Race Hair color Height     Publisher
    ## 1   0      A-Bomb   Male    yellow Human    No Hair    203 Marvel Comics
    ## 2  29     Ant-Man   Male      blue Human      Blond    211 Marvel Comics
    ## 3  59        Bane   Male      NULL Human       NULL    203     DC Comics
    ## 4 119    Bloodaxe Female      blue Human      Brown    218 Marvel Comics
    ## 5 221 Doctor Doom   Male     brown Human      Brown    201 Marvel Comics
    ## 6 373  Juggernaut   Male      blue Human        Red    287 Marvel Comics
    ##   Skin color Alignment Weight
    ## 1       NULL      good    441
    ## 2       NULL      good    122
    ## 3       NULL       bad    180
    ## 4       NULL       bad    495
    ## 5       NULL       bad    187
    ## 6       NULL   neutral    855

``` r
hero2 <- dbGetQuery(mydb, Query_2)
head(hero2)
```

    ##   Count(name)
    ## 1          98

``` r
#Ejercicio 8
#Where, and, or
Query<- "Select Count(name)
        from heroes_information 
        where (`Eye color` = 'blue' or `Eye color` = 'red')"
hero <- dbGetQuery(mydb, Query)
head(hero)
```

    ##   Count(name)
    ## 1         271

``` r
#Ejercicio 9
#Between
Query<- "Select Count(name)
        from heroes_information 
        where weight
        between 100 and 200"
hero <- dbGetQuery(mydb, Query)
head(hero)
```

    ##   Count(name)
    ## 1          98

``` r
#Ejercicio 10 
#Select *   from table  Order by field1 desc, fiedl 2
Query1<- "Select name, weight, height
        from heroes_information 
        where weight>200
        and height >100
        order by height desc"
hero1 <- dbGetQuery(mydb, Query1)
head(hero1)
```

    ##             name weight height
    ## 1          MODOK    338    366
    ## 2      Wolfsbane    473    366
    ## 3      Onslaught    405    305
    ## 4      Sasquatch    900    305
    ## 5     Juggernaut    855    287
    ## 6 Solomon Grundy    437    279

``` r
Query2<- "Select name, race
        from heroes_information 
        order by name, race"
hero2 <- dbGetQuery(mydb, Query2)
head(hero2)
```

    ##            name              race
    ## 1        A-Bomb             Human
    ## 2    Abe Sapien     Icthyo Sapien
    ## 3      Abin Sur           Ungaran
    ## 4   Abomination Human / Radiation
    ## 5       Abraxas     Cosmic Entity
    ## 6 Absorbing Man             Human

``` r
#Ejercicio 11
#Group by
Query<- "Select Publisher, Gender, Count(name)
        from heroes_information 
        where Gender = 'Female'
        group by Publisher
        order by count(name) desc"
hero <- dbGetQuery(mydb, Query)
head(hero)
```

    ##           Publisher Gender Count(name)
    ## 1     Marvel Comics Female         111
    ## 2         DC Comics Female          61
    ## 3      NBC - Heroes Female           7
    ## 4 Dark Horse Comics Female           5
    ## 5      Image Comics Female           2
    ## 6              <NA> Female           2

``` r
#Ejercicio 12
#Having (se aplica al resultado despues de hacer un where)
Query<- "Select Publisher, alignment,race, Count(name)
        from heroes_information 
        where alignment = 'good'
        having count(name)>30"
hero <- dbGetQuery(mydb, Query)
head(hero)
```

    ##       Publisher alignment  race Count(name)
    ## 1 Marvel Comics      good Human         496
