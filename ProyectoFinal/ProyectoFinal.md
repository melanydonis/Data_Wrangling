proyecto final
================
Melany 20170474
11/19/2019

    ## ── Attaching packages ───────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   0.8.1     ✔ stringr 1.4.0
    ## ✔ ggplot2 3.0.0     ✔ forcats 0.3.0

    ## Warning: package 'tibble' was built under R version 3.5.2

    ## Warning: package 'dplyr' was built under R version 3.5.2

    ## Warning: package 'stringr' was built under R version 3.5.2

    ## ── Conflicts ──────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    ## Warning: package 'DataExplorer' was built under R version 3.5.2

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

\#lectura

``` r
#funcion para leer xlsx
readDep <- function(file, r){
  df <- read_xlsx(file, range = cell_limits(c(9,2), c(32,NA)))
  colnames(df) <- r
  return(df)
}

readMun <- function(file,r){
  df <- read_xlsx(file, range = cell_limits(c(9,2), c(350,NA)))
  colnames(df) <- r
  return(df)
}
```

\#tidying

``` r
#funcion para unir headers
names <- function(df){
  colnames1 <- colnames(df)
  colnames2 <- as.character(as.vector(df[1,]))
  #colnames(df) <- na.locf(colnames1)
  for (n in 1:ncol(df)) {
    if(is.na(colnames1[n])){
      colnames(df)[n] <- paste0(colnames2[n])
    }
    else{
      colnames(df)[n] <- paste0(colnames1[n],"_",colnames2[n])
    }
  }
  df = df[-1,]
  return(df)
}

#gathering 
gathering <- function(df){
  df <- df %>% gather(key = 'macro', value = 'value', 3:ncol(df))
  df <- df %>% separate(macro, into = c('macro', 'micro'), sep = '_')
  return(df)
}
```

\#2nd
colnames

``` r
carac <- c(NA, NA, 'TotalPob', 'LugarNacimiento', 'LugarNacimiento', 'LugarNacimiento', 'LugarNacimiento',  
           'LugarResidencia',  'LugarResidencia',  'LugarResidencia',  'LugarResidencia',  
           'LugarResidencia',  'PoblaCuatro', 'DificultadVer', 'DificultadVer', 'DificultadVer', 'DificultadOir', 
           'DificultadOir', 'DificultadOir', 'DificultadCaminar', 'DificultadCaminar', 'DificultadCaminar', 
           'DificultadRecordar', 'DificultadRecordar', 'DificultadRecordar', 'DificultadCuidadoPersonal', 
           'DificultadCuidadoPersonal', 'DificultadCuidadoPersonal', 'DificultadComunicarse', 'DificultadComunicarse', 
           'DificultadComunicarse', 'MujeresFert', 'MujeresHijosNacido', 'MujeresHijosNacido', 'MujeresHijosNacido',
           'MujeresHijosNacido', 'MujeresHijosNacido', 'MujeresHijosNacido', 'MujeresHijosNacido',
           'MujeresHijosSobrev', 'MujeresHijosSobrev', 'MujeresHijosSobrev', 'MujeresHijosSobrev', 
           'MujeresHijosSobrev', 'MujeresHijosSobrev')

educ <- c(NA, NA, 'NivelEducacion','NivelEducacion','NivelEducacion','NivelEducacion','NivelEducacion',
          'NivelEducacion','NivelEducacion','NivelEducacion','NivelEducacion', 'CausaInasistencia',
          'CausaInasistencia', 'CausaInasistencia','CausaInasistencia','CausaInasistencia','CausaInasistencia',
          'CausaInasistencia','CausaInasistencia','CausaInasistencia', 'PoblaSiete', 'Lectura','Lectura','Asistencia',
          'Asistencia','LugarEstudio','LugarEstudio','LugarEstudio','LugarEstudio')

emp <- c(NA,NA,'PoblaSiete', 'PoblaAct', 'PoblaOc', 'Cesante', 'Aspirante', 'NoDeclar', 'Uno')

viv <- c(NA,NA,'TotalViv', 'Vivienda', 'Vivienda', 'TipoCasa', 'TipoCasa', 'TipoCasa', 'TipoCasa', 'TipoCasa',
         'TipoCasa', 'TipoOcup', 'TipoOcup', 'TipoOcup', 'TipoOcup', 'TipoOcup', 'Pared', 'Pared', 'Pared',
         'Pared', 'Pared', 'Pared', 'Pared', 'Pared', 'Pared', 'Pared', 'Pared','Techo', 'Techo', 'Techo', 
         'Techo', 'Techo', 'Techo', 'Techo', 'Techo', 'Material','Material', 'Material', 'Material', 
         'Material', 'Material', 'Material', 'Material')

#Solo dep
pue <- c(NA, NA, 'TotalPob', 'Pueblo','Pueblo','Pueblo','Pueblo','Pueblo','Pueblo','Lengua','Lengua','Lengua',
         'Lengua','Lengua','Lengua','Lengua', 'Lengua','Lengua','Lengua','Lengua','Lengua','Lengua',
         'Lengua','Lengua','Lengua','Lengua','Lengua','Lengua','Lengua','Lengua','Lengua','Idioma',
         'Idioma','Idioma','Idioma','Idioma','Idioma','Idioma','Idioma','Idioma','Idioma','Idioma',
         'Idioma','Idioma','Idioma','Idioma','Idioma','Idioma','Idioma','Idioma','Idioma','Idioma',
         'Idioma','Idioma','Idioma','Idioma','Idioma','Idioma','Idioma','Idioma', 'PoblaCuatro')

hog <- c(NA, NA, 'DistHogares', 'DistHogares', 'Lugar', 'Lugar')

tec <- c(NA, NA, 'PoblaSiete', 'Celular', 'Celular', 'Celular', 'Compu', 'Compu', 'Compu', 'Internet', 'Internet',
         'Internet', 'Uso', 'Uso', 'Uso', 'Uso')

pobla <- c(NA ,NA, 'TotalPob', 'Genero', 'Genero', 'Edad', 'Edad', 'Edad', 'Edad', 'Edad', 'GrupoQuinquenal', 
           'GrupoQuinquenal', 'GrupoQuinquenal', 'GrupoQuinquenal', 'GrupoQuinquenal', 'GrupoQuinquenal', 
           'GrupoQuinquenal', 'GrupoQuinquenal', 'GrupoQuinquenal', 'GrupoQuinquenal', 'GrupoQuinquenal', 
           'GrupoQuinquenal', 'GrupoQuinquenal', 'GrupoQuinquenal', 'GrupoQuinquenal', 'GrupoQuinquenal', 
           'GrupoQuinquenal', 'GrupoQuinquenal', 'GrupoQuinquenal', 'GrupoQuinquenal', 'GrupoQuinquenal', 
           'Zona', 'Zona', 'ParentescoJefe', 'ParentescoJefe', 'ParentescoJefe', 'ParentescoJefe', 
           'ParentescoJefe', 'ParentescoJefe', 'ParentescoJefe', 'ParentescoJefe', 'ParentescoJefe', 
           'ParentescoJefe', 'ParentescoJefe', 'PoblaCol', 'PoblaDiez', 'EstadoCivil', 'EstadoCivil', 'EstadoCivil', 
           'EstadoCivil', 'EstadoCivil', 'EstadoCivil')
```

\#datos y
organizacion

``` r
carac_dep <- readDep('Departamento/caracteristicas_departamental.xlsx', carac)
carac_mun <- readMun('Municipio/caracteristicas_municipal.xlsx', carac)
educ_dep <- readDep('Departamento/educacion_departamental.xlsx', educ)
educ_mun <- readMun('Municipio/educacion_municipal.xlsx', educ)
emp_dep <- readDep('Departamento/empleo_departamental.xlsx', emp)
emp_mun <- readMun('Municipio/empleo_municipal.xlsx', emp)
viv_dep <- readDep('Departamento/vivienda_departamental.xlsx', viv)
viv_mun <- readMun('Municipio/vivienda_municipal.xlsx', viv)
pue_dep <- readDep('Departamento/pueblo_departamental.xlsx', pue)
hog_dep <- readDep('Departamento/hogares_departamental.xlsx', hog)
hog_mun <- readMun('Municipio/hogares_municipal.xlsx', hog)
tec_dep <- readDep('Departamento/tecnologia_departamental.xlsx', tec)
tec_mun <- readMun('Municipio/tecnologia_municipal.xlsx', tec)
pobla_dep <- readDep('Departamento/poblacion_departamental.xlsx', pobla)
pobla_mun <- readMun('Municipio/poblacion_municipal.xlsx', pobla)

carac_dep <- names(carac_dep)
carac_mun <- names(carac_mun)
educ_dep <- names(educ_dep)
educ_mun <- names(educ_mun)
emp_dep <- names(emp_dep)
emp_mun <- names(emp_mun)
viv_dep <- names(viv_dep)
viv_mun <- names(viv_mun)
pue_dep <- names(pue_dep)
hog_dep <- names(hog_dep)
hog_mun <- names(hog_mun)
tec_dep <- names(tec_dep)
tec_mun <- names(tec_mun)
pobla_dep <- names(pobla_dep)
pobla_mun <- names(pobla_mun)

carac_dep <- gathering(carac_dep)
carac_mun <- gathering(carac_mun)
educ_dep <- gathering(educ_dep)
educ_mun <- gathering(educ_mun)
emp_dep <- gathering(emp_dep)
emp_mun <- gathering(emp_mun)
viv_dep <- gathering(viv_dep)
viv_mun <- gathering(viv_mun)
pue_dep <- gathering(pue_dep)
hog_dep <- gathering(hog_dep)
hog_mun <- gathering(hog_mun)
tec_dep <- gathering(tec_dep)
tec_mun <- gathering(tec_mun)
pobla_dep <- gathering(pobla_dep)
pobla_mun <- gathering(pobla_mun)
```

# DEPARTAMENTOS

\#missing
values

``` r
departamentos <- rbind(carac_dep, educ_dep, viv_dep, pue_dep, hog_dep, tec_dep, pobla_dep,  emp_dep)
municipios <- rbind(carac_mun, educ_mun, viv_mun, hog_mun, tec_mun, pobla_mun,  emp_mun)
departamentos <- departamentos %>% distinct()
departamentos$value <- as.double(departamentos$value)
departamentos$PerTot <- 1:nrow(departamentos)
plot_missing(departamentos)
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

\#lista con departamentos y porcentajes

``` r
Total <- departamentos %>% filter(micro == 'Total de personas')
Total
```

    ## # A tibble: 22 x 6
    ##    Código Departamento   macro    micro               value PerTot
    ##    <chr>  <chr>          <chr>    <chr>               <dbl>  <int>
    ##  1 1      Guatemala      TotalPob Total de personas 3015081      1
    ##  2 2      El Progreso    TotalPob Total de personas  176632      2
    ##  3 3      Sacatepéquez   TotalPob Total de personas  330469      3
    ##  4 4      Chimaltenango  TotalPob Total de personas  615776      4
    ##  5 5      Escuintla      TotalPob Total de personas  733181      5
    ##  6 6      Santa Rosa     TotalPob Total de personas  396607      6
    ##  7 7      Sololá         TotalPob Total de personas  421583      7
    ##  8 8      Totonicapán    TotalPob Total de personas  418569      8
    ##  9 9      Quetzaltenango TotalPob Total de personas  799101      9
    ## 10 10     Suchitepéquez  TotalPob Total de personas  554695     10
    ## # … with 12 more rows

``` r
for(i in 1:nrow(departamentos)){
  departamentos$PerTot[i] <- (100*departamentos$value[i])/Total$value[which(Total$Departamento == departamentos$Departamento[i])]
  }

flw <- vector("list", length(unique(departamentos$macro)))
for(i in unique(departamentos$macro)){
  flw[[i]] <- departamentos %>% filter(macro == i)
}
```

\#funciones para graficas

``` r
histogram <- function(df, data){
  ggplot(df, aes(x = micro, y = value, fill = Departamento)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    ggtitle(data)
}


histogram2 <- function(df, data){
  ggplot(df, aes(x = micro, y = PerTot, fill = Departamento)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    ggtitle(data)
}


boxplot <- function(df, data){
  ggplot(df, aes(x = micro, y = value)) +
    geom_boxplot() +
    ggtitle(data)
}
boxplot2 <- function(df, data){
  ggplot(df, aes(x = micro, y = PerTot)) +
    geom_boxplot() +
    ggtitle(data)
}

boxplot3 <- function(df, data){
  ggplot(df, aes(x = micro, y = new_PerTot)) +
    geom_boxplot() +
    ggtitle(data)
}
```

\#histogramas Departamento

``` r
ggplot(Total, aes(x = Departamento, y = value))+
  geom_bar(stat = 'identity', position = 'dodge') +
  ggtitle('TotalPersonas')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
histogram(flw$LugarNacimiento, 'LugarNacimiento')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
histogram(flw$LugarResidencia, 'LugarResidencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
histogram(flw$PoblaCuatro, 'PoblaCuatro')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

``` r
histogram(flw$DificultadVer, 'DificultadVer')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-5.png)<!-- -->

``` r
histogram(flw$DificultadOir, 'DificultadOir')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-6.png)<!-- -->

``` r
histogram(flw$DificultadCaminar, 'DificultadCaminar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-7.png)<!-- -->

``` r
histogram(flw$DificultadRecordar, 'DificultadRecordar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-8.png)<!-- -->

``` r
histogram(flw$DificultadCuidadoPersonal, 'DificultadCuidadoPersonal')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-9.png)<!-- -->

``` r
histogram(flw$DificultadComunicarse, 'DificultadComunicarse')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-10.png)<!-- -->

``` r
histogram(flw$MujeresFert, 'MujeresFert')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-11.png)<!-- -->

``` r
histogram(flw$MujeresHijosNacido, 'MujeresHijosNacido')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-12.png)<!-- -->

``` r
histogram(flw$MujeresHijosSobrev, 'MujeresHijosSobrev')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-13.png)<!-- -->

``` r
histogram(flw$NivelEducacion, 'NivelEducacion')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-14.png)<!-- -->

``` r
histogram(flw$CausaInasistencia, 'CausaInasistencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-15.png)<!-- -->

``` r
histogram(flw$PoblaSiete, 'PoblaSiete')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-16.png)<!-- -->

``` r
histogram(flw$Lectura, 'Lectura')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-17.png)<!-- -->

``` r
histogram(flw$Asistencia, 'Asistencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-18.png)<!-- -->

``` r
histogram(flw$LugarEstudio, 'LugarEstudio')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-19.png)<!-- -->

``` r
histogram(flw$TotalViv, 'TotalViv')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-20.png)<!-- -->

``` r
histogram(flw$Vivienda, 'Vivienda')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-21.png)<!-- -->

``` r
histogram(flw$TipoCasa, 'TipoCasa')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-22.png)<!-- -->

``` r
histogram(flw$TipoOcup, 'TipoOcup')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-23.png)<!-- -->

``` r
histogram(flw$Pared, 'Pared')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-24.png)<!-- -->

``` r
histogram(flw$Techo, 'Techo')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-25.png)<!-- -->

``` r
histogram(flw$Material, 'Material')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-26.png)<!-- -->

``` r
histogram(flw$Pueblo, 'Pueblo')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-27.png)<!-- -->

``` r
histogram(flw$Lengua, 'Lengua')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-28.png)<!-- -->

``` r
histogram(flw$Idioma, 'Idioma')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-29.png)<!-- -->

``` r
histogram(flw$DistHogares, 'DistHogares')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-30.png)<!-- -->

``` r
histogram(flw$Lugar, 'Lugar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-31.png)<!-- -->

``` r
histogram(flw$Celular, 'Celular')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-32.png)<!-- -->

``` r
histogram(flw$Compu, 'Compu')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-33.png)<!-- -->

``` r
histogram(flw$Internet, 'Internet')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-34.png)<!-- -->

``` r
histogram(flw$Uso, 'Uso')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-35.png)<!-- -->

``` r
histogram(flw$Genero, 'Genero')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-36.png)<!-- -->

``` r
histogram(flw$Edad, 'Edad')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-37.png)<!-- -->

``` r
histogram(flw$GrupoQuinquenal, 'GrupoQuinquenal')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-38.png)<!-- -->

``` r
histogram(flw$Zona, 'Zona')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-39.png)<!-- -->

``` r
histogram(flw$ParentescoJefe, 'ParentescoJefe')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-40.png)<!-- -->

``` r
histogram(flw$PoblaCol, 'PoblaCol')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-41.png)<!-- -->

``` r
histogram(flw$PoblaDiez, 'PoblaDiez')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-42.png)<!-- -->

``` r
histogram(flw$EstadoCivil, 'EstadoCivil')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-43.png)<!-- -->

``` r
histogram(flw$PoblaAct,'PoblaAct')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-44.png)<!-- -->

``` r
histogram(flw$PoblaOc,'PoblaOc')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-45.png)<!-- -->

``` r
histogram(flw$Cesante, 'Cesante')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-46.png)<!-- -->

``` r
histogram(flw$Aspirante, 'Aspirante')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-47.png)<!-- -->

``` r
histogram(flw$NoDeclar, 'NoDeclar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-48.png)<!-- -->

``` r
histogram(flw$Uno, 'Uno')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-9-49.png)<!-- -->

\#piechart genero

``` r
ggplot(flw$Genero, aes(x = '' , y = PerTot, fill = micro)) +
  geom_bar(stat = 'identity') + coord_polar(('y')) + scale_fill_manual(values=c('#0039bf', "#672cc7"))  +
  theme(axis.text.x=element_blank())
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

\#Histogramas Departamentos Porcentajes

``` r
histogram2(flw$LugarNacimiento, 'LugarNacimiento')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
histogram2(flw$LugarResidencia, 'LugarResidencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
histogram2(flw$PoblaCuatro, 'PoblaCuatro')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
histogram2(flw$DificultadVer, 'DificultadVer')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

``` r
histogram2(flw$DificultadOir, 'DificultadOir')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-5.png)<!-- -->

``` r
histogram2(flw$DificultadCaminar, 'DificultadCaminar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-6.png)<!-- -->

``` r
histogram2(flw$DificultadRecordar, 'DificultadRecordar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-7.png)<!-- -->

``` r
histogram2(flw$DificultadCuidadoPersonal, 'DificultadCuidadoPersonal')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-8.png)<!-- -->

``` r
histogram2(flw$DificultadComunicarse, 'DificultadComunicarse')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-9.png)<!-- -->

``` r
histogram2(flw$MujeresFert, 'MujeresFert')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-10.png)<!-- -->

``` r
histogram2(flw$MujeresHijosNacido, 'MujeresHijosNacido')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-11.png)<!-- -->

``` r
histogram2(flw$MujeresHijosSobrev, 'MujeresHijosSobrev')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-12.png)<!-- -->

``` r
histogram2(flw$NivelEducacion, 'NivelEducacion')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-13.png)<!-- -->

``` r
histogram2(flw$CausaInasistencia, 'CausaInasistencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-14.png)<!-- -->

``` r
histogram2(flw$PoblaSiete, 'PoblaSiete')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-15.png)<!-- -->

``` r
histogram2(flw$Lectura, 'Lectura')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-16.png)<!-- -->

``` r
histogram2(flw$Asistencia, 'Asistencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-17.png)<!-- -->

``` r
histogram2(flw$LugarEstudio, 'LugarEstudio')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-18.png)<!-- -->

``` r
histogram2(flw$TotalViv, 'TotalViv')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-19.png)<!-- -->

``` r
histogram2(flw$Vivienda, 'Vivienda')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-20.png)<!-- -->

``` r
histogram2(flw$TipoCasa, 'TipoCasa')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-21.png)<!-- -->

``` r
histogram2(flw$TipoOcup, 'TipoOcup')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-22.png)<!-- -->

``` r
histogram2(flw$Pared, 'Pared')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-23.png)<!-- -->

``` r
histogram2(flw$Techo, 'Techo')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-24.png)<!-- -->

``` r
histogram2(flw$Material, 'Material')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-25.png)<!-- -->

``` r
histogram2(flw$Pueblo, 'Pueblo')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-26.png)<!-- -->

``` r
histogram2(flw$Lengua, 'Lengua')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-27.png)<!-- -->

``` r
histogram2(flw$Idioma, 'Idioma')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-28.png)<!-- -->

``` r
histogram2(flw$DistHogares, 'DistHogares')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-29.png)<!-- -->

``` r
histogram2(flw$Lugar, 'Lugar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-30.png)<!-- -->

``` r
histogram2(flw$Celular, 'Celular')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-31.png)<!-- -->

``` r
histogram2(flw$Compu, 'Compu')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-32.png)<!-- -->

``` r
histogram2(flw$Internet, 'Internet')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-33.png)<!-- -->

``` r
histogram2(flw$Uso, 'Uso')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-34.png)<!-- -->

``` r
histogram2(flw$Genero, 'Genero')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-35.png)<!-- -->

``` r
histogram2(flw$Edad, 'Edad')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-36.png)<!-- -->

``` r
histogram2(flw$GrupoQuinquenal, 'GrupoQuinquenal')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-37.png)<!-- -->

``` r
histogram2(flw$Zona, 'Zona')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-38.png)<!-- -->

``` r
histogram2(flw$ParentescoJefe, 'ParentescoJefe')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-39.png)<!-- -->

``` r
histogram2(flw$PoblaCol, 'PoblaCol')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-40.png)<!-- -->

``` r
histogram2(flw$PoblaDiez, 'PoblaDiez')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-41.png)<!-- -->

``` r
histogram2(flw$EstadoCivil, 'EstadoCivil')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-42.png)<!-- -->

``` r
histogram2(flw$PoblaAct,'PoblaAct')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-43.png)<!-- -->

``` r
histogram2(flw$PoblaOc,'PoblaOc')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-44.png)<!-- -->

``` r
histogram2(flw$Cesante, 'Cesante')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-45.png)<!-- -->

``` r
histogram2(flw$Aspirante, 'Aspirante')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-46.png)<!-- -->

``` r
histogram2(flw$NoDeclar, 'NoDeclar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-47.png)<!-- -->

``` r
histogram2(flw$Uno, 'Uno')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-11-48.png)<!-- -->

\#boxplots Departamentos

``` r
boxplot(flw$LugarNacimiento, 'LugarNacimiento')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
boxplot(flw$LugarResidencia, 'LugarResidencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
boxplot(flw$PoblaCuatro, 'PoblaCuatro')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
boxplot(flw$DificultadVer, 'DificultadVer')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

``` r
boxplot(flw$DificultadOir, 'DificultadOir')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-5.png)<!-- -->

``` r
boxplot(flw$DificultadCaminar, 'DificultadCaminar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-6.png)<!-- -->

``` r
boxplot(flw$DificultadRecordar, 'DificultadRecordar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-7.png)<!-- -->

``` r
boxplot(flw$DificultadCuidadoPersonal, 'DificultadCuidadoPersonal')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-8.png)<!-- -->

``` r
boxplot(flw$DificultadComunicarse, 'DificultadComunicarse')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-9.png)<!-- -->

``` r
boxplot(flw$MujeresFert, 'MujeresFert')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-10.png)<!-- -->

``` r
boxplot(flw$MujeresHijosNacido, 'MujeresHijosNacido')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-11.png)<!-- -->

``` r
boxplot(flw$MujeresHijosSobrev, 'MujeresHijosSobrev')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-12.png)<!-- -->

``` r
boxplot(flw$NivelEducacion, 'NivelEducacion')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-13.png)<!-- -->

``` r
boxplot(flw$CausaInasistencia, 'CausaInasistencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-14.png)<!-- -->

``` r
boxplot(flw$PoblaSiete, 'PoblaSiete')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-15.png)<!-- -->

``` r
boxplot(flw$Lectura, 'Lectura')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-16.png)<!-- -->

``` r
boxplot(flw$Asistencia, 'Asistencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-17.png)<!-- -->

``` r
boxplot(flw$LugarEstudio, 'LugarEstudio')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-18.png)<!-- -->

``` r
boxplot(flw$TotalViv, 'TotalViv')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-19.png)<!-- -->

``` r
boxplot(flw$Vivienda, 'Vivienda')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-20.png)<!-- -->

``` r
boxplot(flw$TipoCasa, 'TipoCasa')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-21.png)<!-- -->

``` r
boxplot(flw$TipoOcup, 'TipoOcup')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-22.png)<!-- -->

``` r
boxplot(flw$Pared, 'Pared')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-23.png)<!-- -->

``` r
boxplot(flw$Techo, 'Techo')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-24.png)<!-- -->

``` r
boxplot(flw$Material, 'Material')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-25.png)<!-- -->

``` r
boxplot(flw$Pueblo, 'Pueblo')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-26.png)<!-- -->

``` r
boxplot(flw$Lengua, 'Lengua')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-27.png)<!-- -->

``` r
boxplot(flw$Idioma, 'Idioma')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-28.png)<!-- -->

``` r
boxplot(flw$DistHogares, 'DistHogares')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-29.png)<!-- -->

``` r
boxplot(flw$Lugar, 'Lugar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-30.png)<!-- -->

``` r
boxplot(flw$Celular, 'Celular')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-31.png)<!-- -->

``` r
boxplot(flw$Compu, 'Compu')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-32.png)<!-- -->

``` r
boxplot(flw$Internet, 'Internet')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-33.png)<!-- -->

``` r
boxplot(flw$Uso, 'Uso')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-34.png)<!-- -->

``` r
boxplot(flw$Genero, 'Genero')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-35.png)<!-- -->

``` r
boxplot(flw$Edad, 'Edad')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-36.png)<!-- -->

``` r
boxplot(flw$GrupoQuinquenal, 'GrupoQuinquenal')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-37.png)<!-- -->

``` r
boxplot(flw$Zona, 'Zona')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-38.png)<!-- -->

``` r
boxplot(flw$ParentescoJefe, 'ParentescoJefe')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-39.png)<!-- -->

``` r
boxplot(flw$PoblaCol, 'PoblaCol')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-40.png)<!-- -->

``` r
boxplot(flw$PoblaDiez, 'PoblaDiez')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-41.png)<!-- -->

``` r
boxplot(flw$EstadoCivil, 'EstadoCivil')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-42.png)<!-- -->

``` r
boxplot(flw$PoblaAct,'PoblaAct')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-43.png)<!-- -->

``` r
boxplot(flw$PoblaOc,'PoblaOc')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-44.png)<!-- -->

``` r
boxplot(flw$Cesante, 'Cesante')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-45.png)<!-- -->

``` r
boxplot(flw$Aspirante, 'Aspirante')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-46.png)<!-- -->

``` r
boxplot(flw$NoDeclar, 'NoDeclar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-47.png)<!-- -->

``` r
boxplot(flw$Uno, 'Uno')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-12-48.png)<!-- -->

\#Boxlots Departamentos
Porcentajes

``` r
boxplot2(flw$LugarNacimiento, 'LugarNacimiento%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
boxplot2(flw$LugarResidencia, 'LugarResidencia%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
boxplot2(flw$PoblaCuatro, 'PoblaCuatro%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->

``` r
boxplot2(flw$DificultadVer, 'DificultadVer%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

``` r
boxplot2(flw$DificultadOir, 'DificultadOir%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-5.png)<!-- -->

``` r
boxplot2(flw$DificultadCaminar, 'DificultadCaminar%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-6.png)<!-- -->

``` r
boxplot2(flw$DificultadRecordar, 'DificultadRecordar%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-7.png)<!-- -->

``` r
boxplot2(flw$DificultadCuidadoPersonal, 'DificultadCuidadoPersonal%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-8.png)<!-- -->

``` r
boxplot2(flw$DificultadComunicarse, 'DificultadComunicarse%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-9.png)<!-- -->

``` r
boxplot2(flw$MujeresFert, 'MujeresFert%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-10.png)<!-- -->

``` r
boxplot2(flw$MujeresHijosNacido, 'MujeresHijosNacido%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-11.png)<!-- -->

``` r
boxplot2(flw$MujeresHijosSobrev, 'MujeresHijosSobrev%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-12.png)<!-- -->

``` r
boxplot2(flw$NivelEducacion, 'NivelEducacion%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-13.png)<!-- -->

``` r
boxplot2(flw$CausaInasistencia, 'CausaInasistencia%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-14.png)<!-- -->

``` r
boxplot2(flw$PoblaSiete, 'PoblaSiete%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-15.png)<!-- -->

``` r
boxplot2(flw$Lectura, 'Lectura%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-16.png)<!-- -->

``` r
boxplot2(flw$Asistencia, 'Asistencia%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-17.png)<!-- -->

``` r
boxplot2(flw$LugarEstudio, 'LugarEstudio%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-18.png)<!-- -->

``` r
boxplot2(flw$TotalViv, 'TotalViv%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-19.png)<!-- -->

``` r
boxplot2(flw$Vivienda, 'Vivienda%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-20.png)<!-- -->

``` r
boxplot2(flw$TipoCasa, 'TipoCasa%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-21.png)<!-- -->

``` r
boxplot2(flw$TipoOcup, 'TipoOcup%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-22.png)<!-- -->

``` r
boxplot2(flw$Pared, 'Pared%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-23.png)<!-- -->

``` r
boxplot2(flw$Techo, 'Techo%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-24.png)<!-- -->

``` r
boxplot2(flw$Material, 'Material%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-25.png)<!-- -->

``` r
boxplot2(flw$Pueblo, 'Pueblo%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-26.png)<!-- -->

``` r
boxplot2(flw$Lengua, 'Lengua%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-27.png)<!-- -->

``` r
boxplot2(flw$Idioma, 'Idioma%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-28.png)<!-- -->

``` r
boxplot2(flw$DistHogares, 'DistHogares%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-29.png)<!-- -->

``` r
boxplot2(flw$Lugar, 'Lugar%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-30.png)<!-- -->

``` r
boxplot2(flw$Celular, 'Celular%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-31.png)<!-- -->

``` r
boxplot2(flw$Compu, 'Compu%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-32.png)<!-- -->

``` r
boxplot2(flw$Internet, 'Internet%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-33.png)<!-- -->

``` r
boxplot2(flw$Uso, 'Uso%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-34.png)<!-- -->

``` r
boxplot2(flw$Genero, 'Genero%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-35.png)<!-- -->

``` r
boxplot2(flw$Edad, 'Edad%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-36.png)<!-- -->

``` r
boxplot2(flw$GrupoQuinquenal, 'GrupoQuinquenal%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-37.png)<!-- -->

``` r
boxplot2(flw$Zona, 'Zona%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-38.png)<!-- -->

``` r
boxplot2(flw$ParentescoJefe, 'ParentescoJefe%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-39.png)<!-- -->

``` r
boxplot2(flw$PoblaCol, 'PoblaCol%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-40.png)<!-- -->

``` r
boxplot2(flw$PoblaDiez, 'PoblaDiez%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-41.png)<!-- -->

``` r
boxplot2(flw$EstadoCivil, 'EstadoCivil%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-42.png)<!-- -->

``` r
boxplot2(flw$PoblaAct,'PoblaAct%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-43.png)<!-- -->

``` r
boxplot2(flw$PoblaOc,'PoblaOc%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-44.png)<!-- -->

``` r
boxplot2(flw$Cesante, 'Cesante%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-45.png)<!-- -->

``` r
boxplot2(flw$Aspirante, 'Aspirante%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-46.png)<!-- -->

``` r
boxplot2(flw$NoDeclar, 'NoDeclar%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-47.png)<!-- -->

``` r
boxplot2(flw$Uno, 'Uno%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-13-48.png)<!-- -->

\#Correlacion

``` r
a<- departamentos %>% filter(macro == 'NivelEducacion') %>% filter(micro == 'Preprimaria')%>% select(value)
colnames(a) <- "EducacionPreprimaria"
f<- departamentos %>% filter(macro == 'NivelEducacion') %>% filter(micro == 'Primaria, 6')%>% select(value)
colnames(f) <- "EducacionPrimaria"
g<- departamentos %>% filter(macro == 'NivelEducacion') %>% filter(micro == 'Basico')%>% select(value)
colnames(g) <- "EducacionBasico"
h<- departamentos %>% filter(macro == 'NivelEducacion') %>% filter(micro == 'Diversificado')%>% select(value)
colnames(h) <- "EducacionDiversificado"
i<- departamentos %>% filter(macro == 'NivelEducacion') %>% filter(micro == 'Licenciatura')%>% select(value)
colnames(i) <- "EducacionLicenciatura"
j<- departamentos %>% filter(macro == 'NivelEducacion') %>% filter(micro == 'Maestria y doctorado')%>% select(value)
colnames(j) <- "EducacionMaestriaDoctorado"
k<- departamentos %>% filter(macro == 'CausaInasistencia') %>% filter(micro == 'Falta de dinero')%>% select(value)
colnames(k) <- "InasistenciaFaltaDinero"
l<- departamentos %>% filter(macro == 'CausaInasistencia') %>% filter(micro == 'Tiene que trabajar')%>% select(value)
colnames(l) <- "InasistenciaTrabajo"
m<- departamentos %>% filter(macro == 'CausaInasistencia') %>% filter(micro == 'No hay escuela, instituto o universidad')%>% select(value)
colnames(m) <- "InasistenciaNoEscuela"
n<- departamentos %>% filter(macro == 'Lectura') %>% filter(micro == 'Alfabeta')%>% select(value)
colnames(n) <- "Alfabeta"
o<- departamentos %>% filter(macro == 'Lectura') %>% filter(micro == 'Analfabeta')%>% select(value)
colnames(o) <- "Analfabeta"
p<- departamentos %>% filter(macro == 'Idioma') %>% filter(micro == 'Español')%>% select(value)
colnames(p) <- "IdiomaEspañol"
q<- departamentos %>% filter(macro == 'Idioma') %>% filter(micro == 'Kaqchiquel')%>% select(value)
colnames(q) <- "IdiomaKaqchiquel"
r<- departamentos %>% filter(macro == 'Genero') %>% filter(micro == 'Hombre')%>% select(value)
colnames(r) <- "Hombre"
s<- departamentos %>% filter(macro == 'Genero') %>% filter(micro == 'Mujer')%>% select(value)
colnames(s) <- "Mujer"
t<- departamentos %>% filter(macro == 'Zona') %>% filter(micro == 'Urbano')%>% select(value)
colnames(t) <- "Urbano"
u<- departamentos %>% filter(macro == 'Zona') %>% filter(micro == 'Rural')%>% select(value)
colnames(u) <- "Rural"
v<- departamentos %>% filter(macro == 'EstadoCivil') %>% filter(micro == 'Soltera(o)')%>% select(value)
colnames(v) <- "Soltero"
w<- departamentos %>% filter(macro == 'EstadoCivil') %>% filter(micro == 'Casada(o)')%>% select(value)
colnames(w) <- "Casado"
x<- departamentos %>% filter(macro == 'PoblaAct') %>% filter(micro == 'Población Económicamente Activa')%>% select(value)
colnames(x) <- "PoblacionActiva"
y<- departamentos %>% filter(macro == 'Cesante') %>% filter(micro == 'Cesante')%>% select(value)
colnames(y) <- "PoblacionCesante"

dep <- cbind(a,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y)
dep <- dep[, !duplicated(colnames(dep))]
plot_correlation(dep)
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

\#total de
personas

``` r
sum(departamentos$value[which(departamentos$micro == 'Total de personas')])
```

    ## [1] 14901286

\#Lista Municipios con porcentaje y Funcion
histograma

``` r
guate <- municipios %>% filter(Código != '1202') %>% filter(Municipio %in% c('Villa Nueva','Guatemala','Amatitlán','Chinautla','Chuarrancho','Fraijanes','Mixco','Palencia',
                                              'San José del Golfo','San José Pinula','San Juan Sacatepéquez','San Miguel Petapa',
                                              'San Pedro Ayampuc','San Pedro Sacatepéquez','San Raymundo','Santa Catarina Pinula',
                                              'Villa Canales')) 
guate$value <- as.numeric(guate$value)
Total <- guate %>% filter(micro == 'Total de personas')
for(i in 1:nrow(guate)){
  guate$PerTot[i] <- 100*guate$value[i]/Total$value[which(Total$Municipio == guate$Municipio[i])]
}
```

    ## Warning: Unknown or uninitialised column: 'PerTot'.

    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length
    
    ## Warning in guate$PerTot[i] <- 100 * guate$value[i]/
    ## Total$value[which(Total$Municipio == : number of items to replace is not a
    ## multiple of replacement length

``` r
fl <- vector("list", length(unique(guate$macro)))
for(i in unique(guate$macro)){
  fl[[i]] <- guate %>% filter(macro == i)
}

histogram <- function(df, data){
  ggplot(df, aes(x = micro, y = value, fill = Municipio)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    ggtitle(data)
}
histogram2 <- function(df, data){
  ggplot(df, aes(x = micro, y = PerTot, fill = Municipio)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    ggtitle(data)
}
```

\#Histogramas Municipalidades

``` r
histogram(fl$LugarNacimiento, 'LugarNacimiento')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
histogram(fl$LugarResidencia, 'LugarResidencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
histogram(fl$PoblaCuatro, 'PoblaCuatro')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->

``` r
histogram(fl$DificultadVer, 'DificultadVer')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-4.png)<!-- -->

``` r
histogram(fl$DificultadOir, 'DificultadOir')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-5.png)<!-- -->

``` r
histogram(fl$DificultadCaminar, 'DificultadCaminar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-6.png)<!-- -->

``` r
histogram(fl$DificultadRecordar, 'DificultadRecordar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-7.png)<!-- -->

``` r
histogram(fl$DificultadCuidadoPersonal, 'DificultadCuidadoPersonal')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-8.png)<!-- -->

``` r
histogram(fl$DificultadComunicarse, 'DificultadComunicarse')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-9.png)<!-- -->

``` r
histogram(fl$MujeresFert, 'MujeresFert')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-10.png)<!-- -->

``` r
histogram(fl$MujeresHijosNacido, 'MujeresHijosNacido')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-11.png)<!-- -->

``` r
histogram(fl$MujeresHijosSobrev, 'MujeresHijosSobrev')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-12.png)<!-- -->

``` r
histogram(fl$NivelEducacion, 'NivelEducacion')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-13.png)<!-- -->

``` r
histogram(fl$CausaInasistencia, 'CausaInasistencia')+coord_flip()
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-14.png)<!-- -->

``` r
histogram(fl$PoblaSiete, 'PoblaSiete')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-15.png)<!-- -->

``` r
histogram(fl$Lectura, 'Lectura')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-16.png)<!-- -->

``` r
histogram(fl$Asistencia, 'Asistencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-17.png)<!-- -->

``` r
histogram(fl$LugarEstudio, 'LugarEstudio')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-18.png)<!-- -->

``` r
histogram(fl$TotalViv, 'TotalViv')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-19.png)<!-- -->

``` r
histogram(fl$Vivienda, 'Vivienda')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-20.png)<!-- -->

``` r
histogram(fl$TipoCasa, 'TipoCasa')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-21.png)<!-- -->

``` r
histogram(fl$TipoOcup, 'TipoOcup')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-22.png)<!-- -->

``` r
histogram(fl$Pared, 'Pared')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-23.png)<!-- -->

``` r
histogram(fl$Techo, 'Techo')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-24.png)<!-- -->

``` r
histogram(fl$Material, 'Material')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-25.png)<!-- -->

``` r
histogram(fl$DistHogares, 'DistHogares')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-26.png)<!-- -->

``` r
histogram(fl$Lugar, 'Lugar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-27.png)<!-- -->

``` r
histogram(fl$Celular, 'Celular')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-28.png)<!-- -->

``` r
histogram(fl$Compu, 'Compu')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-29.png)<!-- -->

``` r
histogram(fl$Internet, 'Internet')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-30.png)<!-- -->

``` r
histogram(fl$Uso, 'Uso')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-31.png)<!-- -->

``` r
histogram(fl$Genero, 'Genero')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-32.png)<!-- -->

``` r
histogram(fl$Edad, 'Edad')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-33.png)<!-- -->

``` r
histogram(fl$GrupoQuinquenal, 'GrupoQuinquenal')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-34.png)<!-- -->

``` r
histogram(fl$Zona, 'Zona')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-35.png)<!-- -->

``` r
histogram(fl$ParentescoJefe, 'ParentescoJefe')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-36.png)<!-- -->

``` r
histogram(fl$PoblaCol, 'PoblaCol')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-37.png)<!-- -->

``` r
histogram(fl$PoblaDiez, 'PoblaDiez')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-38.png)<!-- -->

``` r
histogram(fl$EstadoCivil, 'EstadoCivil')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-39.png)<!-- -->

``` r
histogram(fl$PoblaAct,'PoblaAct')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-40.png)<!-- -->

``` r
histogram(fl$PoblaOc,'PoblaOc')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-41.png)<!-- -->

``` r
histogram(fl$Cesante, 'Cesante')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-42.png)<!-- -->

``` r
histogram(fl$Aspirante, 'Aspirante')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-43.png)<!-- -->

``` r
histogram(fl$NoDeclar, 'NoDeclar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-44.png)<!-- -->

``` r
histogram(fl$Uno, 'Uno')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-17-45.png)<!-- -->

\#Histogramas Municipalidades Porcentajes

``` r
histogram2(fl$LugarNacimiento, 'LugarNacimiento')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
histogram2(fl$LugarResidencia, 'LugarResidencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
histogram2(fl$PoblaCuatro, 'PoblaCuatro')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->

``` r
histogram2(fl$DificultadVer, 'DificultadVer')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-4.png)<!-- -->

``` r
histogram2(fl$DificultadOir, 'DificultadOir')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-5.png)<!-- -->

``` r
histogram2(fl$DificultadCaminar, 'DificultadCaminar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-6.png)<!-- -->

``` r
histogram2(fl$DificultadRecordar, 'DificultadRecordar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-7.png)<!-- -->

``` r
histogram2(fl$DificultadCuidadoPersonal, 'DificultadCuidadoPersonal')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-8.png)<!-- -->

``` r
histogram2(fl$DificultadComunicarse, 'DificultadComunicarse')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-9.png)<!-- -->

``` r
histogram2(fl$MujeresFert, 'MujeresFert')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-10.png)<!-- -->

``` r
histogram2(fl$MujeresHijosNacido, 'MujeresHijosNacido')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-11.png)<!-- -->

``` r
histogram2(fl$MujeresHijosSobrev, 'MujeresHijosSobrev')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-12.png)<!-- -->

``` r
histogram2(fl$NivelEducacion, 'NivelEducacion')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-13.png)<!-- -->

``` r
histogram2(fl$CausaInasistencia, 'CausaInasistencia')+coord_flip()
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-14.png)<!-- -->

``` r
histogram2(fl$PoblaSiete, 'PoblaSiete')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-15.png)<!-- -->

``` r
histogram2(fl$Lectura, 'Lectura')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-16.png)<!-- -->

``` r
histogram2(fl$Asistencia, 'Asistencia')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-17.png)<!-- -->

``` r
histogram2(fl$LugarEstudio, 'LugarEstudio')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-18.png)<!-- -->

``` r
histogram2(fl$TotalViv, 'TotalViv')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-19.png)<!-- -->

``` r
histogram2(fl$Vivienda, 'Vivienda')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-20.png)<!-- -->

``` r
histogram2(fl$TipoCasa, 'TipoCasa')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-21.png)<!-- -->

``` r
histogram2(fl$TipoOcup, 'TipoOcup')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-22.png)<!-- -->

``` r
histogram2(fl$Pared, 'Pared')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-23.png)<!-- -->

``` r
histogram2(fl$Techo, 'Techo')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-24.png)<!-- -->

``` r
histogram2(fl$Material, 'Material')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-25.png)<!-- -->

``` r
histogram2(fl$DistHogares, 'DistHogares')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-26.png)<!-- -->

``` r
histogram2(fl$Lugar, 'Lugar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-27.png)<!-- -->

``` r
histogram2(fl$Celular, 'Celular')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-28.png)<!-- -->

``` r
histogram2(fl$Compu, 'Compu')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-29.png)<!-- -->

``` r
histogram2(fl$Internet, 'Internet')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-30.png)<!-- -->

``` r
histogram2(fl$Uso, 'Uso')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-31.png)<!-- -->

``` r
histogram2(fl$Genero, 'Genero')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-32.png)<!-- -->

``` r
histogram2(fl$Edad, 'Edad')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-33.png)<!-- -->

``` r
histogram2(fl$GrupoQuinquenal, 'GrupoQuinquenal')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-34.png)<!-- -->

``` r
histogram2(fl$Zona, 'Zona')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-35.png)<!-- -->

``` r
histogram2(fl$ParentescoJefe, 'ParentescoJefe')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-36.png)<!-- -->

``` r
histogram2(fl$PoblaCol, 'PoblaCol')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-37.png)<!-- -->

``` r
histogram2(fl$PoblaDiez, 'PoblaDiez')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-38.png)<!-- -->

``` r
histogram2(fl$EstadoCivil, 'EstadoCivil')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-39.png)<!-- -->

``` r
histogram2(fl$PoblaAct,'PoblaAct')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-40.png)<!-- -->

``` r
histogram2(fl$PoblaOc,'PoblaOc')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-41.png)<!-- -->

``` r
histogram2(fl$Cesante, 'Cesante')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-42.png)<!-- -->

``` r
histogram2(fl$Aspirante, 'Aspirante')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-43.png)<!-- -->

``` r
histogram2(fl$NoDeclar, 'NoDeclar')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-44.png)<!-- -->

``` r
histogram2(fl$Uno, 'Uno')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-18-45.png)<!-- -->

\#Boxplots Municipalidades

``` r
boxplot(fl$LugarNacimiento, 'LugarNacimiento%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
boxplot(fl$LugarResidencia, 'LugarResidencia%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
boxplot(fl$PoblaCuatro, 'PoblaCuatro%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-3.png)<!-- -->

``` r
boxplot(fl$DificultadVer, 'DificultadVer%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-4.png)<!-- -->

``` r
boxplot(fl$DificultadOir, 'DificultadOir%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-5.png)<!-- -->

``` r
boxplot(fl$DificultadCaminar, 'DificultadCaminar%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-6.png)<!-- -->

``` r
boxplot(fl$DificultadRecordar, 'DificultadRecordar%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-7.png)<!-- -->

``` r
boxplot(fl$DificultadCuidadoPersonal, 'DificultadCuidadoPersonal%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-8.png)<!-- -->

``` r
boxplot(fl$DificultadComunicarse, 'DificultadComunicarse%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-9.png)<!-- -->

``` r
boxplot(fl$MujeresFert, 'MujeresFert%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-10.png)<!-- -->

``` r
boxplot(fl$MujeresHijosNacido, 'MujeresHijosNacido%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-11.png)<!-- -->

``` r
boxplot(fl$MujeresHijosSobrev, 'MujeresHijosSobrev%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-12.png)<!-- -->

``` r
boxplot(fl$NivelEducacion, 'NivelEducacion%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-13.png)<!-- -->

``` r
boxplot(fl$CausaInasistencia, 'CausaInasistencia%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-14.png)<!-- -->

``` r
boxplot(fl$PoblaSiete, 'PoblaSiete%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-15.png)<!-- -->

``` r
boxplot(fl$Lectura, 'Lectura%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-16.png)<!-- -->

``` r
boxplot(fl$Asistencia, 'Asistencia%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-17.png)<!-- -->

``` r
boxplot(fl$LugarEstudio, 'LugarEstudio%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-18.png)<!-- -->

``` r
boxplot(fl$TotalViv, 'TotalViv%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-19.png)<!-- -->

``` r
boxplot(fl$Vivienda, 'Vivienda%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-20.png)<!-- -->

``` r
boxplot(fl$TipoCasa, 'TipoCasa%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-21.png)<!-- -->

``` r
boxplot(fl$TipoOcup, 'TipoOcup%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-22.png)<!-- -->

``` r
boxplot(fl$Pared, 'Pared%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-23.png)<!-- -->

``` r
boxplot(fl$Techo, 'Techo%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-24.png)<!-- -->

``` r
boxplot(fl$Material, 'Material%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-25.png)<!-- -->

``` r
boxplot(fl$DistHogares, 'DistHogares%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-26.png)<!-- -->

``` r
boxplot(fl$Lugar, 'Lugar%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-27.png)<!-- -->

``` r
boxplot(fl$Celular, 'Celular%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-28.png)<!-- -->

``` r
boxplot(fl$Compu, 'Compu%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-29.png)<!-- -->

``` r
boxplot(fl$Internet, 'Internet%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-30.png)<!-- -->

``` r
boxplot(fl$Uso, 'Uso%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-31.png)<!-- -->

``` r
boxplot(fl$Genero, 'Genero%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-32.png)<!-- -->

``` r
boxplot(fl$Edad, 'Edad%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-33.png)<!-- -->

``` r
boxplot(fl$GrupoQuinquenal, 'GrupoQuinquenal%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-34.png)<!-- -->

``` r
boxplot(fl$Zona, 'Zona%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-35.png)<!-- -->

``` r
boxplot(fl$ParentescoJefe, 'ParentescoJefe%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-36.png)<!-- -->

``` r
boxplot(fl$PoblaCol, 'PoblaCol%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-37.png)<!-- -->

``` r
boxplot(fl$PoblaDiez, 'PoblaDiez%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-38.png)<!-- -->

``` r
boxplot(fl$EstadoCivil, 'EstadoCivil%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-39.png)<!-- -->

``` r
boxplot(fl$PoblaAct,'PoblaAct%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-40.png)<!-- -->

``` r
boxplot(fl$PoblaOc,'PoblaOc%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-41.png)<!-- -->

``` r
boxplot(fl$Cesante, 'Cesante%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-42.png)<!-- -->

``` r
boxplot(fl$Aspirante, 'Aspirante%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-43.png)<!-- -->

``` r
boxplot(fl$NoDeclar, 'NoDeclar%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-44.png)<!-- -->

``` r
boxplot(fl$Uno, 'Uno%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-19-45.png)<!-- -->

\#Boxplots Municipalidades Porcentajes

``` r
boxplot2(fl$LugarNacimiento, 'LugarNacimiento%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
boxplot2(fl$LugarResidencia, 'LugarResidencia%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

``` r
boxplot2(fl$PoblaCuatro, 'PoblaCuatro%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-3.png)<!-- -->

``` r
boxplot2(fl$DificultadVer, 'DificultadVer%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-4.png)<!-- -->

``` r
boxplot2(fl$DificultadOir, 'DificultadOir%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-5.png)<!-- -->

``` r
boxplot2(fl$DificultadCaminar, 'DificultadCaminar%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-6.png)<!-- -->

``` r
boxplot2(fl$DificultadRecordar, 'DificultadRecordar%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-7.png)<!-- -->

``` r
boxplot2(fl$DificultadCuidadoPersonal, 'DificultadCuidadoPersonal%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-8.png)<!-- -->

``` r
boxplot2(fl$DificultadComunicarse, 'DificultadComunicarse%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-9.png)<!-- -->

``` r
boxplot2(fl$MujeresFert, 'MujeresFert%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-10.png)<!-- -->

``` r
boxplot2(fl$MujeresHijosNacido, 'MujeresHijosNacido%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-11.png)<!-- -->

``` r
boxplot2(fl$MujeresHijosSobrev, 'MujeresHijosSobrev%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-12.png)<!-- -->

``` r
boxplot2(fl$NivelEducacion, 'NivelEducacion%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-13.png)<!-- -->

``` r
boxplot2(fl$CausaInasistencia, 'CausaInasistencia%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-14.png)<!-- -->

``` r
boxplot2(fl$PoblaSiete, 'PoblaSiete%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-15.png)<!-- -->

``` r
boxplot2(fl$Lectura, 'Lectura%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-16.png)<!-- -->

``` r
boxplot2(fl$Asistencia, 'Asistencia%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-17.png)<!-- -->

``` r
boxplot2(fl$LugarEstudio, 'LugarEstudio%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-18.png)<!-- -->

``` r
boxplot2(fl$TotalViv, 'TotalViv%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-19.png)<!-- -->

``` r
boxplot2(fl$Vivienda, 'Vivienda%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-20.png)<!-- -->

``` r
boxplot2(fl$TipoCasa, 'TipoCasa%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-21.png)<!-- -->

``` r
boxplot2(fl$TipoOcup, 'TipoOcup%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-22.png)<!-- -->

``` r
boxplot2(fl$Pared, 'Pared%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-23.png)<!-- -->

``` r
boxplot2(fl$Techo, 'Techo%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-24.png)<!-- -->

``` r
boxplot2(fl$Material, 'Material%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-25.png)<!-- -->

``` r
boxplot2(fl$DistHogares, 'DistHogares%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-26.png)<!-- -->

``` r
boxplot2(fl$Lugar, 'Lugar%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-27.png)<!-- -->

``` r
boxplot2(fl$Celular, 'Celular%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-28.png)<!-- -->

``` r
boxplot2(fl$Compu, 'Compu%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-29.png)<!-- -->

``` r
boxplot2(fl$Internet, 'Internet%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-30.png)<!-- -->

``` r
boxplot2(fl$Uso, 'Uso%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-31.png)<!-- -->

``` r
boxplot2(fl$Genero, 'Genero%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-32.png)<!-- -->

``` r
boxplot2(fl$Edad, 'Edad%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-33.png)<!-- -->

``` r
boxplot2(fl$GrupoQuinquenal, 'GrupoQuinquenal%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-34.png)<!-- -->

``` r
boxplot2(fl$Zona, 'Zona%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-35.png)<!-- -->

``` r
boxplot2(fl$ParentescoJefe, 'ParentescoJefe%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-36.png)<!-- -->

``` r
boxplot2(fl$PoblaCol, 'PoblaCol%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-37.png)<!-- -->

``` r
boxplot2(fl$PoblaDiez, 'PoblaDiez%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-38.png)<!-- -->

``` r
boxplot2(fl$EstadoCivil, 'EstadoCivil%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-39.png)<!-- -->

``` r
boxplot2(fl$PoblaAct,'PoblaAct%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-40.png)<!-- -->

``` r
boxplot2(fl$PoblaOc,'PoblaOc%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-41.png)<!-- -->

``` r
boxplot2(fl$Cesante, 'Cesante%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-42.png)<!-- -->

``` r
boxplot2(fl$Aspirante, 'Aspirante%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-43.png)<!-- -->

``` r
boxplot2(fl$NoDeclar, 'NoDeclar%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-44.png)<!-- -->

``` r
boxplot2(fl$Uno, 'Uno%')
```

![](ProyectoFinal_files/figure-gfm/unnamed-chunk-20-45.png)<!-- -->
