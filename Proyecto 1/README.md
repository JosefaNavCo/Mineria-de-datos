Proyecto 1
================

## Hecho por Josefa Navarro Cofré

## Cargando datos

``` r
setwd("/Users/jo/Documents/Minería de datos")
sanguchez <- read.csv("sanguchez.csv", sep=";")
```

## Subiendo librerias

Se usara la función summary() para ver cual es la nota minima y maxima y
la clase de datos con las que estamos trabajando

``` r
library(quanteda)
```

    ## Package version: 3.0.0
    ## Unicode version: 10.0
    ## ICU version: 61.1

    ## Parallel computing: 8 of 8 threads used.

    ## See https://quanteda.io for tutorials and examples.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.0     ✓ stringr 1.4.0
    ## ✓ tidyr   1.1.3     ✓ forcats 0.5.1
    ## ✓ readr   1.4.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(utf8)
library(ggplot2)

summary(sanguchez) 
```

    ##      url               Local            Direccion            Precio         
    ##  Length:410         Length:410         Length:410         Length:410        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  Ingredientes            nota          texto          
    ##  Length:410         Min.   :1.000   Length:410        
    ##  Class :character   1st Qu.:3.000   Class :character  
    ##  Mode  :character   Median :3.000   Mode  :character  
    ##                     Mean   :3.167                     
    ##                     3rd Qu.:4.000                     
    ##                     Max.   :5.000                     
    ##                     NA's   :8

## Borrando columnas que no aportan al análisis

``` r
sang <- sanguchez[,!(colnames(sanguchez) %in% c("url", "Local","Direccion", "texto"))]
```

## Ordenando las notas de mayor a menor

``` r
sang_notas <- sang[order(sang$nota, decreasing = TRUE),]
```

## Pasando texto a minuscula

``` r
sang_notas$Ingredientes <- char_tolower(sang_notas$Ingredientes)
```

## Sacando aquellos precios que no sean chilenos

Se asume que el restaurant se abrirá en Chile, es decir, competirá en el
mercado chileno. Por lo que los precios de otros países no nos interesan

``` r
sang_notas$Precio <- as.numeric(gsub("[$.aprox]","",sang_notas$Precio))
```

    ## Warning: NAs introduced by coercion

``` r
sang_notas <- na.omit(sang_notas)
```

## Realizando variables de notas de interes

Se asume que una buena calificacion son aquellas notas 5, ya que estamos
buscando excelencia. Una mala calificacion son 1 y 2. Nota 3 y 4 es
neutral y no es de nuestro interes

``` r
Notas_altas <-  filter(sang_notas, nota ==5)
Notas_bajas <- filter(sang_notas, nota <=2)
```

## Buscando correlacion entre precion y nota

``` r
#Para ver correlacion de notas con precios
ComparaPrecio_table <- NULL
correl <- c("Correlacion",cor(x=sang_notas$Precio,
    y=sang_notas$nota))

Prom_precioNaltas <- c("Promedio precios Notas altas", mean(Notas_altas$Precio))
ComparaPrecio_table <- rbind(ComparaPrecio_table , Prom_precioNaltas)
Max_PrecioNaltas <- c("Max Precio Notas altas", max(Notas_altas$Precio))
ComparaPrecio_table <- rbind(ComparaPrecio_table , Max_PrecioNaltas)
Min_PrecioNaltas <- c("Min Precio Notas altas", min(Notas_altas$Precio))
ComparaPrecio_table <- rbind(ComparaPrecio_table , Min_PrecioNaltas)

Prom_precioNbajas <- c("Promedio precios Notas bajas", mean(Notas_bajas$Precio))
ComparaPrecio_table <- rbind(ComparaPrecio_table , Prom_precioNbajas)
Max_PrecioNbajas <- c("Max Precio Notas bajas", max(Notas_bajas$Precio))
ComparaPrecio_table <- rbind(ComparaPrecio_table , Max_PrecioNbajas)
Min_PrecioNbajas <- c("Min Precio Notas bajas", min(Notas_bajas$Precio))
ComparaPrecio_table <- rbind(ComparaPrecio_table , Min_PrecioNbajas)
ComparaPrecio_table <- rbind(ComparaPrecio_table , correl)
ComparaPrecio_table
```

    ##                   [,1]                           [,2]                
    ## Prom_precioNaltas "Promedio precios Notas altas" "6182.12765957447"  
    ## Max_PrecioNaltas  "Max Precio Notas altas"       "8900"              
    ## Min_PrecioNaltas  "Min Precio Notas altas"       "2900"              
    ## Prom_precioNbajas "Promedio precios Notas bajas" "5924.88372093023"  
    ## Max_PrecioNbajas  "Max Precio Notas bajas"       "14600"             
    ## Min_PrecioNbajas  "Min Precio Notas bajas"       "1650"              
    ## correl            "Correlacion"                  "0.0252257815319803"

De acuerdo al calculo de estadistica, No se logra apreciar una
correlacion entre precios y notas (R ≈ 0). Por lo que, no se considerará
la variable de precio en nuestro análisis Debido a que estamos buscando
los mejores ingredientes, sin catalogar el precio. Esto no quiere decir
que un sandwich que este bien evaluado y se le modifica el precio,
tendrá la misma calificación.

## Borrando precios de los datos de Notas altas y bajas

``` r
Notas_altas <- Notas_altas[,!(colnames(Notas_altas)%in% c("Precio"))]
Notas_bajas <- Notas_bajas[,!(colnames(Notas_bajas)%in% c("Precio"))]
```

## Concluyendo

Cualquier conjunto de ingredientes que se encuentre en Notas\_altas será
considerado una buena calificacion. Cabe destacar que si no se respeta
el conjunto de ingredientes para cada sandwich puede verse afectada la
calificación.

``` r
Notas_altas <- Notas_altas[,!(colnames(Notas_altas)%in% c("nota"))]
Notas_altas
```

    ##  [1] "carne, zucchini grillado con un toque de ajo, piment\xf3n acaramelado, champi\xf1ones y ricotta."                                                                                           
    ##  [2] "filete de res, con relish de betarraga, ra\xedz picante (r\xe1bano), aros de cebolla y champi\xf1ones portobellos"                                                                          
    ##  [3] "hamburguesa 50% carne y 50% prieta, queso parmesano, manzana verde caramelizada, palta, r\xfacula y salsa al ajo."                                                                        
    ##  [4] "salm\xf3n en mantequilla aromatizada, tomates asados, queso crema saborizado, coraz\xf3n de alcachofa y mix verde."                                                                         
    ##  [5] "crudo de res machacado, con crema \xe1cida al ciboullette, alcaparras, pepinillos en vinagre, chucrut de repollo morado y lechuga en sopaipillas."                                          
    ##  [6] " sierra ahumada desmenuzada, con mayonesa de ajo, perejil y lim\xf3n, sobre una cama de lechuga costina y pebre de cochayuyo, en sopaipilla."                                               
    ##  [7] " mechada, palta, tomate y mayonesa casera, en sopaipillas"                                                                                                                                  
    ##  [8] "lomo de cerdo, palta, tomate, mayonesa casera."                                                                                                                                             
    ##  [9] "reineta frita, lechuga, tomate, cebolla morada y blanca, aj\xedes blanqueados y salsa al cilantro."                                                                                         
    ## [10] "longaniza de jabal\xed con cebolla, aj\xed amarillo, r\xfacula y zanahoria, en pan de queso"                                                                                              
    ## [11] "alb\xf3ndigas de quinoa crocante, hojas verdes, huevo frito, brotes de alfalfa y chips de camote."                                                                                          
    ## [12] "carne mechada, cebolla morada, tomate fresco, aj\xed verde, agregado chancho en piedra y merk\xe9n, en marraqueta y 1/2"                                                                    
    ## [13] "lengua, porotos verdes, mayonesa casera, tomate."                                                                                                                                           
    ## [14] "cebolla caramelizada, queso mantecoso derretido, huevo frito, kiss my hass (palta frita)"                                                                                                   
    ## [15] "salsa de queso azul, pepinillos, cebolla caramelizada, queso mantecoso, espinaca y salsa de la casa."                                                                                       
    ## [16] "pulled chicken ahumado, queso de cabra, salsa teriyaki, chips de betarraga frita, mayonesa de cilantro y berros en pan franc\xe9s casero."                                                  
    ## [17] "ajo, aj\xed, mayonesa, salsa de tomate, chucrut"                                                                                                                                            
    ## [18] " hamburguesa de carne de vacuno y cerdo molida condimentada con ajo y aj\xed, tomate, chucrut y mayonesa casera."                                                                           
    ## [19] "relleno de pollo a la plancha, berros, salsa de cilantro, y un mix de champi\xf1ones con queso crema derretido"                                                                             
    ## [20] "queso, tomate, lechuga, cebolla, pepinillos, mostaza, ketchup, tocino."                                                                                                                     
    ## [21] "mix punta paleta, tocino y punta ganso, tocino, queso, encurtidos, salsa."                                                                                                                  
    ## [22] "costillar de cerdo al merk\xe9n, braseado y desmenuzado con cebollas caramelizadas, con tomates confitados al or\xe9gano y fonduta de quesos, montado en un exquisito pan amasado especial."
    ## [23] "churrasco filete, tomate, porotos verdes, aj\xed verde. agregado mayonesa"                                                                                                                  
    ## [24] "camarones fritos en panko, palta, queso mozzarella ,salsa sweet chili"                                                                                                                      
    ## [25] "salsa la birrer\xeda, lechuga, tomate, queso de cabra, queso mozzarella, champi\xf1ones salteados, tocino ahumado crocante, aros de cebolla apanados"                                       
    ## [26] "hamburguesa de 3 cortes de carne, base de lechuga y tomate, queso cheddar, y aj\xedes verdes rellenos con mozzarella, tocino crocante, mayonesa spicy "                                     
    ## [27] "lechuga, tomate, queso cheddar, aj\xedes verdes rellenos con mozzarella y tocino, mayonesa spicy"                                                                                           
    ## [28] "costillar de cerdo 100% org\xe1nico de cocci\xf3n lenta con salsa bbq casera, ensaladilla criolla con palta, en pan ciabatta."                                                              
    ## [29] "plateada de vacuno, pastelera de choclo, papas hilo, huevo frito y cebolla rebosada."                                                                                                       
    ## [30] "carne mechada, palta, tomate, mayonesa, en marraqueta"                                                                                                                                      
    ## [31] "lomo de res, cebolla morada, piment\xf3n y papas hilo, en pan ciabatta peruano"                                                                                                             
    ## [32] "pan focaccia de masa madre, berenjenas, tomates asados, ricotta fr\xeda, pesto de la casa y aceite de ajo"                                                                                  
    ## [33] "chorizo argentino, chimichurri, huevo frito y cebolla caramelizada, en marraqueta"                                                                                                          
    ## [34] "mechada, salsa de tomate, palta, mayonesa, tomate."                                                                                                                                         
    ## [35] "entra\xf1as, chorizo, pebre, cebolla asada, mayonera casada"                                                                                                                                
    ## [36] "milanesa de posta (apanada sin harina), con queso mozzarella, cebollita frita, salsa de tomates casera y un toque de or\xe9gano."                                                           
    ## [37] "hamburguesa (340 grs de carne), con doble queso cheddar, tocino crocante, aros de cebolla apanados, mayonesa de eneldo y cebolla morada"                                                    
    ## [38] "hamburguesa de porotos negros, con cebolla caramelizada, veganesa de cilantro, lechuga, betarraga y zanahoria en juliana y salsa barbecue hecha ah\xed mismo."                              
    ## [39] "doble hamburguesa, queso americano, aj\xed cherry pepper, tocino y salsa streat"                                                                                                            
    ## [40] "pollo deshilachado en curry y leche de coco, con tiras de zanahoria, zapallo italiano, piment\xf3n, cebolla morada y lechuga"                                                               
    ## [41] "soya, s\xe9samo tostado, miel, jengibre y aceite de s\xe9samo, con palta, lechuga y queso crema."                                                                                           
    ## [42] "hamburguesa cubierta con crema de queso azul, mermelada de cebolla, lechuga, pepinillos, panceta ahumada."                                                                                  
    ## [43] "queso americano, aj\xed cherry pepper, tocino, salsa streat"                                                                                                                                
    ## [44] "hamburguesa de champi\xf1ones portobello y legumbres, tomate y k\xe9tchup casero, queso cheddar, cebollitas caramelizadas, mix verde y aderezo de zanahoria, en pan casero."                
    ## [45] "jam\xf3n serrano,queso de huentelauqu\xe9n y pimientos asados."                                                                                                                             
    ## [46] "hamburguesa de ara\xf1ita de wagyu, tomate, lechuga, mermelada de cebolla, tocino, pepinillos, queso mozzarella, servido en pan brioche."                                                   
    ## [47] "zapallo italiano apanado, hummus, r\xfacula, pimiento morr\xf3n y mayonesa de la casa"
