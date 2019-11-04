---
title: "TP 2 - EEA"
author: "Federico Fliguer"
date: "1 de noviembre de 2019"
output:
  html_document:
    toc: true
    toc_float: true
    theme: united
    highlight: tango
---

## 0. Seteo de la herramienta

Seteo de paquetes y de opciones para que en los gráficos no se disponga la notación científica.

```{r setup}
library("tidyverse")
library(broom)
library(modelr)
options(scipen = 50, digits = 10)
```

## 0. Preparacion de los datos

### Selección del dataset

```{r}
df <- readRDS("C:/Users/l0339237/Downloads/ar_properties.rds")
```

### Convertir a factores antes de empezar

```{r}
df$l3 = as.factor(df$l3)
df$property_type = as.factor(df$property_type)
```

## 1. Regresión lineal múltiple

#### base para entender las dummys

```{r}
factores_barrios = contrasts(df$l3)
factores_tipos = contrasts(df$property_type)
```

Si están desactivados todos los flags de l3, el que estará activado será el de Abasto. Si están desactivados todos los flags de tipo de propiedad, el que estará activado será el de Casa.

### 1. a Crear un modelo para predecir el precio con todas las covariables

```{r}
LM1 = lm(price ~ l3 + rooms + bathrooms + surface_total + surface_covered + property_type, data = df)
summary(LM1)
```

### 1. b Analizar los resultados del modelo

#### 1. b. i. Coeficientes

La estimación por mínimos cuadrados tiene intercepto negativo, por lo que el valor estimado de una casa ubicada en el abasto con cero metros cuadrados, ni baños ni ambientes sería -109.406 USD. Esto resulta incoherente por dos motivos en simultáneo: en primer lugar porque al ser un valor monetario está acotado en el 0, y en segundo lugar porque las variables se organizan de modo tal de que no existe posibilidad de una observación sin variables positivas: todos los inmuebles deben tener algo de superficie. El cero en todas las variables no pertenece al espacio que este modelo abarca. Los valores estimados de cada variable representan el aporte que hacen en promedio una unidad más de cada valor, a excepción del caso de las variables dummy (barrios, tipo de propiedad) en donde el valor estimado expresa el aumento en el precio dado por el hecho de que el inmueble esté localizado en ese barrio o sea de ese tipo, con respecto a los bases que son Abasto y Casa.

#### 1. b. ii. Dummy

Respecto de las variables dummy, lo que se observa es que, salvo algunas excepciones, las zonas cuyo coeficiente estimado es más cercano a 0 (más parecidas al Abasto) tienen valores t estimados más cercanos a 0 también, por lo que su p-valor no es menor a 0.05 y por lo tanto el coeficiente no es significativo. Esto ocurre en el caso de los barrios Parque Chas, Liniers, Chacarita, Agronomia, Almagro, Monte Castro, Caballito, Barracas, San Telmo, Villa Crespo, Villa del Parque, Villa Real, Villa Riachuelo, Villa Santa Rita. Una cuestión adicional respecto de estas variables es que su interpretación no debería ser tan inmediata como se hacía en el caso del modelo lineal simple: en ese caso se podía afirmar que el valor teórico de una propiedad aumentaba tanto como el coeficiente, en cuanto las variables se incrementaban en uno. En las variables dummy, correr el análisis al campo de lo práctico es más dificil ya que, habiendo coeficientes negativos como positivos, no sería correcto decir que el valor de una propiedad aumenta porque la propiedad quede en cierto lugar, y cae porque quede en otro lugar. Debe recordarse la cuestión de las variables base Casa y Abasto para estos fines, ya que la contribución de las diferentes zonas y tipos de inmueble es con respecto a estas dos variables base.

#### 1. b. iii. Medidas de evaluación

El R cuadrado del modelo es de 0.7764, lo que implica que del total de la variabilidad que la variable precio tiene respecto de su media, este modelo capta 77%. Es de notar que cuenta con muchas variables (todas las dummy) por lo que un ejercicio interesante sería ir reduciendolas analizando el impacto en la significatividad: Si bien el R2 debería mantenerse bajar pues siempre que se agregan variables sube, el R2 ajustado podría mantenerse, lo que implicaría que algunas de estas variables no tienen utilidad explicativa.

### 1. c. Modelo ajustado

```{r}
depto1 <- data.frame(l3="Abasto", rooms=3, bathrooms=2, surface_total=120, surface_covered=120, property_type="Departamento")
depto2 <- data.frame(l3="Balvanera", rooms=2, bathrooms=3, surface_total=100, surface_covered=80, property_type="PH")
predict(LM1, depto1)
predict(LM1, depto2)
```

En cuanto a los precios, el departamento ubicado en Abasto excede en más de un 50% en su valor a uno ubicado en Balvanera, ambos con las características que presenta el ejercicio. Una salvedad que puede realizarse, es que el ejercicio está pensado desde el punto de vista de la oferta de propiedades, sin embargo la consigna pregunta 'cuál sería mejor tener' lo cual en este contexto podría incorporar también el otro lado, que es la demanda. Dicho de otra forma: la conveniencia de tener un inmueble por sobre otro debería verse con sus precios, pero también con la posibilidad de venderlos. En el ejercicio no se cuenta con datos respecto de eso, pero sí es posible realizar un boxplot de los inmuebles por barrio, para ver si alguno de estos dos tiene alguna característica que sobresale de lo común de su zona, transformándose en una propiedad con características especiales que pueda volver un poco más relativa la cuestión de la conveniencia.

```{r}
Depto_Abasto = df %>% filter(l3 == 'Abasto'&property_type == 'Departamento')
boxplot(Depto_Abasto[5:6])
points(1,120,col="red",pch=16)
points(2,120,col="red",pch=16)
```

```{r}
Ph_Balvanera = df %>% filter(l3 == 'Balvanera'&property_type == 'PH')
boxplot(Ph_Balvanera[5:6])
points(1,100,col="red",pch=16)
points(2,80,col="red",pch=16)
```

Se verifica la hipótesis de las diferencias: Si bien un departamento en con 120 metros cuadrados totales y cubiertos en Balvanera es mucho más caro que un PH de 100 metros cuadrados y 80 cubiertos en Abasto, también es mucho más atípico. Por lo tanto, no arriesgaría a asegurar que ninguno es necesariamente preferible sobre el otro.

### 1. d. Realizar un modelo sin la covariable l3 e interpretar sus resultados.

```{r}
LM2_sinl3 <- lm(price ~ rooms + bathrooms + surface_total + surface_covered + property_type , data = df)
summary(LM2_sinl3)
```

Todas las variables mantienen su signo, aunque algunas se vuelven más potentes (sus coeficientes se agrandan en valor absoluto), y otras se achican. No se encuentra ninguna que 'capture todo' el efecto del barrio, pues debería tener un comportamiento especial que ninguna tiene.

### 1. e. ¿Cuál es el modelo que mejor explica la variabilidad del precio?

Se verifica con este modelo sin la covariable l3 que la capacidad explicativa de con respecto al modelo inicial decreció, ya que el coeficiente R2 ajustado (es necesario utilizar este por la diferencia en la cantidad de covariables) decreció desde 0.78 a 0.68. Esto implica, de otra forma, que el barrio es una variable explicativa para el precio de los inmuebles.

## 2. Creación de variables

### 2. a. Crear una nueva variable barrios que divida a los barrios según el precio por metro cuadrado promedio de las propiedades.

A efectos de generar que los grupos concentren la misma cantidad de viviendas, se fijarán como puntos de corte el percentil 33 y el 66 de la variable del precio medio.

```{r}
l3_mean_price <- df %>% 
        group_by(l3 = as_factor(l3)) %>% 
        summarise(mean_price = mean(price/surface_total))
quantile(l3_mean_price$mean_price, c(.33, .66))
```

```{r}
barrios_agrupados <- df %>% 
  group_by(l3 = as_factor(l3)) %>%
  mutate(l3_mean_price = mean(price/surface_total)) %>% 
  ungroup() %>%
  mutate(p33 = quantile(l3_mean_price, c(.33, .66))[1], 
         p66 = quantile(l3_mean_price, c(.33, .66))[2]) %>% 
  transmute(l3, barrios = as_factor(case_when(
    l3_mean_price <= p33~ "Barrio precio bajo",
    l3_mean_price <= p66 ~ "Barrio precio medio",
    TRUE ~ "Barrio precio alto"))) %>% 
  distinct()%>% 
  arrange(as.character(l3))

df_congrupos <-
  df %>% 
  inner_join(.,barrios_agrupados, by = "l3", suffix = c("","y" )) %>% 
  select(-l3)
```

### 2. b. Calcular el modelo que predice el precio en función de las nuevas covariables.

```{r}
LM3_congrupos = lm(price ~ barrios + rooms + bathrooms + surface_total + surface_covered + property_type, data = df_congrupos)
summary(LM3_congrupos)
```

En este caso, la ausencia de una variable precio activa indica que se trata de un barrio de precio bajo. Se verifica la alta significatividad de todas las variables, ya que
en todas el p-valor es tendiente a 0. La estimación muestra que la condición de 'barrio medio' hace que el valor estimado de una vivienda aumente 34K en promedio con respecto a un barrio bajo, mientras que la condición barrio alto hace que aumente m´sa de 87K. Esto refuerza la idea típica de este mercado, donde la cota es inferior y no superior, y las viviendas de precios altos son más distintas a las de precios medios que las de precios bajos.

### 2. c. ¿Qué modelo explica mejor la variabilidad de los datos?

La variabilidad de los datos es explicada mejor por el primer modelo, ya que su R2 ajustado es mayor. Sin embargo, la poca diferencia entre los dos niveles (y la gran cantidad) de variables adicionales que implica cada uno de los flags de barrios) hace que se imponga este segundo modelo, en la medida que solo pierde un 3% de variabilidad explicada, siendo un modelo mucho más simple y con todas sus variables significativas.

### 2. d. Construir una nueva variable surface_patio para la diferencia entre ambas superficies. 

#### 2. d. i. ¿Cómo se interpretarían los negativos?

```{r}
df_congrupos$surface_patio <- df_congrupos$surface_total - df_congrupos$surface_covered
df_congrupos %>% 
  filter(surface_total < surface_covered)
```

En caso de que hubieran variables de ese tipo, se reaccionaría intentando identificar si existe una regularidad común en este error (un 0 de más, números mal escritos en forma sistemática) o si se trata de casos aislados e independientes entre sí. En tal caso, se los eliminaría del análisis si fueran pocos, o se usaría la superficie cubierta (que se asume comprobada) como la total, dejando 0 para la superficie del patio.

#### 2. d. i. Calcular nuevamente el modelo lineal

```{r}
LM4_conpatio <- lm(price ~ rooms + bathrooms + surface_covered + surface_patio + property_type + barrios, data = df_congrupos)
summary(LM4_conpatio)
```

Como es de esperar, el R2 del modelo no se modifica ya que la potencia explicativa del modelo permanece igual: lo único que cambió es cómo se organizan las variables. Así, parece más lógico que el precio estimado crezca 2456 dólares por cada metro cuadrado adicional cubierto, 
mientras que crezca 916 dólares con cada metro cuadrado de patio adicional. Las variables están mejor organizadas para el carácter explicativo, pese a que el modelo funciona y performa igual.

## 3. Evaluación del modelo

### 3. a. Analizar los residuos del modelo.

Para evaluar el funcionamiento del modelo es posible analizar sus errores, bajo la premisa de que deben ser independientes, homocedásticos y con esperanza cero.

```{r}
aug <- augment(LM4_conpatio,df_congrupos)

ggplot(aug, aes(.resid)) + 
  geom_histogram(binwidth = 1500)

ggplot(aug, aes(.fitted, .resid)) +
  geom_point()+
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)
```

El primer gráfico muestra que los residuos se orientan en torno al 0, por lo que el supuesto de la media 0 parece cumplirse. Sin embargo, en el segundo gráfico se ve que la variabilidad de los residuos no es constante como el modelo requiere, sino que aumenta cuando aumenta el valor predicho.

### 3. b. Calcular el modelo con variables Rooms, Bathroom y Surface_covered en forma logarítmica.

```{r}
df_congrupos = df_congrupos %>% na_if(0)
df_congrupos = na.omit(df_congrupos)

LM4_conlogaritmos = lm(log(price) ~ barrios+log(rooms)+log(bathrooms)+log(surface_patio)+log(surface_covered)+property_type, data = df_congrupos)
summary(LM4_conlogaritmos)

```

La variabilidad explicada de log(price) por este modelo es del 82%, mayor al obtenido anteriormente de aproximadamente 78% de la variabilidad en el caso de la variable sin linealizar. La relación lin-log (como las de las variables dummy) implican el impacto que tiene la activación de la variable (en caso de que ninguna se active, precio bajo y tipo de propiedad casa) en términos porcentuales sobre el precio. La relación log-log (como el de log(rooms) o log(surface_patio) con el log(precio)) es representada por el modelo de tal forma que los coeficientes reflejan el impacto que el crecimiento en 1% de cada covariable tiene en términos porcentuales sobre la estimación de la variable. Se puede buscar verificar el cumplimiento de los supuestos

```{r}
aug2 <- augment(LM4_conlogaritmos,df_congrupos)

ggplot(aug2, aes(.fitted, .resid)) +
  geom_point()+
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)

```

Se verifica que sigue sucediendo el aumento en la variabilidad de los residuos cuando sube el valor esperado, por lo que el supuesto de la homocedasticidad ya no se cumple.

## 4. Dataframes anidados

### 4. a. Anidar por la variable property_type.

```{r}
df_anidados = df_congrupos %>% 
  group_by(property_type) %>% 
  nest()

df_anidados

```

### 4. b. Construir para cada tipo de propiedad el modelo de 2.d e interpretar los resultados en cada caso.

```{r}
modelos = function(df) {
  lm(price ~ barrios+rooms+bathrooms+surface_patio+surface_covered, data = df)
}

df_anidados = df_anidados %>% 
  mutate(model = map(data, modelos))

df_anidados$model
```

En el caso de las casas, se verifica que el valor diferencial de ubicarse en una mejor zona (sea el aumento de clase baja a clase media, o de clase media a clase alta) es mayor para el valor estimado del precio que en los otros casos. El valor que da una mayor superficie cubierta o un mayor patio es más grande en el caso de los departamentos, lo cual es esperable ya que tiende a ser menor esa superficie y sobre todo ese patio. Ese mayor coeficiente de la superficie en los departamentos produce que, a contrapeso, el coeficiente de los baños sea negativo, mostrando la esencia de este tipo de modelos: no es que el valor de los departamentos disminuzca por la incorporación de baños, sino que en el contexto de este modelo con esas variables incluidas la mejor estimación del precio lo hace.
